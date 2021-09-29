// Copyright 2021 Parity Technologies (UK) Ltd.
// This file is part of Polkadot.

// Polkadot is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Polkadot is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Polkadot.  If not, see <http://www.gnu.org/licenses/>.

use super::*;
use crate::inclusion::CandidatePendingAvailability;
use frame_benchmarking::{account, benchmarks, impl_benchmark_test_suite};
use frame_system::{pallet_prelude::*, RawOrigin};
use primitives::v1::{
	CandidateHash, DisputeStatement, DisputeStatementSet, ExplicitDisputeStatement, Id as ParaId,
	InvalidDisputeStatementKind, ValidatorId, ValidatorIndex,
};
use sp_core::{crypto::CryptoType, Pair};
use sp_runtime::traits::{One, Zero};
use std::collections::HashMap;

// Brainstorming worst case aspects:
//
// - there are many fresh disputes, where the disputes have just been initiated.
// - create a new `DisputeState` with blank bitfields.
// - make sure spam slotes is incremented by have DisputeStatementSet U DisputeState < byzantize_thresh
// - force one side to have a super majority, so we enable slashing

fn run_to_block<T: Config>(to: u32) {
	let to = to.into();
	while frame_system::Pallet::<T>::block_number() < to {
		let b = frame_system::Pallet::<T>::block_number();
		crate::initializer::Pallet::<T>::on_finalize(b);

		let b = b + One::one();
		frame_system::Pallet::<T>::set_block_number(b);
		crate::initializer::Pallet::<T>::on_initialize(b);
	}
}

benchmarks! {
	enter {

		// let total_validators = 100;
		let total_validators = 3;
		// let max_dispute_statement_sets = 100;
		let max_dispute_statement_sets = 2;
		let byzantine_statement_thresh = total_validators / 3;
		let max_statements = total_validators;
		// para block candidates. Each candidate has a dispute statement set.
		// let max_candidates = u8::MAX;
		let max_candidates = 3;

		let config = crate::configuration::Pallet::<T>::config();

		let header = T::Header::new(
			One::one(),			// number
			Default::default(), //	extrinsics_root,
			Default::default(), // storage_root,
			Default::default(), // parent_hash,
			Default::default(), // digest,

		);

		let validator_pairs = (0..total_validators).map(|i| {
			let pair = <ValidatorId as CryptoType>::Pair::generate().0;

			let account: T::AccountId = account("validator", i, i);
			(account, pair)
		}).collect::<Vec<_>>();

		// create map of validator public id => signing pair.
		let validator_map = validator_pairs
			.iter()
			.map(|(_, pair)| (pair.public(), pair.clone()))
			.collect::<HashMap<_,_>>();

		let validators_public = validator_pairs.iter().map(|(a, v)| (a, v.public()));

		// initialize session 1.
		crate::initializer::Pallet::<T>::test_trigger_on_new_session(
			true, // indicate the validator set has changed
			1, // session index
			validators_public.clone(), // validators
			None, // queued - when this is None validators are considered queued
			// Some(validators_public) // queued
		);

		let vals = validators_public.collect::<Vec<_>>();
		let val_0 = vals.get(0).unwrap();

		run_to_block::<T>(2);
		frame_system::Pallet::<T>::set_parent_hash(header.hash());

		// assert the current session is 0.
		let current_session = 1;
		assert_eq!(<crate::shared::Pallet<T>>::session_index(), current_session);

		// get validators from session info. We need to refetch them since they have been shuffled.
		let validators_shuffled = crate::session_info::Pallet::<T>::session_info(current_session)
			.unwrap()
			.validators
			.clone()
			.into_iter()
			.map(|public| {
				let pair = validator_map.get(&public).unwrap().clone();
				(public, pair)
			})
			.collect::<Vec<_>>();


		let mut spam_count = 0;
		let disputes = (0..max_candidates).map(|seed| {
			let candidate_hash = CandidateHash(sp_core::H256::repeat_byte(seed));

			// fill corresponding storage items for inclusion that will be `taken` when `collect_disputed`
			// is called.

			// TODO can make benchmark gated function for making this struct inclusion so fields don't
			// need to be pub(crate)
			let candidate_availability = CandidatePendingAvailability::<T::Hash, T::BlockNumber> {
				core: (seed as u32).into(),
				hash: candidate_hash,
				descriptor: Default::default(),
				availability_votes: Default::default(),
				backers: Default::default(),
				relay_parent_number: Zero::zero(),
				backed_in_number: One::one(),
				backing_group: (seed as u32).into(),
			};

			crate::inclusion::PendingAvailability::<T>::insert(
				ParaId::from(seed as u32), candidate_availability
			);

			// create the set of statements to dispute the above candidate hash.
			let statement_range = if spam_count < config.dispute_max_spam_slots {
				// if we have not hit the spam dispute statement limit, only make up to the byzantine
				// threshold number of statements.

				// TODO: we could max the amount of spam even more by  taking 3 1/3 chunks of
				// validator set and having them each attest to different statements. Right now we
				// just use 1 1/3 chunk.
				0..byzantine_statement_thresh
			} else {
				// otherwise, make the maximum number of statements, which is over the byzantine
				// threshold and thus these statements will not be counted as potential spam.
				0..max_statements
			};
			let statements = statement_range.map(|validator_index| {
				let validator_pair = &validators_shuffled.get(validator_index as usize).unwrap().1;
				let signing_payload = ExplicitDisputeStatement {
					valid: false,
					candidate_hash: candidate_hash.clone(),
					session: current_session,
				}
				.signing_payload();
				let statement_sig = validator_pair.sign(&signing_payload);

				(
					DisputeStatement::Invalid(InvalidDisputeStatementKind::Explicit),
					ValidatorIndex(validator_index),
					statement_sig,
				)
			}).collect::<Vec<_>>();

			if spam_count < config.dispute_max_spam_slots {
				spam_count += 1;
			}

			// return dispute statements with metadata.
			DisputeStatementSet {
				candidate_hash: candidate_hash.clone(),
				session: current_session,
				statements
			}

		}).collect::<Vec<_>>();

		assert_eq!(
			crate::disputes::SpamSlots::<T>::get(&current_session),
			None
		);

		// TODO
		// - fill `PendingAvailability`
		// - fill `PendingAvailabilityCommitments`
		// - make sure they are all taken in `Inclusion::collect_disputes`

		let data = ParachainsInherentData {
			bitfields: Default::default(),
			backed_candidates: Default::default(),
			disputes, // Vec<DisputeStatementSet>
			parent_header: header,
		};

	}: _(RawOrigin::None, data)
	verify {
		// check that the disputes storage has updated as expected.

		let spam_slots = crate::disputes::SpamSlots::<T>::get(&current_session).unwrap();
		assert!(
			// we expect the first 1/3rd of validators to have maxed out spam slots. Sub 1 for when
			// there is an odd number of validators.
			&spam_slots[..(byzantine_statement_thresh - 1) as usize]
				.iter()
				.all(|n| *n == config.dispute_max_spam_slots)
		);
		assert!(
			&spam_slots[byzantine_statement_thresh as usize ..]
				.iter()
				.all(|n| *n == 0)
		);
	}
}

// - no spam scenario
// - max backed candidates scenario

impl_benchmark_test_suite!(
	Pallet,
	crate::mock::new_test_ext(Default::default()),
	crate::mock::Test
);
