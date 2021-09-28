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
use frame_benchmarking::{account, benchmarks, impl_benchmark_test_suite};
use frame_system::RawOrigin;
use primitives::v1::{
	CandidateHash, DisputeStatement, DisputeStatementSet, ExplicitDisputeStatement,
	InvalidDisputeStatementKind, ValidatorId, ValidatorIndex,
};
use sp_core::{crypto::CryptoType, Pair};
use sp_runtime::traits::One;
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

		let total_validators = 1_000;
		let max_dispute_statement_sets = 100;
		let max_statements = total_validators / 3;
		// para block candidates. Each candidate has a dispute statement set.
		let max_candidates = u8::MAX;

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


		// let disputes = (0..max_candidates).map(|candidate_seed| {
		let disputes = (0..1).map(|candidate_seed| {
			let candidate_hash = CandidateHash(sp_core::H256::repeat_byte(candidate_seed));

			// create the set of statements to dispute the above candidate hash.
			let statements = (0..1).map(|validator_index| {
			// let statements = (0..max_statements).map(|validator_index| {
				let validator_pair = &validators_shuffled.get(validator_index as usize).unwrap().1;

				//DEBUG

				let statement = DisputeStatement::Invalid(InvalidDisputeStatementKind::Explicit);
				let signing_payload = ExplicitDisputeStatement {
					valid: false,
					candidate_hash: candidate_hash.clone(),
					session: current_session,
				}
				.signing_payload();
				let statement_sig = validator_pair.sign(&signing_payload);

				println!("A val index {}, public is {:?}", validator_index, validator_pair.public());
				println!(
					"A2 hash: {:?}, \n session {:?}, \n statement {:?}, \n sig {:?}",
					candidate_hash, current_session,
					statement, statement_sig
				);

				(
					statement,
					ValidatorIndex(validator_index),
					statement_sig,
				)
			}).collect::<Vec<_>>();

			// return dispute statements with metadata.
			DisputeStatementSet {
				candidate_hash: candidate_hash.clone(),
				session: current_session,
				statements
			}
		}).collect::<Vec<_>>();

		let data = ParachainsInherentData {
			bitfields: Default::default(),
			backed_candidates: Default::default(),
			disputes, // Vec<DisputeStatementSet>
			parent_header: header,
		};
	}: _(RawOrigin::None, data)
	verify {
		// check that the disputes storage has updated as expected.
	}
}

impl_benchmark_test_suite!(
	Pallet,
	crate::mock::new_test_ext(Default::default()),
	crate::mock::Test
);
