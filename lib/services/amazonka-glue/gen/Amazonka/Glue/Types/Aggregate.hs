{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.Types.Aggregate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Aggregate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.AggregateOperation
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that groups rows by chosen fields and computes the
-- aggregated value by specified function.
--
-- /See:/ 'newAggregate' smart constructor.
data Aggregate = Aggregate'
  { -- | The name of the transform node.
    name :: Prelude.Text,
    -- | Specifies the fields and rows to use as inputs for the aggregate
    -- transform.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | Specifies the fields to group by.
    groups :: [[Prelude.Text]],
    -- | Specifies the aggregate functions to be performed on specified fields.
    aggs :: Prelude.NonEmpty AggregateOperation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Aggregate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'aggregate_name' - The name of the transform node.
--
-- 'inputs', 'aggregate_inputs' - Specifies the fields and rows to use as inputs for the aggregate
-- transform.
--
-- 'groups', 'aggregate_groups' - Specifies the fields to group by.
--
-- 'aggs', 'aggregate_aggs' - Specifies the aggregate functions to be performed on specified fields.
newAggregate ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'aggs'
  Prelude.NonEmpty AggregateOperation ->
  Aggregate
newAggregate pName_ pInputs_ pAggs_ =
  Aggregate'
    { name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      groups = Prelude.mempty,
      aggs = Lens.coerced Lens.# pAggs_
    }

-- | The name of the transform node.
aggregate_name :: Lens.Lens' Aggregate Prelude.Text
aggregate_name = Lens.lens (\Aggregate' {name} -> name) (\s@Aggregate' {} a -> s {name = a} :: Aggregate)

-- | Specifies the fields and rows to use as inputs for the aggregate
-- transform.
aggregate_inputs :: Lens.Lens' Aggregate (Prelude.NonEmpty Prelude.Text)
aggregate_inputs = Lens.lens (\Aggregate' {inputs} -> inputs) (\s@Aggregate' {} a -> s {inputs = a} :: Aggregate) Prelude.. Lens.coerced

-- | Specifies the fields to group by.
aggregate_groups :: Lens.Lens' Aggregate [[Prelude.Text]]
aggregate_groups = Lens.lens (\Aggregate' {groups} -> groups) (\s@Aggregate' {} a -> s {groups = a} :: Aggregate) Prelude.. Lens.coerced

-- | Specifies the aggregate functions to be performed on specified fields.
aggregate_aggs :: Lens.Lens' Aggregate (Prelude.NonEmpty AggregateOperation)
aggregate_aggs = Lens.lens (\Aggregate' {aggs} -> aggs) (\s@Aggregate' {} a -> s {aggs = a} :: Aggregate) Prelude.. Lens.coerced

instance Data.FromJSON Aggregate where
  parseJSON =
    Data.withObject
      "Aggregate"
      ( \x ->
          Aggregate'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..:? "Groups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Aggs")
      )

instance Prelude.Hashable Aggregate where
  hashWithSalt _salt Aggregate' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` aggs

instance Prelude.NFData Aggregate where
  rnf Aggregate' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf aggs

instance Data.ToJSON Aggregate where
  toJSON Aggregate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Groups" Data..= groups),
            Prelude.Just ("Aggs" Data..= aggs)
          ]
      )
