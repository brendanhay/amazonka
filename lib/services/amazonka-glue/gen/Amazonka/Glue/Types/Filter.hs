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
-- Module      : Amazonka.Glue.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.FilterExpression
import Amazonka.Glue.Types.FilterLogicalOperator
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that splits a dataset into two, based on a filter
-- condition.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The operator used to filter rows by comparing the key value to a
    -- specified value.
    logicalOperator :: FilterLogicalOperator,
    -- | Specifies a filter expression.
    filters :: [FilterExpression]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'filter_name' - The name of the transform node.
--
-- 'inputs', 'filter_inputs' - The data inputs identified by their node names.
--
-- 'logicalOperator', 'filter_logicalOperator' - The operator used to filter rows by comparing the key value to a
-- specified value.
--
-- 'filters', 'filter_filters' - Specifies a filter expression.
newFilter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'logicalOperator'
  FilterLogicalOperator ->
  Filter
newFilter pName_ pInputs_ pLogicalOperator_ =
  Filter'
    { name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      logicalOperator = pLogicalOperator_,
      filters = Prelude.mempty
    }

-- | The name of the transform node.
filter_name :: Lens.Lens' Filter Prelude.Text
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

-- | The data inputs identified by their node names.
filter_inputs :: Lens.Lens' Filter (Prelude.NonEmpty Prelude.Text)
filter_inputs = Lens.lens (\Filter' {inputs} -> inputs) (\s@Filter' {} a -> s {inputs = a} :: Filter) Prelude.. Lens.coerced

-- | The operator used to filter rows by comparing the key value to a
-- specified value.
filter_logicalOperator :: Lens.Lens' Filter FilterLogicalOperator
filter_logicalOperator = Lens.lens (\Filter' {logicalOperator} -> logicalOperator) (\s@Filter' {} a -> s {logicalOperator = a} :: Filter)

-- | Specifies a filter expression.
filter_filters :: Lens.Lens' Filter [FilterExpression]
filter_filters = Lens.lens (\Filter' {filters} -> filters) (\s@Filter' {} a -> s {filters = a} :: Filter) Prelude.. Lens.coerced

instance Data.FromJSON Filter where
  parseJSON =
    Data.withObject
      "Filter"
      ( \x ->
          Filter'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "LogicalOperator")
            Prelude.<*> (x Data..:? "Filters" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` logicalOperator
      `Prelude.hashWithSalt` filters

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf logicalOperator
      `Prelude.seq` Prelude.rnf filters

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just
              ("LogicalOperator" Data..= logicalOperator),
            Prelude.Just ("Filters" Data..= filters)
          ]
      )
