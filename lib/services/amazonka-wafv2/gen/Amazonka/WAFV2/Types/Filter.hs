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
-- Module      : Amazonka.WAFV2.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.Condition
import Amazonka.WAFV2.Types.FilterBehavior
import Amazonka.WAFV2.Types.FilterRequirement

-- | A single logging filter, used in LoggingFilter.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | How to handle logs that satisfy the filter\'s conditions and
    -- requirement.
    behavior :: FilterBehavior,
    -- | Logic to apply to the filtering conditions. You can specify that, in
    -- order to satisfy the filter, a log must match all conditions or must
    -- match at least one condition.
    requirement :: FilterRequirement,
    -- | Match conditions for the filter.
    conditions :: Prelude.NonEmpty Condition
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
-- 'behavior', 'filter_behavior' - How to handle logs that satisfy the filter\'s conditions and
-- requirement.
--
-- 'requirement', 'filter_requirement' - Logic to apply to the filtering conditions. You can specify that, in
-- order to satisfy the filter, a log must match all conditions or must
-- match at least one condition.
--
-- 'conditions', 'filter_conditions' - Match conditions for the filter.
newFilter ::
  -- | 'behavior'
  FilterBehavior ->
  -- | 'requirement'
  FilterRequirement ->
  -- | 'conditions'
  Prelude.NonEmpty Condition ->
  Filter
newFilter pBehavior_ pRequirement_ pConditions_ =
  Filter'
    { behavior = pBehavior_,
      requirement = pRequirement_,
      conditions = Lens.coerced Lens.# pConditions_
    }

-- | How to handle logs that satisfy the filter\'s conditions and
-- requirement.
filter_behavior :: Lens.Lens' Filter FilterBehavior
filter_behavior = Lens.lens (\Filter' {behavior} -> behavior) (\s@Filter' {} a -> s {behavior = a} :: Filter)

-- | Logic to apply to the filtering conditions. You can specify that, in
-- order to satisfy the filter, a log must match all conditions or must
-- match at least one condition.
filter_requirement :: Lens.Lens' Filter FilterRequirement
filter_requirement = Lens.lens (\Filter' {requirement} -> requirement) (\s@Filter' {} a -> s {requirement = a} :: Filter)

-- | Match conditions for the filter.
filter_conditions :: Lens.Lens' Filter (Prelude.NonEmpty Condition)
filter_conditions = Lens.lens (\Filter' {conditions} -> conditions) (\s@Filter' {} a -> s {conditions = a} :: Filter) Prelude.. Lens.coerced

instance Data.FromJSON Filter where
  parseJSON =
    Data.withObject
      "Filter"
      ( \x ->
          Filter'
            Prelude.<$> (x Data..: "Behavior")
            Prelude.<*> (x Data..: "Requirement")
            Prelude.<*> (x Data..: "Conditions")
      )

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` behavior
      `Prelude.hashWithSalt` requirement
      `Prelude.hashWithSalt` conditions

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf behavior `Prelude.seq`
      Prelude.rnf requirement `Prelude.seq`
        Prelude.rnf conditions

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Behavior" Data..= behavior),
            Prelude.Just ("Requirement" Data..= requirement),
            Prelude.Just ("Conditions" Data..= conditions)
          ]
      )
