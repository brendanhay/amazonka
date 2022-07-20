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
-- Module      : Amazonka.Pinpoint.Types.Condition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.Condition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.Operator
import Amazonka.Pinpoint.Types.SimpleCondition
import qualified Amazonka.Prelude as Prelude

-- | Specifies the conditions to evaluate for an activity in a journey, and
-- how to evaluate those conditions.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | The conditions to evaluate for the activity.
    conditions :: Prelude.Maybe [SimpleCondition],
    -- | Specifies how to handle multiple conditions for the activity. For
    -- example, if you specify two conditions for an activity, whether both or
    -- only one of the conditions must be met for the activity to be performed.
    operator :: Prelude.Maybe Operator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Condition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditions', 'condition_conditions' - The conditions to evaluate for the activity.
--
-- 'operator', 'condition_operator' - Specifies how to handle multiple conditions for the activity. For
-- example, if you specify two conditions for an activity, whether both or
-- only one of the conditions must be met for the activity to be performed.
newCondition ::
  Condition
newCondition =
  Condition'
    { conditions = Prelude.Nothing,
      operator = Prelude.Nothing
    }

-- | The conditions to evaluate for the activity.
condition_conditions :: Lens.Lens' Condition (Prelude.Maybe [SimpleCondition])
condition_conditions = Lens.lens (\Condition' {conditions} -> conditions) (\s@Condition' {} a -> s {conditions = a} :: Condition) Prelude.. Lens.mapping Lens.coerced

-- | Specifies how to handle multiple conditions for the activity. For
-- example, if you specify two conditions for an activity, whether both or
-- only one of the conditions must be met for the activity to be performed.
condition_operator :: Lens.Lens' Condition (Prelude.Maybe Operator)
condition_operator = Lens.lens (\Condition' {operator} -> operator) (\s@Condition' {} a -> s {operator = a} :: Condition)

instance Core.FromJSON Condition where
  parseJSON =
    Core.withObject
      "Condition"
      ( \x ->
          Condition'
            Prelude.<$> (x Core..:? "Conditions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Operator")
      )

instance Prelude.Hashable Condition where
  hashWithSalt _salt Condition' {..} =
    _salt `Prelude.hashWithSalt` conditions
      `Prelude.hashWithSalt` operator

instance Prelude.NFData Condition where
  rnf Condition' {..} =
    Prelude.rnf conditions
      `Prelude.seq` Prelude.rnf operator

instance Core.ToJSON Condition where
  toJSON Condition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Conditions" Core..=) Prelude.<$> conditions,
            ("Operator" Core..=) Prelude.<$> operator
          ]
      )
