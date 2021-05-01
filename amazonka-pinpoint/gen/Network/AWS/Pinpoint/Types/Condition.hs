{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.Condition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Condition where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Operator
import Network.AWS.Pinpoint.Types.SimpleCondition
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the conditions to evaluate for an activity in a journey, and
-- how to evaluate those conditions.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | Specifies how to handle multiple conditions for the activity. For
    -- example, if you specify two conditions for an activity, whether both or
    -- only one of the conditions must be met for the activity to be performed.
    operator :: Prelude.Maybe Operator,
    -- | The conditions to evaluate for the activity.
    conditions :: Prelude.Maybe [SimpleCondition]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Condition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operator', 'condition_operator' - Specifies how to handle multiple conditions for the activity. For
-- example, if you specify two conditions for an activity, whether both or
-- only one of the conditions must be met for the activity to be performed.
--
-- 'conditions', 'condition_conditions' - The conditions to evaluate for the activity.
newCondition ::
  Condition
newCondition =
  Condition'
    { operator = Prelude.Nothing,
      conditions = Prelude.Nothing
    }

-- | Specifies how to handle multiple conditions for the activity. For
-- example, if you specify two conditions for an activity, whether both or
-- only one of the conditions must be met for the activity to be performed.
condition_operator :: Lens.Lens' Condition (Prelude.Maybe Operator)
condition_operator = Lens.lens (\Condition' {operator} -> operator) (\s@Condition' {} a -> s {operator = a} :: Condition)

-- | The conditions to evaluate for the activity.
condition_conditions :: Lens.Lens' Condition (Prelude.Maybe [SimpleCondition])
condition_conditions = Lens.lens (\Condition' {conditions} -> conditions) (\s@Condition' {} a -> s {conditions = a} :: Condition) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Condition where
  parseJSON =
    Prelude.withObject
      "Condition"
      ( \x ->
          Condition'
            Prelude.<$> (x Prelude..:? "Operator")
            Prelude.<*> ( x Prelude..:? "Conditions"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Condition

instance Prelude.NFData Condition

instance Prelude.ToJSON Condition where
  toJSON Condition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Operator" Prelude..=) Prelude.<$> operator,
            ("Conditions" Prelude..=) Prelude.<$> conditions
          ]
      )
