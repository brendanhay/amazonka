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
-- Module      : Amazonka.IoTEventsData.Types.SimpleRuleEvaluation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.SimpleRuleEvaluation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types.ComparisonOperator
import qualified Amazonka.Prelude as Prelude

-- | Information needed to compare two values with a comparison operator.
--
-- /See:/ 'newSimpleRuleEvaluation' smart constructor.
data SimpleRuleEvaluation = SimpleRuleEvaluation'
  { -- | The value of the input property, on the left side of the comparison
    -- operator.
    inputPropertyValue :: Prelude.Maybe Prelude.Text,
    -- | The comparison operator.
    operator :: Prelude.Maybe ComparisonOperator,
    -- | The threshold value, on the right side of the comparison operator.
    thresholdValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimpleRuleEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputPropertyValue', 'simpleRuleEvaluation_inputPropertyValue' - The value of the input property, on the left side of the comparison
-- operator.
--
-- 'operator', 'simpleRuleEvaluation_operator' - The comparison operator.
--
-- 'thresholdValue', 'simpleRuleEvaluation_thresholdValue' - The threshold value, on the right side of the comparison operator.
newSimpleRuleEvaluation ::
  SimpleRuleEvaluation
newSimpleRuleEvaluation =
  SimpleRuleEvaluation'
    { inputPropertyValue =
        Prelude.Nothing,
      operator = Prelude.Nothing,
      thresholdValue = Prelude.Nothing
    }

-- | The value of the input property, on the left side of the comparison
-- operator.
simpleRuleEvaluation_inputPropertyValue :: Lens.Lens' SimpleRuleEvaluation (Prelude.Maybe Prelude.Text)
simpleRuleEvaluation_inputPropertyValue = Lens.lens (\SimpleRuleEvaluation' {inputPropertyValue} -> inputPropertyValue) (\s@SimpleRuleEvaluation' {} a -> s {inputPropertyValue = a} :: SimpleRuleEvaluation)

-- | The comparison operator.
simpleRuleEvaluation_operator :: Lens.Lens' SimpleRuleEvaluation (Prelude.Maybe ComparisonOperator)
simpleRuleEvaluation_operator = Lens.lens (\SimpleRuleEvaluation' {operator} -> operator) (\s@SimpleRuleEvaluation' {} a -> s {operator = a} :: SimpleRuleEvaluation)

-- | The threshold value, on the right side of the comparison operator.
simpleRuleEvaluation_thresholdValue :: Lens.Lens' SimpleRuleEvaluation (Prelude.Maybe Prelude.Text)
simpleRuleEvaluation_thresholdValue = Lens.lens (\SimpleRuleEvaluation' {thresholdValue} -> thresholdValue) (\s@SimpleRuleEvaluation' {} a -> s {thresholdValue = a} :: SimpleRuleEvaluation)

instance Data.FromJSON SimpleRuleEvaluation where
  parseJSON =
    Data.withObject
      "SimpleRuleEvaluation"
      ( \x ->
          SimpleRuleEvaluation'
            Prelude.<$> (x Data..:? "inputPropertyValue")
            Prelude.<*> (x Data..:? "operator")
            Prelude.<*> (x Data..:? "thresholdValue")
      )

instance Prelude.Hashable SimpleRuleEvaluation where
  hashWithSalt _salt SimpleRuleEvaluation' {..} =
    _salt
      `Prelude.hashWithSalt` inputPropertyValue
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` thresholdValue

instance Prelude.NFData SimpleRuleEvaluation where
  rnf SimpleRuleEvaluation' {..} =
    Prelude.rnf inputPropertyValue
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf thresholdValue
