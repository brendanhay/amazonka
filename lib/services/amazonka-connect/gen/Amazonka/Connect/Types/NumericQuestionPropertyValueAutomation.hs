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
-- Module      : Amazonka.Connect.Types.NumericQuestionPropertyValueAutomation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.NumericQuestionPropertyValueAutomation where

import Amazonka.Connect.Types.NumericQuestionPropertyAutomationLabel
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the property value used in automation of a numeric
-- questions. Label values are associated with minimum and maximum values
-- for the numeric question.
--
-- -   Sentiment scores have a minimum value of -5 and maximum value of 5.
--
-- -   Duration labels, such as @NON_TALK_TIME@, @CONTACT_DURATION@,
--     @AGENT_INTERACTION_DURATION@, @CUSTOMER_HOLD_TIME@ have a minimum
--     value of 0 and maximum value of 28800.
--
-- -   Percentages have a minimum value of 0 and maximum value of 100.
--
-- -   @NUMBER_OF_INTERRUPTIONS@ has a minimum value of 0 and maximum value
--     of 1000.
--
-- /See:/ 'newNumericQuestionPropertyValueAutomation' smart constructor.
data NumericQuestionPropertyValueAutomation = NumericQuestionPropertyValueAutomation'
  { -- | The property label of the automation.
    label :: NumericQuestionPropertyAutomationLabel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumericQuestionPropertyValueAutomation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'label', 'numericQuestionPropertyValueAutomation_label' - The property label of the automation.
newNumericQuestionPropertyValueAutomation ::
  -- | 'label'
  NumericQuestionPropertyAutomationLabel ->
  NumericQuestionPropertyValueAutomation
newNumericQuestionPropertyValueAutomation pLabel_ =
  NumericQuestionPropertyValueAutomation'
    { label =
        pLabel_
    }

-- | The property label of the automation.
numericQuestionPropertyValueAutomation_label :: Lens.Lens' NumericQuestionPropertyValueAutomation NumericQuestionPropertyAutomationLabel
numericQuestionPropertyValueAutomation_label = Lens.lens (\NumericQuestionPropertyValueAutomation' {label} -> label) (\s@NumericQuestionPropertyValueAutomation' {} a -> s {label = a} :: NumericQuestionPropertyValueAutomation)

instance
  Data.FromJSON
    NumericQuestionPropertyValueAutomation
  where
  parseJSON =
    Data.withObject
      "NumericQuestionPropertyValueAutomation"
      ( \x ->
          NumericQuestionPropertyValueAutomation'
            Prelude.<$> (x Data..: "Label")
      )

instance
  Prelude.Hashable
    NumericQuestionPropertyValueAutomation
  where
  hashWithSalt
    _salt
    NumericQuestionPropertyValueAutomation' {..} =
      _salt `Prelude.hashWithSalt` label

instance
  Prelude.NFData
    NumericQuestionPropertyValueAutomation
  where
  rnf NumericQuestionPropertyValueAutomation' {..} =
    Prelude.rnf label

instance
  Data.ToJSON
    NumericQuestionPropertyValueAutomation
  where
  toJSON NumericQuestionPropertyValueAutomation' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Label" Data..= label)]
      )
