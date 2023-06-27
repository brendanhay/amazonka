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
-- Module      : Amazonka.Connect.Types.EvaluationFormNumericQuestionAutomation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormNumericQuestionAutomation where

import Amazonka.Connect.Types.NumericQuestionPropertyValueAutomation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the automation configuration in numeric questions.
--
-- /See:/ 'newEvaluationFormNumericQuestionAutomation' smart constructor.
data EvaluationFormNumericQuestionAutomation = EvaluationFormNumericQuestionAutomation'
  { -- | The property value of the automation.
    propertyValue :: Prelude.Maybe NumericQuestionPropertyValueAutomation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormNumericQuestionAutomation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyValue', 'evaluationFormNumericQuestionAutomation_propertyValue' - The property value of the automation.
newEvaluationFormNumericQuestionAutomation ::
  EvaluationFormNumericQuestionAutomation
newEvaluationFormNumericQuestionAutomation =
  EvaluationFormNumericQuestionAutomation'
    { propertyValue =
        Prelude.Nothing
    }

-- | The property value of the automation.
evaluationFormNumericQuestionAutomation_propertyValue :: Lens.Lens' EvaluationFormNumericQuestionAutomation (Prelude.Maybe NumericQuestionPropertyValueAutomation)
evaluationFormNumericQuestionAutomation_propertyValue = Lens.lens (\EvaluationFormNumericQuestionAutomation' {propertyValue} -> propertyValue) (\s@EvaluationFormNumericQuestionAutomation' {} a -> s {propertyValue = a} :: EvaluationFormNumericQuestionAutomation)

instance
  Data.FromJSON
    EvaluationFormNumericQuestionAutomation
  where
  parseJSON =
    Data.withObject
      "EvaluationFormNumericQuestionAutomation"
      ( \x ->
          EvaluationFormNumericQuestionAutomation'
            Prelude.<$> (x Data..:? "PropertyValue")
      )

instance
  Prelude.Hashable
    EvaluationFormNumericQuestionAutomation
  where
  hashWithSalt
    _salt
    EvaluationFormNumericQuestionAutomation' {..} =
      _salt `Prelude.hashWithSalt` propertyValue

instance
  Prelude.NFData
    EvaluationFormNumericQuestionAutomation
  where
  rnf EvaluationFormNumericQuestionAutomation' {..} =
    Prelude.rnf propertyValue

instance
  Data.ToJSON
    EvaluationFormNumericQuestionAutomation
  where
  toJSON EvaluationFormNumericQuestionAutomation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PropertyValue" Data..=)
              Prelude.<$> propertyValue
          ]
      )
