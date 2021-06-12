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
-- Module      : Network.AWS.SageMaker.Types.TrialComponentParameterValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentParameterValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The value of a hyperparameter. Only one of @NumberValue@ or
-- @StringValue@ can be specified.
--
-- This object is specified in the CreateTrialComponent request.
--
-- /See:/ 'newTrialComponentParameterValue' smart constructor.
data TrialComponentParameterValue = TrialComponentParameterValue'
  { -- | The string value of a categorical hyperparameter. If you specify a value
    -- for this parameter, you can\'t specify the @NumberValue@ parameter.
    stringValue :: Core.Maybe Core.Text,
    -- | The numeric value of a numeric hyperparameter. If you specify a value
    -- for this parameter, you can\'t specify the @StringValue@ parameter.
    numberValue :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrialComponentParameterValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stringValue', 'trialComponentParameterValue_stringValue' - The string value of a categorical hyperparameter. If you specify a value
-- for this parameter, you can\'t specify the @NumberValue@ parameter.
--
-- 'numberValue', 'trialComponentParameterValue_numberValue' - The numeric value of a numeric hyperparameter. If you specify a value
-- for this parameter, you can\'t specify the @StringValue@ parameter.
newTrialComponentParameterValue ::
  TrialComponentParameterValue
newTrialComponentParameterValue =
  TrialComponentParameterValue'
    { stringValue =
        Core.Nothing,
      numberValue = Core.Nothing
    }

-- | The string value of a categorical hyperparameter. If you specify a value
-- for this parameter, you can\'t specify the @NumberValue@ parameter.
trialComponentParameterValue_stringValue :: Lens.Lens' TrialComponentParameterValue (Core.Maybe Core.Text)
trialComponentParameterValue_stringValue = Lens.lens (\TrialComponentParameterValue' {stringValue} -> stringValue) (\s@TrialComponentParameterValue' {} a -> s {stringValue = a} :: TrialComponentParameterValue)

-- | The numeric value of a numeric hyperparameter. If you specify a value
-- for this parameter, you can\'t specify the @StringValue@ parameter.
trialComponentParameterValue_numberValue :: Lens.Lens' TrialComponentParameterValue (Core.Maybe Core.Double)
trialComponentParameterValue_numberValue = Lens.lens (\TrialComponentParameterValue' {numberValue} -> numberValue) (\s@TrialComponentParameterValue' {} a -> s {numberValue = a} :: TrialComponentParameterValue)

instance Core.FromJSON TrialComponentParameterValue where
  parseJSON =
    Core.withObject
      "TrialComponentParameterValue"
      ( \x ->
          TrialComponentParameterValue'
            Core.<$> (x Core..:? "StringValue")
            Core.<*> (x Core..:? "NumberValue")
      )

instance Core.Hashable TrialComponentParameterValue

instance Core.NFData TrialComponentParameterValue

instance Core.ToJSON TrialComponentParameterValue where
  toJSON TrialComponentParameterValue' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StringValue" Core..=) Core.<$> stringValue,
            ("NumberValue" Core..=) Core.<$> numberValue
          ]
      )
