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
-- Module      : Network.AWS.SageMaker.Types.TrialComponentParameterValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentParameterValue where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The value of a hyperparameter. Only one of @NumberValue@ or
-- @StringValue@ can be specified.
--
-- This object is specified in the CreateTrialComponent request.
--
-- /See:/ 'newTrialComponentParameterValue' smart constructor.
data TrialComponentParameterValue = TrialComponentParameterValue'
  { -- | The string value of a categorical hyperparameter. If you specify a value
    -- for this parameter, you can\'t specify the @NumberValue@ parameter.
    stringValue :: Prelude.Maybe Prelude.Text,
    -- | The numeric value of a numeric hyperparameter. If you specify a value
    -- for this parameter, you can\'t specify the @StringValue@ parameter.
    numberValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      numberValue = Prelude.Nothing
    }

-- | The string value of a categorical hyperparameter. If you specify a value
-- for this parameter, you can\'t specify the @NumberValue@ parameter.
trialComponentParameterValue_stringValue :: Lens.Lens' TrialComponentParameterValue (Prelude.Maybe Prelude.Text)
trialComponentParameterValue_stringValue = Lens.lens (\TrialComponentParameterValue' {stringValue} -> stringValue) (\s@TrialComponentParameterValue' {} a -> s {stringValue = a} :: TrialComponentParameterValue)

-- | The numeric value of a numeric hyperparameter. If you specify a value
-- for this parameter, you can\'t specify the @StringValue@ parameter.
trialComponentParameterValue_numberValue :: Lens.Lens' TrialComponentParameterValue (Prelude.Maybe Prelude.Double)
trialComponentParameterValue_numberValue = Lens.lens (\TrialComponentParameterValue' {numberValue} -> numberValue) (\s@TrialComponentParameterValue' {} a -> s {numberValue = a} :: TrialComponentParameterValue)

instance
  Prelude.FromJSON
    TrialComponentParameterValue
  where
  parseJSON =
    Prelude.withObject
      "TrialComponentParameterValue"
      ( \x ->
          TrialComponentParameterValue'
            Prelude.<$> (x Prelude..:? "StringValue")
            Prelude.<*> (x Prelude..:? "NumberValue")
      )

instance
  Prelude.Hashable
    TrialComponentParameterValue

instance Prelude.NFData TrialComponentParameterValue

instance Prelude.ToJSON TrialComponentParameterValue where
  toJSON TrialComponentParameterValue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StringValue" Prelude..=) Prelude.<$> stringValue,
            ("NumberValue" Prelude..=) Prelude.<$> numberValue
          ]
      )
