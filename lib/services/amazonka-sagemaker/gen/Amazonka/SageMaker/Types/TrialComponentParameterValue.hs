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
-- Module      : Amazonka.SageMaker.Types.TrialComponentParameterValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialComponentParameterValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The value of a hyperparameter. Only one of @NumberValue@ or
-- @StringValue@ can be specified.
--
-- This object is specified in the CreateTrialComponent request.
--
-- /See:/ 'newTrialComponentParameterValue' smart constructor.
data TrialComponentParameterValue = TrialComponentParameterValue'
  { -- | The numeric value of a numeric hyperparameter. If you specify a value
    -- for this parameter, you can\'t specify the @StringValue@ parameter.
    numberValue :: Prelude.Maybe Prelude.Double,
    -- | The string value of a categorical hyperparameter. If you specify a value
    -- for this parameter, you can\'t specify the @NumberValue@ parameter.
    stringValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrialComponentParameterValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberValue', 'trialComponentParameterValue_numberValue' - The numeric value of a numeric hyperparameter. If you specify a value
-- for this parameter, you can\'t specify the @StringValue@ parameter.
--
-- 'stringValue', 'trialComponentParameterValue_stringValue' - The string value of a categorical hyperparameter. If you specify a value
-- for this parameter, you can\'t specify the @NumberValue@ parameter.
newTrialComponentParameterValue ::
  TrialComponentParameterValue
newTrialComponentParameterValue =
  TrialComponentParameterValue'
    { numberValue =
        Prelude.Nothing,
      stringValue = Prelude.Nothing
    }

-- | The numeric value of a numeric hyperparameter. If you specify a value
-- for this parameter, you can\'t specify the @StringValue@ parameter.
trialComponentParameterValue_numberValue :: Lens.Lens' TrialComponentParameterValue (Prelude.Maybe Prelude.Double)
trialComponentParameterValue_numberValue = Lens.lens (\TrialComponentParameterValue' {numberValue} -> numberValue) (\s@TrialComponentParameterValue' {} a -> s {numberValue = a} :: TrialComponentParameterValue)

-- | The string value of a categorical hyperparameter. If you specify a value
-- for this parameter, you can\'t specify the @NumberValue@ parameter.
trialComponentParameterValue_stringValue :: Lens.Lens' TrialComponentParameterValue (Prelude.Maybe Prelude.Text)
trialComponentParameterValue_stringValue = Lens.lens (\TrialComponentParameterValue' {stringValue} -> stringValue) (\s@TrialComponentParameterValue' {} a -> s {stringValue = a} :: TrialComponentParameterValue)

instance Data.FromJSON TrialComponentParameterValue where
  parseJSON =
    Data.withObject
      "TrialComponentParameterValue"
      ( \x ->
          TrialComponentParameterValue'
            Prelude.<$> (x Data..:? "NumberValue")
            Prelude.<*> (x Data..:? "StringValue")
      )

instance
  Prelude.Hashable
    TrialComponentParameterValue
  where
  hashWithSalt _salt TrialComponentParameterValue' {..} =
    _salt `Prelude.hashWithSalt` numberValue
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData TrialComponentParameterValue where
  rnf TrialComponentParameterValue' {..} =
    Prelude.rnf numberValue
      `Prelude.seq` Prelude.rnf stringValue

instance Data.ToJSON TrialComponentParameterValue where
  toJSON TrialComponentParameterValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NumberValue" Data..=) Prelude.<$> numberValue,
            ("StringValue" Data..=) Prelude.<$> stringValue
          ]
      )
