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
-- Module      : Amazonka.DataPipeline.Types.ParameterValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Types.ParameterValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A value or list of parameter values.
--
-- /See:/ 'newParameterValue' smart constructor.
data ParameterValue = ParameterValue'
  { -- | The ID of the parameter value.
    id :: Prelude.Text,
    -- | The field value, expressed as a String.
    stringValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'parameterValue_id' - The ID of the parameter value.
--
-- 'stringValue', 'parameterValue_stringValue' - The field value, expressed as a String.
newParameterValue ::
  -- | 'id'
  Prelude.Text ->
  -- | 'stringValue'
  Prelude.Text ->
  ParameterValue
newParameterValue pId_ pStringValue_ =
  ParameterValue'
    { id = pId_,
      stringValue = pStringValue_
    }

-- | The ID of the parameter value.
parameterValue_id :: Lens.Lens' ParameterValue Prelude.Text
parameterValue_id = Lens.lens (\ParameterValue' {id} -> id) (\s@ParameterValue' {} a -> s {id = a} :: ParameterValue)

-- | The field value, expressed as a String.
parameterValue_stringValue :: Lens.Lens' ParameterValue Prelude.Text
parameterValue_stringValue = Lens.lens (\ParameterValue' {stringValue} -> stringValue) (\s@ParameterValue' {} a -> s {stringValue = a} :: ParameterValue)

instance Data.FromJSON ParameterValue where
  parseJSON =
    Data.withObject
      "ParameterValue"
      ( \x ->
          ParameterValue'
            Prelude.<$> (x Data..: "id")
            Prelude.<*> (x Data..: "stringValue")
      )

instance Prelude.Hashable ParameterValue where
  hashWithSalt _salt ParameterValue' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData ParameterValue where
  rnf ParameterValue' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf stringValue

instance Data.ToJSON ParameterValue where
  toJSON ParameterValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("id" Data..= id),
            Prelude.Just ("stringValue" Data..= stringValue)
          ]
      )
