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
-- Module      : Network.AWS.DataPipeline.Types.ParameterValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ParameterValue where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A value or list of parameter values.
--
-- /See:/ 'newParameterValue' smart constructor.
data ParameterValue = ParameterValue'
  { -- | The ID of the parameter value.
    id :: Prelude.Text,
    -- | The field value, expressed as a String.
    stringValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON ParameterValue where
  parseJSON =
    Prelude.withObject
      "ParameterValue"
      ( \x ->
          ParameterValue'
            Prelude.<$> (x Prelude..: "id")
            Prelude.<*> (x Prelude..: "stringValue")
      )

instance Prelude.Hashable ParameterValue

instance Prelude.NFData ParameterValue

instance Prelude.ToJSON ParameterValue where
  toJSON ParameterValue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("id" Prelude..= id),
            Prelude.Just ("stringValue" Prelude..= stringValue)
          ]
      )
