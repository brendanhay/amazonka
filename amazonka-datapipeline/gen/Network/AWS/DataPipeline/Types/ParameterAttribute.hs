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
-- Module      : Network.AWS.DataPipeline.Types.ParameterAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ParameterAttribute where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The attributes allowed or specified with a parameter object.
--
-- /See:/ 'newParameterAttribute' smart constructor.
data ParameterAttribute = ParameterAttribute'
  { -- | The field identifier.
    key :: Prelude.Text,
    -- | The field value, expressed as a String.
    stringValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ParameterAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'parameterAttribute_key' - The field identifier.
--
-- 'stringValue', 'parameterAttribute_stringValue' - The field value, expressed as a String.
newParameterAttribute ::
  -- | 'key'
  Prelude.Text ->
  -- | 'stringValue'
  Prelude.Text ->
  ParameterAttribute
newParameterAttribute pKey_ pStringValue_ =
  ParameterAttribute'
    { key = pKey_,
      stringValue = pStringValue_
    }

-- | The field identifier.
parameterAttribute_key :: Lens.Lens' ParameterAttribute Prelude.Text
parameterAttribute_key = Lens.lens (\ParameterAttribute' {key} -> key) (\s@ParameterAttribute' {} a -> s {key = a} :: ParameterAttribute)

-- | The field value, expressed as a String.
parameterAttribute_stringValue :: Lens.Lens' ParameterAttribute Prelude.Text
parameterAttribute_stringValue = Lens.lens (\ParameterAttribute' {stringValue} -> stringValue) (\s@ParameterAttribute' {} a -> s {stringValue = a} :: ParameterAttribute)

instance Prelude.FromJSON ParameterAttribute where
  parseJSON =
    Prelude.withObject
      "ParameterAttribute"
      ( \x ->
          ParameterAttribute'
            Prelude.<$> (x Prelude..: "key")
            Prelude.<*> (x Prelude..: "stringValue")
      )

instance Prelude.Hashable ParameterAttribute

instance Prelude.NFData ParameterAttribute

instance Prelude.ToJSON ParameterAttribute where
  toJSON ParameterAttribute' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Prelude..= key),
            Prelude.Just ("stringValue" Prelude..= stringValue)
          ]
      )
