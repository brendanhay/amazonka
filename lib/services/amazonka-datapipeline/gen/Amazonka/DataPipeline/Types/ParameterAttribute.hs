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
-- Module      : Amazonka.DataPipeline.Types.ParameterAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Types.ParameterAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The attributes allowed or specified with a parameter object.
--
-- /See:/ 'newParameterAttribute' smart constructor.
data ParameterAttribute = ParameterAttribute'
  { -- | The field identifier.
    key :: Prelude.Text,
    -- | The field value, expressed as a String.
    stringValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON ParameterAttribute where
  parseJSON =
    Data.withObject
      "ParameterAttribute"
      ( \x ->
          ParameterAttribute'
            Prelude.<$> (x Data..: "key")
            Prelude.<*> (x Data..: "stringValue")
      )

instance Prelude.Hashable ParameterAttribute where
  hashWithSalt _salt ParameterAttribute' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData ParameterAttribute where
  rnf ParameterAttribute' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf stringValue

instance Data.ToJSON ParameterAttribute where
  toJSON ParameterAttribute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Data..= key),
            Prelude.Just ("stringValue" Data..= stringValue)
          ]
      )
