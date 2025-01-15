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
-- Module      : Amazonka.DataPipeline.Types.Field
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Types.Field where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A key-value pair that describes a property of a pipeline object. The
-- value is specified as either a string value (@StringValue@) or a
-- reference to another object (@RefValue@) but not as both.
--
-- /See:/ 'newField' smart constructor.
data Field = Field'
  { -- | The field value, expressed as the identifier of another object.
    refValue :: Prelude.Maybe Prelude.Text,
    -- | The field value, expressed as a String.
    stringValue :: Prelude.Maybe Prelude.Text,
    -- | The field identifier.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Field' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'refValue', 'field_refValue' - The field value, expressed as the identifier of another object.
--
-- 'stringValue', 'field_stringValue' - The field value, expressed as a String.
--
-- 'key', 'field_key' - The field identifier.
newField ::
  -- | 'key'
  Prelude.Text ->
  Field
newField pKey_ =
  Field'
    { refValue = Prelude.Nothing,
      stringValue = Prelude.Nothing,
      key = pKey_
    }

-- | The field value, expressed as the identifier of another object.
field_refValue :: Lens.Lens' Field (Prelude.Maybe Prelude.Text)
field_refValue = Lens.lens (\Field' {refValue} -> refValue) (\s@Field' {} a -> s {refValue = a} :: Field)

-- | The field value, expressed as a String.
field_stringValue :: Lens.Lens' Field (Prelude.Maybe Prelude.Text)
field_stringValue = Lens.lens (\Field' {stringValue} -> stringValue) (\s@Field' {} a -> s {stringValue = a} :: Field)

-- | The field identifier.
field_key :: Lens.Lens' Field Prelude.Text
field_key = Lens.lens (\Field' {key} -> key) (\s@Field' {} a -> s {key = a} :: Field)

instance Data.FromJSON Field where
  parseJSON =
    Data.withObject
      "Field"
      ( \x ->
          Field'
            Prelude.<$> (x Data..:? "refValue")
            Prelude.<*> (x Data..:? "stringValue")
            Prelude.<*> (x Data..: "key")
      )

instance Prelude.Hashable Field where
  hashWithSalt _salt Field' {..} =
    _salt
      `Prelude.hashWithSalt` refValue
      `Prelude.hashWithSalt` stringValue
      `Prelude.hashWithSalt` key

instance Prelude.NFData Field where
  rnf Field' {..} =
    Prelude.rnf refValue `Prelude.seq`
      Prelude.rnf stringValue `Prelude.seq`
        Prelude.rnf key

instance Data.ToJSON Field where
  toJSON Field' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("refValue" Data..=) Prelude.<$> refValue,
            ("stringValue" Data..=) Prelude.<$> stringValue,
            Prelude.Just ("key" Data..= key)
          ]
      )
