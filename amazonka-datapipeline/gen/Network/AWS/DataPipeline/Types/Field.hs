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
-- Module      : Network.AWS.DataPipeline.Types.Field
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.Field where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A key-value pair that describes a property of a pipeline object. The
-- value is specified as either a string value (@StringValue@) or a
-- reference to another object (@RefValue@) but not as both.
--
-- /See:/ 'newField' smart constructor.
data Field = Field'
  { -- | The field value, expressed as a String.
    stringValue :: Prelude.Maybe Prelude.Text,
    -- | The field value, expressed as the identifier of another object.
    refValue :: Prelude.Maybe Prelude.Text,
    -- | The field identifier.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Field' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stringValue', 'field_stringValue' - The field value, expressed as a String.
--
-- 'refValue', 'field_refValue' - The field value, expressed as the identifier of another object.
--
-- 'key', 'field_key' - The field identifier.
newField ::
  -- | 'key'
  Prelude.Text ->
  Field
newField pKey_ =
  Field'
    { stringValue = Prelude.Nothing,
      refValue = Prelude.Nothing,
      key = pKey_
    }

-- | The field value, expressed as a String.
field_stringValue :: Lens.Lens' Field (Prelude.Maybe Prelude.Text)
field_stringValue = Lens.lens (\Field' {stringValue} -> stringValue) (\s@Field' {} a -> s {stringValue = a} :: Field)

-- | The field value, expressed as the identifier of another object.
field_refValue :: Lens.Lens' Field (Prelude.Maybe Prelude.Text)
field_refValue = Lens.lens (\Field' {refValue} -> refValue) (\s@Field' {} a -> s {refValue = a} :: Field)

-- | The field identifier.
field_key :: Lens.Lens' Field Prelude.Text
field_key = Lens.lens (\Field' {key} -> key) (\s@Field' {} a -> s {key = a} :: Field)

instance Prelude.FromJSON Field where
  parseJSON =
    Prelude.withObject
      "Field"
      ( \x ->
          Field'
            Prelude.<$> (x Prelude..:? "stringValue")
            Prelude.<*> (x Prelude..:? "refValue")
            Prelude.<*> (x Prelude..: "key")
      )

instance Prelude.Hashable Field

instance Prelude.NFData Field

instance Prelude.ToJSON Field where
  toJSON Field' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("stringValue" Prelude..=) Prelude.<$> stringValue,
            ("refValue" Prelude..=) Prelude.<$> refValue,
            Prelude.Just ("key" Prelude..= key)
          ]
      )
