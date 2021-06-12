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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A key-value pair that describes a property of a pipeline object. The
-- value is specified as either a string value (@StringValue@) or a
-- reference to another object (@RefValue@) but not as both.
--
-- /See:/ 'newField' smart constructor.
data Field = Field'
  { -- | The field value, expressed as a String.
    stringValue :: Core.Maybe Core.Text,
    -- | The field value, expressed as the identifier of another object.
    refValue :: Core.Maybe Core.Text,
    -- | The field identifier.
    key :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  Field
newField pKey_ =
  Field'
    { stringValue = Core.Nothing,
      refValue = Core.Nothing,
      key = pKey_
    }

-- | The field value, expressed as a String.
field_stringValue :: Lens.Lens' Field (Core.Maybe Core.Text)
field_stringValue = Lens.lens (\Field' {stringValue} -> stringValue) (\s@Field' {} a -> s {stringValue = a} :: Field)

-- | The field value, expressed as the identifier of another object.
field_refValue :: Lens.Lens' Field (Core.Maybe Core.Text)
field_refValue = Lens.lens (\Field' {refValue} -> refValue) (\s@Field' {} a -> s {refValue = a} :: Field)

-- | The field identifier.
field_key :: Lens.Lens' Field Core.Text
field_key = Lens.lens (\Field' {key} -> key) (\s@Field' {} a -> s {key = a} :: Field)

instance Core.FromJSON Field where
  parseJSON =
    Core.withObject
      "Field"
      ( \x ->
          Field'
            Core.<$> (x Core..:? "stringValue")
            Core.<*> (x Core..:? "refValue")
            Core.<*> (x Core..: "key")
      )

instance Core.Hashable Field

instance Core.NFData Field

instance Core.ToJSON Field where
  toJSON Field' {..} =
    Core.object
      ( Core.catMaybes
          [ ("stringValue" Core..=) Core.<$> stringValue,
            ("refValue" Core..=) Core.<$> refValue,
            Core.Just ("key" Core..= key)
          ]
      )
