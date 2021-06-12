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
-- Module      : Network.AWS.CloudDirectory.Types.TypedAttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedAttributeValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the data for a typed attribute. You can set one, and only
-- one, of the elements. Each attribute in an item is a name-value pair.
-- Attributes have a single value.
--
-- /See:/ 'newTypedAttributeValue' smart constructor.
data TypedAttributeValue = TypedAttributeValue'
  { -- | A string data value.
    stringValue :: Core.Maybe Core.Text,
    -- | A Boolean data value.
    booleanValue :: Core.Maybe Core.Bool,
    -- | A binary data value.
    binaryValue :: Core.Maybe Core.Base64,
    -- | A number data value.
    numberValue :: Core.Maybe Core.Text,
    -- | A date and time value.
    datetimeValue :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TypedAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stringValue', 'typedAttributeValue_stringValue' - A string data value.
--
-- 'booleanValue', 'typedAttributeValue_booleanValue' - A Boolean data value.
--
-- 'binaryValue', 'typedAttributeValue_binaryValue' - A binary data value.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'numberValue', 'typedAttributeValue_numberValue' - A number data value.
--
-- 'datetimeValue', 'typedAttributeValue_datetimeValue' - A date and time value.
newTypedAttributeValue ::
  TypedAttributeValue
newTypedAttributeValue =
  TypedAttributeValue'
    { stringValue = Core.Nothing,
      booleanValue = Core.Nothing,
      binaryValue = Core.Nothing,
      numberValue = Core.Nothing,
      datetimeValue = Core.Nothing
    }

-- | A string data value.
typedAttributeValue_stringValue :: Lens.Lens' TypedAttributeValue (Core.Maybe Core.Text)
typedAttributeValue_stringValue = Lens.lens (\TypedAttributeValue' {stringValue} -> stringValue) (\s@TypedAttributeValue' {} a -> s {stringValue = a} :: TypedAttributeValue)

-- | A Boolean data value.
typedAttributeValue_booleanValue :: Lens.Lens' TypedAttributeValue (Core.Maybe Core.Bool)
typedAttributeValue_booleanValue = Lens.lens (\TypedAttributeValue' {booleanValue} -> booleanValue) (\s@TypedAttributeValue' {} a -> s {booleanValue = a} :: TypedAttributeValue)

-- | A binary data value.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
typedAttributeValue_binaryValue :: Lens.Lens' TypedAttributeValue (Core.Maybe Core.ByteString)
typedAttributeValue_binaryValue = Lens.lens (\TypedAttributeValue' {binaryValue} -> binaryValue) (\s@TypedAttributeValue' {} a -> s {binaryValue = a} :: TypedAttributeValue) Core.. Lens.mapping Core._Base64

-- | A number data value.
typedAttributeValue_numberValue :: Lens.Lens' TypedAttributeValue (Core.Maybe Core.Text)
typedAttributeValue_numberValue = Lens.lens (\TypedAttributeValue' {numberValue} -> numberValue) (\s@TypedAttributeValue' {} a -> s {numberValue = a} :: TypedAttributeValue)

-- | A date and time value.
typedAttributeValue_datetimeValue :: Lens.Lens' TypedAttributeValue (Core.Maybe Core.UTCTime)
typedAttributeValue_datetimeValue = Lens.lens (\TypedAttributeValue' {datetimeValue} -> datetimeValue) (\s@TypedAttributeValue' {} a -> s {datetimeValue = a} :: TypedAttributeValue) Core.. Lens.mapping Core._Time

instance Core.FromJSON TypedAttributeValue where
  parseJSON =
    Core.withObject
      "TypedAttributeValue"
      ( \x ->
          TypedAttributeValue'
            Core.<$> (x Core..:? "StringValue")
            Core.<*> (x Core..:? "BooleanValue")
            Core.<*> (x Core..:? "BinaryValue")
            Core.<*> (x Core..:? "NumberValue")
            Core.<*> (x Core..:? "DatetimeValue")
      )

instance Core.Hashable TypedAttributeValue

instance Core.NFData TypedAttributeValue

instance Core.ToJSON TypedAttributeValue where
  toJSON TypedAttributeValue' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StringValue" Core..=) Core.<$> stringValue,
            ("BooleanValue" Core..=) Core.<$> booleanValue,
            ("BinaryValue" Core..=) Core.<$> binaryValue,
            ("NumberValue" Core..=) Core.<$> numberValue,
            ("DatetimeValue" Core..=) Core.<$> datetimeValue
          ]
      )
