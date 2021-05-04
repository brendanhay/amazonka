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
-- Module      : Network.AWS.CloudDirectory.Types.TypedAttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedAttributeValue where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the data for a typed attribute. You can set one, and only
-- one, of the elements. Each attribute in an item is a name-value pair.
-- Attributes have a single value.
--
-- /See:/ 'newTypedAttributeValue' smart constructor.
data TypedAttributeValue = TypedAttributeValue'
  { -- | A string data value.
    stringValue :: Prelude.Maybe Prelude.Text,
    -- | A Boolean data value.
    booleanValue :: Prelude.Maybe Prelude.Bool,
    -- | A binary data value.
    binaryValue :: Prelude.Maybe Prelude.Base64,
    -- | A number data value.
    numberValue :: Prelude.Maybe Prelude.Text,
    -- | A date and time value.
    datetimeValue :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { stringValue = Prelude.Nothing,
      booleanValue = Prelude.Nothing,
      binaryValue = Prelude.Nothing,
      numberValue = Prelude.Nothing,
      datetimeValue = Prelude.Nothing
    }

-- | A string data value.
typedAttributeValue_stringValue :: Lens.Lens' TypedAttributeValue (Prelude.Maybe Prelude.Text)
typedAttributeValue_stringValue = Lens.lens (\TypedAttributeValue' {stringValue} -> stringValue) (\s@TypedAttributeValue' {} a -> s {stringValue = a} :: TypedAttributeValue)

-- | A Boolean data value.
typedAttributeValue_booleanValue :: Lens.Lens' TypedAttributeValue (Prelude.Maybe Prelude.Bool)
typedAttributeValue_booleanValue = Lens.lens (\TypedAttributeValue' {booleanValue} -> booleanValue) (\s@TypedAttributeValue' {} a -> s {booleanValue = a} :: TypedAttributeValue)

-- | A binary data value.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
typedAttributeValue_binaryValue :: Lens.Lens' TypedAttributeValue (Prelude.Maybe Prelude.ByteString)
typedAttributeValue_binaryValue = Lens.lens (\TypedAttributeValue' {binaryValue} -> binaryValue) (\s@TypedAttributeValue' {} a -> s {binaryValue = a} :: TypedAttributeValue) Prelude.. Lens.mapping Prelude._Base64

-- | A number data value.
typedAttributeValue_numberValue :: Lens.Lens' TypedAttributeValue (Prelude.Maybe Prelude.Text)
typedAttributeValue_numberValue = Lens.lens (\TypedAttributeValue' {numberValue} -> numberValue) (\s@TypedAttributeValue' {} a -> s {numberValue = a} :: TypedAttributeValue)

-- | A date and time value.
typedAttributeValue_datetimeValue :: Lens.Lens' TypedAttributeValue (Prelude.Maybe Prelude.UTCTime)
typedAttributeValue_datetimeValue = Lens.lens (\TypedAttributeValue' {datetimeValue} -> datetimeValue) (\s@TypedAttributeValue' {} a -> s {datetimeValue = a} :: TypedAttributeValue) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON TypedAttributeValue where
  parseJSON =
    Prelude.withObject
      "TypedAttributeValue"
      ( \x ->
          TypedAttributeValue'
            Prelude.<$> (x Prelude..:? "StringValue")
            Prelude.<*> (x Prelude..:? "BooleanValue")
            Prelude.<*> (x Prelude..:? "BinaryValue")
            Prelude.<*> (x Prelude..:? "NumberValue")
            Prelude.<*> (x Prelude..:? "DatetimeValue")
      )

instance Prelude.Hashable TypedAttributeValue

instance Prelude.NFData TypedAttributeValue

instance Prelude.ToJSON TypedAttributeValue where
  toJSON TypedAttributeValue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StringValue" Prelude..=) Prelude.<$> stringValue,
            ("BooleanValue" Prelude..=) Prelude.<$> booleanValue,
            ("BinaryValue" Prelude..=) Prelude.<$> binaryValue,
            ("NumberValue" Prelude..=) Prelude.<$> numberValue,
            ("DatetimeValue" Prelude..=)
              Prelude.<$> datetimeValue
          ]
      )
