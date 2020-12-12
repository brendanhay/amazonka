{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedAttributeValue
  ( TypedAttributeValue (..),

    -- * Smart constructor
    mkTypedAttributeValue,

    -- * Lenses
    tavBinaryValue,
    tavDatetimeValue,
    tavNumberValue,
    tavStringValue,
    tavBooleanValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the data for a typed attribute. You can set one, and only one, of the elements. Each attribute in an item is a name-value pair. Attributes have a single value.
--
-- /See:/ 'mkTypedAttributeValue' smart constructor.
data TypedAttributeValue = TypedAttributeValue'
  { binaryValue ::
      Lude.Maybe Lude.Base64,
    datetimeValue :: Lude.Maybe Lude.Timestamp,
    numberValue :: Lude.Maybe Lude.Text,
    stringValue :: Lude.Maybe Lude.Text,
    booleanValue :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TypedAttributeValue' with the minimum fields required to make a request.
--
-- * 'binaryValue' - A binary data value.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'booleanValue' - A Boolean data value.
-- * 'datetimeValue' - A date and time value.
-- * 'numberValue' - A number data value.
-- * 'stringValue' - A string data value.
mkTypedAttributeValue ::
  TypedAttributeValue
mkTypedAttributeValue =
  TypedAttributeValue'
    { binaryValue = Lude.Nothing,
      datetimeValue = Lude.Nothing,
      numberValue = Lude.Nothing,
      stringValue = Lude.Nothing,
      booleanValue = Lude.Nothing
    }

-- | A binary data value.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'binaryValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavBinaryValue :: Lens.Lens' TypedAttributeValue (Lude.Maybe Lude.Base64)
tavBinaryValue = Lens.lens (binaryValue :: TypedAttributeValue -> Lude.Maybe Lude.Base64) (\s a -> s {binaryValue = a} :: TypedAttributeValue)
{-# DEPRECATED tavBinaryValue "Use generic-lens or generic-optics with 'binaryValue' instead." #-}

-- | A date and time value.
--
-- /Note:/ Consider using 'datetimeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavDatetimeValue :: Lens.Lens' TypedAttributeValue (Lude.Maybe Lude.Timestamp)
tavDatetimeValue = Lens.lens (datetimeValue :: TypedAttributeValue -> Lude.Maybe Lude.Timestamp) (\s a -> s {datetimeValue = a} :: TypedAttributeValue)
{-# DEPRECATED tavDatetimeValue "Use generic-lens or generic-optics with 'datetimeValue' instead." #-}

-- | A number data value.
--
-- /Note:/ Consider using 'numberValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavNumberValue :: Lens.Lens' TypedAttributeValue (Lude.Maybe Lude.Text)
tavNumberValue = Lens.lens (numberValue :: TypedAttributeValue -> Lude.Maybe Lude.Text) (\s a -> s {numberValue = a} :: TypedAttributeValue)
{-# DEPRECATED tavNumberValue "Use generic-lens or generic-optics with 'numberValue' instead." #-}

-- | A string data value.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavStringValue :: Lens.Lens' TypedAttributeValue (Lude.Maybe Lude.Text)
tavStringValue = Lens.lens (stringValue :: TypedAttributeValue -> Lude.Maybe Lude.Text) (\s a -> s {stringValue = a} :: TypedAttributeValue)
{-# DEPRECATED tavStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

-- | A Boolean data value.
--
-- /Note:/ Consider using 'booleanValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavBooleanValue :: Lens.Lens' TypedAttributeValue (Lude.Maybe Lude.Bool)
tavBooleanValue = Lens.lens (booleanValue :: TypedAttributeValue -> Lude.Maybe Lude.Bool) (\s a -> s {booleanValue = a} :: TypedAttributeValue)
{-# DEPRECATED tavBooleanValue "Use generic-lens or generic-optics with 'booleanValue' instead." #-}

instance Lude.FromJSON TypedAttributeValue where
  parseJSON =
    Lude.withObject
      "TypedAttributeValue"
      ( \x ->
          TypedAttributeValue'
            Lude.<$> (x Lude..:? "BinaryValue")
            Lude.<*> (x Lude..:? "DatetimeValue")
            Lude.<*> (x Lude..:? "NumberValue")
            Lude.<*> (x Lude..:? "StringValue")
            Lude.<*> (x Lude..:? "BooleanValue")
      )

instance Lude.ToJSON TypedAttributeValue where
  toJSON TypedAttributeValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BinaryValue" Lude..=) Lude.<$> binaryValue,
            ("DatetimeValue" Lude..=) Lude.<$> datetimeValue,
            ("NumberValue" Lude..=) Lude.<$> numberValue,
            ("StringValue" Lude..=) Lude.<$> stringValue,
            ("BooleanValue" Lude..=) Lude.<$> booleanValue
          ]
      )
