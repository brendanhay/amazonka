{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.Field
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.Field
  ( Field (..),

    -- * Smart constructor
    mkField,

    -- * Lenses
    fRefValue,
    fStringValue,
    fKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A key-value pair that describes a property of a pipeline object. The value is specified as either a string value (@StringValue@ ) or a reference to another object (@RefValue@ ) but not as both.
--
-- /See:/ 'mkField' smart constructor.
data Field = Field'
  { refValue :: Lude.Maybe Lude.Text,
    stringValue :: Lude.Maybe Lude.Text,
    key :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Field' with the minimum fields required to make a request.
--
-- * 'key' - The field identifier.
-- * 'refValue' - The field value, expressed as the identifier of another object.
-- * 'stringValue' - The field value, expressed as a String.
mkField ::
  -- | 'key'
  Lude.Text ->
  Field
mkField pKey_ =
  Field'
    { refValue = Lude.Nothing,
      stringValue = Lude.Nothing,
      key = pKey_
    }

-- | The field value, expressed as the identifier of another object.
--
-- /Note:/ Consider using 'refValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fRefValue :: Lens.Lens' Field (Lude.Maybe Lude.Text)
fRefValue = Lens.lens (refValue :: Field -> Lude.Maybe Lude.Text) (\s a -> s {refValue = a} :: Field)
{-# DEPRECATED fRefValue "Use generic-lens or generic-optics with 'refValue' instead." #-}

-- | The field value, expressed as a String.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fStringValue :: Lens.Lens' Field (Lude.Maybe Lude.Text)
fStringValue = Lens.lens (stringValue :: Field -> Lude.Maybe Lude.Text) (\s a -> s {stringValue = a} :: Field)
{-# DEPRECATED fStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

-- | The field identifier.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fKey :: Lens.Lens' Field Lude.Text
fKey = Lens.lens (key :: Field -> Lude.Text) (\s a -> s {key = a} :: Field)
{-# DEPRECATED fKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON Field where
  parseJSON =
    Lude.withObject
      "Field"
      ( \x ->
          Field'
            Lude.<$> (x Lude..:? "refValue")
            Lude.<*> (x Lude..:? "stringValue")
            Lude.<*> (x Lude..: "key")
      )

instance Lude.ToJSON Field where
  toJSON Field' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("refValue" Lude..=) Lude.<$> refValue,
            ("stringValue" Lude..=) Lude.<$> stringValue,
            Lude.Just ("key" Lude..= key)
          ]
      )
