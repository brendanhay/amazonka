{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AssetPropertyVariant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AssetPropertyVariant
  ( AssetPropertyVariant (..),

    -- * Smart constructor
    mkAssetPropertyVariant,

    -- * Lenses
    apvIntegerValue,
    apvDoubleValue,
    apvStringValue,
    apvBooleanValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains an asset property value (of a single type).
--
-- /See:/ 'mkAssetPropertyVariant' smart constructor.
data AssetPropertyVariant = AssetPropertyVariant'
  { integerValue ::
      Lude.Maybe Lude.Text,
    doubleValue :: Lude.Maybe Lude.Text,
    stringValue :: Lude.Maybe Lude.Text,
    booleanValue :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssetPropertyVariant' with the minimum fields required to make a request.
--
-- * 'booleanValue' - Optional. A string that contains the boolean value (@true@ or @false@ ) of the value entry. Accepts substitution templates.
-- * 'doubleValue' - Optional. A string that contains the double value of the value entry. Accepts substitution templates.
-- * 'integerValue' - Optional. A string that contains the integer value of the value entry. Accepts substitution templates.
-- * 'stringValue' - Optional. The string value of the value entry. Accepts substitution templates.
mkAssetPropertyVariant ::
  AssetPropertyVariant
mkAssetPropertyVariant =
  AssetPropertyVariant'
    { integerValue = Lude.Nothing,
      doubleValue = Lude.Nothing,
      stringValue = Lude.Nothing,
      booleanValue = Lude.Nothing
    }

-- | Optional. A string that contains the integer value of the value entry. Accepts substitution templates.
--
-- /Note:/ Consider using 'integerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvIntegerValue :: Lens.Lens' AssetPropertyVariant (Lude.Maybe Lude.Text)
apvIntegerValue = Lens.lens (integerValue :: AssetPropertyVariant -> Lude.Maybe Lude.Text) (\s a -> s {integerValue = a} :: AssetPropertyVariant)
{-# DEPRECATED apvIntegerValue "Use generic-lens or generic-optics with 'integerValue' instead." #-}

-- | Optional. A string that contains the double value of the value entry. Accepts substitution templates.
--
-- /Note:/ Consider using 'doubleValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvDoubleValue :: Lens.Lens' AssetPropertyVariant (Lude.Maybe Lude.Text)
apvDoubleValue = Lens.lens (doubleValue :: AssetPropertyVariant -> Lude.Maybe Lude.Text) (\s a -> s {doubleValue = a} :: AssetPropertyVariant)
{-# DEPRECATED apvDoubleValue "Use generic-lens or generic-optics with 'doubleValue' instead." #-}

-- | Optional. The string value of the value entry. Accepts substitution templates.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvStringValue :: Lens.Lens' AssetPropertyVariant (Lude.Maybe Lude.Text)
apvStringValue = Lens.lens (stringValue :: AssetPropertyVariant -> Lude.Maybe Lude.Text) (\s a -> s {stringValue = a} :: AssetPropertyVariant)
{-# DEPRECATED apvStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

-- | Optional. A string that contains the boolean value (@true@ or @false@ ) of the value entry. Accepts substitution templates.
--
-- /Note:/ Consider using 'booleanValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvBooleanValue :: Lens.Lens' AssetPropertyVariant (Lude.Maybe Lude.Text)
apvBooleanValue = Lens.lens (booleanValue :: AssetPropertyVariant -> Lude.Maybe Lude.Text) (\s a -> s {booleanValue = a} :: AssetPropertyVariant)
{-# DEPRECATED apvBooleanValue "Use generic-lens or generic-optics with 'booleanValue' instead." #-}

instance Lude.FromJSON AssetPropertyVariant where
  parseJSON =
    Lude.withObject
      "AssetPropertyVariant"
      ( \x ->
          AssetPropertyVariant'
            Lude.<$> (x Lude..:? "integerValue")
            Lude.<*> (x Lude..:? "doubleValue")
            Lude.<*> (x Lude..:? "stringValue")
            Lude.<*> (x Lude..:? "booleanValue")
      )

instance Lude.ToJSON AssetPropertyVariant where
  toJSON AssetPropertyVariant' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("integerValue" Lude..=) Lude.<$> integerValue,
            ("doubleValue" Lude..=) Lude.<$> doubleValue,
            ("stringValue" Lude..=) Lude.<$> stringValue,
            ("booleanValue" Lude..=) Lude.<$> booleanValue
          ]
      )
