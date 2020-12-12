{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AssetPropertyValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AssetPropertyValue
  ( AssetPropertyValue (..),

    -- * Smart constructor
    mkAssetPropertyValue,

    -- * Lenses
    apvQuality,
    apvValue,
    apvTimestamp,
  )
where

import Network.AWS.IoT.Types.AssetPropertyTimestamp
import Network.AWS.IoT.Types.AssetPropertyVariant
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An asset property value entry containing the following information.
--
-- /See:/ 'mkAssetPropertyValue' smart constructor.
data AssetPropertyValue = AssetPropertyValue'
  { quality ::
      Lude.Maybe Lude.Text,
    value :: AssetPropertyVariant,
    timestamp :: AssetPropertyTimestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssetPropertyValue' with the minimum fields required to make a request.
--
-- * 'quality' - Optional. A string that describes the quality of the value. Accepts substitution templates. Must be @GOOD@ , @BAD@ , or @UNCERTAIN@ .
-- * 'timestamp' - The asset property value timestamp.
-- * 'value' - The value of the asset property.
mkAssetPropertyValue ::
  -- | 'value'
  AssetPropertyVariant ->
  -- | 'timestamp'
  AssetPropertyTimestamp ->
  AssetPropertyValue
mkAssetPropertyValue pValue_ pTimestamp_ =
  AssetPropertyValue'
    { quality = Lude.Nothing,
      value = pValue_,
      timestamp = pTimestamp_
    }

-- | Optional. A string that describes the quality of the value. Accepts substitution templates. Must be @GOOD@ , @BAD@ , or @UNCERTAIN@ .
--
-- /Note:/ Consider using 'quality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvQuality :: Lens.Lens' AssetPropertyValue (Lude.Maybe Lude.Text)
apvQuality = Lens.lens (quality :: AssetPropertyValue -> Lude.Maybe Lude.Text) (\s a -> s {quality = a} :: AssetPropertyValue)
{-# DEPRECATED apvQuality "Use generic-lens or generic-optics with 'quality' instead." #-}

-- | The value of the asset property.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvValue :: Lens.Lens' AssetPropertyValue AssetPropertyVariant
apvValue = Lens.lens (value :: AssetPropertyValue -> AssetPropertyVariant) (\s a -> s {value = a} :: AssetPropertyValue)
{-# DEPRECATED apvValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The asset property value timestamp.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvTimestamp :: Lens.Lens' AssetPropertyValue AssetPropertyTimestamp
apvTimestamp = Lens.lens (timestamp :: AssetPropertyValue -> AssetPropertyTimestamp) (\s a -> s {timestamp = a} :: AssetPropertyValue)
{-# DEPRECATED apvTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON AssetPropertyValue where
  parseJSON =
    Lude.withObject
      "AssetPropertyValue"
      ( \x ->
          AssetPropertyValue'
            Lude.<$> (x Lude..:? "quality")
            Lude.<*> (x Lude..: "value")
            Lude.<*> (x Lude..: "timestamp")
      )

instance Lude.ToJSON AssetPropertyValue where
  toJSON AssetPropertyValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("quality" Lude..=) Lude.<$> quality,
            Lude.Just ("value" Lude..= value),
            Lude.Just ("timestamp" Lude..= timestamp)
          ]
      )
