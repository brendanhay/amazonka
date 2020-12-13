{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ColorSpaceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264ColorSpaceSettings
  ( H264ColorSpaceSettings (..),

    -- * Smart constructor
    mkH264ColorSpaceSettings,

    -- * Lenses
    hRec709Settings,
    hRec601Settings,
    hColorSpacePassthroughSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings
import Network.AWS.MediaLive.Types.Rec601Settings
import Network.AWS.MediaLive.Types.Rec709Settings
import qualified Network.AWS.Prelude as Lude

-- | H264 Color Space Settings
--
-- /See:/ 'mkH264ColorSpaceSettings' smart constructor.
data H264ColorSpaceSettings = H264ColorSpaceSettings'
  { rec709Settings :: Lude.Maybe Rec709Settings,
    rec601Settings :: Lude.Maybe Rec601Settings,
    colorSpacePassthroughSettings :: Lude.Maybe ColorSpacePassthroughSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'H264ColorSpaceSettings' with the minimum fields required to make a request.
--
-- * 'rec709Settings' -
-- * 'rec601Settings' -
-- * 'colorSpacePassthroughSettings' -
mkH264ColorSpaceSettings ::
  H264ColorSpaceSettings
mkH264ColorSpaceSettings =
  H264ColorSpaceSettings'
    { rec709Settings = Lude.Nothing,
      rec601Settings = Lude.Nothing,
      colorSpacePassthroughSettings = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'rec709Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hRec709Settings :: Lens.Lens' H264ColorSpaceSettings (Lude.Maybe Rec709Settings)
hRec709Settings = Lens.lens (rec709Settings :: H264ColorSpaceSettings -> Lude.Maybe Rec709Settings) (\s a -> s {rec709Settings = a} :: H264ColorSpaceSettings)
{-# DEPRECATED hRec709Settings "Use generic-lens or generic-optics with 'rec709Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rec601Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hRec601Settings :: Lens.Lens' H264ColorSpaceSettings (Lude.Maybe Rec601Settings)
hRec601Settings = Lens.lens (rec601Settings :: H264ColorSpaceSettings -> Lude.Maybe Rec601Settings) (\s a -> s {rec601Settings = a} :: H264ColorSpaceSettings)
{-# DEPRECATED hRec601Settings "Use generic-lens or generic-optics with 'rec601Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'colorSpacePassthroughSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hColorSpacePassthroughSettings :: Lens.Lens' H264ColorSpaceSettings (Lude.Maybe ColorSpacePassthroughSettings)
hColorSpacePassthroughSettings = Lens.lens (colorSpacePassthroughSettings :: H264ColorSpaceSettings -> Lude.Maybe ColorSpacePassthroughSettings) (\s a -> s {colorSpacePassthroughSettings = a} :: H264ColorSpaceSettings)
{-# DEPRECATED hColorSpacePassthroughSettings "Use generic-lens or generic-optics with 'colorSpacePassthroughSettings' instead." #-}

instance Lude.FromJSON H264ColorSpaceSettings where
  parseJSON =
    Lude.withObject
      "H264ColorSpaceSettings"
      ( \x ->
          H264ColorSpaceSettings'
            Lude.<$> (x Lude..:? "rec709Settings")
            Lude.<*> (x Lude..:? "rec601Settings")
            Lude.<*> (x Lude..:? "colorSpacePassthroughSettings")
      )

instance Lude.ToJSON H264ColorSpaceSettings where
  toJSON H264ColorSpaceSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("rec709Settings" Lude..=) Lude.<$> rec709Settings,
            ("rec601Settings" Lude..=) Lude.<$> rec601Settings,
            ("colorSpacePassthroughSettings" Lude..=)
              Lude.<$> colorSpacePassthroughSettings
          ]
      )
