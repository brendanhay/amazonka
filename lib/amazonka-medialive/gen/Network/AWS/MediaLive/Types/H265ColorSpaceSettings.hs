{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265ColorSpaceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265ColorSpaceSettings
  ( H265ColorSpaceSettings (..),

    -- * Smart constructor
    mkH265ColorSpaceSettings,

    -- * Lenses
    hcssHdr10Settings,
    hcssRec709Settings,
    hcssRec601Settings,
    hcssColorSpacePassthroughSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings
import Network.AWS.MediaLive.Types.Hdr10Settings
import Network.AWS.MediaLive.Types.Rec601Settings
import Network.AWS.MediaLive.Types.Rec709Settings
import qualified Network.AWS.Prelude as Lude

-- | H265 Color Space Settings
--
-- /See:/ 'mkH265ColorSpaceSettings' smart constructor.
data H265ColorSpaceSettings = H265ColorSpaceSettings'
  { hdr10Settings :: Lude.Maybe Hdr10Settings,
    rec709Settings :: Lude.Maybe Rec709Settings,
    rec601Settings :: Lude.Maybe Rec601Settings,
    colorSpacePassthroughSettings :: Lude.Maybe ColorSpacePassthroughSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'H265ColorSpaceSettings' with the minimum fields required to make a request.
--
-- * 'hdr10Settings' -
-- * 'rec709Settings' -
-- * 'rec601Settings' -
-- * 'colorSpacePassthroughSettings' -
mkH265ColorSpaceSettings ::
  H265ColorSpaceSettings
mkH265ColorSpaceSettings =
  H265ColorSpaceSettings'
    { hdr10Settings = Lude.Nothing,
      rec709Settings = Lude.Nothing,
      rec601Settings = Lude.Nothing,
      colorSpacePassthroughSettings = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'hdr10Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcssHdr10Settings :: Lens.Lens' H265ColorSpaceSettings (Lude.Maybe Hdr10Settings)
hcssHdr10Settings = Lens.lens (hdr10Settings :: H265ColorSpaceSettings -> Lude.Maybe Hdr10Settings) (\s a -> s {hdr10Settings = a} :: H265ColorSpaceSettings)
{-# DEPRECATED hcssHdr10Settings "Use generic-lens or generic-optics with 'hdr10Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rec709Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcssRec709Settings :: Lens.Lens' H265ColorSpaceSettings (Lude.Maybe Rec709Settings)
hcssRec709Settings = Lens.lens (rec709Settings :: H265ColorSpaceSettings -> Lude.Maybe Rec709Settings) (\s a -> s {rec709Settings = a} :: H265ColorSpaceSettings)
{-# DEPRECATED hcssRec709Settings "Use generic-lens or generic-optics with 'rec709Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rec601Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcssRec601Settings :: Lens.Lens' H265ColorSpaceSettings (Lude.Maybe Rec601Settings)
hcssRec601Settings = Lens.lens (rec601Settings :: H265ColorSpaceSettings -> Lude.Maybe Rec601Settings) (\s a -> s {rec601Settings = a} :: H265ColorSpaceSettings)
{-# DEPRECATED hcssRec601Settings "Use generic-lens or generic-optics with 'rec601Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'colorSpacePassthroughSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcssColorSpacePassthroughSettings :: Lens.Lens' H265ColorSpaceSettings (Lude.Maybe ColorSpacePassthroughSettings)
hcssColorSpacePassthroughSettings = Lens.lens (colorSpacePassthroughSettings :: H265ColorSpaceSettings -> Lude.Maybe ColorSpacePassthroughSettings) (\s a -> s {colorSpacePassthroughSettings = a} :: H265ColorSpaceSettings)
{-# DEPRECATED hcssColorSpacePassthroughSettings "Use generic-lens or generic-optics with 'colorSpacePassthroughSettings' instead." #-}

instance Lude.FromJSON H265ColorSpaceSettings where
  parseJSON =
    Lude.withObject
      "H265ColorSpaceSettings"
      ( \x ->
          H265ColorSpaceSettings'
            Lude.<$> (x Lude..:? "hdr10Settings")
            Lude.<*> (x Lude..:? "rec709Settings")
            Lude.<*> (x Lude..:? "rec601Settings")
            Lude.<*> (x Lude..:? "colorSpacePassthroughSettings")
      )

instance Lude.ToJSON H265ColorSpaceSettings where
  toJSON H265ColorSpaceSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("hdr10Settings" Lude..=) Lude.<$> hdr10Settings,
            ("rec709Settings" Lude..=) Lude.<$> rec709Settings,
            ("rec601Settings" Lude..=) Lude.<$> rec601Settings,
            ("colorSpacePassthroughSettings" Lude..=)
              Lude.<$> colorSpacePassthroughSettings
          ]
      )
