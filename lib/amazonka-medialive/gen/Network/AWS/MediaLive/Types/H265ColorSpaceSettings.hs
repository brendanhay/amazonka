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
    hColorSpacePassthroughSettings,
    hHdr10Settings,
    hRec601Settings,
    hRec709Settings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings as Types
import qualified Network.AWS.MediaLive.Types.Hdr10Settings as Types
import qualified Network.AWS.MediaLive.Types.Rec601Settings as Types
import qualified Network.AWS.MediaLive.Types.Rec709Settings as Types
import qualified Network.AWS.Prelude as Core

-- | H265 Color Space Settings
--
-- /See:/ 'mkH265ColorSpaceSettings' smart constructor.
data H265ColorSpaceSettings = H265ColorSpaceSettings'
  { colorSpacePassthroughSettings :: Core.Maybe Types.ColorSpacePassthroughSettings,
    hdr10Settings :: Core.Maybe Types.Hdr10Settings,
    rec601Settings :: Core.Maybe Types.Rec601Settings,
    rec709Settings :: Core.Maybe Types.Rec709Settings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'H265ColorSpaceSettings' value with any optional fields omitted.
mkH265ColorSpaceSettings ::
  H265ColorSpaceSettings
mkH265ColorSpaceSettings =
  H265ColorSpaceSettings'
    { colorSpacePassthroughSettings =
        Core.Nothing,
      hdr10Settings = Core.Nothing,
      rec601Settings = Core.Nothing,
      rec709Settings = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'colorSpacePassthroughSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hColorSpacePassthroughSettings :: Lens.Lens' H265ColorSpaceSettings (Core.Maybe Types.ColorSpacePassthroughSettings)
hColorSpacePassthroughSettings = Lens.field @"colorSpacePassthroughSettings"
{-# DEPRECATED hColorSpacePassthroughSettings "Use generic-lens or generic-optics with 'colorSpacePassthroughSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hdr10Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hHdr10Settings :: Lens.Lens' H265ColorSpaceSettings (Core.Maybe Types.Hdr10Settings)
hHdr10Settings = Lens.field @"hdr10Settings"
{-# DEPRECATED hHdr10Settings "Use generic-lens or generic-optics with 'hdr10Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rec601Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hRec601Settings :: Lens.Lens' H265ColorSpaceSettings (Core.Maybe Types.Rec601Settings)
hRec601Settings = Lens.field @"rec601Settings"
{-# DEPRECATED hRec601Settings "Use generic-lens or generic-optics with 'rec601Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rec709Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hRec709Settings :: Lens.Lens' H265ColorSpaceSettings (Core.Maybe Types.Rec709Settings)
hRec709Settings = Lens.field @"rec709Settings"
{-# DEPRECATED hRec709Settings "Use generic-lens or generic-optics with 'rec709Settings' instead." #-}

instance Core.FromJSON H265ColorSpaceSettings where
  toJSON H265ColorSpaceSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("colorSpacePassthroughSettings" Core..=)
              Core.<$> colorSpacePassthroughSettings,
            ("hdr10Settings" Core..=) Core.<$> hdr10Settings,
            ("rec601Settings" Core..=) Core.<$> rec601Settings,
            ("rec709Settings" Core..=) Core.<$> rec709Settings
          ]
      )

instance Core.FromJSON H265ColorSpaceSettings where
  parseJSON =
    Core.withObject "H265ColorSpaceSettings" Core.$
      \x ->
        H265ColorSpaceSettings'
          Core.<$> (x Core..:? "colorSpacePassthroughSettings")
          Core.<*> (x Core..:? "hdr10Settings")
          Core.<*> (x Core..:? "rec601Settings")
          Core.<*> (x Core..:? "rec709Settings")
