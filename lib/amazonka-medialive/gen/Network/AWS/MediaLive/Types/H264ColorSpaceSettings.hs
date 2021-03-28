{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ColorSpaceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264ColorSpaceSettings
  ( H264ColorSpaceSettings (..)
  -- * Smart constructor
  , mkH264ColorSpaceSettings
  -- * Lenses
  , hcssColorSpacePassthroughSettings
  , hcssRec601Settings
  , hcssRec709Settings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings as Types
import qualified Network.AWS.MediaLive.Types.Rec601Settings as Types
import qualified Network.AWS.MediaLive.Types.Rec709Settings as Types
import qualified Network.AWS.Prelude as Core

-- | H264 Color Space Settings
--
-- /See:/ 'mkH264ColorSpaceSettings' smart constructor.
data H264ColorSpaceSettings = H264ColorSpaceSettings'
  { colorSpacePassthroughSettings :: Core.Maybe Types.ColorSpacePassthroughSettings
  , rec601Settings :: Core.Maybe Types.Rec601Settings
  , rec709Settings :: Core.Maybe Types.Rec709Settings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'H264ColorSpaceSettings' value with any optional fields omitted.
mkH264ColorSpaceSettings
    :: H264ColorSpaceSettings
mkH264ColorSpaceSettings
  = H264ColorSpaceSettings'{colorSpacePassthroughSettings =
                              Core.Nothing,
                            rec601Settings = Core.Nothing, rec709Settings = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'colorSpacePassthroughSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcssColorSpacePassthroughSettings :: Lens.Lens' H264ColorSpaceSettings (Core.Maybe Types.ColorSpacePassthroughSettings)
hcssColorSpacePassthroughSettings = Lens.field @"colorSpacePassthroughSettings"
{-# INLINEABLE hcssColorSpacePassthroughSettings #-}
{-# DEPRECATED colorSpacePassthroughSettings "Use generic-lens or generic-optics with 'colorSpacePassthroughSettings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rec601Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcssRec601Settings :: Lens.Lens' H264ColorSpaceSettings (Core.Maybe Types.Rec601Settings)
hcssRec601Settings = Lens.field @"rec601Settings"
{-# INLINEABLE hcssRec601Settings #-}
{-# DEPRECATED rec601Settings "Use generic-lens or generic-optics with 'rec601Settings' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rec709Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcssRec709Settings :: Lens.Lens' H264ColorSpaceSettings (Core.Maybe Types.Rec709Settings)
hcssRec709Settings = Lens.field @"rec709Settings"
{-# INLINEABLE hcssRec709Settings #-}
{-# DEPRECATED rec709Settings "Use generic-lens or generic-optics with 'rec709Settings' instead"  #-}

instance Core.FromJSON H264ColorSpaceSettings where
        toJSON H264ColorSpaceSettings{..}
          = Core.object
              (Core.catMaybes
                 [("colorSpacePassthroughSettings" Core..=) Core.<$>
                    colorSpacePassthroughSettings,
                  ("rec601Settings" Core..=) Core.<$> rec601Settings,
                  ("rec709Settings" Core..=) Core.<$> rec709Settings])

instance Core.FromJSON H264ColorSpaceSettings where
        parseJSON
          = Core.withObject "H264ColorSpaceSettings" Core.$
              \ x ->
                H264ColorSpaceSettings' Core.<$>
                  (x Core..:? "colorSpacePassthroughSettings") Core.<*>
                    x Core..:? "rec601Settings"
                    Core.<*> x Core..:? "rec709Settings"
