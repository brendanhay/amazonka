{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelector
  ( VideoSelector (..),

    -- * Smart constructor
    mkVideoSelector,

    -- * Lenses
    vsColorSpace,
    vsColorSpaceUsage,
    vsSelectorSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.VideoSelectorColorSpace as Types
import qualified Network.AWS.MediaLive.Types.VideoSelectorColorSpaceUsage as Types
import qualified Network.AWS.MediaLive.Types.VideoSelectorSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies a particular video stream within an input source. An input may have only a single video selector.
--
-- /See:/ 'mkVideoSelector' smart constructor.
data VideoSelector = VideoSelector'
  { -- | Specifies the color space of an input. This setting works in tandem with colorSpaceUsage and a video description's colorSpaceSettingsChoice to determine if any conversion will be performed.
    colorSpace :: Core.Maybe Types.VideoSelectorColorSpace,
    -- | Applies only if colorSpace is a value other than follow. This field controls how the value in the colorSpace field will be used. fallback means that when the input does include color space data, that data will be used, but when the input has no color space data, the value in colorSpace will be used. Choose fallback if your input is sometimes missing color space data, but when it does have color space data, that data is correct. force means to always use the value in colorSpace. Choose force if your input usually has no color space data or might have unreliable color space data.
    colorSpaceUsage :: Core.Maybe Types.VideoSelectorColorSpaceUsage,
    -- | The video selector settings.
    selectorSettings :: Core.Maybe Types.VideoSelectorSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VideoSelector' value with any optional fields omitted.
mkVideoSelector ::
  VideoSelector
mkVideoSelector =
  VideoSelector'
    { colorSpace = Core.Nothing,
      colorSpaceUsage = Core.Nothing,
      selectorSettings = Core.Nothing
    }

-- | Specifies the color space of an input. This setting works in tandem with colorSpaceUsage and a video description's colorSpaceSettingsChoice to determine if any conversion will be performed.
--
-- /Note:/ Consider using 'colorSpace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsColorSpace :: Lens.Lens' VideoSelector (Core.Maybe Types.VideoSelectorColorSpace)
vsColorSpace = Lens.field @"colorSpace"
{-# DEPRECATED vsColorSpace "Use generic-lens or generic-optics with 'colorSpace' instead." #-}

-- | Applies only if colorSpace is a value other than follow. This field controls how the value in the colorSpace field will be used. fallback means that when the input does include color space data, that data will be used, but when the input has no color space data, the value in colorSpace will be used. Choose fallback if your input is sometimes missing color space data, but when it does have color space data, that data is correct. force means to always use the value in colorSpace. Choose force if your input usually has no color space data or might have unreliable color space data.
--
-- /Note:/ Consider using 'colorSpaceUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsColorSpaceUsage :: Lens.Lens' VideoSelector (Core.Maybe Types.VideoSelectorColorSpaceUsage)
vsColorSpaceUsage = Lens.field @"colorSpaceUsage"
{-# DEPRECATED vsColorSpaceUsage "Use generic-lens or generic-optics with 'colorSpaceUsage' instead." #-}

-- | The video selector settings.
--
-- /Note:/ Consider using 'selectorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsSelectorSettings :: Lens.Lens' VideoSelector (Core.Maybe Types.VideoSelectorSettings)
vsSelectorSettings = Lens.field @"selectorSettings"
{-# DEPRECATED vsSelectorSettings "Use generic-lens or generic-optics with 'selectorSettings' instead." #-}

instance Core.FromJSON VideoSelector where
  toJSON VideoSelector {..} =
    Core.object
      ( Core.catMaybes
          [ ("colorSpace" Core..=) Core.<$> colorSpace,
            ("colorSpaceUsage" Core..=) Core.<$> colorSpaceUsage,
            ("selectorSettings" Core..=) Core.<$> selectorSettings
          ]
      )

instance Core.FromJSON VideoSelector where
  parseJSON =
    Core.withObject "VideoSelector" Core.$
      \x ->
        VideoSelector'
          Core.<$> (x Core..:? "colorSpace")
          Core.<*> (x Core..:? "colorSpaceUsage")
          Core.<*> (x Core..:? "selectorSettings")
