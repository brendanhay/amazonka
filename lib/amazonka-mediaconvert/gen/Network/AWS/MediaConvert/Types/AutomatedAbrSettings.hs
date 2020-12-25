{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AutomatedAbrSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AutomatedAbrSettings
  ( AutomatedAbrSettings (..),

    -- * Smart constructor
    mkAutomatedAbrSettings,

    -- * Lenses
    aasMaxAbrBitrate,
    aasMaxRenditions,
    aasMinAbrBitrate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Use automated ABR to have MediaConvert set up the renditions in your ABR package for you automatically, based on characteristics of your input video. This feature optimizes video quality while minimizing the overall size of your ABR package.
--
-- /See:/ 'mkAutomatedAbrSettings' smart constructor.
data AutomatedAbrSettings = AutomatedAbrSettings'
  { -- | Optional. The maximum target bit rate used in your automated ABR stack. Use this value to set an upper limit on the bandwidth consumed by the highest-quality rendition. This is the rendition that is delivered to viewers with the fastest internet connections. If you don't specify a value, MediaConvert uses 8,000,000 (8 mb/s) by default.
    maxAbrBitrate :: Core.Maybe Core.Natural,
    -- | Optional. The maximum number of renditions that MediaConvert will create in your automated ABR stack. The number of renditions is determined automatically, based on analysis of each job, but will never exceed this limit. When you set this to Auto in the console, which is equivalent to excluding it from your JSON job specification, MediaConvert defaults to a limit of 15.
    maxRenditions :: Core.Maybe Core.Natural,
    -- | Optional. The minimum target bitrate used in your automated ABR stack. Use this value to set a lower limit on the bitrate of video delivered to viewers with slow internet connections. If you don't specify a value, MediaConvert uses 600,000 (600 kb/s) by default.
    minAbrBitrate :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutomatedAbrSettings' value with any optional fields omitted.
mkAutomatedAbrSettings ::
  AutomatedAbrSettings
mkAutomatedAbrSettings =
  AutomatedAbrSettings'
    { maxAbrBitrate = Core.Nothing,
      maxRenditions = Core.Nothing,
      minAbrBitrate = Core.Nothing
    }

-- | Optional. The maximum target bit rate used in your automated ABR stack. Use this value to set an upper limit on the bandwidth consumed by the highest-quality rendition. This is the rendition that is delivered to viewers with the fastest internet connections. If you don't specify a value, MediaConvert uses 8,000,000 (8 mb/s) by default.
--
-- /Note:/ Consider using 'maxAbrBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasMaxAbrBitrate :: Lens.Lens' AutomatedAbrSettings (Core.Maybe Core.Natural)
aasMaxAbrBitrate = Lens.field @"maxAbrBitrate"
{-# DEPRECATED aasMaxAbrBitrate "Use generic-lens or generic-optics with 'maxAbrBitrate' instead." #-}

-- | Optional. The maximum number of renditions that MediaConvert will create in your automated ABR stack. The number of renditions is determined automatically, based on analysis of each job, but will never exceed this limit. When you set this to Auto in the console, which is equivalent to excluding it from your JSON job specification, MediaConvert defaults to a limit of 15.
--
-- /Note:/ Consider using 'maxRenditions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasMaxRenditions :: Lens.Lens' AutomatedAbrSettings (Core.Maybe Core.Natural)
aasMaxRenditions = Lens.field @"maxRenditions"
{-# DEPRECATED aasMaxRenditions "Use generic-lens or generic-optics with 'maxRenditions' instead." #-}

-- | Optional. The minimum target bitrate used in your automated ABR stack. Use this value to set a lower limit on the bitrate of video delivered to viewers with slow internet connections. If you don't specify a value, MediaConvert uses 600,000 (600 kb/s) by default.
--
-- /Note:/ Consider using 'minAbrBitrate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aasMinAbrBitrate :: Lens.Lens' AutomatedAbrSettings (Core.Maybe Core.Natural)
aasMinAbrBitrate = Lens.field @"minAbrBitrate"
{-# DEPRECATED aasMinAbrBitrate "Use generic-lens or generic-optics with 'minAbrBitrate' instead." #-}

instance Core.FromJSON AutomatedAbrSettings where
  toJSON AutomatedAbrSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("maxAbrBitrate" Core..=) Core.<$> maxAbrBitrate,
            ("maxRenditions" Core..=) Core.<$> maxRenditions,
            ("minAbrBitrate" Core..=) Core.<$> minAbrBitrate
          ]
      )

instance Core.FromJSON AutomatedAbrSettings where
  parseJSON =
    Core.withObject "AutomatedAbrSettings" Core.$
      \x ->
        AutomatedAbrSettings'
          Core.<$> (x Core..:? "maxAbrBitrate")
          Core.<*> (x Core..:? "maxRenditions")
          Core.<*> (x Core..:? "minAbrBitrate")
