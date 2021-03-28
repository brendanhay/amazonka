{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.PresetSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.PresetSettings
  ( PresetSettings (..)
  -- * Smart constructor
  , mkPresetSettings
  -- * Lenses
  , psAudioDescriptions
  , psCaptionDescriptions
  , psContainerSettings
  , psVideoDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AudioDescription as Types
import qualified Network.AWS.MediaConvert.Types.CaptionDescriptionPreset as Types
import qualified Network.AWS.MediaConvert.Types.ContainerSettings as Types
import qualified Network.AWS.MediaConvert.Types.VideoDescription as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for preset
--
-- /See:/ 'mkPresetSettings' smart constructor.
data PresetSettings = PresetSettings'
  { audioDescriptions :: Core.Maybe [Types.AudioDescription]
    -- ^ (AudioDescriptions) contains groups of audio encoding settings organized by audio codec. Include one instance of (AudioDescriptions) per output. (AudioDescriptions) can contain multiple groups of encoding settings.
  , captionDescriptions :: Core.Maybe [Types.CaptionDescriptionPreset]
    -- ^ Caption settings for this preset. There can be multiple caption settings in a single output.
  , containerSettings :: Core.Maybe Types.ContainerSettings
    -- ^ Container specific settings.
  , videoDescription :: Core.Maybe Types.VideoDescription
    -- ^ (VideoDescription) contains a group of video encoding settings. The specific video settings depend on the video codec that you choose when you specify a value for Video codec (codec). Include one instance of (VideoDescription) per output.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PresetSettings' value with any optional fields omitted.
mkPresetSettings
    :: PresetSettings
mkPresetSettings
  = PresetSettings'{audioDescriptions = Core.Nothing,
                    captionDescriptions = Core.Nothing,
                    containerSettings = Core.Nothing, videoDescription = Core.Nothing}

-- | (AudioDescriptions) contains groups of audio encoding settings organized by audio codec. Include one instance of (AudioDescriptions) per output. (AudioDescriptions) can contain multiple groups of encoding settings.
--
-- /Note:/ Consider using 'audioDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psAudioDescriptions :: Lens.Lens' PresetSettings (Core.Maybe [Types.AudioDescription])
psAudioDescriptions = Lens.field @"audioDescriptions"
{-# INLINEABLE psAudioDescriptions #-}
{-# DEPRECATED audioDescriptions "Use generic-lens or generic-optics with 'audioDescriptions' instead"  #-}

-- | Caption settings for this preset. There can be multiple caption settings in a single output.
--
-- /Note:/ Consider using 'captionDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psCaptionDescriptions :: Lens.Lens' PresetSettings (Core.Maybe [Types.CaptionDescriptionPreset])
psCaptionDescriptions = Lens.field @"captionDescriptions"
{-# INLINEABLE psCaptionDescriptions #-}
{-# DEPRECATED captionDescriptions "Use generic-lens or generic-optics with 'captionDescriptions' instead"  #-}

-- | Container specific settings.
--
-- /Note:/ Consider using 'containerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psContainerSettings :: Lens.Lens' PresetSettings (Core.Maybe Types.ContainerSettings)
psContainerSettings = Lens.field @"containerSettings"
{-# INLINEABLE psContainerSettings #-}
{-# DEPRECATED containerSettings "Use generic-lens or generic-optics with 'containerSettings' instead"  #-}

-- | (VideoDescription) contains a group of video encoding settings. The specific video settings depend on the video codec that you choose when you specify a value for Video codec (codec). Include one instance of (VideoDescription) per output.
--
-- /Note:/ Consider using 'videoDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psVideoDescription :: Lens.Lens' PresetSettings (Core.Maybe Types.VideoDescription)
psVideoDescription = Lens.field @"videoDescription"
{-# INLINEABLE psVideoDescription #-}
{-# DEPRECATED videoDescription "Use generic-lens or generic-optics with 'videoDescription' instead"  #-}

instance Core.FromJSON PresetSettings where
        toJSON PresetSettings{..}
          = Core.object
              (Core.catMaybes
                 [("audioDescriptions" Core..=) Core.<$> audioDescriptions,
                  ("captionDescriptions" Core..=) Core.<$> captionDescriptions,
                  ("containerSettings" Core..=) Core.<$> containerSettings,
                  ("videoDescription" Core..=) Core.<$> videoDescription])

instance Core.FromJSON PresetSettings where
        parseJSON
          = Core.withObject "PresetSettings" Core.$
              \ x ->
                PresetSettings' Core.<$>
                  (x Core..:? "audioDescriptions") Core.<*>
                    x Core..:? "captionDescriptions"
                    Core.<*> x Core..:? "containerSettings"
                    Core.<*> x Core..:? "videoDescription"
