{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EncoderSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EncoderSettings
  ( EncoderSettings (..),

    -- * Smart constructor
    mkEncoderSettings,

    -- * Lenses
    esVideoDescriptions,
    esAudioDescriptions,
    esOutputGroups,
    esTimecodeConfig,
    esAvailBlanking,
    esAvailConfiguration,
    esBlackoutSlate,
    esCaptionDescriptions,
    esFeatureActivations,
    esGlobalConfiguration,
    esNielsenConfiguration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AudioDescription as Types
import qualified Network.AWS.MediaLive.Types.AvailBlanking as Types
import qualified Network.AWS.MediaLive.Types.AvailConfiguration as Types
import qualified Network.AWS.MediaLive.Types.BlackoutSlate as Types
import qualified Network.AWS.MediaLive.Types.CaptionDescription as Types
import qualified Network.AWS.MediaLive.Types.FeatureActivations as Types
import qualified Network.AWS.MediaLive.Types.GlobalConfiguration as Types
import qualified Network.AWS.MediaLive.Types.NielsenConfiguration as Types
import qualified Network.AWS.MediaLive.Types.OutputGroup as Types
import qualified Network.AWS.MediaLive.Types.TimecodeConfig as Types
import qualified Network.AWS.MediaLive.Types.VideoDescription as Types
import qualified Network.AWS.Prelude as Core

-- | Encoder Settings
--
-- /See:/ 'mkEncoderSettings' smart constructor.
data EncoderSettings = EncoderSettings'
  { videoDescriptions :: [Types.VideoDescription],
    audioDescriptions :: [Types.AudioDescription],
    outputGroups :: [Types.OutputGroup],
    -- | Contains settings used to acquire and adjust timecode information from inputs.
    timecodeConfig :: Types.TimecodeConfig,
    -- | Settings for ad avail blanking.
    availBlanking :: Core.Maybe Types.AvailBlanking,
    -- | Event-wide configuration settings for ad avail insertion.
    availConfiguration :: Core.Maybe Types.AvailConfiguration,
    -- | Settings for blackout slate.
    blackoutSlate :: Core.Maybe Types.BlackoutSlate,
    -- | Settings for caption decriptions
    captionDescriptions :: Core.Maybe [Types.CaptionDescription],
    -- | Feature Activations
    featureActivations :: Core.Maybe Types.FeatureActivations,
    -- | Configuration settings that apply to the event as a whole.
    globalConfiguration :: Core.Maybe Types.GlobalConfiguration,
    -- | Nielsen configuration settings.
    nielsenConfiguration :: Core.Maybe Types.NielsenConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncoderSettings' value with any optional fields omitted.
mkEncoderSettings ::
  -- | 'timecodeConfig'
  Types.TimecodeConfig ->
  EncoderSettings
mkEncoderSettings timecodeConfig =
  EncoderSettings'
    { videoDescriptions = Core.mempty,
      audioDescriptions = Core.mempty,
      outputGroups = Core.mempty,
      timecodeConfig,
      availBlanking = Core.Nothing,
      availConfiguration = Core.Nothing,
      blackoutSlate = Core.Nothing,
      captionDescriptions = Core.Nothing,
      featureActivations = Core.Nothing,
      globalConfiguration = Core.Nothing,
      nielsenConfiguration = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'videoDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esVideoDescriptions :: Lens.Lens' EncoderSettings [Types.VideoDescription]
esVideoDescriptions = Lens.field @"videoDescriptions"
{-# DEPRECATED esVideoDescriptions "Use generic-lens or generic-optics with 'videoDescriptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'audioDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esAudioDescriptions :: Lens.Lens' EncoderSettings [Types.AudioDescription]
esAudioDescriptions = Lens.field @"audioDescriptions"
{-# DEPRECATED esAudioDescriptions "Use generic-lens or generic-optics with 'audioDescriptions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'outputGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esOutputGroups :: Lens.Lens' EncoderSettings [Types.OutputGroup]
esOutputGroups = Lens.field @"outputGroups"
{-# DEPRECATED esOutputGroups "Use generic-lens or generic-optics with 'outputGroups' instead." #-}

-- | Contains settings used to acquire and adjust timecode information from inputs.
--
-- /Note:/ Consider using 'timecodeConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esTimecodeConfig :: Lens.Lens' EncoderSettings Types.TimecodeConfig
esTimecodeConfig = Lens.field @"timecodeConfig"
{-# DEPRECATED esTimecodeConfig "Use generic-lens or generic-optics with 'timecodeConfig' instead." #-}

-- | Settings for ad avail blanking.
--
-- /Note:/ Consider using 'availBlanking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esAvailBlanking :: Lens.Lens' EncoderSettings (Core.Maybe Types.AvailBlanking)
esAvailBlanking = Lens.field @"availBlanking"
{-# DEPRECATED esAvailBlanking "Use generic-lens or generic-optics with 'availBlanking' instead." #-}

-- | Event-wide configuration settings for ad avail insertion.
--
-- /Note:/ Consider using 'availConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esAvailConfiguration :: Lens.Lens' EncoderSettings (Core.Maybe Types.AvailConfiguration)
esAvailConfiguration = Lens.field @"availConfiguration"
{-# DEPRECATED esAvailConfiguration "Use generic-lens or generic-optics with 'availConfiguration' instead." #-}

-- | Settings for blackout slate.
--
-- /Note:/ Consider using 'blackoutSlate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esBlackoutSlate :: Lens.Lens' EncoderSettings (Core.Maybe Types.BlackoutSlate)
esBlackoutSlate = Lens.field @"blackoutSlate"
{-# DEPRECATED esBlackoutSlate "Use generic-lens or generic-optics with 'blackoutSlate' instead." #-}

-- | Settings for caption decriptions
--
-- /Note:/ Consider using 'captionDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esCaptionDescriptions :: Lens.Lens' EncoderSettings (Core.Maybe [Types.CaptionDescription])
esCaptionDescriptions = Lens.field @"captionDescriptions"
{-# DEPRECATED esCaptionDescriptions "Use generic-lens or generic-optics with 'captionDescriptions' instead." #-}

-- | Feature Activations
--
-- /Note:/ Consider using 'featureActivations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esFeatureActivations :: Lens.Lens' EncoderSettings (Core.Maybe Types.FeatureActivations)
esFeatureActivations = Lens.field @"featureActivations"
{-# DEPRECATED esFeatureActivations "Use generic-lens or generic-optics with 'featureActivations' instead." #-}

-- | Configuration settings that apply to the event as a whole.
--
-- /Note:/ Consider using 'globalConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esGlobalConfiguration :: Lens.Lens' EncoderSettings (Core.Maybe Types.GlobalConfiguration)
esGlobalConfiguration = Lens.field @"globalConfiguration"
{-# DEPRECATED esGlobalConfiguration "Use generic-lens or generic-optics with 'globalConfiguration' instead." #-}

-- | Nielsen configuration settings.
--
-- /Note:/ Consider using 'nielsenConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esNielsenConfiguration :: Lens.Lens' EncoderSettings (Core.Maybe Types.NielsenConfiguration)
esNielsenConfiguration = Lens.field @"nielsenConfiguration"
{-# DEPRECATED esNielsenConfiguration "Use generic-lens or generic-optics with 'nielsenConfiguration' instead." #-}

instance Core.FromJSON EncoderSettings where
  toJSON EncoderSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("videoDescriptions" Core..= videoDescriptions),
            Core.Just ("audioDescriptions" Core..= audioDescriptions),
            Core.Just ("outputGroups" Core..= outputGroups),
            Core.Just ("timecodeConfig" Core..= timecodeConfig),
            ("availBlanking" Core..=) Core.<$> availBlanking,
            ("availConfiguration" Core..=) Core.<$> availConfiguration,
            ("blackoutSlate" Core..=) Core.<$> blackoutSlate,
            ("captionDescriptions" Core..=) Core.<$> captionDescriptions,
            ("featureActivations" Core..=) Core.<$> featureActivations,
            ("globalConfiguration" Core..=) Core.<$> globalConfiguration,
            ("nielsenConfiguration" Core..=) Core.<$> nielsenConfiguration
          ]
      )

instance Core.FromJSON EncoderSettings where
  parseJSON =
    Core.withObject "EncoderSettings" Core.$
      \x ->
        EncoderSettings'
          Core.<$> (x Core..:? "videoDescriptions" Core..!= Core.mempty)
          Core.<*> (x Core..:? "audioDescriptions" Core..!= Core.mempty)
          Core.<*> (x Core..:? "outputGroups" Core..!= Core.mempty)
          Core.<*> (x Core..: "timecodeConfig")
          Core.<*> (x Core..:? "availBlanking")
          Core.<*> (x Core..:? "availConfiguration")
          Core.<*> (x Core..:? "blackoutSlate")
          Core.<*> (x Core..:? "captionDescriptions")
          Core.<*> (x Core..:? "featureActivations")
          Core.<*> (x Core..:? "globalConfiguration")
          Core.<*> (x Core..:? "nielsenConfiguration")
