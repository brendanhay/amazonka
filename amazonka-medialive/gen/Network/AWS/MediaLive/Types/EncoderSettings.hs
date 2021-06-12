{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EncoderSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EncoderSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioDescription
import Network.AWS.MediaLive.Types.AvailBlanking
import Network.AWS.MediaLive.Types.AvailConfiguration
import Network.AWS.MediaLive.Types.BlackoutSlate
import Network.AWS.MediaLive.Types.CaptionDescription
import Network.AWS.MediaLive.Types.FeatureActivations
import Network.AWS.MediaLive.Types.GlobalConfiguration
import Network.AWS.MediaLive.Types.NielsenConfiguration
import Network.AWS.MediaLive.Types.OutputGroup
import Network.AWS.MediaLive.Types.TimecodeConfig
import Network.AWS.MediaLive.Types.VideoDescription

-- | Encoder Settings
--
-- /See:/ 'newEncoderSettings' smart constructor.
data EncoderSettings = EncoderSettings'
  { -- | Configuration settings that apply to the event as a whole.
    globalConfiguration :: Core.Maybe GlobalConfiguration,
    -- | Feature Activations
    featureActivations :: Core.Maybe FeatureActivations,
    -- | Event-wide configuration settings for ad avail insertion.
    availConfiguration :: Core.Maybe AvailConfiguration,
    -- | Settings for ad avail blanking.
    availBlanking :: Core.Maybe AvailBlanking,
    -- | Nielsen configuration settings.
    nielsenConfiguration :: Core.Maybe NielsenConfiguration,
    -- | Settings for blackout slate.
    blackoutSlate :: Core.Maybe BlackoutSlate,
    -- | Settings for caption decriptions
    captionDescriptions :: Core.Maybe [CaptionDescription],
    videoDescriptions :: [VideoDescription],
    audioDescriptions :: [AudioDescription],
    outputGroups :: [OutputGroup],
    -- | Contains settings used to acquire and adjust timecode information from
    -- inputs.
    timecodeConfig :: TimecodeConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EncoderSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalConfiguration', 'encoderSettings_globalConfiguration' - Configuration settings that apply to the event as a whole.
--
-- 'featureActivations', 'encoderSettings_featureActivations' - Feature Activations
--
-- 'availConfiguration', 'encoderSettings_availConfiguration' - Event-wide configuration settings for ad avail insertion.
--
-- 'availBlanking', 'encoderSettings_availBlanking' - Settings for ad avail blanking.
--
-- 'nielsenConfiguration', 'encoderSettings_nielsenConfiguration' - Nielsen configuration settings.
--
-- 'blackoutSlate', 'encoderSettings_blackoutSlate' - Settings for blackout slate.
--
-- 'captionDescriptions', 'encoderSettings_captionDescriptions' - Settings for caption decriptions
--
-- 'videoDescriptions', 'encoderSettings_videoDescriptions' - Undocumented member.
--
-- 'audioDescriptions', 'encoderSettings_audioDescriptions' - Undocumented member.
--
-- 'outputGroups', 'encoderSettings_outputGroups' - Undocumented member.
--
-- 'timecodeConfig', 'encoderSettings_timecodeConfig' - Contains settings used to acquire and adjust timecode information from
-- inputs.
newEncoderSettings ::
  -- | 'timecodeConfig'
  TimecodeConfig ->
  EncoderSettings
newEncoderSettings pTimecodeConfig_ =
  EncoderSettings'
    { globalConfiguration =
        Core.Nothing,
      featureActivations = Core.Nothing,
      availConfiguration = Core.Nothing,
      availBlanking = Core.Nothing,
      nielsenConfiguration = Core.Nothing,
      blackoutSlate = Core.Nothing,
      captionDescriptions = Core.Nothing,
      videoDescriptions = Core.mempty,
      audioDescriptions = Core.mempty,
      outputGroups = Core.mempty,
      timecodeConfig = pTimecodeConfig_
    }

-- | Configuration settings that apply to the event as a whole.
encoderSettings_globalConfiguration :: Lens.Lens' EncoderSettings (Core.Maybe GlobalConfiguration)
encoderSettings_globalConfiguration = Lens.lens (\EncoderSettings' {globalConfiguration} -> globalConfiguration) (\s@EncoderSettings' {} a -> s {globalConfiguration = a} :: EncoderSettings)

-- | Feature Activations
encoderSettings_featureActivations :: Lens.Lens' EncoderSettings (Core.Maybe FeatureActivations)
encoderSettings_featureActivations = Lens.lens (\EncoderSettings' {featureActivations} -> featureActivations) (\s@EncoderSettings' {} a -> s {featureActivations = a} :: EncoderSettings)

-- | Event-wide configuration settings for ad avail insertion.
encoderSettings_availConfiguration :: Lens.Lens' EncoderSettings (Core.Maybe AvailConfiguration)
encoderSettings_availConfiguration = Lens.lens (\EncoderSettings' {availConfiguration} -> availConfiguration) (\s@EncoderSettings' {} a -> s {availConfiguration = a} :: EncoderSettings)

-- | Settings for ad avail blanking.
encoderSettings_availBlanking :: Lens.Lens' EncoderSettings (Core.Maybe AvailBlanking)
encoderSettings_availBlanking = Lens.lens (\EncoderSettings' {availBlanking} -> availBlanking) (\s@EncoderSettings' {} a -> s {availBlanking = a} :: EncoderSettings)

-- | Nielsen configuration settings.
encoderSettings_nielsenConfiguration :: Lens.Lens' EncoderSettings (Core.Maybe NielsenConfiguration)
encoderSettings_nielsenConfiguration = Lens.lens (\EncoderSettings' {nielsenConfiguration} -> nielsenConfiguration) (\s@EncoderSettings' {} a -> s {nielsenConfiguration = a} :: EncoderSettings)

-- | Settings for blackout slate.
encoderSettings_blackoutSlate :: Lens.Lens' EncoderSettings (Core.Maybe BlackoutSlate)
encoderSettings_blackoutSlate = Lens.lens (\EncoderSettings' {blackoutSlate} -> blackoutSlate) (\s@EncoderSettings' {} a -> s {blackoutSlate = a} :: EncoderSettings)

-- | Settings for caption decriptions
encoderSettings_captionDescriptions :: Lens.Lens' EncoderSettings (Core.Maybe [CaptionDescription])
encoderSettings_captionDescriptions = Lens.lens (\EncoderSettings' {captionDescriptions} -> captionDescriptions) (\s@EncoderSettings' {} a -> s {captionDescriptions = a} :: EncoderSettings) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
encoderSettings_videoDescriptions :: Lens.Lens' EncoderSettings [VideoDescription]
encoderSettings_videoDescriptions = Lens.lens (\EncoderSettings' {videoDescriptions} -> videoDescriptions) (\s@EncoderSettings' {} a -> s {videoDescriptions = a} :: EncoderSettings) Core.. Lens._Coerce

-- | Undocumented member.
encoderSettings_audioDescriptions :: Lens.Lens' EncoderSettings [AudioDescription]
encoderSettings_audioDescriptions = Lens.lens (\EncoderSettings' {audioDescriptions} -> audioDescriptions) (\s@EncoderSettings' {} a -> s {audioDescriptions = a} :: EncoderSettings) Core.. Lens._Coerce

-- | Undocumented member.
encoderSettings_outputGroups :: Lens.Lens' EncoderSettings [OutputGroup]
encoderSettings_outputGroups = Lens.lens (\EncoderSettings' {outputGroups} -> outputGroups) (\s@EncoderSettings' {} a -> s {outputGroups = a} :: EncoderSettings) Core.. Lens._Coerce

-- | Contains settings used to acquire and adjust timecode information from
-- inputs.
encoderSettings_timecodeConfig :: Lens.Lens' EncoderSettings TimecodeConfig
encoderSettings_timecodeConfig = Lens.lens (\EncoderSettings' {timecodeConfig} -> timecodeConfig) (\s@EncoderSettings' {} a -> s {timecodeConfig = a} :: EncoderSettings)

instance Core.FromJSON EncoderSettings where
  parseJSON =
    Core.withObject
      "EncoderSettings"
      ( \x ->
          EncoderSettings'
            Core.<$> (x Core..:? "globalConfiguration")
            Core.<*> (x Core..:? "featureActivations")
            Core.<*> (x Core..:? "availConfiguration")
            Core.<*> (x Core..:? "availBlanking")
            Core.<*> (x Core..:? "nielsenConfiguration")
            Core.<*> (x Core..:? "blackoutSlate")
            Core.<*> ( x Core..:? "captionDescriptions"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "videoDescriptions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "audioDescriptions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "outputGroups" Core..!= Core.mempty)
            Core.<*> (x Core..: "timecodeConfig")
      )

instance Core.Hashable EncoderSettings

instance Core.NFData EncoderSettings

instance Core.ToJSON EncoderSettings where
  toJSON EncoderSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("globalConfiguration" Core..=)
              Core.<$> globalConfiguration,
            ("featureActivations" Core..=)
              Core.<$> featureActivations,
            ("availConfiguration" Core..=)
              Core.<$> availConfiguration,
            ("availBlanking" Core..=) Core.<$> availBlanking,
            ("nielsenConfiguration" Core..=)
              Core.<$> nielsenConfiguration,
            ("blackoutSlate" Core..=) Core.<$> blackoutSlate,
            ("captionDescriptions" Core..=)
              Core.<$> captionDescriptions,
            Core.Just
              ("videoDescriptions" Core..= videoDescriptions),
            Core.Just
              ("audioDescriptions" Core..= audioDescriptions),
            Core.Just ("outputGroups" Core..= outputGroups),
            Core.Just ("timecodeConfig" Core..= timecodeConfig)
          ]
      )
