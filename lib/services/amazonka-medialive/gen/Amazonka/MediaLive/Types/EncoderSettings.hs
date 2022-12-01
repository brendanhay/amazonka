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
-- Module      : Amazonka.MediaLive.Types.EncoderSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.EncoderSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.AudioDescription
import Amazonka.MediaLive.Types.AvailBlanking
import Amazonka.MediaLive.Types.AvailConfiguration
import Amazonka.MediaLive.Types.BlackoutSlate
import Amazonka.MediaLive.Types.CaptionDescription
import Amazonka.MediaLive.Types.FeatureActivations
import Amazonka.MediaLive.Types.GlobalConfiguration
import Amazonka.MediaLive.Types.MotionGraphicsConfiguration
import Amazonka.MediaLive.Types.NielsenConfiguration
import Amazonka.MediaLive.Types.OutputGroup
import Amazonka.MediaLive.Types.TimecodeConfig
import Amazonka.MediaLive.Types.VideoDescription
import qualified Amazonka.Prelude as Prelude

-- | Encoder Settings
--
-- /See:/ 'newEncoderSettings' smart constructor.
data EncoderSettings = EncoderSettings'
  { -- | Configuration settings that apply to the event as a whole.
    globalConfiguration :: Prelude.Maybe GlobalConfiguration,
    -- | Settings for ad avail blanking.
    availBlanking :: Prelude.Maybe AvailBlanking,
    -- | Feature Activations
    featureActivations :: Prelude.Maybe FeatureActivations,
    -- | Settings for motion graphics.
    motionGraphicsConfiguration :: Prelude.Maybe MotionGraphicsConfiguration,
    -- | Settings for caption decriptions
    captionDescriptions :: Prelude.Maybe [CaptionDescription],
    -- | Settings for blackout slate.
    blackoutSlate :: Prelude.Maybe BlackoutSlate,
    -- | Nielsen configuration settings.
    nielsenConfiguration :: Prelude.Maybe NielsenConfiguration,
    -- | Event-wide configuration settings for ad avail insertion.
    availConfiguration :: Prelude.Maybe AvailConfiguration,
    videoDescriptions :: [VideoDescription],
    audioDescriptions :: [AudioDescription],
    outputGroups :: [OutputGroup],
    -- | Contains settings used to acquire and adjust timecode information from
    -- inputs.
    timecodeConfig :: TimecodeConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'availBlanking', 'encoderSettings_availBlanking' - Settings for ad avail blanking.
--
-- 'featureActivations', 'encoderSettings_featureActivations' - Feature Activations
--
-- 'motionGraphicsConfiguration', 'encoderSettings_motionGraphicsConfiguration' - Settings for motion graphics.
--
-- 'captionDescriptions', 'encoderSettings_captionDescriptions' - Settings for caption decriptions
--
-- 'blackoutSlate', 'encoderSettings_blackoutSlate' - Settings for blackout slate.
--
-- 'nielsenConfiguration', 'encoderSettings_nielsenConfiguration' - Nielsen configuration settings.
--
-- 'availConfiguration', 'encoderSettings_availConfiguration' - Event-wide configuration settings for ad avail insertion.
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
        Prelude.Nothing,
      availBlanking = Prelude.Nothing,
      featureActivations = Prelude.Nothing,
      motionGraphicsConfiguration = Prelude.Nothing,
      captionDescriptions = Prelude.Nothing,
      blackoutSlate = Prelude.Nothing,
      nielsenConfiguration = Prelude.Nothing,
      availConfiguration = Prelude.Nothing,
      videoDescriptions = Prelude.mempty,
      audioDescriptions = Prelude.mempty,
      outputGroups = Prelude.mempty,
      timecodeConfig = pTimecodeConfig_
    }

-- | Configuration settings that apply to the event as a whole.
encoderSettings_globalConfiguration :: Lens.Lens' EncoderSettings (Prelude.Maybe GlobalConfiguration)
encoderSettings_globalConfiguration = Lens.lens (\EncoderSettings' {globalConfiguration} -> globalConfiguration) (\s@EncoderSettings' {} a -> s {globalConfiguration = a} :: EncoderSettings)

-- | Settings for ad avail blanking.
encoderSettings_availBlanking :: Lens.Lens' EncoderSettings (Prelude.Maybe AvailBlanking)
encoderSettings_availBlanking = Lens.lens (\EncoderSettings' {availBlanking} -> availBlanking) (\s@EncoderSettings' {} a -> s {availBlanking = a} :: EncoderSettings)

-- | Feature Activations
encoderSettings_featureActivations :: Lens.Lens' EncoderSettings (Prelude.Maybe FeatureActivations)
encoderSettings_featureActivations = Lens.lens (\EncoderSettings' {featureActivations} -> featureActivations) (\s@EncoderSettings' {} a -> s {featureActivations = a} :: EncoderSettings)

-- | Settings for motion graphics.
encoderSettings_motionGraphicsConfiguration :: Lens.Lens' EncoderSettings (Prelude.Maybe MotionGraphicsConfiguration)
encoderSettings_motionGraphicsConfiguration = Lens.lens (\EncoderSettings' {motionGraphicsConfiguration} -> motionGraphicsConfiguration) (\s@EncoderSettings' {} a -> s {motionGraphicsConfiguration = a} :: EncoderSettings)

-- | Settings for caption decriptions
encoderSettings_captionDescriptions :: Lens.Lens' EncoderSettings (Prelude.Maybe [CaptionDescription])
encoderSettings_captionDescriptions = Lens.lens (\EncoderSettings' {captionDescriptions} -> captionDescriptions) (\s@EncoderSettings' {} a -> s {captionDescriptions = a} :: EncoderSettings) Prelude.. Lens.mapping Lens.coerced

-- | Settings for blackout slate.
encoderSettings_blackoutSlate :: Lens.Lens' EncoderSettings (Prelude.Maybe BlackoutSlate)
encoderSettings_blackoutSlate = Lens.lens (\EncoderSettings' {blackoutSlate} -> blackoutSlate) (\s@EncoderSettings' {} a -> s {blackoutSlate = a} :: EncoderSettings)

-- | Nielsen configuration settings.
encoderSettings_nielsenConfiguration :: Lens.Lens' EncoderSettings (Prelude.Maybe NielsenConfiguration)
encoderSettings_nielsenConfiguration = Lens.lens (\EncoderSettings' {nielsenConfiguration} -> nielsenConfiguration) (\s@EncoderSettings' {} a -> s {nielsenConfiguration = a} :: EncoderSettings)

-- | Event-wide configuration settings for ad avail insertion.
encoderSettings_availConfiguration :: Lens.Lens' EncoderSettings (Prelude.Maybe AvailConfiguration)
encoderSettings_availConfiguration = Lens.lens (\EncoderSettings' {availConfiguration} -> availConfiguration) (\s@EncoderSettings' {} a -> s {availConfiguration = a} :: EncoderSettings)

-- | Undocumented member.
encoderSettings_videoDescriptions :: Lens.Lens' EncoderSettings [VideoDescription]
encoderSettings_videoDescriptions = Lens.lens (\EncoderSettings' {videoDescriptions} -> videoDescriptions) (\s@EncoderSettings' {} a -> s {videoDescriptions = a} :: EncoderSettings) Prelude.. Lens.coerced

-- | Undocumented member.
encoderSettings_audioDescriptions :: Lens.Lens' EncoderSettings [AudioDescription]
encoderSettings_audioDescriptions = Lens.lens (\EncoderSettings' {audioDescriptions} -> audioDescriptions) (\s@EncoderSettings' {} a -> s {audioDescriptions = a} :: EncoderSettings) Prelude.. Lens.coerced

-- | Undocumented member.
encoderSettings_outputGroups :: Lens.Lens' EncoderSettings [OutputGroup]
encoderSettings_outputGroups = Lens.lens (\EncoderSettings' {outputGroups} -> outputGroups) (\s@EncoderSettings' {} a -> s {outputGroups = a} :: EncoderSettings) Prelude.. Lens.coerced

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
            Prelude.<$> (x Core..:? "globalConfiguration")
            Prelude.<*> (x Core..:? "availBlanking")
            Prelude.<*> (x Core..:? "featureActivations")
            Prelude.<*> (x Core..:? "motionGraphicsConfiguration")
            Prelude.<*> ( x Core..:? "captionDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "blackoutSlate")
            Prelude.<*> (x Core..:? "nielsenConfiguration")
            Prelude.<*> (x Core..:? "availConfiguration")
            Prelude.<*> ( x Core..:? "videoDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "audioDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "outputGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "timecodeConfig")
      )

instance Prelude.Hashable EncoderSettings where
  hashWithSalt _salt EncoderSettings' {..} =
    _salt `Prelude.hashWithSalt` globalConfiguration
      `Prelude.hashWithSalt` availBlanking
      `Prelude.hashWithSalt` featureActivations
      `Prelude.hashWithSalt` motionGraphicsConfiguration
      `Prelude.hashWithSalt` captionDescriptions
      `Prelude.hashWithSalt` blackoutSlate
      `Prelude.hashWithSalt` nielsenConfiguration
      `Prelude.hashWithSalt` availConfiguration
      `Prelude.hashWithSalt` videoDescriptions
      `Prelude.hashWithSalt` audioDescriptions
      `Prelude.hashWithSalt` outputGroups
      `Prelude.hashWithSalt` timecodeConfig

instance Prelude.NFData EncoderSettings where
  rnf EncoderSettings' {..} =
    Prelude.rnf globalConfiguration
      `Prelude.seq` Prelude.rnf availBlanking
      `Prelude.seq` Prelude.rnf featureActivations
      `Prelude.seq` Prelude.rnf motionGraphicsConfiguration
      `Prelude.seq` Prelude.rnf captionDescriptions
      `Prelude.seq` Prelude.rnf blackoutSlate
      `Prelude.seq` Prelude.rnf nielsenConfiguration
      `Prelude.seq` Prelude.rnf availConfiguration
      `Prelude.seq` Prelude.rnf videoDescriptions
      `Prelude.seq` Prelude.rnf audioDescriptions
      `Prelude.seq` Prelude.rnf outputGroups
      `Prelude.seq` Prelude.rnf timecodeConfig

instance Core.ToJSON EncoderSettings where
  toJSON EncoderSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("globalConfiguration" Core..=)
              Prelude.<$> globalConfiguration,
            ("availBlanking" Core..=) Prelude.<$> availBlanking,
            ("featureActivations" Core..=)
              Prelude.<$> featureActivations,
            ("motionGraphicsConfiguration" Core..=)
              Prelude.<$> motionGraphicsConfiguration,
            ("captionDescriptions" Core..=)
              Prelude.<$> captionDescriptions,
            ("blackoutSlate" Core..=) Prelude.<$> blackoutSlate,
            ("nielsenConfiguration" Core..=)
              Prelude.<$> nielsenConfiguration,
            ("availConfiguration" Core..=)
              Prelude.<$> availConfiguration,
            Prelude.Just
              ("videoDescriptions" Core..= videoDescriptions),
            Prelude.Just
              ("audioDescriptions" Core..= audioDescriptions),
            Prelude.Just ("outputGroups" Core..= outputGroups),
            Prelude.Just
              ("timecodeConfig" Core..= timecodeConfig)
          ]
      )
