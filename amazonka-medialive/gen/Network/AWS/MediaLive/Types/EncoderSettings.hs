{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Prelude as Prelude

-- | Encoder Settings
--
-- /See:/ 'newEncoderSettings' smart constructor.
data EncoderSettings = EncoderSettings'
  { -- | Configuration settings that apply to the event as a whole.
    globalConfiguration :: Prelude.Maybe GlobalConfiguration,
    -- | Feature Activations
    featureActivations :: Prelude.Maybe FeatureActivations,
    -- | Event-wide configuration settings for ad avail insertion.
    availConfiguration :: Prelude.Maybe AvailConfiguration,
    -- | Settings for ad avail blanking.
    availBlanking :: Prelude.Maybe AvailBlanking,
    -- | Nielsen configuration settings.
    nielsenConfiguration :: Prelude.Maybe NielsenConfiguration,
    -- | Settings for blackout slate.
    blackoutSlate :: Prelude.Maybe BlackoutSlate,
    -- | Settings for caption decriptions
    captionDescriptions :: Prelude.Maybe [CaptionDescription],
    videoDescriptions :: [VideoDescription],
    audioDescriptions :: [AudioDescription],
    outputGroups :: [OutputGroup],
    -- | Contains settings used to acquire and adjust timecode information from
    -- inputs.
    timecodeConfig :: TimecodeConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      featureActivations = Prelude.Nothing,
      availConfiguration = Prelude.Nothing,
      availBlanking = Prelude.Nothing,
      nielsenConfiguration = Prelude.Nothing,
      blackoutSlate = Prelude.Nothing,
      captionDescriptions = Prelude.Nothing,
      videoDescriptions = Prelude.mempty,
      audioDescriptions = Prelude.mempty,
      outputGroups = Prelude.mempty,
      timecodeConfig = pTimecodeConfig_
    }

-- | Configuration settings that apply to the event as a whole.
encoderSettings_globalConfiguration :: Lens.Lens' EncoderSettings (Prelude.Maybe GlobalConfiguration)
encoderSettings_globalConfiguration = Lens.lens (\EncoderSettings' {globalConfiguration} -> globalConfiguration) (\s@EncoderSettings' {} a -> s {globalConfiguration = a} :: EncoderSettings)

-- | Feature Activations
encoderSettings_featureActivations :: Lens.Lens' EncoderSettings (Prelude.Maybe FeatureActivations)
encoderSettings_featureActivations = Lens.lens (\EncoderSettings' {featureActivations} -> featureActivations) (\s@EncoderSettings' {} a -> s {featureActivations = a} :: EncoderSettings)

-- | Event-wide configuration settings for ad avail insertion.
encoderSettings_availConfiguration :: Lens.Lens' EncoderSettings (Prelude.Maybe AvailConfiguration)
encoderSettings_availConfiguration = Lens.lens (\EncoderSettings' {availConfiguration} -> availConfiguration) (\s@EncoderSettings' {} a -> s {availConfiguration = a} :: EncoderSettings)

-- | Settings for ad avail blanking.
encoderSettings_availBlanking :: Lens.Lens' EncoderSettings (Prelude.Maybe AvailBlanking)
encoderSettings_availBlanking = Lens.lens (\EncoderSettings' {availBlanking} -> availBlanking) (\s@EncoderSettings' {} a -> s {availBlanking = a} :: EncoderSettings)

-- | Nielsen configuration settings.
encoderSettings_nielsenConfiguration :: Lens.Lens' EncoderSettings (Prelude.Maybe NielsenConfiguration)
encoderSettings_nielsenConfiguration = Lens.lens (\EncoderSettings' {nielsenConfiguration} -> nielsenConfiguration) (\s@EncoderSettings' {} a -> s {nielsenConfiguration = a} :: EncoderSettings)

-- | Settings for blackout slate.
encoderSettings_blackoutSlate :: Lens.Lens' EncoderSettings (Prelude.Maybe BlackoutSlate)
encoderSettings_blackoutSlate = Lens.lens (\EncoderSettings' {blackoutSlate} -> blackoutSlate) (\s@EncoderSettings' {} a -> s {blackoutSlate = a} :: EncoderSettings)

-- | Settings for caption decriptions
encoderSettings_captionDescriptions :: Lens.Lens' EncoderSettings (Prelude.Maybe [CaptionDescription])
encoderSettings_captionDescriptions = Lens.lens (\EncoderSettings' {captionDescriptions} -> captionDescriptions) (\s@EncoderSettings' {} a -> s {captionDescriptions = a} :: EncoderSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
encoderSettings_videoDescriptions :: Lens.Lens' EncoderSettings [VideoDescription]
encoderSettings_videoDescriptions = Lens.lens (\EncoderSettings' {videoDescriptions} -> videoDescriptions) (\s@EncoderSettings' {} a -> s {videoDescriptions = a} :: EncoderSettings) Prelude.. Prelude._Coerce

-- | Undocumented member.
encoderSettings_audioDescriptions :: Lens.Lens' EncoderSettings [AudioDescription]
encoderSettings_audioDescriptions = Lens.lens (\EncoderSettings' {audioDescriptions} -> audioDescriptions) (\s@EncoderSettings' {} a -> s {audioDescriptions = a} :: EncoderSettings) Prelude.. Prelude._Coerce

-- | Undocumented member.
encoderSettings_outputGroups :: Lens.Lens' EncoderSettings [OutputGroup]
encoderSettings_outputGroups = Lens.lens (\EncoderSettings' {outputGroups} -> outputGroups) (\s@EncoderSettings' {} a -> s {outputGroups = a} :: EncoderSettings) Prelude.. Prelude._Coerce

-- | Contains settings used to acquire and adjust timecode information from
-- inputs.
encoderSettings_timecodeConfig :: Lens.Lens' EncoderSettings TimecodeConfig
encoderSettings_timecodeConfig = Lens.lens (\EncoderSettings' {timecodeConfig} -> timecodeConfig) (\s@EncoderSettings' {} a -> s {timecodeConfig = a} :: EncoderSettings)

instance Prelude.FromJSON EncoderSettings where
  parseJSON =
    Prelude.withObject
      "EncoderSettings"
      ( \x ->
          EncoderSettings'
            Prelude.<$> (x Prelude..:? "globalConfiguration")
            Prelude.<*> (x Prelude..:? "featureActivations")
            Prelude.<*> (x Prelude..:? "availConfiguration")
            Prelude.<*> (x Prelude..:? "availBlanking")
            Prelude.<*> (x Prelude..:? "nielsenConfiguration")
            Prelude.<*> (x Prelude..:? "blackoutSlate")
            Prelude.<*> ( x Prelude..:? "captionDescriptions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "videoDescriptions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "audioDescriptions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "outputGroups"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "timecodeConfig")
      )

instance Prelude.Hashable EncoderSettings

instance Prelude.NFData EncoderSettings

instance Prelude.ToJSON EncoderSettings where
  toJSON EncoderSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("globalConfiguration" Prelude..=)
              Prelude.<$> globalConfiguration,
            ("featureActivations" Prelude..=)
              Prelude.<$> featureActivations,
            ("availConfiguration" Prelude..=)
              Prelude.<$> availConfiguration,
            ("availBlanking" Prelude..=)
              Prelude.<$> availBlanking,
            ("nielsenConfiguration" Prelude..=)
              Prelude.<$> nielsenConfiguration,
            ("blackoutSlate" Prelude..=)
              Prelude.<$> blackoutSlate,
            ("captionDescriptions" Prelude..=)
              Prelude.<$> captionDescriptions,
            Prelude.Just
              ("videoDescriptions" Prelude..= videoDescriptions),
            Prelude.Just
              ("audioDescriptions" Prelude..= audioDescriptions),
            Prelude.Just
              ("outputGroups" Prelude..= outputGroups),
            Prelude.Just
              ("timecodeConfig" Prelude..= timecodeConfig)
          ]
      )
