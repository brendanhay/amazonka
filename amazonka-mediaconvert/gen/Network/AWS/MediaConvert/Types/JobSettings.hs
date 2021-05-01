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
-- Module      : Network.AWS.MediaConvert.Types.JobSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AvailBlanking
import Network.AWS.MediaConvert.Types.EsamSettings
import Network.AWS.MediaConvert.Types.Input
import Network.AWS.MediaConvert.Types.MotionImageInserter
import Network.AWS.MediaConvert.Types.NielsenConfiguration
import Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings
import Network.AWS.MediaConvert.Types.OutputGroup
import Network.AWS.MediaConvert.Types.TimecodeConfig
import Network.AWS.MediaConvert.Types.TimedMetadataInsertion
import qualified Network.AWS.Prelude as Prelude

-- | JobSettings contains all the transcode settings for a job.
--
-- /See:/ 'newJobSettings' smart constructor.
data JobSettings = JobSettings'
  { -- | When specified, this offset (in milliseconds) is added to the input Ad
    -- Avail PTS time.
    adAvailOffset :: Prelude.Maybe Prelude.Int,
    -- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
    -- tags in any HLS outputs. To include timed metadata, you must enable it
    -- here, enable it in each output container, and specify tags and timecodes
    -- in ID3 insertion (Id3Insertion) objects.
    timedMetadataInsertion :: Prelude.Maybe TimedMetadataInsertion,
    -- | Settings for Event Signaling And Messaging (ESAM).
    esam :: Prelude.Maybe EsamSettings,
    -- | Ignore these settings unless you are using Nielsen non-linear
    -- watermarking. Specify the values that MediaConvert uses to generate and
    -- place Nielsen watermarks in your output audio. In addition to specifying
    -- these values, you also need to set up your cloud TIC server. These
    -- settings apply to every output in your job. The MediaConvert
    -- implementation is currently with the following Nielsen versions: Nielsen
    -- Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7
    -- Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
    nielsenNonLinearWatermark :: Prelude.Maybe NielsenNonLinearWatermarkSettings,
    -- | Overlay motion graphics on top of your video. The motion graphics that
    -- you specify here appear on all outputs in all output groups.
    motionImageInserter :: Prelude.Maybe MotionImageInserter,
    -- | Settings for ad avail blanking. Video can be blanked or overlaid with an
    -- image, and audio muted during SCTE-35 triggered ad avails.
    availBlanking :: Prelude.Maybe AvailBlanking,
    -- | Settings for your Nielsen configuration. If you don\'t do Nielsen
    -- measurement and analytics, ignore these settings. When you enable
    -- Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM
    -- to ID3 tagging for all outputs in the job. To enable Nielsen
    -- configuration programmatically, include an instance of
    -- nielsenConfiguration in your JSON job specification. Even if you don\'t
    -- include any children of nielsenConfiguration, you still enable the
    -- setting.
    nielsenConfiguration :: Prelude.Maybe NielsenConfiguration,
    -- | (OutputGroups) contains one group of settings for each set of outputs
    -- that share a common package type. All unpackaged files (MPEG-4, MPEG-2
    -- TS, Quicktime, MXF, and no container) are grouped in a single output
    -- group as well. Required in (OutputGroups) is a group of settings that
    -- apply to the whole group. This required object depends on the value you
    -- set for (Type) under (OutputGroups)>(OutputGroupSettings). Type,
    -- settings object pairs are as follows. * FILE_GROUP_SETTINGS,
    -- FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings *
    -- DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings *
    -- MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS,
    -- CmafGroupSettings
    outputGroups :: Prelude.Maybe [OutputGroup],
    -- | Use Inputs (inputs) to define source file used in the transcode job.
    -- There can be multiple inputs add in a job. These inputs will be
    -- concantenated together to create the output.
    inputs :: Prelude.Maybe [Input],
    -- | Contains settings used to acquire and adjust timecode information from
    -- inputs.
    timecodeConfig :: Prelude.Maybe TimecodeConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JobSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adAvailOffset', 'jobSettings_adAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time.
--
-- 'timedMetadataInsertion', 'jobSettings_timedMetadataInsertion' - Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
-- tags in any HLS outputs. To include timed metadata, you must enable it
-- here, enable it in each output container, and specify tags and timecodes
-- in ID3 insertion (Id3Insertion) objects.
--
-- 'esam', 'jobSettings_esam' - Settings for Event Signaling And Messaging (ESAM).
--
-- 'nielsenNonLinearWatermark', 'jobSettings_nielsenNonLinearWatermark' - Ignore these settings unless you are using Nielsen non-linear
-- watermarking. Specify the values that MediaConvert uses to generate and
-- place Nielsen watermarks in your output audio. In addition to specifying
-- these values, you also need to set up your cloud TIC server. These
-- settings apply to every output in your job. The MediaConvert
-- implementation is currently with the following Nielsen versions: Nielsen
-- Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7
-- Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
--
-- 'motionImageInserter', 'jobSettings_motionImageInserter' - Overlay motion graphics on top of your video. The motion graphics that
-- you specify here appear on all outputs in all output groups.
--
-- 'availBlanking', 'jobSettings_availBlanking' - Settings for ad avail blanking. Video can be blanked or overlaid with an
-- image, and audio muted during SCTE-35 triggered ad avails.
--
-- 'nielsenConfiguration', 'jobSettings_nielsenConfiguration' - Settings for your Nielsen configuration. If you don\'t do Nielsen
-- measurement and analytics, ignore these settings. When you enable
-- Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM
-- to ID3 tagging for all outputs in the job. To enable Nielsen
-- configuration programmatically, include an instance of
-- nielsenConfiguration in your JSON job specification. Even if you don\'t
-- include any children of nielsenConfiguration, you still enable the
-- setting.
--
-- 'outputGroups', 'jobSettings_outputGroups' - (OutputGroups) contains one group of settings for each set of outputs
-- that share a common package type. All unpackaged files (MPEG-4, MPEG-2
-- TS, Quicktime, MXF, and no container) are grouped in a single output
-- group as well. Required in (OutputGroups) is a group of settings that
-- apply to the whole group. This required object depends on the value you
-- set for (Type) under (OutputGroups)>(OutputGroupSettings). Type,
-- settings object pairs are as follows. * FILE_GROUP_SETTINGS,
-- FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings *
-- DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings *
-- MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS,
-- CmafGroupSettings
--
-- 'inputs', 'jobSettings_inputs' - Use Inputs (inputs) to define source file used in the transcode job.
-- There can be multiple inputs add in a job. These inputs will be
-- concantenated together to create the output.
--
-- 'timecodeConfig', 'jobSettings_timecodeConfig' - Contains settings used to acquire and adjust timecode information from
-- inputs.
newJobSettings ::
  JobSettings
newJobSettings =
  JobSettings'
    { adAvailOffset = Prelude.Nothing,
      timedMetadataInsertion = Prelude.Nothing,
      esam = Prelude.Nothing,
      nielsenNonLinearWatermark = Prelude.Nothing,
      motionImageInserter = Prelude.Nothing,
      availBlanking = Prelude.Nothing,
      nielsenConfiguration = Prelude.Nothing,
      outputGroups = Prelude.Nothing,
      inputs = Prelude.Nothing,
      timecodeConfig = Prelude.Nothing
    }

-- | When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time.
jobSettings_adAvailOffset :: Lens.Lens' JobSettings (Prelude.Maybe Prelude.Int)
jobSettings_adAvailOffset = Lens.lens (\JobSettings' {adAvailOffset} -> adAvailOffset) (\s@JobSettings' {} a -> s {adAvailOffset = a} :: JobSettings)

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
-- tags in any HLS outputs. To include timed metadata, you must enable it
-- here, enable it in each output container, and specify tags and timecodes
-- in ID3 insertion (Id3Insertion) objects.
jobSettings_timedMetadataInsertion :: Lens.Lens' JobSettings (Prelude.Maybe TimedMetadataInsertion)
jobSettings_timedMetadataInsertion = Lens.lens (\JobSettings' {timedMetadataInsertion} -> timedMetadataInsertion) (\s@JobSettings' {} a -> s {timedMetadataInsertion = a} :: JobSettings)

-- | Settings for Event Signaling And Messaging (ESAM).
jobSettings_esam :: Lens.Lens' JobSettings (Prelude.Maybe EsamSettings)
jobSettings_esam = Lens.lens (\JobSettings' {esam} -> esam) (\s@JobSettings' {} a -> s {esam = a} :: JobSettings)

-- | Ignore these settings unless you are using Nielsen non-linear
-- watermarking. Specify the values that MediaConvert uses to generate and
-- place Nielsen watermarks in your output audio. In addition to specifying
-- these values, you also need to set up your cloud TIC server. These
-- settings apply to every output in your job. The MediaConvert
-- implementation is currently with the following Nielsen versions: Nielsen
-- Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7
-- Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
jobSettings_nielsenNonLinearWatermark :: Lens.Lens' JobSettings (Prelude.Maybe NielsenNonLinearWatermarkSettings)
jobSettings_nielsenNonLinearWatermark = Lens.lens (\JobSettings' {nielsenNonLinearWatermark} -> nielsenNonLinearWatermark) (\s@JobSettings' {} a -> s {nielsenNonLinearWatermark = a} :: JobSettings)

-- | Overlay motion graphics on top of your video. The motion graphics that
-- you specify here appear on all outputs in all output groups.
jobSettings_motionImageInserter :: Lens.Lens' JobSettings (Prelude.Maybe MotionImageInserter)
jobSettings_motionImageInserter = Lens.lens (\JobSettings' {motionImageInserter} -> motionImageInserter) (\s@JobSettings' {} a -> s {motionImageInserter = a} :: JobSettings)

-- | Settings for ad avail blanking. Video can be blanked or overlaid with an
-- image, and audio muted during SCTE-35 triggered ad avails.
jobSettings_availBlanking :: Lens.Lens' JobSettings (Prelude.Maybe AvailBlanking)
jobSettings_availBlanking = Lens.lens (\JobSettings' {availBlanking} -> availBlanking) (\s@JobSettings' {} a -> s {availBlanking = a} :: JobSettings)

-- | Settings for your Nielsen configuration. If you don\'t do Nielsen
-- measurement and analytics, ignore these settings. When you enable
-- Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM
-- to ID3 tagging for all outputs in the job. To enable Nielsen
-- configuration programmatically, include an instance of
-- nielsenConfiguration in your JSON job specification. Even if you don\'t
-- include any children of nielsenConfiguration, you still enable the
-- setting.
jobSettings_nielsenConfiguration :: Lens.Lens' JobSettings (Prelude.Maybe NielsenConfiguration)
jobSettings_nielsenConfiguration = Lens.lens (\JobSettings' {nielsenConfiguration} -> nielsenConfiguration) (\s@JobSettings' {} a -> s {nielsenConfiguration = a} :: JobSettings)

-- | (OutputGroups) contains one group of settings for each set of outputs
-- that share a common package type. All unpackaged files (MPEG-4, MPEG-2
-- TS, Quicktime, MXF, and no container) are grouped in a single output
-- group as well. Required in (OutputGroups) is a group of settings that
-- apply to the whole group. This required object depends on the value you
-- set for (Type) under (OutputGroups)>(OutputGroupSettings). Type,
-- settings object pairs are as follows. * FILE_GROUP_SETTINGS,
-- FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings *
-- DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings *
-- MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS,
-- CmafGroupSettings
jobSettings_outputGroups :: Lens.Lens' JobSettings (Prelude.Maybe [OutputGroup])
jobSettings_outputGroups = Lens.lens (\JobSettings' {outputGroups} -> outputGroups) (\s@JobSettings' {} a -> s {outputGroups = a} :: JobSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | Use Inputs (inputs) to define source file used in the transcode job.
-- There can be multiple inputs add in a job. These inputs will be
-- concantenated together to create the output.
jobSettings_inputs :: Lens.Lens' JobSettings (Prelude.Maybe [Input])
jobSettings_inputs = Lens.lens (\JobSettings' {inputs} -> inputs) (\s@JobSettings' {} a -> s {inputs = a} :: JobSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | Contains settings used to acquire and adjust timecode information from
-- inputs.
jobSettings_timecodeConfig :: Lens.Lens' JobSettings (Prelude.Maybe TimecodeConfig)
jobSettings_timecodeConfig = Lens.lens (\JobSettings' {timecodeConfig} -> timecodeConfig) (\s@JobSettings' {} a -> s {timecodeConfig = a} :: JobSettings)

instance Prelude.FromJSON JobSettings where
  parseJSON =
    Prelude.withObject
      "JobSettings"
      ( \x ->
          JobSettings'
            Prelude.<$> (x Prelude..:? "adAvailOffset")
            Prelude.<*> (x Prelude..:? "timedMetadataInsertion")
            Prelude.<*> (x Prelude..:? "esam")
            Prelude.<*> (x Prelude..:? "nielsenNonLinearWatermark")
            Prelude.<*> (x Prelude..:? "motionImageInserter")
            Prelude.<*> (x Prelude..:? "availBlanking")
            Prelude.<*> (x Prelude..:? "nielsenConfiguration")
            Prelude.<*> ( x Prelude..:? "outputGroups"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "inputs" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "timecodeConfig")
      )

instance Prelude.Hashable JobSettings

instance Prelude.NFData JobSettings

instance Prelude.ToJSON JobSettings where
  toJSON JobSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("adAvailOffset" Prelude..=)
              Prelude.<$> adAvailOffset,
            ("timedMetadataInsertion" Prelude..=)
              Prelude.<$> timedMetadataInsertion,
            ("esam" Prelude..=) Prelude.<$> esam,
            ("nielsenNonLinearWatermark" Prelude..=)
              Prelude.<$> nielsenNonLinearWatermark,
            ("motionImageInserter" Prelude..=)
              Prelude.<$> motionImageInserter,
            ("availBlanking" Prelude..=)
              Prelude.<$> availBlanking,
            ("nielsenConfiguration" Prelude..=)
              Prelude.<$> nielsenConfiguration,
            ("outputGroups" Prelude..=) Prelude.<$> outputGroups,
            ("inputs" Prelude..=) Prelude.<$> inputs,
            ("timecodeConfig" Prelude..=)
              Prelude.<$> timecodeConfig
          ]
      )
