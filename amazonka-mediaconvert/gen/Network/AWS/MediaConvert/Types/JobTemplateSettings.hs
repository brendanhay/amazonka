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
-- Module      : Network.AWS.MediaConvert.Types.JobTemplateSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobTemplateSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AvailBlanking
import Network.AWS.MediaConvert.Types.EsamSettings
import Network.AWS.MediaConvert.Types.InputTemplate
import Network.AWS.MediaConvert.Types.MotionImageInserter
import Network.AWS.MediaConvert.Types.NielsenConfiguration
import Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings
import Network.AWS.MediaConvert.Types.OutputGroup
import Network.AWS.MediaConvert.Types.TimecodeConfig
import Network.AWS.MediaConvert.Types.TimedMetadataInsertion
import qualified Network.AWS.Prelude as Prelude

-- | JobTemplateSettings contains all the transcode settings saved in the
-- template that will be applied to jobs created from it.
--
-- /See:/ 'newJobTemplateSettings' smart constructor.
data JobTemplateSettings = JobTemplateSettings'
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
    -- | Use Inputs (inputs) to define the source file used in the transcode job.
    -- There can only be one input in a job template. Using the API, you can
    -- include multiple inputs when referencing a job template.
    inputs :: Prelude.Maybe [InputTemplate],
    -- | Contains settings used to acquire and adjust timecode information from
    -- inputs.
    timecodeConfig :: Prelude.Maybe TimecodeConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JobTemplateSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adAvailOffset', 'jobTemplateSettings_adAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time.
--
-- 'timedMetadataInsertion', 'jobTemplateSettings_timedMetadataInsertion' - Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
-- tags in any HLS outputs. To include timed metadata, you must enable it
-- here, enable it in each output container, and specify tags and timecodes
-- in ID3 insertion (Id3Insertion) objects.
--
-- 'esam', 'jobTemplateSettings_esam' - Settings for Event Signaling And Messaging (ESAM).
--
-- 'nielsenNonLinearWatermark', 'jobTemplateSettings_nielsenNonLinearWatermark' - Ignore these settings unless you are using Nielsen non-linear
-- watermarking. Specify the values that MediaConvert uses to generate and
-- place Nielsen watermarks in your output audio. In addition to specifying
-- these values, you also need to set up your cloud TIC server. These
-- settings apply to every output in your job. The MediaConvert
-- implementation is currently with the following Nielsen versions: Nielsen
-- Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7
-- Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
--
-- 'motionImageInserter', 'jobTemplateSettings_motionImageInserter' - Overlay motion graphics on top of your video. The motion graphics that
-- you specify here appear on all outputs in all output groups.
--
-- 'availBlanking', 'jobTemplateSettings_availBlanking' - Settings for ad avail blanking. Video can be blanked or overlaid with an
-- image, and audio muted during SCTE-35 triggered ad avails.
--
-- 'nielsenConfiguration', 'jobTemplateSettings_nielsenConfiguration' - Settings for your Nielsen configuration. If you don\'t do Nielsen
-- measurement and analytics, ignore these settings. When you enable
-- Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM
-- to ID3 tagging for all outputs in the job. To enable Nielsen
-- configuration programmatically, include an instance of
-- nielsenConfiguration in your JSON job specification. Even if you don\'t
-- include any children of nielsenConfiguration, you still enable the
-- setting.
--
-- 'outputGroups', 'jobTemplateSettings_outputGroups' - (OutputGroups) contains one group of settings for each set of outputs
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
-- 'inputs', 'jobTemplateSettings_inputs' - Use Inputs (inputs) to define the source file used in the transcode job.
-- There can only be one input in a job template. Using the API, you can
-- include multiple inputs when referencing a job template.
--
-- 'timecodeConfig', 'jobTemplateSettings_timecodeConfig' - Contains settings used to acquire and adjust timecode information from
-- inputs.
newJobTemplateSettings ::
  JobTemplateSettings
newJobTemplateSettings =
  JobTemplateSettings'
    { adAvailOffset =
        Prelude.Nothing,
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
jobTemplateSettings_adAvailOffset :: Lens.Lens' JobTemplateSettings (Prelude.Maybe Prelude.Int)
jobTemplateSettings_adAvailOffset = Lens.lens (\JobTemplateSettings' {adAvailOffset} -> adAvailOffset) (\s@JobTemplateSettings' {} a -> s {adAvailOffset = a} :: JobTemplateSettings)

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
-- tags in any HLS outputs. To include timed metadata, you must enable it
-- here, enable it in each output container, and specify tags and timecodes
-- in ID3 insertion (Id3Insertion) objects.
jobTemplateSettings_timedMetadataInsertion :: Lens.Lens' JobTemplateSettings (Prelude.Maybe TimedMetadataInsertion)
jobTemplateSettings_timedMetadataInsertion = Lens.lens (\JobTemplateSettings' {timedMetadataInsertion} -> timedMetadataInsertion) (\s@JobTemplateSettings' {} a -> s {timedMetadataInsertion = a} :: JobTemplateSettings)

-- | Settings for Event Signaling And Messaging (ESAM).
jobTemplateSettings_esam :: Lens.Lens' JobTemplateSettings (Prelude.Maybe EsamSettings)
jobTemplateSettings_esam = Lens.lens (\JobTemplateSettings' {esam} -> esam) (\s@JobTemplateSettings' {} a -> s {esam = a} :: JobTemplateSettings)

-- | Ignore these settings unless you are using Nielsen non-linear
-- watermarking. Specify the values that MediaConvert uses to generate and
-- place Nielsen watermarks in your output audio. In addition to specifying
-- these values, you also need to set up your cloud TIC server. These
-- settings apply to every output in your job. The MediaConvert
-- implementation is currently with the following Nielsen versions: Nielsen
-- Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7
-- Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
jobTemplateSettings_nielsenNonLinearWatermark :: Lens.Lens' JobTemplateSettings (Prelude.Maybe NielsenNonLinearWatermarkSettings)
jobTemplateSettings_nielsenNonLinearWatermark = Lens.lens (\JobTemplateSettings' {nielsenNonLinearWatermark} -> nielsenNonLinearWatermark) (\s@JobTemplateSettings' {} a -> s {nielsenNonLinearWatermark = a} :: JobTemplateSettings)

-- | Overlay motion graphics on top of your video. The motion graphics that
-- you specify here appear on all outputs in all output groups.
jobTemplateSettings_motionImageInserter :: Lens.Lens' JobTemplateSettings (Prelude.Maybe MotionImageInserter)
jobTemplateSettings_motionImageInserter = Lens.lens (\JobTemplateSettings' {motionImageInserter} -> motionImageInserter) (\s@JobTemplateSettings' {} a -> s {motionImageInserter = a} :: JobTemplateSettings)

-- | Settings for ad avail blanking. Video can be blanked or overlaid with an
-- image, and audio muted during SCTE-35 triggered ad avails.
jobTemplateSettings_availBlanking :: Lens.Lens' JobTemplateSettings (Prelude.Maybe AvailBlanking)
jobTemplateSettings_availBlanking = Lens.lens (\JobTemplateSettings' {availBlanking} -> availBlanking) (\s@JobTemplateSettings' {} a -> s {availBlanking = a} :: JobTemplateSettings)

-- | Settings for your Nielsen configuration. If you don\'t do Nielsen
-- measurement and analytics, ignore these settings. When you enable
-- Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM
-- to ID3 tagging for all outputs in the job. To enable Nielsen
-- configuration programmatically, include an instance of
-- nielsenConfiguration in your JSON job specification. Even if you don\'t
-- include any children of nielsenConfiguration, you still enable the
-- setting.
jobTemplateSettings_nielsenConfiguration :: Lens.Lens' JobTemplateSettings (Prelude.Maybe NielsenConfiguration)
jobTemplateSettings_nielsenConfiguration = Lens.lens (\JobTemplateSettings' {nielsenConfiguration} -> nielsenConfiguration) (\s@JobTemplateSettings' {} a -> s {nielsenConfiguration = a} :: JobTemplateSettings)

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
jobTemplateSettings_outputGroups :: Lens.Lens' JobTemplateSettings (Prelude.Maybe [OutputGroup])
jobTemplateSettings_outputGroups = Lens.lens (\JobTemplateSettings' {outputGroups} -> outputGroups) (\s@JobTemplateSettings' {} a -> s {outputGroups = a} :: JobTemplateSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | Use Inputs (inputs) to define the source file used in the transcode job.
-- There can only be one input in a job template. Using the API, you can
-- include multiple inputs when referencing a job template.
jobTemplateSettings_inputs :: Lens.Lens' JobTemplateSettings (Prelude.Maybe [InputTemplate])
jobTemplateSettings_inputs = Lens.lens (\JobTemplateSettings' {inputs} -> inputs) (\s@JobTemplateSettings' {} a -> s {inputs = a} :: JobTemplateSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | Contains settings used to acquire and adjust timecode information from
-- inputs.
jobTemplateSettings_timecodeConfig :: Lens.Lens' JobTemplateSettings (Prelude.Maybe TimecodeConfig)
jobTemplateSettings_timecodeConfig = Lens.lens (\JobTemplateSettings' {timecodeConfig} -> timecodeConfig) (\s@JobTemplateSettings' {} a -> s {timecodeConfig = a} :: JobTemplateSettings)

instance Prelude.FromJSON JobTemplateSettings where
  parseJSON =
    Prelude.withObject
      "JobTemplateSettings"
      ( \x ->
          JobTemplateSettings'
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

instance Prelude.Hashable JobTemplateSettings

instance Prelude.NFData JobTemplateSettings

instance Prelude.ToJSON JobTemplateSettings where
  toJSON JobTemplateSettings' {..} =
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
