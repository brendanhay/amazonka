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

import qualified Network.AWS.Core as Core
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

-- | JobSettings contains all the transcode settings for a job.
--
-- /See:/ 'newJobSettings' smart constructor.
data JobSettings = JobSettings'
  { -- | When specified, this offset (in milliseconds) is added to the input Ad
    -- Avail PTS time.
    adAvailOffset :: Core.Maybe Core.Int,
    -- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
    -- tags in any HLS outputs. To include timed metadata, you must enable it
    -- here, enable it in each output container, and specify tags and timecodes
    -- in ID3 insertion (Id3Insertion) objects.
    timedMetadataInsertion :: Core.Maybe TimedMetadataInsertion,
    -- | Settings for Event Signaling And Messaging (ESAM).
    esam :: Core.Maybe EsamSettings,
    -- | Ignore these settings unless you are using Nielsen non-linear
    -- watermarking. Specify the values that MediaConvert uses to generate and
    -- place Nielsen watermarks in your output audio. In addition to specifying
    -- these values, you also need to set up your cloud TIC server. These
    -- settings apply to every output in your job. The MediaConvert
    -- implementation is currently with the following Nielsen versions: Nielsen
    -- Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7
    -- Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
    nielsenNonLinearWatermark :: Core.Maybe NielsenNonLinearWatermarkSettings,
    -- | Overlay motion graphics on top of your video. The motion graphics that
    -- you specify here appear on all outputs in all output groups.
    motionImageInserter :: Core.Maybe MotionImageInserter,
    -- | Settings for ad avail blanking. Video can be blanked or overlaid with an
    -- image, and audio muted during SCTE-35 triggered ad avails.
    availBlanking :: Core.Maybe AvailBlanking,
    -- | Settings for your Nielsen configuration. If you don\'t do Nielsen
    -- measurement and analytics, ignore these settings. When you enable
    -- Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM
    -- to ID3 tagging for all outputs in the job. To enable Nielsen
    -- configuration programmatically, include an instance of
    -- nielsenConfiguration in your JSON job specification. Even if you don\'t
    -- include any children of nielsenConfiguration, you still enable the
    -- setting.
    nielsenConfiguration :: Core.Maybe NielsenConfiguration,
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
    outputGroups :: Core.Maybe [OutputGroup],
    -- | Use Inputs (inputs) to define source file used in the transcode job.
    -- There can be multiple inputs add in a job. These inputs will be
    -- concantenated together to create the output.
    inputs :: Core.Maybe [Input],
    -- | Contains settings used to acquire and adjust timecode information from
    -- inputs.
    timecodeConfig :: Core.Maybe TimecodeConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { adAvailOffset = Core.Nothing,
      timedMetadataInsertion = Core.Nothing,
      esam = Core.Nothing,
      nielsenNonLinearWatermark = Core.Nothing,
      motionImageInserter = Core.Nothing,
      availBlanking = Core.Nothing,
      nielsenConfiguration = Core.Nothing,
      outputGroups = Core.Nothing,
      inputs = Core.Nothing,
      timecodeConfig = Core.Nothing
    }

-- | When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time.
jobSettings_adAvailOffset :: Lens.Lens' JobSettings (Core.Maybe Core.Int)
jobSettings_adAvailOffset = Lens.lens (\JobSettings' {adAvailOffset} -> adAvailOffset) (\s@JobSettings' {} a -> s {adAvailOffset = a} :: JobSettings)

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
-- tags in any HLS outputs. To include timed metadata, you must enable it
-- here, enable it in each output container, and specify tags and timecodes
-- in ID3 insertion (Id3Insertion) objects.
jobSettings_timedMetadataInsertion :: Lens.Lens' JobSettings (Core.Maybe TimedMetadataInsertion)
jobSettings_timedMetadataInsertion = Lens.lens (\JobSettings' {timedMetadataInsertion} -> timedMetadataInsertion) (\s@JobSettings' {} a -> s {timedMetadataInsertion = a} :: JobSettings)

-- | Settings for Event Signaling And Messaging (ESAM).
jobSettings_esam :: Lens.Lens' JobSettings (Core.Maybe EsamSettings)
jobSettings_esam = Lens.lens (\JobSettings' {esam} -> esam) (\s@JobSettings' {} a -> s {esam = a} :: JobSettings)

-- | Ignore these settings unless you are using Nielsen non-linear
-- watermarking. Specify the values that MediaConvert uses to generate and
-- place Nielsen watermarks in your output audio. In addition to specifying
-- these values, you also need to set up your cloud TIC server. These
-- settings apply to every output in your job. The MediaConvert
-- implementation is currently with the following Nielsen versions: Nielsen
-- Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7
-- Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
jobSettings_nielsenNonLinearWatermark :: Lens.Lens' JobSettings (Core.Maybe NielsenNonLinearWatermarkSettings)
jobSettings_nielsenNonLinearWatermark = Lens.lens (\JobSettings' {nielsenNonLinearWatermark} -> nielsenNonLinearWatermark) (\s@JobSettings' {} a -> s {nielsenNonLinearWatermark = a} :: JobSettings)

-- | Overlay motion graphics on top of your video. The motion graphics that
-- you specify here appear on all outputs in all output groups.
jobSettings_motionImageInserter :: Lens.Lens' JobSettings (Core.Maybe MotionImageInserter)
jobSettings_motionImageInserter = Lens.lens (\JobSettings' {motionImageInserter} -> motionImageInserter) (\s@JobSettings' {} a -> s {motionImageInserter = a} :: JobSettings)

-- | Settings for ad avail blanking. Video can be blanked or overlaid with an
-- image, and audio muted during SCTE-35 triggered ad avails.
jobSettings_availBlanking :: Lens.Lens' JobSettings (Core.Maybe AvailBlanking)
jobSettings_availBlanking = Lens.lens (\JobSettings' {availBlanking} -> availBlanking) (\s@JobSettings' {} a -> s {availBlanking = a} :: JobSettings)

-- | Settings for your Nielsen configuration. If you don\'t do Nielsen
-- measurement and analytics, ignore these settings. When you enable
-- Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM
-- to ID3 tagging for all outputs in the job. To enable Nielsen
-- configuration programmatically, include an instance of
-- nielsenConfiguration in your JSON job specification. Even if you don\'t
-- include any children of nielsenConfiguration, you still enable the
-- setting.
jobSettings_nielsenConfiguration :: Lens.Lens' JobSettings (Core.Maybe NielsenConfiguration)
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
jobSettings_outputGroups :: Lens.Lens' JobSettings (Core.Maybe [OutputGroup])
jobSettings_outputGroups = Lens.lens (\JobSettings' {outputGroups} -> outputGroups) (\s@JobSettings' {} a -> s {outputGroups = a} :: JobSettings) Core.. Lens.mapping Lens._Coerce

-- | Use Inputs (inputs) to define source file used in the transcode job.
-- There can be multiple inputs add in a job. These inputs will be
-- concantenated together to create the output.
jobSettings_inputs :: Lens.Lens' JobSettings (Core.Maybe [Input])
jobSettings_inputs = Lens.lens (\JobSettings' {inputs} -> inputs) (\s@JobSettings' {} a -> s {inputs = a} :: JobSettings) Core.. Lens.mapping Lens._Coerce

-- | Contains settings used to acquire and adjust timecode information from
-- inputs.
jobSettings_timecodeConfig :: Lens.Lens' JobSettings (Core.Maybe TimecodeConfig)
jobSettings_timecodeConfig = Lens.lens (\JobSettings' {timecodeConfig} -> timecodeConfig) (\s@JobSettings' {} a -> s {timecodeConfig = a} :: JobSettings)

instance Core.FromJSON JobSettings where
  parseJSON =
    Core.withObject
      "JobSettings"
      ( \x ->
          JobSettings'
            Core.<$> (x Core..:? "adAvailOffset")
            Core.<*> (x Core..:? "timedMetadataInsertion")
            Core.<*> (x Core..:? "esam")
            Core.<*> (x Core..:? "nielsenNonLinearWatermark")
            Core.<*> (x Core..:? "motionImageInserter")
            Core.<*> (x Core..:? "availBlanking")
            Core.<*> (x Core..:? "nielsenConfiguration")
            Core.<*> (x Core..:? "outputGroups" Core..!= Core.mempty)
            Core.<*> (x Core..:? "inputs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "timecodeConfig")
      )

instance Core.Hashable JobSettings

instance Core.NFData JobSettings

instance Core.ToJSON JobSettings where
  toJSON JobSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("adAvailOffset" Core..=) Core.<$> adAvailOffset,
            ("timedMetadataInsertion" Core..=)
              Core.<$> timedMetadataInsertion,
            ("esam" Core..=) Core.<$> esam,
            ("nielsenNonLinearWatermark" Core..=)
              Core.<$> nielsenNonLinearWatermark,
            ("motionImageInserter" Core..=)
              Core.<$> motionImageInserter,
            ("availBlanking" Core..=) Core.<$> availBlanking,
            ("nielsenConfiguration" Core..=)
              Core.<$> nielsenConfiguration,
            ("outputGroups" Core..=) Core.<$> outputGroups,
            ("inputs" Core..=) Core.<$> inputs,
            ("timecodeConfig" Core..=) Core.<$> timecodeConfig
          ]
      )
