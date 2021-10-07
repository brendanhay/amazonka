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
import Network.AWS.MediaConvert.Types.ExtendedDataServices
import Network.AWS.MediaConvert.Types.Input
import Network.AWS.MediaConvert.Types.KantarWatermarkSettings
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
  { -- | If your source content has EIA-608 Line 21 Data Services, enable this
    -- feature to specify what MediaConvert does with the Extended Data
    -- Services (XDS) packets. You can choose to pass through XDS packets, or
    -- remove them from the output. For more information about XDS, see EIA-608
    -- Line Data Services, section 9.5.1.5 05h Content Advisory.
    extendedDataServices :: Prelude.Maybe ExtendedDataServices,
    -- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
    -- tags in any HLS outputs. To include timed metadata, you must enable it
    -- here, enable it in each output container, and specify tags and timecodes
    -- in ID3 insertion (Id3Insertion) objects.
    timedMetadataInsertion :: Prelude.Maybe TimedMetadataInsertion,
    -- | Use these settings only when you use Kantar watermarking. Specify the
    -- values that MediaConvert uses to generate and place Kantar watermarks in
    -- your output audio. These settings apply to every output in your job. In
    -- addition to specifying these values, you also need to store your Kantar
    -- credentials in AWS Secrets Manager. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/kantar-watermarking.html.
    kantarWatermark :: Prelude.Maybe KantarWatermarkSettings,
    -- | When specified, this offset (in milliseconds) is added to the input Ad
    -- Avail PTS time.
    adAvailOffset :: Prelude.Maybe Prelude.Int,
    -- | Settings for Event Signaling And Messaging (ESAM). If you don\'t do ad
    -- insertion, you can ignore these settings.
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
    -- you specify here appear on all outputs in all output groups. For more
    -- information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/motion-graphic-overlay.html.
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
    -- | These settings control how the service handles timecodes throughout the
    -- job. These settings don\'t affect input clipping.
    timecodeConfig :: Prelude.Maybe TimecodeConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extendedDataServices', 'jobSettings_extendedDataServices' - If your source content has EIA-608 Line 21 Data Services, enable this
-- feature to specify what MediaConvert does with the Extended Data
-- Services (XDS) packets. You can choose to pass through XDS packets, or
-- remove them from the output. For more information about XDS, see EIA-608
-- Line Data Services, section 9.5.1.5 05h Content Advisory.
--
-- 'timedMetadataInsertion', 'jobSettings_timedMetadataInsertion' - Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
-- tags in any HLS outputs. To include timed metadata, you must enable it
-- here, enable it in each output container, and specify tags and timecodes
-- in ID3 insertion (Id3Insertion) objects.
--
-- 'kantarWatermark', 'jobSettings_kantarWatermark' - Use these settings only when you use Kantar watermarking. Specify the
-- values that MediaConvert uses to generate and place Kantar watermarks in
-- your output audio. These settings apply to every output in your job. In
-- addition to specifying these values, you also need to store your Kantar
-- credentials in AWS Secrets Manager. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/kantar-watermarking.html.
--
-- 'adAvailOffset', 'jobSettings_adAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time.
--
-- 'esam', 'jobSettings_esam' - Settings for Event Signaling And Messaging (ESAM). If you don\'t do ad
-- insertion, you can ignore these settings.
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
-- you specify here appear on all outputs in all output groups. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/motion-graphic-overlay.html.
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
-- 'timecodeConfig', 'jobSettings_timecodeConfig' - These settings control how the service handles timecodes throughout the
-- job. These settings don\'t affect input clipping.
newJobSettings ::
  JobSettings
newJobSettings =
  JobSettings'
    { extendedDataServices =
        Prelude.Nothing,
      timedMetadataInsertion = Prelude.Nothing,
      kantarWatermark = Prelude.Nothing,
      adAvailOffset = Prelude.Nothing,
      esam = Prelude.Nothing,
      nielsenNonLinearWatermark = Prelude.Nothing,
      motionImageInserter = Prelude.Nothing,
      availBlanking = Prelude.Nothing,
      nielsenConfiguration = Prelude.Nothing,
      outputGroups = Prelude.Nothing,
      inputs = Prelude.Nothing,
      timecodeConfig = Prelude.Nothing
    }

-- | If your source content has EIA-608 Line 21 Data Services, enable this
-- feature to specify what MediaConvert does with the Extended Data
-- Services (XDS) packets. You can choose to pass through XDS packets, or
-- remove them from the output. For more information about XDS, see EIA-608
-- Line Data Services, section 9.5.1.5 05h Content Advisory.
jobSettings_extendedDataServices :: Lens.Lens' JobSettings (Prelude.Maybe ExtendedDataServices)
jobSettings_extendedDataServices = Lens.lens (\JobSettings' {extendedDataServices} -> extendedDataServices) (\s@JobSettings' {} a -> s {extendedDataServices = a} :: JobSettings)

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
-- tags in any HLS outputs. To include timed metadata, you must enable it
-- here, enable it in each output container, and specify tags and timecodes
-- in ID3 insertion (Id3Insertion) objects.
jobSettings_timedMetadataInsertion :: Lens.Lens' JobSettings (Prelude.Maybe TimedMetadataInsertion)
jobSettings_timedMetadataInsertion = Lens.lens (\JobSettings' {timedMetadataInsertion} -> timedMetadataInsertion) (\s@JobSettings' {} a -> s {timedMetadataInsertion = a} :: JobSettings)

-- | Use these settings only when you use Kantar watermarking. Specify the
-- values that MediaConvert uses to generate and place Kantar watermarks in
-- your output audio. These settings apply to every output in your job. In
-- addition to specifying these values, you also need to store your Kantar
-- credentials in AWS Secrets Manager. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/kantar-watermarking.html.
jobSettings_kantarWatermark :: Lens.Lens' JobSettings (Prelude.Maybe KantarWatermarkSettings)
jobSettings_kantarWatermark = Lens.lens (\JobSettings' {kantarWatermark} -> kantarWatermark) (\s@JobSettings' {} a -> s {kantarWatermark = a} :: JobSettings)

-- | When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time.
jobSettings_adAvailOffset :: Lens.Lens' JobSettings (Prelude.Maybe Prelude.Int)
jobSettings_adAvailOffset = Lens.lens (\JobSettings' {adAvailOffset} -> adAvailOffset) (\s@JobSettings' {} a -> s {adAvailOffset = a} :: JobSettings)

-- | Settings for Event Signaling And Messaging (ESAM). If you don\'t do ad
-- insertion, you can ignore these settings.
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
-- you specify here appear on all outputs in all output groups. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/motion-graphic-overlay.html.
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
jobSettings_outputGroups = Lens.lens (\JobSettings' {outputGroups} -> outputGroups) (\s@JobSettings' {} a -> s {outputGroups = a} :: JobSettings) Prelude.. Lens.mapping Lens._Coerce

-- | Use Inputs (inputs) to define source file used in the transcode job.
-- There can be multiple inputs add in a job. These inputs will be
-- concantenated together to create the output.
jobSettings_inputs :: Lens.Lens' JobSettings (Prelude.Maybe [Input])
jobSettings_inputs = Lens.lens (\JobSettings' {inputs} -> inputs) (\s@JobSettings' {} a -> s {inputs = a} :: JobSettings) Prelude.. Lens.mapping Lens._Coerce

-- | These settings control how the service handles timecodes throughout the
-- job. These settings don\'t affect input clipping.
jobSettings_timecodeConfig :: Lens.Lens' JobSettings (Prelude.Maybe TimecodeConfig)
jobSettings_timecodeConfig = Lens.lens (\JobSettings' {timecodeConfig} -> timecodeConfig) (\s@JobSettings' {} a -> s {timecodeConfig = a} :: JobSettings)

instance Core.FromJSON JobSettings where
  parseJSON =
    Core.withObject
      "JobSettings"
      ( \x ->
          JobSettings'
            Prelude.<$> (x Core..:? "extendedDataServices")
            Prelude.<*> (x Core..:? "timedMetadataInsertion")
            Prelude.<*> (x Core..:? "kantarWatermark")
            Prelude.<*> (x Core..:? "adAvailOffset")
            Prelude.<*> (x Core..:? "esam")
            Prelude.<*> (x Core..:? "nielsenNonLinearWatermark")
            Prelude.<*> (x Core..:? "motionImageInserter")
            Prelude.<*> (x Core..:? "availBlanking")
            Prelude.<*> (x Core..:? "nielsenConfiguration")
            Prelude.<*> (x Core..:? "outputGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "inputs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "timecodeConfig")
      )

instance Prelude.Hashable JobSettings

instance Prelude.NFData JobSettings

instance Core.ToJSON JobSettings where
  toJSON JobSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("extendedDataServices" Core..=)
              Prelude.<$> extendedDataServices,
            ("timedMetadataInsertion" Core..=)
              Prelude.<$> timedMetadataInsertion,
            ("kantarWatermark" Core..=)
              Prelude.<$> kantarWatermark,
            ("adAvailOffset" Core..=) Prelude.<$> adAvailOffset,
            ("esam" Core..=) Prelude.<$> esam,
            ("nielsenNonLinearWatermark" Core..=)
              Prelude.<$> nielsenNonLinearWatermark,
            ("motionImageInserter" Core..=)
              Prelude.<$> motionImageInserter,
            ("availBlanking" Core..=) Prelude.<$> availBlanking,
            ("nielsenConfiguration" Core..=)
              Prelude.<$> nielsenConfiguration,
            ("outputGroups" Core..=) Prelude.<$> outputGroups,
            ("inputs" Core..=) Prelude.<$> inputs,
            ("timecodeConfig" Core..=)
              Prelude.<$> timecodeConfig
          ]
      )
