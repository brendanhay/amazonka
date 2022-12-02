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
-- Module      : Amazonka.MediaConvert.Types.JobSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.JobSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AvailBlanking
import Amazonka.MediaConvert.Types.EsamSettings
import Amazonka.MediaConvert.Types.ExtendedDataServices
import Amazonka.MediaConvert.Types.Input
import Amazonka.MediaConvert.Types.KantarWatermarkSettings
import Amazonka.MediaConvert.Types.MotionImageInserter
import Amazonka.MediaConvert.Types.NielsenConfiguration
import Amazonka.MediaConvert.Types.NielsenNonLinearWatermarkSettings
import Amazonka.MediaConvert.Types.OutputGroup
import Amazonka.MediaConvert.Types.TimecodeConfig
import Amazonka.MediaConvert.Types.TimedMetadataInsertion
import qualified Amazonka.Prelude as Prelude

-- | JobSettings contains all the transcode settings for a job.
--
-- /See:/ 'newJobSettings' smart constructor.
data JobSettings = JobSettings'
  { -- | Settings for ad avail blanking. Video can be blanked or overlaid with an
    -- image, and audio muted during SCTE-35 triggered ad avails.
    availBlanking :: Prelude.Maybe AvailBlanking,
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
    -- | Insert user-defined custom ID3 metadata (id3) at timecodes (timecode)
    -- that you specify. In each output that you want to include this metadata,
    -- you must set ID3 metadata (timedMetadata) to Passthrough (PASSTHROUGH).
    timedMetadataInsertion :: Prelude.Maybe TimedMetadataInsertion,
    -- | If your source content has EIA-608 Line 21 Data Services, enable this
    -- feature to specify what MediaConvert does with the Extended Data
    -- Services (XDS) packets. You can choose to pass through XDS packets, or
    -- remove them from the output. For more information about XDS, see EIA-608
    -- Line Data Services, section 9.5.1.5 05h Content Advisory.
    extendedDataServices :: Prelude.Maybe ExtendedDataServices,
    -- | Use these settings only when you use Kantar watermarking. Specify the
    -- values that MediaConvert uses to generate and place Kantar watermarks in
    -- your output audio. These settings apply to every output in your job. In
    -- addition to specifying these values, you also need to store your Kantar
    -- credentials in AWS Secrets Manager. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/kantar-watermarking.html.
    kantarWatermark :: Prelude.Maybe KantarWatermarkSettings,
    -- | Use Inputs (inputs) to define source file used in the transcode job.
    -- There can be multiple inputs add in a job. These inputs will be
    -- concantenated together to create the output.
    inputs :: Prelude.Maybe [Input],
    -- | These settings control how the service handles timecodes throughout the
    -- job. These settings don\'t affect input clipping.
    timecodeConfig :: Prelude.Maybe TimecodeConfig,
    -- | When specified, this offset (in milliseconds) is added to the input Ad
    -- Avail PTS time.
    adAvailOffset :: Prelude.Maybe Prelude.Int
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
-- 'availBlanking', 'jobSettings_availBlanking' - Settings for ad avail blanking. Video can be blanked or overlaid with an
-- image, and audio muted during SCTE-35 triggered ad avails.
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
-- 'timedMetadataInsertion', 'jobSettings_timedMetadataInsertion' - Insert user-defined custom ID3 metadata (id3) at timecodes (timecode)
-- that you specify. In each output that you want to include this metadata,
-- you must set ID3 metadata (timedMetadata) to Passthrough (PASSTHROUGH).
--
-- 'extendedDataServices', 'jobSettings_extendedDataServices' - If your source content has EIA-608 Line 21 Data Services, enable this
-- feature to specify what MediaConvert does with the Extended Data
-- Services (XDS) packets. You can choose to pass through XDS packets, or
-- remove them from the output. For more information about XDS, see EIA-608
-- Line Data Services, section 9.5.1.5 05h Content Advisory.
--
-- 'kantarWatermark', 'jobSettings_kantarWatermark' - Use these settings only when you use Kantar watermarking. Specify the
-- values that MediaConvert uses to generate and place Kantar watermarks in
-- your output audio. These settings apply to every output in your job. In
-- addition to specifying these values, you also need to store your Kantar
-- credentials in AWS Secrets Manager. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/kantar-watermarking.html.
--
-- 'inputs', 'jobSettings_inputs' - Use Inputs (inputs) to define source file used in the transcode job.
-- There can be multiple inputs add in a job. These inputs will be
-- concantenated together to create the output.
--
-- 'timecodeConfig', 'jobSettings_timecodeConfig' - These settings control how the service handles timecodes throughout the
-- job. These settings don\'t affect input clipping.
--
-- 'adAvailOffset', 'jobSettings_adAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time.
newJobSettings ::
  JobSettings
newJobSettings =
  JobSettings'
    { availBlanking = Prelude.Nothing,
      esam = Prelude.Nothing,
      nielsenNonLinearWatermark = Prelude.Nothing,
      motionImageInserter = Prelude.Nothing,
      nielsenConfiguration = Prelude.Nothing,
      outputGroups = Prelude.Nothing,
      timedMetadataInsertion = Prelude.Nothing,
      extendedDataServices = Prelude.Nothing,
      kantarWatermark = Prelude.Nothing,
      inputs = Prelude.Nothing,
      timecodeConfig = Prelude.Nothing,
      adAvailOffset = Prelude.Nothing
    }

-- | Settings for ad avail blanking. Video can be blanked or overlaid with an
-- image, and audio muted during SCTE-35 triggered ad avails.
jobSettings_availBlanking :: Lens.Lens' JobSettings (Prelude.Maybe AvailBlanking)
jobSettings_availBlanking = Lens.lens (\JobSettings' {availBlanking} -> availBlanking) (\s@JobSettings' {} a -> s {availBlanking = a} :: JobSettings)

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
jobSettings_outputGroups = Lens.lens (\JobSettings' {outputGroups} -> outputGroups) (\s@JobSettings' {} a -> s {outputGroups = a} :: JobSettings) Prelude.. Lens.mapping Lens.coerced

-- | Insert user-defined custom ID3 metadata (id3) at timecodes (timecode)
-- that you specify. In each output that you want to include this metadata,
-- you must set ID3 metadata (timedMetadata) to Passthrough (PASSTHROUGH).
jobSettings_timedMetadataInsertion :: Lens.Lens' JobSettings (Prelude.Maybe TimedMetadataInsertion)
jobSettings_timedMetadataInsertion = Lens.lens (\JobSettings' {timedMetadataInsertion} -> timedMetadataInsertion) (\s@JobSettings' {} a -> s {timedMetadataInsertion = a} :: JobSettings)

-- | If your source content has EIA-608 Line 21 Data Services, enable this
-- feature to specify what MediaConvert does with the Extended Data
-- Services (XDS) packets. You can choose to pass through XDS packets, or
-- remove them from the output. For more information about XDS, see EIA-608
-- Line Data Services, section 9.5.1.5 05h Content Advisory.
jobSettings_extendedDataServices :: Lens.Lens' JobSettings (Prelude.Maybe ExtendedDataServices)
jobSettings_extendedDataServices = Lens.lens (\JobSettings' {extendedDataServices} -> extendedDataServices) (\s@JobSettings' {} a -> s {extendedDataServices = a} :: JobSettings)

-- | Use these settings only when you use Kantar watermarking. Specify the
-- values that MediaConvert uses to generate and place Kantar watermarks in
-- your output audio. These settings apply to every output in your job. In
-- addition to specifying these values, you also need to store your Kantar
-- credentials in AWS Secrets Manager. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/kantar-watermarking.html.
jobSettings_kantarWatermark :: Lens.Lens' JobSettings (Prelude.Maybe KantarWatermarkSettings)
jobSettings_kantarWatermark = Lens.lens (\JobSettings' {kantarWatermark} -> kantarWatermark) (\s@JobSettings' {} a -> s {kantarWatermark = a} :: JobSettings)

-- | Use Inputs (inputs) to define source file used in the transcode job.
-- There can be multiple inputs add in a job. These inputs will be
-- concantenated together to create the output.
jobSettings_inputs :: Lens.Lens' JobSettings (Prelude.Maybe [Input])
jobSettings_inputs = Lens.lens (\JobSettings' {inputs} -> inputs) (\s@JobSettings' {} a -> s {inputs = a} :: JobSettings) Prelude.. Lens.mapping Lens.coerced

-- | These settings control how the service handles timecodes throughout the
-- job. These settings don\'t affect input clipping.
jobSettings_timecodeConfig :: Lens.Lens' JobSettings (Prelude.Maybe TimecodeConfig)
jobSettings_timecodeConfig = Lens.lens (\JobSettings' {timecodeConfig} -> timecodeConfig) (\s@JobSettings' {} a -> s {timecodeConfig = a} :: JobSettings)

-- | When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time.
jobSettings_adAvailOffset :: Lens.Lens' JobSettings (Prelude.Maybe Prelude.Int)
jobSettings_adAvailOffset = Lens.lens (\JobSettings' {adAvailOffset} -> adAvailOffset) (\s@JobSettings' {} a -> s {adAvailOffset = a} :: JobSettings)

instance Data.FromJSON JobSettings where
  parseJSON =
    Data.withObject
      "JobSettings"
      ( \x ->
          JobSettings'
            Prelude.<$> (x Data..:? "availBlanking")
            Prelude.<*> (x Data..:? "esam")
            Prelude.<*> (x Data..:? "nielsenNonLinearWatermark")
            Prelude.<*> (x Data..:? "motionImageInserter")
            Prelude.<*> (x Data..:? "nielsenConfiguration")
            Prelude.<*> (x Data..:? "outputGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "timedMetadataInsertion")
            Prelude.<*> (x Data..:? "extendedDataServices")
            Prelude.<*> (x Data..:? "kantarWatermark")
            Prelude.<*> (x Data..:? "inputs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "timecodeConfig")
            Prelude.<*> (x Data..:? "adAvailOffset")
      )

instance Prelude.Hashable JobSettings where
  hashWithSalt _salt JobSettings' {..} =
    _salt `Prelude.hashWithSalt` availBlanking
      `Prelude.hashWithSalt` esam
      `Prelude.hashWithSalt` nielsenNonLinearWatermark
      `Prelude.hashWithSalt` motionImageInserter
      `Prelude.hashWithSalt` nielsenConfiguration
      `Prelude.hashWithSalt` outputGroups
      `Prelude.hashWithSalt` timedMetadataInsertion
      `Prelude.hashWithSalt` extendedDataServices
      `Prelude.hashWithSalt` kantarWatermark
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` timecodeConfig
      `Prelude.hashWithSalt` adAvailOffset

instance Prelude.NFData JobSettings where
  rnf JobSettings' {..} =
    Prelude.rnf availBlanking
      `Prelude.seq` Prelude.rnf esam
      `Prelude.seq` Prelude.rnf nielsenNonLinearWatermark
      `Prelude.seq` Prelude.rnf motionImageInserter
      `Prelude.seq` Prelude.rnf nielsenConfiguration
      `Prelude.seq` Prelude.rnf outputGroups
      `Prelude.seq` Prelude.rnf timedMetadataInsertion
      `Prelude.seq` Prelude.rnf extendedDataServices
      `Prelude.seq` Prelude.rnf kantarWatermark
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf timecodeConfig
      `Prelude.seq` Prelude.rnf adAvailOffset

instance Data.ToJSON JobSettings where
  toJSON JobSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("availBlanking" Data..=) Prelude.<$> availBlanking,
            ("esam" Data..=) Prelude.<$> esam,
            ("nielsenNonLinearWatermark" Data..=)
              Prelude.<$> nielsenNonLinearWatermark,
            ("motionImageInserter" Data..=)
              Prelude.<$> motionImageInserter,
            ("nielsenConfiguration" Data..=)
              Prelude.<$> nielsenConfiguration,
            ("outputGroups" Data..=) Prelude.<$> outputGroups,
            ("timedMetadataInsertion" Data..=)
              Prelude.<$> timedMetadataInsertion,
            ("extendedDataServices" Data..=)
              Prelude.<$> extendedDataServices,
            ("kantarWatermark" Data..=)
              Prelude.<$> kantarWatermark,
            ("inputs" Data..=) Prelude.<$> inputs,
            ("timecodeConfig" Data..=)
              Prelude.<$> timecodeConfig,
            ("adAvailOffset" Data..=) Prelude.<$> adAvailOffset
          ]
      )
