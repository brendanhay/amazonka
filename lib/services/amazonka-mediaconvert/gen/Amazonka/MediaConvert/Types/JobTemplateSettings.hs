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
-- Module      : Amazonka.MediaConvert.Types.JobTemplateSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.JobTemplateSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.AvailBlanking
import Amazonka.MediaConvert.Types.EsamSettings
import Amazonka.MediaConvert.Types.ExtendedDataServices
import Amazonka.MediaConvert.Types.InputTemplate
import Amazonka.MediaConvert.Types.KantarWatermarkSettings
import Amazonka.MediaConvert.Types.MotionImageInserter
import Amazonka.MediaConvert.Types.NielsenConfiguration
import Amazonka.MediaConvert.Types.NielsenNonLinearWatermarkSettings
import Amazonka.MediaConvert.Types.OutputGroup
import Amazonka.MediaConvert.Types.TimecodeConfig
import Amazonka.MediaConvert.Types.TimedMetadataInsertion
import qualified Amazonka.Prelude as Prelude

-- | JobTemplateSettings contains all the transcode settings saved in the
-- template that will be applied to jobs created from it.
--
-- /See:/ 'newJobTemplateSettings' smart constructor.
data JobTemplateSettings = JobTemplateSettings'
  { -- | Ignore these settings unless you are using Nielsen non-linear
    -- watermarking. Specify the values that MediaConvert uses to generate and
    -- place Nielsen watermarks in your output audio. In addition to specifying
    -- these values, you also need to set up your cloud TIC server. These
    -- settings apply to every output in your job. The MediaConvert
    -- implementation is currently with the following Nielsen versions: Nielsen
    -- Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7
    -- Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
    nielsenNonLinearWatermark :: Prelude.Maybe NielsenNonLinearWatermarkSettings,
    -- | Settings for Event Signaling And Messaging (ESAM). If you don\'t do ad
    -- insertion, you can ignore these settings.
    esam :: Prelude.Maybe EsamSettings,
    -- | Use Inputs (inputs) to define the source file used in the transcode job.
    -- There can only be one input in a job template. Using the API, you can
    -- include multiple inputs when referencing a job template.
    inputs :: Prelude.Maybe [InputTemplate],
    -- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
    -- tags in any HLS outputs. To include timed metadata, you must enable it
    -- here, enable it in each output container, and specify tags and timecodes
    -- in ID3 insertion (Id3Insertion) objects.
    timedMetadataInsertion :: Prelude.Maybe TimedMetadataInsertion,
    -- | Settings for your Nielsen configuration. If you don\'t do Nielsen
    -- measurement and analytics, ignore these settings. When you enable
    -- Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM
    -- to ID3 tagging for all outputs in the job. To enable Nielsen
    -- configuration programmatically, include an instance of
    -- nielsenConfiguration in your JSON job specification. Even if you don\'t
    -- include any children of nielsenConfiguration, you still enable the
    -- setting.
    nielsenConfiguration :: Prelude.Maybe NielsenConfiguration,
    -- | Settings for ad avail blanking. Video can be blanked or overlaid with an
    -- image, and audio muted during SCTE-35 triggered ad avails.
    availBlanking :: Prelude.Maybe AvailBlanking,
    -- | If your source content has EIA-608 Line 21 Data Services, enable this
    -- feature to specify what MediaConvert does with the Extended Data
    -- Services (XDS) packets. You can choose to pass through XDS packets, or
    -- remove them from the output. For more information about XDS, see EIA-608
    -- Line Data Services, section 9.5.1.5 05h Content Advisory.
    extendedDataServices :: Prelude.Maybe ExtendedDataServices,
    -- | Overlay motion graphics on top of your video. The motion graphics that
    -- you specify here appear on all outputs in all output groups. For more
    -- information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/motion-graphic-overlay.html.
    motionImageInserter :: Prelude.Maybe MotionImageInserter,
    -- | These settings control how the service handles timecodes throughout the
    -- job. These settings don\'t affect input clipping.
    timecodeConfig :: Prelude.Maybe TimecodeConfig,
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
    -- | When specified, this offset (in milliseconds) is added to the input Ad
    -- Avail PTS time.
    adAvailOffset :: Prelude.Maybe Prelude.Int,
    -- | Use these settings only when you use Kantar watermarking. Specify the
    -- values that MediaConvert uses to generate and place Kantar watermarks in
    -- your output audio. These settings apply to every output in your job. In
    -- addition to specifying these values, you also need to store your Kantar
    -- credentials in AWS Secrets Manager. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/kantar-watermarking.html.
    kantarWatermark :: Prelude.Maybe KantarWatermarkSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobTemplateSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'esam', 'jobTemplateSettings_esam' - Settings for Event Signaling And Messaging (ESAM). If you don\'t do ad
-- insertion, you can ignore these settings.
--
-- 'inputs', 'jobTemplateSettings_inputs' - Use Inputs (inputs) to define the source file used in the transcode job.
-- There can only be one input in a job template. Using the API, you can
-- include multiple inputs when referencing a job template.
--
-- 'timedMetadataInsertion', 'jobTemplateSettings_timedMetadataInsertion' - Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
-- tags in any HLS outputs. To include timed metadata, you must enable it
-- here, enable it in each output container, and specify tags and timecodes
-- in ID3 insertion (Id3Insertion) objects.
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
-- 'availBlanking', 'jobTemplateSettings_availBlanking' - Settings for ad avail blanking. Video can be blanked or overlaid with an
-- image, and audio muted during SCTE-35 triggered ad avails.
--
-- 'extendedDataServices', 'jobTemplateSettings_extendedDataServices' - If your source content has EIA-608 Line 21 Data Services, enable this
-- feature to specify what MediaConvert does with the Extended Data
-- Services (XDS) packets. You can choose to pass through XDS packets, or
-- remove them from the output. For more information about XDS, see EIA-608
-- Line Data Services, section 9.5.1.5 05h Content Advisory.
--
-- 'motionImageInserter', 'jobTemplateSettings_motionImageInserter' - Overlay motion graphics on top of your video. The motion graphics that
-- you specify here appear on all outputs in all output groups. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/motion-graphic-overlay.html.
--
-- 'timecodeConfig', 'jobTemplateSettings_timecodeConfig' - These settings control how the service handles timecodes throughout the
-- job. These settings don\'t affect input clipping.
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
-- 'adAvailOffset', 'jobTemplateSettings_adAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time.
--
-- 'kantarWatermark', 'jobTemplateSettings_kantarWatermark' - Use these settings only when you use Kantar watermarking. Specify the
-- values that MediaConvert uses to generate and place Kantar watermarks in
-- your output audio. These settings apply to every output in your job. In
-- addition to specifying these values, you also need to store your Kantar
-- credentials in AWS Secrets Manager. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/kantar-watermarking.html.
newJobTemplateSettings ::
  JobTemplateSettings
newJobTemplateSettings =
  JobTemplateSettings'
    { nielsenNonLinearWatermark =
        Prelude.Nothing,
      esam = Prelude.Nothing,
      inputs = Prelude.Nothing,
      timedMetadataInsertion = Prelude.Nothing,
      nielsenConfiguration = Prelude.Nothing,
      availBlanking = Prelude.Nothing,
      extendedDataServices = Prelude.Nothing,
      motionImageInserter = Prelude.Nothing,
      timecodeConfig = Prelude.Nothing,
      outputGroups = Prelude.Nothing,
      adAvailOffset = Prelude.Nothing,
      kantarWatermark = Prelude.Nothing
    }

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

-- | Settings for Event Signaling And Messaging (ESAM). If you don\'t do ad
-- insertion, you can ignore these settings.
jobTemplateSettings_esam :: Lens.Lens' JobTemplateSettings (Prelude.Maybe EsamSettings)
jobTemplateSettings_esam = Lens.lens (\JobTemplateSettings' {esam} -> esam) (\s@JobTemplateSettings' {} a -> s {esam = a} :: JobTemplateSettings)

-- | Use Inputs (inputs) to define the source file used in the transcode job.
-- There can only be one input in a job template. Using the API, you can
-- include multiple inputs when referencing a job template.
jobTemplateSettings_inputs :: Lens.Lens' JobTemplateSettings (Prelude.Maybe [InputTemplate])
jobTemplateSettings_inputs = Lens.lens (\JobTemplateSettings' {inputs} -> inputs) (\s@JobTemplateSettings' {} a -> s {inputs = a} :: JobTemplateSettings) Prelude.. Lens.mapping Lens.coerced

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
-- tags in any HLS outputs. To include timed metadata, you must enable it
-- here, enable it in each output container, and specify tags and timecodes
-- in ID3 insertion (Id3Insertion) objects.
jobTemplateSettings_timedMetadataInsertion :: Lens.Lens' JobTemplateSettings (Prelude.Maybe TimedMetadataInsertion)
jobTemplateSettings_timedMetadataInsertion = Lens.lens (\JobTemplateSettings' {timedMetadataInsertion} -> timedMetadataInsertion) (\s@JobTemplateSettings' {} a -> s {timedMetadataInsertion = a} :: JobTemplateSettings)

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

-- | Settings for ad avail blanking. Video can be blanked or overlaid with an
-- image, and audio muted during SCTE-35 triggered ad avails.
jobTemplateSettings_availBlanking :: Lens.Lens' JobTemplateSettings (Prelude.Maybe AvailBlanking)
jobTemplateSettings_availBlanking = Lens.lens (\JobTemplateSettings' {availBlanking} -> availBlanking) (\s@JobTemplateSettings' {} a -> s {availBlanking = a} :: JobTemplateSettings)

-- | If your source content has EIA-608 Line 21 Data Services, enable this
-- feature to specify what MediaConvert does with the Extended Data
-- Services (XDS) packets. You can choose to pass through XDS packets, or
-- remove them from the output. For more information about XDS, see EIA-608
-- Line Data Services, section 9.5.1.5 05h Content Advisory.
jobTemplateSettings_extendedDataServices :: Lens.Lens' JobTemplateSettings (Prelude.Maybe ExtendedDataServices)
jobTemplateSettings_extendedDataServices = Lens.lens (\JobTemplateSettings' {extendedDataServices} -> extendedDataServices) (\s@JobTemplateSettings' {} a -> s {extendedDataServices = a} :: JobTemplateSettings)

-- | Overlay motion graphics on top of your video. The motion graphics that
-- you specify here appear on all outputs in all output groups. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/motion-graphic-overlay.html.
jobTemplateSettings_motionImageInserter :: Lens.Lens' JobTemplateSettings (Prelude.Maybe MotionImageInserter)
jobTemplateSettings_motionImageInserter = Lens.lens (\JobTemplateSettings' {motionImageInserter} -> motionImageInserter) (\s@JobTemplateSettings' {} a -> s {motionImageInserter = a} :: JobTemplateSettings)

-- | These settings control how the service handles timecodes throughout the
-- job. These settings don\'t affect input clipping.
jobTemplateSettings_timecodeConfig :: Lens.Lens' JobTemplateSettings (Prelude.Maybe TimecodeConfig)
jobTemplateSettings_timecodeConfig = Lens.lens (\JobTemplateSettings' {timecodeConfig} -> timecodeConfig) (\s@JobTemplateSettings' {} a -> s {timecodeConfig = a} :: JobTemplateSettings)

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
jobTemplateSettings_outputGroups = Lens.lens (\JobTemplateSettings' {outputGroups} -> outputGroups) (\s@JobTemplateSettings' {} a -> s {outputGroups = a} :: JobTemplateSettings) Prelude.. Lens.mapping Lens.coerced

-- | When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time.
jobTemplateSettings_adAvailOffset :: Lens.Lens' JobTemplateSettings (Prelude.Maybe Prelude.Int)
jobTemplateSettings_adAvailOffset = Lens.lens (\JobTemplateSettings' {adAvailOffset} -> adAvailOffset) (\s@JobTemplateSettings' {} a -> s {adAvailOffset = a} :: JobTemplateSettings)

-- | Use these settings only when you use Kantar watermarking. Specify the
-- values that MediaConvert uses to generate and place Kantar watermarks in
-- your output audio. These settings apply to every output in your job. In
-- addition to specifying these values, you also need to store your Kantar
-- credentials in AWS Secrets Manager. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/kantar-watermarking.html.
jobTemplateSettings_kantarWatermark :: Lens.Lens' JobTemplateSettings (Prelude.Maybe KantarWatermarkSettings)
jobTemplateSettings_kantarWatermark = Lens.lens (\JobTemplateSettings' {kantarWatermark} -> kantarWatermark) (\s@JobTemplateSettings' {} a -> s {kantarWatermark = a} :: JobTemplateSettings)

instance Core.FromJSON JobTemplateSettings where
  parseJSON =
    Core.withObject
      "JobTemplateSettings"
      ( \x ->
          JobTemplateSettings'
            Prelude.<$> (x Core..:? "nielsenNonLinearWatermark")
            Prelude.<*> (x Core..:? "esam")
            Prelude.<*> (x Core..:? "inputs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "timedMetadataInsertion")
            Prelude.<*> (x Core..:? "nielsenConfiguration")
            Prelude.<*> (x Core..:? "availBlanking")
            Prelude.<*> (x Core..:? "extendedDataServices")
            Prelude.<*> (x Core..:? "motionImageInserter")
            Prelude.<*> (x Core..:? "timecodeConfig")
            Prelude.<*> (x Core..:? "outputGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "adAvailOffset")
            Prelude.<*> (x Core..:? "kantarWatermark")
      )

instance Prelude.Hashable JobTemplateSettings where
  hashWithSalt _salt JobTemplateSettings' {..} =
    _salt
      `Prelude.hashWithSalt` nielsenNonLinearWatermark
      `Prelude.hashWithSalt` esam
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` timedMetadataInsertion
      `Prelude.hashWithSalt` nielsenConfiguration
      `Prelude.hashWithSalt` availBlanking
      `Prelude.hashWithSalt` extendedDataServices
      `Prelude.hashWithSalt` motionImageInserter
      `Prelude.hashWithSalt` timecodeConfig
      `Prelude.hashWithSalt` outputGroups
      `Prelude.hashWithSalt` adAvailOffset
      `Prelude.hashWithSalt` kantarWatermark

instance Prelude.NFData JobTemplateSettings where
  rnf JobTemplateSettings' {..} =
    Prelude.rnf nielsenNonLinearWatermark
      `Prelude.seq` Prelude.rnf esam
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf timedMetadataInsertion
      `Prelude.seq` Prelude.rnf nielsenConfiguration
      `Prelude.seq` Prelude.rnf availBlanking
      `Prelude.seq` Prelude.rnf extendedDataServices
      `Prelude.seq` Prelude.rnf motionImageInserter
      `Prelude.seq` Prelude.rnf timecodeConfig
      `Prelude.seq` Prelude.rnf outputGroups
      `Prelude.seq` Prelude.rnf adAvailOffset
      `Prelude.seq` Prelude.rnf kantarWatermark

instance Core.ToJSON JobTemplateSettings where
  toJSON JobTemplateSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nielsenNonLinearWatermark" Core..=)
              Prelude.<$> nielsenNonLinearWatermark,
            ("esam" Core..=) Prelude.<$> esam,
            ("inputs" Core..=) Prelude.<$> inputs,
            ("timedMetadataInsertion" Core..=)
              Prelude.<$> timedMetadataInsertion,
            ("nielsenConfiguration" Core..=)
              Prelude.<$> nielsenConfiguration,
            ("availBlanking" Core..=) Prelude.<$> availBlanking,
            ("extendedDataServices" Core..=)
              Prelude.<$> extendedDataServices,
            ("motionImageInserter" Core..=)
              Prelude.<$> motionImageInserter,
            ("timecodeConfig" Core..=)
              Prelude.<$> timecodeConfig,
            ("outputGroups" Core..=) Prelude.<$> outputGroups,
            ("adAvailOffset" Core..=) Prelude.<$> adAvailOffset,
            ("kantarWatermark" Core..=)
              Prelude.<$> kantarWatermark
          ]
      )
