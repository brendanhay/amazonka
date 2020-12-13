{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobTemplateSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobTemplateSettings
  ( JobTemplateSettings (..),

    -- * Smart constructor
    mkJobTemplateSettings,

    -- * Lenses
    jtsNielsenNonLinearWatermark,
    jtsEsam,
    jtsInputs,
    jtsTimedMetadataInsertion,
    jtsNielsenConfiguration,
    jtsAvailBlanking,
    jtsMotionImageInserter,
    jtsTimecodeConfig,
    jtsOutputGroups,
    jtsAdAvailOffset,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
--
-- /See:/ 'mkJobTemplateSettings' smart constructor.
data JobTemplateSettings = JobTemplateSettings'
  { -- | Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
    nielsenNonLinearWatermark :: Lude.Maybe NielsenNonLinearWatermarkSettings,
    -- | Settings for Event Signaling And Messaging (ESAM).
    esam :: Lude.Maybe EsamSettings,
    -- | Use Inputs (inputs) to define the source file used in the transcode job. There can only be one input in a job template.  Using the API, you can include multiple inputs when referencing a job template.
    inputs :: Lude.Maybe [InputTemplate],
    -- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
    timedMetadataInsertion :: Lude.Maybe TimedMetadataInsertion,
    -- | Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
    nielsenConfiguration :: Lude.Maybe NielsenConfiguration,
    -- | Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
    availBlanking :: Lude.Maybe AvailBlanking,
    -- | Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
    motionImageInserter :: Lude.Maybe MotionImageInserter,
    -- | Contains settings used to acquire and adjust timecode information from inputs.
    timecodeConfig :: Lude.Maybe TimecodeConfig,
    -- | (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
    outputGroups :: Lude.Maybe [OutputGroup],
    -- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
    adAvailOffset :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobTemplateSettings' with the minimum fields required to make a request.
--
-- * 'nielsenNonLinearWatermark' - Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
-- * 'esam' - Settings for Event Signaling And Messaging (ESAM).
-- * 'inputs' - Use Inputs (inputs) to define the source file used in the transcode job. There can only be one input in a job template.  Using the API, you can include multiple inputs when referencing a job template.
-- * 'timedMetadataInsertion' - Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
-- * 'nielsenConfiguration' - Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
-- * 'availBlanking' - Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
-- * 'motionImageInserter' - Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
-- * 'timecodeConfig' - Contains settings used to acquire and adjust timecode information from inputs.
-- * 'outputGroups' - (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
-- * 'adAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
mkJobTemplateSettings ::
  JobTemplateSettings
mkJobTemplateSettings =
  JobTemplateSettings'
    { nielsenNonLinearWatermark = Lude.Nothing,
      esam = Lude.Nothing,
      inputs = Lude.Nothing,
      timedMetadataInsertion = Lude.Nothing,
      nielsenConfiguration = Lude.Nothing,
      availBlanking = Lude.Nothing,
      motionImageInserter = Lude.Nothing,
      timecodeConfig = Lude.Nothing,
      outputGroups = Lude.Nothing,
      adAvailOffset = Lude.Nothing
    }

-- | Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
--
-- /Note:/ Consider using 'nielsenNonLinearWatermark' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsNielsenNonLinearWatermark :: Lens.Lens' JobTemplateSettings (Lude.Maybe NielsenNonLinearWatermarkSettings)
jtsNielsenNonLinearWatermark = Lens.lens (nielsenNonLinearWatermark :: JobTemplateSettings -> Lude.Maybe NielsenNonLinearWatermarkSettings) (\s a -> s {nielsenNonLinearWatermark = a} :: JobTemplateSettings)
{-# DEPRECATED jtsNielsenNonLinearWatermark "Use generic-lens or generic-optics with 'nielsenNonLinearWatermark' instead." #-}

-- | Settings for Event Signaling And Messaging (ESAM).
--
-- /Note:/ Consider using 'esam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsEsam :: Lens.Lens' JobTemplateSettings (Lude.Maybe EsamSettings)
jtsEsam = Lens.lens (esam :: JobTemplateSettings -> Lude.Maybe EsamSettings) (\s a -> s {esam = a} :: JobTemplateSettings)
{-# DEPRECATED jtsEsam "Use generic-lens or generic-optics with 'esam' instead." #-}

-- | Use Inputs (inputs) to define the source file used in the transcode job. There can only be one input in a job template.  Using the API, you can include multiple inputs when referencing a job template.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsInputs :: Lens.Lens' JobTemplateSettings (Lude.Maybe [InputTemplate])
jtsInputs = Lens.lens (inputs :: JobTemplateSettings -> Lude.Maybe [InputTemplate]) (\s a -> s {inputs = a} :: JobTemplateSettings)
{-# DEPRECATED jtsInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
--
-- /Note:/ Consider using 'timedMetadataInsertion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsTimedMetadataInsertion :: Lens.Lens' JobTemplateSettings (Lude.Maybe TimedMetadataInsertion)
jtsTimedMetadataInsertion = Lens.lens (timedMetadataInsertion :: JobTemplateSettings -> Lude.Maybe TimedMetadataInsertion) (\s a -> s {timedMetadataInsertion = a} :: JobTemplateSettings)
{-# DEPRECATED jtsTimedMetadataInsertion "Use generic-lens or generic-optics with 'timedMetadataInsertion' instead." #-}

-- | Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
--
-- /Note:/ Consider using 'nielsenConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsNielsenConfiguration :: Lens.Lens' JobTemplateSettings (Lude.Maybe NielsenConfiguration)
jtsNielsenConfiguration = Lens.lens (nielsenConfiguration :: JobTemplateSettings -> Lude.Maybe NielsenConfiguration) (\s a -> s {nielsenConfiguration = a} :: JobTemplateSettings)
{-# DEPRECATED jtsNielsenConfiguration "Use generic-lens or generic-optics with 'nielsenConfiguration' instead." #-}

-- | Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
--
-- /Note:/ Consider using 'availBlanking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsAvailBlanking :: Lens.Lens' JobTemplateSettings (Lude.Maybe AvailBlanking)
jtsAvailBlanking = Lens.lens (availBlanking :: JobTemplateSettings -> Lude.Maybe AvailBlanking) (\s a -> s {availBlanking = a} :: JobTemplateSettings)
{-# DEPRECATED jtsAvailBlanking "Use generic-lens or generic-optics with 'availBlanking' instead." #-}

-- | Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
--
-- /Note:/ Consider using 'motionImageInserter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsMotionImageInserter :: Lens.Lens' JobTemplateSettings (Lude.Maybe MotionImageInserter)
jtsMotionImageInserter = Lens.lens (motionImageInserter :: JobTemplateSettings -> Lude.Maybe MotionImageInserter) (\s a -> s {motionImageInserter = a} :: JobTemplateSettings)
{-# DEPRECATED jtsMotionImageInserter "Use generic-lens or generic-optics with 'motionImageInserter' instead." #-}

-- | Contains settings used to acquire and adjust timecode information from inputs.
--
-- /Note:/ Consider using 'timecodeConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsTimecodeConfig :: Lens.Lens' JobTemplateSettings (Lude.Maybe TimecodeConfig)
jtsTimecodeConfig = Lens.lens (timecodeConfig :: JobTemplateSettings -> Lude.Maybe TimecodeConfig) (\s a -> s {timecodeConfig = a} :: JobTemplateSettings)
{-# DEPRECATED jtsTimecodeConfig "Use generic-lens or generic-optics with 'timecodeConfig' instead." #-}

-- | (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
--
-- /Note:/ Consider using 'outputGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsOutputGroups :: Lens.Lens' JobTemplateSettings (Lude.Maybe [OutputGroup])
jtsOutputGroups = Lens.lens (outputGroups :: JobTemplateSettings -> Lude.Maybe [OutputGroup]) (\s a -> s {outputGroups = a} :: JobTemplateSettings)
{-# DEPRECATED jtsOutputGroups "Use generic-lens or generic-optics with 'outputGroups' instead." #-}

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
--
-- /Note:/ Consider using 'adAvailOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsAdAvailOffset :: Lens.Lens' JobTemplateSettings (Lude.Maybe Lude.Int)
jtsAdAvailOffset = Lens.lens (adAvailOffset :: JobTemplateSettings -> Lude.Maybe Lude.Int) (\s a -> s {adAvailOffset = a} :: JobTemplateSettings)
{-# DEPRECATED jtsAdAvailOffset "Use generic-lens or generic-optics with 'adAvailOffset' instead." #-}

instance Lude.FromJSON JobTemplateSettings where
  parseJSON =
    Lude.withObject
      "JobTemplateSettings"
      ( \x ->
          JobTemplateSettings'
            Lude.<$> (x Lude..:? "nielsenNonLinearWatermark")
            Lude.<*> (x Lude..:? "esam")
            Lude.<*> (x Lude..:? "inputs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "timedMetadataInsertion")
            Lude.<*> (x Lude..:? "nielsenConfiguration")
            Lude.<*> (x Lude..:? "availBlanking")
            Lude.<*> (x Lude..:? "motionImageInserter")
            Lude.<*> (x Lude..:? "timecodeConfig")
            Lude.<*> (x Lude..:? "outputGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "adAvailOffset")
      )

instance Lude.ToJSON JobTemplateSettings where
  toJSON JobTemplateSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nielsenNonLinearWatermark" Lude..=)
              Lude.<$> nielsenNonLinearWatermark,
            ("esam" Lude..=) Lude.<$> esam,
            ("inputs" Lude..=) Lude.<$> inputs,
            ("timedMetadataInsertion" Lude..=) Lude.<$> timedMetadataInsertion,
            ("nielsenConfiguration" Lude..=) Lude.<$> nielsenConfiguration,
            ("availBlanking" Lude..=) Lude.<$> availBlanking,
            ("motionImageInserter" Lude..=) Lude.<$> motionImageInserter,
            ("timecodeConfig" Lude..=) Lude.<$> timecodeConfig,
            ("outputGroups" Lude..=) Lude.<$> outputGroups,
            ("adAvailOffset" Lude..=) Lude.<$> adAvailOffset
          ]
      )
