{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobSettings
  ( JobSettings (..),

    -- * Smart constructor
    mkJobSettings,

    -- * Lenses
    jsNielsenNonLinearWatermark,
    jsEsam,
    jsInputs,
    jsTimedMetadataInsertion,
    jsNielsenConfiguration,
    jsAvailBlanking,
    jsMotionImageInserter,
    jsTimecodeConfig,
    jsOutputGroups,
    jsAdAvailOffset,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | JobSettings contains all the transcode settings for a job.
--
-- /See:/ 'mkJobSettings' smart constructor.
data JobSettings = JobSettings'
  { nielsenNonLinearWatermark ::
      Lude.Maybe NielsenNonLinearWatermarkSettings,
    esam :: Lude.Maybe EsamSettings,
    inputs :: Lude.Maybe [Input],
    timedMetadataInsertion :: Lude.Maybe TimedMetadataInsertion,
    nielsenConfiguration :: Lude.Maybe NielsenConfiguration,
    availBlanking :: Lude.Maybe AvailBlanking,
    motionImageInserter :: Lude.Maybe MotionImageInserter,
    timecodeConfig :: Lude.Maybe TimecodeConfig,
    outputGroups :: Lude.Maybe [OutputGroup],
    adAvailOffset :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobSettings' with the minimum fields required to make a request.
--
-- * 'adAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
-- * 'availBlanking' - Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
-- * 'esam' - Settings for Event Signaling And Messaging (ESAM).
-- * 'inputs' - Use Inputs (inputs) to define source file used in the transcode job. There can be multiple inputs add in a job. These inputs will be concantenated together to create the output.
-- * 'motionImageInserter' - Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
-- * 'nielsenConfiguration' - Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
-- * 'nielsenNonLinearWatermark' - Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
-- * 'outputGroups' - (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
-- * 'timecodeConfig' - Contains settings used to acquire and adjust timecode information from inputs.
-- * 'timedMetadataInsertion' - Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
mkJobSettings ::
  JobSettings
mkJobSettings =
  JobSettings'
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
jsNielsenNonLinearWatermark :: Lens.Lens' JobSettings (Lude.Maybe NielsenNonLinearWatermarkSettings)
jsNielsenNonLinearWatermark = Lens.lens (nielsenNonLinearWatermark :: JobSettings -> Lude.Maybe NielsenNonLinearWatermarkSettings) (\s a -> s {nielsenNonLinearWatermark = a} :: JobSettings)
{-# DEPRECATED jsNielsenNonLinearWatermark "Use generic-lens or generic-optics with 'nielsenNonLinearWatermark' instead." #-}

-- | Settings for Event Signaling And Messaging (ESAM).
--
-- /Note:/ Consider using 'esam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsEsam :: Lens.Lens' JobSettings (Lude.Maybe EsamSettings)
jsEsam = Lens.lens (esam :: JobSettings -> Lude.Maybe EsamSettings) (\s a -> s {esam = a} :: JobSettings)
{-# DEPRECATED jsEsam "Use generic-lens or generic-optics with 'esam' instead." #-}

-- | Use Inputs (inputs) to define source file used in the transcode job. There can be multiple inputs add in a job. These inputs will be concantenated together to create the output.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsInputs :: Lens.Lens' JobSettings (Lude.Maybe [Input])
jsInputs = Lens.lens (inputs :: JobSettings -> Lude.Maybe [Input]) (\s a -> s {inputs = a} :: JobSettings)
{-# DEPRECATED jsInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
--
-- /Note:/ Consider using 'timedMetadataInsertion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsTimedMetadataInsertion :: Lens.Lens' JobSettings (Lude.Maybe TimedMetadataInsertion)
jsTimedMetadataInsertion = Lens.lens (timedMetadataInsertion :: JobSettings -> Lude.Maybe TimedMetadataInsertion) (\s a -> s {timedMetadataInsertion = a} :: JobSettings)
{-# DEPRECATED jsTimedMetadataInsertion "Use generic-lens or generic-optics with 'timedMetadataInsertion' instead." #-}

-- | Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
--
-- /Note:/ Consider using 'nielsenConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsNielsenConfiguration :: Lens.Lens' JobSettings (Lude.Maybe NielsenConfiguration)
jsNielsenConfiguration = Lens.lens (nielsenConfiguration :: JobSettings -> Lude.Maybe NielsenConfiguration) (\s a -> s {nielsenConfiguration = a} :: JobSettings)
{-# DEPRECATED jsNielsenConfiguration "Use generic-lens or generic-optics with 'nielsenConfiguration' instead." #-}

-- | Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
--
-- /Note:/ Consider using 'availBlanking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsAvailBlanking :: Lens.Lens' JobSettings (Lude.Maybe AvailBlanking)
jsAvailBlanking = Lens.lens (availBlanking :: JobSettings -> Lude.Maybe AvailBlanking) (\s a -> s {availBlanking = a} :: JobSettings)
{-# DEPRECATED jsAvailBlanking "Use generic-lens or generic-optics with 'availBlanking' instead." #-}

-- | Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
--
-- /Note:/ Consider using 'motionImageInserter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsMotionImageInserter :: Lens.Lens' JobSettings (Lude.Maybe MotionImageInserter)
jsMotionImageInserter = Lens.lens (motionImageInserter :: JobSettings -> Lude.Maybe MotionImageInserter) (\s a -> s {motionImageInserter = a} :: JobSettings)
{-# DEPRECATED jsMotionImageInserter "Use generic-lens or generic-optics with 'motionImageInserter' instead." #-}

-- | Contains settings used to acquire and adjust timecode information from inputs.
--
-- /Note:/ Consider using 'timecodeConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsTimecodeConfig :: Lens.Lens' JobSettings (Lude.Maybe TimecodeConfig)
jsTimecodeConfig = Lens.lens (timecodeConfig :: JobSettings -> Lude.Maybe TimecodeConfig) (\s a -> s {timecodeConfig = a} :: JobSettings)
{-# DEPRECATED jsTimecodeConfig "Use generic-lens or generic-optics with 'timecodeConfig' instead." #-}

-- | (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
--
-- /Note:/ Consider using 'outputGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsOutputGroups :: Lens.Lens' JobSettings (Lude.Maybe [OutputGroup])
jsOutputGroups = Lens.lens (outputGroups :: JobSettings -> Lude.Maybe [OutputGroup]) (\s a -> s {outputGroups = a} :: JobSettings)
{-# DEPRECATED jsOutputGroups "Use generic-lens or generic-optics with 'outputGroups' instead." #-}

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
--
-- /Note:/ Consider using 'adAvailOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsAdAvailOffset :: Lens.Lens' JobSettings (Lude.Maybe Lude.Int)
jsAdAvailOffset = Lens.lens (adAvailOffset :: JobSettings -> Lude.Maybe Lude.Int) (\s a -> s {adAvailOffset = a} :: JobSettings)
{-# DEPRECATED jsAdAvailOffset "Use generic-lens or generic-optics with 'adAvailOffset' instead." #-}

instance Lude.FromJSON JobSettings where
  parseJSON =
    Lude.withObject
      "JobSettings"
      ( \x ->
          JobSettings'
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

instance Lude.ToJSON JobSettings where
  toJSON JobSettings' {..} =
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
