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
    jtsAdAvailOffset,
    jtsAvailBlanking,
    jtsEsam,
    jtsInputs,
    jtsMotionImageInserter,
    jtsNielsenConfiguration,
    jtsNielsenNonLinearWatermark,
    jtsOutputGroups,
    jtsTimecodeConfig,
    jtsTimedMetadataInsertion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AvailBlanking as Types
import qualified Network.AWS.MediaConvert.Types.EsamSettings as Types
import qualified Network.AWS.MediaConvert.Types.InputTemplate as Types
import qualified Network.AWS.MediaConvert.Types.MotionImageInserter as Types
import qualified Network.AWS.MediaConvert.Types.NielsenConfiguration as Types
import qualified Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings as Types
import qualified Network.AWS.MediaConvert.Types.OutputGroup as Types
import qualified Network.AWS.MediaConvert.Types.TimecodeConfig as Types
import qualified Network.AWS.MediaConvert.Types.TimedMetadataInsertion as Types
import qualified Network.AWS.Prelude as Core

-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
--
-- /See:/ 'mkJobTemplateSettings' smart constructor.
data JobTemplateSettings = JobTemplateSettings'
  { -- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
    adAvailOffset :: Core.Maybe Core.Int,
    -- | Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
    availBlanking :: Core.Maybe Types.AvailBlanking,
    -- | Settings for Event Signaling And Messaging (ESAM).
    esam :: Core.Maybe Types.EsamSettings,
    -- | Use Inputs (inputs) to define the source file used in the transcode job. There can only be one input in a job template.  Using the API, you can include multiple inputs when referencing a job template.
    inputs :: Core.Maybe [Types.InputTemplate],
    -- | Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
    motionImageInserter :: Core.Maybe Types.MotionImageInserter,
    -- | Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
    nielsenConfiguration :: Core.Maybe Types.NielsenConfiguration,
    -- | Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
    nielsenNonLinearWatermark :: Core.Maybe Types.NielsenNonLinearWatermarkSettings,
    -- | (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
    outputGroups :: Core.Maybe [Types.OutputGroup],
    -- | Contains settings used to acquire and adjust timecode information from inputs.
    timecodeConfig :: Core.Maybe Types.TimecodeConfig,
    -- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
    timedMetadataInsertion :: Core.Maybe Types.TimedMetadataInsertion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobTemplateSettings' value with any optional fields omitted.
mkJobTemplateSettings ::
  JobTemplateSettings
mkJobTemplateSettings =
  JobTemplateSettings'
    { adAvailOffset = Core.Nothing,
      availBlanking = Core.Nothing,
      esam = Core.Nothing,
      inputs = Core.Nothing,
      motionImageInserter = Core.Nothing,
      nielsenConfiguration = Core.Nothing,
      nielsenNonLinearWatermark = Core.Nothing,
      outputGroups = Core.Nothing,
      timecodeConfig = Core.Nothing,
      timedMetadataInsertion = Core.Nothing
    }

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
--
-- /Note:/ Consider using 'adAvailOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsAdAvailOffset :: Lens.Lens' JobTemplateSettings (Core.Maybe Core.Int)
jtsAdAvailOffset = Lens.field @"adAvailOffset"
{-# DEPRECATED jtsAdAvailOffset "Use generic-lens or generic-optics with 'adAvailOffset' instead." #-}

-- | Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
--
-- /Note:/ Consider using 'availBlanking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsAvailBlanking :: Lens.Lens' JobTemplateSettings (Core.Maybe Types.AvailBlanking)
jtsAvailBlanking = Lens.field @"availBlanking"
{-# DEPRECATED jtsAvailBlanking "Use generic-lens or generic-optics with 'availBlanking' instead." #-}

-- | Settings for Event Signaling And Messaging (ESAM).
--
-- /Note:/ Consider using 'esam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsEsam :: Lens.Lens' JobTemplateSettings (Core.Maybe Types.EsamSettings)
jtsEsam = Lens.field @"esam"
{-# DEPRECATED jtsEsam "Use generic-lens or generic-optics with 'esam' instead." #-}

-- | Use Inputs (inputs) to define the source file used in the transcode job. There can only be one input in a job template.  Using the API, you can include multiple inputs when referencing a job template.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsInputs :: Lens.Lens' JobTemplateSettings (Core.Maybe [Types.InputTemplate])
jtsInputs = Lens.field @"inputs"
{-# DEPRECATED jtsInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
--
-- /Note:/ Consider using 'motionImageInserter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsMotionImageInserter :: Lens.Lens' JobTemplateSettings (Core.Maybe Types.MotionImageInserter)
jtsMotionImageInserter = Lens.field @"motionImageInserter"
{-# DEPRECATED jtsMotionImageInserter "Use generic-lens or generic-optics with 'motionImageInserter' instead." #-}

-- | Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
--
-- /Note:/ Consider using 'nielsenConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsNielsenConfiguration :: Lens.Lens' JobTemplateSettings (Core.Maybe Types.NielsenConfiguration)
jtsNielsenConfiguration = Lens.field @"nielsenConfiguration"
{-# DEPRECATED jtsNielsenConfiguration "Use generic-lens or generic-optics with 'nielsenConfiguration' instead." #-}

-- | Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
--
-- /Note:/ Consider using 'nielsenNonLinearWatermark' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsNielsenNonLinearWatermark :: Lens.Lens' JobTemplateSettings (Core.Maybe Types.NielsenNonLinearWatermarkSettings)
jtsNielsenNonLinearWatermark = Lens.field @"nielsenNonLinearWatermark"
{-# DEPRECATED jtsNielsenNonLinearWatermark "Use generic-lens or generic-optics with 'nielsenNonLinearWatermark' instead." #-}

-- | (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
--
-- /Note:/ Consider using 'outputGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsOutputGroups :: Lens.Lens' JobTemplateSettings (Core.Maybe [Types.OutputGroup])
jtsOutputGroups = Lens.field @"outputGroups"
{-# DEPRECATED jtsOutputGroups "Use generic-lens or generic-optics with 'outputGroups' instead." #-}

-- | Contains settings used to acquire and adjust timecode information from inputs.
--
-- /Note:/ Consider using 'timecodeConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsTimecodeConfig :: Lens.Lens' JobTemplateSettings (Core.Maybe Types.TimecodeConfig)
jtsTimecodeConfig = Lens.field @"timecodeConfig"
{-# DEPRECATED jtsTimecodeConfig "Use generic-lens or generic-optics with 'timecodeConfig' instead." #-}

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
--
-- /Note:/ Consider using 'timedMetadataInsertion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jtsTimedMetadataInsertion :: Lens.Lens' JobTemplateSettings (Core.Maybe Types.TimedMetadataInsertion)
jtsTimedMetadataInsertion = Lens.field @"timedMetadataInsertion"
{-# DEPRECATED jtsTimedMetadataInsertion "Use generic-lens or generic-optics with 'timedMetadataInsertion' instead." #-}

instance Core.FromJSON JobTemplateSettings where
  toJSON JobTemplateSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("adAvailOffset" Core..=) Core.<$> adAvailOffset,
            ("availBlanking" Core..=) Core.<$> availBlanking,
            ("esam" Core..=) Core.<$> esam,
            ("inputs" Core..=) Core.<$> inputs,
            ("motionImageInserter" Core..=) Core.<$> motionImageInserter,
            ("nielsenConfiguration" Core..=) Core.<$> nielsenConfiguration,
            ("nielsenNonLinearWatermark" Core..=)
              Core.<$> nielsenNonLinearWatermark,
            ("outputGroups" Core..=) Core.<$> outputGroups,
            ("timecodeConfig" Core..=) Core.<$> timecodeConfig,
            ("timedMetadataInsertion" Core..=)
              Core.<$> timedMetadataInsertion
          ]
      )

instance Core.FromJSON JobTemplateSettings where
  parseJSON =
    Core.withObject "JobTemplateSettings" Core.$
      \x ->
        JobTemplateSettings'
          Core.<$> (x Core..:? "adAvailOffset")
          Core.<*> (x Core..:? "availBlanking")
          Core.<*> (x Core..:? "esam")
          Core.<*> (x Core..:? "inputs")
          Core.<*> (x Core..:? "motionImageInserter")
          Core.<*> (x Core..:? "nielsenConfiguration")
          Core.<*> (x Core..:? "nielsenNonLinearWatermark")
          Core.<*> (x Core..:? "outputGroups")
          Core.<*> (x Core..:? "timecodeConfig")
          Core.<*> (x Core..:? "timedMetadataInsertion")
