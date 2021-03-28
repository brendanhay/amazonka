{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.JobSettings
  ( JobSettings (..)
  -- * Smart constructor
  , mkJobSettings
  -- * Lenses
  , jsAdAvailOffset
  , jsAvailBlanking
  , jsEsam
  , jsInputs
  , jsMotionImageInserter
  , jsNielsenConfiguration
  , jsNielsenNonLinearWatermark
  , jsOutputGroups
  , jsTimecodeConfig
  , jsTimedMetadataInsertion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AvailBlanking as Types
import qualified Network.AWS.MediaConvert.Types.EsamSettings as Types
import qualified Network.AWS.MediaConvert.Types.Input as Types
import qualified Network.AWS.MediaConvert.Types.MotionImageInserter as Types
import qualified Network.AWS.MediaConvert.Types.NielsenConfiguration as Types
import qualified Network.AWS.MediaConvert.Types.NielsenNonLinearWatermarkSettings as Types
import qualified Network.AWS.MediaConvert.Types.OutputGroup as Types
import qualified Network.AWS.MediaConvert.Types.TimecodeConfig as Types
import qualified Network.AWS.MediaConvert.Types.TimedMetadataInsertion as Types
import qualified Network.AWS.Prelude as Core

-- | JobSettings contains all the transcode settings for a job.
--
-- /See:/ 'mkJobSettings' smart constructor.
data JobSettings = JobSettings'
  { adAvailOffset :: Core.Maybe Core.Int
    -- ^ When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
  , availBlanking :: Core.Maybe Types.AvailBlanking
    -- ^ Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
  , esam :: Core.Maybe Types.EsamSettings
    -- ^ Settings for Event Signaling And Messaging (ESAM).
  , inputs :: Core.Maybe [Types.Input]
    -- ^ Use Inputs (inputs) to define source file used in the transcode job. There can be multiple inputs add in a job. These inputs will be concantenated together to create the output.
  , motionImageInserter :: Core.Maybe Types.MotionImageInserter
    -- ^ Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
  , nielsenConfiguration :: Core.Maybe Types.NielsenConfiguration
    -- ^ Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
  , nielsenNonLinearWatermark :: Core.Maybe Types.NielsenNonLinearWatermarkSettings
    -- ^ Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
  , outputGroups :: Core.Maybe [Types.OutputGroup]
    -- ^ (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
  , timecodeConfig :: Core.Maybe Types.TimecodeConfig
    -- ^ Contains settings used to acquire and adjust timecode information from inputs.
  , timedMetadataInsertion :: Core.Maybe Types.TimedMetadataInsertion
    -- ^ Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobSettings' value with any optional fields omitted.
mkJobSettings
    :: JobSettings
mkJobSettings
  = JobSettings'{adAvailOffset = Core.Nothing,
                 availBlanking = Core.Nothing, esam = Core.Nothing,
                 inputs = Core.Nothing, motionImageInserter = Core.Nothing,
                 nielsenConfiguration = Core.Nothing,
                 nielsenNonLinearWatermark = Core.Nothing,
                 outputGroups = Core.Nothing, timecodeConfig = Core.Nothing,
                 timedMetadataInsertion = Core.Nothing}

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
--
-- /Note:/ Consider using 'adAvailOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsAdAvailOffset :: Lens.Lens' JobSettings (Core.Maybe Core.Int)
jsAdAvailOffset = Lens.field @"adAvailOffset"
{-# INLINEABLE jsAdAvailOffset #-}
{-# DEPRECATED adAvailOffset "Use generic-lens or generic-optics with 'adAvailOffset' instead"  #-}

-- | Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
--
-- /Note:/ Consider using 'availBlanking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsAvailBlanking :: Lens.Lens' JobSettings (Core.Maybe Types.AvailBlanking)
jsAvailBlanking = Lens.field @"availBlanking"
{-# INLINEABLE jsAvailBlanking #-}
{-# DEPRECATED availBlanking "Use generic-lens or generic-optics with 'availBlanking' instead"  #-}

-- | Settings for Event Signaling And Messaging (ESAM).
--
-- /Note:/ Consider using 'esam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsEsam :: Lens.Lens' JobSettings (Core.Maybe Types.EsamSettings)
jsEsam = Lens.field @"esam"
{-# INLINEABLE jsEsam #-}
{-# DEPRECATED esam "Use generic-lens or generic-optics with 'esam' instead"  #-}

-- | Use Inputs (inputs) to define source file used in the transcode job. There can be multiple inputs add in a job. These inputs will be concantenated together to create the output.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsInputs :: Lens.Lens' JobSettings (Core.Maybe [Types.Input])
jsInputs = Lens.field @"inputs"
{-# INLINEABLE jsInputs #-}
{-# DEPRECATED inputs "Use generic-lens or generic-optics with 'inputs' instead"  #-}

-- | Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
--
-- /Note:/ Consider using 'motionImageInserter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsMotionImageInserter :: Lens.Lens' JobSettings (Core.Maybe Types.MotionImageInserter)
jsMotionImageInserter = Lens.field @"motionImageInserter"
{-# INLINEABLE jsMotionImageInserter #-}
{-# DEPRECATED motionImageInserter "Use generic-lens or generic-optics with 'motionImageInserter' instead"  #-}

-- | Settings for your Nielsen configuration. If you don't do Nielsen measurement and analytics, ignore these settings. When you enable Nielsen configuration (nielsenConfiguration), MediaConvert enables PCM to ID3 tagging for all outputs in the job. To enable Nielsen configuration programmatically, include an instance of nielsenConfiguration in your JSON job specification. Even if you don't include any children of nielsenConfiguration, you still enable the setting.
--
-- /Note:/ Consider using 'nielsenConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsNielsenConfiguration :: Lens.Lens' JobSettings (Core.Maybe Types.NielsenConfiguration)
jsNielsenConfiguration = Lens.field @"nielsenConfiguration"
{-# INLINEABLE jsNielsenConfiguration #-}
{-# DEPRECATED nielsenConfiguration "Use generic-lens or generic-optics with 'nielsenConfiguration' instead"  #-}

-- | Ignore these settings unless you are using Nielsen non-linear watermarking. Specify the values that  MediaConvert uses to generate and place Nielsen watermarks in your output audio. In addition to  specifying these values, you also need to set up your cloud TIC server. These settings apply to  every output in your job. The MediaConvert implementation is currently with the following Nielsen versions: Nielsen Watermark SDK Version 5.2.1 Nielsen NLM Watermark Engine Version 1.2.7 Nielsen Watermark Authenticator [SID_TIC] Version [5.0.0]
--
-- /Note:/ Consider using 'nielsenNonLinearWatermark' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsNielsenNonLinearWatermark :: Lens.Lens' JobSettings (Core.Maybe Types.NielsenNonLinearWatermarkSettings)
jsNielsenNonLinearWatermark = Lens.field @"nielsenNonLinearWatermark"
{-# INLINEABLE jsNielsenNonLinearWatermark #-}
{-# DEPRECATED nielsenNonLinearWatermark "Use generic-lens or generic-optics with 'nielsenNonLinearWatermark' instead"  #-}

-- | (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
--
-- /Note:/ Consider using 'outputGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsOutputGroups :: Lens.Lens' JobSettings (Core.Maybe [Types.OutputGroup])
jsOutputGroups = Lens.field @"outputGroups"
{-# INLINEABLE jsOutputGroups #-}
{-# DEPRECATED outputGroups "Use generic-lens or generic-optics with 'outputGroups' instead"  #-}

-- | Contains settings used to acquire and adjust timecode information from inputs.
--
-- /Note:/ Consider using 'timecodeConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsTimecodeConfig :: Lens.Lens' JobSettings (Core.Maybe Types.TimecodeConfig)
jsTimecodeConfig = Lens.field @"timecodeConfig"
{-# INLINEABLE jsTimecodeConfig #-}
{-# DEPRECATED timecodeConfig "Use generic-lens or generic-optics with 'timecodeConfig' instead"  #-}

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
--
-- /Note:/ Consider using 'timedMetadataInsertion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsTimedMetadataInsertion :: Lens.Lens' JobSettings (Core.Maybe Types.TimedMetadataInsertion)
jsTimedMetadataInsertion = Lens.field @"timedMetadataInsertion"
{-# INLINEABLE jsTimedMetadataInsertion #-}
{-# DEPRECATED timedMetadataInsertion "Use generic-lens or generic-optics with 'timedMetadataInsertion' instead"  #-}

instance Core.FromJSON JobSettings where
        toJSON JobSettings{..}
          = Core.object
              (Core.catMaybes
                 [("adAvailOffset" Core..=) Core.<$> adAvailOffset,
                  ("availBlanking" Core..=) Core.<$> availBlanking,
                  ("esam" Core..=) Core.<$> esam, ("inputs" Core..=) Core.<$> inputs,
                  ("motionImageInserter" Core..=) Core.<$> motionImageInserter,
                  ("nielsenConfiguration" Core..=) Core.<$> nielsenConfiguration,
                  ("nielsenNonLinearWatermark" Core..=) Core.<$>
                    nielsenNonLinearWatermark,
                  ("outputGroups" Core..=) Core.<$> outputGroups,
                  ("timecodeConfig" Core..=) Core.<$> timecodeConfig,
                  ("timedMetadataInsertion" Core..=) Core.<$>
                    timedMetadataInsertion])

instance Core.FromJSON JobSettings where
        parseJSON
          = Core.withObject "JobSettings" Core.$
              \ x ->
                JobSettings' Core.<$>
                  (x Core..:? "adAvailOffset") Core.<*> x Core..:? "availBlanking"
                    Core.<*> x Core..:? "esam"
                    Core.<*> x Core..:? "inputs"
                    Core.<*> x Core..:? "motionImageInserter"
                    Core.<*> x Core..:? "nielsenConfiguration"
                    Core.<*> x Core..:? "nielsenNonLinearWatermark"
                    Core.<*> x Core..:? "outputGroups"
                    Core.<*> x Core..:? "timecodeConfig"
                    Core.<*> x Core..:? "timedMetadataInsertion"
