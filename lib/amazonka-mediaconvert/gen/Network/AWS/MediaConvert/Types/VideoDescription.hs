{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.VideoDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoDescription
  ( VideoDescription (..),

    -- * Smart constructor
    mkVideoDescription,

    -- * Lenses
    vdAfdSignaling,
    vdAntiAlias,
    vdCodecSettings,
    vdColorMetadata,
    vdCrop,
    vdDropFrameTimecode,
    vdFixedAfd,
    vdHeight,
    vdPosition,
    vdRespondToAfd,
    vdScalingBehavior,
    vdSharpness,
    vdTimecodeInsertion,
    vdVideoPreprocessors,
    vdWidth,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AfdSignaling as Types
import qualified Network.AWS.MediaConvert.Types.AntiAlias as Types
import qualified Network.AWS.MediaConvert.Types.ColorMetadata as Types
import qualified Network.AWS.MediaConvert.Types.DropFrameTimecode as Types
import qualified Network.AWS.MediaConvert.Types.Rectangle as Types
import qualified Network.AWS.MediaConvert.Types.RespondToAfd as Types
import qualified Network.AWS.MediaConvert.Types.ScalingBehavior as Types
import qualified Network.AWS.MediaConvert.Types.VideoCodecSettings as Types
import qualified Network.AWS.MediaConvert.Types.VideoPreprocessor as Types
import qualified Network.AWS.MediaConvert.Types.VideoTimecodeInsertion as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for video outputs
--
-- /See:/ 'mkVideoDescription' smart constructor.
data VideoDescription = VideoDescription'
  { -- | This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert AFD signaling (AfdSignaling) to specify whether the service includes AFD values in the output video data and what those values are. * Choose None to remove all AFD values from this output. * Choose Fixed to ignore input AFD values and instead encode the value specified in the job. * Choose Auto to calculate output AFD values based on the input AFD scaler data.
    afdSignaling :: Core.Maybe Types.AfdSignaling,
    -- | The anti-alias filter is automatically applied to all outputs. The service no longer accepts the value DISABLED for AntiAlias. If you specify that in your job, the service will ignore the setting.
    antiAlias :: Core.Maybe Types.AntiAlias,
    -- | Video codec settings, (CodecSettings) under (VideoDescription), contains the group of settings related to video encoding. The settings in this group vary depending on the value that you choose for Video codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE, FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings * MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8, Vp8Settings * VP9, Vp9Settings
    codecSettings :: Core.Maybe Types.VideoCodecSettings,
    -- | Choose Insert (INSERT) for this setting to include color metadata in this output. Choose Ignore (IGNORE) to exclude color metadata from this output. If you don't specify a value, the service sets this to Insert by default.
    colorMetadata :: Core.Maybe Types.ColorMetadata,
    -- | Use Cropping selection (crop) to specify the video area that the service will include in the output video frame.
    crop :: Core.Maybe Types.Rectangle,
    -- | Applies only to 29.97 fps outputs. When this feature is enabled, the service will use drop-frame timecode on outputs. If it is not possible to use drop-frame timecode, the system will fall back to non-drop-frame. This setting is enabled by default when Timecode insertion (TimecodeInsertion) is enabled.
    dropFrameTimecode :: Core.Maybe Types.DropFrameTimecode,
    -- | Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED). Use Fixed (FixedAfd) to specify a four-bit AFD value which the service will write on all  frames of this video output.
    fixedAfd :: Core.Maybe Core.Natural,
    -- | Use the Height (Height) setting to define the video resolution height for this output. Specify in pixels. If you don't provide a value here, the service will use the input height.
    height :: Core.Maybe Core.Natural,
    -- | Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black.
    position :: Core.Maybe Types.Rectangle,
    -- | Use Respond to AFD (RespondToAfd) to specify how the service changes the video itself in response to AFD values in the input. * Choose Respond to clip the input video frame according to the AFD value, input display aspect ratio, and output display aspect ratio. * Choose Passthrough to include the input AFD values. Do not choose this when AfdSignaling is set to (NONE). A preferred implementation of this workflow is to set RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to remove all input AFD values from this output.
    respondToAfd :: Core.Maybe Types.RespondToAfd,
    -- | Specify how the service handles outputs that have a different aspect ratio from the input aspect ratio. Choose Stretch to output (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit. Keep the setting Default (DEFAULT) to have the service letterbox your video instead. This setting overrides any value that you specify for the setting Selection placement (position) in this output.
    scalingBehavior :: Core.Maybe Types.ScalingBehavior,
    -- | Use Sharpness (Sharpness) setting to specify the strength of anti-aliasing. This setting changes the width of the anti-alias filter kernel used for scaling. Sharpness only applies if your output resolution is different from your input resolution. 0 is the softest setting, 100 the sharpest, and 50 recommended for most content.
    sharpness :: Core.Maybe Core.Natural,
    -- | Applies only to H.264, H.265, MPEG2, and ProRes outputs. Only enable Timecode insertion when the input frame rate is identical to the output frame rate. To include timecodes in this output, set Timecode insertion (VideoTimecodeInsertion) to PIC_TIMING_SEI. To leave them out, set it to DISABLED. Default is DISABLED. When the service inserts timecodes in an output, by default, it uses any embedded timecodes from the input. If none are present, the service will set the timecode for the first output frame to zero. To change this default behavior, adjust the settings under Timecode configuration (TimecodeConfig). In the console, these settings are located under Job > Job settings > Timecode configuration. Note - Timecode source under input settings (InputTimecodeSource) does not affect the timecodes that are inserted in the output. Source under Job settings > Timecode configuration (TimecodeSource) does.
    timecodeInsertion :: Core.Maybe Types.VideoTimecodeInsertion,
    -- | Find additional transcoding features under Preprocessors (VideoPreprocessors). Enable the features at each output individually. These features are disabled by default.
    videoPreprocessors :: Core.Maybe Types.VideoPreprocessor,
    -- | Use Width (Width) to define the video resolution width, in pixels, for this output. If you don't provide a value here, the service will use the input width.
    width :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VideoDescription' value with any optional fields omitted.
mkVideoDescription ::
  VideoDescription
mkVideoDescription =
  VideoDescription'
    { afdSignaling = Core.Nothing,
      antiAlias = Core.Nothing,
      codecSettings = Core.Nothing,
      colorMetadata = Core.Nothing,
      crop = Core.Nothing,
      dropFrameTimecode = Core.Nothing,
      fixedAfd = Core.Nothing,
      height = Core.Nothing,
      position = Core.Nothing,
      respondToAfd = Core.Nothing,
      scalingBehavior = Core.Nothing,
      sharpness = Core.Nothing,
      timecodeInsertion = Core.Nothing,
      videoPreprocessors = Core.Nothing,
      width = Core.Nothing
    }

-- | This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert AFD signaling (AfdSignaling) to specify whether the service includes AFD values in the output video data and what those values are. * Choose None to remove all AFD values from this output. * Choose Fixed to ignore input AFD values and instead encode the value specified in the job. * Choose Auto to calculate output AFD values based on the input AFD scaler data.
--
-- /Note:/ Consider using 'afdSignaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdAfdSignaling :: Lens.Lens' VideoDescription (Core.Maybe Types.AfdSignaling)
vdAfdSignaling = Lens.field @"afdSignaling"
{-# DEPRECATED vdAfdSignaling "Use generic-lens or generic-optics with 'afdSignaling' instead." #-}

-- | The anti-alias filter is automatically applied to all outputs. The service no longer accepts the value DISABLED for AntiAlias. If you specify that in your job, the service will ignore the setting.
--
-- /Note:/ Consider using 'antiAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdAntiAlias :: Lens.Lens' VideoDescription (Core.Maybe Types.AntiAlias)
vdAntiAlias = Lens.field @"antiAlias"
{-# DEPRECATED vdAntiAlias "Use generic-lens or generic-optics with 'antiAlias' instead." #-}

-- | Video codec settings, (CodecSettings) under (VideoDescription), contains the group of settings related to video encoding. The settings in this group vary depending on the value that you choose for Video codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE, FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings * MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8, Vp8Settings * VP9, Vp9Settings
--
-- /Note:/ Consider using 'codecSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdCodecSettings :: Lens.Lens' VideoDescription (Core.Maybe Types.VideoCodecSettings)
vdCodecSettings = Lens.field @"codecSettings"
{-# DEPRECATED vdCodecSettings "Use generic-lens or generic-optics with 'codecSettings' instead." #-}

-- | Choose Insert (INSERT) for this setting to include color metadata in this output. Choose Ignore (IGNORE) to exclude color metadata from this output. If you don't specify a value, the service sets this to Insert by default.
--
-- /Note:/ Consider using 'colorMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdColorMetadata :: Lens.Lens' VideoDescription (Core.Maybe Types.ColorMetadata)
vdColorMetadata = Lens.field @"colorMetadata"
{-# DEPRECATED vdColorMetadata "Use generic-lens or generic-optics with 'colorMetadata' instead." #-}

-- | Use Cropping selection (crop) to specify the video area that the service will include in the output video frame.
--
-- /Note:/ Consider using 'crop' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdCrop :: Lens.Lens' VideoDescription (Core.Maybe Types.Rectangle)
vdCrop = Lens.field @"crop"
{-# DEPRECATED vdCrop "Use generic-lens or generic-optics with 'crop' instead." #-}

-- | Applies only to 29.97 fps outputs. When this feature is enabled, the service will use drop-frame timecode on outputs. If it is not possible to use drop-frame timecode, the system will fall back to non-drop-frame. This setting is enabled by default when Timecode insertion (TimecodeInsertion) is enabled.
--
-- /Note:/ Consider using 'dropFrameTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdDropFrameTimecode :: Lens.Lens' VideoDescription (Core.Maybe Types.DropFrameTimecode)
vdDropFrameTimecode = Lens.field @"dropFrameTimecode"
{-# DEPRECATED vdDropFrameTimecode "Use generic-lens or generic-optics with 'dropFrameTimecode' instead." #-}

-- | Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED). Use Fixed (FixedAfd) to specify a four-bit AFD value which the service will write on all  frames of this video output.
--
-- /Note:/ Consider using 'fixedAfd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdFixedAfd :: Lens.Lens' VideoDescription (Core.Maybe Core.Natural)
vdFixedAfd = Lens.field @"fixedAfd"
{-# DEPRECATED vdFixedAfd "Use generic-lens or generic-optics with 'fixedAfd' instead." #-}

-- | Use the Height (Height) setting to define the video resolution height for this output. Specify in pixels. If you don't provide a value here, the service will use the input height.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdHeight :: Lens.Lens' VideoDescription (Core.Maybe Core.Natural)
vdHeight = Lens.field @"height"
{-# DEPRECATED vdHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdPosition :: Lens.Lens' VideoDescription (Core.Maybe Types.Rectangle)
vdPosition = Lens.field @"position"
{-# DEPRECATED vdPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | Use Respond to AFD (RespondToAfd) to specify how the service changes the video itself in response to AFD values in the input. * Choose Respond to clip the input video frame according to the AFD value, input display aspect ratio, and output display aspect ratio. * Choose Passthrough to include the input AFD values. Do not choose this when AfdSignaling is set to (NONE). A preferred implementation of this workflow is to set RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to remove all input AFD values from this output.
--
-- /Note:/ Consider using 'respondToAfd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdRespondToAfd :: Lens.Lens' VideoDescription (Core.Maybe Types.RespondToAfd)
vdRespondToAfd = Lens.field @"respondToAfd"
{-# DEPRECATED vdRespondToAfd "Use generic-lens or generic-optics with 'respondToAfd' instead." #-}

-- | Specify how the service handles outputs that have a different aspect ratio from the input aspect ratio. Choose Stretch to output (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit. Keep the setting Default (DEFAULT) to have the service letterbox your video instead. This setting overrides any value that you specify for the setting Selection placement (position) in this output.
--
-- /Note:/ Consider using 'scalingBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdScalingBehavior :: Lens.Lens' VideoDescription (Core.Maybe Types.ScalingBehavior)
vdScalingBehavior = Lens.field @"scalingBehavior"
{-# DEPRECATED vdScalingBehavior "Use generic-lens or generic-optics with 'scalingBehavior' instead." #-}

-- | Use Sharpness (Sharpness) setting to specify the strength of anti-aliasing. This setting changes the width of the anti-alias filter kernel used for scaling. Sharpness only applies if your output resolution is different from your input resolution. 0 is the softest setting, 100 the sharpest, and 50 recommended for most content.
--
-- /Note:/ Consider using 'sharpness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdSharpness :: Lens.Lens' VideoDescription (Core.Maybe Core.Natural)
vdSharpness = Lens.field @"sharpness"
{-# DEPRECATED vdSharpness "Use generic-lens or generic-optics with 'sharpness' instead." #-}

-- | Applies only to H.264, H.265, MPEG2, and ProRes outputs. Only enable Timecode insertion when the input frame rate is identical to the output frame rate. To include timecodes in this output, set Timecode insertion (VideoTimecodeInsertion) to PIC_TIMING_SEI. To leave them out, set it to DISABLED. Default is DISABLED. When the service inserts timecodes in an output, by default, it uses any embedded timecodes from the input. If none are present, the service will set the timecode for the first output frame to zero. To change this default behavior, adjust the settings under Timecode configuration (TimecodeConfig). In the console, these settings are located under Job > Job settings > Timecode configuration. Note - Timecode source under input settings (InputTimecodeSource) does not affect the timecodes that are inserted in the output. Source under Job settings > Timecode configuration (TimecodeSource) does.
--
-- /Note:/ Consider using 'timecodeInsertion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdTimecodeInsertion :: Lens.Lens' VideoDescription (Core.Maybe Types.VideoTimecodeInsertion)
vdTimecodeInsertion = Lens.field @"timecodeInsertion"
{-# DEPRECATED vdTimecodeInsertion "Use generic-lens or generic-optics with 'timecodeInsertion' instead." #-}

-- | Find additional transcoding features under Preprocessors (VideoPreprocessors). Enable the features at each output individually. These features are disabled by default.
--
-- /Note:/ Consider using 'videoPreprocessors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVideoPreprocessors :: Lens.Lens' VideoDescription (Core.Maybe Types.VideoPreprocessor)
vdVideoPreprocessors = Lens.field @"videoPreprocessors"
{-# DEPRECATED vdVideoPreprocessors "Use generic-lens or generic-optics with 'videoPreprocessors' instead." #-}

-- | Use Width (Width) to define the video resolution width, in pixels, for this output. If you don't provide a value here, the service will use the input width.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdWidth :: Lens.Lens' VideoDescription (Core.Maybe Core.Natural)
vdWidth = Lens.field @"width"
{-# DEPRECATED vdWidth "Use generic-lens or generic-optics with 'width' instead." #-}

instance Core.FromJSON VideoDescription where
  toJSON VideoDescription {..} =
    Core.object
      ( Core.catMaybes
          [ ("afdSignaling" Core..=) Core.<$> afdSignaling,
            ("antiAlias" Core..=) Core.<$> antiAlias,
            ("codecSettings" Core..=) Core.<$> codecSettings,
            ("colorMetadata" Core..=) Core.<$> colorMetadata,
            ("crop" Core..=) Core.<$> crop,
            ("dropFrameTimecode" Core..=) Core.<$> dropFrameTimecode,
            ("fixedAfd" Core..=) Core.<$> fixedAfd,
            ("height" Core..=) Core.<$> height,
            ("position" Core..=) Core.<$> position,
            ("respondToAfd" Core..=) Core.<$> respondToAfd,
            ("scalingBehavior" Core..=) Core.<$> scalingBehavior,
            ("sharpness" Core..=) Core.<$> sharpness,
            ("timecodeInsertion" Core..=) Core.<$> timecodeInsertion,
            ("videoPreprocessors" Core..=) Core.<$> videoPreprocessors,
            ("width" Core..=) Core.<$> width
          ]
      )

instance Core.FromJSON VideoDescription where
  parseJSON =
    Core.withObject "VideoDescription" Core.$
      \x ->
        VideoDescription'
          Core.<$> (x Core..:? "afdSignaling")
          Core.<*> (x Core..:? "antiAlias")
          Core.<*> (x Core..:? "codecSettings")
          Core.<*> (x Core..:? "colorMetadata")
          Core.<*> (x Core..:? "crop")
          Core.<*> (x Core..:? "dropFrameTimecode")
          Core.<*> (x Core..:? "fixedAfd")
          Core.<*> (x Core..:? "height")
          Core.<*> (x Core..:? "position")
          Core.<*> (x Core..:? "respondToAfd")
          Core.<*> (x Core..:? "scalingBehavior")
          Core.<*> (x Core..:? "sharpness")
          Core.<*> (x Core..:? "timecodeInsertion")
          Core.<*> (x Core..:? "videoPreprocessors")
          Core.<*> (x Core..:? "width")
