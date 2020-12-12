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
    vdTimecodeInsertion,
    vdHeight,
    vdAfdSignaling,
    vdSharpness,
    vdCrop,
    vdWidth,
    vdScalingBehavior,
    vdRespondToAfd,
    vdDropFrameTimecode,
    vdAntiAlias,
    vdFixedAfd,
    vdColorMetadata,
    vdCodecSettings,
    vdVideoPreprocessors,
    vdPosition,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AfdSignaling
import Network.AWS.MediaConvert.Types.AntiAlias
import Network.AWS.MediaConvert.Types.ColorMetadata
import Network.AWS.MediaConvert.Types.DropFrameTimecode
import Network.AWS.MediaConvert.Types.Rectangle
import Network.AWS.MediaConvert.Types.RespondToAfd
import Network.AWS.MediaConvert.Types.ScalingBehavior
import Network.AWS.MediaConvert.Types.VideoCodecSettings
import Network.AWS.MediaConvert.Types.VideoPreprocessor
import Network.AWS.MediaConvert.Types.VideoTimecodeInsertion
import qualified Network.AWS.Prelude as Lude

-- | Settings for video outputs
--
-- /See:/ 'mkVideoDescription' smart constructor.
data VideoDescription = VideoDescription'
  { timecodeInsertion ::
      Lude.Maybe VideoTimecodeInsertion,
    height :: Lude.Maybe Lude.Natural,
    afdSignaling :: Lude.Maybe AfdSignaling,
    sharpness :: Lude.Maybe Lude.Natural,
    crop :: Lude.Maybe Rectangle,
    width :: Lude.Maybe Lude.Natural,
    scalingBehavior :: Lude.Maybe ScalingBehavior,
    respondToAfd :: Lude.Maybe RespondToAfd,
    dropFrameTimecode :: Lude.Maybe DropFrameTimecode,
    antiAlias :: Lude.Maybe AntiAlias,
    fixedAfd :: Lude.Maybe Lude.Natural,
    colorMetadata :: Lude.Maybe ColorMetadata,
    codecSettings :: Lude.Maybe VideoCodecSettings,
    videoPreprocessors :: Lude.Maybe VideoPreprocessor,
    position :: Lude.Maybe Rectangle
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VideoDescription' with the minimum fields required to make a request.
--
-- * 'afdSignaling' - This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert AFD signaling (AfdSignaling) to specify whether the service includes AFD values in the output video data and what those values are. * Choose None to remove all AFD values from this output. * Choose Fixed to ignore input AFD values and instead encode the value specified in the job. * Choose Auto to calculate output AFD values based on the input AFD scaler data.
-- * 'antiAlias' - The anti-alias filter is automatically applied to all outputs. The service no longer accepts the value DISABLED for AntiAlias. If you specify that in your job, the service will ignore the setting.
-- * 'codecSettings' - Video codec settings, (CodecSettings) under (VideoDescription), contains the group of settings related to video encoding. The settings in this group vary depending on the value that you choose for Video codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE, FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings * MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8, Vp8Settings * VP9, Vp9Settings
-- * 'colorMetadata' - Choose Insert (INSERT) for this setting to include color metadata in this output. Choose Ignore (IGNORE) to exclude color metadata from this output. If you don't specify a value, the service sets this to Insert by default.
-- * 'crop' - Use Cropping selection (crop) to specify the video area that the service will include in the output video frame.
-- * 'dropFrameTimecode' - Applies only to 29.97 fps outputs. When this feature is enabled, the service will use drop-frame timecode on outputs. If it is not possible to use drop-frame timecode, the system will fall back to non-drop-frame. This setting is enabled by default when Timecode insertion (TimecodeInsertion) is enabled.
-- * 'fixedAfd' - Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED). Use Fixed (FixedAfd) to specify a four-bit AFD value which the service will write on all  frames of this video output.
-- * 'height' - Use the Height (Height) setting to define the video resolution height for this output. Specify in pixels. If you don't provide a value here, the service will use the input height.
-- * 'position' - Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black.
-- * 'respondToAfd' - Use Respond to AFD (RespondToAfd) to specify how the service changes the video itself in response to AFD values in the input. * Choose Respond to clip the input video frame according to the AFD value, input display aspect ratio, and output display aspect ratio. * Choose Passthrough to include the input AFD values. Do not choose this when AfdSignaling is set to (NONE). A preferred implementation of this workflow is to set RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to remove all input AFD values from this output.
-- * 'scalingBehavior' - Specify how the service handles outputs that have a different aspect ratio from the input aspect ratio. Choose Stretch to output (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit. Keep the setting Default (DEFAULT) to have the service letterbox your video instead. This setting overrides any value that you specify for the setting Selection placement (position) in this output.
-- * 'sharpness' - Use Sharpness (Sharpness) setting to specify the strength of anti-aliasing. This setting changes the width of the anti-alias filter kernel used for scaling. Sharpness only applies if your output resolution is different from your input resolution. 0 is the softest setting, 100 the sharpest, and 50 recommended for most content.
-- * 'timecodeInsertion' - Applies only to H.264, H.265, MPEG2, and ProRes outputs. Only enable Timecode insertion when the input frame rate is identical to the output frame rate. To include timecodes in this output, set Timecode insertion (VideoTimecodeInsertion) to PIC_TIMING_SEI. To leave them out, set it to DISABLED. Default is DISABLED. When the service inserts timecodes in an output, by default, it uses any embedded timecodes from the input. If none are present, the service will set the timecode for the first output frame to zero. To change this default behavior, adjust the settings under Timecode configuration (TimecodeConfig). In the console, these settings are located under Job > Job settings > Timecode configuration. Note - Timecode source under input settings (InputTimecodeSource) does not affect the timecodes that are inserted in the output. Source under Job settings > Timecode configuration (TimecodeSource) does.
-- * 'videoPreprocessors' - Find additional transcoding features under Preprocessors (VideoPreprocessors). Enable the features at each output individually. These features are disabled by default.
-- * 'width' - Use Width (Width) to define the video resolution width, in pixels, for this output. If you don't provide a value here, the service will use the input width.
mkVideoDescription ::
  VideoDescription
mkVideoDescription =
  VideoDescription'
    { timecodeInsertion = Lude.Nothing,
      height = Lude.Nothing,
      afdSignaling = Lude.Nothing,
      sharpness = Lude.Nothing,
      crop = Lude.Nothing,
      width = Lude.Nothing,
      scalingBehavior = Lude.Nothing,
      respondToAfd = Lude.Nothing,
      dropFrameTimecode = Lude.Nothing,
      antiAlias = Lude.Nothing,
      fixedAfd = Lude.Nothing,
      colorMetadata = Lude.Nothing,
      codecSettings = Lude.Nothing,
      videoPreprocessors = Lude.Nothing,
      position = Lude.Nothing
    }

-- | Applies only to H.264, H.265, MPEG2, and ProRes outputs. Only enable Timecode insertion when the input frame rate is identical to the output frame rate. To include timecodes in this output, set Timecode insertion (VideoTimecodeInsertion) to PIC_TIMING_SEI. To leave them out, set it to DISABLED. Default is DISABLED. When the service inserts timecodes in an output, by default, it uses any embedded timecodes from the input. If none are present, the service will set the timecode for the first output frame to zero. To change this default behavior, adjust the settings under Timecode configuration (TimecodeConfig). In the console, these settings are located under Job > Job settings > Timecode configuration. Note - Timecode source under input settings (InputTimecodeSource) does not affect the timecodes that are inserted in the output. Source under Job settings > Timecode configuration (TimecodeSource) does.
--
-- /Note:/ Consider using 'timecodeInsertion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdTimecodeInsertion :: Lens.Lens' VideoDescription (Lude.Maybe VideoTimecodeInsertion)
vdTimecodeInsertion = Lens.lens (timecodeInsertion :: VideoDescription -> Lude.Maybe VideoTimecodeInsertion) (\s a -> s {timecodeInsertion = a} :: VideoDescription)
{-# DEPRECATED vdTimecodeInsertion "Use generic-lens or generic-optics with 'timecodeInsertion' instead." #-}

-- | Use the Height (Height) setting to define the video resolution height for this output. Specify in pixels. If you don't provide a value here, the service will use the input height.
--
-- /Note:/ Consider using 'height' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdHeight :: Lens.Lens' VideoDescription (Lude.Maybe Lude.Natural)
vdHeight = Lens.lens (height :: VideoDescription -> Lude.Maybe Lude.Natural) (\s a -> s {height = a} :: VideoDescription)
{-# DEPRECATED vdHeight "Use generic-lens or generic-optics with 'height' instead." #-}

-- | This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert AFD signaling (AfdSignaling) to specify whether the service includes AFD values in the output video data and what those values are. * Choose None to remove all AFD values from this output. * Choose Fixed to ignore input AFD values and instead encode the value specified in the job. * Choose Auto to calculate output AFD values based on the input AFD scaler data.
--
-- /Note:/ Consider using 'afdSignaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdAfdSignaling :: Lens.Lens' VideoDescription (Lude.Maybe AfdSignaling)
vdAfdSignaling = Lens.lens (afdSignaling :: VideoDescription -> Lude.Maybe AfdSignaling) (\s a -> s {afdSignaling = a} :: VideoDescription)
{-# DEPRECATED vdAfdSignaling "Use generic-lens or generic-optics with 'afdSignaling' instead." #-}

-- | Use Sharpness (Sharpness) setting to specify the strength of anti-aliasing. This setting changes the width of the anti-alias filter kernel used for scaling. Sharpness only applies if your output resolution is different from your input resolution. 0 is the softest setting, 100 the sharpest, and 50 recommended for most content.
--
-- /Note:/ Consider using 'sharpness' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdSharpness :: Lens.Lens' VideoDescription (Lude.Maybe Lude.Natural)
vdSharpness = Lens.lens (sharpness :: VideoDescription -> Lude.Maybe Lude.Natural) (\s a -> s {sharpness = a} :: VideoDescription)
{-# DEPRECATED vdSharpness "Use generic-lens or generic-optics with 'sharpness' instead." #-}

-- | Use Cropping selection (crop) to specify the video area that the service will include in the output video frame.
--
-- /Note:/ Consider using 'crop' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdCrop :: Lens.Lens' VideoDescription (Lude.Maybe Rectangle)
vdCrop = Lens.lens (crop :: VideoDescription -> Lude.Maybe Rectangle) (\s a -> s {crop = a} :: VideoDescription)
{-# DEPRECATED vdCrop "Use generic-lens or generic-optics with 'crop' instead." #-}

-- | Use Width (Width) to define the video resolution width, in pixels, for this output. If you don't provide a value here, the service will use the input width.
--
-- /Note:/ Consider using 'width' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdWidth :: Lens.Lens' VideoDescription (Lude.Maybe Lude.Natural)
vdWidth = Lens.lens (width :: VideoDescription -> Lude.Maybe Lude.Natural) (\s a -> s {width = a} :: VideoDescription)
{-# DEPRECATED vdWidth "Use generic-lens or generic-optics with 'width' instead." #-}

-- | Specify how the service handles outputs that have a different aspect ratio from the input aspect ratio. Choose Stretch to output (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit. Keep the setting Default (DEFAULT) to have the service letterbox your video instead. This setting overrides any value that you specify for the setting Selection placement (position) in this output.
--
-- /Note:/ Consider using 'scalingBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdScalingBehavior :: Lens.Lens' VideoDescription (Lude.Maybe ScalingBehavior)
vdScalingBehavior = Lens.lens (scalingBehavior :: VideoDescription -> Lude.Maybe ScalingBehavior) (\s a -> s {scalingBehavior = a} :: VideoDescription)
{-# DEPRECATED vdScalingBehavior "Use generic-lens or generic-optics with 'scalingBehavior' instead." #-}

-- | Use Respond to AFD (RespondToAfd) to specify how the service changes the video itself in response to AFD values in the input. * Choose Respond to clip the input video frame according to the AFD value, input display aspect ratio, and output display aspect ratio. * Choose Passthrough to include the input AFD values. Do not choose this when AfdSignaling is set to (NONE). A preferred implementation of this workflow is to set RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to remove all input AFD values from this output.
--
-- /Note:/ Consider using 'respondToAfd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdRespondToAfd :: Lens.Lens' VideoDescription (Lude.Maybe RespondToAfd)
vdRespondToAfd = Lens.lens (respondToAfd :: VideoDescription -> Lude.Maybe RespondToAfd) (\s a -> s {respondToAfd = a} :: VideoDescription)
{-# DEPRECATED vdRespondToAfd "Use generic-lens or generic-optics with 'respondToAfd' instead." #-}

-- | Applies only to 29.97 fps outputs. When this feature is enabled, the service will use drop-frame timecode on outputs. If it is not possible to use drop-frame timecode, the system will fall back to non-drop-frame. This setting is enabled by default when Timecode insertion (TimecodeInsertion) is enabled.
--
-- /Note:/ Consider using 'dropFrameTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdDropFrameTimecode :: Lens.Lens' VideoDescription (Lude.Maybe DropFrameTimecode)
vdDropFrameTimecode = Lens.lens (dropFrameTimecode :: VideoDescription -> Lude.Maybe DropFrameTimecode) (\s a -> s {dropFrameTimecode = a} :: VideoDescription)
{-# DEPRECATED vdDropFrameTimecode "Use generic-lens or generic-optics with 'dropFrameTimecode' instead." #-}

-- | The anti-alias filter is automatically applied to all outputs. The service no longer accepts the value DISABLED for AntiAlias. If you specify that in your job, the service will ignore the setting.
--
-- /Note:/ Consider using 'antiAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdAntiAlias :: Lens.Lens' VideoDescription (Lude.Maybe AntiAlias)
vdAntiAlias = Lens.lens (antiAlias :: VideoDescription -> Lude.Maybe AntiAlias) (\s a -> s {antiAlias = a} :: VideoDescription)
{-# DEPRECATED vdAntiAlias "Use generic-lens or generic-optics with 'antiAlias' instead." #-}

-- | Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED). Use Fixed (FixedAfd) to specify a four-bit AFD value which the service will write on all  frames of this video output.
--
-- /Note:/ Consider using 'fixedAfd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdFixedAfd :: Lens.Lens' VideoDescription (Lude.Maybe Lude.Natural)
vdFixedAfd = Lens.lens (fixedAfd :: VideoDescription -> Lude.Maybe Lude.Natural) (\s a -> s {fixedAfd = a} :: VideoDescription)
{-# DEPRECATED vdFixedAfd "Use generic-lens or generic-optics with 'fixedAfd' instead." #-}

-- | Choose Insert (INSERT) for this setting to include color metadata in this output. Choose Ignore (IGNORE) to exclude color metadata from this output. If you don't specify a value, the service sets this to Insert by default.
--
-- /Note:/ Consider using 'colorMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdColorMetadata :: Lens.Lens' VideoDescription (Lude.Maybe ColorMetadata)
vdColorMetadata = Lens.lens (colorMetadata :: VideoDescription -> Lude.Maybe ColorMetadata) (\s a -> s {colorMetadata = a} :: VideoDescription)
{-# DEPRECATED vdColorMetadata "Use generic-lens or generic-optics with 'colorMetadata' instead." #-}

-- | Video codec settings, (CodecSettings) under (VideoDescription), contains the group of settings related to video encoding. The settings in this group vary depending on the value that you choose for Video codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE, FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings * MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8, Vp8Settings * VP9, Vp9Settings
--
-- /Note:/ Consider using 'codecSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdCodecSettings :: Lens.Lens' VideoDescription (Lude.Maybe VideoCodecSettings)
vdCodecSettings = Lens.lens (codecSettings :: VideoDescription -> Lude.Maybe VideoCodecSettings) (\s a -> s {codecSettings = a} :: VideoDescription)
{-# DEPRECATED vdCodecSettings "Use generic-lens or generic-optics with 'codecSettings' instead." #-}

-- | Find additional transcoding features under Preprocessors (VideoPreprocessors). Enable the features at each output individually. These features are disabled by default.
--
-- /Note:/ Consider using 'videoPreprocessors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdVideoPreprocessors :: Lens.Lens' VideoDescription (Lude.Maybe VideoPreprocessor)
vdVideoPreprocessors = Lens.lens (videoPreprocessors :: VideoDescription -> Lude.Maybe VideoPreprocessor) (\s a -> s {videoPreprocessors = a} :: VideoDescription)
{-# DEPRECATED vdVideoPreprocessors "Use generic-lens or generic-optics with 'videoPreprocessors' instead." #-}

-- | Use Selection placement (position) to define the video area in your output frame. The area outside of the rectangle that you specify here is black.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdPosition :: Lens.Lens' VideoDescription (Lude.Maybe Rectangle)
vdPosition = Lens.lens (position :: VideoDescription -> Lude.Maybe Rectangle) (\s a -> s {position = a} :: VideoDescription)
{-# DEPRECATED vdPosition "Use generic-lens or generic-optics with 'position' instead." #-}

instance Lude.FromJSON VideoDescription where
  parseJSON =
    Lude.withObject
      "VideoDescription"
      ( \x ->
          VideoDescription'
            Lude.<$> (x Lude..:? "timecodeInsertion")
            Lude.<*> (x Lude..:? "height")
            Lude.<*> (x Lude..:? "afdSignaling")
            Lude.<*> (x Lude..:? "sharpness")
            Lude.<*> (x Lude..:? "crop")
            Lude.<*> (x Lude..:? "width")
            Lude.<*> (x Lude..:? "scalingBehavior")
            Lude.<*> (x Lude..:? "respondToAfd")
            Lude.<*> (x Lude..:? "dropFrameTimecode")
            Lude.<*> (x Lude..:? "antiAlias")
            Lude.<*> (x Lude..:? "fixedAfd")
            Lude.<*> (x Lude..:? "colorMetadata")
            Lude.<*> (x Lude..:? "codecSettings")
            Lude.<*> (x Lude..:? "videoPreprocessors")
            Lude.<*> (x Lude..:? "position")
      )

instance Lude.ToJSON VideoDescription where
  toJSON VideoDescription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("timecodeInsertion" Lude..=) Lude.<$> timecodeInsertion,
            ("height" Lude..=) Lude.<$> height,
            ("afdSignaling" Lude..=) Lude.<$> afdSignaling,
            ("sharpness" Lude..=) Lude.<$> sharpness,
            ("crop" Lude..=) Lude.<$> crop,
            ("width" Lude..=) Lude.<$> width,
            ("scalingBehavior" Lude..=) Lude.<$> scalingBehavior,
            ("respondToAfd" Lude..=) Lude.<$> respondToAfd,
            ("dropFrameTimecode" Lude..=) Lude.<$> dropFrameTimecode,
            ("antiAlias" Lude..=) Lude.<$> antiAlias,
            ("fixedAfd" Lude..=) Lude.<$> fixedAfd,
            ("colorMetadata" Lude..=) Lude.<$> colorMetadata,
            ("codecSettings" Lude..=) Lude.<$> codecSettings,
            ("videoPreprocessors" Lude..=) Lude.<$> videoPreprocessors,
            ("position" Lude..=) Lude.<$> position
          ]
      )
