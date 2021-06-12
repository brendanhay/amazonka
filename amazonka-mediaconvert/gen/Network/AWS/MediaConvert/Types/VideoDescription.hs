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
-- Module      : Network.AWS.MediaConvert.Types.VideoDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.VideoDescription where

import qualified Network.AWS.Core as Core
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

-- | Settings for video outputs
--
-- /See:/ 'newVideoDescription' smart constructor.
data VideoDescription = VideoDescription'
  { -- | Use the Height (Height) setting to define the video resolution height
    -- for this output. Specify in pixels. If you don\'t provide a value here,
    -- the service will use the input height.
    height :: Core.Maybe Core.Natural,
    -- | The anti-alias filter is automatically applied to all outputs. The
    -- service no longer accepts the value DISABLED for AntiAlias. If you
    -- specify that in your job, the service will ignore the setting.
    antiAlias :: Core.Maybe AntiAlias,
    -- | Applies only to 29.97 fps outputs. When this feature is enabled, the
    -- service will use drop-frame timecode on outputs. If it is not possible
    -- to use drop-frame timecode, the system will fall back to non-drop-frame.
    -- This setting is enabled by default when Timecode insertion
    -- (TimecodeInsertion) is enabled.
    dropFrameTimecode :: Core.Maybe DropFrameTimecode,
    -- | Use Respond to AFD (RespondToAfd) to specify how the service changes the
    -- video itself in response to AFD values in the input. * Choose Respond to
    -- clip the input video frame according to the AFD value, input display
    -- aspect ratio, and output display aspect ratio. * Choose Passthrough to
    -- include the input AFD values. Do not choose this when AfdSignaling is
    -- set to (NONE). A preferred implementation of this workflow is to set
    -- RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to
    -- remove all input AFD values from this output.
    respondToAfd :: Core.Maybe RespondToAfd,
    -- | Use Width (Width) to define the video resolution width, in pixels, for
    -- this output. If you don\'t provide a value here, the service will use
    -- the input width.
    width :: Core.Maybe Core.Natural,
    -- | Video codec settings, (CodecSettings) under (VideoDescription), contains
    -- the group of settings related to video encoding. The settings in this
    -- group vary depending on the value that you choose for Video codec
    -- (Codec). For each codec enum that you choose, define the corresponding
    -- settings object. The following lists the codec enum, settings object
    -- pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE,
    -- FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings *
    -- MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8,
    -- Vp8Settings * VP9, Vp9Settings
    codecSettings :: Core.Maybe VideoCodecSettings,
    -- | Choose Insert (INSERT) for this setting to include color metadata in
    -- this output. Choose Ignore (IGNORE) to exclude color metadata from this
    -- output. If you don\'t specify a value, the service sets this to Insert
    -- by default.
    colorMetadata :: Core.Maybe ColorMetadata,
    -- | Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED).
    -- Use Fixed (FixedAfd) to specify a four-bit AFD value which the service
    -- will write on all frames of this video output.
    fixedAfd :: Core.Maybe Core.Natural,
    -- | Applies only to H.264, H.265, MPEG2, and ProRes outputs. Only enable
    -- Timecode insertion when the input frame rate is identical to the output
    -- frame rate. To include timecodes in this output, set Timecode insertion
    -- (VideoTimecodeInsertion) to PIC_TIMING_SEI. To leave them out, set it to
    -- DISABLED. Default is DISABLED. When the service inserts timecodes in an
    -- output, by default, it uses any embedded timecodes from the input. If
    -- none are present, the service will set the timecode for the first output
    -- frame to zero. To change this default behavior, adjust the settings
    -- under Timecode configuration (TimecodeConfig). In the console, these
    -- settings are located under Job > Job settings > Timecode configuration.
    -- Note - Timecode source under input settings (InputTimecodeSource) does
    -- not affect the timecodes that are inserted in the output. Source under
    -- Job settings > Timecode configuration (TimecodeSource) does.
    timecodeInsertion :: Core.Maybe VideoTimecodeInsertion,
    -- | Specify how the service handles outputs that have a different aspect
    -- ratio from the input aspect ratio. Choose Stretch to output
    -- (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit.
    -- Keep the setting Default (DEFAULT) to have the service letterbox your
    -- video instead. This setting overrides any value that you specify for the
    -- setting Selection placement (position) in this output.
    scalingBehavior :: Core.Maybe ScalingBehavior,
    -- | Use Selection placement (position) to define the video area in your
    -- output frame. The area outside of the rectangle that you specify here is
    -- black.
    position :: Core.Maybe Rectangle,
    -- | Use Cropping selection (crop) to specify the video area that the service
    -- will include in the output video frame.
    crop :: Core.Maybe Rectangle,
    -- | Find additional transcoding features under Preprocessors
    -- (VideoPreprocessors). Enable the features at each output individually.
    -- These features are disabled by default.
    videoPreprocessors :: Core.Maybe VideoPreprocessor,
    -- | Use Sharpness (Sharpness) setting to specify the strength of
    -- anti-aliasing. This setting changes the width of the anti-alias filter
    -- kernel used for scaling. Sharpness only applies if your output
    -- resolution is different from your input resolution. 0 is the softest
    -- setting, 100 the sharpest, and 50 recommended for most content.
    sharpness :: Core.Maybe Core.Natural,
    -- | This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert
    -- AFD signaling (AfdSignaling) to specify whether the service includes AFD
    -- values in the output video data and what those values are. * Choose None
    -- to remove all AFD values from this output. * Choose Fixed to ignore
    -- input AFD values and instead encode the value specified in the job. *
    -- Choose Auto to calculate output AFD values based on the input AFD scaler
    -- data.
    afdSignaling :: Core.Maybe AfdSignaling
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VideoDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'height', 'videoDescription_height' - Use the Height (Height) setting to define the video resolution height
-- for this output. Specify in pixels. If you don\'t provide a value here,
-- the service will use the input height.
--
-- 'antiAlias', 'videoDescription_antiAlias' - The anti-alias filter is automatically applied to all outputs. The
-- service no longer accepts the value DISABLED for AntiAlias. If you
-- specify that in your job, the service will ignore the setting.
--
-- 'dropFrameTimecode', 'videoDescription_dropFrameTimecode' - Applies only to 29.97 fps outputs. When this feature is enabled, the
-- service will use drop-frame timecode on outputs. If it is not possible
-- to use drop-frame timecode, the system will fall back to non-drop-frame.
-- This setting is enabled by default when Timecode insertion
-- (TimecodeInsertion) is enabled.
--
-- 'respondToAfd', 'videoDescription_respondToAfd' - Use Respond to AFD (RespondToAfd) to specify how the service changes the
-- video itself in response to AFD values in the input. * Choose Respond to
-- clip the input video frame according to the AFD value, input display
-- aspect ratio, and output display aspect ratio. * Choose Passthrough to
-- include the input AFD values. Do not choose this when AfdSignaling is
-- set to (NONE). A preferred implementation of this workflow is to set
-- RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to
-- remove all input AFD values from this output.
--
-- 'width', 'videoDescription_width' - Use Width (Width) to define the video resolution width, in pixels, for
-- this output. If you don\'t provide a value here, the service will use
-- the input width.
--
-- 'codecSettings', 'videoDescription_codecSettings' - Video codec settings, (CodecSettings) under (VideoDescription), contains
-- the group of settings related to video encoding. The settings in this
-- group vary depending on the value that you choose for Video codec
-- (Codec). For each codec enum that you choose, define the corresponding
-- settings object. The following lists the codec enum, settings object
-- pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE,
-- FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings *
-- MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8,
-- Vp8Settings * VP9, Vp9Settings
--
-- 'colorMetadata', 'videoDescription_colorMetadata' - Choose Insert (INSERT) for this setting to include color metadata in
-- this output. Choose Ignore (IGNORE) to exclude color metadata from this
-- output. If you don\'t specify a value, the service sets this to Insert
-- by default.
--
-- 'fixedAfd', 'videoDescription_fixedAfd' - Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED).
-- Use Fixed (FixedAfd) to specify a four-bit AFD value which the service
-- will write on all frames of this video output.
--
-- 'timecodeInsertion', 'videoDescription_timecodeInsertion' - Applies only to H.264, H.265, MPEG2, and ProRes outputs. Only enable
-- Timecode insertion when the input frame rate is identical to the output
-- frame rate. To include timecodes in this output, set Timecode insertion
-- (VideoTimecodeInsertion) to PIC_TIMING_SEI. To leave them out, set it to
-- DISABLED. Default is DISABLED. When the service inserts timecodes in an
-- output, by default, it uses any embedded timecodes from the input. If
-- none are present, the service will set the timecode for the first output
-- frame to zero. To change this default behavior, adjust the settings
-- under Timecode configuration (TimecodeConfig). In the console, these
-- settings are located under Job > Job settings > Timecode configuration.
-- Note - Timecode source under input settings (InputTimecodeSource) does
-- not affect the timecodes that are inserted in the output. Source under
-- Job settings > Timecode configuration (TimecodeSource) does.
--
-- 'scalingBehavior', 'videoDescription_scalingBehavior' - Specify how the service handles outputs that have a different aspect
-- ratio from the input aspect ratio. Choose Stretch to output
-- (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit.
-- Keep the setting Default (DEFAULT) to have the service letterbox your
-- video instead. This setting overrides any value that you specify for the
-- setting Selection placement (position) in this output.
--
-- 'position', 'videoDescription_position' - Use Selection placement (position) to define the video area in your
-- output frame. The area outside of the rectangle that you specify here is
-- black.
--
-- 'crop', 'videoDescription_crop' - Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame.
--
-- 'videoPreprocessors', 'videoDescription_videoPreprocessors' - Find additional transcoding features under Preprocessors
-- (VideoPreprocessors). Enable the features at each output individually.
-- These features are disabled by default.
--
-- 'sharpness', 'videoDescription_sharpness' - Use Sharpness (Sharpness) setting to specify the strength of
-- anti-aliasing. This setting changes the width of the anti-alias filter
-- kernel used for scaling. Sharpness only applies if your output
-- resolution is different from your input resolution. 0 is the softest
-- setting, 100 the sharpest, and 50 recommended for most content.
--
-- 'afdSignaling', 'videoDescription_afdSignaling' - This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert
-- AFD signaling (AfdSignaling) to specify whether the service includes AFD
-- values in the output video data and what those values are. * Choose None
-- to remove all AFD values from this output. * Choose Fixed to ignore
-- input AFD values and instead encode the value specified in the job. *
-- Choose Auto to calculate output AFD values based on the input AFD scaler
-- data.
newVideoDescription ::
  VideoDescription
newVideoDescription =
  VideoDescription'
    { height = Core.Nothing,
      antiAlias = Core.Nothing,
      dropFrameTimecode = Core.Nothing,
      respondToAfd = Core.Nothing,
      width = Core.Nothing,
      codecSettings = Core.Nothing,
      colorMetadata = Core.Nothing,
      fixedAfd = Core.Nothing,
      timecodeInsertion = Core.Nothing,
      scalingBehavior = Core.Nothing,
      position = Core.Nothing,
      crop = Core.Nothing,
      videoPreprocessors = Core.Nothing,
      sharpness = Core.Nothing,
      afdSignaling = Core.Nothing
    }

-- | Use the Height (Height) setting to define the video resolution height
-- for this output. Specify in pixels. If you don\'t provide a value here,
-- the service will use the input height.
videoDescription_height :: Lens.Lens' VideoDescription (Core.Maybe Core.Natural)
videoDescription_height = Lens.lens (\VideoDescription' {height} -> height) (\s@VideoDescription' {} a -> s {height = a} :: VideoDescription)

-- | The anti-alias filter is automatically applied to all outputs. The
-- service no longer accepts the value DISABLED for AntiAlias. If you
-- specify that in your job, the service will ignore the setting.
videoDescription_antiAlias :: Lens.Lens' VideoDescription (Core.Maybe AntiAlias)
videoDescription_antiAlias = Lens.lens (\VideoDescription' {antiAlias} -> antiAlias) (\s@VideoDescription' {} a -> s {antiAlias = a} :: VideoDescription)

-- | Applies only to 29.97 fps outputs. When this feature is enabled, the
-- service will use drop-frame timecode on outputs. If it is not possible
-- to use drop-frame timecode, the system will fall back to non-drop-frame.
-- This setting is enabled by default when Timecode insertion
-- (TimecodeInsertion) is enabled.
videoDescription_dropFrameTimecode :: Lens.Lens' VideoDescription (Core.Maybe DropFrameTimecode)
videoDescription_dropFrameTimecode = Lens.lens (\VideoDescription' {dropFrameTimecode} -> dropFrameTimecode) (\s@VideoDescription' {} a -> s {dropFrameTimecode = a} :: VideoDescription)

-- | Use Respond to AFD (RespondToAfd) to specify how the service changes the
-- video itself in response to AFD values in the input. * Choose Respond to
-- clip the input video frame according to the AFD value, input display
-- aspect ratio, and output display aspect ratio. * Choose Passthrough to
-- include the input AFD values. Do not choose this when AfdSignaling is
-- set to (NONE). A preferred implementation of this workflow is to set
-- RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to
-- remove all input AFD values from this output.
videoDescription_respondToAfd :: Lens.Lens' VideoDescription (Core.Maybe RespondToAfd)
videoDescription_respondToAfd = Lens.lens (\VideoDescription' {respondToAfd} -> respondToAfd) (\s@VideoDescription' {} a -> s {respondToAfd = a} :: VideoDescription)

-- | Use Width (Width) to define the video resolution width, in pixels, for
-- this output. If you don\'t provide a value here, the service will use
-- the input width.
videoDescription_width :: Lens.Lens' VideoDescription (Core.Maybe Core.Natural)
videoDescription_width = Lens.lens (\VideoDescription' {width} -> width) (\s@VideoDescription' {} a -> s {width = a} :: VideoDescription)

-- | Video codec settings, (CodecSettings) under (VideoDescription), contains
-- the group of settings related to video encoding. The settings in this
-- group vary depending on the value that you choose for Video codec
-- (Codec). For each codec enum that you choose, define the corresponding
-- settings object. The following lists the codec enum, settings object
-- pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE,
-- FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings *
-- MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8,
-- Vp8Settings * VP9, Vp9Settings
videoDescription_codecSettings :: Lens.Lens' VideoDescription (Core.Maybe VideoCodecSettings)
videoDescription_codecSettings = Lens.lens (\VideoDescription' {codecSettings} -> codecSettings) (\s@VideoDescription' {} a -> s {codecSettings = a} :: VideoDescription)

-- | Choose Insert (INSERT) for this setting to include color metadata in
-- this output. Choose Ignore (IGNORE) to exclude color metadata from this
-- output. If you don\'t specify a value, the service sets this to Insert
-- by default.
videoDescription_colorMetadata :: Lens.Lens' VideoDescription (Core.Maybe ColorMetadata)
videoDescription_colorMetadata = Lens.lens (\VideoDescription' {colorMetadata} -> colorMetadata) (\s@VideoDescription' {} a -> s {colorMetadata = a} :: VideoDescription)

-- | Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED).
-- Use Fixed (FixedAfd) to specify a four-bit AFD value which the service
-- will write on all frames of this video output.
videoDescription_fixedAfd :: Lens.Lens' VideoDescription (Core.Maybe Core.Natural)
videoDescription_fixedAfd = Lens.lens (\VideoDescription' {fixedAfd} -> fixedAfd) (\s@VideoDescription' {} a -> s {fixedAfd = a} :: VideoDescription)

-- | Applies only to H.264, H.265, MPEG2, and ProRes outputs. Only enable
-- Timecode insertion when the input frame rate is identical to the output
-- frame rate. To include timecodes in this output, set Timecode insertion
-- (VideoTimecodeInsertion) to PIC_TIMING_SEI. To leave them out, set it to
-- DISABLED. Default is DISABLED. When the service inserts timecodes in an
-- output, by default, it uses any embedded timecodes from the input. If
-- none are present, the service will set the timecode for the first output
-- frame to zero. To change this default behavior, adjust the settings
-- under Timecode configuration (TimecodeConfig). In the console, these
-- settings are located under Job > Job settings > Timecode configuration.
-- Note - Timecode source under input settings (InputTimecodeSource) does
-- not affect the timecodes that are inserted in the output. Source under
-- Job settings > Timecode configuration (TimecodeSource) does.
videoDescription_timecodeInsertion :: Lens.Lens' VideoDescription (Core.Maybe VideoTimecodeInsertion)
videoDescription_timecodeInsertion = Lens.lens (\VideoDescription' {timecodeInsertion} -> timecodeInsertion) (\s@VideoDescription' {} a -> s {timecodeInsertion = a} :: VideoDescription)

-- | Specify how the service handles outputs that have a different aspect
-- ratio from the input aspect ratio. Choose Stretch to output
-- (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit.
-- Keep the setting Default (DEFAULT) to have the service letterbox your
-- video instead. This setting overrides any value that you specify for the
-- setting Selection placement (position) in this output.
videoDescription_scalingBehavior :: Lens.Lens' VideoDescription (Core.Maybe ScalingBehavior)
videoDescription_scalingBehavior = Lens.lens (\VideoDescription' {scalingBehavior} -> scalingBehavior) (\s@VideoDescription' {} a -> s {scalingBehavior = a} :: VideoDescription)

-- | Use Selection placement (position) to define the video area in your
-- output frame. The area outside of the rectangle that you specify here is
-- black.
videoDescription_position :: Lens.Lens' VideoDescription (Core.Maybe Rectangle)
videoDescription_position = Lens.lens (\VideoDescription' {position} -> position) (\s@VideoDescription' {} a -> s {position = a} :: VideoDescription)

-- | Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame.
videoDescription_crop :: Lens.Lens' VideoDescription (Core.Maybe Rectangle)
videoDescription_crop = Lens.lens (\VideoDescription' {crop} -> crop) (\s@VideoDescription' {} a -> s {crop = a} :: VideoDescription)

-- | Find additional transcoding features under Preprocessors
-- (VideoPreprocessors). Enable the features at each output individually.
-- These features are disabled by default.
videoDescription_videoPreprocessors :: Lens.Lens' VideoDescription (Core.Maybe VideoPreprocessor)
videoDescription_videoPreprocessors = Lens.lens (\VideoDescription' {videoPreprocessors} -> videoPreprocessors) (\s@VideoDescription' {} a -> s {videoPreprocessors = a} :: VideoDescription)

-- | Use Sharpness (Sharpness) setting to specify the strength of
-- anti-aliasing. This setting changes the width of the anti-alias filter
-- kernel used for scaling. Sharpness only applies if your output
-- resolution is different from your input resolution. 0 is the softest
-- setting, 100 the sharpest, and 50 recommended for most content.
videoDescription_sharpness :: Lens.Lens' VideoDescription (Core.Maybe Core.Natural)
videoDescription_sharpness = Lens.lens (\VideoDescription' {sharpness} -> sharpness) (\s@VideoDescription' {} a -> s {sharpness = a} :: VideoDescription)

-- | This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert
-- AFD signaling (AfdSignaling) to specify whether the service includes AFD
-- values in the output video data and what those values are. * Choose None
-- to remove all AFD values from this output. * Choose Fixed to ignore
-- input AFD values and instead encode the value specified in the job. *
-- Choose Auto to calculate output AFD values based on the input AFD scaler
-- data.
videoDescription_afdSignaling :: Lens.Lens' VideoDescription (Core.Maybe AfdSignaling)
videoDescription_afdSignaling = Lens.lens (\VideoDescription' {afdSignaling} -> afdSignaling) (\s@VideoDescription' {} a -> s {afdSignaling = a} :: VideoDescription)

instance Core.FromJSON VideoDescription where
  parseJSON =
    Core.withObject
      "VideoDescription"
      ( \x ->
          VideoDescription'
            Core.<$> (x Core..:? "height")
            Core.<*> (x Core..:? "antiAlias")
            Core.<*> (x Core..:? "dropFrameTimecode")
            Core.<*> (x Core..:? "respondToAfd")
            Core.<*> (x Core..:? "width")
            Core.<*> (x Core..:? "codecSettings")
            Core.<*> (x Core..:? "colorMetadata")
            Core.<*> (x Core..:? "fixedAfd")
            Core.<*> (x Core..:? "timecodeInsertion")
            Core.<*> (x Core..:? "scalingBehavior")
            Core.<*> (x Core..:? "position")
            Core.<*> (x Core..:? "crop")
            Core.<*> (x Core..:? "videoPreprocessors")
            Core.<*> (x Core..:? "sharpness")
            Core.<*> (x Core..:? "afdSignaling")
      )

instance Core.Hashable VideoDescription

instance Core.NFData VideoDescription

instance Core.ToJSON VideoDescription where
  toJSON VideoDescription' {..} =
    Core.object
      ( Core.catMaybes
          [ ("height" Core..=) Core.<$> height,
            ("antiAlias" Core..=) Core.<$> antiAlias,
            ("dropFrameTimecode" Core..=)
              Core.<$> dropFrameTimecode,
            ("respondToAfd" Core..=) Core.<$> respondToAfd,
            ("width" Core..=) Core.<$> width,
            ("codecSettings" Core..=) Core.<$> codecSettings,
            ("colorMetadata" Core..=) Core.<$> colorMetadata,
            ("fixedAfd" Core..=) Core.<$> fixedAfd,
            ("timecodeInsertion" Core..=)
              Core.<$> timecodeInsertion,
            ("scalingBehavior" Core..=) Core.<$> scalingBehavior,
            ("position" Core..=) Core.<$> position,
            ("crop" Core..=) Core.<$> crop,
            ("videoPreprocessors" Core..=)
              Core.<$> videoPreprocessors,
            ("sharpness" Core..=) Core.<$> sharpness,
            ("afdSignaling" Core..=) Core.<$> afdSignaling
          ]
      )
