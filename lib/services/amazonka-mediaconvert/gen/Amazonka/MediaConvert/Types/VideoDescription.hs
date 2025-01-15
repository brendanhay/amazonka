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
-- Module      : Amazonka.MediaConvert.Types.VideoDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.VideoDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AfdSignaling
import Amazonka.MediaConvert.Types.AntiAlias
import Amazonka.MediaConvert.Types.ColorMetadata
import Amazonka.MediaConvert.Types.DropFrameTimecode
import Amazonka.MediaConvert.Types.Rectangle
import Amazonka.MediaConvert.Types.RespondToAfd
import Amazonka.MediaConvert.Types.ScalingBehavior
import Amazonka.MediaConvert.Types.VideoCodecSettings
import Amazonka.MediaConvert.Types.VideoPreprocessor
import Amazonka.MediaConvert.Types.VideoTimecodeInsertion
import qualified Amazonka.Prelude as Prelude

-- | Settings related to video encoding of your output. The specific video
-- settings depend on the video codec that you choose. When you work
-- directly in your JSON job specification, include one instance of Video
-- description (VideoDescription) per output.
--
-- /See:/ 'newVideoDescription' smart constructor.
data VideoDescription = VideoDescription'
  { -- | This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert
    -- AFD signaling (AfdSignaling) to specify whether the service includes AFD
    -- values in the output video data and what those values are. * Choose None
    -- to remove all AFD values from this output. * Choose Fixed to ignore
    -- input AFD values and instead encode the value specified in the job. *
    -- Choose Auto to calculate output AFD values based on the input AFD scaler
    -- data.
    afdSignaling :: Prelude.Maybe AfdSignaling,
    -- | The anti-alias filter is automatically applied to all outputs. The
    -- service no longer accepts the value DISABLED for AntiAlias. If you
    -- specify that in your job, the service will ignore the setting.
    antiAlias :: Prelude.Maybe AntiAlias,
    -- | Video codec settings, (CodecSettings) under (VideoDescription), contains
    -- the group of settings related to video encoding. The settings in this
    -- group vary depending on the value that you choose for Video codec
    -- (Codec). For each codec enum that you choose, define the corresponding
    -- settings object. The following lists the codec enum, settings object
    -- pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE,
    -- FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings *
    -- MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8,
    -- Vp8Settings * VP9, Vp9Settings * XAVC, XavcSettings
    codecSettings :: Prelude.Maybe VideoCodecSettings,
    -- | Choose Insert (INSERT) for this setting to include color metadata in
    -- this output. Choose Ignore (IGNORE) to exclude color metadata from this
    -- output. If you don\'t specify a value, the service sets this to Insert
    -- by default.
    colorMetadata :: Prelude.Maybe ColorMetadata,
    -- | Use Cropping selection (crop) to specify the video area that the service
    -- will include in the output video frame.
    crop :: Prelude.Maybe Rectangle,
    -- | Applies only to 29.97 fps outputs. When this feature is enabled, the
    -- service will use drop-frame timecode on outputs. If it is not possible
    -- to use drop-frame timecode, the system will fall back to non-drop-frame.
    -- This setting is enabled by default when Timecode insertion
    -- (TimecodeInsertion) is enabled.
    dropFrameTimecode :: Prelude.Maybe DropFrameTimecode,
    -- | Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED).
    -- Use Fixed (FixedAfd) to specify a four-bit AFD value which the service
    -- will write on all frames of this video output.
    fixedAfd :: Prelude.Maybe Prelude.Natural,
    -- | Use the Height (Height) setting to define the video resolution height
    -- for this output. Specify in pixels. If you don\'t provide a value here,
    -- the service will use the input height.
    height :: Prelude.Maybe Prelude.Natural,
    -- | Use Selection placement (position) to define the video area in your
    -- output frame. The area outside of the rectangle that you specify here is
    -- black.
    position :: Prelude.Maybe Rectangle,
    -- | Use Respond to AFD (RespondToAfd) to specify how the service changes the
    -- video itself in response to AFD values in the input. * Choose Respond to
    -- clip the input video frame according to the AFD value, input display
    -- aspect ratio, and output display aspect ratio. * Choose Passthrough to
    -- include the input AFD values. Do not choose this when AfdSignaling is
    -- set to (NONE). A preferred implementation of this workflow is to set
    -- RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to
    -- remove all input AFD values from this output.
    respondToAfd :: Prelude.Maybe RespondToAfd,
    -- | Specify how the service handles outputs that have a different aspect
    -- ratio from the input aspect ratio. Choose Stretch to output
    -- (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit.
    -- Keep the setting Default (DEFAULT) to have the service letterbox your
    -- video instead. This setting overrides any value that you specify for the
    -- setting Selection placement (position) in this output.
    scalingBehavior :: Prelude.Maybe ScalingBehavior,
    -- | Use Sharpness (Sharpness) setting to specify the strength of
    -- anti-aliasing. This setting changes the width of the anti-alias filter
    -- kernel used for scaling. Sharpness only applies if your output
    -- resolution is different from your input resolution. 0 is the softest
    -- setting, 100 the sharpest, and 50 recommended for most content.
    sharpness :: Prelude.Maybe Prelude.Natural,
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
    timecodeInsertion :: Prelude.Maybe VideoTimecodeInsertion,
    -- | Find additional transcoding features under Preprocessors
    -- (VideoPreprocessors). Enable the features at each output individually.
    -- These features are disabled by default.
    videoPreprocessors :: Prelude.Maybe VideoPreprocessor,
    -- | Use Width (Width) to define the video resolution width, in pixels, for
    -- this output. If you don\'t provide a value here, the service will use
    -- the input width.
    width :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'afdSignaling', 'videoDescription_afdSignaling' - This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert
-- AFD signaling (AfdSignaling) to specify whether the service includes AFD
-- values in the output video data and what those values are. * Choose None
-- to remove all AFD values from this output. * Choose Fixed to ignore
-- input AFD values and instead encode the value specified in the job. *
-- Choose Auto to calculate output AFD values based on the input AFD scaler
-- data.
--
-- 'antiAlias', 'videoDescription_antiAlias' - The anti-alias filter is automatically applied to all outputs. The
-- service no longer accepts the value DISABLED for AntiAlias. If you
-- specify that in your job, the service will ignore the setting.
--
-- 'codecSettings', 'videoDescription_codecSettings' - Video codec settings, (CodecSettings) under (VideoDescription), contains
-- the group of settings related to video encoding. The settings in this
-- group vary depending on the value that you choose for Video codec
-- (Codec). For each codec enum that you choose, define the corresponding
-- settings object. The following lists the codec enum, settings object
-- pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE,
-- FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings *
-- MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8,
-- Vp8Settings * VP9, Vp9Settings * XAVC, XavcSettings
--
-- 'colorMetadata', 'videoDescription_colorMetadata' - Choose Insert (INSERT) for this setting to include color metadata in
-- this output. Choose Ignore (IGNORE) to exclude color metadata from this
-- output. If you don\'t specify a value, the service sets this to Insert
-- by default.
--
-- 'crop', 'videoDescription_crop' - Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame.
--
-- 'dropFrameTimecode', 'videoDescription_dropFrameTimecode' - Applies only to 29.97 fps outputs. When this feature is enabled, the
-- service will use drop-frame timecode on outputs. If it is not possible
-- to use drop-frame timecode, the system will fall back to non-drop-frame.
-- This setting is enabled by default when Timecode insertion
-- (TimecodeInsertion) is enabled.
--
-- 'fixedAfd', 'videoDescription_fixedAfd' - Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED).
-- Use Fixed (FixedAfd) to specify a four-bit AFD value which the service
-- will write on all frames of this video output.
--
-- 'height', 'videoDescription_height' - Use the Height (Height) setting to define the video resolution height
-- for this output. Specify in pixels. If you don\'t provide a value here,
-- the service will use the input height.
--
-- 'position', 'videoDescription_position' - Use Selection placement (position) to define the video area in your
-- output frame. The area outside of the rectangle that you specify here is
-- black.
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
-- 'scalingBehavior', 'videoDescription_scalingBehavior' - Specify how the service handles outputs that have a different aspect
-- ratio from the input aspect ratio. Choose Stretch to output
-- (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit.
-- Keep the setting Default (DEFAULT) to have the service letterbox your
-- video instead. This setting overrides any value that you specify for the
-- setting Selection placement (position) in this output.
--
-- 'sharpness', 'videoDescription_sharpness' - Use Sharpness (Sharpness) setting to specify the strength of
-- anti-aliasing. This setting changes the width of the anti-alias filter
-- kernel used for scaling. Sharpness only applies if your output
-- resolution is different from your input resolution. 0 is the softest
-- setting, 100 the sharpest, and 50 recommended for most content.
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
-- 'videoPreprocessors', 'videoDescription_videoPreprocessors' - Find additional transcoding features under Preprocessors
-- (VideoPreprocessors). Enable the features at each output individually.
-- These features are disabled by default.
--
-- 'width', 'videoDescription_width' - Use Width (Width) to define the video resolution width, in pixels, for
-- this output. If you don\'t provide a value here, the service will use
-- the input width.
newVideoDescription ::
  VideoDescription
newVideoDescription =
  VideoDescription'
    { afdSignaling = Prelude.Nothing,
      antiAlias = Prelude.Nothing,
      codecSettings = Prelude.Nothing,
      colorMetadata = Prelude.Nothing,
      crop = Prelude.Nothing,
      dropFrameTimecode = Prelude.Nothing,
      fixedAfd = Prelude.Nothing,
      height = Prelude.Nothing,
      position = Prelude.Nothing,
      respondToAfd = Prelude.Nothing,
      scalingBehavior = Prelude.Nothing,
      sharpness = Prelude.Nothing,
      timecodeInsertion = Prelude.Nothing,
      videoPreprocessors = Prelude.Nothing,
      width = Prelude.Nothing
    }

-- | This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert
-- AFD signaling (AfdSignaling) to specify whether the service includes AFD
-- values in the output video data and what those values are. * Choose None
-- to remove all AFD values from this output. * Choose Fixed to ignore
-- input AFD values and instead encode the value specified in the job. *
-- Choose Auto to calculate output AFD values based on the input AFD scaler
-- data.
videoDescription_afdSignaling :: Lens.Lens' VideoDescription (Prelude.Maybe AfdSignaling)
videoDescription_afdSignaling = Lens.lens (\VideoDescription' {afdSignaling} -> afdSignaling) (\s@VideoDescription' {} a -> s {afdSignaling = a} :: VideoDescription)

-- | The anti-alias filter is automatically applied to all outputs. The
-- service no longer accepts the value DISABLED for AntiAlias. If you
-- specify that in your job, the service will ignore the setting.
videoDescription_antiAlias :: Lens.Lens' VideoDescription (Prelude.Maybe AntiAlias)
videoDescription_antiAlias = Lens.lens (\VideoDescription' {antiAlias} -> antiAlias) (\s@VideoDescription' {} a -> s {antiAlias = a} :: VideoDescription)

-- | Video codec settings, (CodecSettings) under (VideoDescription), contains
-- the group of settings related to video encoding. The settings in this
-- group vary depending on the value that you choose for Video codec
-- (Codec). For each codec enum that you choose, define the corresponding
-- settings object. The following lists the codec enum, settings object
-- pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE,
-- FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings *
-- MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8,
-- Vp8Settings * VP9, Vp9Settings * XAVC, XavcSettings
videoDescription_codecSettings :: Lens.Lens' VideoDescription (Prelude.Maybe VideoCodecSettings)
videoDescription_codecSettings = Lens.lens (\VideoDescription' {codecSettings} -> codecSettings) (\s@VideoDescription' {} a -> s {codecSettings = a} :: VideoDescription)

-- | Choose Insert (INSERT) for this setting to include color metadata in
-- this output. Choose Ignore (IGNORE) to exclude color metadata from this
-- output. If you don\'t specify a value, the service sets this to Insert
-- by default.
videoDescription_colorMetadata :: Lens.Lens' VideoDescription (Prelude.Maybe ColorMetadata)
videoDescription_colorMetadata = Lens.lens (\VideoDescription' {colorMetadata} -> colorMetadata) (\s@VideoDescription' {} a -> s {colorMetadata = a} :: VideoDescription)

-- | Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame.
videoDescription_crop :: Lens.Lens' VideoDescription (Prelude.Maybe Rectangle)
videoDescription_crop = Lens.lens (\VideoDescription' {crop} -> crop) (\s@VideoDescription' {} a -> s {crop = a} :: VideoDescription)

-- | Applies only to 29.97 fps outputs. When this feature is enabled, the
-- service will use drop-frame timecode on outputs. If it is not possible
-- to use drop-frame timecode, the system will fall back to non-drop-frame.
-- This setting is enabled by default when Timecode insertion
-- (TimecodeInsertion) is enabled.
videoDescription_dropFrameTimecode :: Lens.Lens' VideoDescription (Prelude.Maybe DropFrameTimecode)
videoDescription_dropFrameTimecode = Lens.lens (\VideoDescription' {dropFrameTimecode} -> dropFrameTimecode) (\s@VideoDescription' {} a -> s {dropFrameTimecode = a} :: VideoDescription)

-- | Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED).
-- Use Fixed (FixedAfd) to specify a four-bit AFD value which the service
-- will write on all frames of this video output.
videoDescription_fixedAfd :: Lens.Lens' VideoDescription (Prelude.Maybe Prelude.Natural)
videoDescription_fixedAfd = Lens.lens (\VideoDescription' {fixedAfd} -> fixedAfd) (\s@VideoDescription' {} a -> s {fixedAfd = a} :: VideoDescription)

-- | Use the Height (Height) setting to define the video resolution height
-- for this output. Specify in pixels. If you don\'t provide a value here,
-- the service will use the input height.
videoDescription_height :: Lens.Lens' VideoDescription (Prelude.Maybe Prelude.Natural)
videoDescription_height = Lens.lens (\VideoDescription' {height} -> height) (\s@VideoDescription' {} a -> s {height = a} :: VideoDescription)

-- | Use Selection placement (position) to define the video area in your
-- output frame. The area outside of the rectangle that you specify here is
-- black.
videoDescription_position :: Lens.Lens' VideoDescription (Prelude.Maybe Rectangle)
videoDescription_position = Lens.lens (\VideoDescription' {position} -> position) (\s@VideoDescription' {} a -> s {position = a} :: VideoDescription)

-- | Use Respond to AFD (RespondToAfd) to specify how the service changes the
-- video itself in response to AFD values in the input. * Choose Respond to
-- clip the input video frame according to the AFD value, input display
-- aspect ratio, and output display aspect ratio. * Choose Passthrough to
-- include the input AFD values. Do not choose this when AfdSignaling is
-- set to (NONE). A preferred implementation of this workflow is to set
-- RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to
-- remove all input AFD values from this output.
videoDescription_respondToAfd :: Lens.Lens' VideoDescription (Prelude.Maybe RespondToAfd)
videoDescription_respondToAfd = Lens.lens (\VideoDescription' {respondToAfd} -> respondToAfd) (\s@VideoDescription' {} a -> s {respondToAfd = a} :: VideoDescription)

-- | Specify how the service handles outputs that have a different aspect
-- ratio from the input aspect ratio. Choose Stretch to output
-- (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit.
-- Keep the setting Default (DEFAULT) to have the service letterbox your
-- video instead. This setting overrides any value that you specify for the
-- setting Selection placement (position) in this output.
videoDescription_scalingBehavior :: Lens.Lens' VideoDescription (Prelude.Maybe ScalingBehavior)
videoDescription_scalingBehavior = Lens.lens (\VideoDescription' {scalingBehavior} -> scalingBehavior) (\s@VideoDescription' {} a -> s {scalingBehavior = a} :: VideoDescription)

-- | Use Sharpness (Sharpness) setting to specify the strength of
-- anti-aliasing. This setting changes the width of the anti-alias filter
-- kernel used for scaling. Sharpness only applies if your output
-- resolution is different from your input resolution. 0 is the softest
-- setting, 100 the sharpest, and 50 recommended for most content.
videoDescription_sharpness :: Lens.Lens' VideoDescription (Prelude.Maybe Prelude.Natural)
videoDescription_sharpness = Lens.lens (\VideoDescription' {sharpness} -> sharpness) (\s@VideoDescription' {} a -> s {sharpness = a} :: VideoDescription)

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
videoDescription_timecodeInsertion :: Lens.Lens' VideoDescription (Prelude.Maybe VideoTimecodeInsertion)
videoDescription_timecodeInsertion = Lens.lens (\VideoDescription' {timecodeInsertion} -> timecodeInsertion) (\s@VideoDescription' {} a -> s {timecodeInsertion = a} :: VideoDescription)

-- | Find additional transcoding features under Preprocessors
-- (VideoPreprocessors). Enable the features at each output individually.
-- These features are disabled by default.
videoDescription_videoPreprocessors :: Lens.Lens' VideoDescription (Prelude.Maybe VideoPreprocessor)
videoDescription_videoPreprocessors = Lens.lens (\VideoDescription' {videoPreprocessors} -> videoPreprocessors) (\s@VideoDescription' {} a -> s {videoPreprocessors = a} :: VideoDescription)

-- | Use Width (Width) to define the video resolution width, in pixels, for
-- this output. If you don\'t provide a value here, the service will use
-- the input width.
videoDescription_width :: Lens.Lens' VideoDescription (Prelude.Maybe Prelude.Natural)
videoDescription_width = Lens.lens (\VideoDescription' {width} -> width) (\s@VideoDescription' {} a -> s {width = a} :: VideoDescription)

instance Data.FromJSON VideoDescription where
  parseJSON =
    Data.withObject
      "VideoDescription"
      ( \x ->
          VideoDescription'
            Prelude.<$> (x Data..:? "afdSignaling")
            Prelude.<*> (x Data..:? "antiAlias")
            Prelude.<*> (x Data..:? "codecSettings")
            Prelude.<*> (x Data..:? "colorMetadata")
            Prelude.<*> (x Data..:? "crop")
            Prelude.<*> (x Data..:? "dropFrameTimecode")
            Prelude.<*> (x Data..:? "fixedAfd")
            Prelude.<*> (x Data..:? "height")
            Prelude.<*> (x Data..:? "position")
            Prelude.<*> (x Data..:? "respondToAfd")
            Prelude.<*> (x Data..:? "scalingBehavior")
            Prelude.<*> (x Data..:? "sharpness")
            Prelude.<*> (x Data..:? "timecodeInsertion")
            Prelude.<*> (x Data..:? "videoPreprocessors")
            Prelude.<*> (x Data..:? "width")
      )

instance Prelude.Hashable VideoDescription where
  hashWithSalt _salt VideoDescription' {..} =
    _salt
      `Prelude.hashWithSalt` afdSignaling
      `Prelude.hashWithSalt` antiAlias
      `Prelude.hashWithSalt` codecSettings
      `Prelude.hashWithSalt` colorMetadata
      `Prelude.hashWithSalt` crop
      `Prelude.hashWithSalt` dropFrameTimecode
      `Prelude.hashWithSalt` fixedAfd
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` respondToAfd
      `Prelude.hashWithSalt` scalingBehavior
      `Prelude.hashWithSalt` sharpness
      `Prelude.hashWithSalt` timecodeInsertion
      `Prelude.hashWithSalt` videoPreprocessors
      `Prelude.hashWithSalt` width

instance Prelude.NFData VideoDescription where
  rnf VideoDescription' {..} =
    Prelude.rnf afdSignaling `Prelude.seq`
      Prelude.rnf antiAlias `Prelude.seq`
        Prelude.rnf codecSettings `Prelude.seq`
          Prelude.rnf colorMetadata `Prelude.seq`
            Prelude.rnf crop `Prelude.seq`
              Prelude.rnf dropFrameTimecode `Prelude.seq`
                Prelude.rnf fixedAfd `Prelude.seq`
                  Prelude.rnf height `Prelude.seq`
                    Prelude.rnf position `Prelude.seq`
                      Prelude.rnf respondToAfd `Prelude.seq`
                        Prelude.rnf scalingBehavior `Prelude.seq`
                          Prelude.rnf sharpness `Prelude.seq`
                            Prelude.rnf timecodeInsertion `Prelude.seq`
                              Prelude.rnf videoPreprocessors `Prelude.seq`
                                Prelude.rnf width

instance Data.ToJSON VideoDescription where
  toJSON VideoDescription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("afdSignaling" Data..=) Prelude.<$> afdSignaling,
            ("antiAlias" Data..=) Prelude.<$> antiAlias,
            ("codecSettings" Data..=) Prelude.<$> codecSettings,
            ("colorMetadata" Data..=) Prelude.<$> colorMetadata,
            ("crop" Data..=) Prelude.<$> crop,
            ("dropFrameTimecode" Data..=)
              Prelude.<$> dropFrameTimecode,
            ("fixedAfd" Data..=) Prelude.<$> fixedAfd,
            ("height" Data..=) Prelude.<$> height,
            ("position" Data..=) Prelude.<$> position,
            ("respondToAfd" Data..=) Prelude.<$> respondToAfd,
            ("scalingBehavior" Data..=)
              Prelude.<$> scalingBehavior,
            ("sharpness" Data..=) Prelude.<$> sharpness,
            ("timecodeInsertion" Data..=)
              Prelude.<$> timecodeInsertion,
            ("videoPreprocessors" Data..=)
              Prelude.<$> videoPreprocessors,
            ("width" Data..=) Prelude.<$> width
          ]
      )
