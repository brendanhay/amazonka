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
-- Module      : Amazonka.ElasticTranscoder.Types.VideoParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.VideoParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types.PresetWatermark
import qualified Amazonka.Prelude as Prelude

-- | The @VideoParameters@ structure.
--
-- /See:/ 'newVideoParameters' smart constructor.
data VideoParameters = VideoParameters'
  { -- | Specify one of the following values to control scaling of the output
    -- video:
    --
    -- -   @Fit@: Elastic Transcoder scales the output video so it matches the
    --     value that you specified in either @MaxWidth@ or @MaxHeight@ without
    --     exceeding the other value.
    --
    -- -   @Fill@: Elastic Transcoder scales the output video so it matches the
    --     value that you specified in either @MaxWidth@ or @MaxHeight@ and
    --     matches or exceeds the other value. Elastic Transcoder centers the
    --     output video and then crops it in the dimension (if any) that
    --     exceeds the maximum value.
    --
    -- -   @Stretch@: Elastic Transcoder stretches the output video to match
    --     the values that you specified for @MaxWidth@ and @MaxHeight@. If the
    --     relative proportions of the input video and the output video are
    --     different, the output video will be distorted.
    --
    -- -   @Keep@: Elastic Transcoder does not scale the output video. If
    --     either dimension of the input video exceeds the values that you
    --     specified for @MaxWidth@ and @MaxHeight@, Elastic Transcoder crops
    --     the output video.
    --
    -- -   @ShrinkToFit@: Elastic Transcoder scales the output video down so
    --     that its dimensions match the values that you specified for at least
    --     one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If
    --     you specify this option, Elastic Transcoder does not scale the video
    --     up.
    --
    -- -   @ShrinkToFill@: Elastic Transcoder scales the output video down so
    --     that its dimensions match the values that you specified for at least
    --     one of @MaxWidth@ and @MaxHeight@ without dropping below either
    --     value. If you specify this option, Elastic Transcoder does not scale
    --     the video up.
    sizingPolicy :: Prelude.Maybe Prelude.Text,
    -- | Settings for the size, location, and opacity of graphics that you want
    -- Elastic Transcoder to overlay over videos that are transcoded using this
    -- preset. You can specify settings for up to four watermarks. Watermarks
    -- appear in the specified size and location, and with the specified
    -- opacity for the duration of the transcoded video.
    --
    -- Watermarks can be in .png or .jpg format. If you want to display a
    -- watermark that is not rectangular, use the .png format, which supports
    -- transparency.
    --
    -- When you create a job that uses this preset, you specify the .png or
    -- .jpg graphics that you want Elastic Transcoder to include in the
    -- transcoded videos. You can specify fewer graphics in the job than you
    -- specify watermark settings in the preset, which allows you to use the
    -- same preset for up to four watermarks that have different dimensions.
    watermarks :: Prelude.Maybe [PresetWatermark],
    -- | __Profile (H.264\/VP8\/VP9 Only)__
    --
    -- The H.264 profile that you want to use for the output file. Elastic
    -- Transcoder supports the following profiles:
    --
    -- -   @baseline@: The profile most commonly used for videoconferencing and
    --     for mobile applications.
    --
    -- -   @main@: The profile used for standard-definition digital TV
    --     broadcasts.
    --
    -- -   @high@: The profile used for high-definition digital TV broadcasts
    --     and for Blu-ray discs.
    --
    -- __Level (H.264 Only)__
    --
    -- The H.264 level that you want to use for the output file. Elastic
    -- Transcoder supports the following levels:
    --
    -- @1@, @1b@, @1.1@, @1.2@, @1.3@, @2@, @2.1@, @2.2@, @3@, @3.1@, @3.2@,
    -- @4@, @4.1@
    --
    -- __MaxReferenceFrames (H.264 Only)__
    --
    -- Applicable only when the value of Video:Codec is H.264. The maximum
    -- number of previously decoded frames to use as a reference for decoding
    -- future frames. Valid values are integers 0 through 16, but we recommend
    -- that you not use a value greater than the following:
    --
    -- @Min(Floor(Maximum decoded picture buffer in macroblocks * 256 \/ (Width in pixels * Height in pixels)), 16)@
    --
    -- where /Width in pixels/ and /Height in pixels/ represent either MaxWidth
    -- and MaxHeight, or Resolution. /Maximum decoded picture buffer in
    -- macroblocks/ depends on the value of the @Level@ object. See the list
    -- below. (A macroblock is a block of pixels measuring 16x16.)
    --
    -- -   1 - 396
    --
    -- -   1b - 396
    --
    -- -   1.1 - 900
    --
    -- -   1.2 - 2376
    --
    -- -   1.3 - 2376
    --
    -- -   2 - 2376
    --
    -- -   2.1 - 4752
    --
    -- -   2.2 - 8100
    --
    -- -   3 - 8100
    --
    -- -   3.1 - 18000
    --
    -- -   3.2 - 20480
    --
    -- -   4 - 32768
    --
    -- -   4.1 - 32768
    --
    -- __MaxBitRate (Optional, H.264\/MPEG2\/VP8\/VP9 only)__
    --
    -- The maximum number of bits per second in a video buffer; the size of the
    -- buffer is specified by @BufferSize@. Specify a value between 16 and
    -- 62,500. You can reduce the bandwidth required to stream a video by
    -- reducing the maximum bit rate, but this also reduces the quality of the
    -- video.
    --
    -- __BufferSize (Optional, H.264\/MPEG2\/VP8\/VP9 only)__
    --
    -- The maximum number of bits in any x seconds of the output video. This
    -- window is commonly 10 seconds, the standard segment duration when
    -- you\'re using FMP4 or MPEG-TS for the container type of the output
    -- video. Specify an integer greater than 0. If you specify @MaxBitRate@
    -- and omit @BufferSize@, Elastic Transcoder sets @BufferSize@ to 10 times
    -- the value of @MaxBitRate@.
    --
    -- __InterlacedMode (Optional, H.264\/MPEG2 Only)__
    --
    -- The interlace mode for the output video.
    --
    -- Interlaced video is used to double the perceived frame rate for a video
    -- by interlacing two fields (one field on every other line, the other
    -- field on the other lines) so that the human eye registers multiple
    -- pictures per frame. Interlacing reduces the bandwidth required for
    -- transmitting a video, but can result in blurred images and flickering.
    --
    -- Valid values include @Progressive@ (no interlacing, top to bottom),
    -- @TopFirst@ (top field first), @BottomFirst@ (bottom field first), and
    -- @Auto@.
    --
    -- If @InterlaceMode@ is not specified, Elastic Transcoder uses
    -- @Progressive@ for the output. If @Auto@ is specified, Elastic Transcoder
    -- interlaces the output.
    --
    -- __ColorSpaceConversionMode (Optional, H.264\/MPEG2 Only)__
    --
    -- The color space conversion Elastic Transcoder applies to the output
    -- video. Color spaces are the algorithms used by the computer to store
    -- information about how to render color. @Bt.601@ is the standard for
    -- standard definition video, while @Bt.709@ is the standard for high
    -- definition video.
    --
    -- Valid values include @None@, @Bt709toBt601@, @Bt601toBt709@, and @Auto@.
    --
    -- If you chose @Auto@ for @ColorSpaceConversionMode@ and your output is
    -- interlaced, your frame rate is one of @23.97@, @24@, @25@, @29.97@,
    -- @50@, or @60@, your @SegmentDuration@ is null, and you are using one of
    -- the resolution changes from the list below, Elastic Transcoder applies
    -- the following color space conversions:
    --
    -- -   /Standard to HD, 720x480 to 1920x1080/ - Elastic Transcoder applies
    --     @Bt601ToBt709@
    --
    -- -   /Standard to HD, 720x576 to 1920x1080/ - Elastic Transcoder applies
    --     @Bt601ToBt709@
    --
    -- -   /HD to Standard, 1920x1080 to 720x480/ - Elastic Transcoder applies
    --     @Bt709ToBt601@
    --
    -- -   /HD to Standard, 1920x1080 to 720x576/ - Elastic Transcoder applies
    --     @Bt709ToBt601@
    --
    -- Elastic Transcoder may change the behavior of the
    -- @ColorspaceConversionMode@ @Auto@ mode in the future. All outputs in a
    -- playlist must use the same @ColorSpaceConversionMode@.
    --
    -- If you do not specify a @ColorSpaceConversionMode@, Elastic Transcoder
    -- does not change the color space of a file. If you are unsure what
    -- @ColorSpaceConversionMode@ was applied to your output file, you can
    -- check the @AppliedColorSpaceConversion@ parameter included in your job
    -- response. If your job does not have an @AppliedColorSpaceConversion@ in
    -- its response, no @ColorSpaceConversionMode@ was applied.
    --
    -- __ChromaSubsampling__
    --
    -- The sampling pattern for the chroma (color) channels of the output
    -- video. Valid values include @yuv420p@ and @yuv422p@.
    --
    -- @yuv420p@ samples the chroma information of every other horizontal and
    -- every other vertical line, @yuv422p@ samples the color information of
    -- every horizontal line and every other vertical line.
    --
    -- __LoopCount (Gif Only)__
    --
    -- The number of times you want the output gif to loop. Valid values
    -- include @Infinite@ and integers between @0@ and @100@, inclusive.
    codecOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add black
    -- bars to the top and bottom and\/or left and right sides of the output
    -- video to make the total size of the output video match the values that
    -- you specified for @MaxWidth@ and @MaxHeight@.
    paddingPolicy :: Prelude.Maybe Prelude.Text,
    -- | Applicable only when the value of Video:Codec is one of @H.264@,
    -- @MPEG2@, or @VP8@.
    --
    -- The maximum number of frames between key frames. Key frames are fully
    -- encoded frames; the frames between key frames are encoded based, in
    -- part, on the content of the key frames. The value is an integer
    -- formatted as a string; valid values are between 1 (every frame is a key
    -- frame) and 100000, inclusive. A higher value results in higher
    -- compression but may also discernibly decrease video quality.
    --
    -- For @Smooth@ outputs, the @FrameRate@ must have a constant ratio to the
    -- @KeyframesMaxDist@. This allows @Smooth@ playlists to switch between
    -- different quality levels while the file is being played.
    --
    -- For example, an input file can have a @FrameRate@ of 30 with a
    -- @KeyframesMaxDist@ of 90. The output file then needs to have a ratio of
    -- 1:3. Valid outputs would have @FrameRate@ of 30, 25, and 10, and
    -- @KeyframesMaxDist@ of 90, 75, and 30, respectively.
    --
    -- Alternately, this can be achieved by setting @FrameRate@ to auto and
    -- having the same values for @MaxFrameRate@ and @KeyframesMaxDist@.
    keyframesMaxDist :: Prelude.Maybe Prelude.Text,
    -- | The bit rate of the video stream in the output file, in
    -- kilobits\/second. Valid values depend on the values of @Level@ and
    -- @Profile@. If you specify @auto@, Elastic Transcoder uses the detected
    -- bit rate of the input source. If you specify a value other than @auto@,
    -- we recommend that you specify a value less than or equal to the maximum
    -- H.264-compliant value listed for your level and profile:
    --
    -- /Level - Maximum video bit rate in kilobits\/second (baseline and main
    -- Profile) : maximum video bit rate in kilobits\/second (high Profile)/
    --
    -- -   1 - 64 : 80
    --
    -- -   1b - 128 : 160
    --
    -- -   1.1 - 192 : 240
    --
    -- -   1.2 - 384 : 480
    --
    -- -   1.3 - 768 : 960
    --
    -- -   2 - 2000 : 2500
    --
    -- -   3 - 10000 : 12500
    --
    -- -   3.1 - 14000 : 17500
    --
    -- -   3.2 - 20000 : 25000
    --
    -- -   4 - 20000 : 25000
    --
    -- -   4.1 - 50000 : 62500
    bitRate :: Prelude.Maybe Prelude.Text,
    -- | To better control resolution and aspect ratio of output videos, we
    -- recommend that you use the values @MaxWidth@, @MaxHeight@,
    -- @SizingPolicy@, @PaddingPolicy@, and @DisplayAspectRatio@ instead of
    -- @Resolution@ and @AspectRatio@. The two groups of settings are mutually
    -- exclusive. Do not use them together.
    --
    -- The display aspect ratio of the video in the output file. Valid values
    -- include:
    --
    -- @auto@, @1:1@, @4:3@, @3:2@, @16:9@
    --
    -- If you specify @auto@, Elastic Transcoder tries to preserve the aspect
    -- ratio of the input file.
    --
    -- If you specify an aspect ratio for the output file that differs from
    -- aspect ratio of the input file, Elastic Transcoder adds pillarboxing
    -- (black bars on the sides) or letterboxing (black bars on the top and
    -- bottom) to maintain the aspect ratio of the active region of the video.
    aspectRatio :: Prelude.Maybe Prelude.Text,
    -- | The video codec for the output file. Valid values include @gif@,
    -- @H.264@, @mpeg2@, @vp8@, and @vp9@. You can only specify @vp8@ and @vp9@
    -- when the container type is @webm@, @gif@ when the container type is
    -- @gif@, and @mpeg2@ when the container type is @mpg@.
    codec :: Prelude.Maybe Prelude.Text,
    -- | Applicable only when the value of Video:Codec is one of @H.264@,
    -- @MPEG2@, or @VP8@.
    --
    -- Whether to use a fixed value for @FixedGOP@. Valid values are @true@ and
    -- @false@:
    --
    -- -   @true@: Elastic Transcoder uses the value of @KeyframesMaxDist@ for
    --     the distance between key frames (the number of frames in a group of
    --     pictures, or GOP).
    --
    -- -   @false@: The distance between key frames can vary.
    --
    -- @FixedGOP@ must be set to @true@ for @fmp4@ containers.
    fixedGOP :: Prelude.Maybe Prelude.Text,
    -- | To better control resolution and aspect ratio of output videos, we
    -- recommend that you use the values @MaxWidth@, @MaxHeight@,
    -- @SizingPolicy@, @PaddingPolicy@, and @DisplayAspectRatio@ instead of
    -- @Resolution@ and @AspectRatio@. The two groups of settings are mutually
    -- exclusive. Do not use them together.
    --
    -- The width and height of the video in the output file, in pixels. Valid
    -- values are @auto@ and /width/ x /height/:
    --
    -- -   @auto@: Elastic Transcoder attempts to preserve the width and height
    --     of the input file, subject to the following rules.
    --
    -- -   @ width x height @: The width and height of the output video in
    --     pixels.
    --
    -- Note the following about specifying the width and height:
    --
    -- -   The width must be an even integer between 128 and 4096, inclusive.
    --
    -- -   The height must be an even integer between 96 and 3072, inclusive.
    --
    -- -   If you specify a resolution that is less than the resolution of the
    --     input file, Elastic Transcoder rescales the output file to the lower
    --     resolution.
    --
    -- -   If you specify a resolution that is greater than the resolution of
    --     the input file, Elastic Transcoder rescales the output to the higher
    --     resolution.
    --
    -- -   We recommend that you specify a resolution for which the product of
    --     width and height is less than or equal to the applicable value in
    --     the following list (/List - Max width x height value/):
    --
    --     -   1 - 25344
    --
    --     -   1b - 25344
    --
    --     -   1.1 - 101376
    --
    --     -   1.2 - 101376
    --
    --     -   1.3 - 101376
    --
    --     -   2 - 101376
    --
    --     -   2.1 - 202752
    --
    --     -   2.2 - 404720
    --
    --     -   3 - 404720
    --
    --     -   3.1 - 921600
    --
    --     -   3.2 - 1310720
    --
    --     -   4 - 2097152
    --
    --     -   4.1 - 2097152
    resolution :: Prelude.Maybe Prelude.Text,
    -- | The maximum height of the output video in pixels. If you specify @auto@,
    -- Elastic Transcoder uses 1080 (Full HD) as the default value. If you
    -- specify a numeric value, enter an even integer between 96 and 3072.
    maxHeight :: Prelude.Maybe Prelude.Text,
    -- | The value that Elastic Transcoder adds to the metadata in the output
    -- file.
    displayAspectRatio :: Prelude.Maybe Prelude.Text,
    -- | The maximum width of the output video in pixels. If you specify @auto@,
    -- Elastic Transcoder uses 1920 (Full HD) as the default value. If you
    -- specify a numeric value, enter an even integer between 128 and 4096.
    maxWidth :: Prelude.Maybe Prelude.Text,
    -- | If you specify @auto@ for @FrameRate@, Elastic Transcoder uses the frame
    -- rate of the input video for the frame rate of the output video. Specify
    -- the maximum frame rate that you want Elastic Transcoder to use when the
    -- frame rate of the input video is greater than the desired maximum frame
    -- rate of the output video. Valid values include: @10@, @15@, @23.97@,
    -- @24@, @25@, @29.97@, @30@, @60@.
    maxFrameRate :: Prelude.Maybe Prelude.Text,
    -- | The frames per second for the video stream in the output file. Valid
    -- values include:
    --
    -- @auto@, @10@, @15@, @23.97@, @24@, @25@, @29.97@, @30@, @60@
    --
    -- If you specify @auto@, Elastic Transcoder uses the detected frame rate
    -- of the input source. If you specify a frame rate, we recommend that you
    -- perform the following calculation:
    --
    -- @Frame rate = maximum recommended decoding speed in luma samples\/second \/ (width in pixels * height in pixels)@
    --
    -- where:
    --
    -- -   /width in pixels/ and /height in pixels/ represent the Resolution of
    --     the output video.
    --
    -- -   /maximum recommended decoding speed in Luma samples\/second/ is less
    --     than or equal to the maximum value listed in the following table,
    --     based on the value that you specified for Level.
    --
    -- The maximum recommended decoding speed in Luma samples\/second for each
    -- level is described in the following list (/Level - Decoding speed/):
    --
    -- -   1 - 380160
    --
    -- -   1b - 380160
    --
    -- -   1.1 - 76800
    --
    -- -   1.2 - 1536000
    --
    -- -   1.3 - 3041280
    --
    -- -   2 - 3041280
    --
    -- -   2.1 - 5068800
    --
    -- -   2.2 - 5184000
    --
    -- -   3 - 10368000
    --
    -- -   3.1 - 27648000
    --
    -- -   3.2 - 55296000
    --
    -- -   4 - 62914560
    --
    -- -   4.1 - 62914560
    frameRate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizingPolicy', 'videoParameters_sizingPolicy' - Specify one of the following values to control scaling of the output
-- video:
--
-- -   @Fit@: Elastic Transcoder scales the output video so it matches the
--     value that you specified in either @MaxWidth@ or @MaxHeight@ without
--     exceeding the other value.
--
-- -   @Fill@: Elastic Transcoder scales the output video so it matches the
--     value that you specified in either @MaxWidth@ or @MaxHeight@ and
--     matches or exceeds the other value. Elastic Transcoder centers the
--     output video and then crops it in the dimension (if any) that
--     exceeds the maximum value.
--
-- -   @Stretch@: Elastic Transcoder stretches the output video to match
--     the values that you specified for @MaxWidth@ and @MaxHeight@. If the
--     relative proportions of the input video and the output video are
--     different, the output video will be distorted.
--
-- -   @Keep@: Elastic Transcoder does not scale the output video. If
--     either dimension of the input video exceeds the values that you
--     specified for @MaxWidth@ and @MaxHeight@, Elastic Transcoder crops
--     the output video.
--
-- -   @ShrinkToFit@: Elastic Transcoder scales the output video down so
--     that its dimensions match the values that you specified for at least
--     one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If
--     you specify this option, Elastic Transcoder does not scale the video
--     up.
--
-- -   @ShrinkToFill@: Elastic Transcoder scales the output video down so
--     that its dimensions match the values that you specified for at least
--     one of @MaxWidth@ and @MaxHeight@ without dropping below either
--     value. If you specify this option, Elastic Transcoder does not scale
--     the video up.
--
-- 'watermarks', 'videoParameters_watermarks' - Settings for the size, location, and opacity of graphics that you want
-- Elastic Transcoder to overlay over videos that are transcoded using this
-- preset. You can specify settings for up to four watermarks. Watermarks
-- appear in the specified size and location, and with the specified
-- opacity for the duration of the transcoded video.
--
-- Watermarks can be in .png or .jpg format. If you want to display a
-- watermark that is not rectangular, use the .png format, which supports
-- transparency.
--
-- When you create a job that uses this preset, you specify the .png or
-- .jpg graphics that you want Elastic Transcoder to include in the
-- transcoded videos. You can specify fewer graphics in the job than you
-- specify watermark settings in the preset, which allows you to use the
-- same preset for up to four watermarks that have different dimensions.
--
-- 'codecOptions', 'videoParameters_codecOptions' - __Profile (H.264\/VP8\/VP9 Only)__
--
-- The H.264 profile that you want to use for the output file. Elastic
-- Transcoder supports the following profiles:
--
-- -   @baseline@: The profile most commonly used for videoconferencing and
--     for mobile applications.
--
-- -   @main@: The profile used for standard-definition digital TV
--     broadcasts.
--
-- -   @high@: The profile used for high-definition digital TV broadcasts
--     and for Blu-ray discs.
--
-- __Level (H.264 Only)__
--
-- The H.264 level that you want to use for the output file. Elastic
-- Transcoder supports the following levels:
--
-- @1@, @1b@, @1.1@, @1.2@, @1.3@, @2@, @2.1@, @2.2@, @3@, @3.1@, @3.2@,
-- @4@, @4.1@
--
-- __MaxReferenceFrames (H.264 Only)__
--
-- Applicable only when the value of Video:Codec is H.264. The maximum
-- number of previously decoded frames to use as a reference for decoding
-- future frames. Valid values are integers 0 through 16, but we recommend
-- that you not use a value greater than the following:
--
-- @Min(Floor(Maximum decoded picture buffer in macroblocks * 256 \/ (Width in pixels * Height in pixels)), 16)@
--
-- where /Width in pixels/ and /Height in pixels/ represent either MaxWidth
-- and MaxHeight, or Resolution. /Maximum decoded picture buffer in
-- macroblocks/ depends on the value of the @Level@ object. See the list
-- below. (A macroblock is a block of pixels measuring 16x16.)
--
-- -   1 - 396
--
-- -   1b - 396
--
-- -   1.1 - 900
--
-- -   1.2 - 2376
--
-- -   1.3 - 2376
--
-- -   2 - 2376
--
-- -   2.1 - 4752
--
-- -   2.2 - 8100
--
-- -   3 - 8100
--
-- -   3.1 - 18000
--
-- -   3.2 - 20480
--
-- -   4 - 32768
--
-- -   4.1 - 32768
--
-- __MaxBitRate (Optional, H.264\/MPEG2\/VP8\/VP9 only)__
--
-- The maximum number of bits per second in a video buffer; the size of the
-- buffer is specified by @BufferSize@. Specify a value between 16 and
-- 62,500. You can reduce the bandwidth required to stream a video by
-- reducing the maximum bit rate, but this also reduces the quality of the
-- video.
--
-- __BufferSize (Optional, H.264\/MPEG2\/VP8\/VP9 only)__
--
-- The maximum number of bits in any x seconds of the output video. This
-- window is commonly 10 seconds, the standard segment duration when
-- you\'re using FMP4 or MPEG-TS for the container type of the output
-- video. Specify an integer greater than 0. If you specify @MaxBitRate@
-- and omit @BufferSize@, Elastic Transcoder sets @BufferSize@ to 10 times
-- the value of @MaxBitRate@.
--
-- __InterlacedMode (Optional, H.264\/MPEG2 Only)__
--
-- The interlace mode for the output video.
--
-- Interlaced video is used to double the perceived frame rate for a video
-- by interlacing two fields (one field on every other line, the other
-- field on the other lines) so that the human eye registers multiple
-- pictures per frame. Interlacing reduces the bandwidth required for
-- transmitting a video, but can result in blurred images and flickering.
--
-- Valid values include @Progressive@ (no interlacing, top to bottom),
-- @TopFirst@ (top field first), @BottomFirst@ (bottom field first), and
-- @Auto@.
--
-- If @InterlaceMode@ is not specified, Elastic Transcoder uses
-- @Progressive@ for the output. If @Auto@ is specified, Elastic Transcoder
-- interlaces the output.
--
-- __ColorSpaceConversionMode (Optional, H.264\/MPEG2 Only)__
--
-- The color space conversion Elastic Transcoder applies to the output
-- video. Color spaces are the algorithms used by the computer to store
-- information about how to render color. @Bt.601@ is the standard for
-- standard definition video, while @Bt.709@ is the standard for high
-- definition video.
--
-- Valid values include @None@, @Bt709toBt601@, @Bt601toBt709@, and @Auto@.
--
-- If you chose @Auto@ for @ColorSpaceConversionMode@ and your output is
-- interlaced, your frame rate is one of @23.97@, @24@, @25@, @29.97@,
-- @50@, or @60@, your @SegmentDuration@ is null, and you are using one of
-- the resolution changes from the list below, Elastic Transcoder applies
-- the following color space conversions:
--
-- -   /Standard to HD, 720x480 to 1920x1080/ - Elastic Transcoder applies
--     @Bt601ToBt709@
--
-- -   /Standard to HD, 720x576 to 1920x1080/ - Elastic Transcoder applies
--     @Bt601ToBt709@
--
-- -   /HD to Standard, 1920x1080 to 720x480/ - Elastic Transcoder applies
--     @Bt709ToBt601@
--
-- -   /HD to Standard, 1920x1080 to 720x576/ - Elastic Transcoder applies
--     @Bt709ToBt601@
--
-- Elastic Transcoder may change the behavior of the
-- @ColorspaceConversionMode@ @Auto@ mode in the future. All outputs in a
-- playlist must use the same @ColorSpaceConversionMode@.
--
-- If you do not specify a @ColorSpaceConversionMode@, Elastic Transcoder
-- does not change the color space of a file. If you are unsure what
-- @ColorSpaceConversionMode@ was applied to your output file, you can
-- check the @AppliedColorSpaceConversion@ parameter included in your job
-- response. If your job does not have an @AppliedColorSpaceConversion@ in
-- its response, no @ColorSpaceConversionMode@ was applied.
--
-- __ChromaSubsampling__
--
-- The sampling pattern for the chroma (color) channels of the output
-- video. Valid values include @yuv420p@ and @yuv422p@.
--
-- @yuv420p@ samples the chroma information of every other horizontal and
-- every other vertical line, @yuv422p@ samples the color information of
-- every horizontal line and every other vertical line.
--
-- __LoopCount (Gif Only)__
--
-- The number of times you want the output gif to loop. Valid values
-- include @Infinite@ and integers between @0@ and @100@, inclusive.
--
-- 'paddingPolicy', 'videoParameters_paddingPolicy' - When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add black
-- bars to the top and bottom and\/or left and right sides of the output
-- video to make the total size of the output video match the values that
-- you specified for @MaxWidth@ and @MaxHeight@.
--
-- 'keyframesMaxDist', 'videoParameters_keyframesMaxDist' - Applicable only when the value of Video:Codec is one of @H.264@,
-- @MPEG2@, or @VP8@.
--
-- The maximum number of frames between key frames. Key frames are fully
-- encoded frames; the frames between key frames are encoded based, in
-- part, on the content of the key frames. The value is an integer
-- formatted as a string; valid values are between 1 (every frame is a key
-- frame) and 100000, inclusive. A higher value results in higher
-- compression but may also discernibly decrease video quality.
--
-- For @Smooth@ outputs, the @FrameRate@ must have a constant ratio to the
-- @KeyframesMaxDist@. This allows @Smooth@ playlists to switch between
-- different quality levels while the file is being played.
--
-- For example, an input file can have a @FrameRate@ of 30 with a
-- @KeyframesMaxDist@ of 90. The output file then needs to have a ratio of
-- 1:3. Valid outputs would have @FrameRate@ of 30, 25, and 10, and
-- @KeyframesMaxDist@ of 90, 75, and 30, respectively.
--
-- Alternately, this can be achieved by setting @FrameRate@ to auto and
-- having the same values for @MaxFrameRate@ and @KeyframesMaxDist@.
--
-- 'bitRate', 'videoParameters_bitRate' - The bit rate of the video stream in the output file, in
-- kilobits\/second. Valid values depend on the values of @Level@ and
-- @Profile@. If you specify @auto@, Elastic Transcoder uses the detected
-- bit rate of the input source. If you specify a value other than @auto@,
-- we recommend that you specify a value less than or equal to the maximum
-- H.264-compliant value listed for your level and profile:
--
-- /Level - Maximum video bit rate in kilobits\/second (baseline and main
-- Profile) : maximum video bit rate in kilobits\/second (high Profile)/
--
-- -   1 - 64 : 80
--
-- -   1b - 128 : 160
--
-- -   1.1 - 192 : 240
--
-- -   1.2 - 384 : 480
--
-- -   1.3 - 768 : 960
--
-- -   2 - 2000 : 2500
--
-- -   3 - 10000 : 12500
--
-- -   3.1 - 14000 : 17500
--
-- -   3.2 - 20000 : 25000
--
-- -   4 - 20000 : 25000
--
-- -   4.1 - 50000 : 62500
--
-- 'aspectRatio', 'videoParameters_aspectRatio' - To better control resolution and aspect ratio of output videos, we
-- recommend that you use the values @MaxWidth@, @MaxHeight@,
-- @SizingPolicy@, @PaddingPolicy@, and @DisplayAspectRatio@ instead of
-- @Resolution@ and @AspectRatio@. The two groups of settings are mutually
-- exclusive. Do not use them together.
--
-- The display aspect ratio of the video in the output file. Valid values
-- include:
--
-- @auto@, @1:1@, @4:3@, @3:2@, @16:9@
--
-- If you specify @auto@, Elastic Transcoder tries to preserve the aspect
-- ratio of the input file.
--
-- If you specify an aspect ratio for the output file that differs from
-- aspect ratio of the input file, Elastic Transcoder adds pillarboxing
-- (black bars on the sides) or letterboxing (black bars on the top and
-- bottom) to maintain the aspect ratio of the active region of the video.
--
-- 'codec', 'videoParameters_codec' - The video codec for the output file. Valid values include @gif@,
-- @H.264@, @mpeg2@, @vp8@, and @vp9@. You can only specify @vp8@ and @vp9@
-- when the container type is @webm@, @gif@ when the container type is
-- @gif@, and @mpeg2@ when the container type is @mpg@.
--
-- 'fixedGOP', 'videoParameters_fixedGOP' - Applicable only when the value of Video:Codec is one of @H.264@,
-- @MPEG2@, or @VP8@.
--
-- Whether to use a fixed value for @FixedGOP@. Valid values are @true@ and
-- @false@:
--
-- -   @true@: Elastic Transcoder uses the value of @KeyframesMaxDist@ for
--     the distance between key frames (the number of frames in a group of
--     pictures, or GOP).
--
-- -   @false@: The distance between key frames can vary.
--
-- @FixedGOP@ must be set to @true@ for @fmp4@ containers.
--
-- 'resolution', 'videoParameters_resolution' - To better control resolution and aspect ratio of output videos, we
-- recommend that you use the values @MaxWidth@, @MaxHeight@,
-- @SizingPolicy@, @PaddingPolicy@, and @DisplayAspectRatio@ instead of
-- @Resolution@ and @AspectRatio@. The two groups of settings are mutually
-- exclusive. Do not use them together.
--
-- The width and height of the video in the output file, in pixels. Valid
-- values are @auto@ and /width/ x /height/:
--
-- -   @auto@: Elastic Transcoder attempts to preserve the width and height
--     of the input file, subject to the following rules.
--
-- -   @ width x height @: The width and height of the output video in
--     pixels.
--
-- Note the following about specifying the width and height:
--
-- -   The width must be an even integer between 128 and 4096, inclusive.
--
-- -   The height must be an even integer between 96 and 3072, inclusive.
--
-- -   If you specify a resolution that is less than the resolution of the
--     input file, Elastic Transcoder rescales the output file to the lower
--     resolution.
--
-- -   If you specify a resolution that is greater than the resolution of
--     the input file, Elastic Transcoder rescales the output to the higher
--     resolution.
--
-- -   We recommend that you specify a resolution for which the product of
--     width and height is less than or equal to the applicable value in
--     the following list (/List - Max width x height value/):
--
--     -   1 - 25344
--
--     -   1b - 25344
--
--     -   1.1 - 101376
--
--     -   1.2 - 101376
--
--     -   1.3 - 101376
--
--     -   2 - 101376
--
--     -   2.1 - 202752
--
--     -   2.2 - 404720
--
--     -   3 - 404720
--
--     -   3.1 - 921600
--
--     -   3.2 - 1310720
--
--     -   4 - 2097152
--
--     -   4.1 - 2097152
--
-- 'maxHeight', 'videoParameters_maxHeight' - The maximum height of the output video in pixels. If you specify @auto@,
-- Elastic Transcoder uses 1080 (Full HD) as the default value. If you
-- specify a numeric value, enter an even integer between 96 and 3072.
--
-- 'displayAspectRatio', 'videoParameters_displayAspectRatio' - The value that Elastic Transcoder adds to the metadata in the output
-- file.
--
-- 'maxWidth', 'videoParameters_maxWidth' - The maximum width of the output video in pixels. If you specify @auto@,
-- Elastic Transcoder uses 1920 (Full HD) as the default value. If you
-- specify a numeric value, enter an even integer between 128 and 4096.
--
-- 'maxFrameRate', 'videoParameters_maxFrameRate' - If you specify @auto@ for @FrameRate@, Elastic Transcoder uses the frame
-- rate of the input video for the frame rate of the output video. Specify
-- the maximum frame rate that you want Elastic Transcoder to use when the
-- frame rate of the input video is greater than the desired maximum frame
-- rate of the output video. Valid values include: @10@, @15@, @23.97@,
-- @24@, @25@, @29.97@, @30@, @60@.
--
-- 'frameRate', 'videoParameters_frameRate' - The frames per second for the video stream in the output file. Valid
-- values include:
--
-- @auto@, @10@, @15@, @23.97@, @24@, @25@, @29.97@, @30@, @60@
--
-- If you specify @auto@, Elastic Transcoder uses the detected frame rate
-- of the input source. If you specify a frame rate, we recommend that you
-- perform the following calculation:
--
-- @Frame rate = maximum recommended decoding speed in luma samples\/second \/ (width in pixels * height in pixels)@
--
-- where:
--
-- -   /width in pixels/ and /height in pixels/ represent the Resolution of
--     the output video.
--
-- -   /maximum recommended decoding speed in Luma samples\/second/ is less
--     than or equal to the maximum value listed in the following table,
--     based on the value that you specified for Level.
--
-- The maximum recommended decoding speed in Luma samples\/second for each
-- level is described in the following list (/Level - Decoding speed/):
--
-- -   1 - 380160
--
-- -   1b - 380160
--
-- -   1.1 - 76800
--
-- -   1.2 - 1536000
--
-- -   1.3 - 3041280
--
-- -   2 - 3041280
--
-- -   2.1 - 5068800
--
-- -   2.2 - 5184000
--
-- -   3 - 10368000
--
-- -   3.1 - 27648000
--
-- -   3.2 - 55296000
--
-- -   4 - 62914560
--
-- -   4.1 - 62914560
newVideoParameters ::
  VideoParameters
newVideoParameters =
  VideoParameters'
    { sizingPolicy = Prelude.Nothing,
      watermarks = Prelude.Nothing,
      codecOptions = Prelude.Nothing,
      paddingPolicy = Prelude.Nothing,
      keyframesMaxDist = Prelude.Nothing,
      bitRate = Prelude.Nothing,
      aspectRatio = Prelude.Nothing,
      codec = Prelude.Nothing,
      fixedGOP = Prelude.Nothing,
      resolution = Prelude.Nothing,
      maxHeight = Prelude.Nothing,
      displayAspectRatio = Prelude.Nothing,
      maxWidth = Prelude.Nothing,
      maxFrameRate = Prelude.Nothing,
      frameRate = Prelude.Nothing
    }

-- | Specify one of the following values to control scaling of the output
-- video:
--
-- -   @Fit@: Elastic Transcoder scales the output video so it matches the
--     value that you specified in either @MaxWidth@ or @MaxHeight@ without
--     exceeding the other value.
--
-- -   @Fill@: Elastic Transcoder scales the output video so it matches the
--     value that you specified in either @MaxWidth@ or @MaxHeight@ and
--     matches or exceeds the other value. Elastic Transcoder centers the
--     output video and then crops it in the dimension (if any) that
--     exceeds the maximum value.
--
-- -   @Stretch@: Elastic Transcoder stretches the output video to match
--     the values that you specified for @MaxWidth@ and @MaxHeight@. If the
--     relative proportions of the input video and the output video are
--     different, the output video will be distorted.
--
-- -   @Keep@: Elastic Transcoder does not scale the output video. If
--     either dimension of the input video exceeds the values that you
--     specified for @MaxWidth@ and @MaxHeight@, Elastic Transcoder crops
--     the output video.
--
-- -   @ShrinkToFit@: Elastic Transcoder scales the output video down so
--     that its dimensions match the values that you specified for at least
--     one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If
--     you specify this option, Elastic Transcoder does not scale the video
--     up.
--
-- -   @ShrinkToFill@: Elastic Transcoder scales the output video down so
--     that its dimensions match the values that you specified for at least
--     one of @MaxWidth@ and @MaxHeight@ without dropping below either
--     value. If you specify this option, Elastic Transcoder does not scale
--     the video up.
videoParameters_sizingPolicy :: Lens.Lens' VideoParameters (Prelude.Maybe Prelude.Text)
videoParameters_sizingPolicy = Lens.lens (\VideoParameters' {sizingPolicy} -> sizingPolicy) (\s@VideoParameters' {} a -> s {sizingPolicy = a} :: VideoParameters)

-- | Settings for the size, location, and opacity of graphics that you want
-- Elastic Transcoder to overlay over videos that are transcoded using this
-- preset. You can specify settings for up to four watermarks. Watermarks
-- appear in the specified size and location, and with the specified
-- opacity for the duration of the transcoded video.
--
-- Watermarks can be in .png or .jpg format. If you want to display a
-- watermark that is not rectangular, use the .png format, which supports
-- transparency.
--
-- When you create a job that uses this preset, you specify the .png or
-- .jpg graphics that you want Elastic Transcoder to include in the
-- transcoded videos. You can specify fewer graphics in the job than you
-- specify watermark settings in the preset, which allows you to use the
-- same preset for up to four watermarks that have different dimensions.
videoParameters_watermarks :: Lens.Lens' VideoParameters (Prelude.Maybe [PresetWatermark])
videoParameters_watermarks = Lens.lens (\VideoParameters' {watermarks} -> watermarks) (\s@VideoParameters' {} a -> s {watermarks = a} :: VideoParameters) Prelude.. Lens.mapping Lens.coerced

-- | __Profile (H.264\/VP8\/VP9 Only)__
--
-- The H.264 profile that you want to use for the output file. Elastic
-- Transcoder supports the following profiles:
--
-- -   @baseline@: The profile most commonly used for videoconferencing and
--     for mobile applications.
--
-- -   @main@: The profile used for standard-definition digital TV
--     broadcasts.
--
-- -   @high@: The profile used for high-definition digital TV broadcasts
--     and for Blu-ray discs.
--
-- __Level (H.264 Only)__
--
-- The H.264 level that you want to use for the output file. Elastic
-- Transcoder supports the following levels:
--
-- @1@, @1b@, @1.1@, @1.2@, @1.3@, @2@, @2.1@, @2.2@, @3@, @3.1@, @3.2@,
-- @4@, @4.1@
--
-- __MaxReferenceFrames (H.264 Only)__
--
-- Applicable only when the value of Video:Codec is H.264. The maximum
-- number of previously decoded frames to use as a reference for decoding
-- future frames. Valid values are integers 0 through 16, but we recommend
-- that you not use a value greater than the following:
--
-- @Min(Floor(Maximum decoded picture buffer in macroblocks * 256 \/ (Width in pixels * Height in pixels)), 16)@
--
-- where /Width in pixels/ and /Height in pixels/ represent either MaxWidth
-- and MaxHeight, or Resolution. /Maximum decoded picture buffer in
-- macroblocks/ depends on the value of the @Level@ object. See the list
-- below. (A macroblock is a block of pixels measuring 16x16.)
--
-- -   1 - 396
--
-- -   1b - 396
--
-- -   1.1 - 900
--
-- -   1.2 - 2376
--
-- -   1.3 - 2376
--
-- -   2 - 2376
--
-- -   2.1 - 4752
--
-- -   2.2 - 8100
--
-- -   3 - 8100
--
-- -   3.1 - 18000
--
-- -   3.2 - 20480
--
-- -   4 - 32768
--
-- -   4.1 - 32768
--
-- __MaxBitRate (Optional, H.264\/MPEG2\/VP8\/VP9 only)__
--
-- The maximum number of bits per second in a video buffer; the size of the
-- buffer is specified by @BufferSize@. Specify a value between 16 and
-- 62,500. You can reduce the bandwidth required to stream a video by
-- reducing the maximum bit rate, but this also reduces the quality of the
-- video.
--
-- __BufferSize (Optional, H.264\/MPEG2\/VP8\/VP9 only)__
--
-- The maximum number of bits in any x seconds of the output video. This
-- window is commonly 10 seconds, the standard segment duration when
-- you\'re using FMP4 or MPEG-TS for the container type of the output
-- video. Specify an integer greater than 0. If you specify @MaxBitRate@
-- and omit @BufferSize@, Elastic Transcoder sets @BufferSize@ to 10 times
-- the value of @MaxBitRate@.
--
-- __InterlacedMode (Optional, H.264\/MPEG2 Only)__
--
-- The interlace mode for the output video.
--
-- Interlaced video is used to double the perceived frame rate for a video
-- by interlacing two fields (one field on every other line, the other
-- field on the other lines) so that the human eye registers multiple
-- pictures per frame. Interlacing reduces the bandwidth required for
-- transmitting a video, but can result in blurred images and flickering.
--
-- Valid values include @Progressive@ (no interlacing, top to bottom),
-- @TopFirst@ (top field first), @BottomFirst@ (bottom field first), and
-- @Auto@.
--
-- If @InterlaceMode@ is not specified, Elastic Transcoder uses
-- @Progressive@ for the output. If @Auto@ is specified, Elastic Transcoder
-- interlaces the output.
--
-- __ColorSpaceConversionMode (Optional, H.264\/MPEG2 Only)__
--
-- The color space conversion Elastic Transcoder applies to the output
-- video. Color spaces are the algorithms used by the computer to store
-- information about how to render color. @Bt.601@ is the standard for
-- standard definition video, while @Bt.709@ is the standard for high
-- definition video.
--
-- Valid values include @None@, @Bt709toBt601@, @Bt601toBt709@, and @Auto@.
--
-- If you chose @Auto@ for @ColorSpaceConversionMode@ and your output is
-- interlaced, your frame rate is one of @23.97@, @24@, @25@, @29.97@,
-- @50@, or @60@, your @SegmentDuration@ is null, and you are using one of
-- the resolution changes from the list below, Elastic Transcoder applies
-- the following color space conversions:
--
-- -   /Standard to HD, 720x480 to 1920x1080/ - Elastic Transcoder applies
--     @Bt601ToBt709@
--
-- -   /Standard to HD, 720x576 to 1920x1080/ - Elastic Transcoder applies
--     @Bt601ToBt709@
--
-- -   /HD to Standard, 1920x1080 to 720x480/ - Elastic Transcoder applies
--     @Bt709ToBt601@
--
-- -   /HD to Standard, 1920x1080 to 720x576/ - Elastic Transcoder applies
--     @Bt709ToBt601@
--
-- Elastic Transcoder may change the behavior of the
-- @ColorspaceConversionMode@ @Auto@ mode in the future. All outputs in a
-- playlist must use the same @ColorSpaceConversionMode@.
--
-- If you do not specify a @ColorSpaceConversionMode@, Elastic Transcoder
-- does not change the color space of a file. If you are unsure what
-- @ColorSpaceConversionMode@ was applied to your output file, you can
-- check the @AppliedColorSpaceConversion@ parameter included in your job
-- response. If your job does not have an @AppliedColorSpaceConversion@ in
-- its response, no @ColorSpaceConversionMode@ was applied.
--
-- __ChromaSubsampling__
--
-- The sampling pattern for the chroma (color) channels of the output
-- video. Valid values include @yuv420p@ and @yuv422p@.
--
-- @yuv420p@ samples the chroma information of every other horizontal and
-- every other vertical line, @yuv422p@ samples the color information of
-- every horizontal line and every other vertical line.
--
-- __LoopCount (Gif Only)__
--
-- The number of times you want the output gif to loop. Valid values
-- include @Infinite@ and integers between @0@ and @100@, inclusive.
videoParameters_codecOptions :: Lens.Lens' VideoParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
videoParameters_codecOptions = Lens.lens (\VideoParameters' {codecOptions} -> codecOptions) (\s@VideoParameters' {} a -> s {codecOptions = a} :: VideoParameters) Prelude.. Lens.mapping Lens.coerced

-- | When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add black
-- bars to the top and bottom and\/or left and right sides of the output
-- video to make the total size of the output video match the values that
-- you specified for @MaxWidth@ and @MaxHeight@.
videoParameters_paddingPolicy :: Lens.Lens' VideoParameters (Prelude.Maybe Prelude.Text)
videoParameters_paddingPolicy = Lens.lens (\VideoParameters' {paddingPolicy} -> paddingPolicy) (\s@VideoParameters' {} a -> s {paddingPolicy = a} :: VideoParameters)

-- | Applicable only when the value of Video:Codec is one of @H.264@,
-- @MPEG2@, or @VP8@.
--
-- The maximum number of frames between key frames. Key frames are fully
-- encoded frames; the frames between key frames are encoded based, in
-- part, on the content of the key frames. The value is an integer
-- formatted as a string; valid values are between 1 (every frame is a key
-- frame) and 100000, inclusive. A higher value results in higher
-- compression but may also discernibly decrease video quality.
--
-- For @Smooth@ outputs, the @FrameRate@ must have a constant ratio to the
-- @KeyframesMaxDist@. This allows @Smooth@ playlists to switch between
-- different quality levels while the file is being played.
--
-- For example, an input file can have a @FrameRate@ of 30 with a
-- @KeyframesMaxDist@ of 90. The output file then needs to have a ratio of
-- 1:3. Valid outputs would have @FrameRate@ of 30, 25, and 10, and
-- @KeyframesMaxDist@ of 90, 75, and 30, respectively.
--
-- Alternately, this can be achieved by setting @FrameRate@ to auto and
-- having the same values for @MaxFrameRate@ and @KeyframesMaxDist@.
videoParameters_keyframesMaxDist :: Lens.Lens' VideoParameters (Prelude.Maybe Prelude.Text)
videoParameters_keyframesMaxDist = Lens.lens (\VideoParameters' {keyframesMaxDist} -> keyframesMaxDist) (\s@VideoParameters' {} a -> s {keyframesMaxDist = a} :: VideoParameters)

-- | The bit rate of the video stream in the output file, in
-- kilobits\/second. Valid values depend on the values of @Level@ and
-- @Profile@. If you specify @auto@, Elastic Transcoder uses the detected
-- bit rate of the input source. If you specify a value other than @auto@,
-- we recommend that you specify a value less than or equal to the maximum
-- H.264-compliant value listed for your level and profile:
--
-- /Level - Maximum video bit rate in kilobits\/second (baseline and main
-- Profile) : maximum video bit rate in kilobits\/second (high Profile)/
--
-- -   1 - 64 : 80
--
-- -   1b - 128 : 160
--
-- -   1.1 - 192 : 240
--
-- -   1.2 - 384 : 480
--
-- -   1.3 - 768 : 960
--
-- -   2 - 2000 : 2500
--
-- -   3 - 10000 : 12500
--
-- -   3.1 - 14000 : 17500
--
-- -   3.2 - 20000 : 25000
--
-- -   4 - 20000 : 25000
--
-- -   4.1 - 50000 : 62500
videoParameters_bitRate :: Lens.Lens' VideoParameters (Prelude.Maybe Prelude.Text)
videoParameters_bitRate = Lens.lens (\VideoParameters' {bitRate} -> bitRate) (\s@VideoParameters' {} a -> s {bitRate = a} :: VideoParameters)

-- | To better control resolution and aspect ratio of output videos, we
-- recommend that you use the values @MaxWidth@, @MaxHeight@,
-- @SizingPolicy@, @PaddingPolicy@, and @DisplayAspectRatio@ instead of
-- @Resolution@ and @AspectRatio@. The two groups of settings are mutually
-- exclusive. Do not use them together.
--
-- The display aspect ratio of the video in the output file. Valid values
-- include:
--
-- @auto@, @1:1@, @4:3@, @3:2@, @16:9@
--
-- If you specify @auto@, Elastic Transcoder tries to preserve the aspect
-- ratio of the input file.
--
-- If you specify an aspect ratio for the output file that differs from
-- aspect ratio of the input file, Elastic Transcoder adds pillarboxing
-- (black bars on the sides) or letterboxing (black bars on the top and
-- bottom) to maintain the aspect ratio of the active region of the video.
videoParameters_aspectRatio :: Lens.Lens' VideoParameters (Prelude.Maybe Prelude.Text)
videoParameters_aspectRatio = Lens.lens (\VideoParameters' {aspectRatio} -> aspectRatio) (\s@VideoParameters' {} a -> s {aspectRatio = a} :: VideoParameters)

-- | The video codec for the output file. Valid values include @gif@,
-- @H.264@, @mpeg2@, @vp8@, and @vp9@. You can only specify @vp8@ and @vp9@
-- when the container type is @webm@, @gif@ when the container type is
-- @gif@, and @mpeg2@ when the container type is @mpg@.
videoParameters_codec :: Lens.Lens' VideoParameters (Prelude.Maybe Prelude.Text)
videoParameters_codec = Lens.lens (\VideoParameters' {codec} -> codec) (\s@VideoParameters' {} a -> s {codec = a} :: VideoParameters)

-- | Applicable only when the value of Video:Codec is one of @H.264@,
-- @MPEG2@, or @VP8@.
--
-- Whether to use a fixed value for @FixedGOP@. Valid values are @true@ and
-- @false@:
--
-- -   @true@: Elastic Transcoder uses the value of @KeyframesMaxDist@ for
--     the distance between key frames (the number of frames in a group of
--     pictures, or GOP).
--
-- -   @false@: The distance between key frames can vary.
--
-- @FixedGOP@ must be set to @true@ for @fmp4@ containers.
videoParameters_fixedGOP :: Lens.Lens' VideoParameters (Prelude.Maybe Prelude.Text)
videoParameters_fixedGOP = Lens.lens (\VideoParameters' {fixedGOP} -> fixedGOP) (\s@VideoParameters' {} a -> s {fixedGOP = a} :: VideoParameters)

-- | To better control resolution and aspect ratio of output videos, we
-- recommend that you use the values @MaxWidth@, @MaxHeight@,
-- @SizingPolicy@, @PaddingPolicy@, and @DisplayAspectRatio@ instead of
-- @Resolution@ and @AspectRatio@. The two groups of settings are mutually
-- exclusive. Do not use them together.
--
-- The width and height of the video in the output file, in pixels. Valid
-- values are @auto@ and /width/ x /height/:
--
-- -   @auto@: Elastic Transcoder attempts to preserve the width and height
--     of the input file, subject to the following rules.
--
-- -   @ width x height @: The width and height of the output video in
--     pixels.
--
-- Note the following about specifying the width and height:
--
-- -   The width must be an even integer between 128 and 4096, inclusive.
--
-- -   The height must be an even integer between 96 and 3072, inclusive.
--
-- -   If you specify a resolution that is less than the resolution of the
--     input file, Elastic Transcoder rescales the output file to the lower
--     resolution.
--
-- -   If you specify a resolution that is greater than the resolution of
--     the input file, Elastic Transcoder rescales the output to the higher
--     resolution.
--
-- -   We recommend that you specify a resolution for which the product of
--     width and height is less than or equal to the applicable value in
--     the following list (/List - Max width x height value/):
--
--     -   1 - 25344
--
--     -   1b - 25344
--
--     -   1.1 - 101376
--
--     -   1.2 - 101376
--
--     -   1.3 - 101376
--
--     -   2 - 101376
--
--     -   2.1 - 202752
--
--     -   2.2 - 404720
--
--     -   3 - 404720
--
--     -   3.1 - 921600
--
--     -   3.2 - 1310720
--
--     -   4 - 2097152
--
--     -   4.1 - 2097152
videoParameters_resolution :: Lens.Lens' VideoParameters (Prelude.Maybe Prelude.Text)
videoParameters_resolution = Lens.lens (\VideoParameters' {resolution} -> resolution) (\s@VideoParameters' {} a -> s {resolution = a} :: VideoParameters)

-- | The maximum height of the output video in pixels. If you specify @auto@,
-- Elastic Transcoder uses 1080 (Full HD) as the default value. If you
-- specify a numeric value, enter an even integer between 96 and 3072.
videoParameters_maxHeight :: Lens.Lens' VideoParameters (Prelude.Maybe Prelude.Text)
videoParameters_maxHeight = Lens.lens (\VideoParameters' {maxHeight} -> maxHeight) (\s@VideoParameters' {} a -> s {maxHeight = a} :: VideoParameters)

-- | The value that Elastic Transcoder adds to the metadata in the output
-- file.
videoParameters_displayAspectRatio :: Lens.Lens' VideoParameters (Prelude.Maybe Prelude.Text)
videoParameters_displayAspectRatio = Lens.lens (\VideoParameters' {displayAspectRatio} -> displayAspectRatio) (\s@VideoParameters' {} a -> s {displayAspectRatio = a} :: VideoParameters)

-- | The maximum width of the output video in pixels. If you specify @auto@,
-- Elastic Transcoder uses 1920 (Full HD) as the default value. If you
-- specify a numeric value, enter an even integer between 128 and 4096.
videoParameters_maxWidth :: Lens.Lens' VideoParameters (Prelude.Maybe Prelude.Text)
videoParameters_maxWidth = Lens.lens (\VideoParameters' {maxWidth} -> maxWidth) (\s@VideoParameters' {} a -> s {maxWidth = a} :: VideoParameters)

-- | If you specify @auto@ for @FrameRate@, Elastic Transcoder uses the frame
-- rate of the input video for the frame rate of the output video. Specify
-- the maximum frame rate that you want Elastic Transcoder to use when the
-- frame rate of the input video is greater than the desired maximum frame
-- rate of the output video. Valid values include: @10@, @15@, @23.97@,
-- @24@, @25@, @29.97@, @30@, @60@.
videoParameters_maxFrameRate :: Lens.Lens' VideoParameters (Prelude.Maybe Prelude.Text)
videoParameters_maxFrameRate = Lens.lens (\VideoParameters' {maxFrameRate} -> maxFrameRate) (\s@VideoParameters' {} a -> s {maxFrameRate = a} :: VideoParameters)

-- | The frames per second for the video stream in the output file. Valid
-- values include:
--
-- @auto@, @10@, @15@, @23.97@, @24@, @25@, @29.97@, @30@, @60@
--
-- If you specify @auto@, Elastic Transcoder uses the detected frame rate
-- of the input source. If you specify a frame rate, we recommend that you
-- perform the following calculation:
--
-- @Frame rate = maximum recommended decoding speed in luma samples\/second \/ (width in pixels * height in pixels)@
--
-- where:
--
-- -   /width in pixels/ and /height in pixels/ represent the Resolution of
--     the output video.
--
-- -   /maximum recommended decoding speed in Luma samples\/second/ is less
--     than or equal to the maximum value listed in the following table,
--     based on the value that you specified for Level.
--
-- The maximum recommended decoding speed in Luma samples\/second for each
-- level is described in the following list (/Level - Decoding speed/):
--
-- -   1 - 380160
--
-- -   1b - 380160
--
-- -   1.1 - 76800
--
-- -   1.2 - 1536000
--
-- -   1.3 - 3041280
--
-- -   2 - 3041280
--
-- -   2.1 - 5068800
--
-- -   2.2 - 5184000
--
-- -   3 - 10368000
--
-- -   3.1 - 27648000
--
-- -   3.2 - 55296000
--
-- -   4 - 62914560
--
-- -   4.1 - 62914560
videoParameters_frameRate :: Lens.Lens' VideoParameters (Prelude.Maybe Prelude.Text)
videoParameters_frameRate = Lens.lens (\VideoParameters' {frameRate} -> frameRate) (\s@VideoParameters' {} a -> s {frameRate = a} :: VideoParameters)

instance Data.FromJSON VideoParameters where
  parseJSON =
    Data.withObject
      "VideoParameters"
      ( \x ->
          VideoParameters'
            Prelude.<$> (x Data..:? "SizingPolicy")
            Prelude.<*> (x Data..:? "Watermarks" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CodecOptions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PaddingPolicy")
            Prelude.<*> (x Data..:? "KeyframesMaxDist")
            Prelude.<*> (x Data..:? "BitRate")
            Prelude.<*> (x Data..:? "AspectRatio")
            Prelude.<*> (x Data..:? "Codec")
            Prelude.<*> (x Data..:? "FixedGOP")
            Prelude.<*> (x Data..:? "Resolution")
            Prelude.<*> (x Data..:? "MaxHeight")
            Prelude.<*> (x Data..:? "DisplayAspectRatio")
            Prelude.<*> (x Data..:? "MaxWidth")
            Prelude.<*> (x Data..:? "MaxFrameRate")
            Prelude.<*> (x Data..:? "FrameRate")
      )

instance Prelude.Hashable VideoParameters where
  hashWithSalt _salt VideoParameters' {..} =
    _salt `Prelude.hashWithSalt` sizingPolicy
      `Prelude.hashWithSalt` watermarks
      `Prelude.hashWithSalt` codecOptions
      `Prelude.hashWithSalt` paddingPolicy
      `Prelude.hashWithSalt` keyframesMaxDist
      `Prelude.hashWithSalt` bitRate
      `Prelude.hashWithSalt` aspectRatio
      `Prelude.hashWithSalt` codec
      `Prelude.hashWithSalt` fixedGOP
      `Prelude.hashWithSalt` resolution
      `Prelude.hashWithSalt` maxHeight
      `Prelude.hashWithSalt` displayAspectRatio
      `Prelude.hashWithSalt` maxWidth
      `Prelude.hashWithSalt` maxFrameRate
      `Prelude.hashWithSalt` frameRate

instance Prelude.NFData VideoParameters where
  rnf VideoParameters' {..} =
    Prelude.rnf sizingPolicy
      `Prelude.seq` Prelude.rnf watermarks
      `Prelude.seq` Prelude.rnf codecOptions
      `Prelude.seq` Prelude.rnf paddingPolicy
      `Prelude.seq` Prelude.rnf keyframesMaxDist
      `Prelude.seq` Prelude.rnf bitRate
      `Prelude.seq` Prelude.rnf aspectRatio
      `Prelude.seq` Prelude.rnf codec
      `Prelude.seq` Prelude.rnf fixedGOP
      `Prelude.seq` Prelude.rnf resolution
      `Prelude.seq` Prelude.rnf maxHeight
      `Prelude.seq` Prelude.rnf displayAspectRatio
      `Prelude.seq` Prelude.rnf maxWidth
      `Prelude.seq` Prelude.rnf maxFrameRate
      `Prelude.seq` Prelude.rnf frameRate

instance Data.ToJSON VideoParameters where
  toJSON VideoParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SizingPolicy" Data..=) Prelude.<$> sizingPolicy,
            ("Watermarks" Data..=) Prelude.<$> watermarks,
            ("CodecOptions" Data..=) Prelude.<$> codecOptions,
            ("PaddingPolicy" Data..=) Prelude.<$> paddingPolicy,
            ("KeyframesMaxDist" Data..=)
              Prelude.<$> keyframesMaxDist,
            ("BitRate" Data..=) Prelude.<$> bitRate,
            ("AspectRatio" Data..=) Prelude.<$> aspectRatio,
            ("Codec" Data..=) Prelude.<$> codec,
            ("FixedGOP" Data..=) Prelude.<$> fixedGOP,
            ("Resolution" Data..=) Prelude.<$> resolution,
            ("MaxHeight" Data..=) Prelude.<$> maxHeight,
            ("DisplayAspectRatio" Data..=)
              Prelude.<$> displayAspectRatio,
            ("MaxWidth" Data..=) Prelude.<$> maxWidth,
            ("MaxFrameRate" Data..=) Prelude.<$> maxFrameRate,
            ("FrameRate" Data..=) Prelude.<$> frameRate
          ]
      )
