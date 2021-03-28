{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.VideoParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.VideoParameters
  ( VideoParameters (..)
  -- * Smart constructor
  , mkVideoParameters
  -- * Lenses
  , vpAspectRatio
  , vpBitRate
  , vpCodec
  , vpCodecOptions
  , vpDisplayAspectRatio
  , vpFixedGOP
  , vpFrameRate
  , vpKeyframesMaxDist
  , vpMaxFrameRate
  , vpMaxHeight
  , vpMaxWidth
  , vpPaddingPolicy
  , vpResolution
  , vpSizingPolicy
  , vpWatermarks
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.AspectRatio as Types
import qualified Network.AWS.ElasticTranscoder.Types.CodecOption as Types
import qualified Network.AWS.ElasticTranscoder.Types.DigitsOrAuto as Types
import qualified Network.AWS.ElasticTranscoder.Types.FixedGOP as Types
import qualified Network.AWS.ElasticTranscoder.Types.FrameRate as Types
import qualified Network.AWS.ElasticTranscoder.Types.KeyframesMaxDist as Types
import qualified Network.AWS.ElasticTranscoder.Types.MaxFrameRate as Types
import qualified Network.AWS.ElasticTranscoder.Types.PaddingPolicy as Types
import qualified Network.AWS.ElasticTranscoder.Types.PresetWatermark as Types
import qualified Network.AWS.ElasticTranscoder.Types.Resolution as Types
import qualified Network.AWS.ElasticTranscoder.Types.SizingPolicy as Types
import qualified Network.AWS.ElasticTranscoder.Types.VideoBitRate as Types
import qualified Network.AWS.ElasticTranscoder.Types.VideoCodec as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @VideoParameters@ structure.
--
-- /See:/ 'mkVideoParameters' smart constructor.
data VideoParameters = VideoParameters'
  { aspectRatio :: Core.Maybe Types.AspectRatio
    -- ^ /Important:/ To better control resolution and aspect ratio of output videos, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , @PaddingPolicy@ , and @DisplayAspectRatio@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together.
--
-- The display aspect ratio of the video in the output file. Valid values include:
-- @auto@ , @1:1@ , @4:3@ , @3:2@ , @16:9@ 
-- If you specify @auto@ , Elastic Transcoder tries to preserve the aspect ratio of the input file.
-- If you specify an aspect ratio for the output file that differs from aspect ratio of the input file, Elastic Transcoder adds pillarboxing (black bars on the sides) or letterboxing (black bars on the top and bottom) to maintain the aspect ratio of the active region of the video.
  , bitRate :: Core.Maybe Types.VideoBitRate
    -- ^ The bit rate of the video stream in the output file, in kilobits/second. Valid values depend on the values of @Level@ and @Profile@ . If you specify @auto@ , Elastic Transcoder uses the detected bit rate of the input source. If you specify a value other than @auto@ , we recommend that you specify a value less than or equal to the maximum H.264-compliant value listed for your level and profile:
--
-- /Level - Maximum video bit rate in kilobits\/second (baseline and main Profile) : maximum video bit rate in kilobits\/second (high Profile)/ 
--
--     * 1 - 64 : 80
--
--
--     * 1b - 128 : 160
--
--
--     * 1.1 - 192 : 240
--
--
--     * 1.2 - 384 : 480
--
--
--     * 1.3 - 768 : 960
--
--
--     * 2 - 2000 : 2500
--
--
--     * 3 - 10000 : 12500
--
--
--     * 3.1 - 14000 : 17500
--
--
--     * 3.2 - 20000 : 25000
--
--
--     * 4 - 20000 : 25000
--
--
--     * 4.1 - 50000 : 62500
--
--
  , codec :: Core.Maybe Types.VideoCodec
    -- ^ The video codec for the output file. Valid values include @gif@ , @H.264@ , @mpeg2@ , @vp8@ , and @vp9@ . You can only specify @vp8@ and @vp9@ when the container type is @webm@ , @gif@ when the container type is @gif@ , and @mpeg2@ when the container type is @mpg@ .
  , codecOptions :: Core.Maybe (Core.HashMap Types.CodecOption Types.CodecOption)
    -- ^ __Profile (H.264/VP8/VP9 Only)__ 
--
-- The H.264 profile that you want to use for the output file. Elastic Transcoder supports the following profiles:
--
--     * @baseline@ : The profile most commonly used for videoconferencing and for mobile applications.
--
--
--     * @main@ : The profile used for standard-definition digital TV broadcasts.
--
--
--     * @high@ : The profile used for high-definition digital TV broadcasts and for Blu-ray discs.
--
--
-- __Level (H.264 Only)__ 
-- The H.264 level that you want to use for the output file. Elastic Transcoder supports the following levels:
-- @1@ , @1b@ , @1.1@ , @1.2@ , @1.3@ , @2@ , @2.1@ , @2.2@ , @3@ , @3.1@ , @3.2@ , @4@ , @4.1@ 
-- __MaxReferenceFrames (H.264 Only)__ 
-- Applicable only when the value of Video:Codec is H.264. The maximum number of previously decoded frames to use as a reference for decoding future frames. Valid values are integers 0 through 16, but we recommend that you not use a value greater than the following:
-- @Min(Floor(Maximum decoded picture buffer in macroblocks * 256 / (Width in pixels * Height in pixels)), 16)@ 
-- where /Width in pixels/ and /Height in pixels/ represent either MaxWidth and MaxHeight, or Resolution. /Maximum decoded picture buffer in macroblocks/ depends on the value of the @Level@ object. See the list below. (A macroblock is a block of pixels measuring 16x16.) 
--
--     * 1 - 396
--
--
--     * 1b - 396
--
--
--     * 1.1 - 900
--
--
--     * 1.2 - 2376
--
--
--     * 1.3 - 2376
--
--
--     * 2 - 2376
--
--
--     * 2.1 - 4752
--
--
--     * 2.2 - 8100
--
--
--     * 3 - 8100
--
--
--     * 3.1 - 18000
--
--
--     * 3.2 - 20480
--
--
--     * 4 - 32768
--
--
--     * 4.1 - 32768
--
--
-- __MaxBitRate (Optional, H.264/MPEG2/VP8/VP9 only)__ 
-- The maximum number of bits per second in a video buffer; the size of the buffer is specified by @BufferSize@ . Specify a value between 16 and 62,500. You can reduce the bandwidth required to stream a video by reducing the maximum bit rate, but this also reduces the quality of the video.
-- __BufferSize (Optional, H.264/MPEG2/VP8/VP9 only)__ 
-- The maximum number of bits in any x seconds of the output video. This window is commonly 10 seconds, the standard segment duration when you're using FMP4 or MPEG-TS for the container type of the output video. Specify an integer greater than 0. If you specify @MaxBitRate@ and omit @BufferSize@ , Elastic Transcoder sets @BufferSize@ to 10 times the value of @MaxBitRate@ .
-- __InterlacedMode (Optional, H.264/MPEG2 Only)__ 
-- The interlace mode for the output video.
-- Interlaced video is used to double the perceived frame rate for a video by interlacing two fields (one field on every other line, the other field on the other lines) so that the human eye registers multiple pictures per frame. Interlacing reduces the bandwidth required for transmitting a video, but can result in blurred images and flickering.
-- Valid values include @Progressive@ (no interlacing, top to bottom), @TopFirst@ (top field first), @BottomFirst@ (bottom field first), and @Auto@ .
-- If @InterlaceMode@ is not specified, Elastic Transcoder uses @Progressive@ for the output. If @Auto@ is specified, Elastic Transcoder interlaces the output.
-- __ColorSpaceConversionMode (Optional, H.264/MPEG2 Only)__ 
-- The color space conversion Elastic Transcoder applies to the output video. Color spaces are the algorithms used by the computer to store information about how to render color. @Bt.601@ is the standard for standard definition video, while @Bt.709@ is the standard for high definition video.
-- Valid values include @None@ , @Bt709toBt601@ , @Bt601toBt709@ , and @Auto@ .
-- If you chose @Auto@ for @ColorSpaceConversionMode@ and your output is interlaced, your frame rate is one of @23.97@ , @24@ , @25@ , @29.97@ , @50@ , or @60@ , your @SegmentDuration@ is null, and you are using one of the resolution changes from the list below, Elastic Transcoder applies the following color space conversions:
--
--     * /Standard to HD, 720x480 to 1920x1080/ - Elastic Transcoder applies @Bt601ToBt709@ 
--
--
--     * /Standard to HD, 720x576 to 1920x1080/ - Elastic Transcoder applies @Bt601ToBt709@ 
--
--
--     * /HD to Standard, 1920x1080 to 720x480/ - Elastic Transcoder applies @Bt709ToBt601@ 
--
--
--     * /HD to Standard, 1920x1080 to 720x576/ - Elastic Transcoder applies @Bt709ToBt601@ 
--
--
-- If you do not specify a @ColorSpaceConversionMode@ , Elastic Transcoder does not change the color space of a file. If you are unsure what @ColorSpaceConversionMode@ was applied to your output file, you can check the @AppliedColorSpaceConversion@ parameter included in your job response. If your job does not have an @AppliedColorSpaceConversion@ in its response, no @ColorSpaceConversionMode@ was applied.
-- __ChromaSubsampling__ 
-- The sampling pattern for the chroma (color) channels of the output video. Valid values include @yuv420p@ and @yuv422p@ .
-- @yuv420p@ samples the chroma information of every other horizontal and every other vertical line, @yuv422p@ samples the color information of every horizontal line and every other vertical line.
-- __LoopCount (Gif Only)__ 
-- The number of times you want the output gif to loop. Valid values include @Infinite@ and integers between @0@ and @100@ , inclusive.
  , displayAspectRatio :: Core.Maybe Types.AspectRatio
    -- ^ The value that Elastic Transcoder adds to the metadata in the output file.
  , fixedGOP :: Core.Maybe Types.FixedGOP
    -- ^ Applicable only when the value of Video:Codec is one of @H.264@ , @MPEG2@ , or @VP8@ .
--
-- Whether to use a fixed value for @FixedGOP@ . Valid values are @true@ and @false@ :
--
--     * @true@ : Elastic Transcoder uses the value of @KeyframesMaxDist@ for the distance between key frames (the number of frames in a group of pictures, or GOP).
--
--
--     * @false@ : The distance between key frames can vary.
--
--
-- /Important:/ @FixedGOP@ must be set to @true@ for @fmp4@ containers.
  , frameRate :: Core.Maybe Types.FrameRate
    -- ^ The frames per second for the video stream in the output file. Valid values include:
--
-- @auto@ , @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@ 
-- If you specify @auto@ , Elastic Transcoder uses the detected frame rate of the input source. If you specify a frame rate, we recommend that you perform the following calculation:
-- @Frame rate = maximum recommended decoding speed in luma samples/second / (width in pixels * height in pixels)@ 
-- where:
--
--     * /width in pixels/ and /height in pixels/ represent the Resolution of the output video.
--
--
--     * /maximum recommended decoding speed in Luma samples\/second/ is less than or equal to the maximum value listed in the following table, based on the value that you specified for Level.
--
--
-- The maximum recommended decoding speed in Luma samples/second for each level is described in the following list (/Level - Decoding speed/ ):
--
--     * 1 - 380160
--
--
--     * 1b - 380160
--
--
--     * 1.1 - 76800
--
--
--     * 1.2 - 1536000
--
--
--     * 1.3 - 3041280
--
--
--     * 2 - 3041280
--
--
--     * 2.1 - 5068800
--
--
--     * 2.2 - 5184000
--
--
--     * 3 - 10368000
--
--
--     * 3.1 - 27648000
--
--
--     * 3.2 - 55296000
--
--
--     * 4 - 62914560
--
--
--     * 4.1 - 62914560
--
--
  , keyframesMaxDist :: Core.Maybe Types.KeyframesMaxDist
    -- ^ Applicable only when the value of Video:Codec is one of @H.264@ , @MPEG2@ , or @VP8@ .
--
-- The maximum number of frames between key frames. Key frames are fully encoded frames; the frames between key frames are encoded based, in part, on the content of the key frames. The value is an integer formatted as a string; valid values are between 1 (every frame is a key frame) and 100000, inclusive. A higher value results in higher compression but may also discernibly decrease video quality.
-- For @Smooth@ outputs, the @FrameRate@ must have a constant ratio to the @KeyframesMaxDist@ . This allows @Smooth@ playlists to switch between different quality levels while the file is being played.
-- For example, an input file can have a @FrameRate@ of 30 with a @KeyframesMaxDist@ of 90. The output file then needs to have a ratio of 1:3. Valid outputs would have @FrameRate@ of 30, 25, and 10, and @KeyframesMaxDist@ of 90, 75, and 30, respectively.
-- Alternately, this can be achieved by setting @FrameRate@ to auto and having the same values for @MaxFrameRate@ and @KeyframesMaxDist@ .
  , maxFrameRate :: Core.Maybe Types.MaxFrameRate
    -- ^ If you specify @auto@ for @FrameRate@ , Elastic Transcoder uses the frame rate of the input video for the frame rate of the output video. Specify the maximum frame rate that you want Elastic Transcoder to use when the frame rate of the input video is greater than the desired maximum frame rate of the output video. Valid values include: @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@ .
  , maxHeight :: Core.Maybe Types.DigitsOrAuto
    -- ^ The maximum height of the output video in pixels. If you specify @auto@ , Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 96 and 3072.
  , maxWidth :: Core.Maybe Types.DigitsOrAuto
    -- ^ The maximum width of the output video in pixels. If you specify @auto@ , Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 128 and 4096. 
  , paddingPolicy :: Core.Maybe Types.PaddingPolicy
    -- ^ When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add black bars to the top and bottom and/or left and right sides of the output video to make the total size of the output video match the values that you specified for @MaxWidth@ and @MaxHeight@ .
  , resolution :: Core.Maybe Types.Resolution
    -- ^ /Important:/ To better control resolution and aspect ratio of output videos, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , @PaddingPolicy@ , and @DisplayAspectRatio@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together.
--
-- The width and height of the video in the output file, in pixels. Valid values are @auto@ and /width/ x /height/ :
--
--     * @auto@ : Elastic Transcoder attempts to preserve the width and height of the input file, subject to the following rules.
--
--
--     * @/width/ x /height/ @ : The width and height of the output video in pixels.
--
--
-- Note the following about specifying the width and height:
--
--     * The width must be an even integer between 128 and 4096, inclusive.
--
--
--     * The height must be an even integer between 96 and 3072, inclusive.
--
--
--     * If you specify a resolution that is less than the resolution of the input file, Elastic Transcoder rescales the output file to the lower resolution.
--
--
--     * If you specify a resolution that is greater than the resolution of the input file, Elastic Transcoder rescales the output to the higher resolution.
--
--
--     * We recommend that you specify a resolution for which the product of width and height is less than or equal to the applicable value in the following list (/List - Max width x height value/ ):
--
--     * 1 - 25344
--
--
--     * 1b - 25344
--
--
--     * 1.1 - 101376
--
--
--     * 1.2 - 101376
--
--
--     * 1.3 - 101376
--
--
--     * 2 - 101376
--
--
--     * 2.1 - 202752
--
--
--     * 2.2 - 404720
--
--
--     * 3 - 404720
--
--
--     * 3.1 - 921600
--
--
--     * 3.2 - 1310720
--
--
--     * 4 - 2097152
--
--
--     * 4.1 - 2097152
--
--
--
--
  , sizingPolicy :: Core.Maybe Types.SizingPolicy
    -- ^ Specify one of the following values to control scaling of the output video:
--
--
--     * @Fit@ : Elastic Transcoder scales the output video so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.
--
--
--     * @Fill@ : Elastic Transcoder scales the output video so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ and matches or exceeds the other value. Elastic Transcoder centers the output video and then crops it in the dimension (if any) that exceeds the maximum value.
--
--
--     * @Stretch@ : Elastic Transcoder stretches the output video to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the input video and the output video are different, the output video will be distorted.
--
--
--     * @Keep@ : Elastic Transcoder does not scale the output video. If either dimension of the input video exceeds the values that you specified for @MaxWidth@ and @MaxHeight@ , Elastic Transcoder crops the output video.
--
--
--     * @ShrinkToFit@ : Elastic Transcoder scales the output video down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the video up.
--
--
--     * @ShrinkToFill@ : Elastic Transcoder scales the output video down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale the video up.
--
--
  , watermarks :: Core.Maybe [Types.PresetWatermark]
    -- ^ Settings for the size, location, and opacity of graphics that you want Elastic Transcoder to overlay over videos that are transcoded using this preset. You can specify settings for up to four watermarks. Watermarks appear in the specified size and location, and with the specified opacity for the duration of the transcoded video.
--
-- Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.
-- When you create a job that uses this preset, you specify the .png or .jpg graphics that you want Elastic Transcoder to include in the transcoded videos. You can specify fewer graphics in the job than you specify watermark settings in the preset, which allows you to use the same preset for up to four watermarks that have different dimensions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VideoParameters' value with any optional fields omitted.
mkVideoParameters
    :: VideoParameters
mkVideoParameters
  = VideoParameters'{aspectRatio = Core.Nothing,
                     bitRate = Core.Nothing, codec = Core.Nothing,
                     codecOptions = Core.Nothing, displayAspectRatio = Core.Nothing,
                     fixedGOP = Core.Nothing, frameRate = Core.Nothing,
                     keyframesMaxDist = Core.Nothing, maxFrameRate = Core.Nothing,
                     maxHeight = Core.Nothing, maxWidth = Core.Nothing,
                     paddingPolicy = Core.Nothing, resolution = Core.Nothing,
                     sizingPolicy = Core.Nothing, watermarks = Core.Nothing}

-- | /Important:/ To better control resolution and aspect ratio of output videos, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , @PaddingPolicy@ , and @DisplayAspectRatio@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together.
--
-- The display aspect ratio of the video in the output file. Valid values include:
-- @auto@ , @1:1@ , @4:3@ , @3:2@ , @16:9@ 
-- If you specify @auto@ , Elastic Transcoder tries to preserve the aspect ratio of the input file.
-- If you specify an aspect ratio for the output file that differs from aspect ratio of the input file, Elastic Transcoder adds pillarboxing (black bars on the sides) or letterboxing (black bars on the top and bottom) to maintain the aspect ratio of the active region of the video.
--
-- /Note:/ Consider using 'aspectRatio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpAspectRatio :: Lens.Lens' VideoParameters (Core.Maybe Types.AspectRatio)
vpAspectRatio = Lens.field @"aspectRatio"
{-# INLINEABLE vpAspectRatio #-}
{-# DEPRECATED aspectRatio "Use generic-lens or generic-optics with 'aspectRatio' instead"  #-}

-- | The bit rate of the video stream in the output file, in kilobits/second. Valid values depend on the values of @Level@ and @Profile@ . If you specify @auto@ , Elastic Transcoder uses the detected bit rate of the input source. If you specify a value other than @auto@ , we recommend that you specify a value less than or equal to the maximum H.264-compliant value listed for your level and profile:
--
-- /Level - Maximum video bit rate in kilobits\/second (baseline and main Profile) : maximum video bit rate in kilobits\/second (high Profile)/ 
--
--     * 1 - 64 : 80
--
--
--     * 1b - 128 : 160
--
--
--     * 1.1 - 192 : 240
--
--
--     * 1.2 - 384 : 480
--
--
--     * 1.3 - 768 : 960
--
--
--     * 2 - 2000 : 2500
--
--
--     * 3 - 10000 : 12500
--
--
--     * 3.1 - 14000 : 17500
--
--
--     * 3.2 - 20000 : 25000
--
--
--     * 4 - 20000 : 25000
--
--
--     * 4.1 - 50000 : 62500
--
--
--
-- /Note:/ Consider using 'bitRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpBitRate :: Lens.Lens' VideoParameters (Core.Maybe Types.VideoBitRate)
vpBitRate = Lens.field @"bitRate"
{-# INLINEABLE vpBitRate #-}
{-# DEPRECATED bitRate "Use generic-lens or generic-optics with 'bitRate' instead"  #-}

-- | The video codec for the output file. Valid values include @gif@ , @H.264@ , @mpeg2@ , @vp8@ , and @vp9@ . You can only specify @vp8@ and @vp9@ when the container type is @webm@ , @gif@ when the container type is @gif@ , and @mpeg2@ when the container type is @mpg@ .
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpCodec :: Lens.Lens' VideoParameters (Core.Maybe Types.VideoCodec)
vpCodec = Lens.field @"codec"
{-# INLINEABLE vpCodec #-}
{-# DEPRECATED codec "Use generic-lens or generic-optics with 'codec' instead"  #-}

-- | __Profile (H.264/VP8/VP9 Only)__ 
--
-- The H.264 profile that you want to use for the output file. Elastic Transcoder supports the following profiles:
--
--     * @baseline@ : The profile most commonly used for videoconferencing and for mobile applications.
--
--
--     * @main@ : The profile used for standard-definition digital TV broadcasts.
--
--
--     * @high@ : The profile used for high-definition digital TV broadcasts and for Blu-ray discs.
--
--
-- __Level (H.264 Only)__ 
-- The H.264 level that you want to use for the output file. Elastic Transcoder supports the following levels:
-- @1@ , @1b@ , @1.1@ , @1.2@ , @1.3@ , @2@ , @2.1@ , @2.2@ , @3@ , @3.1@ , @3.2@ , @4@ , @4.1@ 
-- __MaxReferenceFrames (H.264 Only)__ 
-- Applicable only when the value of Video:Codec is H.264. The maximum number of previously decoded frames to use as a reference for decoding future frames. Valid values are integers 0 through 16, but we recommend that you not use a value greater than the following:
-- @Min(Floor(Maximum decoded picture buffer in macroblocks * 256 / (Width in pixels * Height in pixels)), 16)@ 
-- where /Width in pixels/ and /Height in pixels/ represent either MaxWidth and MaxHeight, or Resolution. /Maximum decoded picture buffer in macroblocks/ depends on the value of the @Level@ object. See the list below. (A macroblock is a block of pixels measuring 16x16.) 
--
--     * 1 - 396
--
--
--     * 1b - 396
--
--
--     * 1.1 - 900
--
--
--     * 1.2 - 2376
--
--
--     * 1.3 - 2376
--
--
--     * 2 - 2376
--
--
--     * 2.1 - 4752
--
--
--     * 2.2 - 8100
--
--
--     * 3 - 8100
--
--
--     * 3.1 - 18000
--
--
--     * 3.2 - 20480
--
--
--     * 4 - 32768
--
--
--     * 4.1 - 32768
--
--
-- __MaxBitRate (Optional, H.264/MPEG2/VP8/VP9 only)__ 
-- The maximum number of bits per second in a video buffer; the size of the buffer is specified by @BufferSize@ . Specify a value between 16 and 62,500. You can reduce the bandwidth required to stream a video by reducing the maximum bit rate, but this also reduces the quality of the video.
-- __BufferSize (Optional, H.264/MPEG2/VP8/VP9 only)__ 
-- The maximum number of bits in any x seconds of the output video. This window is commonly 10 seconds, the standard segment duration when you're using FMP4 or MPEG-TS for the container type of the output video. Specify an integer greater than 0. If you specify @MaxBitRate@ and omit @BufferSize@ , Elastic Transcoder sets @BufferSize@ to 10 times the value of @MaxBitRate@ .
-- __InterlacedMode (Optional, H.264/MPEG2 Only)__ 
-- The interlace mode for the output video.
-- Interlaced video is used to double the perceived frame rate for a video by interlacing two fields (one field on every other line, the other field on the other lines) so that the human eye registers multiple pictures per frame. Interlacing reduces the bandwidth required for transmitting a video, but can result in blurred images and flickering.
-- Valid values include @Progressive@ (no interlacing, top to bottom), @TopFirst@ (top field first), @BottomFirst@ (bottom field first), and @Auto@ .
-- If @InterlaceMode@ is not specified, Elastic Transcoder uses @Progressive@ for the output. If @Auto@ is specified, Elastic Transcoder interlaces the output.
-- __ColorSpaceConversionMode (Optional, H.264/MPEG2 Only)__ 
-- The color space conversion Elastic Transcoder applies to the output video. Color spaces are the algorithms used by the computer to store information about how to render color. @Bt.601@ is the standard for standard definition video, while @Bt.709@ is the standard for high definition video.
-- Valid values include @None@ , @Bt709toBt601@ , @Bt601toBt709@ , and @Auto@ .
-- If you chose @Auto@ for @ColorSpaceConversionMode@ and your output is interlaced, your frame rate is one of @23.97@ , @24@ , @25@ , @29.97@ , @50@ , or @60@ , your @SegmentDuration@ is null, and you are using one of the resolution changes from the list below, Elastic Transcoder applies the following color space conversions:
--
--     * /Standard to HD, 720x480 to 1920x1080/ - Elastic Transcoder applies @Bt601ToBt709@ 
--
--
--     * /Standard to HD, 720x576 to 1920x1080/ - Elastic Transcoder applies @Bt601ToBt709@ 
--
--
--     * /HD to Standard, 1920x1080 to 720x480/ - Elastic Transcoder applies @Bt709ToBt601@ 
--
--
--     * /HD to Standard, 1920x1080 to 720x576/ - Elastic Transcoder applies @Bt709ToBt601@ 
--
--
-- If you do not specify a @ColorSpaceConversionMode@ , Elastic Transcoder does not change the color space of a file. If you are unsure what @ColorSpaceConversionMode@ was applied to your output file, you can check the @AppliedColorSpaceConversion@ parameter included in your job response. If your job does not have an @AppliedColorSpaceConversion@ in its response, no @ColorSpaceConversionMode@ was applied.
-- __ChromaSubsampling__ 
-- The sampling pattern for the chroma (color) channels of the output video. Valid values include @yuv420p@ and @yuv422p@ .
-- @yuv420p@ samples the chroma information of every other horizontal and every other vertical line, @yuv422p@ samples the color information of every horizontal line and every other vertical line.
-- __LoopCount (Gif Only)__ 
-- The number of times you want the output gif to loop. Valid values include @Infinite@ and integers between @0@ and @100@ , inclusive.
--
-- /Note:/ Consider using 'codecOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpCodecOptions :: Lens.Lens' VideoParameters (Core.Maybe (Core.HashMap Types.CodecOption Types.CodecOption))
vpCodecOptions = Lens.field @"codecOptions"
{-# INLINEABLE vpCodecOptions #-}
{-# DEPRECATED codecOptions "Use generic-lens or generic-optics with 'codecOptions' instead"  #-}

-- | The value that Elastic Transcoder adds to the metadata in the output file.
--
-- /Note:/ Consider using 'displayAspectRatio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpDisplayAspectRatio :: Lens.Lens' VideoParameters (Core.Maybe Types.AspectRatio)
vpDisplayAspectRatio = Lens.field @"displayAspectRatio"
{-# INLINEABLE vpDisplayAspectRatio #-}
{-# DEPRECATED displayAspectRatio "Use generic-lens or generic-optics with 'displayAspectRatio' instead"  #-}

-- | Applicable only when the value of Video:Codec is one of @H.264@ , @MPEG2@ , or @VP8@ .
--
-- Whether to use a fixed value for @FixedGOP@ . Valid values are @true@ and @false@ :
--
--     * @true@ : Elastic Transcoder uses the value of @KeyframesMaxDist@ for the distance between key frames (the number of frames in a group of pictures, or GOP).
--
--
--     * @false@ : The distance between key frames can vary.
--
--
-- /Important:/ @FixedGOP@ must be set to @true@ for @fmp4@ containers.
--
-- /Note:/ Consider using 'fixedGOP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpFixedGOP :: Lens.Lens' VideoParameters (Core.Maybe Types.FixedGOP)
vpFixedGOP = Lens.field @"fixedGOP"
{-# INLINEABLE vpFixedGOP #-}
{-# DEPRECATED fixedGOP "Use generic-lens or generic-optics with 'fixedGOP' instead"  #-}

-- | The frames per second for the video stream in the output file. Valid values include:
--
-- @auto@ , @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@ 
-- If you specify @auto@ , Elastic Transcoder uses the detected frame rate of the input source. If you specify a frame rate, we recommend that you perform the following calculation:
-- @Frame rate = maximum recommended decoding speed in luma samples/second / (width in pixels * height in pixels)@ 
-- where:
--
--     * /width in pixels/ and /height in pixels/ represent the Resolution of the output video.
--
--
--     * /maximum recommended decoding speed in Luma samples\/second/ is less than or equal to the maximum value listed in the following table, based on the value that you specified for Level.
--
--
-- The maximum recommended decoding speed in Luma samples/second for each level is described in the following list (/Level - Decoding speed/ ):
--
--     * 1 - 380160
--
--
--     * 1b - 380160
--
--
--     * 1.1 - 76800
--
--
--     * 1.2 - 1536000
--
--
--     * 1.3 - 3041280
--
--
--     * 2 - 3041280
--
--
--     * 2.1 - 5068800
--
--
--     * 2.2 - 5184000
--
--
--     * 3 - 10368000
--
--
--     * 3.1 - 27648000
--
--
--     * 3.2 - 55296000
--
--
--     * 4 - 62914560
--
--
--     * 4.1 - 62914560
--
--
--
-- /Note:/ Consider using 'frameRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpFrameRate :: Lens.Lens' VideoParameters (Core.Maybe Types.FrameRate)
vpFrameRate = Lens.field @"frameRate"
{-# INLINEABLE vpFrameRate #-}
{-# DEPRECATED frameRate "Use generic-lens or generic-optics with 'frameRate' instead"  #-}

-- | Applicable only when the value of Video:Codec is one of @H.264@ , @MPEG2@ , or @VP8@ .
--
-- The maximum number of frames between key frames. Key frames are fully encoded frames; the frames between key frames are encoded based, in part, on the content of the key frames. The value is an integer formatted as a string; valid values are between 1 (every frame is a key frame) and 100000, inclusive. A higher value results in higher compression but may also discernibly decrease video quality.
-- For @Smooth@ outputs, the @FrameRate@ must have a constant ratio to the @KeyframesMaxDist@ . This allows @Smooth@ playlists to switch between different quality levels while the file is being played.
-- For example, an input file can have a @FrameRate@ of 30 with a @KeyframesMaxDist@ of 90. The output file then needs to have a ratio of 1:3. Valid outputs would have @FrameRate@ of 30, 25, and 10, and @KeyframesMaxDist@ of 90, 75, and 30, respectively.
-- Alternately, this can be achieved by setting @FrameRate@ to auto and having the same values for @MaxFrameRate@ and @KeyframesMaxDist@ .
--
-- /Note:/ Consider using 'keyframesMaxDist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpKeyframesMaxDist :: Lens.Lens' VideoParameters (Core.Maybe Types.KeyframesMaxDist)
vpKeyframesMaxDist = Lens.field @"keyframesMaxDist"
{-# INLINEABLE vpKeyframesMaxDist #-}
{-# DEPRECATED keyframesMaxDist "Use generic-lens or generic-optics with 'keyframesMaxDist' instead"  #-}

-- | If you specify @auto@ for @FrameRate@ , Elastic Transcoder uses the frame rate of the input video for the frame rate of the output video. Specify the maximum frame rate that you want Elastic Transcoder to use when the frame rate of the input video is greater than the desired maximum frame rate of the output video. Valid values include: @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@ .
--
-- /Note:/ Consider using 'maxFrameRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpMaxFrameRate :: Lens.Lens' VideoParameters (Core.Maybe Types.MaxFrameRate)
vpMaxFrameRate = Lens.field @"maxFrameRate"
{-# INLINEABLE vpMaxFrameRate #-}
{-# DEPRECATED maxFrameRate "Use generic-lens or generic-optics with 'maxFrameRate' instead"  #-}

-- | The maximum height of the output video in pixels. If you specify @auto@ , Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 96 and 3072.
--
-- /Note:/ Consider using 'maxHeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpMaxHeight :: Lens.Lens' VideoParameters (Core.Maybe Types.DigitsOrAuto)
vpMaxHeight = Lens.field @"maxHeight"
{-# INLINEABLE vpMaxHeight #-}
{-# DEPRECATED maxHeight "Use generic-lens or generic-optics with 'maxHeight' instead"  #-}

-- | The maximum width of the output video in pixels. If you specify @auto@ , Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 128 and 4096. 
--
-- /Note:/ Consider using 'maxWidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpMaxWidth :: Lens.Lens' VideoParameters (Core.Maybe Types.DigitsOrAuto)
vpMaxWidth = Lens.field @"maxWidth"
{-# INLINEABLE vpMaxWidth #-}
{-# DEPRECATED maxWidth "Use generic-lens or generic-optics with 'maxWidth' instead"  #-}

-- | When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add black bars to the top and bottom and/or left and right sides of the output video to make the total size of the output video match the values that you specified for @MaxWidth@ and @MaxHeight@ .
--
-- /Note:/ Consider using 'paddingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpPaddingPolicy :: Lens.Lens' VideoParameters (Core.Maybe Types.PaddingPolicy)
vpPaddingPolicy = Lens.field @"paddingPolicy"
{-# INLINEABLE vpPaddingPolicy #-}
{-# DEPRECATED paddingPolicy "Use generic-lens or generic-optics with 'paddingPolicy' instead"  #-}

-- | /Important:/ To better control resolution and aspect ratio of output videos, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , @PaddingPolicy@ , and @DisplayAspectRatio@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together.
--
-- The width and height of the video in the output file, in pixels. Valid values are @auto@ and /width/ x /height/ :
--
--     * @auto@ : Elastic Transcoder attempts to preserve the width and height of the input file, subject to the following rules.
--
--
--     * @/width/ x /height/ @ : The width and height of the output video in pixels.
--
--
-- Note the following about specifying the width and height:
--
--     * The width must be an even integer between 128 and 4096, inclusive.
--
--
--     * The height must be an even integer between 96 and 3072, inclusive.
--
--
--     * If you specify a resolution that is less than the resolution of the input file, Elastic Transcoder rescales the output file to the lower resolution.
--
--
--     * If you specify a resolution that is greater than the resolution of the input file, Elastic Transcoder rescales the output to the higher resolution.
--
--
--     * We recommend that you specify a resolution for which the product of width and height is less than or equal to the applicable value in the following list (/List - Max width x height value/ ):
--
--     * 1 - 25344
--
--
--     * 1b - 25344
--
--
--     * 1.1 - 101376
--
--
--     * 1.2 - 101376
--
--
--     * 1.3 - 101376
--
--
--     * 2 - 101376
--
--
--     * 2.1 - 202752
--
--
--     * 2.2 - 404720
--
--
--     * 3 - 404720
--
--
--     * 3.1 - 921600
--
--
--     * 3.2 - 1310720
--
--
--     * 4 - 2097152
--
--
--     * 4.1 - 2097152
--
--
--
--
--
-- /Note:/ Consider using 'resolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpResolution :: Lens.Lens' VideoParameters (Core.Maybe Types.Resolution)
vpResolution = Lens.field @"resolution"
{-# INLINEABLE vpResolution #-}
{-# DEPRECATED resolution "Use generic-lens or generic-optics with 'resolution' instead"  #-}

-- | Specify one of the following values to control scaling of the output video:
--
--
--     * @Fit@ : Elastic Transcoder scales the output video so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.
--
--
--     * @Fill@ : Elastic Transcoder scales the output video so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ and matches or exceeds the other value. Elastic Transcoder centers the output video and then crops it in the dimension (if any) that exceeds the maximum value.
--
--
--     * @Stretch@ : Elastic Transcoder stretches the output video to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the input video and the output video are different, the output video will be distorted.
--
--
--     * @Keep@ : Elastic Transcoder does not scale the output video. If either dimension of the input video exceeds the values that you specified for @MaxWidth@ and @MaxHeight@ , Elastic Transcoder crops the output video.
--
--
--     * @ShrinkToFit@ : Elastic Transcoder scales the output video down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the video up.
--
--
--     * @ShrinkToFill@ : Elastic Transcoder scales the output video down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale the video up.
--
--
--
-- /Note:/ Consider using 'sizingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpSizingPolicy :: Lens.Lens' VideoParameters (Core.Maybe Types.SizingPolicy)
vpSizingPolicy = Lens.field @"sizingPolicy"
{-# INLINEABLE vpSizingPolicy #-}
{-# DEPRECATED sizingPolicy "Use generic-lens or generic-optics with 'sizingPolicy' instead"  #-}

-- | Settings for the size, location, and opacity of graphics that you want Elastic Transcoder to overlay over videos that are transcoded using this preset. You can specify settings for up to four watermarks. Watermarks appear in the specified size and location, and with the specified opacity for the duration of the transcoded video.
--
-- Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.
-- When you create a job that uses this preset, you specify the .png or .jpg graphics that you want Elastic Transcoder to include in the transcoded videos. You can specify fewer graphics in the job than you specify watermark settings in the preset, which allows you to use the same preset for up to four watermarks that have different dimensions.
--
-- /Note:/ Consider using 'watermarks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpWatermarks :: Lens.Lens' VideoParameters (Core.Maybe [Types.PresetWatermark])
vpWatermarks = Lens.field @"watermarks"
{-# INLINEABLE vpWatermarks #-}
{-# DEPRECATED watermarks "Use generic-lens or generic-optics with 'watermarks' instead"  #-}

instance Core.FromJSON VideoParameters where
        toJSON VideoParameters{..}
          = Core.object
              (Core.catMaybes
                 [("AspectRatio" Core..=) Core.<$> aspectRatio,
                  ("BitRate" Core..=) Core.<$> bitRate,
                  ("Codec" Core..=) Core.<$> codec,
                  ("CodecOptions" Core..=) Core.<$> codecOptions,
                  ("DisplayAspectRatio" Core..=) Core.<$> displayAspectRatio,
                  ("FixedGOP" Core..=) Core.<$> fixedGOP,
                  ("FrameRate" Core..=) Core.<$> frameRate,
                  ("KeyframesMaxDist" Core..=) Core.<$> keyframesMaxDist,
                  ("MaxFrameRate" Core..=) Core.<$> maxFrameRate,
                  ("MaxHeight" Core..=) Core.<$> maxHeight,
                  ("MaxWidth" Core..=) Core.<$> maxWidth,
                  ("PaddingPolicy" Core..=) Core.<$> paddingPolicy,
                  ("Resolution" Core..=) Core.<$> resolution,
                  ("SizingPolicy" Core..=) Core.<$> sizingPolicy,
                  ("Watermarks" Core..=) Core.<$> watermarks])

instance Core.FromJSON VideoParameters where
        parseJSON
          = Core.withObject "VideoParameters" Core.$
              \ x ->
                VideoParameters' Core.<$>
                  (x Core..:? "AspectRatio") Core.<*> x Core..:? "BitRate" Core.<*>
                    x Core..:? "Codec"
                    Core.<*> x Core..:? "CodecOptions"
                    Core.<*> x Core..:? "DisplayAspectRatio"
                    Core.<*> x Core..:? "FixedGOP"
                    Core.<*> x Core..:? "FrameRate"
                    Core.<*> x Core..:? "KeyframesMaxDist"
                    Core.<*> x Core..:? "MaxFrameRate"
                    Core.<*> x Core..:? "MaxHeight"
                    Core.<*> x Core..:? "MaxWidth"
                    Core.<*> x Core..:? "PaddingPolicy"
                    Core.<*> x Core..:? "Resolution"
                    Core.<*> x Core..:? "SizingPolicy"
                    Core.<*> x Core..:? "Watermarks"
