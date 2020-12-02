{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.H264AdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264CodecLevel
import Network.AWS.MediaConvert.Types.H264CodecProfile
import Network.AWS.MediaConvert.Types.H264DynamicSubGop
import Network.AWS.MediaConvert.Types.H264EntropyEncoding
import Network.AWS.MediaConvert.Types.H264FieldEncoding
import Network.AWS.MediaConvert.Types.H264FlickerAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264FramerateControl
import Network.AWS.MediaConvert.Types.H264FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.H264GopBReference
import Network.AWS.MediaConvert.Types.H264GopSizeUnits
import Network.AWS.MediaConvert.Types.H264InterlaceMode
import Network.AWS.MediaConvert.Types.H264ParControl
import Network.AWS.MediaConvert.Types.H264QualityTuningLevel
import Network.AWS.MediaConvert.Types.H264QvbrSettings
import Network.AWS.MediaConvert.Types.H264RateControlMode
import Network.AWS.MediaConvert.Types.H264RepeatPps
import Network.AWS.MediaConvert.Types.H264SceneChangeDetect
import Network.AWS.MediaConvert.Types.H264SlowPal
import Network.AWS.MediaConvert.Types.H264SpatialAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264Syntax
import Network.AWS.MediaConvert.Types.H264Telecine
import Network.AWS.MediaConvert.Types.H264TemporalAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
import Network.AWS.Prelude

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
--
-- /See:/ 'h264Settings' smart constructor.
data H264Settings = H264Settings'
  { _hssUnregisteredSeiTimecode ::
      !(Maybe H264UnregisteredSeiTimecode),
    _hssQualityTuningLevel :: !(Maybe H264QualityTuningLevel),
    _hssTemporalAdaptiveQuantization ::
      !(Maybe H264TemporalAdaptiveQuantization),
    _hssSceneChangeDetect :: !(Maybe H264SceneChangeDetect),
    _hssHrdBufferInitialFillPercentage :: !(Maybe Nat),
    _hssSlowPal :: !(Maybe H264SlowPal),
    _hssParNumerator :: !(Maybe Nat),
    _hssGopSize :: !(Maybe Double),
    _hssNumberBFramesBetweenReferenceFrames :: !(Maybe Nat),
    _hssGopSizeUnits :: !(Maybe H264GopSizeUnits),
    _hssHrdBufferSize :: !(Maybe Nat),
    _hssSlices :: !(Maybe Nat),
    _hssRateControlMode :: !(Maybe H264RateControlMode),
    _hssNumberReferenceFrames :: !(Maybe Nat),
    _hssTelecine :: !(Maybe H264Telecine),
    _hssDynamicSubGop :: !(Maybe H264DynamicSubGop),
    _hssMinIInterval :: !(Maybe Nat),
    _hssInterlaceMode :: !(Maybe H264InterlaceMode),
    _hssParControl :: !(Maybe H264ParControl),
    _hssRepeatPps :: !(Maybe H264RepeatPps),
    _hssFlickerAdaptiveQuantization ::
      !(Maybe H264FlickerAdaptiveQuantization),
    _hssQvbrSettings :: !(Maybe H264QvbrSettings),
    _hssSoftness :: !(Maybe Nat),
    _hssCodecProfile :: !(Maybe H264CodecProfile),
    _hssBitrate :: !(Maybe Nat),
    _hssFramerateDenominator :: !(Maybe Nat),
    _hssFramerateConversionAlgorithm ::
      !(Maybe H264FramerateConversionAlgorithm),
    _hssCodecLevel :: !(Maybe H264CodecLevel),
    _hssEntropyEncoding :: !(Maybe H264EntropyEncoding),
    _hssFramerateControl :: !(Maybe H264FramerateControl),
    _hssAdaptiveQuantization :: !(Maybe H264AdaptiveQuantization),
    _hssFramerateNumerator :: !(Maybe Nat),
    _hssGopBReference :: !(Maybe H264GopBReference),
    _hssMaxBitrate :: !(Maybe Nat),
    _hssSyntax :: !(Maybe H264Syntax),
    _hssFieldEncoding :: !(Maybe H264FieldEncoding),
    _hssGopClosedCadence :: !(Maybe Nat),
    _hssParDenominator :: !(Maybe Nat),
    _hssSpatialAdaptiveQuantization ::
      !(Maybe H264SpatialAdaptiveQuantization)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'H264Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hssUnregisteredSeiTimecode' - Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
--
-- * 'hssQualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
--
-- * 'hssTemporalAdaptiveQuantization' - Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264TemporalAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to set H264TemporalAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization). To manually enable or disable H264TemporalAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
--
-- * 'hssSceneChangeDetect' - Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
--
-- * 'hssHrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- * 'hssSlowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- * 'hssParNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- * 'hssGopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- * 'hssNumberBFramesBetweenReferenceFrames' - Number of B-frames between reference frames.
--
-- * 'hssGopSizeUnits' - Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
--
-- * 'hssHrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- * 'hssSlices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- * 'hssRateControlMode' - Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
--
-- * 'hssNumberReferenceFrames' - Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- * 'hssTelecine' - When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
--
-- * 'hssDynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
--
-- * 'hssMinIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'hssInterlaceMode' - Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- * 'hssParControl' - Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- * 'hssRepeatPps' - Places a PPS header on each encoded picture, even if repeated.
--
-- * 'hssFlickerAdaptiveQuantization' - Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264FlickerAdaptiveQuantization is Disabled (DISABLED). Change this value to Enabled (ENABLED) to reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. To manually enable or disable H264FlickerAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
--
-- * 'hssQvbrSettings' - Settings for quality-defined variable bitrate encoding with the H.264 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- * 'hssSoftness' - Ignore this setting unless you need to comply with a specification that requires a specific value. If you don't have a specification requirement, we recommend that you adjust the softness of your output by using a lower value for the setting Sharpness (sharpness) or by enabling a noise reducer filter (noiseReducerFilter). The Softness (softness) setting specifies the quantization matrices that the encoder uses. Keep the default value, 0, for flat quantization. Choose the value 1 or 16 to use the default JVT softening quantization matricies from the H.264 specification. Choose a value from 17 to 128 to use planar interpolation. Increasing values from 17 to 128 result in increasing reduction of high-frequency data. The value 128 results in the softest video.
--
-- * 'hssCodecProfile' - H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
--
-- * 'hssBitrate' - Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- * 'hssFramerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'hssFramerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- * 'hssCodecLevel' - Specify an H.264 level that is consistent with your output video settings. If you aren't sure what level to specify, choose Auto (AUTO).
--
-- * 'hssEntropyEncoding' - Entropy encoding mode. Use CABAC (must be in Main or High profile) or CAVLC.
--
-- * 'hssFramerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- * 'hssAdaptiveQuantization' - Keep the default value, Auto (AUTO), for this setting to have MediaConvert automatically apply the best types of quantization for your video content. When you want to apply your quantization settings manually, you must set H264AdaptiveQuantization to a value other than Auto (AUTO). Use this setting to specify the strength of any adaptive quantization filters that you enable. If you don't want MediaConvert to do any adaptive quantization in this transcode, set Adaptive quantization (H264AdaptiveQuantization) to Off (OFF). Related settings: The value that you choose here applies to the following settings: H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and H264TemporalAdaptiveQuantization.
--
-- * 'hssFramerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'hssGopBReference' - If enable, use reference B frames for GOP structures that have B frames > 1.
--
-- * 'hssMaxBitrate' - Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
--
-- * 'hssSyntax' - Produces a bitstream compliant with SMPTE RP-2027.
--
-- * 'hssFieldEncoding' - Keep the default value, PAFF, to have MediaConvert use PAFF encoding for interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF encoding and create separate interlaced fields.
--
-- * 'hssGopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- * 'hssParDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- * 'hssSpatialAdaptiveQuantization' - Only use this setting when you change the default value, Auto (AUTO), for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264SpatialAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to set H264SpatialAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (H264AdaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher. To manually enable or disable H264SpatialAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
h264Settings ::
  H264Settings
h264Settings =
  H264Settings'
    { _hssUnregisteredSeiTimecode = Nothing,
      _hssQualityTuningLevel = Nothing,
      _hssTemporalAdaptiveQuantization = Nothing,
      _hssSceneChangeDetect = Nothing,
      _hssHrdBufferInitialFillPercentage = Nothing,
      _hssSlowPal = Nothing,
      _hssParNumerator = Nothing,
      _hssGopSize = Nothing,
      _hssNumberBFramesBetweenReferenceFrames = Nothing,
      _hssGopSizeUnits = Nothing,
      _hssHrdBufferSize = Nothing,
      _hssSlices = Nothing,
      _hssRateControlMode = Nothing,
      _hssNumberReferenceFrames = Nothing,
      _hssTelecine = Nothing,
      _hssDynamicSubGop = Nothing,
      _hssMinIInterval = Nothing,
      _hssInterlaceMode = Nothing,
      _hssParControl = Nothing,
      _hssRepeatPps = Nothing,
      _hssFlickerAdaptiveQuantization = Nothing,
      _hssQvbrSettings = Nothing,
      _hssSoftness = Nothing,
      _hssCodecProfile = Nothing,
      _hssBitrate = Nothing,
      _hssFramerateDenominator = Nothing,
      _hssFramerateConversionAlgorithm = Nothing,
      _hssCodecLevel = Nothing,
      _hssEntropyEncoding = Nothing,
      _hssFramerateControl = Nothing,
      _hssAdaptiveQuantization = Nothing,
      _hssFramerateNumerator = Nothing,
      _hssGopBReference = Nothing,
      _hssMaxBitrate = Nothing,
      _hssSyntax = Nothing,
      _hssFieldEncoding = Nothing,
      _hssGopClosedCadence = Nothing,
      _hssParDenominator = Nothing,
      _hssSpatialAdaptiveQuantization = Nothing
    }

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
hssUnregisteredSeiTimecode :: Lens' H264Settings (Maybe H264UnregisteredSeiTimecode)
hssUnregisteredSeiTimecode = lens _hssUnregisteredSeiTimecode (\s a -> s {_hssUnregisteredSeiTimecode = a})

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
hssQualityTuningLevel :: Lens' H264Settings (Maybe H264QualityTuningLevel)
hssQualityTuningLevel = lens _hssQualityTuningLevel (\s a -> s {_hssQualityTuningLevel = a})

-- | Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264TemporalAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to set H264TemporalAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization). To manually enable or disable H264TemporalAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
hssTemporalAdaptiveQuantization :: Lens' H264Settings (Maybe H264TemporalAdaptiveQuantization)
hssTemporalAdaptiveQuantization = lens _hssTemporalAdaptiveQuantization (\s a -> s {_hssTemporalAdaptiveQuantization = a})

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
hssSceneChangeDetect :: Lens' H264Settings (Maybe H264SceneChangeDetect)
hssSceneChangeDetect = lens _hssSceneChangeDetect (\s a -> s {_hssSceneChangeDetect = a})

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
hssHrdBufferInitialFillPercentage :: Lens' H264Settings (Maybe Natural)
hssHrdBufferInitialFillPercentage = lens _hssHrdBufferInitialFillPercentage (\s a -> s {_hssHrdBufferInitialFillPercentage = a}) . mapping _Nat

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
hssSlowPal :: Lens' H264Settings (Maybe H264SlowPal)
hssSlowPal = lens _hssSlowPal (\s a -> s {_hssSlowPal = a})

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
hssParNumerator :: Lens' H264Settings (Maybe Natural)
hssParNumerator = lens _hssParNumerator (\s a -> s {_hssParNumerator = a}) . mapping _Nat

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
hssGopSize :: Lens' H264Settings (Maybe Double)
hssGopSize = lens _hssGopSize (\s a -> s {_hssGopSize = a})

-- | Number of B-frames between reference frames.
hssNumberBFramesBetweenReferenceFrames :: Lens' H264Settings (Maybe Natural)
hssNumberBFramesBetweenReferenceFrames = lens _hssNumberBFramesBetweenReferenceFrames (\s a -> s {_hssNumberBFramesBetweenReferenceFrames = a}) . mapping _Nat

-- | Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
hssGopSizeUnits :: Lens' H264Settings (Maybe H264GopSizeUnits)
hssGopSizeUnits = lens _hssGopSizeUnits (\s a -> s {_hssGopSizeUnits = a})

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
hssHrdBufferSize :: Lens' H264Settings (Maybe Natural)
hssHrdBufferSize = lens _hssHrdBufferSize (\s a -> s {_hssHrdBufferSize = a}) . mapping _Nat

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
hssSlices :: Lens' H264Settings (Maybe Natural)
hssSlices = lens _hssSlices (\s a -> s {_hssSlices = a}) . mapping _Nat

-- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
hssRateControlMode :: Lens' H264Settings (Maybe H264RateControlMode)
hssRateControlMode = lens _hssRateControlMode (\s a -> s {_hssRateControlMode = a})

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
hssNumberReferenceFrames :: Lens' H264Settings (Maybe Natural)
hssNumberReferenceFrames = lens _hssNumberReferenceFrames (\s a -> s {_hssNumberReferenceFrames = a}) . mapping _Nat

-- | When you do frame rate conversion from 23.976 frames per second (fps) to 29.97 fps, and your output scan type is interlaced, you can optionally enable hard or soft telecine to create a smoother picture. Hard telecine (HARD) produces a 29.97i output. Soft telecine (SOFT) produces an output with a 23.976 output that signals to the video player device to do the conversion during play back. When you keep the default value, None (NONE), MediaConvert does a standard frame rate conversion to 29.97 without doing anything with the field polarity to create a smoother picture.
hssTelecine :: Lens' H264Settings (Maybe H264Telecine)
hssTelecine = lens _hssTelecine (\s a -> s {_hssTelecine = a})

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
hssDynamicSubGop :: Lens' H264Settings (Maybe H264DynamicSubGop)
hssDynamicSubGop = lens _hssDynamicSubGop (\s a -> s {_hssDynamicSubGop = a})

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
hssMinIInterval :: Lens' H264Settings (Maybe Natural)
hssMinIInterval = lens _hssMinIInterval (\s a -> s {_hssMinIInterval = a}) . mapping _Nat

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
hssInterlaceMode :: Lens' H264Settings (Maybe H264InterlaceMode)
hssInterlaceMode = lens _hssInterlaceMode (\s a -> s {_hssInterlaceMode = a})

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
hssParControl :: Lens' H264Settings (Maybe H264ParControl)
hssParControl = lens _hssParControl (\s a -> s {_hssParControl = a})

-- | Places a PPS header on each encoded picture, even if repeated.
hssRepeatPps :: Lens' H264Settings (Maybe H264RepeatPps)
hssRepeatPps = lens _hssRepeatPps (\s a -> s {_hssRepeatPps = a})

-- | Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264FlickerAdaptiveQuantization is Disabled (DISABLED). Change this value to Enabled (ENABLED) to reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. To manually enable or disable H264FlickerAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
hssFlickerAdaptiveQuantization :: Lens' H264Settings (Maybe H264FlickerAdaptiveQuantization)
hssFlickerAdaptiveQuantization = lens _hssFlickerAdaptiveQuantization (\s a -> s {_hssFlickerAdaptiveQuantization = a})

-- | Settings for quality-defined variable bitrate encoding with the H.264 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
hssQvbrSettings :: Lens' H264Settings (Maybe H264QvbrSettings)
hssQvbrSettings = lens _hssQvbrSettings (\s a -> s {_hssQvbrSettings = a})

-- | Ignore this setting unless you need to comply with a specification that requires a specific value. If you don't have a specification requirement, we recommend that you adjust the softness of your output by using a lower value for the setting Sharpness (sharpness) or by enabling a noise reducer filter (noiseReducerFilter). The Softness (softness) setting specifies the quantization matrices that the encoder uses. Keep the default value, 0, for flat quantization. Choose the value 1 or 16 to use the default JVT softening quantization matricies from the H.264 specification. Choose a value from 17 to 128 to use planar interpolation. Increasing values from 17 to 128 result in increasing reduction of high-frequency data. The value 128 results in the softest video.
hssSoftness :: Lens' H264Settings (Maybe Natural)
hssSoftness = lens _hssSoftness (\s a -> s {_hssSoftness = a}) . mapping _Nat

-- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
hssCodecProfile :: Lens' H264Settings (Maybe H264CodecProfile)
hssCodecProfile = lens _hssCodecProfile (\s a -> s {_hssCodecProfile = a})

-- | Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
hssBitrate :: Lens' H264Settings (Maybe Natural)
hssBitrate = lens _hssBitrate (\s a -> s {_hssBitrate = a}) . mapping _Nat

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
hssFramerateDenominator :: Lens' H264Settings (Maybe Natural)
hssFramerateDenominator = lens _hssFramerateDenominator (\s a -> s {_hssFramerateDenominator = a}) . mapping _Nat

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
hssFramerateConversionAlgorithm :: Lens' H264Settings (Maybe H264FramerateConversionAlgorithm)
hssFramerateConversionAlgorithm = lens _hssFramerateConversionAlgorithm (\s a -> s {_hssFramerateConversionAlgorithm = a})

-- | Specify an H.264 level that is consistent with your output video settings. If you aren't sure what level to specify, choose Auto (AUTO).
hssCodecLevel :: Lens' H264Settings (Maybe H264CodecLevel)
hssCodecLevel = lens _hssCodecLevel (\s a -> s {_hssCodecLevel = a})

-- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or CAVLC.
hssEntropyEncoding :: Lens' H264Settings (Maybe H264EntropyEncoding)
hssEntropyEncoding = lens _hssEntropyEncoding (\s a -> s {_hssEntropyEncoding = a})

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
hssFramerateControl :: Lens' H264Settings (Maybe H264FramerateControl)
hssFramerateControl = lens _hssFramerateControl (\s a -> s {_hssFramerateControl = a})

-- | Keep the default value, Auto (AUTO), for this setting to have MediaConvert automatically apply the best types of quantization for your video content. When you want to apply your quantization settings manually, you must set H264AdaptiveQuantization to a value other than Auto (AUTO). Use this setting to specify the strength of any adaptive quantization filters that you enable. If you don't want MediaConvert to do any adaptive quantization in this transcode, set Adaptive quantization (H264AdaptiveQuantization) to Off (OFF). Related settings: The value that you choose here applies to the following settings: H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and H264TemporalAdaptiveQuantization.
hssAdaptiveQuantization :: Lens' H264Settings (Maybe H264AdaptiveQuantization)
hssAdaptiveQuantization = lens _hssAdaptiveQuantization (\s a -> s {_hssAdaptiveQuantization = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
hssFramerateNumerator :: Lens' H264Settings (Maybe Natural)
hssFramerateNumerator = lens _hssFramerateNumerator (\s a -> s {_hssFramerateNumerator = a}) . mapping _Nat

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
hssGopBReference :: Lens' H264Settings (Maybe H264GopBReference)
hssGopBReference = lens _hssGopBReference (\s a -> s {_hssGopBReference = a})

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
hssMaxBitrate :: Lens' H264Settings (Maybe Natural)
hssMaxBitrate = lens _hssMaxBitrate (\s a -> s {_hssMaxBitrate = a}) . mapping _Nat

-- | Produces a bitstream compliant with SMPTE RP-2027.
hssSyntax :: Lens' H264Settings (Maybe H264Syntax)
hssSyntax = lens _hssSyntax (\s a -> s {_hssSyntax = a})

-- | Keep the default value, PAFF, to have MediaConvert use PAFF encoding for interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF encoding and create separate interlaced fields.
hssFieldEncoding :: Lens' H264Settings (Maybe H264FieldEncoding)
hssFieldEncoding = lens _hssFieldEncoding (\s a -> s {_hssFieldEncoding = a})

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
hssGopClosedCadence :: Lens' H264Settings (Maybe Natural)
hssGopClosedCadence = lens _hssGopClosedCadence (\s a -> s {_hssGopClosedCadence = a}) . mapping _Nat

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
hssParDenominator :: Lens' H264Settings (Maybe Natural)
hssParDenominator = lens _hssParDenominator (\s a -> s {_hssParDenominator = a}) . mapping _Nat

-- | Only use this setting when you change the default value, Auto (AUTO), for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264SpatialAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to set H264SpatialAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (H264AdaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher. To manually enable or disable H264SpatialAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
hssSpatialAdaptiveQuantization :: Lens' H264Settings (Maybe H264SpatialAdaptiveQuantization)
hssSpatialAdaptiveQuantization = lens _hssSpatialAdaptiveQuantization (\s a -> s {_hssSpatialAdaptiveQuantization = a})

instance FromJSON H264Settings where
  parseJSON =
    withObject
      "H264Settings"
      ( \x ->
          H264Settings'
            <$> (x .:? "unregisteredSeiTimecode")
            <*> (x .:? "qualityTuningLevel")
            <*> (x .:? "temporalAdaptiveQuantization")
            <*> (x .:? "sceneChangeDetect")
            <*> (x .:? "hrdBufferInitialFillPercentage")
            <*> (x .:? "slowPal")
            <*> (x .:? "parNumerator")
            <*> (x .:? "gopSize")
            <*> (x .:? "numberBFramesBetweenReferenceFrames")
            <*> (x .:? "gopSizeUnits")
            <*> (x .:? "hrdBufferSize")
            <*> (x .:? "slices")
            <*> (x .:? "rateControlMode")
            <*> (x .:? "numberReferenceFrames")
            <*> (x .:? "telecine")
            <*> (x .:? "dynamicSubGop")
            <*> (x .:? "minIInterval")
            <*> (x .:? "interlaceMode")
            <*> (x .:? "parControl")
            <*> (x .:? "repeatPps")
            <*> (x .:? "flickerAdaptiveQuantization")
            <*> (x .:? "qvbrSettings")
            <*> (x .:? "softness")
            <*> (x .:? "codecProfile")
            <*> (x .:? "bitrate")
            <*> (x .:? "framerateDenominator")
            <*> (x .:? "framerateConversionAlgorithm")
            <*> (x .:? "codecLevel")
            <*> (x .:? "entropyEncoding")
            <*> (x .:? "framerateControl")
            <*> (x .:? "adaptiveQuantization")
            <*> (x .:? "framerateNumerator")
            <*> (x .:? "gopBReference")
            <*> (x .:? "maxBitrate")
            <*> (x .:? "syntax")
            <*> (x .:? "fieldEncoding")
            <*> (x .:? "gopClosedCadence")
            <*> (x .:? "parDenominator")
            <*> (x .:? "spatialAdaptiveQuantization")
      )

instance Hashable H264Settings

instance NFData H264Settings

instance ToJSON H264Settings where
  toJSON H264Settings' {..} =
    object
      ( catMaybes
          [ ("unregisteredSeiTimecode" .=) <$> _hssUnregisteredSeiTimecode,
            ("qualityTuningLevel" .=) <$> _hssQualityTuningLevel,
            ("temporalAdaptiveQuantization" .=)
              <$> _hssTemporalAdaptiveQuantization,
            ("sceneChangeDetect" .=) <$> _hssSceneChangeDetect,
            ("hrdBufferInitialFillPercentage" .=)
              <$> _hssHrdBufferInitialFillPercentage,
            ("slowPal" .=) <$> _hssSlowPal,
            ("parNumerator" .=) <$> _hssParNumerator,
            ("gopSize" .=) <$> _hssGopSize,
            ("numberBFramesBetweenReferenceFrames" .=)
              <$> _hssNumberBFramesBetweenReferenceFrames,
            ("gopSizeUnits" .=) <$> _hssGopSizeUnits,
            ("hrdBufferSize" .=) <$> _hssHrdBufferSize,
            ("slices" .=) <$> _hssSlices,
            ("rateControlMode" .=) <$> _hssRateControlMode,
            ("numberReferenceFrames" .=) <$> _hssNumberReferenceFrames,
            ("telecine" .=) <$> _hssTelecine,
            ("dynamicSubGop" .=) <$> _hssDynamicSubGop,
            ("minIInterval" .=) <$> _hssMinIInterval,
            ("interlaceMode" .=) <$> _hssInterlaceMode,
            ("parControl" .=) <$> _hssParControl,
            ("repeatPps" .=) <$> _hssRepeatPps,
            ("flickerAdaptiveQuantization" .=)
              <$> _hssFlickerAdaptiveQuantization,
            ("qvbrSettings" .=) <$> _hssQvbrSettings,
            ("softness" .=) <$> _hssSoftness,
            ("codecProfile" .=) <$> _hssCodecProfile,
            ("bitrate" .=) <$> _hssBitrate,
            ("framerateDenominator" .=) <$> _hssFramerateDenominator,
            ("framerateConversionAlgorithm" .=)
              <$> _hssFramerateConversionAlgorithm,
            ("codecLevel" .=) <$> _hssCodecLevel,
            ("entropyEncoding" .=) <$> _hssEntropyEncoding,
            ("framerateControl" .=) <$> _hssFramerateControl,
            ("adaptiveQuantization" .=) <$> _hssAdaptiveQuantization,
            ("framerateNumerator" .=) <$> _hssFramerateNumerator,
            ("gopBReference" .=) <$> _hssGopBReference,
            ("maxBitrate" .=) <$> _hssMaxBitrate,
            ("syntax" .=) <$> _hssSyntax,
            ("fieldEncoding" .=) <$> _hssFieldEncoding,
            ("gopClosedCadence" .=) <$> _hssGopClosedCadence,
            ("parDenominator" .=) <$> _hssParDenominator,
            ("spatialAdaptiveQuantization" .=)
              <$> _hssSpatialAdaptiveQuantization
          ]
      )
