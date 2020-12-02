{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.H265AdaptiveQuantization
import Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei
import Network.AWS.MediaConvert.Types.H265CodecLevel
import Network.AWS.MediaConvert.Types.H265CodecProfile
import Network.AWS.MediaConvert.Types.H265DynamicSubGop
import Network.AWS.MediaConvert.Types.H265FlickerAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H265FramerateControl
import Network.AWS.MediaConvert.Types.H265FramerateConversionAlgorithm
import Network.AWS.MediaConvert.Types.H265GopBReference
import Network.AWS.MediaConvert.Types.H265GopSizeUnits
import Network.AWS.MediaConvert.Types.H265InterlaceMode
import Network.AWS.MediaConvert.Types.H265ParControl
import Network.AWS.MediaConvert.Types.H265QualityTuningLevel
import Network.AWS.MediaConvert.Types.H265QvbrSettings
import Network.AWS.MediaConvert.Types.H265RateControlMode
import Network.AWS.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode
import Network.AWS.MediaConvert.Types.H265SceneChangeDetect
import Network.AWS.MediaConvert.Types.H265SlowPal
import Network.AWS.MediaConvert.Types.H265SpatialAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H265Telecine
import Network.AWS.MediaConvert.Types.H265TemporalAdaptiveQuantization
import Network.AWS.MediaConvert.Types.H265TemporalIds
import Network.AWS.MediaConvert.Types.H265Tiles
import Network.AWS.MediaConvert.Types.H265UnregisteredSeiTimecode
import Network.AWS.MediaConvert.Types.H265WriteMp4PackagingType
import Network.AWS.Prelude

-- | Settings for H265 codec
--
-- /See:/ 'h265Settings' smart constructor.
data H265Settings = H265Settings'
  { _hsUnregisteredSeiTimecode ::
      !(Maybe H265UnregisteredSeiTimecode),
    _hsQualityTuningLevel :: !(Maybe H265QualityTuningLevel),
    _hsTemporalAdaptiveQuantization ::
      !(Maybe H265TemporalAdaptiveQuantization),
    _hsSceneChangeDetect :: !(Maybe H265SceneChangeDetect),
    _hsHrdBufferInitialFillPercentage :: !(Maybe Nat),
    _hsTiles :: !(Maybe H265Tiles),
    _hsSlowPal :: !(Maybe H265SlowPal),
    _hsTemporalIds :: !(Maybe H265TemporalIds),
    _hsParNumerator :: !(Maybe Nat),
    _hsGopSize :: !(Maybe Double),
    _hsNumberBFramesBetweenReferenceFrames :: !(Maybe Nat),
    _hsGopSizeUnits :: !(Maybe H265GopSizeUnits),
    _hsHrdBufferSize :: !(Maybe Nat),
    _hsSlices :: !(Maybe Nat),
    _hsAlternateTransferFunctionSei ::
      !(Maybe H265AlternateTransferFunctionSei),
    _hsRateControlMode :: !(Maybe H265RateControlMode),
    _hsNumberReferenceFrames :: !(Maybe Nat),
    _hsTelecine :: !(Maybe H265Telecine),
    _hsDynamicSubGop :: !(Maybe H265DynamicSubGop),
    _hsMinIInterval :: !(Maybe Nat),
    _hsInterlaceMode :: !(Maybe H265InterlaceMode),
    _hsParControl :: !(Maybe H265ParControl),
    _hsFlickerAdaptiveQuantization ::
      !(Maybe H265FlickerAdaptiveQuantization),
    _hsQvbrSettings :: !(Maybe H265QvbrSettings),
    _hsSampleAdaptiveOffsetFilterMode ::
      !(Maybe H265SampleAdaptiveOffsetFilterMode),
    _hsCodecProfile :: !(Maybe H265CodecProfile),
    _hsBitrate :: !(Maybe Nat),
    _hsFramerateDenominator :: !(Maybe Nat),
    _hsFramerateConversionAlgorithm ::
      !(Maybe H265FramerateConversionAlgorithm),
    _hsCodecLevel :: !(Maybe H265CodecLevel),
    _hsFramerateControl :: !(Maybe H265FramerateControl),
    _hsWriteMp4PackagingType :: !(Maybe H265WriteMp4PackagingType),
    _hsAdaptiveQuantization :: !(Maybe H265AdaptiveQuantization),
    _hsFramerateNumerator :: !(Maybe Nat),
    _hsGopBReference :: !(Maybe H265GopBReference),
    _hsMaxBitrate :: !(Maybe Nat),
    _hsGopClosedCadence :: !(Maybe Nat),
    _hsParDenominator :: !(Maybe Nat),
    _hsSpatialAdaptiveQuantization ::
      !(Maybe H265SpatialAdaptiveQuantization)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'H265Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsUnregisteredSeiTimecode' - Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
--
-- * 'hsQualityTuningLevel' - Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
--
-- * 'hsTemporalAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
--
-- * 'hsSceneChangeDetect' - Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
--
-- * 'hsHrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- * 'hsTiles' - Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.
--
-- * 'hsSlowPal' - Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
--
-- * 'hsTemporalIds' - Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.
--
-- * 'hsParNumerator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
--
-- * 'hsGopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- * 'hsNumberBFramesBetweenReferenceFrames' - Number of B-frames between reference frames.
--
-- * 'hsGopSizeUnits' - Indicates if the GOP Size in H265 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
--
-- * 'hsHrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- * 'hsSlices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- * 'hsAlternateTransferFunctionSei' - Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
--
-- * 'hsRateControlMode' - Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
--
-- * 'hsNumberReferenceFrames' - Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- * 'hsTelecine' - This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
--
-- * 'hsDynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
--
-- * 'hsMinIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'hsInterlaceMode' - Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
--
-- * 'hsParControl' - Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
--
-- * 'hsFlickerAdaptiveQuantization' - Enable this setting to have the encoder reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. This setting is disabled by default. Related setting: In addition to enabling this setting, you must also set adaptiveQuantization to a value other than Off (OFF).
--
-- * 'hsQvbrSettings' - Settings for quality-defined variable bitrate encoding with the H.265 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- * 'hsSampleAdaptiveOffsetFilterMode' - Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content
--
-- * 'hsCodecProfile' - Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
--
-- * 'hsBitrate' - Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- * 'hsFramerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'hsFramerateConversionAlgorithm' - Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
--
-- * 'hsCodecLevel' - H.265 Level.
--
-- * 'hsFramerateControl' - If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
--
-- * 'hsWriteMp4PackagingType' - If the location of parameter set NAL units doesn't matter in your workflow, ignore this setting. Use this setting only with CMAF or DASH outputs, or with standalone file outputs in an MPEG-4 container (MP4 outputs). Choose HVC1 to mark your output as HVC1. This makes your output compliant with the following specification: ISO IECJTC1 SC29 N13798 Text ISO/IEC FDIS 14496-15 3rd Edition. For these outputs, the service stores parameter set NAL units in the sample headers but not in the samples directly. For MP4 outputs, when you choose HVC1, your output video might not work properly with some downstream systems and video players. The service defaults to marking your output as HEV1. For these outputs, the service writes parameter set NAL units directly into the samples.
--
-- * 'hsAdaptiveQuantization' - Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Flicker adaptive quantization (flickerAdaptiveQuantization), Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
--
-- * 'hsFramerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'hsGopBReference' - If enable, use reference B frames for GOP structures that have B frames > 1.
--
-- * 'hsMaxBitrate' - Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
--
-- * 'hsGopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- * 'hsParDenominator' - Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
--
-- * 'hsSpatialAdaptiveQuantization' - Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
h265Settings ::
  H265Settings
h265Settings =
  H265Settings'
    { _hsUnregisteredSeiTimecode = Nothing,
      _hsQualityTuningLevel = Nothing,
      _hsTemporalAdaptiveQuantization = Nothing,
      _hsSceneChangeDetect = Nothing,
      _hsHrdBufferInitialFillPercentage = Nothing,
      _hsTiles = Nothing,
      _hsSlowPal = Nothing,
      _hsTemporalIds = Nothing,
      _hsParNumerator = Nothing,
      _hsGopSize = Nothing,
      _hsNumberBFramesBetweenReferenceFrames = Nothing,
      _hsGopSizeUnits = Nothing,
      _hsHrdBufferSize = Nothing,
      _hsSlices = Nothing,
      _hsAlternateTransferFunctionSei = Nothing,
      _hsRateControlMode = Nothing,
      _hsNumberReferenceFrames = Nothing,
      _hsTelecine = Nothing,
      _hsDynamicSubGop = Nothing,
      _hsMinIInterval = Nothing,
      _hsInterlaceMode = Nothing,
      _hsParControl = Nothing,
      _hsFlickerAdaptiveQuantization = Nothing,
      _hsQvbrSettings = Nothing,
      _hsSampleAdaptiveOffsetFilterMode = Nothing,
      _hsCodecProfile = Nothing,
      _hsBitrate = Nothing,
      _hsFramerateDenominator = Nothing,
      _hsFramerateConversionAlgorithm = Nothing,
      _hsCodecLevel = Nothing,
      _hsFramerateControl = Nothing,
      _hsWriteMp4PackagingType = Nothing,
      _hsAdaptiveQuantization = Nothing,
      _hsFramerateNumerator = Nothing,
      _hsGopBReference = Nothing,
      _hsMaxBitrate = Nothing,
      _hsGopClosedCadence = Nothing,
      _hsParDenominator = Nothing,
      _hsSpatialAdaptiveQuantization = Nothing
    }

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
hsUnregisteredSeiTimecode :: Lens' H265Settings (Maybe H265UnregisteredSeiTimecode)
hsUnregisteredSeiTimecode = lens _hsUnregisteredSeiTimecode (\s a -> s {_hsUnregisteredSeiTimecode = a})

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
hsQualityTuningLevel :: Lens' H265Settings (Maybe H265QualityTuningLevel)
hsQualityTuningLevel = lens _hsQualityTuningLevel (\s a -> s {_hsQualityTuningLevel = a})

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
hsTemporalAdaptiveQuantization :: Lens' H265Settings (Maybe H265TemporalAdaptiveQuantization)
hsTemporalAdaptiveQuantization = lens _hsTemporalAdaptiveQuantization (\s a -> s {_hsTemporalAdaptiveQuantization = a})

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
hsSceneChangeDetect :: Lens' H265Settings (Maybe H265SceneChangeDetect)
hsSceneChangeDetect = lens _hsSceneChangeDetect (\s a -> s {_hsSceneChangeDetect = a})

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
hsHrdBufferInitialFillPercentage :: Lens' H265Settings (Maybe Natural)
hsHrdBufferInitialFillPercentage = lens _hsHrdBufferInitialFillPercentage (\s a -> s {_hsHrdBufferInitialFillPercentage = a}) . mapping _Nat

-- | Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.
hsTiles :: Lens' H265Settings (Maybe H265Tiles)
hsTiles = lens _hsTiles (\s a -> s {_hsTiles = a})

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
hsSlowPal :: Lens' H265Settings (Maybe H265SlowPal)
hsSlowPal = lens _hsSlowPal (\s a -> s {_hsSlowPal = a})

-- | Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.
hsTemporalIds :: Lens' H265Settings (Maybe H265TemporalIds)
hsTemporalIds = lens _hsTemporalIds (\s a -> s {_hsTemporalIds = a})

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parNumerator is 40.
hsParNumerator :: Lens' H265Settings (Maybe Natural)
hsParNumerator = lens _hsParNumerator (\s a -> s {_hsParNumerator = a}) . mapping _Nat

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
hsGopSize :: Lens' H265Settings (Maybe Double)
hsGopSize = lens _hsGopSize (\s a -> s {_hsGopSize = a})

-- | Number of B-frames between reference frames.
hsNumberBFramesBetweenReferenceFrames :: Lens' H265Settings (Maybe Natural)
hsNumberBFramesBetweenReferenceFrames = lens _hsNumberBFramesBetweenReferenceFrames (\s a -> s {_hsNumberBFramesBetweenReferenceFrames = a}) . mapping _Nat

-- | Indicates if the GOP Size in H265 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
hsGopSizeUnits :: Lens' H265Settings (Maybe H265GopSizeUnits)
hsGopSizeUnits = lens _hsGopSizeUnits (\s a -> s {_hsGopSizeUnits = a})

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
hsHrdBufferSize :: Lens' H265Settings (Maybe Natural)
hsHrdBufferSize = lens _hsHrdBufferSize (\s a -> s {_hsHrdBufferSize = a}) . mapping _Nat

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
hsSlices :: Lens' H265Settings (Maybe Natural)
hsSlices = lens _hsSlices (\s a -> s {_hsSlices = a}) . mapping _Nat

-- | Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
hsAlternateTransferFunctionSei :: Lens' H265Settings (Maybe H265AlternateTransferFunctionSei)
hsAlternateTransferFunctionSei = lens _hsAlternateTransferFunctionSei (\s a -> s {_hsAlternateTransferFunctionSei = a})

-- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
hsRateControlMode :: Lens' H265Settings (Maybe H265RateControlMode)
hsRateControlMode = lens _hsRateControlMode (\s a -> s {_hsRateControlMode = a})

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
hsNumberReferenceFrames :: Lens' H265Settings (Maybe Natural)
hsNumberReferenceFrames = lens _hsNumberReferenceFrames (\s a -> s {_hsNumberReferenceFrames = a}) . mapping _Nat

-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
hsTelecine :: Lens' H265Settings (Maybe H265Telecine)
hsTelecine = lens _hsTelecine (\s a -> s {_hsTelecine = a})

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
hsDynamicSubGop :: Lens' H265Settings (Maybe H265DynamicSubGop)
hsDynamicSubGop = lens _hsDynamicSubGop (\s a -> s {_hsDynamicSubGop = a})

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
hsMinIInterval :: Lens' H265Settings (Maybe Natural)
hsMinIInterval = lens _hsMinIInterval (\s a -> s {_hsMinIInterval = a}) . mapping _Nat

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
hsInterlaceMode :: Lens' H265Settings (Maybe H265InterlaceMode)
hsInterlaceMode = lens _hsInterlaceMode (\s a -> s {_hsInterlaceMode = a})

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
hsParControl :: Lens' H265Settings (Maybe H265ParControl)
hsParControl = lens _hsParControl (\s a -> s {_hsParControl = a})

-- | Enable this setting to have the encoder reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. This setting is disabled by default. Related setting: In addition to enabling this setting, you must also set adaptiveQuantization to a value other than Off (OFF).
hsFlickerAdaptiveQuantization :: Lens' H265Settings (Maybe H265FlickerAdaptiveQuantization)
hsFlickerAdaptiveQuantization = lens _hsFlickerAdaptiveQuantization (\s a -> s {_hsFlickerAdaptiveQuantization = a})

-- | Settings for quality-defined variable bitrate encoding with the H.265 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
hsQvbrSettings :: Lens' H265Settings (Maybe H265QvbrSettings)
hsQvbrSettings = lens _hsQvbrSettings (\s a -> s {_hsQvbrSettings = a})

-- | Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content
hsSampleAdaptiveOffsetFilterMode :: Lens' H265Settings (Maybe H265SampleAdaptiveOffsetFilterMode)
hsSampleAdaptiveOffsetFilterMode = lens _hsSampleAdaptiveOffsetFilterMode (\s a -> s {_hsSampleAdaptiveOffsetFilterMode = a})

-- | Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
hsCodecProfile :: Lens' H265Settings (Maybe H265CodecProfile)
hsCodecProfile = lens _hsCodecProfile (\s a -> s {_hsCodecProfile = a})

-- | Specify the average bitrate in bits per second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
hsBitrate :: Lens' H265Settings (Maybe Natural)
hsBitrate = lens _hsBitrate (\s a -> s {_hsBitrate = a}) . mapping _Nat

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
hsFramerateDenominator :: Lens' H265Settings (Maybe Natural)
hsFramerateDenominator = lens _hsFramerateDenominator (\s a -> s {_hsFramerateDenominator = a}) . mapping _Nat

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
hsFramerateConversionAlgorithm :: Lens' H265Settings (Maybe H265FramerateConversionAlgorithm)
hsFramerateConversionAlgorithm = lens _hsFramerateConversionAlgorithm (\s a -> s {_hsFramerateConversionAlgorithm = a})

-- | H.265 Level.
hsCodecLevel :: Lens' H265Settings (Maybe H265CodecLevel)
hsCodecLevel = lens _hsCodecLevel (\s a -> s {_hsCodecLevel = a})

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
hsFramerateControl :: Lens' H265Settings (Maybe H265FramerateControl)
hsFramerateControl = lens _hsFramerateControl (\s a -> s {_hsFramerateControl = a})

-- | If the location of parameter set NAL units doesn't matter in your workflow, ignore this setting. Use this setting only with CMAF or DASH outputs, or with standalone file outputs in an MPEG-4 container (MP4 outputs). Choose HVC1 to mark your output as HVC1. This makes your output compliant with the following specification: ISO IECJTC1 SC29 N13798 Text ISO/IEC FDIS 14496-15 3rd Edition. For these outputs, the service stores parameter set NAL units in the sample headers but not in the samples directly. For MP4 outputs, when you choose HVC1, your output video might not work properly with some downstream systems and video players. The service defaults to marking your output as HEV1. For these outputs, the service writes parameter set NAL units directly into the samples.
hsWriteMp4PackagingType :: Lens' H265Settings (Maybe H265WriteMp4PackagingType)
hsWriteMp4PackagingType = lens _hsWriteMp4PackagingType (\s a -> s {_hsWriteMp4PackagingType = a})

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Flicker adaptive quantization (flickerAdaptiveQuantization), Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
hsAdaptiveQuantization :: Lens' H265Settings (Maybe H265AdaptiveQuantization)
hsAdaptiveQuantization = lens _hsAdaptiveQuantization (\s a -> s {_hsAdaptiveQuantization = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
hsFramerateNumerator :: Lens' H265Settings (Maybe Natural)
hsFramerateNumerator = lens _hsFramerateNumerator (\s a -> s {_hsFramerateNumerator = a}) . mapping _Nat

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
hsGopBReference :: Lens' H265Settings (Maybe H265GopBReference)
hsGopBReference = lens _hsGopBReference (\s a -> s {_hsGopBReference = a})

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
hsMaxBitrate :: Lens' H265Settings (Maybe Natural)
hsMaxBitrate = lens _hsMaxBitrate (\s a -> s {_hsMaxBitrate = a}) . mapping _Nat

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
hsGopClosedCadence :: Lens' H265Settings (Maybe Natural)
hsGopClosedCadence = lens _hsGopClosedCadence (\s a -> s {_hsGopClosedCadence = a}) . mapping _Nat

-- | Required when you set Pixel aspect ratio (parControl) to SPECIFIED. On the console, this corresponds to any value other than Follow source. When you specify an output pixel aspect ratio (PAR) that is different from your input video PAR, provide your output PAR as a ratio. For example, for D1/DV NTSC widescreen, you would specify the ratio 40:33. In this example, the value for parDenominator is 33.
hsParDenominator :: Lens' H265Settings (Maybe Natural)
hsParDenominator = lens _hsParDenominator (\s a -> s {_hsParDenominator = a}) . mapping _Nat

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
hsSpatialAdaptiveQuantization :: Lens' H265Settings (Maybe H265SpatialAdaptiveQuantization)
hsSpatialAdaptiveQuantization = lens _hsSpatialAdaptiveQuantization (\s a -> s {_hsSpatialAdaptiveQuantization = a})

instance FromJSON H265Settings where
  parseJSON =
    withObject
      "H265Settings"
      ( \x ->
          H265Settings'
            <$> (x .:? "unregisteredSeiTimecode")
            <*> (x .:? "qualityTuningLevel")
            <*> (x .:? "temporalAdaptiveQuantization")
            <*> (x .:? "sceneChangeDetect")
            <*> (x .:? "hrdBufferInitialFillPercentage")
            <*> (x .:? "tiles")
            <*> (x .:? "slowPal")
            <*> (x .:? "temporalIds")
            <*> (x .:? "parNumerator")
            <*> (x .:? "gopSize")
            <*> (x .:? "numberBFramesBetweenReferenceFrames")
            <*> (x .:? "gopSizeUnits")
            <*> (x .:? "hrdBufferSize")
            <*> (x .:? "slices")
            <*> (x .:? "alternateTransferFunctionSei")
            <*> (x .:? "rateControlMode")
            <*> (x .:? "numberReferenceFrames")
            <*> (x .:? "telecine")
            <*> (x .:? "dynamicSubGop")
            <*> (x .:? "minIInterval")
            <*> (x .:? "interlaceMode")
            <*> (x .:? "parControl")
            <*> (x .:? "flickerAdaptiveQuantization")
            <*> (x .:? "qvbrSettings")
            <*> (x .:? "sampleAdaptiveOffsetFilterMode")
            <*> (x .:? "codecProfile")
            <*> (x .:? "bitrate")
            <*> (x .:? "framerateDenominator")
            <*> (x .:? "framerateConversionAlgorithm")
            <*> (x .:? "codecLevel")
            <*> (x .:? "framerateControl")
            <*> (x .:? "writeMp4PackagingType")
            <*> (x .:? "adaptiveQuantization")
            <*> (x .:? "framerateNumerator")
            <*> (x .:? "gopBReference")
            <*> (x .:? "maxBitrate")
            <*> (x .:? "gopClosedCadence")
            <*> (x .:? "parDenominator")
            <*> (x .:? "spatialAdaptiveQuantization")
      )

instance Hashable H265Settings

instance NFData H265Settings

instance ToJSON H265Settings where
  toJSON H265Settings' {..} =
    object
      ( catMaybes
          [ ("unregisteredSeiTimecode" .=) <$> _hsUnregisteredSeiTimecode,
            ("qualityTuningLevel" .=) <$> _hsQualityTuningLevel,
            ("temporalAdaptiveQuantization" .=)
              <$> _hsTemporalAdaptiveQuantization,
            ("sceneChangeDetect" .=) <$> _hsSceneChangeDetect,
            ("hrdBufferInitialFillPercentage" .=)
              <$> _hsHrdBufferInitialFillPercentage,
            ("tiles" .=) <$> _hsTiles,
            ("slowPal" .=) <$> _hsSlowPal,
            ("temporalIds" .=) <$> _hsTemporalIds,
            ("parNumerator" .=) <$> _hsParNumerator,
            ("gopSize" .=) <$> _hsGopSize,
            ("numberBFramesBetweenReferenceFrames" .=)
              <$> _hsNumberBFramesBetweenReferenceFrames,
            ("gopSizeUnits" .=) <$> _hsGopSizeUnits,
            ("hrdBufferSize" .=) <$> _hsHrdBufferSize,
            ("slices" .=) <$> _hsSlices,
            ("alternateTransferFunctionSei" .=)
              <$> _hsAlternateTransferFunctionSei,
            ("rateControlMode" .=) <$> _hsRateControlMode,
            ("numberReferenceFrames" .=) <$> _hsNumberReferenceFrames,
            ("telecine" .=) <$> _hsTelecine,
            ("dynamicSubGop" .=) <$> _hsDynamicSubGop,
            ("minIInterval" .=) <$> _hsMinIInterval,
            ("interlaceMode" .=) <$> _hsInterlaceMode,
            ("parControl" .=) <$> _hsParControl,
            ("flickerAdaptiveQuantization" .=)
              <$> _hsFlickerAdaptiveQuantization,
            ("qvbrSettings" .=) <$> _hsQvbrSettings,
            ("sampleAdaptiveOffsetFilterMode" .=)
              <$> _hsSampleAdaptiveOffsetFilterMode,
            ("codecProfile" .=) <$> _hsCodecProfile,
            ("bitrate" .=) <$> _hsBitrate,
            ("framerateDenominator" .=) <$> _hsFramerateDenominator,
            ("framerateConversionAlgorithm" .=)
              <$> _hsFramerateConversionAlgorithm,
            ("codecLevel" .=) <$> _hsCodecLevel,
            ("framerateControl" .=) <$> _hsFramerateControl,
            ("writeMp4PackagingType" .=) <$> _hsWriteMp4PackagingType,
            ("adaptiveQuantization" .=) <$> _hsAdaptiveQuantization,
            ("framerateNumerator" .=) <$> _hsFramerateNumerator,
            ("gopBReference" .=) <$> _hsGopBReference,
            ("maxBitrate" .=) <$> _hsMaxBitrate,
            ("gopClosedCadence" .=) <$> _hsGopClosedCadence,
            ("parDenominator" .=) <$> _hsParDenominator,
            ("spatialAdaptiveQuantization" .=)
              <$> _hsSpatialAdaptiveQuantization
          ]
      )
