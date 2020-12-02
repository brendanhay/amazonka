{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264Settings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AfdSignaling
import Network.AWS.MediaLive.Types.FixedAfd
import Network.AWS.MediaLive.Types.H264AdaptiveQuantization
import Network.AWS.MediaLive.Types.H264ColorMetadata
import Network.AWS.MediaLive.Types.H264ColorSpaceSettings
import Network.AWS.MediaLive.Types.H264EntropyEncoding
import Network.AWS.MediaLive.Types.H264FilterSettings
import Network.AWS.MediaLive.Types.H264FlickerAq
import Network.AWS.MediaLive.Types.H264ForceFieldPictures
import Network.AWS.MediaLive.Types.H264FramerateControl
import Network.AWS.MediaLive.Types.H264GopBReference
import Network.AWS.MediaLive.Types.H264GopSizeUnits
import Network.AWS.MediaLive.Types.H264Level
import Network.AWS.MediaLive.Types.H264LookAheadRateControl
import Network.AWS.MediaLive.Types.H264ParControl
import Network.AWS.MediaLive.Types.H264Profile
import Network.AWS.MediaLive.Types.H264QualityLevel
import Network.AWS.MediaLive.Types.H264RateControlMode
import Network.AWS.MediaLive.Types.H264ScanType
import Network.AWS.MediaLive.Types.H264SceneChangeDetect
import Network.AWS.MediaLive.Types.H264SpatialAq
import Network.AWS.MediaLive.Types.H264SubGopLength
import Network.AWS.MediaLive.Types.H264Syntax
import Network.AWS.MediaLive.Types.H264TemporalAq
import Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior
import Network.AWS.Prelude

-- | H264 Settings
--
-- /See:/ 'h264Settings' smart constructor.
data H264Settings = H264Settings'
  { _hssTemporalAq ::
      !(Maybe H264TemporalAq),
    _hssSceneChangeDetect :: !(Maybe H264SceneChangeDetect),
    _hssScanType :: !(Maybe H264ScanType),
    _hssTimecodeInsertion :: !(Maybe H264TimecodeInsertionBehavior),
    _hssParNumerator :: !(Maybe Nat),
    _hssAfdSignaling :: !(Maybe AfdSignaling),
    _hssGopSize :: !(Maybe Double),
    _hssGopSizeUnits :: !(Maybe H264GopSizeUnits),
    _hssSubgopLength :: !(Maybe H264SubGopLength),
    _hssQualityLevel :: !(Maybe H264QualityLevel),
    _hssSlices :: !(Maybe Nat),
    _hssProfile :: !(Maybe H264Profile),
    _hssRateControlMode :: !(Maybe H264RateControlMode),
    _hssMinIInterval :: !(Maybe Nat),
    _hssQvbrQualityLevel :: !(Maybe Nat),
    _hssColorSpaceSettings :: !(Maybe H264ColorSpaceSettings),
    _hssParControl :: !(Maybe H264ParControl),
    _hssFlickerAq :: !(Maybe H264FlickerAq),
    _hssBufSize :: !(Maybe Nat),
    _hssSpatialAq :: !(Maybe H264SpatialAq),
    _hssGopNumBFrames :: !(Maybe Nat),
    _hssFixedAfd :: !(Maybe FixedAfd),
    _hssSoftness :: !(Maybe Nat),
    _hssFilterSettings :: !(Maybe H264FilterSettings),
    _hssBitrate :: !(Maybe Nat),
    _hssFramerateDenominator :: !(Maybe Nat),
    _hssForceFieldPictures :: !(Maybe H264ForceFieldPictures),
    _hssEntropyEncoding :: !(Maybe H264EntropyEncoding),
    _hssFramerateControl :: !(Maybe H264FramerateControl),
    _hssColorMetadata :: !(Maybe H264ColorMetadata),
    _hssLookAheadRateControl :: !(Maybe H264LookAheadRateControl),
    _hssAdaptiveQuantization :: !(Maybe H264AdaptiveQuantization),
    _hssFramerateNumerator :: !(Maybe Nat),
    _hssLevel :: !(Maybe H264Level),
    _hssGopBReference :: !(Maybe H264GopBReference),
    _hssMaxBitrate :: !(Maybe Nat),
    _hssSyntax :: !(Maybe H264Syntax),
    _hssBufFillPct :: !(Maybe Nat),
    _hssGopClosedCadence :: !(Maybe Nat),
    _hssNumRefFrames :: !(Maybe Nat),
    _hssParDenominator :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'H264Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hssTemporalAq' - If set to enabled, adjust quantization within each frame based on temporal variation of content complexity.
--
-- * 'hssSceneChangeDetect' - Scene change detection. - On: inserts I-frames when scene change is detected. - Off: does not force an I-frame when scene change is detected.
--
-- * 'hssScanType' - Sets the scan type of the output to progressive or top-field-first interlaced.
--
-- * 'hssTimecodeInsertion' - Determines how timecodes should be inserted into the video elementary stream. - 'disabled': Do not include timecodes - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
--
-- * 'hssParNumerator' - Pixel Aspect Ratio numerator.
--
-- * 'hssAfdSignaling' - Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
--
-- * 'hssGopSize' - GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
--
-- * 'hssGopSizeUnits' - Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
--
-- * 'hssSubgopLength' - If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to dynamic, optimize the number of B-frames used for each sub-GOP to improve visual quality.
--
-- * 'hssQualityLevel' - Leave as STANDARD_QUALITY or choose a different value (which might result in additional costs to run the channel). - ENHANCED_QUALITY: Produces a slightly better video quality without an increase in the bitrate. Has an effect only when the Rate control mode is QVBR or CBR. If this channel is in a MediaLive multiplex, the value must be ENHANCED_QUALITY. - STANDARD_QUALITY: Valid for any Rate control mode.
--
-- * 'hssSlices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures. This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
--
-- * 'hssProfile' - H.264 Profile.
--
-- * 'hssRateControlMode' - Rate control mode. QVBR: Quality will match the specified quality level except when it is constrained by the maximum bitrate.  Recommended if you or your viewers pay for bandwidth. VBR: Quality and bitrate vary, depending on the video complexity. Recommended instead of QVBR if you want to maintain a specific average bitrate over the duration of the channel. CBR: Quality varies, depending on the video complexity. Recommended only if you distribute your assets to devices that cannot handle variable bitrates. Multiplex: This rate control mode is only supported (and is required) when the video is being delivered to a MediaLive Multiplex in which case the rate control configuration is controlled by the properties within the Multiplex Program.
--
-- * 'hssMinIInterval' - Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'hssQvbrQualityLevel' - Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are: - Primary screen: Quality level: 8 to 10. Max bitrate: 4M - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
--
-- * 'hssColorSpaceSettings' - Color Space settings
--
-- * 'hssParControl' - This field indicates how the output pixel aspect ratio is specified.  If "specified" is selected then the output video pixel aspect ratio is determined by parNumerator and parDenominator, else if "initializeFromSource" is selected then the output pixsel aspect ratio will be set equal to the input video pixel aspect ratio of the first input.
--
-- * 'hssFlickerAq' - If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
--
-- * 'hssBufSize' - Size of buffer (HRD buffer model) in bits.
--
-- * 'hssSpatialAq' - If set to enabled, adjust quantization within each frame based on spatial variation of content complexity.
--
-- * 'hssGopNumBFrames' - Number of B-frames between reference frames.
--
-- * 'hssFixedAfd' - Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
--
-- * 'hssSoftness' - Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
--
-- * 'hssFilterSettings' - Optional filters that you can apply to an encode.
--
-- * 'hssBitrate' - Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
--
-- * 'hssFramerateDenominator' - Framerate denominator.
--
-- * 'hssForceFieldPictures' - This setting applies only when scan type is "interlaced." It controls whether coding is performed on a field basis or on a frame basis. (When the video is progressive, the coding is always performed on a frame basis.) enabled: Force MediaLive to code on a field basis, so that odd and even sets of fields are coded separately. disabled: Code the two sets of fields separately (on a field basis) or together (on a frame basis using PAFF), depending on what is most appropriate for the content.
--
-- * 'hssEntropyEncoding' - Entropy encoding mode.  Use cabac (must be in Main or High profile) or cavlc.
--
-- * 'hssFramerateControl' - This field indicates how the output video frame rate is specified.  If "specified" is selected then the output video frame rate is determined by framerateNumerator and framerateDenominator, else if "initializeFromSource" is selected then the output video frame rate will be set equal to the input video frame rate of the first input.
--
-- * 'hssColorMetadata' - Includes colorspace metadata in the output.
--
-- * 'hssLookAheadRateControl' - Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
--
-- * 'hssAdaptiveQuantization' - Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
--
-- * 'hssFramerateNumerator' - Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- * 'hssLevel' - H.264 Level.
--
-- * 'hssGopBReference' - Documentation update needed
--
-- * 'hssMaxBitrate' - For QVBR: See the tooltip for Quality level For VBR: Set the maximum bitrate in order to accommodate expected spikes in the complexity of the video.
--
-- * 'hssSyntax' - Produces a bitstream compliant with SMPTE RP-2027.
--
-- * 'hssBufFillPct' - Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- * 'hssGopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- * 'hssNumRefFrames' - Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- * 'hssParDenominator' - Pixel Aspect Ratio denominator.
h264Settings ::
  H264Settings
h264Settings =
  H264Settings'
    { _hssTemporalAq = Nothing,
      _hssSceneChangeDetect = Nothing,
      _hssScanType = Nothing,
      _hssTimecodeInsertion = Nothing,
      _hssParNumerator = Nothing,
      _hssAfdSignaling = Nothing,
      _hssGopSize = Nothing,
      _hssGopSizeUnits = Nothing,
      _hssSubgopLength = Nothing,
      _hssQualityLevel = Nothing,
      _hssSlices = Nothing,
      _hssProfile = Nothing,
      _hssRateControlMode = Nothing,
      _hssMinIInterval = Nothing,
      _hssQvbrQualityLevel = Nothing,
      _hssColorSpaceSettings = Nothing,
      _hssParControl = Nothing,
      _hssFlickerAq = Nothing,
      _hssBufSize = Nothing,
      _hssSpatialAq = Nothing,
      _hssGopNumBFrames = Nothing,
      _hssFixedAfd = Nothing,
      _hssSoftness = Nothing,
      _hssFilterSettings = Nothing,
      _hssBitrate = Nothing,
      _hssFramerateDenominator = Nothing,
      _hssForceFieldPictures = Nothing,
      _hssEntropyEncoding = Nothing,
      _hssFramerateControl = Nothing,
      _hssColorMetadata = Nothing,
      _hssLookAheadRateControl = Nothing,
      _hssAdaptiveQuantization = Nothing,
      _hssFramerateNumerator = Nothing,
      _hssLevel = Nothing,
      _hssGopBReference = Nothing,
      _hssMaxBitrate = Nothing,
      _hssSyntax = Nothing,
      _hssBufFillPct = Nothing,
      _hssGopClosedCadence = Nothing,
      _hssNumRefFrames = Nothing,
      _hssParDenominator = Nothing
    }

-- | If set to enabled, adjust quantization within each frame based on temporal variation of content complexity.
hssTemporalAq :: Lens' H264Settings (Maybe H264TemporalAq)
hssTemporalAq = lens _hssTemporalAq (\s a -> s {_hssTemporalAq = a})

-- | Scene change detection. - On: inserts I-frames when scene change is detected. - Off: does not force an I-frame when scene change is detected.
hssSceneChangeDetect :: Lens' H264Settings (Maybe H264SceneChangeDetect)
hssSceneChangeDetect = lens _hssSceneChangeDetect (\s a -> s {_hssSceneChangeDetect = a})

-- | Sets the scan type of the output to progressive or top-field-first interlaced.
hssScanType :: Lens' H264Settings (Maybe H264ScanType)
hssScanType = lens _hssScanType (\s a -> s {_hssScanType = a})

-- | Determines how timecodes should be inserted into the video elementary stream. - 'disabled': Do not include timecodes - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
hssTimecodeInsertion :: Lens' H264Settings (Maybe H264TimecodeInsertionBehavior)
hssTimecodeInsertion = lens _hssTimecodeInsertion (\s a -> s {_hssTimecodeInsertion = a})

-- | Pixel Aspect Ratio numerator.
hssParNumerator :: Lens' H264Settings (Maybe Natural)
hssParNumerator = lens _hssParNumerator (\s a -> s {_hssParNumerator = a}) . mapping _Nat

-- | Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
hssAfdSignaling :: Lens' H264Settings (Maybe AfdSignaling)
hssAfdSignaling = lens _hssAfdSignaling (\s a -> s {_hssAfdSignaling = a})

-- | GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
hssGopSize :: Lens' H264Settings (Maybe Double)
hssGopSize = lens _hssGopSize (\s a -> s {_hssGopSize = a})

-- | Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
hssGopSizeUnits :: Lens' H264Settings (Maybe H264GopSizeUnits)
hssGopSizeUnits = lens _hssGopSizeUnits (\s a -> s {_hssGopSizeUnits = a})

-- | If set to fixed, use gopNumBFrames B-frames per sub-GOP. If set to dynamic, optimize the number of B-frames used for each sub-GOP to improve visual quality.
hssSubgopLength :: Lens' H264Settings (Maybe H264SubGopLength)
hssSubgopLength = lens _hssSubgopLength (\s a -> s {_hssSubgopLength = a})

-- | Leave as STANDARD_QUALITY or choose a different value (which might result in additional costs to run the channel). - ENHANCED_QUALITY: Produces a slightly better video quality without an increase in the bitrate. Has an effect only when the Rate control mode is QVBR or CBR. If this channel is in a MediaLive multiplex, the value must be ENHANCED_QUALITY. - STANDARD_QUALITY: Valid for any Rate control mode.
hssQualityLevel :: Lens' H264Settings (Maybe H264QualityLevel)
hssQualityLevel = lens _hssQualityLevel (\s a -> s {_hssQualityLevel = a})

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures. This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
hssSlices :: Lens' H264Settings (Maybe Natural)
hssSlices = lens _hssSlices (\s a -> s {_hssSlices = a}) . mapping _Nat

-- | H.264 Profile.
hssProfile :: Lens' H264Settings (Maybe H264Profile)
hssProfile = lens _hssProfile (\s a -> s {_hssProfile = a})

-- | Rate control mode. QVBR: Quality will match the specified quality level except when it is constrained by the maximum bitrate.  Recommended if you or your viewers pay for bandwidth. VBR: Quality and bitrate vary, depending on the video complexity. Recommended instead of QVBR if you want to maintain a specific average bitrate over the duration of the channel. CBR: Quality varies, depending on the video complexity. Recommended only if you distribute your assets to devices that cannot handle variable bitrates. Multiplex: This rate control mode is only supported (and is required) when the video is being delivered to a MediaLive Multiplex in which case the rate control configuration is controlled by the properties within the Multiplex Program.
hssRateControlMode :: Lens' H264Settings (Maybe H264RateControlMode)
hssRateControlMode = lens _hssRateControlMode (\s a -> s {_hssRateControlMode = a})

-- | Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
hssMinIInterval :: Lens' H264Settings (Maybe Natural)
hssMinIInterval = lens _hssMinIInterval (\s a -> s {_hssMinIInterval = a}) . mapping _Nat

-- | Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are: - Primary screen: Quality level: 8 to 10. Max bitrate: 4M - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
hssQvbrQualityLevel :: Lens' H264Settings (Maybe Natural)
hssQvbrQualityLevel = lens _hssQvbrQualityLevel (\s a -> s {_hssQvbrQualityLevel = a}) . mapping _Nat

-- | Color Space settings
hssColorSpaceSettings :: Lens' H264Settings (Maybe H264ColorSpaceSettings)
hssColorSpaceSettings = lens _hssColorSpaceSettings (\s a -> s {_hssColorSpaceSettings = a})

-- | This field indicates how the output pixel aspect ratio is specified.  If "specified" is selected then the output video pixel aspect ratio is determined by parNumerator and parDenominator, else if "initializeFromSource" is selected then the output pixsel aspect ratio will be set equal to the input video pixel aspect ratio of the first input.
hssParControl :: Lens' H264Settings (Maybe H264ParControl)
hssParControl = lens _hssParControl (\s a -> s {_hssParControl = a})

-- | If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
hssFlickerAq :: Lens' H264Settings (Maybe H264FlickerAq)
hssFlickerAq = lens _hssFlickerAq (\s a -> s {_hssFlickerAq = a})

-- | Size of buffer (HRD buffer model) in bits.
hssBufSize :: Lens' H264Settings (Maybe Natural)
hssBufSize = lens _hssBufSize (\s a -> s {_hssBufSize = a}) . mapping _Nat

-- | If set to enabled, adjust quantization within each frame based on spatial variation of content complexity.
hssSpatialAq :: Lens' H264Settings (Maybe H264SpatialAq)
hssSpatialAq = lens _hssSpatialAq (\s a -> s {_hssSpatialAq = a})

-- | Number of B-frames between reference frames.
hssGopNumBFrames :: Lens' H264Settings (Maybe Natural)
hssGopNumBFrames = lens _hssGopNumBFrames (\s a -> s {_hssGopNumBFrames = a}) . mapping _Nat

-- | Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
hssFixedAfd :: Lens' H264Settings (Maybe FixedAfd)
hssFixedAfd = lens _hssFixedAfd (\s a -> s {_hssFixedAfd = a})

-- | Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
hssSoftness :: Lens' H264Settings (Maybe Natural)
hssSoftness = lens _hssSoftness (\s a -> s {_hssSoftness = a}) . mapping _Nat

-- | Optional filters that you can apply to an encode.
hssFilterSettings :: Lens' H264Settings (Maybe H264FilterSettings)
hssFilterSettings = lens _hssFilterSettings (\s a -> s {_hssFilterSettings = a})

-- | Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
hssBitrate :: Lens' H264Settings (Maybe Natural)
hssBitrate = lens _hssBitrate (\s a -> s {_hssBitrate = a}) . mapping _Nat

-- | Framerate denominator.
hssFramerateDenominator :: Lens' H264Settings (Maybe Natural)
hssFramerateDenominator = lens _hssFramerateDenominator (\s a -> s {_hssFramerateDenominator = a}) . mapping _Nat

-- | This setting applies only when scan type is "interlaced." It controls whether coding is performed on a field basis or on a frame basis. (When the video is progressive, the coding is always performed on a frame basis.) enabled: Force MediaLive to code on a field basis, so that odd and even sets of fields are coded separately. disabled: Code the two sets of fields separately (on a field basis) or together (on a frame basis using PAFF), depending on what is most appropriate for the content.
hssForceFieldPictures :: Lens' H264Settings (Maybe H264ForceFieldPictures)
hssForceFieldPictures = lens _hssForceFieldPictures (\s a -> s {_hssForceFieldPictures = a})

-- | Entropy encoding mode.  Use cabac (must be in Main or High profile) or cavlc.
hssEntropyEncoding :: Lens' H264Settings (Maybe H264EntropyEncoding)
hssEntropyEncoding = lens _hssEntropyEncoding (\s a -> s {_hssEntropyEncoding = a})

-- | This field indicates how the output video frame rate is specified.  If "specified" is selected then the output video frame rate is determined by framerateNumerator and framerateDenominator, else if "initializeFromSource" is selected then the output video frame rate will be set equal to the input video frame rate of the first input.
hssFramerateControl :: Lens' H264Settings (Maybe H264FramerateControl)
hssFramerateControl = lens _hssFramerateControl (\s a -> s {_hssFramerateControl = a})

-- | Includes colorspace metadata in the output.
hssColorMetadata :: Lens' H264Settings (Maybe H264ColorMetadata)
hssColorMetadata = lens _hssColorMetadata (\s a -> s {_hssColorMetadata = a})

-- | Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
hssLookAheadRateControl :: Lens' H264Settings (Maybe H264LookAheadRateControl)
hssLookAheadRateControl = lens _hssLookAheadRateControl (\s a -> s {_hssLookAheadRateControl = a})

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
hssAdaptiveQuantization :: Lens' H264Settings (Maybe H264AdaptiveQuantization)
hssAdaptiveQuantization = lens _hssAdaptiveQuantization (\s a -> s {_hssAdaptiveQuantization = a})

-- | Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
hssFramerateNumerator :: Lens' H264Settings (Maybe Natural)
hssFramerateNumerator = lens _hssFramerateNumerator (\s a -> s {_hssFramerateNumerator = a}) . mapping _Nat

-- | H.264 Level.
hssLevel :: Lens' H264Settings (Maybe H264Level)
hssLevel = lens _hssLevel (\s a -> s {_hssLevel = a})

-- | Documentation update needed
hssGopBReference :: Lens' H264Settings (Maybe H264GopBReference)
hssGopBReference = lens _hssGopBReference (\s a -> s {_hssGopBReference = a})

-- | For QVBR: See the tooltip for Quality level For VBR: Set the maximum bitrate in order to accommodate expected spikes in the complexity of the video.
hssMaxBitrate :: Lens' H264Settings (Maybe Natural)
hssMaxBitrate = lens _hssMaxBitrate (\s a -> s {_hssMaxBitrate = a}) . mapping _Nat

-- | Produces a bitstream compliant with SMPTE RP-2027.
hssSyntax :: Lens' H264Settings (Maybe H264Syntax)
hssSyntax = lens _hssSyntax (\s a -> s {_hssSyntax = a})

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
hssBufFillPct :: Lens' H264Settings (Maybe Natural)
hssBufFillPct = lens _hssBufFillPct (\s a -> s {_hssBufFillPct = a}) . mapping _Nat

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
hssGopClosedCadence :: Lens' H264Settings (Maybe Natural)
hssGopClosedCadence = lens _hssGopClosedCadence (\s a -> s {_hssGopClosedCadence = a}) . mapping _Nat

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
hssNumRefFrames :: Lens' H264Settings (Maybe Natural)
hssNumRefFrames = lens _hssNumRefFrames (\s a -> s {_hssNumRefFrames = a}) . mapping _Nat

-- | Pixel Aspect Ratio denominator.
hssParDenominator :: Lens' H264Settings (Maybe Natural)
hssParDenominator = lens _hssParDenominator (\s a -> s {_hssParDenominator = a}) . mapping _Nat

instance FromJSON H264Settings where
  parseJSON =
    withObject
      "H264Settings"
      ( \x ->
          H264Settings'
            <$> (x .:? "temporalAq")
            <*> (x .:? "sceneChangeDetect")
            <*> (x .:? "scanType")
            <*> (x .:? "timecodeInsertion")
            <*> (x .:? "parNumerator")
            <*> (x .:? "afdSignaling")
            <*> (x .:? "gopSize")
            <*> (x .:? "gopSizeUnits")
            <*> (x .:? "subgopLength")
            <*> (x .:? "qualityLevel")
            <*> (x .:? "slices")
            <*> (x .:? "profile")
            <*> (x .:? "rateControlMode")
            <*> (x .:? "minIInterval")
            <*> (x .:? "qvbrQualityLevel")
            <*> (x .:? "colorSpaceSettings")
            <*> (x .:? "parControl")
            <*> (x .:? "flickerAq")
            <*> (x .:? "bufSize")
            <*> (x .:? "spatialAq")
            <*> (x .:? "gopNumBFrames")
            <*> (x .:? "fixedAfd")
            <*> (x .:? "softness")
            <*> (x .:? "filterSettings")
            <*> (x .:? "bitrate")
            <*> (x .:? "framerateDenominator")
            <*> (x .:? "forceFieldPictures")
            <*> (x .:? "entropyEncoding")
            <*> (x .:? "framerateControl")
            <*> (x .:? "colorMetadata")
            <*> (x .:? "lookAheadRateControl")
            <*> (x .:? "adaptiveQuantization")
            <*> (x .:? "framerateNumerator")
            <*> (x .:? "level")
            <*> (x .:? "gopBReference")
            <*> (x .:? "maxBitrate")
            <*> (x .:? "syntax")
            <*> (x .:? "bufFillPct")
            <*> (x .:? "gopClosedCadence")
            <*> (x .:? "numRefFrames")
            <*> (x .:? "parDenominator")
      )

instance Hashable H264Settings

instance NFData H264Settings

instance ToJSON H264Settings where
  toJSON H264Settings' {..} =
    object
      ( catMaybes
          [ ("temporalAq" .=) <$> _hssTemporalAq,
            ("sceneChangeDetect" .=) <$> _hssSceneChangeDetect,
            ("scanType" .=) <$> _hssScanType,
            ("timecodeInsertion" .=) <$> _hssTimecodeInsertion,
            ("parNumerator" .=) <$> _hssParNumerator,
            ("afdSignaling" .=) <$> _hssAfdSignaling,
            ("gopSize" .=) <$> _hssGopSize,
            ("gopSizeUnits" .=) <$> _hssGopSizeUnits,
            ("subgopLength" .=) <$> _hssSubgopLength,
            ("qualityLevel" .=) <$> _hssQualityLevel,
            ("slices" .=) <$> _hssSlices,
            ("profile" .=) <$> _hssProfile,
            ("rateControlMode" .=) <$> _hssRateControlMode,
            ("minIInterval" .=) <$> _hssMinIInterval,
            ("qvbrQualityLevel" .=) <$> _hssQvbrQualityLevel,
            ("colorSpaceSettings" .=) <$> _hssColorSpaceSettings,
            ("parControl" .=) <$> _hssParControl,
            ("flickerAq" .=) <$> _hssFlickerAq,
            ("bufSize" .=) <$> _hssBufSize,
            ("spatialAq" .=) <$> _hssSpatialAq,
            ("gopNumBFrames" .=) <$> _hssGopNumBFrames,
            ("fixedAfd" .=) <$> _hssFixedAfd,
            ("softness" .=) <$> _hssSoftness,
            ("filterSettings" .=) <$> _hssFilterSettings,
            ("bitrate" .=) <$> _hssBitrate,
            ("framerateDenominator" .=) <$> _hssFramerateDenominator,
            ("forceFieldPictures" .=) <$> _hssForceFieldPictures,
            ("entropyEncoding" .=) <$> _hssEntropyEncoding,
            ("framerateControl" .=) <$> _hssFramerateControl,
            ("colorMetadata" .=) <$> _hssColorMetadata,
            ("lookAheadRateControl" .=) <$> _hssLookAheadRateControl,
            ("adaptiveQuantization" .=) <$> _hssAdaptiveQuantization,
            ("framerateNumerator" .=) <$> _hssFramerateNumerator,
            ("level" .=) <$> _hssLevel,
            ("gopBReference" .=) <$> _hssGopBReference,
            ("maxBitrate" .=) <$> _hssMaxBitrate,
            ("syntax" .=) <$> _hssSyntax,
            ("bufFillPct" .=) <$> _hssBufFillPct,
            ("gopClosedCadence" .=) <$> _hssGopClosedCadence,
            ("numRefFrames" .=) <$> _hssNumRefFrames,
            ("parDenominator" .=) <$> _hssParDenominator
          ]
      )
