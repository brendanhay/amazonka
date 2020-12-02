{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265Settings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AfdSignaling
import Network.AWS.MediaLive.Types.FixedAfd
import Network.AWS.MediaLive.Types.H265AdaptiveQuantization
import Network.AWS.MediaLive.Types.H265AlternativeTransferFunction
import Network.AWS.MediaLive.Types.H265ColorMetadata
import Network.AWS.MediaLive.Types.H265ColorSpaceSettings
import Network.AWS.MediaLive.Types.H265FilterSettings
import Network.AWS.MediaLive.Types.H265FlickerAq
import Network.AWS.MediaLive.Types.H265GopSizeUnits
import Network.AWS.MediaLive.Types.H265Level
import Network.AWS.MediaLive.Types.H265LookAheadRateControl
import Network.AWS.MediaLive.Types.H265Profile
import Network.AWS.MediaLive.Types.H265RateControlMode
import Network.AWS.MediaLive.Types.H265ScanType
import Network.AWS.MediaLive.Types.H265SceneChangeDetect
import Network.AWS.MediaLive.Types.H265Tier
import Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior
import Network.AWS.Prelude

-- | H265 Settings
--
-- /See:/ 'h265Settings' smart constructor.
data H265Settings = H265Settings'
  { _hsSceneChangeDetect ::
      !(Maybe H265SceneChangeDetect),
    _hsScanType :: !(Maybe H265ScanType),
    _hsTimecodeInsertion :: !(Maybe H265TimecodeInsertionBehavior),
    _hsParNumerator :: !(Maybe Nat),
    _hsAfdSignaling :: !(Maybe AfdSignaling),
    _hsGopSize :: !(Maybe Double),
    _hsGopSizeUnits :: !(Maybe H265GopSizeUnits),
    _hsSlices :: !(Maybe Nat),
    _hsProfile :: !(Maybe H265Profile),
    _hsAlternativeTransferFunction ::
      !(Maybe H265AlternativeTransferFunction),
    _hsRateControlMode :: !(Maybe H265RateControlMode),
    _hsMinIInterval :: !(Maybe Nat),
    _hsQvbrQualityLevel :: !(Maybe Nat),
    _hsColorSpaceSettings :: !(Maybe H265ColorSpaceSettings),
    _hsFlickerAq :: !(Maybe H265FlickerAq),
    _hsBufSize :: !(Maybe Nat),
    _hsTier :: !(Maybe H265Tier),
    _hsFixedAfd :: !(Maybe FixedAfd),
    _hsFilterSettings :: !(Maybe H265FilterSettings),
    _hsBitrate :: !(Maybe Nat),
    _hsColorMetadata :: !(Maybe H265ColorMetadata),
    _hsLookAheadRateControl :: !(Maybe H265LookAheadRateControl),
    _hsAdaptiveQuantization :: !(Maybe H265AdaptiveQuantization),
    _hsLevel :: !(Maybe H265Level),
    _hsMaxBitrate :: !(Maybe Nat),
    _hsGopClosedCadence :: !(Maybe Nat),
    _hsParDenominator :: !(Maybe Nat),
    _hsFramerateNumerator :: !Nat,
    _hsFramerateDenominator :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'H265Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsSceneChangeDetect' - Scene change detection.
--
-- * 'hsScanType' - Sets the scan type of the output to progressive or top-field-first interlaced.
--
-- * 'hsTimecodeInsertion' - Determines how timecodes should be inserted into the video elementary stream. - 'disabled': Do not include timecodes - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
--
-- * 'hsParNumerator' - Pixel Aspect Ratio numerator.
--
-- * 'hsAfdSignaling' - Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
--
-- * 'hsGopSize' - GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
--
-- * 'hsGopSizeUnits' - Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
--
-- * 'hsSlices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures. This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
--
-- * 'hsProfile' - H.265 Profile.
--
-- * 'hsAlternativeTransferFunction' - Whether or not EML should insert an Alternative Transfer Function SEI message to support backwards compatibility with non-HDR decoders and displays.
--
-- * 'hsRateControlMode' - Rate control mode. QVBR: Quality will match the specified quality level except when it is constrained by the maximum bitrate.  Recommended if you or your viewers pay for bandwidth. CBR: Quality varies, depending on the video complexity. Recommended only if you distribute your assets to devices that cannot handle variable bitrates. Multiplex: This rate control mode is only supported (and is required) when the video is being delivered to a MediaLive Multiplex in which case the rate control configuration is controlled by the properties within the Multiplex Program.
--
-- * 'hsMinIInterval' - Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'hsQvbrQualityLevel' - Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are: - Primary screen: Quality level: 8 to 10. Max bitrate: 4M - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
--
-- * 'hsColorSpaceSettings' - Color Space settings
--
-- * 'hsFlickerAq' - If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
--
-- * 'hsBufSize' - Size of buffer (HRD buffer model) in bits.
--
-- * 'hsTier' - H.265 Tier.
--
-- * 'hsFixedAfd' - Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
--
-- * 'hsFilterSettings' - Optional filters that you can apply to an encode.
--
-- * 'hsBitrate' - Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
--
-- * 'hsColorMetadata' - Includes colorspace metadata in the output.
--
-- * 'hsLookAheadRateControl' - Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
--
-- * 'hsAdaptiveQuantization' - Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
--
-- * 'hsLevel' - H.265 Level.
--
-- * 'hsMaxBitrate' - For QVBR: See the tooltip for Quality level
--
-- * 'hsGopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- * 'hsParDenominator' - Pixel Aspect Ratio denominator.
--
-- * 'hsFramerateNumerator' - Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- * 'hsFramerateDenominator' - Framerate denominator.
h265Settings ::
  -- | 'hsFramerateNumerator'
  Natural ->
  -- | 'hsFramerateDenominator'
  Natural ->
  H265Settings
h265Settings pFramerateNumerator_ pFramerateDenominator_ =
  H265Settings'
    { _hsSceneChangeDetect = Nothing,
      _hsScanType = Nothing,
      _hsTimecodeInsertion = Nothing,
      _hsParNumerator = Nothing,
      _hsAfdSignaling = Nothing,
      _hsGopSize = Nothing,
      _hsGopSizeUnits = Nothing,
      _hsSlices = Nothing,
      _hsProfile = Nothing,
      _hsAlternativeTransferFunction = Nothing,
      _hsRateControlMode = Nothing,
      _hsMinIInterval = Nothing,
      _hsQvbrQualityLevel = Nothing,
      _hsColorSpaceSettings = Nothing,
      _hsFlickerAq = Nothing,
      _hsBufSize = Nothing,
      _hsTier = Nothing,
      _hsFixedAfd = Nothing,
      _hsFilterSettings = Nothing,
      _hsBitrate = Nothing,
      _hsColorMetadata = Nothing,
      _hsLookAheadRateControl = Nothing,
      _hsAdaptiveQuantization = Nothing,
      _hsLevel = Nothing,
      _hsMaxBitrate = Nothing,
      _hsGopClosedCadence = Nothing,
      _hsParDenominator = Nothing,
      _hsFramerateNumerator = _Nat # pFramerateNumerator_,
      _hsFramerateDenominator = _Nat # pFramerateDenominator_
    }

-- | Scene change detection.
hsSceneChangeDetect :: Lens' H265Settings (Maybe H265SceneChangeDetect)
hsSceneChangeDetect = lens _hsSceneChangeDetect (\s a -> s {_hsSceneChangeDetect = a})

-- | Sets the scan type of the output to progressive or top-field-first interlaced.
hsScanType :: Lens' H265Settings (Maybe H265ScanType)
hsScanType = lens _hsScanType (\s a -> s {_hsScanType = a})

-- | Determines how timecodes should be inserted into the video elementary stream. - 'disabled': Do not include timecodes - 'picTimingSei': Pass through picture timing SEI messages from the source specified in Timecode Config
hsTimecodeInsertion :: Lens' H265Settings (Maybe H265TimecodeInsertionBehavior)
hsTimecodeInsertion = lens _hsTimecodeInsertion (\s a -> s {_hsTimecodeInsertion = a})

-- | Pixel Aspect Ratio numerator.
hsParNumerator :: Lens' H265Settings (Maybe Natural)
hsParNumerator = lens _hsParNumerator (\s a -> s {_hsParNumerator = a}) . mapping _Nat

-- | Indicates that AFD values will be written into the output stream.  If afdSignaling is "auto", the system will try to preserve the input AFD value (in cases where multiple AFD values are valid). If set to "fixed", the AFD value will be the value configured in the fixedAfd parameter.
hsAfdSignaling :: Lens' H265Settings (Maybe AfdSignaling)
hsAfdSignaling = lens _hsAfdSignaling (\s a -> s {_hsAfdSignaling = a})

-- | GOP size (keyframe interval) in units of either frames or seconds per gopSizeUnits. If gopSizeUnits is frames, gopSize must be an integer and must be greater than or equal to 1. If gopSizeUnits is seconds, gopSize must be greater than 0, but need not be an integer.
hsGopSize :: Lens' H265Settings (Maybe Double)
hsGopSize = lens _hsGopSize (\s a -> s {_hsGopSize = a})

-- | Indicates if the gopSize is specified in frames or seconds. If seconds the system will convert the gopSize into a frame count at run time.
hsGopSizeUnits :: Lens' H265Settings (Maybe H265GopSizeUnits)
hsGopSizeUnits = lens _hsGopSizeUnits (\s a -> s {_hsGopSizeUnits = a})

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures. This field is optional; when no value is specified the encoder will choose the number of slices based on encode resolution.
hsSlices :: Lens' H265Settings (Maybe Natural)
hsSlices = lens _hsSlices (\s a -> s {_hsSlices = a}) . mapping _Nat

-- | H.265 Profile.
hsProfile :: Lens' H265Settings (Maybe H265Profile)
hsProfile = lens _hsProfile (\s a -> s {_hsProfile = a})

-- | Whether or not EML should insert an Alternative Transfer Function SEI message to support backwards compatibility with non-HDR decoders and displays.
hsAlternativeTransferFunction :: Lens' H265Settings (Maybe H265AlternativeTransferFunction)
hsAlternativeTransferFunction = lens _hsAlternativeTransferFunction (\s a -> s {_hsAlternativeTransferFunction = a})

-- | Rate control mode. QVBR: Quality will match the specified quality level except when it is constrained by the maximum bitrate.  Recommended if you or your viewers pay for bandwidth. CBR: Quality varies, depending on the video complexity. Recommended only if you distribute your assets to devices that cannot handle variable bitrates. Multiplex: This rate control mode is only supported (and is required) when the video is being delivered to a MediaLive Multiplex in which case the rate control configuration is controlled by the properties within the Multiplex Program.
hsRateControlMode :: Lens' H265Settings (Maybe H265RateControlMode)
hsRateControlMode = lens _hsRateControlMode (\s a -> s {_hsRateControlMode = a})

-- | Only meaningful if sceneChangeDetect is set to enabled.  Defaults to 5 if multiplex rate control is used.  Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
hsMinIInterval :: Lens' H265Settings (Maybe Natural)
hsMinIInterval = lens _hsMinIInterval (\s a -> s {_hsMinIInterval = a}) . mapping _Nat

-- | Controls the target quality for the video encode. Applies only when the rate control mode is QVBR. Set values for the QVBR quality level field and Max bitrate field that suit your most important viewing devices. Recommended values are: - Primary screen: Quality level: 8 to 10. Max bitrate: 4M - PC or tablet: Quality level: 7. Max bitrate: 1.5M to 3M - Smartphone: Quality level: 6. Max bitrate: 1M to 1.5M
hsQvbrQualityLevel :: Lens' H265Settings (Maybe Natural)
hsQvbrQualityLevel = lens _hsQvbrQualityLevel (\s a -> s {_hsQvbrQualityLevel = a}) . mapping _Nat

-- | Color Space settings
hsColorSpaceSettings :: Lens' H265Settings (Maybe H265ColorSpaceSettings)
hsColorSpaceSettings = lens _hsColorSpaceSettings (\s a -> s {_hsColorSpaceSettings = a})

-- | If set to enabled, adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
hsFlickerAq :: Lens' H265Settings (Maybe H265FlickerAq)
hsFlickerAq = lens _hsFlickerAq (\s a -> s {_hsFlickerAq = a})

-- | Size of buffer (HRD buffer model) in bits.
hsBufSize :: Lens' H265Settings (Maybe Natural)
hsBufSize = lens _hsBufSize (\s a -> s {_hsBufSize = a}) . mapping _Nat

-- | H.265 Tier.
hsTier :: Lens' H265Settings (Maybe H265Tier)
hsTier = lens _hsTier (\s a -> s {_hsTier = a})

-- | Four bit AFD value to write on all frames of video in the output stream. Only valid when afdSignaling is set to 'Fixed'.
hsFixedAfd :: Lens' H265Settings (Maybe FixedAfd)
hsFixedAfd = lens _hsFixedAfd (\s a -> s {_hsFixedAfd = a})

-- | Optional filters that you can apply to an encode.
hsFilterSettings :: Lens' H265Settings (Maybe H265FilterSettings)
hsFilterSettings = lens _hsFilterSettings (\s a -> s {_hsFilterSettings = a})

-- | Average bitrate in bits/second. Required when the rate control mode is VBR or CBR. Not used for QVBR. In an MS Smooth output group, each output must have a unique value when its bitrate is rounded down to the nearest multiple of 1000.
hsBitrate :: Lens' H265Settings (Maybe Natural)
hsBitrate = lens _hsBitrate (\s a -> s {_hsBitrate = a}) . mapping _Nat

-- | Includes colorspace metadata in the output.
hsColorMetadata :: Lens' H265Settings (Maybe H265ColorMetadata)
hsColorMetadata = lens _hsColorMetadata (\s a -> s {_hsColorMetadata = a})

-- | Amount of lookahead. A value of low can decrease latency and memory usage, while high can produce better quality for certain content.
hsLookAheadRateControl :: Lens' H265Settings (Maybe H265LookAheadRateControl)
hsLookAheadRateControl = lens _hsLookAheadRateControl (\s a -> s {_hsLookAheadRateControl = a})

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
hsAdaptiveQuantization :: Lens' H265Settings (Maybe H265AdaptiveQuantization)
hsAdaptiveQuantization = lens _hsAdaptiveQuantization (\s a -> s {_hsAdaptiveQuantization = a})

-- | H.265 Level.
hsLevel :: Lens' H265Settings (Maybe H265Level)
hsLevel = lens _hsLevel (\s a -> s {_hsLevel = a})

-- | For QVBR: See the tooltip for Quality level
hsMaxBitrate :: Lens' H265Settings (Maybe Natural)
hsMaxBitrate = lens _hsMaxBitrate (\s a -> s {_hsMaxBitrate = a}) . mapping _Nat

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
hsGopClosedCadence :: Lens' H265Settings (Maybe Natural)
hsGopClosedCadence = lens _hsGopClosedCadence (\s a -> s {_hsGopClosedCadence = a}) . mapping _Nat

-- | Pixel Aspect Ratio denominator.
hsParDenominator :: Lens' H265Settings (Maybe Natural)
hsParDenominator = lens _hsParDenominator (\s a -> s {_hsParDenominator = a}) . mapping _Nat

-- | Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
hsFramerateNumerator :: Lens' H265Settings Natural
hsFramerateNumerator = lens _hsFramerateNumerator (\s a -> s {_hsFramerateNumerator = a}) . _Nat

-- | Framerate denominator.
hsFramerateDenominator :: Lens' H265Settings Natural
hsFramerateDenominator = lens _hsFramerateDenominator (\s a -> s {_hsFramerateDenominator = a}) . _Nat

instance FromJSON H265Settings where
  parseJSON =
    withObject
      "H265Settings"
      ( \x ->
          H265Settings'
            <$> (x .:? "sceneChangeDetect")
            <*> (x .:? "scanType")
            <*> (x .:? "timecodeInsertion")
            <*> (x .:? "parNumerator")
            <*> (x .:? "afdSignaling")
            <*> (x .:? "gopSize")
            <*> (x .:? "gopSizeUnits")
            <*> (x .:? "slices")
            <*> (x .:? "profile")
            <*> (x .:? "alternativeTransferFunction")
            <*> (x .:? "rateControlMode")
            <*> (x .:? "minIInterval")
            <*> (x .:? "qvbrQualityLevel")
            <*> (x .:? "colorSpaceSettings")
            <*> (x .:? "flickerAq")
            <*> (x .:? "bufSize")
            <*> (x .:? "tier")
            <*> (x .:? "fixedAfd")
            <*> (x .:? "filterSettings")
            <*> (x .:? "bitrate")
            <*> (x .:? "colorMetadata")
            <*> (x .:? "lookAheadRateControl")
            <*> (x .:? "adaptiveQuantization")
            <*> (x .:? "level")
            <*> (x .:? "maxBitrate")
            <*> (x .:? "gopClosedCadence")
            <*> (x .:? "parDenominator")
            <*> (x .: "framerateNumerator")
            <*> (x .: "framerateDenominator")
      )

instance Hashable H265Settings

instance NFData H265Settings

instance ToJSON H265Settings where
  toJSON H265Settings' {..} =
    object
      ( catMaybes
          [ ("sceneChangeDetect" .=) <$> _hsSceneChangeDetect,
            ("scanType" .=) <$> _hsScanType,
            ("timecodeInsertion" .=) <$> _hsTimecodeInsertion,
            ("parNumerator" .=) <$> _hsParNumerator,
            ("afdSignaling" .=) <$> _hsAfdSignaling,
            ("gopSize" .=) <$> _hsGopSize,
            ("gopSizeUnits" .=) <$> _hsGopSizeUnits,
            ("slices" .=) <$> _hsSlices,
            ("profile" .=) <$> _hsProfile,
            ("alternativeTransferFunction" .=)
              <$> _hsAlternativeTransferFunction,
            ("rateControlMode" .=) <$> _hsRateControlMode,
            ("minIInterval" .=) <$> _hsMinIInterval,
            ("qvbrQualityLevel" .=) <$> _hsQvbrQualityLevel,
            ("colorSpaceSettings" .=) <$> _hsColorSpaceSettings,
            ("flickerAq" .=) <$> _hsFlickerAq,
            ("bufSize" .=) <$> _hsBufSize,
            ("tier" .=) <$> _hsTier,
            ("fixedAfd" .=) <$> _hsFixedAfd,
            ("filterSettings" .=) <$> _hsFilterSettings,
            ("bitrate" .=) <$> _hsBitrate,
            ("colorMetadata" .=) <$> _hsColorMetadata,
            ("lookAheadRateControl" .=) <$> _hsLookAheadRateControl,
            ("adaptiveQuantization" .=) <$> _hsAdaptiveQuantization,
            ("level" .=) <$> _hsLevel,
            ("maxBitrate" .=) <$> _hsMaxBitrate,
            ("gopClosedCadence" .=) <$> _hsGopClosedCadence,
            ("parDenominator" .=) <$> _hsParDenominator,
            Just ("framerateNumerator" .= _hsFramerateNumerator),
            Just ("framerateDenominator" .= _hsFramerateDenominator)
          ]
      )
