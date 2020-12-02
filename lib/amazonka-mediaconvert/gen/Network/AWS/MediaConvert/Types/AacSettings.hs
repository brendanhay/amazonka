{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AacSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AacAudioDescriptionBroadcasterMix
import Network.AWS.MediaConvert.Types.AacCodecProfile
import Network.AWS.MediaConvert.Types.AacCodingMode
import Network.AWS.MediaConvert.Types.AacRateControlMode
import Network.AWS.MediaConvert.Types.AacRawFormat
import Network.AWS.MediaConvert.Types.AacSpecification
import Network.AWS.MediaConvert.Types.AacVbrQuality
import Network.AWS.Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC. The service accepts one of two mutually exclusive groups of AAC settings--VBR and CBR. To select one of these modes, set the value of Bitrate control mode (rateControlMode) to "VBR" or "CBR".  In VBR mode, you control the audio quality with the setting VBR quality (vbrQuality). In CBR mode, you use the setting Bitrate (bitrate). Defaults and valid values depend on the rate control mode.
--
-- /See:/ 'aacSettings' smart constructor.
data AacSettings = AacSettings'
  { _assAudioDescriptionBroadcasterMix ::
      !(Maybe AacAudioDescriptionBroadcasterMix),
    _assRawFormat :: !(Maybe AacRawFormat),
    _assCodingMode :: !(Maybe AacCodingMode),
    _assRateControlMode :: !(Maybe AacRateControlMode),
    _assSampleRate :: !(Maybe Nat),
    _assSpecification :: !(Maybe AacSpecification),
    _assCodecProfile :: !(Maybe AacCodecProfile),
    _assBitrate :: !(Maybe Nat),
    _assVbrQuality :: !(Maybe AacVbrQuality)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AacSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assAudioDescriptionBroadcasterMix' - Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio + audio description (AD) as a stereo pair. The value for AudioType will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any values you provide in AudioType and  FollowInputAudioType. Choose NORMAL when the input does not contain pre-mixed audio + audio description (AD). In this case, the encoder will use any values you provide for AudioType and FollowInputAudioType.
--
-- * 'assRawFormat' - Enables LATM/LOAS AAC output. Note that if you use LATM/LOAS AAC in an output, you must choose "No container" for the output container.
--
-- * 'assCodingMode' - Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. "1.0 - Audio Description (Receiver Mix)" setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
--
-- * 'assRateControlMode' - Rate Control Mode.
--
-- * 'assSampleRate' - Sample rate in Hz. Valid values depend on rate control mode and profile.
--
-- * 'assSpecification' - Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
--
-- * 'assCodecProfile' - AAC Profile.
--
-- * 'assBitrate' - Specify the average bitrate in bits per second. The set of valid values for this setting is: 6000, 8000, 10000, 12000, 14000, 16000, 20000, 24000, 28000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 288000, 320000, 384000, 448000, 512000, 576000, 640000, 768000, 896000, 1024000. The value you set is also constrained by the values that you choose for Profile (codecProfile), Bitrate control mode (codingMode), and Sample rate (sampleRate). Default values depend on Bitrate control mode and Profile.
--
-- * 'assVbrQuality' - VBR Quality Level - Only used if rate_control_mode is VBR.
aacSettings ::
  AacSettings
aacSettings =
  AacSettings'
    { _assAudioDescriptionBroadcasterMix = Nothing,
      _assRawFormat = Nothing,
      _assCodingMode = Nothing,
      _assRateControlMode = Nothing,
      _assSampleRate = Nothing,
      _assSpecification = Nothing,
      _assCodecProfile = Nothing,
      _assBitrate = Nothing,
      _assVbrQuality = Nothing
    }

-- | Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio + audio description (AD) as a stereo pair. The value for AudioType will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any values you provide in AudioType and  FollowInputAudioType. Choose NORMAL when the input does not contain pre-mixed audio + audio description (AD). In this case, the encoder will use any values you provide for AudioType and FollowInputAudioType.
assAudioDescriptionBroadcasterMix :: Lens' AacSettings (Maybe AacAudioDescriptionBroadcasterMix)
assAudioDescriptionBroadcasterMix = lens _assAudioDescriptionBroadcasterMix (\s a -> s {_assAudioDescriptionBroadcasterMix = a})

-- | Enables LATM/LOAS AAC output. Note that if you use LATM/LOAS AAC in an output, you must choose "No container" for the output container.
assRawFormat :: Lens' AacSettings (Maybe AacRawFormat)
assRawFormat = lens _assRawFormat (\s a -> s {_assRawFormat = a})

-- | Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. "1.0 - Audio Description (Receiver Mix)" setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
assCodingMode :: Lens' AacSettings (Maybe AacCodingMode)
assCodingMode = lens _assCodingMode (\s a -> s {_assCodingMode = a})

-- | Rate Control Mode.
assRateControlMode :: Lens' AacSettings (Maybe AacRateControlMode)
assRateControlMode = lens _assRateControlMode (\s a -> s {_assRateControlMode = a})

-- | Sample rate in Hz. Valid values depend on rate control mode and profile.
assSampleRate :: Lens' AacSettings (Maybe Natural)
assSampleRate = lens _assSampleRate (\s a -> s {_assSampleRate = a}) . mapping _Nat

-- | Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
assSpecification :: Lens' AacSettings (Maybe AacSpecification)
assSpecification = lens _assSpecification (\s a -> s {_assSpecification = a})

-- | AAC Profile.
assCodecProfile :: Lens' AacSettings (Maybe AacCodecProfile)
assCodecProfile = lens _assCodecProfile (\s a -> s {_assCodecProfile = a})

-- | Specify the average bitrate in bits per second. The set of valid values for this setting is: 6000, 8000, 10000, 12000, 14000, 16000, 20000, 24000, 28000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 288000, 320000, 384000, 448000, 512000, 576000, 640000, 768000, 896000, 1024000. The value you set is also constrained by the values that you choose for Profile (codecProfile), Bitrate control mode (codingMode), and Sample rate (sampleRate). Default values depend on Bitrate control mode and Profile.
assBitrate :: Lens' AacSettings (Maybe Natural)
assBitrate = lens _assBitrate (\s a -> s {_assBitrate = a}) . mapping _Nat

-- | VBR Quality Level - Only used if rate_control_mode is VBR.
assVbrQuality :: Lens' AacSettings (Maybe AacVbrQuality)
assVbrQuality = lens _assVbrQuality (\s a -> s {_assVbrQuality = a})

instance FromJSON AacSettings where
  parseJSON =
    withObject
      "AacSettings"
      ( \x ->
          AacSettings'
            <$> (x .:? "audioDescriptionBroadcasterMix")
            <*> (x .:? "rawFormat")
            <*> (x .:? "codingMode")
            <*> (x .:? "rateControlMode")
            <*> (x .:? "sampleRate")
            <*> (x .:? "specification")
            <*> (x .:? "codecProfile")
            <*> (x .:? "bitrate")
            <*> (x .:? "vbrQuality")
      )

instance Hashable AacSettings

instance NFData AacSettings

instance ToJSON AacSettings where
  toJSON AacSettings' {..} =
    object
      ( catMaybes
          [ ("audioDescriptionBroadcasterMix" .=)
              <$> _assAudioDescriptionBroadcasterMix,
            ("rawFormat" .=) <$> _assRawFormat,
            ("codingMode" .=) <$> _assCodingMode,
            ("rateControlMode" .=) <$> _assRateControlMode,
            ("sampleRate" .=) <$> _assSampleRate,
            ("specification" .=) <$> _assSpecification,
            ("codecProfile" .=) <$> _assCodecProfile,
            ("bitrate" .=) <$> _assBitrate,
            ("vbrQuality" .=) <$> _assVbrQuality
          ]
      )
