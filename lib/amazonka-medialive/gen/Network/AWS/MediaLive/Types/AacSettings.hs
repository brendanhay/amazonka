{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AacCodingMode
import Network.AWS.MediaLive.Types.AacInputType
import Network.AWS.MediaLive.Types.AacProfile
import Network.AWS.MediaLive.Types.AacRateControlMode
import Network.AWS.MediaLive.Types.AacRawFormat
import Network.AWS.MediaLive.Types.AacSpec
import Network.AWS.MediaLive.Types.AacVbrQuality
import Network.AWS.Prelude

-- | Aac Settings
--
-- /See:/ 'aacSettings' smart constructor.
data AacSettings = AacSettings'
  { _aRawFormat ::
      !(Maybe AacRawFormat),
    _aCodingMode :: !(Maybe AacCodingMode),
    _aProfile :: !(Maybe AacProfile),
    _aRateControlMode :: !(Maybe AacRateControlMode),
    _aSampleRate :: !(Maybe Double),
    _aSpec :: !(Maybe AacSpec),
    _aBitrate :: !(Maybe Double),
    _aVbrQuality :: !(Maybe AacVbrQuality),
    _aInputType :: !(Maybe AacInputType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AacSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aRawFormat' - Sets LATM / LOAS AAC output for raw containers.
--
-- * 'aCodingMode' - Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. The adReceiverMix setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
--
-- * 'aProfile' - AAC Profile.
--
-- * 'aRateControlMode' - Rate Control Mode.
--
-- * 'aSampleRate' - Sample rate in Hz. Valid values depend on rate control mode and profile.
--
-- * 'aSpec' - Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
--
-- * 'aBitrate' - Average bitrate in bits/second. Valid values depend on rate control mode and profile.
--
-- * 'aVbrQuality' - VBR Quality Level - Only used if rateControlMode is VBR.
--
-- * 'aInputType' - Set to "broadcasterMixedAd" when input contains pre-mixed main audio + AD (narration) as a stereo pair.  The Audio Type field (audioType) will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. The values in audioTypeControl and audioType (in AudioDescription) are ignored when set to broadcasterMixedAd. Leave set to "normal" when input does not contain pre-mixed audio + AD.
aacSettings ::
  AacSettings
aacSettings =
  AacSettings'
    { _aRawFormat = Nothing,
      _aCodingMode = Nothing,
      _aProfile = Nothing,
      _aRateControlMode = Nothing,
      _aSampleRate = Nothing,
      _aSpec = Nothing,
      _aBitrate = Nothing,
      _aVbrQuality = Nothing,
      _aInputType = Nothing
    }

-- | Sets LATM / LOAS AAC output for raw containers.
aRawFormat :: Lens' AacSettings (Maybe AacRawFormat)
aRawFormat = lens _aRawFormat (\s a -> s {_aRawFormat = a})

-- | Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. The adReceiverMix setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
aCodingMode :: Lens' AacSettings (Maybe AacCodingMode)
aCodingMode = lens _aCodingMode (\s a -> s {_aCodingMode = a})

-- | AAC Profile.
aProfile :: Lens' AacSettings (Maybe AacProfile)
aProfile = lens _aProfile (\s a -> s {_aProfile = a})

-- | Rate Control Mode.
aRateControlMode :: Lens' AacSettings (Maybe AacRateControlMode)
aRateControlMode = lens _aRateControlMode (\s a -> s {_aRateControlMode = a})

-- | Sample rate in Hz. Valid values depend on rate control mode and profile.
aSampleRate :: Lens' AacSettings (Maybe Double)
aSampleRate = lens _aSampleRate (\s a -> s {_aSampleRate = a})

-- | Use MPEG-2 AAC audio instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
aSpec :: Lens' AacSettings (Maybe AacSpec)
aSpec = lens _aSpec (\s a -> s {_aSpec = a})

-- | Average bitrate in bits/second. Valid values depend on rate control mode and profile.
aBitrate :: Lens' AacSettings (Maybe Double)
aBitrate = lens _aBitrate (\s a -> s {_aBitrate = a})

-- | VBR Quality Level - Only used if rateControlMode is VBR.
aVbrQuality :: Lens' AacSettings (Maybe AacVbrQuality)
aVbrQuality = lens _aVbrQuality (\s a -> s {_aVbrQuality = a})

-- | Set to "broadcasterMixedAd" when input contains pre-mixed main audio + AD (narration) as a stereo pair.  The Audio Type field (audioType) will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. The values in audioTypeControl and audioType (in AudioDescription) are ignored when set to broadcasterMixedAd. Leave set to "normal" when input does not contain pre-mixed audio + AD.
aInputType :: Lens' AacSettings (Maybe AacInputType)
aInputType = lens _aInputType (\s a -> s {_aInputType = a})

instance FromJSON AacSettings where
  parseJSON =
    withObject
      "AacSettings"
      ( \x ->
          AacSettings'
            <$> (x .:? "rawFormat")
            <*> (x .:? "codingMode")
            <*> (x .:? "profile")
            <*> (x .:? "rateControlMode")
            <*> (x .:? "sampleRate")
            <*> (x .:? "spec")
            <*> (x .:? "bitrate")
            <*> (x .:? "vbrQuality")
            <*> (x .:? "inputType")
      )

instance Hashable AacSettings

instance NFData AacSettings

instance ToJSON AacSettings where
  toJSON AacSettings' {..} =
    object
      ( catMaybes
          [ ("rawFormat" .=) <$> _aRawFormat,
            ("codingMode" .=) <$> _aCodingMode,
            ("profile" .=) <$> _aProfile,
            ("rateControlMode" .=) <$> _aRateControlMode,
            ("sampleRate" .=) <$> _aSampleRate,
            ("spec" .=) <$> _aSpec,
            ("bitrate" .=) <$> _aBitrate,
            ("vbrQuality" .=) <$> _aVbrQuality,
            ("inputType" .=) <$> _aInputType
          ]
      )
