{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Ac3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Ac3BitstreamMode
import Network.AWS.MediaConvert.Types.Ac3CodingMode
import Network.AWS.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
import Network.AWS.MediaConvert.Types.Ac3LfeFilter
import Network.AWS.MediaConvert.Types.Ac3MetadataControl
import Network.AWS.Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AC3.
--
-- /See:/ 'ac3Settings' smart constructor.
data Ac3Settings = Ac3Settings'
  { _aLfeFilter ::
      !(Maybe Ac3LfeFilter),
    _aMetadataControl :: !(Maybe Ac3MetadataControl),
    _aBitstreamMode :: !(Maybe Ac3BitstreamMode),
    _aCodingMode :: !(Maybe Ac3CodingMode),
    _aSampleRate :: !(Maybe Nat),
    _aDynamicRangeCompressionProfile ::
      !(Maybe Ac3DynamicRangeCompressionProfile),
    _aBitrate :: !(Maybe Nat),
    _aDialnorm :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Ac3Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aLfeFilter' - Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
--
-- * 'aMetadataControl' - When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
--
-- * 'aBitstreamMode' - Specify the bitstream mode for the AC-3 stream that the encoder emits. For more information about the AC3 bitstream mode, see ATSC A/52-2012 (Annex E).
--
-- * 'aCodingMode' - Dolby Digital coding mode. Determines number of channels.
--
-- * 'aSampleRate' - This value is always 48000. It represents the sample rate in Hz.
--
-- * 'aDynamicRangeCompressionProfile' - If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
--
-- * 'aBitrate' - Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
--
-- * 'aDialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby Digital, dialnorm will be passed through.
ac3Settings ::
  Ac3Settings
ac3Settings =
  Ac3Settings'
    { _aLfeFilter = Nothing,
      _aMetadataControl = Nothing,
      _aBitstreamMode = Nothing,
      _aCodingMode = Nothing,
      _aSampleRate = Nothing,
      _aDynamicRangeCompressionProfile = Nothing,
      _aBitrate = Nothing,
      _aDialnorm = Nothing
    }

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
aLfeFilter :: Lens' Ac3Settings (Maybe Ac3LfeFilter)
aLfeFilter = lens _aLfeFilter (\s a -> s {_aLfeFilter = a})

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
aMetadataControl :: Lens' Ac3Settings (Maybe Ac3MetadataControl)
aMetadataControl = lens _aMetadataControl (\s a -> s {_aMetadataControl = a})

-- | Specify the bitstream mode for the AC-3 stream that the encoder emits. For more information about the AC3 bitstream mode, see ATSC A/52-2012 (Annex E).
aBitstreamMode :: Lens' Ac3Settings (Maybe Ac3BitstreamMode)
aBitstreamMode = lens _aBitstreamMode (\s a -> s {_aBitstreamMode = a})

-- | Dolby Digital coding mode. Determines number of channels.
aCodingMode :: Lens' Ac3Settings (Maybe Ac3CodingMode)
aCodingMode = lens _aCodingMode (\s a -> s {_aCodingMode = a})

-- | This value is always 48000. It represents the sample rate in Hz.
aSampleRate :: Lens' Ac3Settings (Maybe Natural)
aSampleRate = lens _aSampleRate (\s a -> s {_aSampleRate = a}) . mapping _Nat

-- | If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
aDynamicRangeCompressionProfile :: Lens' Ac3Settings (Maybe Ac3DynamicRangeCompressionProfile)
aDynamicRangeCompressionProfile = lens _aDynamicRangeCompressionProfile (\s a -> s {_aDynamicRangeCompressionProfile = a})

-- | Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
aBitrate :: Lens' Ac3Settings (Maybe Natural)
aBitrate = lens _aBitrate (\s a -> s {_aBitrate = a}) . mapping _Nat

-- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital, dialnorm will be passed through.
aDialnorm :: Lens' Ac3Settings (Maybe Natural)
aDialnorm = lens _aDialnorm (\s a -> s {_aDialnorm = a}) . mapping _Nat

instance FromJSON Ac3Settings where
  parseJSON =
    withObject
      "Ac3Settings"
      ( \x ->
          Ac3Settings'
            <$> (x .:? "lfeFilter")
            <*> (x .:? "metadataControl")
            <*> (x .:? "bitstreamMode")
            <*> (x .:? "codingMode")
            <*> (x .:? "sampleRate")
            <*> (x .:? "dynamicRangeCompressionProfile")
            <*> (x .:? "bitrate")
            <*> (x .:? "dialnorm")
      )

instance Hashable Ac3Settings

instance NFData Ac3Settings

instance ToJSON Ac3Settings where
  toJSON Ac3Settings' {..} =
    object
      ( catMaybes
          [ ("lfeFilter" .=) <$> _aLfeFilter,
            ("metadataControl" .=) <$> _aMetadataControl,
            ("bitstreamMode" .=) <$> _aBitstreamMode,
            ("codingMode" .=) <$> _aCodingMode,
            ("sampleRate" .=) <$> _aSampleRate,
            ("dynamicRangeCompressionProfile" .=)
              <$> _aDynamicRangeCompressionProfile,
            ("bitrate" .=) <$> _aBitrate,
            ("dialnorm" .=) <$> _aDialnorm
          ]
      )
