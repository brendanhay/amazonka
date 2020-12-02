{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioCodecSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioCodecSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AacSettings
import Network.AWS.MediaConvert.Types.Ac3Settings
import Network.AWS.MediaConvert.Types.AiffSettings
import Network.AWS.MediaConvert.Types.AudioCodec
import Network.AWS.MediaConvert.Types.Eac3AtmosSettings
import Network.AWS.MediaConvert.Types.Eac3Settings
import Network.AWS.MediaConvert.Types.Mp2Settings
import Network.AWS.MediaConvert.Types.Mp3Settings
import Network.AWS.MediaConvert.Types.OpusSettings
import Network.AWS.MediaConvert.Types.VorbisSettings
import Network.AWS.MediaConvert.Types.WavSettings
import Network.AWS.Prelude

-- | Audio codec settings (CodecSettings) under (AudioDescriptions) contains the group of settings related to audio encoding. The settings in this group vary depending on the value that you choose for Audio codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AAC, AacSettings * MP2, Mp2Settings * MP3, Mp3Settings * WAV, WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings * EAC3_ATMOS, Eac3AtmosSettings * VORBIS, VorbisSettings * OPUS, OpusSettings
--
-- /See:/ 'audioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'
  { _acsAiffSettings ::
      !(Maybe AiffSettings),
    _acsCodec :: !(Maybe AudioCodec),
    _acsAc3Settings :: !(Maybe Ac3Settings),
    _acsOpusSettings :: !(Maybe OpusSettings),
    _acsMp2Settings :: !(Maybe Mp2Settings),
    _acsWavSettings :: !(Maybe WavSettings),
    _acsEac3AtmosSettings :: !(Maybe Eac3AtmosSettings),
    _acsMp3Settings :: !(Maybe Mp3Settings),
    _acsVorbisSettings :: !(Maybe VorbisSettings),
    _acsAacSettings :: !(Maybe AacSettings),
    _acsEac3Settings :: !(Maybe Eac3Settings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioCodecSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsAiffSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.
--
-- * 'acsCodec' - Type of Audio codec.
--
-- * 'acsAc3Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AC3.
--
-- * 'acsOpusSettings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to the value OPUS.
--
-- * 'acsMp2Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value MP2.
--
-- * 'acsWavSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value WAV.
--
-- * 'acsEac3AtmosSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3_ATMOS.
--
-- * 'acsMp3Settings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to the value MP3.
--
-- * 'acsVorbisSettings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to the value Vorbis.
--
-- * 'acsAacSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC. The service accepts one of two mutually exclusive groups of AAC settings--VBR and CBR. To select one of these modes, set the value of Bitrate control mode (rateControlMode) to "VBR" or "CBR".  In VBR mode, you control the audio quality with the setting VBR quality (vbrQuality). In CBR mode, you use the setting Bitrate (bitrate). Defaults and valid values depend on the rate control mode.
--
-- * 'acsEac3Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3.
audioCodecSettings ::
  AudioCodecSettings
audioCodecSettings =
  AudioCodecSettings'
    { _acsAiffSettings = Nothing,
      _acsCodec = Nothing,
      _acsAc3Settings = Nothing,
      _acsOpusSettings = Nothing,
      _acsMp2Settings = Nothing,
      _acsWavSettings = Nothing,
      _acsEac3AtmosSettings = Nothing,
      _acsMp3Settings = Nothing,
      _acsVorbisSettings = Nothing,
      _acsAacSettings = Nothing,
      _acsEac3Settings = Nothing
    }

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.
acsAiffSettings :: Lens' AudioCodecSettings (Maybe AiffSettings)
acsAiffSettings = lens _acsAiffSettings (\s a -> s {_acsAiffSettings = a})

-- | Type of Audio codec.
acsCodec :: Lens' AudioCodecSettings (Maybe AudioCodec)
acsCodec = lens _acsCodec (\s a -> s {_acsCodec = a})

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AC3.
acsAc3Settings :: Lens' AudioCodecSettings (Maybe Ac3Settings)
acsAc3Settings = lens _acsAc3Settings (\s a -> s {_acsAc3Settings = a})

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value OPUS.
acsOpusSettings :: Lens' AudioCodecSettings (Maybe OpusSettings)
acsOpusSettings = lens _acsOpusSettings (\s a -> s {_acsOpusSettings = a})

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value MP2.
acsMp2Settings :: Lens' AudioCodecSettings (Maybe Mp2Settings)
acsMp2Settings = lens _acsMp2Settings (\s a -> s {_acsMp2Settings = a})

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value WAV.
acsWavSettings :: Lens' AudioCodecSettings (Maybe WavSettings)
acsWavSettings = lens _acsWavSettings (\s a -> s {_acsWavSettings = a})

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3_ATMOS.
acsEac3AtmosSettings :: Lens' AudioCodecSettings (Maybe Eac3AtmosSettings)
acsEac3AtmosSettings = lens _acsEac3AtmosSettings (\s a -> s {_acsEac3AtmosSettings = a})

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value MP3.
acsMp3Settings :: Lens' AudioCodecSettings (Maybe Mp3Settings)
acsMp3Settings = lens _acsMp3Settings (\s a -> s {_acsMp3Settings = a})

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value Vorbis.
acsVorbisSettings :: Lens' AudioCodecSettings (Maybe VorbisSettings)
acsVorbisSettings = lens _acsVorbisSettings (\s a -> s {_acsVorbisSettings = a})

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC. The service accepts one of two mutually exclusive groups of AAC settings--VBR and CBR. To select one of these modes, set the value of Bitrate control mode (rateControlMode) to "VBR" or "CBR".  In VBR mode, you control the audio quality with the setting VBR quality (vbrQuality). In CBR mode, you use the setting Bitrate (bitrate). Defaults and valid values depend on the rate control mode.
acsAacSettings :: Lens' AudioCodecSettings (Maybe AacSettings)
acsAacSettings = lens _acsAacSettings (\s a -> s {_acsAacSettings = a})

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3.
acsEac3Settings :: Lens' AudioCodecSettings (Maybe Eac3Settings)
acsEac3Settings = lens _acsEac3Settings (\s a -> s {_acsEac3Settings = a})

instance FromJSON AudioCodecSettings where
  parseJSON =
    withObject
      "AudioCodecSettings"
      ( \x ->
          AudioCodecSettings'
            <$> (x .:? "aiffSettings")
            <*> (x .:? "codec")
            <*> (x .:? "ac3Settings")
            <*> (x .:? "opusSettings")
            <*> (x .:? "mp2Settings")
            <*> (x .:? "wavSettings")
            <*> (x .:? "eac3AtmosSettings")
            <*> (x .:? "mp3Settings")
            <*> (x .:? "vorbisSettings")
            <*> (x .:? "aacSettings")
            <*> (x .:? "eac3Settings")
      )

instance Hashable AudioCodecSettings

instance NFData AudioCodecSettings

instance ToJSON AudioCodecSettings where
  toJSON AudioCodecSettings' {..} =
    object
      ( catMaybes
          [ ("aiffSettings" .=) <$> _acsAiffSettings,
            ("codec" .=) <$> _acsCodec,
            ("ac3Settings" .=) <$> _acsAc3Settings,
            ("opusSettings" .=) <$> _acsOpusSettings,
            ("mp2Settings" .=) <$> _acsMp2Settings,
            ("wavSettings" .=) <$> _acsWavSettings,
            ("eac3AtmosSettings" .=) <$> _acsEac3AtmosSettings,
            ("mp3Settings" .=) <$> _acsMp3Settings,
            ("vorbisSettings" .=) <$> _acsVorbisSettings,
            ("aacSettings" .=) <$> _acsAacSettings,
            ("eac3Settings" .=) <$> _acsEac3Settings
          ]
      )
