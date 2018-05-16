{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Product where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Sum
import Network.AWS.Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC.
--
-- /See:/ 'aacSettings' smart constructor.
data AacSettings = AacSettings'
  { _assAudioDescriptionBroadcasterMix :: !(Maybe AacAudioDescriptionBroadcasterMix)
  , _assRawFormat :: !(Maybe AacRawFormat)
  , _assCodingMode :: !(Maybe AacCodingMode)
  , _assRateControlMode :: !(Maybe AacRateControlMode)
  , _assSampleRate :: !(Maybe Int)
  , _assSpecification :: !(Maybe AacSpecification)
  , _assCodecProfile :: !(Maybe AacCodecProfile)
  , _assBitrate :: !(Maybe Int)
  , _assVbrQuality :: !(Maybe AacVbrQuality)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AacSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assAudioDescriptionBroadcasterMix' - Undocumented member.
--
-- * 'assRawFormat' - Undocumented member.
--
-- * 'assCodingMode' - Undocumented member.
--
-- * 'assRateControlMode' - Undocumented member.
--
-- * 'assSampleRate' - Sample rate in Hz. Valid values depend on rate control mode and profile.
--
-- * 'assSpecification' - Undocumented member.
--
-- * 'assCodecProfile' - Undocumented member.
--
-- * 'assBitrate' - Average bitrate in bits/second. Valid values depend on rate control mode and profile.
--
-- * 'assVbrQuality' - Undocumented member.
aacSettings
    :: AacSettings
aacSettings =
  AacSettings'
    { _assAudioDescriptionBroadcasterMix = Nothing
    , _assRawFormat = Nothing
    , _assCodingMode = Nothing
    , _assRateControlMode = Nothing
    , _assSampleRate = Nothing
    , _assSpecification = Nothing
    , _assCodecProfile = Nothing
    , _assBitrate = Nothing
    , _assVbrQuality = Nothing
    }


-- | Undocumented member.
assAudioDescriptionBroadcasterMix :: Lens' AacSettings (Maybe AacAudioDescriptionBroadcasterMix)
assAudioDescriptionBroadcasterMix = lens _assAudioDescriptionBroadcasterMix (\ s a -> s{_assAudioDescriptionBroadcasterMix = a})

-- | Undocumented member.
assRawFormat :: Lens' AacSettings (Maybe AacRawFormat)
assRawFormat = lens _assRawFormat (\ s a -> s{_assRawFormat = a})

-- | Undocumented member.
assCodingMode :: Lens' AacSettings (Maybe AacCodingMode)
assCodingMode = lens _assCodingMode (\ s a -> s{_assCodingMode = a})

-- | Undocumented member.
assRateControlMode :: Lens' AacSettings (Maybe AacRateControlMode)
assRateControlMode = lens _assRateControlMode (\ s a -> s{_assRateControlMode = a})

-- | Sample rate in Hz. Valid values depend on rate control mode and profile.
assSampleRate :: Lens' AacSettings (Maybe Int)
assSampleRate = lens _assSampleRate (\ s a -> s{_assSampleRate = a})

-- | Undocumented member.
assSpecification :: Lens' AacSettings (Maybe AacSpecification)
assSpecification = lens _assSpecification (\ s a -> s{_assSpecification = a})

-- | Undocumented member.
assCodecProfile :: Lens' AacSettings (Maybe AacCodecProfile)
assCodecProfile = lens _assCodecProfile (\ s a -> s{_assCodecProfile = a})

-- | Average bitrate in bits/second. Valid values depend on rate control mode and profile.
assBitrate :: Lens' AacSettings (Maybe Int)
assBitrate = lens _assBitrate (\ s a -> s{_assBitrate = a})

-- | Undocumented member.
assVbrQuality :: Lens' AacSettings (Maybe AacVbrQuality)
assVbrQuality = lens _assVbrQuality (\ s a -> s{_assVbrQuality = a})

instance FromJSON AacSettings where
        parseJSON
          = withObject "AacSettings"
              (\ x ->
                 AacSettings' <$>
                   (x .:? "audioDescriptionBroadcasterMix") <*>
                     (x .:? "rawFormat")
                     <*> (x .:? "codingMode")
                     <*> (x .:? "rateControlMode")
                     <*> (x .:? "sampleRate")
                     <*> (x .:? "specification")
                     <*> (x .:? "codecProfile")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "vbrQuality"))

instance Hashable AacSettings where

instance NFData AacSettings where

instance ToJSON AacSettings where
        toJSON AacSettings'{..}
          = object
              (catMaybes
                 [("audioDescriptionBroadcasterMix" .=) <$>
                    _assAudioDescriptionBroadcasterMix,
                  ("rawFormat" .=) <$> _assRawFormat,
                  ("codingMode" .=) <$> _assCodingMode,
                  ("rateControlMode" .=) <$> _assRateControlMode,
                  ("sampleRate" .=) <$> _assSampleRate,
                  ("specification" .=) <$> _assSpecification,
                  ("codecProfile" .=) <$> _assCodecProfile,
                  ("bitrate" .=) <$> _assBitrate,
                  ("vbrQuality" .=) <$> _assVbrQuality])

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AC3.
--
-- /See:/ 'ac3Settings' smart constructor.
data Ac3Settings = Ac3Settings'
  { _aLfeFilter :: !(Maybe Ac3LfeFilter)
  , _aMetadataControl :: !(Maybe Ac3MetadataControl)
  , _aBitstreamMode :: !(Maybe Ac3BitstreamMode)
  , _aCodingMode :: !(Maybe Ac3CodingMode)
  , _aSampleRate :: !(Maybe Int)
  , _aDynamicRangeCompressionProfile :: !(Maybe Ac3DynamicRangeCompressionProfile)
  , _aBitrate :: !(Maybe Int)
  , _aDialnorm :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Ac3Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aLfeFilter' - Undocumented member.
--
-- * 'aMetadataControl' - Undocumented member.
--
-- * 'aBitstreamMode' - Undocumented member.
--
-- * 'aCodingMode' - Undocumented member.
--
-- * 'aSampleRate' - Sample rate in hz. Sample rate is always 48000.
--
-- * 'aDynamicRangeCompressionProfile' - Undocumented member.
--
-- * 'aBitrate' - Average bitrate in bits/second. Valid bitrates depend on the coding mode.
--
-- * 'aDialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby Digital, dialnorm will be passed through.
ac3Settings
    :: Ac3Settings
ac3Settings =
  Ac3Settings'
    { _aLfeFilter = Nothing
    , _aMetadataControl = Nothing
    , _aBitstreamMode = Nothing
    , _aCodingMode = Nothing
    , _aSampleRate = Nothing
    , _aDynamicRangeCompressionProfile = Nothing
    , _aBitrate = Nothing
    , _aDialnorm = Nothing
    }


-- | Undocumented member.
aLfeFilter :: Lens' Ac3Settings (Maybe Ac3LfeFilter)
aLfeFilter = lens _aLfeFilter (\ s a -> s{_aLfeFilter = a})

-- | Undocumented member.
aMetadataControl :: Lens' Ac3Settings (Maybe Ac3MetadataControl)
aMetadataControl = lens _aMetadataControl (\ s a -> s{_aMetadataControl = a})

-- | Undocumented member.
aBitstreamMode :: Lens' Ac3Settings (Maybe Ac3BitstreamMode)
aBitstreamMode = lens _aBitstreamMode (\ s a -> s{_aBitstreamMode = a})

-- | Undocumented member.
aCodingMode :: Lens' Ac3Settings (Maybe Ac3CodingMode)
aCodingMode = lens _aCodingMode (\ s a -> s{_aCodingMode = a})

-- | Sample rate in hz. Sample rate is always 48000.
aSampleRate :: Lens' Ac3Settings (Maybe Int)
aSampleRate = lens _aSampleRate (\ s a -> s{_aSampleRate = a})

-- | Undocumented member.
aDynamicRangeCompressionProfile :: Lens' Ac3Settings (Maybe Ac3DynamicRangeCompressionProfile)
aDynamicRangeCompressionProfile = lens _aDynamicRangeCompressionProfile (\ s a -> s{_aDynamicRangeCompressionProfile = a})

-- | Average bitrate in bits/second. Valid bitrates depend on the coding mode.
aBitrate :: Lens' Ac3Settings (Maybe Int)
aBitrate = lens _aBitrate (\ s a -> s{_aBitrate = a})

-- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital, dialnorm will be passed through.
aDialnorm :: Lens' Ac3Settings (Maybe Int)
aDialnorm = lens _aDialnorm (\ s a -> s{_aDialnorm = a})

instance FromJSON Ac3Settings where
        parseJSON
          = withObject "Ac3Settings"
              (\ x ->
                 Ac3Settings' <$>
                   (x .:? "lfeFilter") <*> (x .:? "metadataControl") <*>
                     (x .:? "bitstreamMode")
                     <*> (x .:? "codingMode")
                     <*> (x .:? "sampleRate")
                     <*> (x .:? "dynamicRangeCompressionProfile")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "dialnorm"))

instance Hashable Ac3Settings where

instance NFData Ac3Settings where

instance ToJSON Ac3Settings where
        toJSON Ac3Settings'{..}
          = object
              (catMaybes
                 [("lfeFilter" .=) <$> _aLfeFilter,
                  ("metadataControl" .=) <$> _aMetadataControl,
                  ("bitstreamMode" .=) <$> _aBitstreamMode,
                  ("codingMode" .=) <$> _aCodingMode,
                  ("sampleRate" .=) <$> _aSampleRate,
                  ("dynamicRangeCompressionProfile" .=) <$>
                    _aDynamicRangeCompressionProfile,
                  ("bitrate" .=) <$> _aBitrate,
                  ("dialnorm" .=) <$> _aDialnorm])

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.
--
-- /See:/ 'aiffSettings' smart constructor.
data AiffSettings = AiffSettings'
  { _asBitDepth   :: !(Maybe Int)
  , _asChannels   :: !(Maybe Int)
  , _asSampleRate :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AiffSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asBitDepth' - Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
--
-- * 'asChannels' - Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
--
-- * 'asSampleRate' - Sample rate in hz.
aiffSettings
    :: AiffSettings
aiffSettings =
  AiffSettings'
    {_asBitDepth = Nothing, _asChannels = Nothing, _asSampleRate = Nothing}


-- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
asBitDepth :: Lens' AiffSettings (Maybe Int)
asBitDepth = lens _asBitDepth (\ s a -> s{_asBitDepth = a})

-- | Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
asChannels :: Lens' AiffSettings (Maybe Int)
asChannels = lens _asChannels (\ s a -> s{_asChannels = a})

-- | Sample rate in hz.
asSampleRate :: Lens' AiffSettings (Maybe Int)
asSampleRate = lens _asSampleRate (\ s a -> s{_asSampleRate = a})

instance FromJSON AiffSettings where
        parseJSON
          = withObject "AiffSettings"
              (\ x ->
                 AiffSettings' <$>
                   (x .:? "bitDepth") <*> (x .:? "channels") <*>
                     (x .:? "sampleRate"))

instance Hashable AiffSettings where

instance NFData AiffSettings where

instance ToJSON AiffSettings where
        toJSON AiffSettings'{..}
          = object
              (catMaybes
                 [("bitDepth" .=) <$> _asBitDepth,
                  ("channels" .=) <$> _asChannels,
                  ("sampleRate" .=) <$> _asSampleRate])

-- | Settings for ancillary captions source.
--
-- /See:/ 'ancillarySourceSettings' smart constructor.
newtype AncillarySourceSettings = AncillarySourceSettings'
  { _assSourceAncillaryChannelNumber :: Maybe Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AncillarySourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assSourceAncillaryChannelNumber' - Specifies the 608 channel number in the ancillary data track from which to extract captions. Unused for passthrough.
ancillarySourceSettings
    :: AncillarySourceSettings
ancillarySourceSettings =
  AncillarySourceSettings' {_assSourceAncillaryChannelNumber = Nothing}


-- | Specifies the 608 channel number in the ancillary data track from which to extract captions. Unused for passthrough.
assSourceAncillaryChannelNumber :: Lens' AncillarySourceSettings (Maybe Int)
assSourceAncillaryChannelNumber = lens _assSourceAncillaryChannelNumber (\ s a -> s{_assSourceAncillaryChannelNumber = a})

instance FromJSON AncillarySourceSettings where
        parseJSON
          = withObject "AncillarySourceSettings"
              (\ x ->
                 AncillarySourceSettings' <$>
                   (x .:? "sourceAncillaryChannelNumber"))

instance Hashable AncillarySourceSettings where

instance NFData AncillarySourceSettings where

instance ToJSON AncillarySourceSettings where
        toJSON AncillarySourceSettings'{..}
          = object
              (catMaybes
                 [("sourceAncillaryChannelNumber" .=) <$>
                    _assSourceAncillaryChannelNumber])

-- | Audio codec settings (CodecSettings) under (AudioDescriptions) contains the group of settings related to audio encoding. The settings in this group vary depending on the value you choose for Audio codec (Codec). For each codec enum you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AAC, AacSettings * MP2, Mp2Settings * WAV, WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings
--
-- /See:/ 'audioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'
  { _acsAiffSettings :: !(Maybe AiffSettings)
  , _acsCodec        :: !(Maybe AudioCodec)
  , _acsAc3Settings  :: !(Maybe Ac3Settings)
  , _acsMp2Settings  :: !(Maybe Mp2Settings)
  , _acsWavSettings  :: !(Maybe WavSettings)
  , _acsAacSettings  :: !(Maybe AacSettings)
  , _acsEac3Settings :: !(Maybe Eac3Settings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AudioCodecSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsAiffSettings' - Undocumented member.
--
-- * 'acsCodec' - Undocumented member.
--
-- * 'acsAc3Settings' - Undocumented member.
--
-- * 'acsMp2Settings' - Undocumented member.
--
-- * 'acsWavSettings' - Undocumented member.
--
-- * 'acsAacSettings' - Undocumented member.
--
-- * 'acsEac3Settings' - Undocumented member.
audioCodecSettings
    :: AudioCodecSettings
audioCodecSettings =
  AudioCodecSettings'
    { _acsAiffSettings = Nothing
    , _acsCodec = Nothing
    , _acsAc3Settings = Nothing
    , _acsMp2Settings = Nothing
    , _acsWavSettings = Nothing
    , _acsAacSettings = Nothing
    , _acsEac3Settings = Nothing
    }


-- | Undocumented member.
acsAiffSettings :: Lens' AudioCodecSettings (Maybe AiffSettings)
acsAiffSettings = lens _acsAiffSettings (\ s a -> s{_acsAiffSettings = a})

-- | Undocumented member.
acsCodec :: Lens' AudioCodecSettings (Maybe AudioCodec)
acsCodec = lens _acsCodec (\ s a -> s{_acsCodec = a})

-- | Undocumented member.
acsAc3Settings :: Lens' AudioCodecSettings (Maybe Ac3Settings)
acsAc3Settings = lens _acsAc3Settings (\ s a -> s{_acsAc3Settings = a})

-- | Undocumented member.
acsMp2Settings :: Lens' AudioCodecSettings (Maybe Mp2Settings)
acsMp2Settings = lens _acsMp2Settings (\ s a -> s{_acsMp2Settings = a})

-- | Undocumented member.
acsWavSettings :: Lens' AudioCodecSettings (Maybe WavSettings)
acsWavSettings = lens _acsWavSettings (\ s a -> s{_acsWavSettings = a})

-- | Undocumented member.
acsAacSettings :: Lens' AudioCodecSettings (Maybe AacSettings)
acsAacSettings = lens _acsAacSettings (\ s a -> s{_acsAacSettings = a})

-- | Undocumented member.
acsEac3Settings :: Lens' AudioCodecSettings (Maybe Eac3Settings)
acsEac3Settings = lens _acsEac3Settings (\ s a -> s{_acsEac3Settings = a})

instance FromJSON AudioCodecSettings where
        parseJSON
          = withObject "AudioCodecSettings"
              (\ x ->
                 AudioCodecSettings' <$>
                   (x .:? "aiffSettings") <*> (x .:? "codec") <*>
                     (x .:? "ac3Settings")
                     <*> (x .:? "mp2Settings")
                     <*> (x .:? "wavSettings")
                     <*> (x .:? "aacSettings")
                     <*> (x .:? "eac3Settings"))

instance Hashable AudioCodecSettings where

instance NFData AudioCodecSettings where

instance ToJSON AudioCodecSettings where
        toJSON AudioCodecSettings'{..}
          = object
              (catMaybes
                 [("aiffSettings" .=) <$> _acsAiffSettings,
                  ("codec" .=) <$> _acsCodec,
                  ("ac3Settings" .=) <$> _acsAc3Settings,
                  ("mp2Settings" .=) <$> _acsMp2Settings,
                  ("wavSettings" .=) <$> _acsWavSettings,
                  ("aacSettings" .=) <$> _acsAacSettings,
                  ("eac3Settings" .=) <$> _acsEac3Settings])

-- | Description of audio output
--
-- /See:/ 'audioDescription' smart constructor.
data AudioDescription = AudioDescription'
  { _adAudioSourceName            :: !(Maybe Text)
  , _adLanguageCode               :: !(Maybe LanguageCode)
  , _adAudioType                  :: !(Maybe Int)
  , _adAudioNormalizationSettings :: !(Maybe AudioNormalizationSettings)
  , _adLanguageCodeControl        :: !(Maybe AudioLanguageCodeControl)
  , _adCodecSettings              :: !(Maybe AudioCodecSettings)
  , _adStreamName                 :: !(Maybe Text)
  , _adRemixSettings              :: !(Maybe RemixSettings)
  , _adAudioTypeControl           :: !(Maybe AudioTypeControl)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AudioDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adAudioSourceName' - Specifies which audio data to use from each input. In the simplest case, specify an "Audio Selector":#inputs-audio_selector by name based on its order within each input. For example if you specify "Audio Selector 3", then the third audio selector will be used from each input. If an input does not have an "Audio Selector 3", then the audio selector marked as "default" in that input will be used. If there is no audio selector marked as "default", silence will be inserted for the duration of that input. Alternatively, an "Audio Selector Group":#inputs-audio_selector_group name may be specified, with similar default/silence behavior. If no audio_source_name is specified, then "Audio Selector 1" will be chosen automatically.
--
-- * 'adLanguageCode' - Indicates the language of the audio output track. The ISO 639 language specified in the 'Language Code' drop down will be used when 'Follow Input Language Code' is not selected or when 'Follow Input Language Code' is selected but there is no ISO 639 language code specified by the input.
--
-- * 'adAudioType' - Applies only if Follow Input Audio Type is unchecked (false). A number between 0 and 255. The following are defined in ISO-IEC 13818-1: 0 = Undefined, 1 = Clean Effects, 2 = Hearing Impaired, 3 = Visually Impaired Commentary, 4-255 = Reserved.
--
-- * 'adAudioNormalizationSettings' - Undocumented member.
--
-- * 'adLanguageCodeControl' - Undocumented member.
--
-- * 'adCodecSettings' - Undocumented member.
--
-- * 'adStreamName' - Used for MS Smooth and Apple HLS outputs. Indicates the name displayed by the player (eg. English, or Director Commentary). Alphanumeric characters, spaces, and underscore are legal.
--
-- * 'adRemixSettings' - Advanced audio remixing settings.
--
-- * 'adAudioTypeControl' - Undocumented member.
audioDescription
    :: AudioDescription
audioDescription =
  AudioDescription'
    { _adAudioSourceName = Nothing
    , _adLanguageCode = Nothing
    , _adAudioType = Nothing
    , _adAudioNormalizationSettings = Nothing
    , _adLanguageCodeControl = Nothing
    , _adCodecSettings = Nothing
    , _adStreamName = Nothing
    , _adRemixSettings = Nothing
    , _adAudioTypeControl = Nothing
    }


-- | Specifies which audio data to use from each input. In the simplest case, specify an "Audio Selector":#inputs-audio_selector by name based on its order within each input. For example if you specify "Audio Selector 3", then the third audio selector will be used from each input. If an input does not have an "Audio Selector 3", then the audio selector marked as "default" in that input will be used. If there is no audio selector marked as "default", silence will be inserted for the duration of that input. Alternatively, an "Audio Selector Group":#inputs-audio_selector_group name may be specified, with similar default/silence behavior. If no audio_source_name is specified, then "Audio Selector 1" will be chosen automatically.
adAudioSourceName :: Lens' AudioDescription (Maybe Text)
adAudioSourceName = lens _adAudioSourceName (\ s a -> s{_adAudioSourceName = a})

-- | Indicates the language of the audio output track. The ISO 639 language specified in the 'Language Code' drop down will be used when 'Follow Input Language Code' is not selected or when 'Follow Input Language Code' is selected but there is no ISO 639 language code specified by the input.
adLanguageCode :: Lens' AudioDescription (Maybe LanguageCode)
adLanguageCode = lens _adLanguageCode (\ s a -> s{_adLanguageCode = a})

-- | Applies only if Follow Input Audio Type is unchecked (false). A number between 0 and 255. The following are defined in ISO-IEC 13818-1: 0 = Undefined, 1 = Clean Effects, 2 = Hearing Impaired, 3 = Visually Impaired Commentary, 4-255 = Reserved.
adAudioType :: Lens' AudioDescription (Maybe Int)
adAudioType = lens _adAudioType (\ s a -> s{_adAudioType = a})

-- | Undocumented member.
adAudioNormalizationSettings :: Lens' AudioDescription (Maybe AudioNormalizationSettings)
adAudioNormalizationSettings = lens _adAudioNormalizationSettings (\ s a -> s{_adAudioNormalizationSettings = a})

-- | Undocumented member.
adLanguageCodeControl :: Lens' AudioDescription (Maybe AudioLanguageCodeControl)
adLanguageCodeControl = lens _adLanguageCodeControl (\ s a -> s{_adLanguageCodeControl = a})

-- | Undocumented member.
adCodecSettings :: Lens' AudioDescription (Maybe AudioCodecSettings)
adCodecSettings = lens _adCodecSettings (\ s a -> s{_adCodecSettings = a})

-- | Used for MS Smooth and Apple HLS outputs. Indicates the name displayed by the player (eg. English, or Director Commentary). Alphanumeric characters, spaces, and underscore are legal.
adStreamName :: Lens' AudioDescription (Maybe Text)
adStreamName = lens _adStreamName (\ s a -> s{_adStreamName = a})

-- | Advanced audio remixing settings.
adRemixSettings :: Lens' AudioDescription (Maybe RemixSettings)
adRemixSettings = lens _adRemixSettings (\ s a -> s{_adRemixSettings = a})

-- | Undocumented member.
adAudioTypeControl :: Lens' AudioDescription (Maybe AudioTypeControl)
adAudioTypeControl = lens _adAudioTypeControl (\ s a -> s{_adAudioTypeControl = a})

instance FromJSON AudioDescription where
        parseJSON
          = withObject "AudioDescription"
              (\ x ->
                 AudioDescription' <$>
                   (x .:? "audioSourceName") <*> (x .:? "languageCode")
                     <*> (x .:? "audioType")
                     <*> (x .:? "audioNormalizationSettings")
                     <*> (x .:? "languageCodeControl")
                     <*> (x .:? "codecSettings")
                     <*> (x .:? "streamName")
                     <*> (x .:? "remixSettings")
                     <*> (x .:? "audioTypeControl"))

instance Hashable AudioDescription where

instance NFData AudioDescription where

instance ToJSON AudioDescription where
        toJSON AudioDescription'{..}
          = object
              (catMaybes
                 [("audioSourceName" .=) <$> _adAudioSourceName,
                  ("languageCode" .=) <$> _adLanguageCode,
                  ("audioType" .=) <$> _adAudioType,
                  ("audioNormalizationSettings" .=) <$>
                    _adAudioNormalizationSettings,
                  ("languageCodeControl" .=) <$>
                    _adLanguageCodeControl,
                  ("codecSettings" .=) <$> _adCodecSettings,
                  ("streamName" .=) <$> _adStreamName,
                  ("remixSettings" .=) <$> _adRemixSettings,
                  ("audioTypeControl" .=) <$> _adAudioTypeControl])

-- | Advanced audio normalization settings.
--
-- /See:/ 'audioNormalizationSettings' smart constructor.
data AudioNormalizationSettings = AudioNormalizationSettings'
  { _ansAlgorithmControl    :: !(Maybe AudioNormalizationAlgorithmControl)
  , _ansTargetLkfs          :: !(Maybe Double)
  , _ansPeakCalculation     :: !(Maybe AudioNormalizationPeakCalculation)
  , _ansCorrectionGateLevel :: !(Maybe Int)
  , _ansAlgorithm           :: !(Maybe AudioNormalizationAlgorithm)
  , _ansLoudnessLogging     :: !(Maybe AudioNormalizationLoudnessLogging)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AudioNormalizationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ansAlgorithmControl' - Undocumented member.
--
-- * 'ansTargetLkfs' - Target LKFS(loudness) to adjust volume to. If no value is entered, a default value will be used according to the chosen algorithm. The CALM Act (1770-1) recommends a target of -24 LKFS. The EBU R-128 specification (1770-2) recommends a target of -23 LKFS.
--
-- * 'ansPeakCalculation' - Undocumented member.
--
-- * 'ansCorrectionGateLevel' - Content measuring above this level will be corrected to the target level. Content measuring below this level will not be corrected. Gating only applies when not using real_time_correction.
--
-- * 'ansAlgorithm' - Undocumented member.
--
-- * 'ansLoudnessLogging' - Undocumented member.
audioNormalizationSettings
    :: AudioNormalizationSettings
audioNormalizationSettings =
  AudioNormalizationSettings'
    { _ansAlgorithmControl = Nothing
    , _ansTargetLkfs = Nothing
    , _ansPeakCalculation = Nothing
    , _ansCorrectionGateLevel = Nothing
    , _ansAlgorithm = Nothing
    , _ansLoudnessLogging = Nothing
    }


-- | Undocumented member.
ansAlgorithmControl :: Lens' AudioNormalizationSettings (Maybe AudioNormalizationAlgorithmControl)
ansAlgorithmControl = lens _ansAlgorithmControl (\ s a -> s{_ansAlgorithmControl = a})

-- | Target LKFS(loudness) to adjust volume to. If no value is entered, a default value will be used according to the chosen algorithm. The CALM Act (1770-1) recommends a target of -24 LKFS. The EBU R-128 specification (1770-2) recommends a target of -23 LKFS.
ansTargetLkfs :: Lens' AudioNormalizationSettings (Maybe Double)
ansTargetLkfs = lens _ansTargetLkfs (\ s a -> s{_ansTargetLkfs = a})

-- | Undocumented member.
ansPeakCalculation :: Lens' AudioNormalizationSettings (Maybe AudioNormalizationPeakCalculation)
ansPeakCalculation = lens _ansPeakCalculation (\ s a -> s{_ansPeakCalculation = a})

-- | Content measuring above this level will be corrected to the target level. Content measuring below this level will not be corrected. Gating only applies when not using real_time_correction.
ansCorrectionGateLevel :: Lens' AudioNormalizationSettings (Maybe Int)
ansCorrectionGateLevel = lens _ansCorrectionGateLevel (\ s a -> s{_ansCorrectionGateLevel = a})

-- | Undocumented member.
ansAlgorithm :: Lens' AudioNormalizationSettings (Maybe AudioNormalizationAlgorithm)
ansAlgorithm = lens _ansAlgorithm (\ s a -> s{_ansAlgorithm = a})

-- | Undocumented member.
ansLoudnessLogging :: Lens' AudioNormalizationSettings (Maybe AudioNormalizationLoudnessLogging)
ansLoudnessLogging = lens _ansLoudnessLogging (\ s a -> s{_ansLoudnessLogging = a})

instance FromJSON AudioNormalizationSettings where
        parseJSON
          = withObject "AudioNormalizationSettings"
              (\ x ->
                 AudioNormalizationSettings' <$>
                   (x .:? "algorithmControl") <*> (x .:? "targetLkfs")
                     <*> (x .:? "peakCalculation")
                     <*> (x .:? "correctionGateLevel")
                     <*> (x .:? "algorithm")
                     <*> (x .:? "loudnessLogging"))

instance Hashable AudioNormalizationSettings where

instance NFData AudioNormalizationSettings where

instance ToJSON AudioNormalizationSettings where
        toJSON AudioNormalizationSettings'{..}
          = object
              (catMaybes
                 [("algorithmControl" .=) <$> _ansAlgorithmControl,
                  ("targetLkfs" .=) <$> _ansTargetLkfs,
                  ("peakCalculation" .=) <$> _ansPeakCalculation,
                  ("correctionGateLevel" .=) <$>
                    _ansCorrectionGateLevel,
                  ("algorithm" .=) <$> _ansAlgorithm,
                  ("loudnessLogging" .=) <$> _ansLoudnessLogging])

-- | Selector for Audio
--
-- /See:/ 'audioSelector' smart constructor.
data AudioSelector = AudioSelector'
  { _asTracks                 :: !(Maybe [Int])
  , _asProgramSelection       :: !(Maybe Int)
  , _asLanguageCode           :: !(Maybe LanguageCode)
  , _asOffset                 :: !(Maybe Int)
  , _asDefaultSelection       :: !(Maybe AudioDefaultSelection)
  , _asPids                   :: !(Maybe [Int])
  , _asSelectorType           :: !(Maybe AudioSelectorType)
  , _asExternalAudioFileInput :: !(Maybe Text)
  , _asRemixSettings          :: !(Maybe RemixSettings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AudioSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asTracks' - Identify the channel to include in this selector by entering the 1-based track index.  To combine several tracks, enter a comma-separated list, e.g. "1,2,3" for tracks 1-3.
--
-- * 'asProgramSelection' - Applies only when input streams contain Dolby E. Enter the program ID (according to the metadata in the audio) of the Dolby E program to extract from the specified track. One program extracted per audio selector. To select multiple programs, create multiple selectors with the same Track and different Program numbers. "All channels" means to ignore the program IDs and include all the channels in this selector; useful if metadata is known to be incorrect.
--
-- * 'asLanguageCode' - Selects a specific language code from within an audio source.
--
-- * 'asOffset' - Specifies a time delta in milliseconds to offset the audio from the input video.
--
-- * 'asDefaultSelection' - Undocumented member.
--
-- * 'asPids' - Selects a specific PID from within an audio source (e.g. 257 selects PID 0x101).
--
-- * 'asSelectorType' - Undocumented member.
--
-- * 'asExternalAudioFileInput' - Specifies audio data from an external file source.
--
-- * 'asRemixSettings' - Advanced audio remixing settings.
audioSelector
    :: AudioSelector
audioSelector =
  AudioSelector'
    { _asTracks = Nothing
    , _asProgramSelection = Nothing
    , _asLanguageCode = Nothing
    , _asOffset = Nothing
    , _asDefaultSelection = Nothing
    , _asPids = Nothing
    , _asSelectorType = Nothing
    , _asExternalAudioFileInput = Nothing
    , _asRemixSettings = Nothing
    }


-- | Identify the channel to include in this selector by entering the 1-based track index.  To combine several tracks, enter a comma-separated list, e.g. "1,2,3" for tracks 1-3.
asTracks :: Lens' AudioSelector [Int]
asTracks = lens _asTracks (\ s a -> s{_asTracks = a}) . _Default . _Coerce

-- | Applies only when input streams contain Dolby E. Enter the program ID (according to the metadata in the audio) of the Dolby E program to extract from the specified track. One program extracted per audio selector. To select multiple programs, create multiple selectors with the same Track and different Program numbers. "All channels" means to ignore the program IDs and include all the channels in this selector; useful if metadata is known to be incorrect.
asProgramSelection :: Lens' AudioSelector (Maybe Int)
asProgramSelection = lens _asProgramSelection (\ s a -> s{_asProgramSelection = a})

-- | Selects a specific language code from within an audio source.
asLanguageCode :: Lens' AudioSelector (Maybe LanguageCode)
asLanguageCode = lens _asLanguageCode (\ s a -> s{_asLanguageCode = a})

-- | Specifies a time delta in milliseconds to offset the audio from the input video.
asOffset :: Lens' AudioSelector (Maybe Int)
asOffset = lens _asOffset (\ s a -> s{_asOffset = a})

-- | Undocumented member.
asDefaultSelection :: Lens' AudioSelector (Maybe AudioDefaultSelection)
asDefaultSelection = lens _asDefaultSelection (\ s a -> s{_asDefaultSelection = a})

-- | Selects a specific PID from within an audio source (e.g. 257 selects PID 0x101).
asPids :: Lens' AudioSelector [Int]
asPids = lens _asPids (\ s a -> s{_asPids = a}) . _Default . _Coerce

-- | Undocumented member.
asSelectorType :: Lens' AudioSelector (Maybe AudioSelectorType)
asSelectorType = lens _asSelectorType (\ s a -> s{_asSelectorType = a})

-- | Specifies audio data from an external file source.
asExternalAudioFileInput :: Lens' AudioSelector (Maybe Text)
asExternalAudioFileInput = lens _asExternalAudioFileInput (\ s a -> s{_asExternalAudioFileInput = a})

-- | Advanced audio remixing settings.
asRemixSettings :: Lens' AudioSelector (Maybe RemixSettings)
asRemixSettings = lens _asRemixSettings (\ s a -> s{_asRemixSettings = a})

instance FromJSON AudioSelector where
        parseJSON
          = withObject "AudioSelector"
              (\ x ->
                 AudioSelector' <$>
                   (x .:? "tracks" .!= mempty) <*>
                     (x .:? "programSelection")
                     <*> (x .:? "languageCode")
                     <*> (x .:? "offset")
                     <*> (x .:? "defaultSelection")
                     <*> (x .:? "pids" .!= mempty)
                     <*> (x .:? "selectorType")
                     <*> (x .:? "externalAudioFileInput")
                     <*> (x .:? "remixSettings"))

instance Hashable AudioSelector where

instance NFData AudioSelector where

instance ToJSON AudioSelector where
        toJSON AudioSelector'{..}
          = object
              (catMaybes
                 [("tracks" .=) <$> _asTracks,
                  ("programSelection" .=) <$> _asProgramSelection,
                  ("languageCode" .=) <$> _asLanguageCode,
                  ("offset" .=) <$> _asOffset,
                  ("defaultSelection" .=) <$> _asDefaultSelection,
                  ("pids" .=) <$> _asPids,
                  ("selectorType" .=) <$> _asSelectorType,
                  ("externalAudioFileInput" .=) <$>
                    _asExternalAudioFileInput,
                  ("remixSettings" .=) <$> _asRemixSettings])

-- | Group of Audio Selectors
--
-- /See:/ 'audioSelectorGroup' smart constructor.
newtype AudioSelectorGroup = AudioSelectorGroup'
  { _asgAudioSelectorNames :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AudioSelectorGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgAudioSelectorNames' - Name of an "Audio Selector":#inputs-audio_selector within the same input to include in the group.  Audio selector names are standardized, based on their order within the input (e.g. "Audio Selector 1").  The audio_selector_name parameter can be repeated to add any number of audio selectors to the group.
audioSelectorGroup
    :: AudioSelectorGroup
audioSelectorGroup = AudioSelectorGroup' {_asgAudioSelectorNames = Nothing}


-- | Name of an "Audio Selector":#inputs-audio_selector within the same input to include in the group.  Audio selector names are standardized, based on their order within the input (e.g. "Audio Selector 1").  The audio_selector_name parameter can be repeated to add any number of audio selectors to the group.
asgAudioSelectorNames :: Lens' AudioSelectorGroup [Text]
asgAudioSelectorNames = lens _asgAudioSelectorNames (\ s a -> s{_asgAudioSelectorNames = a}) . _Default . _Coerce

instance FromJSON AudioSelectorGroup where
        parseJSON
          = withObject "AudioSelectorGroup"
              (\ x ->
                 AudioSelectorGroup' <$>
                   (x .:? "audioSelectorNames" .!= mempty))

instance Hashable AudioSelectorGroup where

instance NFData AudioSelectorGroup where

instance ToJSON AudioSelectorGroup where
        toJSON AudioSelectorGroup'{..}
          = object
              (catMaybes
                 [("audioSelectorNames" .=) <$>
                    _asgAudioSelectorNames])

-- | Settings for Avail Blanking
--
-- /See:/ 'availBlanking' smart constructor.
newtype AvailBlanking = AvailBlanking'
  { _abAvailBlankingImage :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailBlanking' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'abAvailBlankingImage' - Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
availBlanking
    :: AvailBlanking
availBlanking = AvailBlanking' {_abAvailBlankingImage = Nothing}


-- | Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
abAvailBlankingImage :: Lens' AvailBlanking (Maybe Text)
abAvailBlankingImage = lens _abAvailBlankingImage (\ s a -> s{_abAvailBlankingImage = a})

instance FromJSON AvailBlanking where
        parseJSON
          = withObject "AvailBlanking"
              (\ x ->
                 AvailBlanking' <$> (x .:? "availBlankingImage"))

instance Hashable AvailBlanking where

instance NFData AvailBlanking where

instance ToJSON AvailBlanking where
        toJSON AvailBlanking'{..}
          = object
              (catMaybes
                 [("availBlankingImage" .=) <$>
                    _abAvailBlankingImage])

-- | Burn-In Destination Settings.
--
-- /See:/ 'burninDestinationSettings' smart constructor.
data BurninDestinationSettings = BurninDestinationSettings'
  { _bdsBackgroundOpacity :: !(Maybe Int)
  , _bdsFontOpacity       :: !(Maybe Int)
  , _bdsShadowYOffset     :: !(Maybe Int)
  , _bdsFontResolution    :: !(Maybe Int)
  , _bdsYPosition         :: !(Maybe Int)
  , _bdsBackgroundColor   :: !(Maybe BurninSubtitleBackgroundColor)
  , _bdsShadowXOffset     :: !(Maybe Int)
  , _bdsFontSize          :: !(Maybe Int)
  , _bdsXPosition         :: !(Maybe Int)
  , _bdsTeletextSpacing   :: !(Maybe BurninSubtitleTeletextSpacing)
  , _bdsAlignment         :: !(Maybe BurninSubtitleAlignment)
  , _bdsShadowOpacity     :: !(Maybe Int)
  , _bdsOutlineColor      :: !(Maybe BurninSubtitleOutlineColor)
  , _bdsOutlineSize       :: !(Maybe Int)
  , _bdsShadowColor       :: !(Maybe BurninSubtitleShadowColor)
  , _bdsFontColor         :: !(Maybe BurninSubtitleFontColor)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BurninDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdsBackgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsFontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsShadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsFontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsYPosition' - Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsBackgroundColor' - Undocumented member.
--
-- * 'bdsShadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsFontSize' - A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsXPosition' - Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsTeletextSpacing' - Undocumented member.
--
-- * 'bdsAlignment' - Undocumented member.
--
-- * 'bdsShadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsOutlineColor' - Undocumented member.
--
-- * 'bdsOutlineSize' - Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'bdsShadowColor' - Undocumented member.
--
-- * 'bdsFontColor' - Undocumented member.
burninDestinationSettings
    :: BurninDestinationSettings
burninDestinationSettings =
  BurninDestinationSettings'
    { _bdsBackgroundOpacity = Nothing
    , _bdsFontOpacity = Nothing
    , _bdsShadowYOffset = Nothing
    , _bdsFontResolution = Nothing
    , _bdsYPosition = Nothing
    , _bdsBackgroundColor = Nothing
    , _bdsShadowXOffset = Nothing
    , _bdsFontSize = Nothing
    , _bdsXPosition = Nothing
    , _bdsTeletextSpacing = Nothing
    , _bdsAlignment = Nothing
    , _bdsShadowOpacity = Nothing
    , _bdsOutlineColor = Nothing
    , _bdsOutlineSize = Nothing
    , _bdsShadowColor = Nothing
    , _bdsFontColor = Nothing
    }


-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
bdsBackgroundOpacity :: Lens' BurninDestinationSettings (Maybe Int)
bdsBackgroundOpacity = lens _bdsBackgroundOpacity (\ s a -> s{_bdsBackgroundOpacity = a})

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent. All burn-in and DVB-Sub font settings must match.
bdsFontOpacity :: Lens' BurninDestinationSettings (Maybe Int)
bdsFontOpacity = lens _bdsFontOpacity (\ s a -> s{_bdsFontOpacity = a})

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
bdsShadowYOffset :: Lens' BurninDestinationSettings (Maybe Int)
bdsShadowYOffset = lens _bdsShadowYOffset (\ s a -> s{_bdsShadowYOffset = a})

-- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in and DVB-Sub font settings must match.
bdsFontResolution :: Lens' BurninDestinationSettings (Maybe Int)
bdsFontResolution = lens _bdsFontResolution (\ s a -> s{_bdsFontResolution = a})

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bdsYPosition :: Lens' BurninDestinationSettings (Maybe Int)
bdsYPosition = lens _bdsYPosition (\ s a -> s{_bdsYPosition = a})

-- | Undocumented member.
bdsBackgroundColor :: Lens' BurninDestinationSettings (Maybe BurninSubtitleBackgroundColor)
bdsBackgroundColor = lens _bdsBackgroundColor (\ s a -> s{_bdsBackgroundColor = a})

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
bdsShadowXOffset :: Lens' BurninDestinationSettings (Maybe Int)
bdsShadowXOffset = lens _bdsShadowXOffset (\ s a -> s{_bdsShadowXOffset = a})

-- | A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
bdsFontSize :: Lens' BurninDestinationSettings (Maybe Int)
bdsFontSize = lens _bdsFontSize (\ s a -> s{_bdsFontSize = a})

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bdsXPosition :: Lens' BurninDestinationSettings (Maybe Int)
bdsXPosition = lens _bdsXPosition (\ s a -> s{_bdsXPosition = a})

-- | Undocumented member.
bdsTeletextSpacing :: Lens' BurninDestinationSettings (Maybe BurninSubtitleTeletextSpacing)
bdsTeletextSpacing = lens _bdsTeletextSpacing (\ s a -> s{_bdsTeletextSpacing = a})

-- | Undocumented member.
bdsAlignment :: Lens' BurninDestinationSettings (Maybe BurninSubtitleAlignment)
bdsAlignment = lens _bdsAlignment (\ s a -> s{_bdsAlignment = a})

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
bdsShadowOpacity :: Lens' BurninDestinationSettings (Maybe Int)
bdsShadowOpacity = lens _bdsShadowOpacity (\ s a -> s{_bdsShadowOpacity = a})

-- | Undocumented member.
bdsOutlineColor :: Lens' BurninDestinationSettings (Maybe BurninSubtitleOutlineColor)
bdsOutlineColor = lens _bdsOutlineColor (\ s a -> s{_bdsOutlineColor = a})

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bdsOutlineSize :: Lens' BurninDestinationSettings (Maybe Int)
bdsOutlineSize = lens _bdsOutlineSize (\ s a -> s{_bdsOutlineSize = a})

-- | Undocumented member.
bdsShadowColor :: Lens' BurninDestinationSettings (Maybe BurninSubtitleShadowColor)
bdsShadowColor = lens _bdsShadowColor (\ s a -> s{_bdsShadowColor = a})

-- | Undocumented member.
bdsFontColor :: Lens' BurninDestinationSettings (Maybe BurninSubtitleFontColor)
bdsFontColor = lens _bdsFontColor (\ s a -> s{_bdsFontColor = a})

instance FromJSON BurninDestinationSettings where
        parseJSON
          = withObject "BurninDestinationSettings"
              (\ x ->
                 BurninDestinationSettings' <$>
                   (x .:? "backgroundOpacity") <*> (x .:? "fontOpacity")
                     <*> (x .:? "shadowYOffset")
                     <*> (x .:? "fontResolution")
                     <*> (x .:? "yPosition")
                     <*> (x .:? "backgroundColor")
                     <*> (x .:? "shadowXOffset")
                     <*> (x .:? "fontSize")
                     <*> (x .:? "xPosition")
                     <*> (x .:? "teletextSpacing")
                     <*> (x .:? "alignment")
                     <*> (x .:? "shadowOpacity")
                     <*> (x .:? "outlineColor")
                     <*> (x .:? "outlineSize")
                     <*> (x .:? "shadowColor")
                     <*> (x .:? "fontColor"))

instance Hashable BurninDestinationSettings where

instance NFData BurninDestinationSettings where

instance ToJSON BurninDestinationSettings where
        toJSON BurninDestinationSettings'{..}
          = object
              (catMaybes
                 [("backgroundOpacity" .=) <$> _bdsBackgroundOpacity,
                  ("fontOpacity" .=) <$> _bdsFontOpacity,
                  ("shadowYOffset" .=) <$> _bdsShadowYOffset,
                  ("fontResolution" .=) <$> _bdsFontResolution,
                  ("yPosition" .=) <$> _bdsYPosition,
                  ("backgroundColor" .=) <$> _bdsBackgroundColor,
                  ("shadowXOffset" .=) <$> _bdsShadowXOffset,
                  ("fontSize" .=) <$> _bdsFontSize,
                  ("xPosition" .=) <$> _bdsXPosition,
                  ("teletextSpacing" .=) <$> _bdsTeletextSpacing,
                  ("alignment" .=) <$> _bdsAlignment,
                  ("shadowOpacity" .=) <$> _bdsShadowOpacity,
                  ("outlineColor" .=) <$> _bdsOutlineColor,
                  ("outlineSize" .=) <$> _bdsOutlineSize,
                  ("shadowColor" .=) <$> _bdsShadowColor,
                  ("fontColor" .=) <$> _bdsFontColor])

-- | Description of Caption output
--
-- /See:/ 'captionDescription' smart constructor.
data CaptionDescription = CaptionDescription'
  { _cdCaptionSelectorName :: !(Maybe Text)
  , _cdLanguageCode        :: !(Maybe LanguageCode)
  , _cdDestinationSettings :: !(Maybe CaptionDestinationSettings)
  , _cdLanguageDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CaptionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdCaptionSelectorName' - <N>", which denotes that the Nth Caption Selector will be used from each input.
--
-- * 'cdLanguageCode' - Indicates the language of the caption output track.
--
-- * 'cdDestinationSettings' - Undocumented member.
--
-- * 'cdLanguageDescription' - Human readable information to indicate captions available for players (eg. English, or Spanish). Alphanumeric characters, spaces, and underscore are legal.
captionDescription
    :: CaptionDescription
captionDescription =
  CaptionDescription'
    { _cdCaptionSelectorName = Nothing
    , _cdLanguageCode = Nothing
    , _cdDestinationSettings = Nothing
    , _cdLanguageDescription = Nothing
    }


-- | <N>", which denotes that the Nth Caption Selector will be used from each input.
cdCaptionSelectorName :: Lens' CaptionDescription (Maybe Text)
cdCaptionSelectorName = lens _cdCaptionSelectorName (\ s a -> s{_cdCaptionSelectorName = a})

-- | Indicates the language of the caption output track.
cdLanguageCode :: Lens' CaptionDescription (Maybe LanguageCode)
cdLanguageCode = lens _cdLanguageCode (\ s a -> s{_cdLanguageCode = a})

-- | Undocumented member.
cdDestinationSettings :: Lens' CaptionDescription (Maybe CaptionDestinationSettings)
cdDestinationSettings = lens _cdDestinationSettings (\ s a -> s{_cdDestinationSettings = a})

-- | Human readable information to indicate captions available for players (eg. English, or Spanish). Alphanumeric characters, spaces, and underscore are legal.
cdLanguageDescription :: Lens' CaptionDescription (Maybe Text)
cdLanguageDescription = lens _cdLanguageDescription (\ s a -> s{_cdLanguageDescription = a})

instance FromJSON CaptionDescription where
        parseJSON
          = withObject "CaptionDescription"
              (\ x ->
                 CaptionDescription' <$>
                   (x .:? "captionSelectorName") <*>
                     (x .:? "languageCode")
                     <*> (x .:? "destinationSettings")
                     <*> (x .:? "languageDescription"))

instance Hashable CaptionDescription where

instance NFData CaptionDescription where

instance ToJSON CaptionDescription where
        toJSON CaptionDescription'{..}
          = object
              (catMaybes
                 [("captionSelectorName" .=) <$>
                    _cdCaptionSelectorName,
                  ("languageCode" .=) <$> _cdLanguageCode,
                  ("destinationSettings" .=) <$>
                    _cdDestinationSettings,
                  ("languageDescription" .=) <$>
                    _cdLanguageDescription])

-- | Caption Description for preset
--
-- /See:/ 'captionDescriptionPreset' smart constructor.
data CaptionDescriptionPreset = CaptionDescriptionPreset'
  { _cdpLanguageCode        :: !(Maybe LanguageCode)
  , _cdpDestinationSettings :: !(Maybe CaptionDestinationSettings)
  , _cdpLanguageDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CaptionDescriptionPreset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdpLanguageCode' - Indicates the language of the caption output track.
--
-- * 'cdpDestinationSettings' - Undocumented member.
--
-- * 'cdpLanguageDescription' - Human readable information to indicate captions available for players (eg. English, or Spanish). Alphanumeric characters, spaces, and underscore are legal.
captionDescriptionPreset
    :: CaptionDescriptionPreset
captionDescriptionPreset =
  CaptionDescriptionPreset'
    { _cdpLanguageCode = Nothing
    , _cdpDestinationSettings = Nothing
    , _cdpLanguageDescription = Nothing
    }


-- | Indicates the language of the caption output track.
cdpLanguageCode :: Lens' CaptionDescriptionPreset (Maybe LanguageCode)
cdpLanguageCode = lens _cdpLanguageCode (\ s a -> s{_cdpLanguageCode = a})

-- | Undocumented member.
cdpDestinationSettings :: Lens' CaptionDescriptionPreset (Maybe CaptionDestinationSettings)
cdpDestinationSettings = lens _cdpDestinationSettings (\ s a -> s{_cdpDestinationSettings = a})

-- | Human readable information to indicate captions available for players (eg. English, or Spanish). Alphanumeric characters, spaces, and underscore are legal.
cdpLanguageDescription :: Lens' CaptionDescriptionPreset (Maybe Text)
cdpLanguageDescription = lens _cdpLanguageDescription (\ s a -> s{_cdpLanguageDescription = a})

instance FromJSON CaptionDescriptionPreset where
        parseJSON
          = withObject "CaptionDescriptionPreset"
              (\ x ->
                 CaptionDescriptionPreset' <$>
                   (x .:? "languageCode") <*>
                     (x .:? "destinationSettings")
                     <*> (x .:? "languageDescription"))

instance Hashable CaptionDescriptionPreset where

instance NFData CaptionDescriptionPreset where

instance ToJSON CaptionDescriptionPreset where
        toJSON CaptionDescriptionPreset'{..}
          = object
              (catMaybes
                 [("languageCode" .=) <$> _cdpLanguageCode,
                  ("destinationSettings" .=) <$>
                    _cdpDestinationSettings,
                  ("languageDescription" .=) <$>
                    _cdpLanguageDescription])

-- | Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.
--
-- /See:/ 'captionDestinationSettings' smart constructor.
data CaptionDestinationSettings = CaptionDestinationSettings'
  { _cdsTeletextDestinationSettings :: !(Maybe TeletextDestinationSettings)
  , _cdsDvbSubDestinationSettings   :: !(Maybe DvbSubDestinationSettings)
  , _cdsTtmlDestinationSettings     :: !(Maybe TtmlDestinationSettings)
  , _cdsDestinationType             :: !(Maybe CaptionDestinationType)
  , _cdsSccDestinationSettings      :: !(Maybe SccDestinationSettings)
  , _cdsBurninDestinationSettings   :: !(Maybe BurninDestinationSettings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CaptionDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsTeletextDestinationSettings' - Undocumented member.
--
-- * 'cdsDvbSubDestinationSettings' - Undocumented member.
--
-- * 'cdsTtmlDestinationSettings' - Undocumented member.
--
-- * 'cdsDestinationType' - Undocumented member.
--
-- * 'cdsSccDestinationSettings' - Undocumented member.
--
-- * 'cdsBurninDestinationSettings' - Undocumented member.
captionDestinationSettings
    :: CaptionDestinationSettings
captionDestinationSettings =
  CaptionDestinationSettings'
    { _cdsTeletextDestinationSettings = Nothing
    , _cdsDvbSubDestinationSettings = Nothing
    , _cdsTtmlDestinationSettings = Nothing
    , _cdsDestinationType = Nothing
    , _cdsSccDestinationSettings = Nothing
    , _cdsBurninDestinationSettings = Nothing
    }


-- | Undocumented member.
cdsTeletextDestinationSettings :: Lens' CaptionDestinationSettings (Maybe TeletextDestinationSettings)
cdsTeletextDestinationSettings = lens _cdsTeletextDestinationSettings (\ s a -> s{_cdsTeletextDestinationSettings = a})

-- | Undocumented member.
cdsDvbSubDestinationSettings :: Lens' CaptionDestinationSettings (Maybe DvbSubDestinationSettings)
cdsDvbSubDestinationSettings = lens _cdsDvbSubDestinationSettings (\ s a -> s{_cdsDvbSubDestinationSettings = a})

-- | Undocumented member.
cdsTtmlDestinationSettings :: Lens' CaptionDestinationSettings (Maybe TtmlDestinationSettings)
cdsTtmlDestinationSettings = lens _cdsTtmlDestinationSettings (\ s a -> s{_cdsTtmlDestinationSettings = a})

-- | Undocumented member.
cdsDestinationType :: Lens' CaptionDestinationSettings (Maybe CaptionDestinationType)
cdsDestinationType = lens _cdsDestinationType (\ s a -> s{_cdsDestinationType = a})

-- | Undocumented member.
cdsSccDestinationSettings :: Lens' CaptionDestinationSettings (Maybe SccDestinationSettings)
cdsSccDestinationSettings = lens _cdsSccDestinationSettings (\ s a -> s{_cdsSccDestinationSettings = a})

-- | Undocumented member.
cdsBurninDestinationSettings :: Lens' CaptionDestinationSettings (Maybe BurninDestinationSettings)
cdsBurninDestinationSettings = lens _cdsBurninDestinationSettings (\ s a -> s{_cdsBurninDestinationSettings = a})

instance FromJSON CaptionDestinationSettings where
        parseJSON
          = withObject "CaptionDestinationSettings"
              (\ x ->
                 CaptionDestinationSettings' <$>
                   (x .:? "teletextDestinationSettings") <*>
                     (x .:? "dvbSubDestinationSettings")
                     <*> (x .:? "ttmlDestinationSettings")
                     <*> (x .:? "destinationType")
                     <*> (x .:? "sccDestinationSettings")
                     <*> (x .:? "burninDestinationSettings"))

instance Hashable CaptionDestinationSettings where

instance NFData CaptionDestinationSettings where

instance ToJSON CaptionDestinationSettings where
        toJSON CaptionDestinationSettings'{..}
          = object
              (catMaybes
                 [("teletextDestinationSettings" .=) <$>
                    _cdsTeletextDestinationSettings,
                  ("dvbSubDestinationSettings" .=) <$>
                    _cdsDvbSubDestinationSettings,
                  ("ttmlDestinationSettings" .=) <$>
                    _cdsTtmlDestinationSettings,
                  ("destinationType" .=) <$> _cdsDestinationType,
                  ("sccDestinationSettings" .=) <$>
                    _cdsSccDestinationSettings,
                  ("burninDestinationSettings" .=) <$>
                    _cdsBurninDestinationSettings])

-- | Caption inputs to be mapped to caption outputs.
--
-- /See:/ 'captionSelector' smart constructor.
data CaptionSelector = CaptionSelector'
  { _csLanguageCode   :: !(Maybe LanguageCode)
  , _csSourceSettings :: !(Maybe CaptionSourceSettings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CaptionSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csLanguageCode' - The specific language to extract from source. If input is SCTE-27, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub that is being passed through, omit this field (and PID field); there is no way to extract a specific language with pass-through captions.
--
-- * 'csSourceSettings' - Undocumented member.
captionSelector
    :: CaptionSelector
captionSelector =
  CaptionSelector' {_csLanguageCode = Nothing, _csSourceSettings = Nothing}


-- | The specific language to extract from source. If input is SCTE-27, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub that is being passed through, omit this field (and PID field); there is no way to extract a specific language with pass-through captions.
csLanguageCode :: Lens' CaptionSelector (Maybe LanguageCode)
csLanguageCode = lens _csLanguageCode (\ s a -> s{_csLanguageCode = a})

-- | Undocumented member.
csSourceSettings :: Lens' CaptionSelector (Maybe CaptionSourceSettings)
csSourceSettings = lens _csSourceSettings (\ s a -> s{_csSourceSettings = a})

instance FromJSON CaptionSelector where
        parseJSON
          = withObject "CaptionSelector"
              (\ x ->
                 CaptionSelector' <$>
                   (x .:? "languageCode") <*> (x .:? "sourceSettings"))

instance Hashable CaptionSelector where

instance NFData CaptionSelector where

instance ToJSON CaptionSelector where
        toJSON CaptionSelector'{..}
          = object
              (catMaybes
                 [("languageCode" .=) <$> _csLanguageCode,
                  ("sourceSettings" .=) <$> _csSourceSettings])

-- | Source settings (SourceSettings) contains the group of settings for captions in the input.
--
-- /See:/ 'captionSourceSettings' smart constructor.
data CaptionSourceSettings = CaptionSourceSettings'
  { _cssTeletextSourceSettings  :: !(Maybe TeletextSourceSettings)
  , _cssSourceType              :: !(Maybe CaptionSourceType)
  , _cssFileSourceSettings      :: !(Maybe FileSourceSettings)
  , _cssDvbSubSourceSettings    :: !(Maybe DvbSubSourceSettings)
  , _cssAncillarySourceSettings :: !(Maybe AncillarySourceSettings)
  , _cssEmbeddedSourceSettings  :: !(Maybe EmbeddedSourceSettings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CaptionSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssTeletextSourceSettings' - Undocumented member.
--
-- * 'cssSourceType' - Undocumented member.
--
-- * 'cssFileSourceSettings' - Undocumented member.
--
-- * 'cssDvbSubSourceSettings' - Undocumented member.
--
-- * 'cssAncillarySourceSettings' - Undocumented member.
--
-- * 'cssEmbeddedSourceSettings' - Undocumented member.
captionSourceSettings
    :: CaptionSourceSettings
captionSourceSettings =
  CaptionSourceSettings'
    { _cssTeletextSourceSettings = Nothing
    , _cssSourceType = Nothing
    , _cssFileSourceSettings = Nothing
    , _cssDvbSubSourceSettings = Nothing
    , _cssAncillarySourceSettings = Nothing
    , _cssEmbeddedSourceSettings = Nothing
    }


-- | Undocumented member.
cssTeletextSourceSettings :: Lens' CaptionSourceSettings (Maybe TeletextSourceSettings)
cssTeletextSourceSettings = lens _cssTeletextSourceSettings (\ s a -> s{_cssTeletextSourceSettings = a})

-- | Undocumented member.
cssSourceType :: Lens' CaptionSourceSettings (Maybe CaptionSourceType)
cssSourceType = lens _cssSourceType (\ s a -> s{_cssSourceType = a})

-- | Undocumented member.
cssFileSourceSettings :: Lens' CaptionSourceSettings (Maybe FileSourceSettings)
cssFileSourceSettings = lens _cssFileSourceSettings (\ s a -> s{_cssFileSourceSettings = a})

-- | Undocumented member.
cssDvbSubSourceSettings :: Lens' CaptionSourceSettings (Maybe DvbSubSourceSettings)
cssDvbSubSourceSettings = lens _cssDvbSubSourceSettings (\ s a -> s{_cssDvbSubSourceSettings = a})

-- | Undocumented member.
cssAncillarySourceSettings :: Lens' CaptionSourceSettings (Maybe AncillarySourceSettings)
cssAncillarySourceSettings = lens _cssAncillarySourceSettings (\ s a -> s{_cssAncillarySourceSettings = a})

-- | Undocumented member.
cssEmbeddedSourceSettings :: Lens' CaptionSourceSettings (Maybe EmbeddedSourceSettings)
cssEmbeddedSourceSettings = lens _cssEmbeddedSourceSettings (\ s a -> s{_cssEmbeddedSourceSettings = a})

instance FromJSON CaptionSourceSettings where
        parseJSON
          = withObject "CaptionSourceSettings"
              (\ x ->
                 CaptionSourceSettings' <$>
                   (x .:? "teletextSourceSettings") <*>
                     (x .:? "sourceType")
                     <*> (x .:? "fileSourceSettings")
                     <*> (x .:? "dvbSubSourceSettings")
                     <*> (x .:? "ancillarySourceSettings")
                     <*> (x .:? "embeddedSourceSettings"))

instance Hashable CaptionSourceSettings where

instance NFData CaptionSourceSettings where

instance ToJSON CaptionSourceSettings where
        toJSON CaptionSourceSettings'{..}
          = object
              (catMaybes
                 [("teletextSourceSettings" .=) <$>
                    _cssTeletextSourceSettings,
                  ("sourceType" .=) <$> _cssSourceType,
                  ("fileSourceSettings" .=) <$> _cssFileSourceSettings,
                  ("dvbSubSourceSettings" .=) <$>
                    _cssDvbSubSourceSettings,
                  ("ancillarySourceSettings" .=) <$>
                    _cssAncillarySourceSettings,
                  ("embeddedSourceSettings" .=) <$>
                    _cssEmbeddedSourceSettings])

-- | Channel mapping (ChannelMapping) contains the group of fields that hold the remixing value for each channel. Units are in dB. Acceptable values are within the range from -60 (mute) through 6. A setting of 0 passes the input channel unchanged to the output channel (no attenuation or amplification).
--
-- /See:/ 'channelMapping' smart constructor.
newtype ChannelMapping = ChannelMapping'
  { _cmOutputChannels :: Maybe [OutputChannelMapping]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChannelMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmOutputChannels' - List of output channels
channelMapping
    :: ChannelMapping
channelMapping = ChannelMapping' {_cmOutputChannels = Nothing}


-- | List of output channels
cmOutputChannels :: Lens' ChannelMapping [OutputChannelMapping]
cmOutputChannels = lens _cmOutputChannels (\ s a -> s{_cmOutputChannels = a}) . _Default . _Coerce

instance FromJSON ChannelMapping where
        parseJSON
          = withObject "ChannelMapping"
              (\ x ->
                 ChannelMapping' <$>
                   (x .:? "outputChannels" .!= mempty))

instance Hashable ChannelMapping where

instance NFData ChannelMapping where

instance ToJSON ChannelMapping where
        toJSON ChannelMapping'{..}
          = object
              (catMaybes
                 [("outputChannels" .=) <$> _cmOutputChannels])

-- | Settings for color correction.
--
-- /See:/ 'colorCorrector' smart constructor.
data ColorCorrector = ColorCorrector'
  { _ccSaturation           :: !(Maybe Int)
  , _ccHue                  :: !(Maybe Int)
  , _ccColorSpaceConversion :: !(Maybe ColorSpaceConversion)
  , _ccHdr10Metadata        :: !(Maybe Hdr10Metadata)
  , _ccContrast             :: !(Maybe Int)
  , _ccBrightness           :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ColorCorrector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccSaturation' - Saturation level.
--
-- * 'ccHue' - Hue in degrees.
--
-- * 'ccColorSpaceConversion' - Undocumented member.
--
-- * 'ccHdr10Metadata' - Undocumented member.
--
-- * 'ccContrast' - Contrast level.
--
-- * 'ccBrightness' - Brightness level.
colorCorrector
    :: ColorCorrector
colorCorrector =
  ColorCorrector'
    { _ccSaturation = Nothing
    , _ccHue = Nothing
    , _ccColorSpaceConversion = Nothing
    , _ccHdr10Metadata = Nothing
    , _ccContrast = Nothing
    , _ccBrightness = Nothing
    }


-- | Saturation level.
ccSaturation :: Lens' ColorCorrector (Maybe Int)
ccSaturation = lens _ccSaturation (\ s a -> s{_ccSaturation = a})

-- | Hue in degrees.
ccHue :: Lens' ColorCorrector (Maybe Int)
ccHue = lens _ccHue (\ s a -> s{_ccHue = a})

-- | Undocumented member.
ccColorSpaceConversion :: Lens' ColorCorrector (Maybe ColorSpaceConversion)
ccColorSpaceConversion = lens _ccColorSpaceConversion (\ s a -> s{_ccColorSpaceConversion = a})

-- | Undocumented member.
ccHdr10Metadata :: Lens' ColorCorrector (Maybe Hdr10Metadata)
ccHdr10Metadata = lens _ccHdr10Metadata (\ s a -> s{_ccHdr10Metadata = a})

-- | Contrast level.
ccContrast :: Lens' ColorCorrector (Maybe Int)
ccContrast = lens _ccContrast (\ s a -> s{_ccContrast = a})

-- | Brightness level.
ccBrightness :: Lens' ColorCorrector (Maybe Int)
ccBrightness = lens _ccBrightness (\ s a -> s{_ccBrightness = a})

instance FromJSON ColorCorrector where
        parseJSON
          = withObject "ColorCorrector"
              (\ x ->
                 ColorCorrector' <$>
                   (x .:? "saturation") <*> (x .:? "hue") <*>
                     (x .:? "colorSpaceConversion")
                     <*> (x .:? "hdr10Metadata")
                     <*> (x .:? "contrast")
                     <*> (x .:? "brightness"))

instance Hashable ColorCorrector where

instance NFData ColorCorrector where

instance ToJSON ColorCorrector where
        toJSON ColorCorrector'{..}
          = object
              (catMaybes
                 [("saturation" .=) <$> _ccSaturation,
                  ("hue" .=) <$> _ccHue,
                  ("colorSpaceConversion" .=) <$>
                    _ccColorSpaceConversion,
                  ("hdr10Metadata" .=) <$> _ccHdr10Metadata,
                  ("contrast" .=) <$> _ccContrast,
                  ("brightness" .=) <$> _ccBrightness])

-- | Container specific settings.
--
-- /See:/ 'containerSettings' smart constructor.
data ContainerSettings = ContainerSettings'
  { _csM2tsSettings :: !(Maybe M2tsSettings)
  , _csM3u8Settings :: !(Maybe M3u8Settings)
  , _csMovSettings  :: !(Maybe MovSettings)
  , _csMp4Settings  :: !(Maybe Mp4Settings)
  , _csContainer    :: !(Maybe ContainerType)
  , _csF4vSettings  :: !(Maybe F4vSettings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContainerSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csM2tsSettings' - Undocumented member.
--
-- * 'csM3u8Settings' - Undocumented member.
--
-- * 'csMovSettings' - Undocumented member.
--
-- * 'csMp4Settings' - Undocumented member.
--
-- * 'csContainer' - Undocumented member.
--
-- * 'csF4vSettings' - Undocumented member.
containerSettings
    :: ContainerSettings
containerSettings =
  ContainerSettings'
    { _csM2tsSettings = Nothing
    , _csM3u8Settings = Nothing
    , _csMovSettings = Nothing
    , _csMp4Settings = Nothing
    , _csContainer = Nothing
    , _csF4vSettings = Nothing
    }


-- | Undocumented member.
csM2tsSettings :: Lens' ContainerSettings (Maybe M2tsSettings)
csM2tsSettings = lens _csM2tsSettings (\ s a -> s{_csM2tsSettings = a})

-- | Undocumented member.
csM3u8Settings :: Lens' ContainerSettings (Maybe M3u8Settings)
csM3u8Settings = lens _csM3u8Settings (\ s a -> s{_csM3u8Settings = a})

-- | Undocumented member.
csMovSettings :: Lens' ContainerSettings (Maybe MovSettings)
csMovSettings = lens _csMovSettings (\ s a -> s{_csMovSettings = a})

-- | Undocumented member.
csMp4Settings :: Lens' ContainerSettings (Maybe Mp4Settings)
csMp4Settings = lens _csMp4Settings (\ s a -> s{_csMp4Settings = a})

-- | Undocumented member.
csContainer :: Lens' ContainerSettings (Maybe ContainerType)
csContainer = lens _csContainer (\ s a -> s{_csContainer = a})

-- | Undocumented member.
csF4vSettings :: Lens' ContainerSettings (Maybe F4vSettings)
csF4vSettings = lens _csF4vSettings (\ s a -> s{_csF4vSettings = a})

instance FromJSON ContainerSettings where
        parseJSON
          = withObject "ContainerSettings"
              (\ x ->
                 ContainerSettings' <$>
                   (x .:? "m2tsSettings") <*> (x .:? "m3u8Settings") <*>
                     (x .:? "movSettings")
                     <*> (x .:? "mp4Settings")
                     <*> (x .:? "container")
                     <*> (x .:? "f4vSettings"))

instance Hashable ContainerSettings where

instance NFData ContainerSettings where

instance ToJSON ContainerSettings where
        toJSON ContainerSettings'{..}
          = object
              (catMaybes
                 [("m2tsSettings" .=) <$> _csM2tsSettings,
                  ("m3u8Settings" .=) <$> _csM3u8Settings,
                  ("movSettings" .=) <$> _csMovSettings,
                  ("mp4Settings" .=) <$> _csMp4Settings,
                  ("container" .=) <$> _csContainer,
                  ("f4vSettings" .=) <$> _csF4vSettings])

-- | Specifies DRM settings for DASH outputs.
--
-- /See:/ 'dashIsoEncryptionSettings' smart constructor.
newtype DashIsoEncryptionSettings = DashIsoEncryptionSettings'
  { _diesSpekeKeyProvider :: Maybe SpekeKeyProvider
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DashIsoEncryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diesSpekeKeyProvider' - Undocumented member.
dashIsoEncryptionSettings
    :: DashIsoEncryptionSettings
dashIsoEncryptionSettings =
  DashIsoEncryptionSettings' {_diesSpekeKeyProvider = Nothing}


-- | Undocumented member.
diesSpekeKeyProvider :: Lens' DashIsoEncryptionSettings (Maybe SpekeKeyProvider)
diesSpekeKeyProvider = lens _diesSpekeKeyProvider (\ s a -> s{_diesSpekeKeyProvider = a})

instance FromJSON DashIsoEncryptionSettings where
        parseJSON
          = withObject "DashIsoEncryptionSettings"
              (\ x ->
                 DashIsoEncryptionSettings' <$>
                   (x .:? "spekeKeyProvider"))

instance Hashable DashIsoEncryptionSettings where

instance NFData DashIsoEncryptionSettings where

instance ToJSON DashIsoEncryptionSettings where
        toJSON DashIsoEncryptionSettings'{..}
          = object
              (catMaybes
                 [("spekeKeyProvider" .=) <$> _diesSpekeKeyProvider])

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to DASH_ISO_GROUP_SETTINGS.
--
-- /See:/ 'dashIsoGroupSettings' smart constructor.
data DashIsoGroupSettings = DashIsoGroupSettings'
  { _digsFragmentLength  :: !(Maybe Int)
  , _digsSegmentControl  :: !(Maybe DashIsoSegmentControl)
  , _digsDestination     :: !(Maybe Text)
  , _digsHbbtvCompliance :: !(Maybe DashIsoHbbtvCompliance)
  , _digsMinBufferTime   :: !(Maybe Int)
  , _digsBaseURL         :: !(Maybe Text)
  , _digsEncryption      :: !(Maybe DashIsoEncryptionSettings)
  , _digsSegmentLength   :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DashIsoGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'digsFragmentLength' - Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
--
-- * 'digsSegmentControl' - Undocumented member.
--
-- * 'digsDestination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- * 'digsHbbtvCompliance' - Undocumented member.
--
-- * 'digsMinBufferTime' - Minimum time of initially buffered media that is needed to ensure smooth playout.
--
-- * 'digsBaseURL' - A partial URI prefix that will be put in the manifest (.mpd) file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
--
-- * 'digsEncryption' - DRM settings.
--
-- * 'digsSegmentLength' - Length of mpd segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer. When Emit Single File is checked, the segmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
dashIsoGroupSettings
    :: DashIsoGroupSettings
dashIsoGroupSettings =
  DashIsoGroupSettings'
    { _digsFragmentLength = Nothing
    , _digsSegmentControl = Nothing
    , _digsDestination = Nothing
    , _digsHbbtvCompliance = Nothing
    , _digsMinBufferTime = Nothing
    , _digsBaseURL = Nothing
    , _digsEncryption = Nothing
    , _digsSegmentLength = Nothing
    }


-- | Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
digsFragmentLength :: Lens' DashIsoGroupSettings (Maybe Int)
digsFragmentLength = lens _digsFragmentLength (\ s a -> s{_digsFragmentLength = a})

-- | Undocumented member.
digsSegmentControl :: Lens' DashIsoGroupSettings (Maybe DashIsoSegmentControl)
digsSegmentControl = lens _digsSegmentControl (\ s a -> s{_digsSegmentControl = a})

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
digsDestination :: Lens' DashIsoGroupSettings (Maybe Text)
digsDestination = lens _digsDestination (\ s a -> s{_digsDestination = a})

-- | Undocumented member.
digsHbbtvCompliance :: Lens' DashIsoGroupSettings (Maybe DashIsoHbbtvCompliance)
digsHbbtvCompliance = lens _digsHbbtvCompliance (\ s a -> s{_digsHbbtvCompliance = a})

-- | Minimum time of initially buffered media that is needed to ensure smooth playout.
digsMinBufferTime :: Lens' DashIsoGroupSettings (Maybe Int)
digsMinBufferTime = lens _digsMinBufferTime (\ s a -> s{_digsMinBufferTime = a})

-- | A partial URI prefix that will be put in the manifest (.mpd) file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
digsBaseURL :: Lens' DashIsoGroupSettings (Maybe Text)
digsBaseURL = lens _digsBaseURL (\ s a -> s{_digsBaseURL = a})

-- | DRM settings.
digsEncryption :: Lens' DashIsoGroupSettings (Maybe DashIsoEncryptionSettings)
digsEncryption = lens _digsEncryption (\ s a -> s{_digsEncryption = a})

-- | Length of mpd segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer. When Emit Single File is checked, the segmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
digsSegmentLength :: Lens' DashIsoGroupSettings (Maybe Int)
digsSegmentLength = lens _digsSegmentLength (\ s a -> s{_digsSegmentLength = a})

instance FromJSON DashIsoGroupSettings where
        parseJSON
          = withObject "DashIsoGroupSettings"
              (\ x ->
                 DashIsoGroupSettings' <$>
                   (x .:? "fragmentLength") <*> (x .:? "segmentControl")
                     <*> (x .:? "destination")
                     <*> (x .:? "hbbtvCompliance")
                     <*> (x .:? "minBufferTime")
                     <*> (x .:? "baseUrl")
                     <*> (x .:? "encryption")
                     <*> (x .:? "segmentLength"))

instance Hashable DashIsoGroupSettings where

instance NFData DashIsoGroupSettings where

instance ToJSON DashIsoGroupSettings where
        toJSON DashIsoGroupSettings'{..}
          = object
              (catMaybes
                 [("fragmentLength" .=) <$> _digsFragmentLength,
                  ("segmentControl" .=) <$> _digsSegmentControl,
                  ("destination" .=) <$> _digsDestination,
                  ("hbbtvCompliance" .=) <$> _digsHbbtvCompliance,
                  ("minBufferTime" .=) <$> _digsMinBufferTime,
                  ("baseUrl" .=) <$> _digsBaseURL,
                  ("encryption" .=) <$> _digsEncryption,
                  ("segmentLength" .=) <$> _digsSegmentLength])

-- | Settings for deinterlacer
--
-- /See:/ 'deinterlacer' smart constructor.
data Deinterlacer = Deinterlacer'
  { _dControl   :: !(Maybe DeinterlacerControl)
  , _dMode      :: !(Maybe DeinterlacerMode)
  , _dAlgorithm :: !(Maybe DeinterlaceAlgorithm)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Deinterlacer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dControl' - Undocumented member.
--
-- * 'dMode' - Undocumented member.
--
-- * 'dAlgorithm' - Undocumented member.
deinterlacer
    :: Deinterlacer
deinterlacer =
  Deinterlacer' {_dControl = Nothing, _dMode = Nothing, _dAlgorithm = Nothing}


-- | Undocumented member.
dControl :: Lens' Deinterlacer (Maybe DeinterlacerControl)
dControl = lens _dControl (\ s a -> s{_dControl = a})

-- | Undocumented member.
dMode :: Lens' Deinterlacer (Maybe DeinterlacerMode)
dMode = lens _dMode (\ s a -> s{_dMode = a})

-- | Undocumented member.
dAlgorithm :: Lens' Deinterlacer (Maybe DeinterlaceAlgorithm)
dAlgorithm = lens _dAlgorithm (\ s a -> s{_dAlgorithm = a})

instance FromJSON Deinterlacer where
        parseJSON
          = withObject "Deinterlacer"
              (\ x ->
                 Deinterlacer' <$>
                   (x .:? "control") <*> (x .:? "mode") <*>
                     (x .:? "algorithm"))

instance Hashable Deinterlacer where

instance NFData Deinterlacer where

instance ToJSON Deinterlacer where
        toJSON Deinterlacer'{..}
          = object
              (catMaybes
                 [("control" .=) <$> _dControl,
                  ("mode" .=) <$> _dMode,
                  ("algorithm" .=) <$> _dAlgorithm])

-- | Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
--
-- /See:/ 'dvbNitSettings' smart constructor.
data DvbNitSettings = DvbNitSettings'
  { _dnsNetworkId   :: !(Maybe Int)
  , _dnsNetworkName :: !(Maybe Text)
  , _dnsNitInterval :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DvbNitSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnsNetworkId' - The numeric value placed in the Network Information Table (NIT).
--
-- * 'dnsNetworkName' - The network name text placed in the network_name_descriptor inside the Network Information Table. Maximum length is 256 characters.
--
-- * 'dnsNitInterval' - The number of milliseconds between instances of this table in the output transport stream.
dvbNitSettings
    :: DvbNitSettings
dvbNitSettings =
  DvbNitSettings'
    { _dnsNetworkId = Nothing
    , _dnsNetworkName = Nothing
    , _dnsNitInterval = Nothing
    }


-- | The numeric value placed in the Network Information Table (NIT).
dnsNetworkId :: Lens' DvbNitSettings (Maybe Int)
dnsNetworkId = lens _dnsNetworkId (\ s a -> s{_dnsNetworkId = a})

-- | The network name text placed in the network_name_descriptor inside the Network Information Table. Maximum length is 256 characters.
dnsNetworkName :: Lens' DvbNitSettings (Maybe Text)
dnsNetworkName = lens _dnsNetworkName (\ s a -> s{_dnsNetworkName = a})

-- | The number of milliseconds between instances of this table in the output transport stream.
dnsNitInterval :: Lens' DvbNitSettings (Maybe Int)
dnsNitInterval = lens _dnsNitInterval (\ s a -> s{_dnsNitInterval = a})

instance FromJSON DvbNitSettings where
        parseJSON
          = withObject "DvbNitSettings"
              (\ x ->
                 DvbNitSettings' <$>
                   (x .:? "networkId") <*> (x .:? "networkName") <*>
                     (x .:? "nitInterval"))

instance Hashable DvbNitSettings where

instance NFData DvbNitSettings where

instance ToJSON DvbNitSettings where
        toJSON DvbNitSettings'{..}
          = object
              (catMaybes
                 [("networkId" .=) <$> _dnsNetworkId,
                  ("networkName" .=) <$> _dnsNetworkName,
                  ("nitInterval" .=) <$> _dnsNitInterval])

-- | Inserts DVB Service Description Table (NIT) at the specified table repetition interval.
--
-- /See:/ 'dvbSdtSettings' smart constructor.
data DvbSdtSettings = DvbSdtSettings'
  { _dssSdtInterval         :: !(Maybe Int)
  , _dssServiceProviderName :: !(Maybe Text)
  , _dssOutputSdt           :: !(Maybe OutputSdt)
  , _dssServiceName         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DvbSdtSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssSdtInterval' - The number of milliseconds between instances of this table in the output transport stream.
--
-- * 'dssServiceProviderName' - The service provider name placed in the service_descriptor in the Service Description Table. Maximum length is 256 characters.
--
-- * 'dssOutputSdt' - Undocumented member.
--
-- * 'dssServiceName' - The service name placed in the service_descriptor in the Service Description Table. Maximum length is 256 characters.
dvbSdtSettings
    :: DvbSdtSettings
dvbSdtSettings =
  DvbSdtSettings'
    { _dssSdtInterval = Nothing
    , _dssServiceProviderName = Nothing
    , _dssOutputSdt = Nothing
    , _dssServiceName = Nothing
    }


-- | The number of milliseconds between instances of this table in the output transport stream.
dssSdtInterval :: Lens' DvbSdtSettings (Maybe Int)
dssSdtInterval = lens _dssSdtInterval (\ s a -> s{_dssSdtInterval = a})

-- | The service provider name placed in the service_descriptor in the Service Description Table. Maximum length is 256 characters.
dssServiceProviderName :: Lens' DvbSdtSettings (Maybe Text)
dssServiceProviderName = lens _dssServiceProviderName (\ s a -> s{_dssServiceProviderName = a})

-- | Undocumented member.
dssOutputSdt :: Lens' DvbSdtSettings (Maybe OutputSdt)
dssOutputSdt = lens _dssOutputSdt (\ s a -> s{_dssOutputSdt = a})

-- | The service name placed in the service_descriptor in the Service Description Table. Maximum length is 256 characters.
dssServiceName :: Lens' DvbSdtSettings (Maybe Text)
dssServiceName = lens _dssServiceName (\ s a -> s{_dssServiceName = a})

instance FromJSON DvbSdtSettings where
        parseJSON
          = withObject "DvbSdtSettings"
              (\ x ->
                 DvbSdtSettings' <$>
                   (x .:? "sdtInterval") <*>
                     (x .:? "serviceProviderName")
                     <*> (x .:? "outputSdt")
                     <*> (x .:? "serviceName"))

instance Hashable DvbSdtSettings where

instance NFData DvbSdtSettings where

instance ToJSON DvbSdtSettings where
        toJSON DvbSdtSettings'{..}
          = object
              (catMaybes
                 [("sdtInterval" .=) <$> _dssSdtInterval,
                  ("serviceProviderName" .=) <$>
                    _dssServiceProviderName,
                  ("outputSdt" .=) <$> _dssOutputSdt,
                  ("serviceName" .=) <$> _dssServiceName])

-- | DVB-Sub Destination Settings
--
-- /See:/ 'dvbSubDestinationSettings' smart constructor.
data DvbSubDestinationSettings = DvbSubDestinationSettings'
  { _dsdsBackgroundOpacity :: !(Maybe Int)
  , _dsdsFontOpacity       :: !(Maybe Int)
  , _dsdsShadowYOffset     :: !(Maybe Int)
  , _dsdsFontResolution    :: !(Maybe Int)
  , _dsdsYPosition         :: !(Maybe Int)
  , _dsdsBackgroundColor   :: !(Maybe DvbSubtitleBackgroundColor)
  , _dsdsShadowXOffset     :: !(Maybe Int)
  , _dsdsFontSize          :: !(Maybe Int)
  , _dsdsXPosition         :: !(Maybe Int)
  , _dsdsTeletextSpacing   :: !(Maybe DvbSubtitleTeletextSpacing)
  , _dsdsAlignment         :: !(Maybe DvbSubtitleAlignment)
  , _dsdsShadowOpacity     :: !(Maybe Int)
  , _dsdsOutlineColor      :: !(Maybe DvbSubtitleOutlineColor)
  , _dsdsOutlineSize       :: !(Maybe Int)
  , _dsdsShadowColor       :: !(Maybe DvbSubtitleShadowColor)
  , _dsdsFontColor         :: !(Maybe DvbSubtitleFontColor)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DvbSubDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdsBackgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsFontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsShadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsFontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsYPosition' - Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsBackgroundColor' - Undocumented member.
--
-- * 'dsdsShadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsFontSize' - A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsXPosition' - Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsTeletextSpacing' - Undocumented member.
--
-- * 'dsdsAlignment' - Undocumented member.
--
-- * 'dsdsShadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsOutlineColor' - Undocumented member.
--
-- * 'dsdsOutlineSize' - Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- * 'dsdsShadowColor' - Undocumented member.
--
-- * 'dsdsFontColor' - Undocumented member.
dvbSubDestinationSettings
    :: DvbSubDestinationSettings
dvbSubDestinationSettings =
  DvbSubDestinationSettings'
    { _dsdsBackgroundOpacity = Nothing
    , _dsdsFontOpacity = Nothing
    , _dsdsShadowYOffset = Nothing
    , _dsdsFontResolution = Nothing
    , _dsdsYPosition = Nothing
    , _dsdsBackgroundColor = Nothing
    , _dsdsShadowXOffset = Nothing
    , _dsdsFontSize = Nothing
    , _dsdsXPosition = Nothing
    , _dsdsTeletextSpacing = Nothing
    , _dsdsAlignment = Nothing
    , _dsdsShadowOpacity = Nothing
    , _dsdsOutlineColor = Nothing
    , _dsdsOutlineSize = Nothing
    , _dsdsShadowColor = Nothing
    , _dsdsFontColor = Nothing
    }


-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
dsdsBackgroundOpacity :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsBackgroundOpacity = lens _dsdsBackgroundOpacity (\ s a -> s{_dsdsBackgroundOpacity = a})

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent. All burn-in and DVB-Sub font settings must match.
dsdsFontOpacity :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsFontOpacity = lens _dsdsFontOpacity (\ s a -> s{_dsdsFontOpacity = a})

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
dsdsShadowYOffset :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsShadowYOffset = lens _dsdsShadowYOffset (\ s a -> s{_dsdsShadowYOffset = a})

-- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in and DVB-Sub font settings must match.
dsdsFontResolution :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsFontResolution = lens _dsdsFontResolution (\ s a -> s{_dsdsFontResolution = a})

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsYPosition :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsYPosition = lens _dsdsYPosition (\ s a -> s{_dsdsYPosition = a})

-- | Undocumented member.
dsdsBackgroundColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleBackgroundColor)
dsdsBackgroundColor = lens _dsdsBackgroundColor (\ s a -> s{_dsdsBackgroundColor = a})

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
dsdsShadowXOffset :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsShadowXOffset = lens _dsdsShadowXOffset (\ s a -> s{_dsdsShadowXOffset = a})

-- | A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
dsdsFontSize :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsFontSize = lens _dsdsFontSize (\ s a -> s{_dsdsFontSize = a})

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsXPosition :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsXPosition = lens _dsdsXPosition (\ s a -> s{_dsdsXPosition = a})

-- | Undocumented member.
dsdsTeletextSpacing :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleTeletextSpacing)
dsdsTeletextSpacing = lens _dsdsTeletextSpacing (\ s a -> s{_dsdsTeletextSpacing = a})

-- | Undocumented member.
dsdsAlignment :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleAlignment)
dsdsAlignment = lens _dsdsAlignment (\ s a -> s{_dsdsAlignment = a})

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
dsdsShadowOpacity :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsShadowOpacity = lens _dsdsShadowOpacity (\ s a -> s{_dsdsShadowOpacity = a})

-- | Undocumented member.
dsdsOutlineColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleOutlineColor)
dsdsOutlineColor = lens _dsdsOutlineColor (\ s a -> s{_dsdsOutlineColor = a})

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsOutlineSize :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsOutlineSize = lens _dsdsOutlineSize (\ s a -> s{_dsdsOutlineSize = a})

-- | Undocumented member.
dsdsShadowColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleShadowColor)
dsdsShadowColor = lens _dsdsShadowColor (\ s a -> s{_dsdsShadowColor = a})

-- | Undocumented member.
dsdsFontColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleFontColor)
dsdsFontColor = lens _dsdsFontColor (\ s a -> s{_dsdsFontColor = a})

instance FromJSON DvbSubDestinationSettings where
        parseJSON
          = withObject "DvbSubDestinationSettings"
              (\ x ->
                 DvbSubDestinationSettings' <$>
                   (x .:? "backgroundOpacity") <*> (x .:? "fontOpacity")
                     <*> (x .:? "shadowYOffset")
                     <*> (x .:? "fontResolution")
                     <*> (x .:? "yPosition")
                     <*> (x .:? "backgroundColor")
                     <*> (x .:? "shadowXOffset")
                     <*> (x .:? "fontSize")
                     <*> (x .:? "xPosition")
                     <*> (x .:? "teletextSpacing")
                     <*> (x .:? "alignment")
                     <*> (x .:? "shadowOpacity")
                     <*> (x .:? "outlineColor")
                     <*> (x .:? "outlineSize")
                     <*> (x .:? "shadowColor")
                     <*> (x .:? "fontColor"))

instance Hashable DvbSubDestinationSettings where

instance NFData DvbSubDestinationSettings where

instance ToJSON DvbSubDestinationSettings where
        toJSON DvbSubDestinationSettings'{..}
          = object
              (catMaybes
                 [("backgroundOpacity" .=) <$> _dsdsBackgroundOpacity,
                  ("fontOpacity" .=) <$> _dsdsFontOpacity,
                  ("shadowYOffset" .=) <$> _dsdsShadowYOffset,
                  ("fontResolution" .=) <$> _dsdsFontResolution,
                  ("yPosition" .=) <$> _dsdsYPosition,
                  ("backgroundColor" .=) <$> _dsdsBackgroundColor,
                  ("shadowXOffset" .=) <$> _dsdsShadowXOffset,
                  ("fontSize" .=) <$> _dsdsFontSize,
                  ("xPosition" .=) <$> _dsdsXPosition,
                  ("teletextSpacing" .=) <$> _dsdsTeletextSpacing,
                  ("alignment" .=) <$> _dsdsAlignment,
                  ("shadowOpacity" .=) <$> _dsdsShadowOpacity,
                  ("outlineColor" .=) <$> _dsdsOutlineColor,
                  ("outlineSize" .=) <$> _dsdsOutlineSize,
                  ("shadowColor" .=) <$> _dsdsShadowColor,
                  ("fontColor" .=) <$> _dsdsFontColor])

-- | DVB Sub Source Settings
--
-- /See:/ 'dvbSubSourceSettings' smart constructor.
newtype DvbSubSourceSettings = DvbSubSourceSettings'
  { _dsssPid :: Maybe Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DvbSubSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsssPid' - When using DVB-Sub with Burn-In or SMPTE-TT, use this PID for the source content. Unused for DVB-Sub passthrough. All DVB-Sub content is passed through, regardless of selectors.
dvbSubSourceSettings
    :: DvbSubSourceSettings
dvbSubSourceSettings = DvbSubSourceSettings' {_dsssPid = Nothing}


-- | When using DVB-Sub with Burn-In or SMPTE-TT, use this PID for the source content. Unused for DVB-Sub passthrough. All DVB-Sub content is passed through, regardless of selectors.
dsssPid :: Lens' DvbSubSourceSettings (Maybe Int)
dsssPid = lens _dsssPid (\ s a -> s{_dsssPid = a})

instance FromJSON DvbSubSourceSettings where
        parseJSON
          = withObject "DvbSubSourceSettings"
              (\ x -> DvbSubSourceSettings' <$> (x .:? "pid"))

instance Hashable DvbSubSourceSettings where

instance NFData DvbSubSourceSettings where

instance ToJSON DvbSubSourceSettings where
        toJSON DvbSubSourceSettings'{..}
          = object (catMaybes [("pid" .=) <$> _dsssPid])

-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
--
-- /See:/ 'dvbTdtSettings' smart constructor.
newtype DvbTdtSettings = DvbTdtSettings'
  { _dtsTdtInterval :: Maybe Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DvbTdtSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsTdtInterval' - The number of milliseconds between instances of this table in the output transport stream.
dvbTdtSettings
    :: DvbTdtSettings
dvbTdtSettings = DvbTdtSettings' {_dtsTdtInterval = Nothing}


-- | The number of milliseconds between instances of this table in the output transport stream.
dtsTdtInterval :: Lens' DvbTdtSettings (Maybe Int)
dtsTdtInterval = lens _dtsTdtInterval (\ s a -> s{_dtsTdtInterval = a})

instance FromJSON DvbTdtSettings where
        parseJSON
          = withObject "DvbTdtSettings"
              (\ x -> DvbTdtSettings' <$> (x .:? "tdtInterval"))

instance Hashable DvbTdtSettings where

instance NFData DvbTdtSettings where

instance ToJSON DvbTdtSettings where
        toJSON DvbTdtSettings'{..}
          = object
              (catMaybes [("tdtInterval" .=) <$> _dtsTdtInterval])

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3.
--
-- /See:/ 'eac3Settings' smart constructor.
data Eac3Settings = Eac3Settings'
  { _esStereoDownmix               :: !(Maybe Eac3StereoDownmix)
  , _esLoRoCenterMixLevel          :: !(Maybe Double)
  , _esLtRtCenterMixLevel          :: !(Maybe Double)
  , _esLfeFilter                   :: !(Maybe Eac3LfeFilter)
  , _esDynamicRangeCompressionLine :: !(Maybe Eac3DynamicRangeCompressionLine)
  , _esLtRtSurroundMixLevel        :: !(Maybe Double)
  , _esMetadataControl             :: !(Maybe Eac3MetadataControl)
  , _esLoRoSurroundMixLevel        :: !(Maybe Double)
  , _esSurroundMode                :: !(Maybe Eac3SurroundMode)
  , _esAttenuationControl          :: !(Maybe Eac3AttenuationControl)
  , _esPassthroughControl          :: !(Maybe Eac3PassthroughControl)
  , _esBitstreamMode               :: !(Maybe Eac3BitstreamMode)
  , _esLfeControl                  :: !(Maybe Eac3LfeControl)
  , _esDynamicRangeCompressionRf   :: !(Maybe Eac3DynamicRangeCompressionRf)
  , _esCodingMode                  :: !(Maybe Eac3CodingMode)
  , _esSampleRate                  :: !(Maybe Int)
  , _esDcFilter                    :: !(Maybe Eac3DcFilter)
  , _esBitrate                     :: !(Maybe Int)
  , _esPhaseControl                :: !(Maybe Eac3PhaseControl)
  , _esSurroundExMode              :: !(Maybe Eac3SurroundExMode)
  , _esDialnorm                    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Eac3Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esStereoDownmix' - Undocumented member.
--
-- * 'esLoRoCenterMixLevel' - Left only/Right only center mix level. Only used for 3/2 coding mode. Valid values: 3.0, 1.5, 0.0, -1.5 -3.0 -4.5 -6.0 -60
--
-- * 'esLtRtCenterMixLevel' - Left total/Right total center mix level. Only used for 3/2 coding mode. Valid values: 3.0, 1.5, 0.0, -1.5 -3.0 -4.5 -6.0 -60
--
-- * 'esLfeFilter' - Undocumented member.
--
-- * 'esDynamicRangeCompressionLine' - Undocumented member.
--
-- * 'esLtRtSurroundMixLevel' - Left total/Right total surround mix level. Only used for 3/2 coding mode. Valid values: -1.5 -3.0 -4.5 -6.0 -60
--
-- * 'esMetadataControl' - Undocumented member.
--
-- * 'esLoRoSurroundMixLevel' - Left only/Right only surround mix level. Only used for 3/2 coding mode. Valid values: -1.5 -3.0 -4.5 -6.0 -60
--
-- * 'esSurroundMode' - Undocumented member.
--
-- * 'esAttenuationControl' - Undocumented member.
--
-- * 'esPassthroughControl' - Undocumented member.
--
-- * 'esBitstreamMode' - Undocumented member.
--
-- * 'esLfeControl' - Undocumented member.
--
-- * 'esDynamicRangeCompressionRf' - Undocumented member.
--
-- * 'esCodingMode' - Undocumented member.
--
-- * 'esSampleRate' - Sample rate in hz. Sample rate is always 48000.
--
-- * 'esDcFilter' - Undocumented member.
--
-- * 'esBitrate' - Average bitrate in bits/second. Valid bitrates depend on the coding mode.
--
-- * 'esPhaseControl' - Undocumented member.
--
-- * 'esSurroundExMode' - Undocumented member.
--
-- * 'esDialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
eac3Settings
    :: Eac3Settings
eac3Settings =
  Eac3Settings'
    { _esStereoDownmix = Nothing
    , _esLoRoCenterMixLevel = Nothing
    , _esLtRtCenterMixLevel = Nothing
    , _esLfeFilter = Nothing
    , _esDynamicRangeCompressionLine = Nothing
    , _esLtRtSurroundMixLevel = Nothing
    , _esMetadataControl = Nothing
    , _esLoRoSurroundMixLevel = Nothing
    , _esSurroundMode = Nothing
    , _esAttenuationControl = Nothing
    , _esPassthroughControl = Nothing
    , _esBitstreamMode = Nothing
    , _esLfeControl = Nothing
    , _esDynamicRangeCompressionRf = Nothing
    , _esCodingMode = Nothing
    , _esSampleRate = Nothing
    , _esDcFilter = Nothing
    , _esBitrate = Nothing
    , _esPhaseControl = Nothing
    , _esSurroundExMode = Nothing
    , _esDialnorm = Nothing
    }


-- | Undocumented member.
esStereoDownmix :: Lens' Eac3Settings (Maybe Eac3StereoDownmix)
esStereoDownmix = lens _esStereoDownmix (\ s a -> s{_esStereoDownmix = a})

-- | Left only/Right only center mix level. Only used for 3/2 coding mode. Valid values: 3.0, 1.5, 0.0, -1.5 -3.0 -4.5 -6.0 -60
esLoRoCenterMixLevel :: Lens' Eac3Settings (Maybe Double)
esLoRoCenterMixLevel = lens _esLoRoCenterMixLevel (\ s a -> s{_esLoRoCenterMixLevel = a})

-- | Left total/Right total center mix level. Only used for 3/2 coding mode. Valid values: 3.0, 1.5, 0.0, -1.5 -3.0 -4.5 -6.0 -60
esLtRtCenterMixLevel :: Lens' Eac3Settings (Maybe Double)
esLtRtCenterMixLevel = lens _esLtRtCenterMixLevel (\ s a -> s{_esLtRtCenterMixLevel = a})

-- | Undocumented member.
esLfeFilter :: Lens' Eac3Settings (Maybe Eac3LfeFilter)
esLfeFilter = lens _esLfeFilter (\ s a -> s{_esLfeFilter = a})

-- | Undocumented member.
esDynamicRangeCompressionLine :: Lens' Eac3Settings (Maybe Eac3DynamicRangeCompressionLine)
esDynamicRangeCompressionLine = lens _esDynamicRangeCompressionLine (\ s a -> s{_esDynamicRangeCompressionLine = a})

-- | Left total/Right total surround mix level. Only used for 3/2 coding mode. Valid values: -1.5 -3.0 -4.5 -6.0 -60
esLtRtSurroundMixLevel :: Lens' Eac3Settings (Maybe Double)
esLtRtSurroundMixLevel = lens _esLtRtSurroundMixLevel (\ s a -> s{_esLtRtSurroundMixLevel = a})

-- | Undocumented member.
esMetadataControl :: Lens' Eac3Settings (Maybe Eac3MetadataControl)
esMetadataControl = lens _esMetadataControl (\ s a -> s{_esMetadataControl = a})

-- | Left only/Right only surround mix level. Only used for 3/2 coding mode. Valid values: -1.5 -3.0 -4.5 -6.0 -60
esLoRoSurroundMixLevel :: Lens' Eac3Settings (Maybe Double)
esLoRoSurroundMixLevel = lens _esLoRoSurroundMixLevel (\ s a -> s{_esLoRoSurroundMixLevel = a})

-- | Undocumented member.
esSurroundMode :: Lens' Eac3Settings (Maybe Eac3SurroundMode)
esSurroundMode = lens _esSurroundMode (\ s a -> s{_esSurroundMode = a})

-- | Undocumented member.
esAttenuationControl :: Lens' Eac3Settings (Maybe Eac3AttenuationControl)
esAttenuationControl = lens _esAttenuationControl (\ s a -> s{_esAttenuationControl = a})

-- | Undocumented member.
esPassthroughControl :: Lens' Eac3Settings (Maybe Eac3PassthroughControl)
esPassthroughControl = lens _esPassthroughControl (\ s a -> s{_esPassthroughControl = a})

-- | Undocumented member.
esBitstreamMode :: Lens' Eac3Settings (Maybe Eac3BitstreamMode)
esBitstreamMode = lens _esBitstreamMode (\ s a -> s{_esBitstreamMode = a})

-- | Undocumented member.
esLfeControl :: Lens' Eac3Settings (Maybe Eac3LfeControl)
esLfeControl = lens _esLfeControl (\ s a -> s{_esLfeControl = a})

-- | Undocumented member.
esDynamicRangeCompressionRf :: Lens' Eac3Settings (Maybe Eac3DynamicRangeCompressionRf)
esDynamicRangeCompressionRf = lens _esDynamicRangeCompressionRf (\ s a -> s{_esDynamicRangeCompressionRf = a})

-- | Undocumented member.
esCodingMode :: Lens' Eac3Settings (Maybe Eac3CodingMode)
esCodingMode = lens _esCodingMode (\ s a -> s{_esCodingMode = a})

-- | Sample rate in hz. Sample rate is always 48000.
esSampleRate :: Lens' Eac3Settings (Maybe Int)
esSampleRate = lens _esSampleRate (\ s a -> s{_esSampleRate = a})

-- | Undocumented member.
esDcFilter :: Lens' Eac3Settings (Maybe Eac3DcFilter)
esDcFilter = lens _esDcFilter (\ s a -> s{_esDcFilter = a})

-- | Average bitrate in bits/second. Valid bitrates depend on the coding mode.
esBitrate :: Lens' Eac3Settings (Maybe Int)
esBitrate = lens _esBitrate (\ s a -> s{_esBitrate = a})

-- | Undocumented member.
esPhaseControl :: Lens' Eac3Settings (Maybe Eac3PhaseControl)
esPhaseControl = lens _esPhaseControl (\ s a -> s{_esPhaseControl = a})

-- | Undocumented member.
esSurroundExMode :: Lens' Eac3Settings (Maybe Eac3SurroundExMode)
esSurroundExMode = lens _esSurroundExMode (\ s a -> s{_esSurroundExMode = a})

-- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
esDialnorm :: Lens' Eac3Settings (Maybe Int)
esDialnorm = lens _esDialnorm (\ s a -> s{_esDialnorm = a})

instance FromJSON Eac3Settings where
        parseJSON
          = withObject "Eac3Settings"
              (\ x ->
                 Eac3Settings' <$>
                   (x .:? "stereoDownmix") <*>
                     (x .:? "loRoCenterMixLevel")
                     <*> (x .:? "ltRtCenterMixLevel")
                     <*> (x .:? "lfeFilter")
                     <*> (x .:? "dynamicRangeCompressionLine")
                     <*> (x .:? "ltRtSurroundMixLevel")
                     <*> (x .:? "metadataControl")
                     <*> (x .:? "loRoSurroundMixLevel")
                     <*> (x .:? "surroundMode")
                     <*> (x .:? "attenuationControl")
                     <*> (x .:? "passthroughControl")
                     <*> (x .:? "bitstreamMode")
                     <*> (x .:? "lfeControl")
                     <*> (x .:? "dynamicRangeCompressionRf")
                     <*> (x .:? "codingMode")
                     <*> (x .:? "sampleRate")
                     <*> (x .:? "dcFilter")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "phaseControl")
                     <*> (x .:? "surroundExMode")
                     <*> (x .:? "dialnorm"))

instance Hashable Eac3Settings where

instance NFData Eac3Settings where

instance ToJSON Eac3Settings where
        toJSON Eac3Settings'{..}
          = object
              (catMaybes
                 [("stereoDownmix" .=) <$> _esStereoDownmix,
                  ("loRoCenterMixLevel" .=) <$> _esLoRoCenterMixLevel,
                  ("ltRtCenterMixLevel" .=) <$> _esLtRtCenterMixLevel,
                  ("lfeFilter" .=) <$> _esLfeFilter,
                  ("dynamicRangeCompressionLine" .=) <$>
                    _esDynamicRangeCompressionLine,
                  ("ltRtSurroundMixLevel" .=) <$>
                    _esLtRtSurroundMixLevel,
                  ("metadataControl" .=) <$> _esMetadataControl,
                  ("loRoSurroundMixLevel" .=) <$>
                    _esLoRoSurroundMixLevel,
                  ("surroundMode" .=) <$> _esSurroundMode,
                  ("attenuationControl" .=) <$> _esAttenuationControl,
                  ("passthroughControl" .=) <$> _esPassthroughControl,
                  ("bitstreamMode" .=) <$> _esBitstreamMode,
                  ("lfeControl" .=) <$> _esLfeControl,
                  ("dynamicRangeCompressionRf" .=) <$>
                    _esDynamicRangeCompressionRf,
                  ("codingMode" .=) <$> _esCodingMode,
                  ("sampleRate" .=) <$> _esSampleRate,
                  ("dcFilter" .=) <$> _esDcFilter,
                  ("bitrate" .=) <$> _esBitrate,
                  ("phaseControl" .=) <$> _esPhaseControl,
                  ("surroundExMode" .=) <$> _esSurroundExMode,
                  ("dialnorm" .=) <$> _esDialnorm])

-- | Settings for embedded captions Source
--
-- /See:/ 'embeddedSourceSettings' smart constructor.
data EmbeddedSourceSettings = EmbeddedSourceSettings'
  { _essConvert608To708        :: !(Maybe EmbeddedConvert608To708)
  , _essSource608TrackNumber   :: !(Maybe Int)
  , _essSource608ChannelNumber :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EmbeddedSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'essConvert608To708' - Undocumented member.
--
-- * 'essSource608TrackNumber' - Specifies the video track index used for extracting captions. The system only supports one input video track, so this should always be set to '1'.
--
-- * 'essSource608ChannelNumber' - Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
embeddedSourceSettings
    :: EmbeddedSourceSettings
embeddedSourceSettings =
  EmbeddedSourceSettings'
    { _essConvert608To708 = Nothing
    , _essSource608TrackNumber = Nothing
    , _essSource608ChannelNumber = Nothing
    }


-- | Undocumented member.
essConvert608To708 :: Lens' EmbeddedSourceSettings (Maybe EmbeddedConvert608To708)
essConvert608To708 = lens _essConvert608To708 (\ s a -> s{_essConvert608To708 = a})

-- | Specifies the video track index used for extracting captions. The system only supports one input video track, so this should always be set to '1'.
essSource608TrackNumber :: Lens' EmbeddedSourceSettings (Maybe Int)
essSource608TrackNumber = lens _essSource608TrackNumber (\ s a -> s{_essSource608TrackNumber = a})

-- | Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
essSource608ChannelNumber :: Lens' EmbeddedSourceSettings (Maybe Int)
essSource608ChannelNumber = lens _essSource608ChannelNumber (\ s a -> s{_essSource608ChannelNumber = a})

instance FromJSON EmbeddedSourceSettings where
        parseJSON
          = withObject "EmbeddedSourceSettings"
              (\ x ->
                 EmbeddedSourceSettings' <$>
                   (x .:? "convert608To708") <*>
                     (x .:? "source608TrackNumber")
                     <*> (x .:? "source608ChannelNumber"))

instance Hashable EmbeddedSourceSettings where

instance NFData EmbeddedSourceSettings where

instance ToJSON EmbeddedSourceSettings where
        toJSON EmbeddedSourceSettings'{..}
          = object
              (catMaybes
                 [("convert608To708" .=) <$> _essConvert608To708,
                  ("source608TrackNumber" .=) <$>
                    _essSource608TrackNumber,
                  ("source608ChannelNumber" .=) <$>
                    _essSource608ChannelNumber])

-- | Describes account specific API endpoint
--
-- /See:/ 'endpoint' smart constructor.
newtype Endpoint = Endpoint'
  { _eURL :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eURL' - URL of endpoint
endpoint
    :: Endpoint
endpoint = Endpoint' {_eURL = Nothing}


-- | URL of endpoint
eURL :: Lens' Endpoint (Maybe Text)
eURL = lens _eURL (\ s a -> s{_eURL = a})

instance FromJSON Endpoint where
        parseJSON
          = withObject "Endpoint"
              (\ x -> Endpoint' <$> (x .:? "url"))

instance Hashable Endpoint where

instance NFData Endpoint where

-- | Settings for F4v container
--
-- /See:/ 'f4vSettings' smart constructor.
newtype F4vSettings = F4vSettings'
  { _fsMoovPlacement :: Maybe F4vMoovPlacement
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'F4vSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsMoovPlacement' - Undocumented member.
f4vSettings
    :: F4vSettings
f4vSettings = F4vSettings' {_fsMoovPlacement = Nothing}


-- | Undocumented member.
fsMoovPlacement :: Lens' F4vSettings (Maybe F4vMoovPlacement)
fsMoovPlacement = lens _fsMoovPlacement (\ s a -> s{_fsMoovPlacement = a})

instance FromJSON F4vSettings where
        parseJSON
          = withObject "F4vSettings"
              (\ x -> F4vSettings' <$> (x .:? "moovPlacement"))

instance Hashable F4vSettings where

instance NFData F4vSettings where

instance ToJSON F4vSettings where
        toJSON F4vSettings'{..}
          = object
              (catMaybes
                 [("moovPlacement" .=) <$> _fsMoovPlacement])

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to FILE_GROUP_SETTINGS.
--
-- /See:/ 'fileGroupSettings' smart constructor.
newtype FileGroupSettings = FileGroupSettings'
  { _fgsDestination :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FileGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fgsDestination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
fileGroupSettings
    :: FileGroupSettings
fileGroupSettings = FileGroupSettings' {_fgsDestination = Nothing}


-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
fgsDestination :: Lens' FileGroupSettings (Maybe Text)
fgsDestination = lens _fgsDestination (\ s a -> s{_fgsDestination = a})

instance FromJSON FileGroupSettings where
        parseJSON
          = withObject "FileGroupSettings"
              (\ x -> FileGroupSettings' <$> (x .:? "destination"))

instance Hashable FileGroupSettings where

instance NFData FileGroupSettings where

instance ToJSON FileGroupSettings where
        toJSON FileGroupSettings'{..}
          = object
              (catMaybes [("destination" .=) <$> _fgsDestination])

-- | Settings for File-based Captions in Source
--
-- /See:/ 'fileSourceSettings' smart constructor.
data FileSourceSettings = FileSourceSettings'
  { _fssConvert608To708 :: !(Maybe FileSourceConvert608To708)
  , _fssTimeDelta       :: !(Maybe Int)
  , _fssSourceFile      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FileSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fssConvert608To708' - Undocumented member.
--
-- * 'fssTimeDelta' - Specifies a time delta in seconds to offset the captions from the source file.
--
-- * 'fssSourceFile' - External caption file used for loading captions. Accepted file extensions are 'scc', 'ttml', 'dfxp', 'stl', 'srt', and 'smi'.
fileSourceSettings
    :: FileSourceSettings
fileSourceSettings =
  FileSourceSettings'
    { _fssConvert608To708 = Nothing
    , _fssTimeDelta = Nothing
    , _fssSourceFile = Nothing
    }


-- | Undocumented member.
fssConvert608To708 :: Lens' FileSourceSettings (Maybe FileSourceConvert608To708)
fssConvert608To708 = lens _fssConvert608To708 (\ s a -> s{_fssConvert608To708 = a})

-- | Specifies a time delta in seconds to offset the captions from the source file.
fssTimeDelta :: Lens' FileSourceSettings (Maybe Int)
fssTimeDelta = lens _fssTimeDelta (\ s a -> s{_fssTimeDelta = a})

-- | External caption file used for loading captions. Accepted file extensions are 'scc', 'ttml', 'dfxp', 'stl', 'srt', and 'smi'.
fssSourceFile :: Lens' FileSourceSettings (Maybe Text)
fssSourceFile = lens _fssSourceFile (\ s a -> s{_fssSourceFile = a})

instance FromJSON FileSourceSettings where
        parseJSON
          = withObject "FileSourceSettings"
              (\ x ->
                 FileSourceSettings' <$>
                   (x .:? "convert608To708") <*> (x .:? "timeDelta") <*>
                     (x .:? "sourceFile"))

instance Hashable FileSourceSettings where

instance NFData FileSourceSettings where

instance ToJSON FileSourceSettings where
        toJSON FileSourceSettings'{..}
          = object
              (catMaybes
                 [("convert608To708" .=) <$> _fssConvert608To708,
                  ("timeDelta" .=) <$> _fssTimeDelta,
                  ("sourceFile" .=) <$> _fssSourceFile])

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value FRAME_CAPTURE.
--
-- /See:/ 'frameCaptureSettings' smart constructor.
data FrameCaptureSettings = FrameCaptureSettings'
  { _fcsQuality              :: !(Maybe Int)
  , _fcsFramerateDenominator :: !(Maybe Int)
  , _fcsMaxCaptures          :: !(Maybe Int)
  , _fcsFramerateNumerator   :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FrameCaptureSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcsQuality' - JPEG Quality - a higher value equals higher quality.
--
-- * 'fcsFramerateDenominator' - Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.n.jpg where n is the 0-based sequence number of each Capture.
--
-- * 'fcsMaxCaptures' - Maximum number of captures (encoded jpg output files).
--
-- * 'fcsFramerateNumerator' - Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.NNNNNNN.jpg where N is the 0-based frame sequence number zero padded to 7 decimal places.
frameCaptureSettings
    :: FrameCaptureSettings
frameCaptureSettings =
  FrameCaptureSettings'
    { _fcsQuality = Nothing
    , _fcsFramerateDenominator = Nothing
    , _fcsMaxCaptures = Nothing
    , _fcsFramerateNumerator = Nothing
    }


-- | JPEG Quality - a higher value equals higher quality.
fcsQuality :: Lens' FrameCaptureSettings (Maybe Int)
fcsQuality = lens _fcsQuality (\ s a -> s{_fcsQuality = a})

-- | Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.n.jpg where n is the 0-based sequence number of each Capture.
fcsFramerateDenominator :: Lens' FrameCaptureSettings (Maybe Int)
fcsFramerateDenominator = lens _fcsFramerateDenominator (\ s a -> s{_fcsFramerateDenominator = a})

-- | Maximum number of captures (encoded jpg output files).
fcsMaxCaptures :: Lens' FrameCaptureSettings (Maybe Int)
fcsMaxCaptures = lens _fcsMaxCaptures (\ s a -> s{_fcsMaxCaptures = a})

-- | Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.NNNNNNN.jpg where N is the 0-based frame sequence number zero padded to 7 decimal places.
fcsFramerateNumerator :: Lens' FrameCaptureSettings (Maybe Int)
fcsFramerateNumerator = lens _fcsFramerateNumerator (\ s a -> s{_fcsFramerateNumerator = a})

instance FromJSON FrameCaptureSettings where
        parseJSON
          = withObject "FrameCaptureSettings"
              (\ x ->
                 FrameCaptureSettings' <$>
                   (x .:? "quality") <*> (x .:? "framerateDenominator")
                     <*> (x .:? "maxCaptures")
                     <*> (x .:? "framerateNumerator"))

instance Hashable FrameCaptureSettings where

instance NFData FrameCaptureSettings where

instance ToJSON FrameCaptureSettings where
        toJSON FrameCaptureSettings'{..}
          = object
              (catMaybes
                 [("quality" .=) <$> _fcsQuality,
                  ("framerateDenominator" .=) <$>
                    _fcsFramerateDenominator,
                  ("maxCaptures" .=) <$> _fcsMaxCaptures,
                  ("framerateNumerator" .=) <$>
                    _fcsFramerateNumerator])

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
--
-- /See:/ 'h264Settings' smart constructor.
data H264Settings = H264Settings'
  { _hUnregisteredSeiTimecode :: !(Maybe H264UnregisteredSeiTimecode)
  , _hQualityTuningLevel :: !(Maybe H264QualityTuningLevel)
  , _hTemporalAdaptiveQuantization :: !(Maybe H264TemporalAdaptiveQuantization)
  , _hSceneChangeDetect :: !(Maybe H264SceneChangeDetect)
  , _hHrdBufferInitialFillPercentage :: !(Maybe Int)
  , _hSlowPal :: !(Maybe H264SlowPal)
  , _hParNumerator :: !(Maybe Int)
  , _hGopSize :: !(Maybe Double)
  , _hNumberBFramesBetweenReferenceFrames :: !(Maybe Int)
  , _hGopSizeUnits :: !(Maybe H264GopSizeUnits)
  , _hHrdBufferSize :: !(Maybe Int)
  , _hSlices :: !(Maybe Int)
  , _hRateControlMode :: !(Maybe H264RateControlMode)
  , _hNumberReferenceFrames :: !(Maybe Int)
  , _hTelecine :: !(Maybe H264Telecine)
  , _hMinIInterval :: !(Maybe Int)
  , _hInterlaceMode :: !(Maybe H264InterlaceMode)
  , _hParControl :: !(Maybe H264ParControl)
  , _hRepeatPps :: !(Maybe H264RepeatPps)
  , _hFlickerAdaptiveQuantization :: !(Maybe H264FlickerAdaptiveQuantization)
  , _hSoftness :: !(Maybe Int)
  , _hCodecProfile :: !(Maybe H264CodecProfile)
  , _hBitrate :: !(Maybe Int)
  , _hFramerateDenominator :: !(Maybe Int)
  , _hFramerateConversionAlgorithm :: !(Maybe H264FramerateConversionAlgorithm)
  , _hCodecLevel :: !(Maybe H264CodecLevel)
  , _hEntropyEncoding :: !(Maybe H264EntropyEncoding)
  , _hFramerateControl :: !(Maybe H264FramerateControl)
  , _hAdaptiveQuantization :: !(Maybe H264AdaptiveQuantization)
  , _hFramerateNumerator :: !(Maybe Int)
  , _hGopBReference :: !(Maybe H264GopBReference)
  , _hMaxBitrate :: !(Maybe Int)
  , _hSyntax :: !(Maybe H264Syntax)
  , _hFieldEncoding :: !(Maybe H264FieldEncoding)
  , _hGopClosedCadence :: !(Maybe Int)
  , _hParDenominator :: !(Maybe Int)
  , _hSpatialAdaptiveQuantization :: !(Maybe H264SpatialAdaptiveQuantization)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'H264Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hUnregisteredSeiTimecode' - Undocumented member.
--
-- * 'hQualityTuningLevel' - Undocumented member.
--
-- * 'hTemporalAdaptiveQuantization' - Undocumented member.
--
-- * 'hSceneChangeDetect' - Undocumented member.
--
-- * 'hHrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- * 'hSlowPal' - Undocumented member.
--
-- * 'hParNumerator' - Pixel Aspect Ratio numerator.
--
-- * 'hGopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- * 'hNumberBFramesBetweenReferenceFrames' - Number of B-frames between reference frames.
--
-- * 'hGopSizeUnits' - Undocumented member.
--
-- * 'hHrdBufferSize' - Size of buffer (HRD buffer model). Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m.
--
-- * 'hSlices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- * 'hRateControlMode' - Undocumented member.
--
-- * 'hNumberReferenceFrames' - Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- * 'hTelecine' - Undocumented member.
--
-- * 'hMinIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'hInterlaceMode' - Undocumented member.
--
-- * 'hParControl' - Undocumented member.
--
-- * 'hRepeatPps' - Undocumented member.
--
-- * 'hFlickerAdaptiveQuantization' - Undocumented member.
--
-- * 'hSoftness' - Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
--
-- * 'hCodecProfile' - Undocumented member.
--
-- * 'hBitrate' - Average bitrate in bits/second. Required for VBR, CBR, and ABR. Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- * 'hFramerateDenominator' - When you use the API for transcode jobs that use framerate conversion, specify the framerate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use framerate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'hFramerateConversionAlgorithm' - Undocumented member.
--
-- * 'hCodecLevel' - Undocumented member.
--
-- * 'hEntropyEncoding' - Undocumented member.
--
-- * 'hFramerateControl' - Undocumented member.
--
-- * 'hAdaptiveQuantization' - Undocumented member.
--
-- * 'hFramerateNumerator' - Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- * 'hGopBReference' - Undocumented member.
--
-- * 'hMaxBitrate' - Maximum bitrate in bits/second (for VBR mode only). Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m.
--
-- * 'hSyntax' - Undocumented member.
--
-- * 'hFieldEncoding' - Undocumented member.
--
-- * 'hGopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- * 'hParDenominator' - Pixel Aspect Ratio denominator.
--
-- * 'hSpatialAdaptiveQuantization' - Undocumented member.
h264Settings
    :: H264Settings
h264Settings =
  H264Settings'
    { _hUnregisteredSeiTimecode = Nothing
    , _hQualityTuningLevel = Nothing
    , _hTemporalAdaptiveQuantization = Nothing
    , _hSceneChangeDetect = Nothing
    , _hHrdBufferInitialFillPercentage = Nothing
    , _hSlowPal = Nothing
    , _hParNumerator = Nothing
    , _hGopSize = Nothing
    , _hNumberBFramesBetweenReferenceFrames = Nothing
    , _hGopSizeUnits = Nothing
    , _hHrdBufferSize = Nothing
    , _hSlices = Nothing
    , _hRateControlMode = Nothing
    , _hNumberReferenceFrames = Nothing
    , _hTelecine = Nothing
    , _hMinIInterval = Nothing
    , _hInterlaceMode = Nothing
    , _hParControl = Nothing
    , _hRepeatPps = Nothing
    , _hFlickerAdaptiveQuantization = Nothing
    , _hSoftness = Nothing
    , _hCodecProfile = Nothing
    , _hBitrate = Nothing
    , _hFramerateDenominator = Nothing
    , _hFramerateConversionAlgorithm = Nothing
    , _hCodecLevel = Nothing
    , _hEntropyEncoding = Nothing
    , _hFramerateControl = Nothing
    , _hAdaptiveQuantization = Nothing
    , _hFramerateNumerator = Nothing
    , _hGopBReference = Nothing
    , _hMaxBitrate = Nothing
    , _hSyntax = Nothing
    , _hFieldEncoding = Nothing
    , _hGopClosedCadence = Nothing
    , _hParDenominator = Nothing
    , _hSpatialAdaptiveQuantization = Nothing
    }


-- | Undocumented member.
hUnregisteredSeiTimecode :: Lens' H264Settings (Maybe H264UnregisteredSeiTimecode)
hUnregisteredSeiTimecode = lens _hUnregisteredSeiTimecode (\ s a -> s{_hUnregisteredSeiTimecode = a})

-- | Undocumented member.
hQualityTuningLevel :: Lens' H264Settings (Maybe H264QualityTuningLevel)
hQualityTuningLevel = lens _hQualityTuningLevel (\ s a -> s{_hQualityTuningLevel = a})

-- | Undocumented member.
hTemporalAdaptiveQuantization :: Lens' H264Settings (Maybe H264TemporalAdaptiveQuantization)
hTemporalAdaptiveQuantization = lens _hTemporalAdaptiveQuantization (\ s a -> s{_hTemporalAdaptiveQuantization = a})

-- | Undocumented member.
hSceneChangeDetect :: Lens' H264Settings (Maybe H264SceneChangeDetect)
hSceneChangeDetect = lens _hSceneChangeDetect (\ s a -> s{_hSceneChangeDetect = a})

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
hHrdBufferInitialFillPercentage :: Lens' H264Settings (Maybe Int)
hHrdBufferInitialFillPercentage = lens _hHrdBufferInitialFillPercentage (\ s a -> s{_hHrdBufferInitialFillPercentage = a})

-- | Undocumented member.
hSlowPal :: Lens' H264Settings (Maybe H264SlowPal)
hSlowPal = lens _hSlowPal (\ s a -> s{_hSlowPal = a})

-- | Pixel Aspect Ratio numerator.
hParNumerator :: Lens' H264Settings (Maybe Int)
hParNumerator = lens _hParNumerator (\ s a -> s{_hParNumerator = a})

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
hGopSize :: Lens' H264Settings (Maybe Double)
hGopSize = lens _hGopSize (\ s a -> s{_hGopSize = a})

-- | Number of B-frames between reference frames.
hNumberBFramesBetweenReferenceFrames :: Lens' H264Settings (Maybe Int)
hNumberBFramesBetweenReferenceFrames = lens _hNumberBFramesBetweenReferenceFrames (\ s a -> s{_hNumberBFramesBetweenReferenceFrames = a})

-- | Undocumented member.
hGopSizeUnits :: Lens' H264Settings (Maybe H264GopSizeUnits)
hGopSizeUnits = lens _hGopSizeUnits (\ s a -> s{_hGopSizeUnits = a})

-- | Size of buffer (HRD buffer model). Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m.
hHrdBufferSize :: Lens' H264Settings (Maybe Int)
hHrdBufferSize = lens _hHrdBufferSize (\ s a -> s{_hHrdBufferSize = a})

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
hSlices :: Lens' H264Settings (Maybe Int)
hSlices = lens _hSlices (\ s a -> s{_hSlices = a})

-- | Undocumented member.
hRateControlMode :: Lens' H264Settings (Maybe H264RateControlMode)
hRateControlMode = lens _hRateControlMode (\ s a -> s{_hRateControlMode = a})

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
hNumberReferenceFrames :: Lens' H264Settings (Maybe Int)
hNumberReferenceFrames = lens _hNumberReferenceFrames (\ s a -> s{_hNumberReferenceFrames = a})

-- | Undocumented member.
hTelecine :: Lens' H264Settings (Maybe H264Telecine)
hTelecine = lens _hTelecine (\ s a -> s{_hTelecine = a})

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
hMinIInterval :: Lens' H264Settings (Maybe Int)
hMinIInterval = lens _hMinIInterval (\ s a -> s{_hMinIInterval = a})

-- | Undocumented member.
hInterlaceMode :: Lens' H264Settings (Maybe H264InterlaceMode)
hInterlaceMode = lens _hInterlaceMode (\ s a -> s{_hInterlaceMode = a})

-- | Undocumented member.
hParControl :: Lens' H264Settings (Maybe H264ParControl)
hParControl = lens _hParControl (\ s a -> s{_hParControl = a})

-- | Undocumented member.
hRepeatPps :: Lens' H264Settings (Maybe H264RepeatPps)
hRepeatPps = lens _hRepeatPps (\ s a -> s{_hRepeatPps = a})

-- | Undocumented member.
hFlickerAdaptiveQuantization :: Lens' H264Settings (Maybe H264FlickerAdaptiveQuantization)
hFlickerAdaptiveQuantization = lens _hFlickerAdaptiveQuantization (\ s a -> s{_hFlickerAdaptiveQuantization = a})

-- | Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
hSoftness :: Lens' H264Settings (Maybe Int)
hSoftness = lens _hSoftness (\ s a -> s{_hSoftness = a})

-- | Undocumented member.
hCodecProfile :: Lens' H264Settings (Maybe H264CodecProfile)
hCodecProfile = lens _hCodecProfile (\ s a -> s{_hCodecProfile = a})

-- | Average bitrate in bits/second. Required for VBR, CBR, and ABR. Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
hBitrate :: Lens' H264Settings (Maybe Int)
hBitrate = lens _hBitrate (\ s a -> s{_hBitrate = a})

-- | When you use the API for transcode jobs that use framerate conversion, specify the framerate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use framerate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
hFramerateDenominator :: Lens' H264Settings (Maybe Int)
hFramerateDenominator = lens _hFramerateDenominator (\ s a -> s{_hFramerateDenominator = a})

-- | Undocumented member.
hFramerateConversionAlgorithm :: Lens' H264Settings (Maybe H264FramerateConversionAlgorithm)
hFramerateConversionAlgorithm = lens _hFramerateConversionAlgorithm (\ s a -> s{_hFramerateConversionAlgorithm = a})

-- | Undocumented member.
hCodecLevel :: Lens' H264Settings (Maybe H264CodecLevel)
hCodecLevel = lens _hCodecLevel (\ s a -> s{_hCodecLevel = a})

-- | Undocumented member.
hEntropyEncoding :: Lens' H264Settings (Maybe H264EntropyEncoding)
hEntropyEncoding = lens _hEntropyEncoding (\ s a -> s{_hEntropyEncoding = a})

-- | Undocumented member.
hFramerateControl :: Lens' H264Settings (Maybe H264FramerateControl)
hFramerateControl = lens _hFramerateControl (\ s a -> s{_hFramerateControl = a})

-- | Undocumented member.
hAdaptiveQuantization :: Lens' H264Settings (Maybe H264AdaptiveQuantization)
hAdaptiveQuantization = lens _hAdaptiveQuantization (\ s a -> s{_hAdaptiveQuantization = a})

-- | Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
hFramerateNumerator :: Lens' H264Settings (Maybe Int)
hFramerateNumerator = lens _hFramerateNumerator (\ s a -> s{_hFramerateNumerator = a})

-- | Undocumented member.
hGopBReference :: Lens' H264Settings (Maybe H264GopBReference)
hGopBReference = lens _hGopBReference (\ s a -> s{_hGopBReference = a})

-- | Maximum bitrate in bits/second (for VBR mode only). Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m.
hMaxBitrate :: Lens' H264Settings (Maybe Int)
hMaxBitrate = lens _hMaxBitrate (\ s a -> s{_hMaxBitrate = a})

-- | Undocumented member.
hSyntax :: Lens' H264Settings (Maybe H264Syntax)
hSyntax = lens _hSyntax (\ s a -> s{_hSyntax = a})

-- | Undocumented member.
hFieldEncoding :: Lens' H264Settings (Maybe H264FieldEncoding)
hFieldEncoding = lens _hFieldEncoding (\ s a -> s{_hFieldEncoding = a})

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
hGopClosedCadence :: Lens' H264Settings (Maybe Int)
hGopClosedCadence = lens _hGopClosedCadence (\ s a -> s{_hGopClosedCadence = a})

-- | Pixel Aspect Ratio denominator.
hParDenominator :: Lens' H264Settings (Maybe Int)
hParDenominator = lens _hParDenominator (\ s a -> s{_hParDenominator = a})

-- | Undocumented member.
hSpatialAdaptiveQuantization :: Lens' H264Settings (Maybe H264SpatialAdaptiveQuantization)
hSpatialAdaptiveQuantization = lens _hSpatialAdaptiveQuantization (\ s a -> s{_hSpatialAdaptiveQuantization = a})

instance FromJSON H264Settings where
        parseJSON
          = withObject "H264Settings"
              (\ x ->
                 H264Settings' <$>
                   (x .:? "unregisteredSeiTimecode") <*>
                     (x .:? "qualityTuningLevel")
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
                     <*> (x .:? "minIInterval")
                     <*> (x .:? "interlaceMode")
                     <*> (x .:? "parControl")
                     <*> (x .:? "repeatPps")
                     <*> (x .:? "flickerAdaptiveQuantization")
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
                     <*> (x .:? "spatialAdaptiveQuantization"))

instance Hashable H264Settings where

instance NFData H264Settings where

instance ToJSON H264Settings where
        toJSON H264Settings'{..}
          = object
              (catMaybes
                 [("unregisteredSeiTimecode" .=) <$>
                    _hUnregisteredSeiTimecode,
                  ("qualityTuningLevel" .=) <$> _hQualityTuningLevel,
                  ("temporalAdaptiveQuantization" .=) <$>
                    _hTemporalAdaptiveQuantization,
                  ("sceneChangeDetect" .=) <$> _hSceneChangeDetect,
                  ("hrdBufferInitialFillPercentage" .=) <$>
                    _hHrdBufferInitialFillPercentage,
                  ("slowPal" .=) <$> _hSlowPal,
                  ("parNumerator" .=) <$> _hParNumerator,
                  ("gopSize" .=) <$> _hGopSize,
                  ("numberBFramesBetweenReferenceFrames" .=) <$>
                    _hNumberBFramesBetweenReferenceFrames,
                  ("gopSizeUnits" .=) <$> _hGopSizeUnits,
                  ("hrdBufferSize" .=) <$> _hHrdBufferSize,
                  ("slices" .=) <$> _hSlices,
                  ("rateControlMode" .=) <$> _hRateControlMode,
                  ("numberReferenceFrames" .=) <$>
                    _hNumberReferenceFrames,
                  ("telecine" .=) <$> _hTelecine,
                  ("minIInterval" .=) <$> _hMinIInterval,
                  ("interlaceMode" .=) <$> _hInterlaceMode,
                  ("parControl" .=) <$> _hParControl,
                  ("repeatPps" .=) <$> _hRepeatPps,
                  ("flickerAdaptiveQuantization" .=) <$>
                    _hFlickerAdaptiveQuantization,
                  ("softness" .=) <$> _hSoftness,
                  ("codecProfile" .=) <$> _hCodecProfile,
                  ("bitrate" .=) <$> _hBitrate,
                  ("framerateDenominator" .=) <$>
                    _hFramerateDenominator,
                  ("framerateConversionAlgorithm" .=) <$>
                    _hFramerateConversionAlgorithm,
                  ("codecLevel" .=) <$> _hCodecLevel,
                  ("entropyEncoding" .=) <$> _hEntropyEncoding,
                  ("framerateControl" .=) <$> _hFramerateControl,
                  ("adaptiveQuantization" .=) <$>
                    _hAdaptiveQuantization,
                  ("framerateNumerator" .=) <$> _hFramerateNumerator,
                  ("gopBReference" .=) <$> _hGopBReference,
                  ("maxBitrate" .=) <$> _hMaxBitrate,
                  ("syntax" .=) <$> _hSyntax,
                  ("fieldEncoding" .=) <$> _hFieldEncoding,
                  ("gopClosedCadence" .=) <$> _hGopClosedCadence,
                  ("parDenominator" .=) <$> _hParDenominator,
                  ("spatialAdaptiveQuantization" .=) <$>
                    _hSpatialAdaptiveQuantization])

-- | Settings for H265 codec
--
-- /See:/ 'h265Settings' smart constructor.
data H265Settings = H265Settings'
  { _hsUnregisteredSeiTimecode :: !(Maybe H265UnregisteredSeiTimecode)
  , _hsQualityTuningLevel :: !(Maybe H265QualityTuningLevel)
  , _hsTemporalAdaptiveQuantization :: !(Maybe H265TemporalAdaptiveQuantization)
  , _hsSceneChangeDetect :: !(Maybe H265SceneChangeDetect)
  , _hsHrdBufferInitialFillPercentage :: !(Maybe Int)
  , _hsTiles :: !(Maybe H265Tiles)
  , _hsSlowPal :: !(Maybe H265SlowPal)
  , _hsTemporalIds :: !(Maybe H265TemporalIds)
  , _hsParNumerator :: !(Maybe Int)
  , _hsGopSize :: !(Maybe Double)
  , _hsNumberBFramesBetweenReferenceFrames :: !(Maybe Int)
  , _hsGopSizeUnits :: !(Maybe H265GopSizeUnits)
  , _hsHrdBufferSize :: !(Maybe Int)
  , _hsSlices :: !(Maybe Int)
  , _hsAlternateTransferFunctionSei :: !(Maybe H265AlternateTransferFunctionSei)
  , _hsRateControlMode :: !(Maybe H265RateControlMode)
  , _hsNumberReferenceFrames :: !(Maybe Int)
  , _hsTelecine :: !(Maybe H265Telecine)
  , _hsMinIInterval :: !(Maybe Int)
  , _hsInterlaceMode :: !(Maybe H265InterlaceMode)
  , _hsParControl :: !(Maybe H265ParControl)
  , _hsFlickerAdaptiveQuantization :: !(Maybe H265FlickerAdaptiveQuantization)
  , _hsSampleAdaptiveOffsetFilterMode :: !(Maybe H265SampleAdaptiveOffsetFilterMode)
  , _hsCodecProfile :: !(Maybe H265CodecProfile)
  , _hsBitrate :: !(Maybe Int)
  , _hsFramerateDenominator :: !(Maybe Int)
  , _hsFramerateConversionAlgorithm :: !(Maybe H265FramerateConversionAlgorithm)
  , _hsCodecLevel :: !(Maybe H265CodecLevel)
  , _hsFramerateControl :: !(Maybe H265FramerateControl)
  , _hsAdaptiveQuantization :: !(Maybe H265AdaptiveQuantization)
  , _hsFramerateNumerator :: !(Maybe Int)
  , _hsGopBReference :: !(Maybe H265GopBReference)
  , _hsMaxBitrate :: !(Maybe Int)
  , _hsGopClosedCadence :: !(Maybe Int)
  , _hsParDenominator :: !(Maybe Int)
  , _hsSpatialAdaptiveQuantization :: !(Maybe H265SpatialAdaptiveQuantization)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'H265Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsUnregisteredSeiTimecode' - Undocumented member.
--
-- * 'hsQualityTuningLevel' - Undocumented member.
--
-- * 'hsTemporalAdaptiveQuantization' - Undocumented member.
--
-- * 'hsSceneChangeDetect' - Undocumented member.
--
-- * 'hsHrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- * 'hsTiles' - Undocumented member.
--
-- * 'hsSlowPal' - Undocumented member.
--
-- * 'hsTemporalIds' - Undocumented member.
--
-- * 'hsParNumerator' - Pixel Aspect Ratio numerator.
--
-- * 'hsGopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- * 'hsNumberBFramesBetweenReferenceFrames' - Number of B-frames between reference frames.
--
-- * 'hsGopSizeUnits' - Undocumented member.
--
-- * 'hsHrdBufferSize' - Size of buffer (HRD buffer model). Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m.
--
-- * 'hsSlices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- * 'hsAlternateTransferFunctionSei' - Undocumented member.
--
-- * 'hsRateControlMode' - Undocumented member.
--
-- * 'hsNumberReferenceFrames' - Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- * 'hsTelecine' - Undocumented member.
--
-- * 'hsMinIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'hsInterlaceMode' - Undocumented member.
--
-- * 'hsParControl' - Undocumented member.
--
-- * 'hsFlickerAdaptiveQuantization' - Undocumented member.
--
-- * 'hsSampleAdaptiveOffsetFilterMode' - Undocumented member.
--
-- * 'hsCodecProfile' - Undocumented member.
--
-- * 'hsBitrate' - Average bitrate in bits/second. Required for VBR, CBR, and ABR. Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- * 'hsFramerateDenominator' - Framerate denominator.
--
-- * 'hsFramerateConversionAlgorithm' - Undocumented member.
--
-- * 'hsCodecLevel' - Undocumented member.
--
-- * 'hsFramerateControl' - Undocumented member.
--
-- * 'hsAdaptiveQuantization' - Undocumented member.
--
-- * 'hsFramerateNumerator' - Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- * 'hsGopBReference' - Undocumented member.
--
-- * 'hsMaxBitrate' - Maximum bitrate in bits/second (for VBR mode only). Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m.
--
-- * 'hsGopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- * 'hsParDenominator' - Pixel Aspect Ratio denominator.
--
-- * 'hsSpatialAdaptiveQuantization' - Undocumented member.
h265Settings
    :: H265Settings
h265Settings =
  H265Settings'
    { _hsUnregisteredSeiTimecode = Nothing
    , _hsQualityTuningLevel = Nothing
    , _hsTemporalAdaptiveQuantization = Nothing
    , _hsSceneChangeDetect = Nothing
    , _hsHrdBufferInitialFillPercentage = Nothing
    , _hsTiles = Nothing
    , _hsSlowPal = Nothing
    , _hsTemporalIds = Nothing
    , _hsParNumerator = Nothing
    , _hsGopSize = Nothing
    , _hsNumberBFramesBetweenReferenceFrames = Nothing
    , _hsGopSizeUnits = Nothing
    , _hsHrdBufferSize = Nothing
    , _hsSlices = Nothing
    , _hsAlternateTransferFunctionSei = Nothing
    , _hsRateControlMode = Nothing
    , _hsNumberReferenceFrames = Nothing
    , _hsTelecine = Nothing
    , _hsMinIInterval = Nothing
    , _hsInterlaceMode = Nothing
    , _hsParControl = Nothing
    , _hsFlickerAdaptiveQuantization = Nothing
    , _hsSampleAdaptiveOffsetFilterMode = Nothing
    , _hsCodecProfile = Nothing
    , _hsBitrate = Nothing
    , _hsFramerateDenominator = Nothing
    , _hsFramerateConversionAlgorithm = Nothing
    , _hsCodecLevel = Nothing
    , _hsFramerateControl = Nothing
    , _hsAdaptiveQuantization = Nothing
    , _hsFramerateNumerator = Nothing
    , _hsGopBReference = Nothing
    , _hsMaxBitrate = Nothing
    , _hsGopClosedCadence = Nothing
    , _hsParDenominator = Nothing
    , _hsSpatialAdaptiveQuantization = Nothing
    }


-- | Undocumented member.
hsUnregisteredSeiTimecode :: Lens' H265Settings (Maybe H265UnregisteredSeiTimecode)
hsUnregisteredSeiTimecode = lens _hsUnregisteredSeiTimecode (\ s a -> s{_hsUnregisteredSeiTimecode = a})

-- | Undocumented member.
hsQualityTuningLevel :: Lens' H265Settings (Maybe H265QualityTuningLevel)
hsQualityTuningLevel = lens _hsQualityTuningLevel (\ s a -> s{_hsQualityTuningLevel = a})

-- | Undocumented member.
hsTemporalAdaptiveQuantization :: Lens' H265Settings (Maybe H265TemporalAdaptiveQuantization)
hsTemporalAdaptiveQuantization = lens _hsTemporalAdaptiveQuantization (\ s a -> s{_hsTemporalAdaptiveQuantization = a})

-- | Undocumented member.
hsSceneChangeDetect :: Lens' H265Settings (Maybe H265SceneChangeDetect)
hsSceneChangeDetect = lens _hsSceneChangeDetect (\ s a -> s{_hsSceneChangeDetect = a})

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
hsHrdBufferInitialFillPercentage :: Lens' H265Settings (Maybe Int)
hsHrdBufferInitialFillPercentage = lens _hsHrdBufferInitialFillPercentage (\ s a -> s{_hsHrdBufferInitialFillPercentage = a})

-- | Undocumented member.
hsTiles :: Lens' H265Settings (Maybe H265Tiles)
hsTiles = lens _hsTiles (\ s a -> s{_hsTiles = a})

-- | Undocumented member.
hsSlowPal :: Lens' H265Settings (Maybe H265SlowPal)
hsSlowPal = lens _hsSlowPal (\ s a -> s{_hsSlowPal = a})

-- | Undocumented member.
hsTemporalIds :: Lens' H265Settings (Maybe H265TemporalIds)
hsTemporalIds = lens _hsTemporalIds (\ s a -> s{_hsTemporalIds = a})

-- | Pixel Aspect Ratio numerator.
hsParNumerator :: Lens' H265Settings (Maybe Int)
hsParNumerator = lens _hsParNumerator (\ s a -> s{_hsParNumerator = a})

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
hsGopSize :: Lens' H265Settings (Maybe Double)
hsGopSize = lens _hsGopSize (\ s a -> s{_hsGopSize = a})

-- | Number of B-frames between reference frames.
hsNumberBFramesBetweenReferenceFrames :: Lens' H265Settings (Maybe Int)
hsNumberBFramesBetweenReferenceFrames = lens _hsNumberBFramesBetweenReferenceFrames (\ s a -> s{_hsNumberBFramesBetweenReferenceFrames = a})

-- | Undocumented member.
hsGopSizeUnits :: Lens' H265Settings (Maybe H265GopSizeUnits)
hsGopSizeUnits = lens _hsGopSizeUnits (\ s a -> s{_hsGopSizeUnits = a})

-- | Size of buffer (HRD buffer model). Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m.
hsHrdBufferSize :: Lens' H265Settings (Maybe Int)
hsHrdBufferSize = lens _hsHrdBufferSize (\ s a -> s{_hsHrdBufferSize = a})

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
hsSlices :: Lens' H265Settings (Maybe Int)
hsSlices = lens _hsSlices (\ s a -> s{_hsSlices = a})

-- | Undocumented member.
hsAlternateTransferFunctionSei :: Lens' H265Settings (Maybe H265AlternateTransferFunctionSei)
hsAlternateTransferFunctionSei = lens _hsAlternateTransferFunctionSei (\ s a -> s{_hsAlternateTransferFunctionSei = a})

-- | Undocumented member.
hsRateControlMode :: Lens' H265Settings (Maybe H265RateControlMode)
hsRateControlMode = lens _hsRateControlMode (\ s a -> s{_hsRateControlMode = a})

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
hsNumberReferenceFrames :: Lens' H265Settings (Maybe Int)
hsNumberReferenceFrames = lens _hsNumberReferenceFrames (\ s a -> s{_hsNumberReferenceFrames = a})

-- | Undocumented member.
hsTelecine :: Lens' H265Settings (Maybe H265Telecine)
hsTelecine = lens _hsTelecine (\ s a -> s{_hsTelecine = a})

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
hsMinIInterval :: Lens' H265Settings (Maybe Int)
hsMinIInterval = lens _hsMinIInterval (\ s a -> s{_hsMinIInterval = a})

-- | Undocumented member.
hsInterlaceMode :: Lens' H265Settings (Maybe H265InterlaceMode)
hsInterlaceMode = lens _hsInterlaceMode (\ s a -> s{_hsInterlaceMode = a})

-- | Undocumented member.
hsParControl :: Lens' H265Settings (Maybe H265ParControl)
hsParControl = lens _hsParControl (\ s a -> s{_hsParControl = a})

-- | Undocumented member.
hsFlickerAdaptiveQuantization :: Lens' H265Settings (Maybe H265FlickerAdaptiveQuantization)
hsFlickerAdaptiveQuantization = lens _hsFlickerAdaptiveQuantization (\ s a -> s{_hsFlickerAdaptiveQuantization = a})

-- | Undocumented member.
hsSampleAdaptiveOffsetFilterMode :: Lens' H265Settings (Maybe H265SampleAdaptiveOffsetFilterMode)
hsSampleAdaptiveOffsetFilterMode = lens _hsSampleAdaptiveOffsetFilterMode (\ s a -> s{_hsSampleAdaptiveOffsetFilterMode = a})

-- | Undocumented member.
hsCodecProfile :: Lens' H265Settings (Maybe H265CodecProfile)
hsCodecProfile = lens _hsCodecProfile (\ s a -> s{_hsCodecProfile = a})

-- | Average bitrate in bits/second. Required for VBR, CBR, and ABR. Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
hsBitrate :: Lens' H265Settings (Maybe Int)
hsBitrate = lens _hsBitrate (\ s a -> s{_hsBitrate = a})

-- | Framerate denominator.
hsFramerateDenominator :: Lens' H265Settings (Maybe Int)
hsFramerateDenominator = lens _hsFramerateDenominator (\ s a -> s{_hsFramerateDenominator = a})

-- | Undocumented member.
hsFramerateConversionAlgorithm :: Lens' H265Settings (Maybe H265FramerateConversionAlgorithm)
hsFramerateConversionAlgorithm = lens _hsFramerateConversionAlgorithm (\ s a -> s{_hsFramerateConversionAlgorithm = a})

-- | Undocumented member.
hsCodecLevel :: Lens' H265Settings (Maybe H265CodecLevel)
hsCodecLevel = lens _hsCodecLevel (\ s a -> s{_hsCodecLevel = a})

-- | Undocumented member.
hsFramerateControl :: Lens' H265Settings (Maybe H265FramerateControl)
hsFramerateControl = lens _hsFramerateControl (\ s a -> s{_hsFramerateControl = a})

-- | Undocumented member.
hsAdaptiveQuantization :: Lens' H265Settings (Maybe H265AdaptiveQuantization)
hsAdaptiveQuantization = lens _hsAdaptiveQuantization (\ s a -> s{_hsAdaptiveQuantization = a})

-- | Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
hsFramerateNumerator :: Lens' H265Settings (Maybe Int)
hsFramerateNumerator = lens _hsFramerateNumerator (\ s a -> s{_hsFramerateNumerator = a})

-- | Undocumented member.
hsGopBReference :: Lens' H265Settings (Maybe H265GopBReference)
hsGopBReference = lens _hsGopBReference (\ s a -> s{_hsGopBReference = a})

-- | Maximum bitrate in bits/second (for VBR mode only). Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m.
hsMaxBitrate :: Lens' H265Settings (Maybe Int)
hsMaxBitrate = lens _hsMaxBitrate (\ s a -> s{_hsMaxBitrate = a})

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
hsGopClosedCadence :: Lens' H265Settings (Maybe Int)
hsGopClosedCadence = lens _hsGopClosedCadence (\ s a -> s{_hsGopClosedCadence = a})

-- | Pixel Aspect Ratio denominator.
hsParDenominator :: Lens' H265Settings (Maybe Int)
hsParDenominator = lens _hsParDenominator (\ s a -> s{_hsParDenominator = a})

-- | Undocumented member.
hsSpatialAdaptiveQuantization :: Lens' H265Settings (Maybe H265SpatialAdaptiveQuantization)
hsSpatialAdaptiveQuantization = lens _hsSpatialAdaptiveQuantization (\ s a -> s{_hsSpatialAdaptiveQuantization = a})

instance FromJSON H265Settings where
        parseJSON
          = withObject "H265Settings"
              (\ x ->
                 H265Settings' <$>
                   (x .:? "unregisteredSeiTimecode") <*>
                     (x .:? "qualityTuningLevel")
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
                     <*> (x .:? "minIInterval")
                     <*> (x .:? "interlaceMode")
                     <*> (x .:? "parControl")
                     <*> (x .:? "flickerAdaptiveQuantization")
                     <*> (x .:? "sampleAdaptiveOffsetFilterMode")
                     <*> (x .:? "codecProfile")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "framerateDenominator")
                     <*> (x .:? "framerateConversionAlgorithm")
                     <*> (x .:? "codecLevel")
                     <*> (x .:? "framerateControl")
                     <*> (x .:? "adaptiveQuantization")
                     <*> (x .:? "framerateNumerator")
                     <*> (x .:? "gopBReference")
                     <*> (x .:? "maxBitrate")
                     <*> (x .:? "gopClosedCadence")
                     <*> (x .:? "parDenominator")
                     <*> (x .:? "spatialAdaptiveQuantization"))

instance Hashable H265Settings where

instance NFData H265Settings where

instance ToJSON H265Settings where
        toJSON H265Settings'{..}
          = object
              (catMaybes
                 [("unregisteredSeiTimecode" .=) <$>
                    _hsUnregisteredSeiTimecode,
                  ("qualityTuningLevel" .=) <$> _hsQualityTuningLevel,
                  ("temporalAdaptiveQuantization" .=) <$>
                    _hsTemporalAdaptiveQuantization,
                  ("sceneChangeDetect" .=) <$> _hsSceneChangeDetect,
                  ("hrdBufferInitialFillPercentage" .=) <$>
                    _hsHrdBufferInitialFillPercentage,
                  ("tiles" .=) <$> _hsTiles,
                  ("slowPal" .=) <$> _hsSlowPal,
                  ("temporalIds" .=) <$> _hsTemporalIds,
                  ("parNumerator" .=) <$> _hsParNumerator,
                  ("gopSize" .=) <$> _hsGopSize,
                  ("numberBFramesBetweenReferenceFrames" .=) <$>
                    _hsNumberBFramesBetweenReferenceFrames,
                  ("gopSizeUnits" .=) <$> _hsGopSizeUnits,
                  ("hrdBufferSize" .=) <$> _hsHrdBufferSize,
                  ("slices" .=) <$> _hsSlices,
                  ("alternateTransferFunctionSei" .=) <$>
                    _hsAlternateTransferFunctionSei,
                  ("rateControlMode" .=) <$> _hsRateControlMode,
                  ("numberReferenceFrames" .=) <$>
                    _hsNumberReferenceFrames,
                  ("telecine" .=) <$> _hsTelecine,
                  ("minIInterval" .=) <$> _hsMinIInterval,
                  ("interlaceMode" .=) <$> _hsInterlaceMode,
                  ("parControl" .=) <$> _hsParControl,
                  ("flickerAdaptiveQuantization" .=) <$>
                    _hsFlickerAdaptiveQuantization,
                  ("sampleAdaptiveOffsetFilterMode" .=) <$>
                    _hsSampleAdaptiveOffsetFilterMode,
                  ("codecProfile" .=) <$> _hsCodecProfile,
                  ("bitrate" .=) <$> _hsBitrate,
                  ("framerateDenominator" .=) <$>
                    _hsFramerateDenominator,
                  ("framerateConversionAlgorithm" .=) <$>
                    _hsFramerateConversionAlgorithm,
                  ("codecLevel" .=) <$> _hsCodecLevel,
                  ("framerateControl" .=) <$> _hsFramerateControl,
                  ("adaptiveQuantization" .=) <$>
                    _hsAdaptiveQuantization,
                  ("framerateNumerator" .=) <$> _hsFramerateNumerator,
                  ("gopBReference" .=) <$> _hsGopBReference,
                  ("maxBitrate" .=) <$> _hsMaxBitrate,
                  ("gopClosedCadence" .=) <$> _hsGopClosedCadence,
                  ("parDenominator" .=) <$> _hsParDenominator,
                  ("spatialAdaptiveQuantization" .=) <$>
                    _hsSpatialAdaptiveQuantization])

-- | Use the HDR master display (Hdr10Metadata) settings to provide values for HDR color. These values vary depending on the input video and must be provided by a color grader. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
--
-- /See:/ 'hdr10Metadata' smart constructor.
data Hdr10Metadata = Hdr10Metadata'
  { _hmRedPrimaryX               :: !(Maybe Int)
  , _hmBluePrimaryX              :: !(Maybe Int)
  , _hmMaxFrameAverageLightLevel :: !(Maybe Int)
  , _hmWhitePointY               :: !(Maybe Int)
  , _hmMaxContentLightLevel      :: !(Maybe Int)
  , _hmWhitePointX               :: !(Maybe Int)
  , _hmBluePrimaryY              :: !(Maybe Int)
  , _hmGreenPrimaryY             :: !(Maybe Int)
  , _hmGreenPrimaryX             :: !(Maybe Int)
  , _hmMinLuminance              :: !(Maybe Int)
  , _hmRedPrimaryY               :: !(Maybe Int)
  , _hmMaxLuminance              :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Hdr10Metadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hmRedPrimaryX' - HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
--
-- * 'hmBluePrimaryX' - HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
--
-- * 'hmMaxFrameAverageLightLevel' - Maximum average light level of any frame in the coded video sequence, in units of candelas per square meter.
--
-- * 'hmWhitePointY' - HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
--
-- * 'hmMaxContentLightLevel' - Maximum light level among all samples in the coded video sequence, in units of candelas per square meter.
--
-- * 'hmWhitePointX' - HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
--
-- * 'hmBluePrimaryY' - HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
--
-- * 'hmGreenPrimaryY' - HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
--
-- * 'hmGreenPrimaryX' - HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
--
-- * 'hmMinLuminance' - Nominal minimum mastering display luminance in units of of 0.0001 candelas per square meter
--
-- * 'hmRedPrimaryY' - HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
--
-- * 'hmMaxLuminance' - Nominal maximum mastering display luminance in units of of 0.0001 candelas per square meter.
hdr10Metadata
    :: Hdr10Metadata
hdr10Metadata =
  Hdr10Metadata'
    { _hmRedPrimaryX = Nothing
    , _hmBluePrimaryX = Nothing
    , _hmMaxFrameAverageLightLevel = Nothing
    , _hmWhitePointY = Nothing
    , _hmMaxContentLightLevel = Nothing
    , _hmWhitePointX = Nothing
    , _hmBluePrimaryY = Nothing
    , _hmGreenPrimaryY = Nothing
    , _hmGreenPrimaryX = Nothing
    , _hmMinLuminance = Nothing
    , _hmRedPrimaryY = Nothing
    , _hmMaxLuminance = Nothing
    }


-- | HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
hmRedPrimaryX :: Lens' Hdr10Metadata (Maybe Int)
hmRedPrimaryX = lens _hmRedPrimaryX (\ s a -> s{_hmRedPrimaryX = a})

-- | HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
hmBluePrimaryX :: Lens' Hdr10Metadata (Maybe Int)
hmBluePrimaryX = lens _hmBluePrimaryX (\ s a -> s{_hmBluePrimaryX = a})

-- | Maximum average light level of any frame in the coded video sequence, in units of candelas per square meter.
hmMaxFrameAverageLightLevel :: Lens' Hdr10Metadata (Maybe Int)
hmMaxFrameAverageLightLevel = lens _hmMaxFrameAverageLightLevel (\ s a -> s{_hmMaxFrameAverageLightLevel = a})

-- | HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
hmWhitePointY :: Lens' Hdr10Metadata (Maybe Int)
hmWhitePointY = lens _hmWhitePointY (\ s a -> s{_hmWhitePointY = a})

-- | Maximum light level among all samples in the coded video sequence, in units of candelas per square meter.
hmMaxContentLightLevel :: Lens' Hdr10Metadata (Maybe Int)
hmMaxContentLightLevel = lens _hmMaxContentLightLevel (\ s a -> s{_hmMaxContentLightLevel = a})

-- | HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
hmWhitePointX :: Lens' Hdr10Metadata (Maybe Int)
hmWhitePointX = lens _hmWhitePointX (\ s a -> s{_hmWhitePointX = a})

-- | HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
hmBluePrimaryY :: Lens' Hdr10Metadata (Maybe Int)
hmBluePrimaryY = lens _hmBluePrimaryY (\ s a -> s{_hmBluePrimaryY = a})

-- | HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
hmGreenPrimaryY :: Lens' Hdr10Metadata (Maybe Int)
hmGreenPrimaryY = lens _hmGreenPrimaryY (\ s a -> s{_hmGreenPrimaryY = a})

-- | HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
hmGreenPrimaryX :: Lens' Hdr10Metadata (Maybe Int)
hmGreenPrimaryX = lens _hmGreenPrimaryX (\ s a -> s{_hmGreenPrimaryX = a})

-- | Nominal minimum mastering display luminance in units of of 0.0001 candelas per square meter
hmMinLuminance :: Lens' Hdr10Metadata (Maybe Int)
hmMinLuminance = lens _hmMinLuminance (\ s a -> s{_hmMinLuminance = a})

-- | HDR Master Display Information comes from the color grader and the color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
hmRedPrimaryY :: Lens' Hdr10Metadata (Maybe Int)
hmRedPrimaryY = lens _hmRedPrimaryY (\ s a -> s{_hmRedPrimaryY = a})

-- | Nominal maximum mastering display luminance in units of of 0.0001 candelas per square meter.
hmMaxLuminance :: Lens' Hdr10Metadata (Maybe Int)
hmMaxLuminance = lens _hmMaxLuminance (\ s a -> s{_hmMaxLuminance = a})

instance FromJSON Hdr10Metadata where
        parseJSON
          = withObject "Hdr10Metadata"
              (\ x ->
                 Hdr10Metadata' <$>
                   (x .:? "redPrimaryX") <*> (x .:? "bluePrimaryX") <*>
                     (x .:? "maxFrameAverageLightLevel")
                     <*> (x .:? "whitePointY")
                     <*> (x .:? "maxContentLightLevel")
                     <*> (x .:? "whitePointX")
                     <*> (x .:? "bluePrimaryY")
                     <*> (x .:? "greenPrimaryY")
                     <*> (x .:? "greenPrimaryX")
                     <*> (x .:? "minLuminance")
                     <*> (x .:? "redPrimaryY")
                     <*> (x .:? "maxLuminance"))

instance Hashable Hdr10Metadata where

instance NFData Hdr10Metadata where

instance ToJSON Hdr10Metadata where
        toJSON Hdr10Metadata'{..}
          = object
              (catMaybes
                 [("redPrimaryX" .=) <$> _hmRedPrimaryX,
                  ("bluePrimaryX" .=) <$> _hmBluePrimaryX,
                  ("maxFrameAverageLightLevel" .=) <$>
                    _hmMaxFrameAverageLightLevel,
                  ("whitePointY" .=) <$> _hmWhitePointY,
                  ("maxContentLightLevel" .=) <$>
                    _hmMaxContentLightLevel,
                  ("whitePointX" .=) <$> _hmWhitePointX,
                  ("bluePrimaryY" .=) <$> _hmBluePrimaryY,
                  ("greenPrimaryY" .=) <$> _hmGreenPrimaryY,
                  ("greenPrimaryX" .=) <$> _hmGreenPrimaryX,
                  ("minLuminance" .=) <$> _hmMinLuminance,
                  ("redPrimaryY" .=) <$> _hmRedPrimaryY,
                  ("maxLuminance" .=) <$> _hmMaxLuminance])

-- | Caption Language Mapping
--
-- /See:/ 'hlsCaptionLanguageMapping' smart constructor.
data HlsCaptionLanguageMapping = HlsCaptionLanguageMapping'
  { _hclmLanguageCode        :: !(Maybe LanguageCode)
  , _hclmLanguageDescription :: !(Maybe Text)
  , _hclmCaptionChannel      :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HlsCaptionLanguageMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hclmLanguageCode' - Undocumented member.
--
-- * 'hclmLanguageDescription' - Caption language description.
--
-- * 'hclmCaptionChannel' - Caption channel.
hlsCaptionLanguageMapping
    :: HlsCaptionLanguageMapping
hlsCaptionLanguageMapping =
  HlsCaptionLanguageMapping'
    { _hclmLanguageCode = Nothing
    , _hclmLanguageDescription = Nothing
    , _hclmCaptionChannel = Nothing
    }


-- | Undocumented member.
hclmLanguageCode :: Lens' HlsCaptionLanguageMapping (Maybe LanguageCode)
hclmLanguageCode = lens _hclmLanguageCode (\ s a -> s{_hclmLanguageCode = a})

-- | Caption language description.
hclmLanguageDescription :: Lens' HlsCaptionLanguageMapping (Maybe Text)
hclmLanguageDescription = lens _hclmLanguageDescription (\ s a -> s{_hclmLanguageDescription = a})

-- | Caption channel.
hclmCaptionChannel :: Lens' HlsCaptionLanguageMapping (Maybe Int)
hclmCaptionChannel = lens _hclmCaptionChannel (\ s a -> s{_hclmCaptionChannel = a})

instance FromJSON HlsCaptionLanguageMapping where
        parseJSON
          = withObject "HlsCaptionLanguageMapping"
              (\ x ->
                 HlsCaptionLanguageMapping' <$>
                   (x .:? "languageCode") <*>
                     (x .:? "languageDescription")
                     <*> (x .:? "captionChannel"))

instance Hashable HlsCaptionLanguageMapping where

instance NFData HlsCaptionLanguageMapping where

instance ToJSON HlsCaptionLanguageMapping where
        toJSON HlsCaptionLanguageMapping'{..}
          = object
              (catMaybes
                 [("languageCode" .=) <$> _hclmLanguageCode,
                  ("languageDescription" .=) <$>
                    _hclmLanguageDescription,
                  ("captionChannel" .=) <$> _hclmCaptionChannel])

-- | Settings for HLS encryption
--
-- /See:/ 'hlsEncryptionSettings' smart constructor.
data HlsEncryptionSettings = HlsEncryptionSettings'
  { _hesEncryptionMethod :: !(Maybe HlsEncryptionType)
  , _hesConstantInitializationVector :: !(Maybe Text)
  , _hesType :: !(Maybe HlsKeyProviderType)
  , _hesStaticKeyProvider :: !(Maybe StaticKeyProvider)
  , _hesSpekeKeyProvider :: !(Maybe SpekeKeyProvider)
  , _hesInitializationVectorInManifest :: !(Maybe HlsInitializationVectorInManifest)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HlsEncryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hesEncryptionMethod' - Undocumented member.
--
-- * 'hesConstantInitializationVector' - This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
--
-- * 'hesType' - Undocumented member.
--
-- * 'hesStaticKeyProvider' - Undocumented member.
--
-- * 'hesSpekeKeyProvider' - Undocumented member.
--
-- * 'hesInitializationVectorInManifest' - Undocumented member.
hlsEncryptionSettings
    :: HlsEncryptionSettings
hlsEncryptionSettings =
  HlsEncryptionSettings'
    { _hesEncryptionMethod = Nothing
    , _hesConstantInitializationVector = Nothing
    , _hesType = Nothing
    , _hesStaticKeyProvider = Nothing
    , _hesSpekeKeyProvider = Nothing
    , _hesInitializationVectorInManifest = Nothing
    }


-- | Undocumented member.
hesEncryptionMethod :: Lens' HlsEncryptionSettings (Maybe HlsEncryptionType)
hesEncryptionMethod = lens _hesEncryptionMethod (\ s a -> s{_hesEncryptionMethod = a})

-- | This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
hesConstantInitializationVector :: Lens' HlsEncryptionSettings (Maybe Text)
hesConstantInitializationVector = lens _hesConstantInitializationVector (\ s a -> s{_hesConstantInitializationVector = a})

-- | Undocumented member.
hesType :: Lens' HlsEncryptionSettings (Maybe HlsKeyProviderType)
hesType = lens _hesType (\ s a -> s{_hesType = a})

-- | Undocumented member.
hesStaticKeyProvider :: Lens' HlsEncryptionSettings (Maybe StaticKeyProvider)
hesStaticKeyProvider = lens _hesStaticKeyProvider (\ s a -> s{_hesStaticKeyProvider = a})

-- | Undocumented member.
hesSpekeKeyProvider :: Lens' HlsEncryptionSettings (Maybe SpekeKeyProvider)
hesSpekeKeyProvider = lens _hesSpekeKeyProvider (\ s a -> s{_hesSpekeKeyProvider = a})

-- | Undocumented member.
hesInitializationVectorInManifest :: Lens' HlsEncryptionSettings (Maybe HlsInitializationVectorInManifest)
hesInitializationVectorInManifest = lens _hesInitializationVectorInManifest (\ s a -> s{_hesInitializationVectorInManifest = a})

instance FromJSON HlsEncryptionSettings where
        parseJSON
          = withObject "HlsEncryptionSettings"
              (\ x ->
                 HlsEncryptionSettings' <$>
                   (x .:? "encryptionMethod") <*>
                     (x .:? "constantInitializationVector")
                     <*> (x .:? "type")
                     <*> (x .:? "staticKeyProvider")
                     <*> (x .:? "spekeKeyProvider")
                     <*> (x .:? "initializationVectorInManifest"))

instance Hashable HlsEncryptionSettings where

instance NFData HlsEncryptionSettings where

instance ToJSON HlsEncryptionSettings where
        toJSON HlsEncryptionSettings'{..}
          = object
              (catMaybes
                 [("encryptionMethod" .=) <$> _hesEncryptionMethod,
                  ("constantInitializationVector" .=) <$>
                    _hesConstantInitializationVector,
                  ("type" .=) <$> _hesType,
                  ("staticKeyProvider" .=) <$> _hesStaticKeyProvider,
                  ("spekeKeyProvider" .=) <$> _hesSpekeKeyProvider,
                  ("initializationVectorInManifest" .=) <$>
                    _hesInitializationVectorInManifest])

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to HLS_GROUP_SETTINGS.
--
-- /See:/ 'hlsGroupSettings' smart constructor.
data HlsGroupSettings = HlsGroupSettings'
  { _hgsDirectoryStructure         :: !(Maybe HlsDirectoryStructure)
  , _hgsSegmentControl             :: !(Maybe HlsSegmentControl)
  , _hgsDestination                :: !(Maybe Text)
  , _hgsTimedMetadataId3Period     :: !(Maybe Int)
  , _hgsMinSegmentLength           :: !(Maybe Int)
  , _hgsProgramDateTime            :: !(Maybe HlsProgramDateTime)
  , _hgsProgramDateTimePeriod      :: !(Maybe Int)
  , _hgsCodecSpecification         :: !(Maybe HlsCodecSpecification)
  , _hgsCaptionLanguageMappings    :: !(Maybe [HlsCaptionLanguageMapping])
  , _hgsBaseURL                    :: !(Maybe Text)
  , _hgsAdMarkers                  :: !(Maybe [HlsAdMarkers])
  , _hgsEncryption                 :: !(Maybe HlsEncryptionSettings)
  , _hgsSegmentLength              :: !(Maybe Int)
  , _hgsTimedMetadataId3Frame      :: !(Maybe HlsTimedMetadataId3Frame)
  , _hgsOutputSelection            :: !(Maybe HlsOutputSelection)
  , _hgsCaptionLanguageSetting     :: !(Maybe HlsCaptionLanguageSetting)
  , _hgsSegmentsPerSubdirectory    :: !(Maybe Int)
  , _hgsManifestDurationFormat     :: !(Maybe HlsManifestDurationFormat)
  , _hgsClientCache                :: !(Maybe HlsClientCache)
  , _hgsTimestampDeltaMilliseconds :: !(Maybe Int)
  , _hgsStreamInfResolution        :: !(Maybe HlsStreamInfResolution)
  , _hgsManifestCompression        :: !(Maybe HlsManifestCompression)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HlsGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hgsDirectoryStructure' - Undocumented member.
--
-- * 'hgsSegmentControl' - Undocumented member.
--
-- * 'hgsDestination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- * 'hgsTimedMetadataId3Period' - Timed Metadata interval in seconds.
--
-- * 'hgsMinSegmentLength' - When set, Minimum Segment Size is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
--
-- * 'hgsProgramDateTime' - Undocumented member.
--
-- * 'hgsProgramDateTimePeriod' - Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
--
-- * 'hgsCodecSpecification' - Undocumented member.
--
-- * 'hgsCaptionLanguageMappings' - Language to be used on Caption outputs
--
-- * 'hgsBaseURL' - A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
--
-- * 'hgsAdMarkers' - Choose one or more ad marker types to pass SCTE35 signals through to this group of Apple HLS outputs.
--
-- * 'hgsEncryption' - DRM settings.
--
-- * 'hgsSegmentLength' - Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
--
-- * 'hgsTimedMetadataId3Frame' - Undocumented member.
--
-- * 'hgsOutputSelection' - Undocumented member.
--
-- * 'hgsCaptionLanguageSetting' - Undocumented member.
--
-- * 'hgsSegmentsPerSubdirectory' - Number of segments to write to a subdirectory before starting a new one. directoryStructure must be SINGLE_DIRECTORY for this setting to have an effect.
--
-- * 'hgsManifestDurationFormat' - Undocumented member.
--
-- * 'hgsClientCache' - Undocumented member.
--
-- * 'hgsTimestampDeltaMilliseconds' - Provides an extra millisecond delta offset to fine tune the timestamps.
--
-- * 'hgsStreamInfResolution' - Undocumented member.
--
-- * 'hgsManifestCompression' - Undocumented member.
hlsGroupSettings
    :: HlsGroupSettings
hlsGroupSettings =
  HlsGroupSettings'
    { _hgsDirectoryStructure = Nothing
    , _hgsSegmentControl = Nothing
    , _hgsDestination = Nothing
    , _hgsTimedMetadataId3Period = Nothing
    , _hgsMinSegmentLength = Nothing
    , _hgsProgramDateTime = Nothing
    , _hgsProgramDateTimePeriod = Nothing
    , _hgsCodecSpecification = Nothing
    , _hgsCaptionLanguageMappings = Nothing
    , _hgsBaseURL = Nothing
    , _hgsAdMarkers = Nothing
    , _hgsEncryption = Nothing
    , _hgsSegmentLength = Nothing
    , _hgsTimedMetadataId3Frame = Nothing
    , _hgsOutputSelection = Nothing
    , _hgsCaptionLanguageSetting = Nothing
    , _hgsSegmentsPerSubdirectory = Nothing
    , _hgsManifestDurationFormat = Nothing
    , _hgsClientCache = Nothing
    , _hgsTimestampDeltaMilliseconds = Nothing
    , _hgsStreamInfResolution = Nothing
    , _hgsManifestCompression = Nothing
    }


-- | Undocumented member.
hgsDirectoryStructure :: Lens' HlsGroupSettings (Maybe HlsDirectoryStructure)
hgsDirectoryStructure = lens _hgsDirectoryStructure (\ s a -> s{_hgsDirectoryStructure = a})

-- | Undocumented member.
hgsSegmentControl :: Lens' HlsGroupSettings (Maybe HlsSegmentControl)
hgsSegmentControl = lens _hgsSegmentControl (\ s a -> s{_hgsSegmentControl = a})

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
hgsDestination :: Lens' HlsGroupSettings (Maybe Text)
hgsDestination = lens _hgsDestination (\ s a -> s{_hgsDestination = a})

-- | Timed Metadata interval in seconds.
hgsTimedMetadataId3Period :: Lens' HlsGroupSettings (Maybe Int)
hgsTimedMetadataId3Period = lens _hgsTimedMetadataId3Period (\ s a -> s{_hgsTimedMetadataId3Period = a})

-- | When set, Minimum Segment Size is enforced by looking ahead and back within the specified range for a nearby avail and extending the segment size if needed.
hgsMinSegmentLength :: Lens' HlsGroupSettings (Maybe Int)
hgsMinSegmentLength = lens _hgsMinSegmentLength (\ s a -> s{_hgsMinSegmentLength = a})

-- | Undocumented member.
hgsProgramDateTime :: Lens' HlsGroupSettings (Maybe HlsProgramDateTime)
hgsProgramDateTime = lens _hgsProgramDateTime (\ s a -> s{_hgsProgramDateTime = a})

-- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
hgsProgramDateTimePeriod :: Lens' HlsGroupSettings (Maybe Int)
hgsProgramDateTimePeriod = lens _hgsProgramDateTimePeriod (\ s a -> s{_hgsProgramDateTimePeriod = a})

-- | Undocumented member.
hgsCodecSpecification :: Lens' HlsGroupSettings (Maybe HlsCodecSpecification)
hgsCodecSpecification = lens _hgsCodecSpecification (\ s a -> s{_hgsCodecSpecification = a})

-- | Language to be used on Caption outputs
hgsCaptionLanguageMappings :: Lens' HlsGroupSettings [HlsCaptionLanguageMapping]
hgsCaptionLanguageMappings = lens _hgsCaptionLanguageMappings (\ s a -> s{_hgsCaptionLanguageMappings = a}) . _Default . _Coerce

-- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
hgsBaseURL :: Lens' HlsGroupSettings (Maybe Text)
hgsBaseURL = lens _hgsBaseURL (\ s a -> s{_hgsBaseURL = a})

-- | Choose one or more ad marker types to pass SCTE35 signals through to this group of Apple HLS outputs.
hgsAdMarkers :: Lens' HlsGroupSettings [HlsAdMarkers]
hgsAdMarkers = lens _hgsAdMarkers (\ s a -> s{_hgsAdMarkers = a}) . _Default . _Coerce

-- | DRM settings.
hgsEncryption :: Lens' HlsGroupSettings (Maybe HlsEncryptionSettings)
hgsEncryption = lens _hgsEncryption (\ s a -> s{_hgsEncryption = a})

-- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
hgsSegmentLength :: Lens' HlsGroupSettings (Maybe Int)
hgsSegmentLength = lens _hgsSegmentLength (\ s a -> s{_hgsSegmentLength = a})

-- | Undocumented member.
hgsTimedMetadataId3Frame :: Lens' HlsGroupSettings (Maybe HlsTimedMetadataId3Frame)
hgsTimedMetadataId3Frame = lens _hgsTimedMetadataId3Frame (\ s a -> s{_hgsTimedMetadataId3Frame = a})

-- | Undocumented member.
hgsOutputSelection :: Lens' HlsGroupSettings (Maybe HlsOutputSelection)
hgsOutputSelection = lens _hgsOutputSelection (\ s a -> s{_hgsOutputSelection = a})

-- | Undocumented member.
hgsCaptionLanguageSetting :: Lens' HlsGroupSettings (Maybe HlsCaptionLanguageSetting)
hgsCaptionLanguageSetting = lens _hgsCaptionLanguageSetting (\ s a -> s{_hgsCaptionLanguageSetting = a})

-- | Number of segments to write to a subdirectory before starting a new one. directoryStructure must be SINGLE_DIRECTORY for this setting to have an effect.
hgsSegmentsPerSubdirectory :: Lens' HlsGroupSettings (Maybe Int)
hgsSegmentsPerSubdirectory = lens _hgsSegmentsPerSubdirectory (\ s a -> s{_hgsSegmentsPerSubdirectory = a})

-- | Undocumented member.
hgsManifestDurationFormat :: Lens' HlsGroupSettings (Maybe HlsManifestDurationFormat)
hgsManifestDurationFormat = lens _hgsManifestDurationFormat (\ s a -> s{_hgsManifestDurationFormat = a})

-- | Undocumented member.
hgsClientCache :: Lens' HlsGroupSettings (Maybe HlsClientCache)
hgsClientCache = lens _hgsClientCache (\ s a -> s{_hgsClientCache = a})

-- | Provides an extra millisecond delta offset to fine tune the timestamps.
hgsTimestampDeltaMilliseconds :: Lens' HlsGroupSettings (Maybe Int)
hgsTimestampDeltaMilliseconds = lens _hgsTimestampDeltaMilliseconds (\ s a -> s{_hgsTimestampDeltaMilliseconds = a})

-- | Undocumented member.
hgsStreamInfResolution :: Lens' HlsGroupSettings (Maybe HlsStreamInfResolution)
hgsStreamInfResolution = lens _hgsStreamInfResolution (\ s a -> s{_hgsStreamInfResolution = a})

-- | Undocumented member.
hgsManifestCompression :: Lens' HlsGroupSettings (Maybe HlsManifestCompression)
hgsManifestCompression = lens _hgsManifestCompression (\ s a -> s{_hgsManifestCompression = a})

instance FromJSON HlsGroupSettings where
        parseJSON
          = withObject "HlsGroupSettings"
              (\ x ->
                 HlsGroupSettings' <$>
                   (x .:? "directoryStructure") <*>
                     (x .:? "segmentControl")
                     <*> (x .:? "destination")
                     <*> (x .:? "timedMetadataId3Period")
                     <*> (x .:? "minSegmentLength")
                     <*> (x .:? "programDateTime")
                     <*> (x .:? "programDateTimePeriod")
                     <*> (x .:? "codecSpecification")
                     <*> (x .:? "captionLanguageMappings" .!= mempty)
                     <*> (x .:? "baseUrl")
                     <*> (x .:? "adMarkers" .!= mempty)
                     <*> (x .:? "encryption")
                     <*> (x .:? "segmentLength")
                     <*> (x .:? "timedMetadataId3Frame")
                     <*> (x .:? "outputSelection")
                     <*> (x .:? "captionLanguageSetting")
                     <*> (x .:? "segmentsPerSubdirectory")
                     <*> (x .:? "manifestDurationFormat")
                     <*> (x .:? "clientCache")
                     <*> (x .:? "timestampDeltaMilliseconds")
                     <*> (x .:? "streamInfResolution")
                     <*> (x .:? "manifestCompression"))

instance Hashable HlsGroupSettings where

instance NFData HlsGroupSettings where

instance ToJSON HlsGroupSettings where
        toJSON HlsGroupSettings'{..}
          = object
              (catMaybes
                 [("directoryStructure" .=) <$>
                    _hgsDirectoryStructure,
                  ("segmentControl" .=) <$> _hgsSegmentControl,
                  ("destination" .=) <$> _hgsDestination,
                  ("timedMetadataId3Period" .=) <$>
                    _hgsTimedMetadataId3Period,
                  ("minSegmentLength" .=) <$> _hgsMinSegmentLength,
                  ("programDateTime" .=) <$> _hgsProgramDateTime,
                  ("programDateTimePeriod" .=) <$>
                    _hgsProgramDateTimePeriod,
                  ("codecSpecification" .=) <$> _hgsCodecSpecification,
                  ("captionLanguageMappings" .=) <$>
                    _hgsCaptionLanguageMappings,
                  ("baseUrl" .=) <$> _hgsBaseURL,
                  ("adMarkers" .=) <$> _hgsAdMarkers,
                  ("encryption" .=) <$> _hgsEncryption,
                  ("segmentLength" .=) <$> _hgsSegmentLength,
                  ("timedMetadataId3Frame" .=) <$>
                    _hgsTimedMetadataId3Frame,
                  ("outputSelection" .=) <$> _hgsOutputSelection,
                  ("captionLanguageSetting" .=) <$>
                    _hgsCaptionLanguageSetting,
                  ("segmentsPerSubdirectory" .=) <$>
                    _hgsSegmentsPerSubdirectory,
                  ("manifestDurationFormat" .=) <$>
                    _hgsManifestDurationFormat,
                  ("clientCache" .=) <$> _hgsClientCache,
                  ("timestampDeltaMilliseconds" .=) <$>
                    _hgsTimestampDeltaMilliseconds,
                  ("streamInfResolution" .=) <$>
                    _hgsStreamInfResolution,
                  ("manifestCompression" .=) <$>
                    _hgsManifestCompression])

-- | Settings for HLS output groups
--
-- /See:/ 'hlsSettings' smart constructor.
data HlsSettings = HlsSettings'
  { _hsAudioRenditionSets :: !(Maybe Text)
  , _hsIFrameOnlyManifest :: !(Maybe HlsIFrameOnlyManifest)
  , _hsAudioGroupId       :: !(Maybe Text)
  , _hsSegmentModifier    :: !(Maybe Text)
  , _hsAudioTrackType     :: !(Maybe HlsAudioTrackType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HlsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hsAudioRenditionSets' - List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
--
-- * 'hsIFrameOnlyManifest' - Undocumented member.
--
-- * 'hsAudioGroupId' - Specifies the group to which the audio Rendition belongs.
--
-- * 'hsSegmentModifier' - String concatenated to end of segment filenames. Accepts "Format Identifiers":#format_identifier_parameters.
--
-- * 'hsAudioTrackType' - Undocumented member.
hlsSettings
    :: HlsSettings
hlsSettings =
  HlsSettings'
    { _hsAudioRenditionSets = Nothing
    , _hsIFrameOnlyManifest = Nothing
    , _hsAudioGroupId = Nothing
    , _hsSegmentModifier = Nothing
    , _hsAudioTrackType = Nothing
    }


-- | List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
hsAudioRenditionSets :: Lens' HlsSettings (Maybe Text)
hsAudioRenditionSets = lens _hsAudioRenditionSets (\ s a -> s{_hsAudioRenditionSets = a})

-- | Undocumented member.
hsIFrameOnlyManifest :: Lens' HlsSettings (Maybe HlsIFrameOnlyManifest)
hsIFrameOnlyManifest = lens _hsIFrameOnlyManifest (\ s a -> s{_hsIFrameOnlyManifest = a})

-- | Specifies the group to which the audio Rendition belongs.
hsAudioGroupId :: Lens' HlsSettings (Maybe Text)
hsAudioGroupId = lens _hsAudioGroupId (\ s a -> s{_hsAudioGroupId = a})

-- | String concatenated to end of segment filenames. Accepts "Format Identifiers":#format_identifier_parameters.
hsSegmentModifier :: Lens' HlsSettings (Maybe Text)
hsSegmentModifier = lens _hsSegmentModifier (\ s a -> s{_hsSegmentModifier = a})

-- | Undocumented member.
hsAudioTrackType :: Lens' HlsSettings (Maybe HlsAudioTrackType)
hsAudioTrackType = lens _hsAudioTrackType (\ s a -> s{_hsAudioTrackType = a})

instance FromJSON HlsSettings where
        parseJSON
          = withObject "HlsSettings"
              (\ x ->
                 HlsSettings' <$>
                   (x .:? "audioRenditionSets") <*>
                     (x .:? "iFrameOnlyManifest")
                     <*> (x .:? "audioGroupId")
                     <*> (x .:? "segmentModifier")
                     <*> (x .:? "audioTrackType"))

instance Hashable HlsSettings where

instance NFData HlsSettings where

instance ToJSON HlsSettings where
        toJSON HlsSettings'{..}
          = object
              (catMaybes
                 [("audioRenditionSets" .=) <$> _hsAudioRenditionSets,
                  ("iFrameOnlyManifest" .=) <$> _hsIFrameOnlyManifest,
                  ("audioGroupId" .=) <$> _hsAudioGroupId,
                  ("segmentModifier" .=) <$> _hsSegmentModifier,
                  ("audioTrackType" .=) <$> _hsAudioTrackType])

-- | To insert ID3 tags in your output, specify two values. Use ID3 tag (Id3) to specify the base 64 encoded string and use Timecode (TimeCode) to specify the time when the tag should be inserted. To insert multiple ID3 tags in your output, create mulitple instances of ID3 insertion (Id3Insertion).
--
-- /See:/ 'id3Insertion' smart constructor.
data Id3Insertion = Id3Insertion'
  { _iiId3      :: !(Maybe Text)
  , _iiTimecode :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Id3Insertion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiId3' - Use ID3 tag (Id3) to provide a tag value in base64-encode format.
--
-- * 'iiTimecode' - Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
id3Insertion
    :: Id3Insertion
id3Insertion = Id3Insertion' {_iiId3 = Nothing, _iiTimecode = Nothing}


-- | Use ID3 tag (Id3) to provide a tag value in base64-encode format.
iiId3 :: Lens' Id3Insertion (Maybe Text)
iiId3 = lens _iiId3 (\ s a -> s{_iiId3 = a})

-- | Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
iiTimecode :: Lens' Id3Insertion (Maybe Text)
iiTimecode = lens _iiTimecode (\ s a -> s{_iiTimecode = a})

instance FromJSON Id3Insertion where
        parseJSON
          = withObject "Id3Insertion"
              (\ x ->
                 Id3Insertion' <$>
                   (x .:? "id3") <*> (x .:? "timecode"))

instance Hashable Id3Insertion where

instance NFData Id3Insertion where

instance ToJSON Id3Insertion where
        toJSON Id3Insertion'{..}
          = object
              (catMaybes
                 [("id3" .=) <$> _iiId3,
                  ("timecode" .=) <$> _iiTimecode])

-- | Enable the Image inserter (ImageInserter) feature to include a graphic overlay on your video. Enable or disable this feature for each output individually. This setting is disabled by default.
--
-- /See:/ 'imageInserter' smart constructor.
newtype ImageInserter = ImageInserter'
  { _iiInsertableImages :: Maybe [InsertableImage]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImageInserter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiInsertableImages' - Image to insert. Must be 32 bit windows BMP, PNG, or TGA file. Must not be  larger than the output frames.
imageInserter
    :: ImageInserter
imageInserter = ImageInserter' {_iiInsertableImages = Nothing}


-- | Image to insert. Must be 32 bit windows BMP, PNG, or TGA file. Must not be  larger than the output frames.
iiInsertableImages :: Lens' ImageInserter [InsertableImage]
iiInsertableImages = lens _iiInsertableImages (\ s a -> s{_iiInsertableImages = a}) . _Default . _Coerce

instance FromJSON ImageInserter where
        parseJSON
          = withObject "ImageInserter"
              (\ x ->
                 ImageInserter' <$>
                   (x .:? "insertableImages" .!= mempty))

instance Hashable ImageInserter where

instance NFData ImageInserter where

instance ToJSON ImageInserter where
        toJSON ImageInserter'{..}
          = object
              (catMaybes
                 [("insertableImages" .=) <$> _iiInsertableImages])

-- | Specifies media input
--
-- /See:/ 'input' smart constructor.
data Input = Input'
  { _iVideoSelector       :: !(Maybe VideoSelector)
  , _iProgramNumber       :: !(Maybe Int)
  , _iAudioSelectorGroups :: !(Maybe (Map Text AudioSelectorGroup))
  , _iTimecodeSource      :: !(Maybe InputTimecodeSource)
  , _iAudioSelectors      :: !(Maybe (Map Text AudioSelector))
  , _iDeblockFilter       :: !(Maybe InputDeblockFilter)
  , _iInputClippings      :: !(Maybe [InputClipping])
  , _iDenoiseFilter       :: !(Maybe InputDenoiseFilter)
  , _iFilterStrength      :: !(Maybe Int)
  , _iPsiControl          :: !(Maybe InputPsiControl)
  , _iCaptionSelectors    :: !(Maybe (Map Text CaptionSelector))
  , _iFileInput           :: !(Maybe Text)
  , _iFilterEnable        :: !(Maybe InputFilterEnable)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Input' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iVideoSelector' - Undocumented member.
--
-- * 'iProgramNumber' - Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
--
-- * 'iAudioSelectorGroups' - Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
--
-- * 'iTimecodeSource' - Undocumented member.
--
-- * 'iAudioSelectors' - Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use mutiple Audio selectors per input.
--
-- * 'iDeblockFilter' - Undocumented member.
--
-- * 'iInputClippings' - (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
--
-- * 'iDenoiseFilter' - Undocumented member.
--
-- * 'iFilterStrength' - Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
--
-- * 'iPsiControl' - Undocumented member.
--
-- * 'iCaptionSelectors' - Use Captions selectors (CaptionSelectors) to specify the captions data from the input that you will use in your outputs. You can use mutiple captions selectors per input.
--
-- * 'iFileInput' - Use Input (fileInput) to define the source file used in the transcode job. There can be multiple inputs in a job. These inputs are concantenated, in the order they are specified in the job, to create the output.
--
-- * 'iFilterEnable' - Undocumented member.
input
    :: Input
input =
  Input'
    { _iVideoSelector = Nothing
    , _iProgramNumber = Nothing
    , _iAudioSelectorGroups = Nothing
    , _iTimecodeSource = Nothing
    , _iAudioSelectors = Nothing
    , _iDeblockFilter = Nothing
    , _iInputClippings = Nothing
    , _iDenoiseFilter = Nothing
    , _iFilterStrength = Nothing
    , _iPsiControl = Nothing
    , _iCaptionSelectors = Nothing
    , _iFileInput = Nothing
    , _iFilterEnable = Nothing
    }


-- | Undocumented member.
iVideoSelector :: Lens' Input (Maybe VideoSelector)
iVideoSelector = lens _iVideoSelector (\ s a -> s{_iVideoSelector = a})

-- | Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
iProgramNumber :: Lens' Input (Maybe Int)
iProgramNumber = lens _iProgramNumber (\ s a -> s{_iProgramNumber = a})

-- | Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
iAudioSelectorGroups :: Lens' Input (HashMap Text AudioSelectorGroup)
iAudioSelectorGroups = lens _iAudioSelectorGroups (\ s a -> s{_iAudioSelectorGroups = a}) . _Default . _Map

-- | Undocumented member.
iTimecodeSource :: Lens' Input (Maybe InputTimecodeSource)
iTimecodeSource = lens _iTimecodeSource (\ s a -> s{_iTimecodeSource = a})

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use mutiple Audio selectors per input.
iAudioSelectors :: Lens' Input (HashMap Text AudioSelector)
iAudioSelectors = lens _iAudioSelectors (\ s a -> s{_iAudioSelectors = a}) . _Default . _Map

-- | Undocumented member.
iDeblockFilter :: Lens' Input (Maybe InputDeblockFilter)
iDeblockFilter = lens _iDeblockFilter (\ s a -> s{_iDeblockFilter = a})

-- | (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
iInputClippings :: Lens' Input [InputClipping]
iInputClippings = lens _iInputClippings (\ s a -> s{_iInputClippings = a}) . _Default . _Coerce

-- | Undocumented member.
iDenoiseFilter :: Lens' Input (Maybe InputDenoiseFilter)
iDenoiseFilter = lens _iDenoiseFilter (\ s a -> s{_iDenoiseFilter = a})

-- | Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
iFilterStrength :: Lens' Input (Maybe Int)
iFilterStrength = lens _iFilterStrength (\ s a -> s{_iFilterStrength = a})

-- | Undocumented member.
iPsiControl :: Lens' Input (Maybe InputPsiControl)
iPsiControl = lens _iPsiControl (\ s a -> s{_iPsiControl = a})

-- | Use Captions selectors (CaptionSelectors) to specify the captions data from the input that you will use in your outputs. You can use mutiple captions selectors per input.
iCaptionSelectors :: Lens' Input (HashMap Text CaptionSelector)
iCaptionSelectors = lens _iCaptionSelectors (\ s a -> s{_iCaptionSelectors = a}) . _Default . _Map

-- | Use Input (fileInput) to define the source file used in the transcode job. There can be multiple inputs in a job. These inputs are concantenated, in the order they are specified in the job, to create the output.
iFileInput :: Lens' Input (Maybe Text)
iFileInput = lens _iFileInput (\ s a -> s{_iFileInput = a})

-- | Undocumented member.
iFilterEnable :: Lens' Input (Maybe InputFilterEnable)
iFilterEnable = lens _iFilterEnable (\ s a -> s{_iFilterEnable = a})

instance FromJSON Input where
        parseJSON
          = withObject "Input"
              (\ x ->
                 Input' <$>
                   (x .:? "videoSelector") <*> (x .:? "programNumber")
                     <*> (x .:? "audioSelectorGroups" .!= mempty)
                     <*> (x .:? "timecodeSource")
                     <*> (x .:? "audioSelectors" .!= mempty)
                     <*> (x .:? "deblockFilter")
                     <*> (x .:? "inputClippings" .!= mempty)
                     <*> (x .:? "denoiseFilter")
                     <*> (x .:? "filterStrength")
                     <*> (x .:? "psiControl")
                     <*> (x .:? "captionSelectors" .!= mempty)
                     <*> (x .:? "fileInput")
                     <*> (x .:? "filterEnable"))

instance Hashable Input where

instance NFData Input where

instance ToJSON Input where
        toJSON Input'{..}
          = object
              (catMaybes
                 [("videoSelector" .=) <$> _iVideoSelector,
                  ("programNumber" .=) <$> _iProgramNumber,
                  ("audioSelectorGroups" .=) <$> _iAudioSelectorGroups,
                  ("timecodeSource" .=) <$> _iTimecodeSource,
                  ("audioSelectors" .=) <$> _iAudioSelectors,
                  ("deblockFilter" .=) <$> _iDeblockFilter,
                  ("inputClippings" .=) <$> _iInputClippings,
                  ("denoiseFilter" .=) <$> _iDenoiseFilter,
                  ("filterStrength" .=) <$> _iFilterStrength,
                  ("psiControl" .=) <$> _iPsiControl,
                  ("captionSelectors" .=) <$> _iCaptionSelectors,
                  ("fileInput" .=) <$> _iFileInput,
                  ("filterEnable" .=) <$> _iFilterEnable])

-- | Include one instance of (InputClipping) for each input clip.
--
-- /See:/ 'inputClipping' smart constructor.
data InputClipping = InputClipping'
  { _icEndTimecode   :: !(Maybe Text)
  , _icStartTimecode :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputClipping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icEndTimecode' - Set End timecode (EndTimecode) to the end of the portion of the input you are clipping. The frame corresponding to the End timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. When choosing this value, take into account your setting for Input timecode source. For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to begin five minutes into the video, use 01:00:05:00.
--
-- * 'icStartTimecode' - Set Start timecode (StartTimecode) to the beginning of the portion of the input you are clipping. The frame corresponding to the Start timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. When choosing this value, take into account your setting for Input timecode source. For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to begin five minutes into the video, use 01:00:05:00.
inputClipping
    :: InputClipping
inputClipping =
  InputClipping' {_icEndTimecode = Nothing, _icStartTimecode = Nothing}


-- | Set End timecode (EndTimecode) to the end of the portion of the input you are clipping. The frame corresponding to the End timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. When choosing this value, take into account your setting for Input timecode source. For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to begin five minutes into the video, use 01:00:05:00.
icEndTimecode :: Lens' InputClipping (Maybe Text)
icEndTimecode = lens _icEndTimecode (\ s a -> s{_icEndTimecode = a})

-- | Set Start timecode (StartTimecode) to the beginning of the portion of the input you are clipping. The frame corresponding to the Start timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. When choosing this value, take into account your setting for Input timecode source. For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to begin five minutes into the video, use 01:00:05:00.
icStartTimecode :: Lens' InputClipping (Maybe Text)
icStartTimecode = lens _icStartTimecode (\ s a -> s{_icStartTimecode = a})

instance FromJSON InputClipping where
        parseJSON
          = withObject "InputClipping"
              (\ x ->
                 InputClipping' <$>
                   (x .:? "endTimecode") <*> (x .:? "startTimecode"))

instance Hashable InputClipping where

instance NFData InputClipping where

instance ToJSON InputClipping where
        toJSON InputClipping'{..}
          = object
              (catMaybes
                 [("endTimecode" .=) <$> _icEndTimecode,
                  ("startTimecode" .=) <$> _icStartTimecode])

-- | Specified video input in a template.
--
-- /See:/ 'inputTemplate' smart constructor.
data InputTemplate = InputTemplate'
  { _itVideoSelector       :: !(Maybe VideoSelector)
  , _itProgramNumber       :: !(Maybe Int)
  , _itAudioSelectorGroups :: !(Maybe (Map Text AudioSelectorGroup))
  , _itTimecodeSource      :: !(Maybe InputTimecodeSource)
  , _itAudioSelectors      :: !(Maybe (Map Text AudioSelector))
  , _itDeblockFilter       :: !(Maybe InputDeblockFilter)
  , _itInputClippings      :: !(Maybe [InputClipping])
  , _itDenoiseFilter       :: !(Maybe InputDenoiseFilter)
  , _itFilterStrength      :: !(Maybe Int)
  , _itPsiControl          :: !(Maybe InputPsiControl)
  , _itCaptionSelectors    :: !(Maybe (Map Text CaptionSelector))
  , _itFilterEnable        :: !(Maybe InputFilterEnable)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itVideoSelector' - Undocumented member.
--
-- * 'itProgramNumber' - Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
--
-- * 'itAudioSelectorGroups' - Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
--
-- * 'itTimecodeSource' - Undocumented member.
--
-- * 'itAudioSelectors' - Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use mutiple Audio selectors per input.
--
-- * 'itDeblockFilter' - Undocumented member.
--
-- * 'itInputClippings' - (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
--
-- * 'itDenoiseFilter' - Undocumented member.
--
-- * 'itFilterStrength' - Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
--
-- * 'itPsiControl' - Undocumented member.
--
-- * 'itCaptionSelectors' - Use Captions selectors (CaptionSelectors) to specify the captions data from the input that you will use in your outputs. You can use mutiple captions selectors per input.
--
-- * 'itFilterEnable' - Undocumented member.
inputTemplate
    :: InputTemplate
inputTemplate =
  InputTemplate'
    { _itVideoSelector = Nothing
    , _itProgramNumber = Nothing
    , _itAudioSelectorGroups = Nothing
    , _itTimecodeSource = Nothing
    , _itAudioSelectors = Nothing
    , _itDeblockFilter = Nothing
    , _itInputClippings = Nothing
    , _itDenoiseFilter = Nothing
    , _itFilterStrength = Nothing
    , _itPsiControl = Nothing
    , _itCaptionSelectors = Nothing
    , _itFilterEnable = Nothing
    }


-- | Undocumented member.
itVideoSelector :: Lens' InputTemplate (Maybe VideoSelector)
itVideoSelector = lens _itVideoSelector (\ s a -> s{_itVideoSelector = a})

-- | Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
itProgramNumber :: Lens' InputTemplate (Maybe Int)
itProgramNumber = lens _itProgramNumber (\ s a -> s{_itProgramNumber = a})

-- | Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
itAudioSelectorGroups :: Lens' InputTemplate (HashMap Text AudioSelectorGroup)
itAudioSelectorGroups = lens _itAudioSelectorGroups (\ s a -> s{_itAudioSelectorGroups = a}) . _Default . _Map

-- | Undocumented member.
itTimecodeSource :: Lens' InputTemplate (Maybe InputTimecodeSource)
itTimecodeSource = lens _itTimecodeSource (\ s a -> s{_itTimecodeSource = a})

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use mutiple Audio selectors per input.
itAudioSelectors :: Lens' InputTemplate (HashMap Text AudioSelector)
itAudioSelectors = lens _itAudioSelectors (\ s a -> s{_itAudioSelectors = a}) . _Default . _Map

-- | Undocumented member.
itDeblockFilter :: Lens' InputTemplate (Maybe InputDeblockFilter)
itDeblockFilter = lens _itDeblockFilter (\ s a -> s{_itDeblockFilter = a})

-- | (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
itInputClippings :: Lens' InputTemplate [InputClipping]
itInputClippings = lens _itInputClippings (\ s a -> s{_itInputClippings = a}) . _Default . _Coerce

-- | Undocumented member.
itDenoiseFilter :: Lens' InputTemplate (Maybe InputDenoiseFilter)
itDenoiseFilter = lens _itDenoiseFilter (\ s a -> s{_itDenoiseFilter = a})

-- | Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
itFilterStrength :: Lens' InputTemplate (Maybe Int)
itFilterStrength = lens _itFilterStrength (\ s a -> s{_itFilterStrength = a})

-- | Undocumented member.
itPsiControl :: Lens' InputTemplate (Maybe InputPsiControl)
itPsiControl = lens _itPsiControl (\ s a -> s{_itPsiControl = a})

-- | Use Captions selectors (CaptionSelectors) to specify the captions data from the input that you will use in your outputs. You can use mutiple captions selectors per input.
itCaptionSelectors :: Lens' InputTemplate (HashMap Text CaptionSelector)
itCaptionSelectors = lens _itCaptionSelectors (\ s a -> s{_itCaptionSelectors = a}) . _Default . _Map

-- | Undocumented member.
itFilterEnable :: Lens' InputTemplate (Maybe InputFilterEnable)
itFilterEnable = lens _itFilterEnable (\ s a -> s{_itFilterEnable = a})

instance FromJSON InputTemplate where
        parseJSON
          = withObject "InputTemplate"
              (\ x ->
                 InputTemplate' <$>
                   (x .:? "videoSelector") <*> (x .:? "programNumber")
                     <*> (x .:? "audioSelectorGroups" .!= mempty)
                     <*> (x .:? "timecodeSource")
                     <*> (x .:? "audioSelectors" .!= mempty)
                     <*> (x .:? "deblockFilter")
                     <*> (x .:? "inputClippings" .!= mempty)
                     <*> (x .:? "denoiseFilter")
                     <*> (x .:? "filterStrength")
                     <*> (x .:? "psiControl")
                     <*> (x .:? "captionSelectors" .!= mempty)
                     <*> (x .:? "filterEnable"))

instance Hashable InputTemplate where

instance NFData InputTemplate where

instance ToJSON InputTemplate where
        toJSON InputTemplate'{..}
          = object
              (catMaybes
                 [("videoSelector" .=) <$> _itVideoSelector,
                  ("programNumber" .=) <$> _itProgramNumber,
                  ("audioSelectorGroups" .=) <$>
                    _itAudioSelectorGroups,
                  ("timecodeSource" .=) <$> _itTimecodeSource,
                  ("audioSelectors" .=) <$> _itAudioSelectors,
                  ("deblockFilter" .=) <$> _itDeblockFilter,
                  ("inputClippings" .=) <$> _itInputClippings,
                  ("denoiseFilter" .=) <$> _itDenoiseFilter,
                  ("filterStrength" .=) <$> _itFilterStrength,
                  ("psiControl" .=) <$> _itPsiControl,
                  ("captionSelectors" .=) <$> _itCaptionSelectors,
                  ("filterEnable" .=) <$> _itFilterEnable])

-- | Settings for Insertable Image
--
-- /See:/ 'insertableImage' smart constructor.
data InsertableImage = InsertableImage'
  { _iiImageX             :: !(Maybe Int)
  , _iiHeight             :: !(Maybe Int)
  , _iiStartTime          :: !(Maybe Text)
  , _iiFadeOut            :: !(Maybe Int)
  , _iiWidth              :: !(Maybe Int)
  , _iiOpacity            :: !(Maybe Int)
  , _iiLayer              :: !(Maybe Int)
  , _iiDuration           :: !(Maybe Int)
  , _iiImageY             :: !(Maybe Int)
  , _iiImageInserterInput :: !(Maybe Text)
  , _iiFadeIn             :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InsertableImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiImageX' - Use Left (ImageX) to set the distance, in pixels, between the inserted image and the left edge of the frame. Required for BMP, PNG and TGA input.
--
-- * 'iiHeight' - Specify the Height (Height) of the inserted image. Use a value that is less than or equal to the video resolution height. Leave this setting blank to use the native height of the image.
--
-- * 'iiStartTime' - Use Start time (StartTime) to specify the video timecode when the image is inserted in the output. This must be in timecode format (HH:MM:SS:FF)
--
-- * 'iiFadeOut' - Use Fade out (FadeOut) to set the length, in milliseconds, of the inserted image fade out. If you don't specify a value for Fade out, the image will disappear abruptly at the end of the inserted image duration.
--
-- * 'iiWidth' - Specify the Width (Width) of the inserted image. Use a value that is less than or equal to the video resolution width. Leave this setting blank to use the native width of the image.
--
-- * 'iiOpacity' - Use Opacity (Opacity) to specify how much of the underlying video shows through the inserted image. 0 is transparent and 100 is fully opaque. Default is 50.
--
-- * 'iiLayer' - Use Layer (Layer) to specify how overlapping inserted images appear. Images with higher values of layer appear on top of images with lower values of layer.
--
-- * 'iiDuration' - Use Duration (Duration) to set the time, in milliseconds, for the image to remain on the output video.
--
-- * 'iiImageY' - Use Top (ImageY) to set the distance, in pixels, between the inserted image and the top edge of the video frame. Required for BMP, PNG and TGA input.
--
-- * 'iiImageInserterInput' - Use Image location (imageInserterInput) to specify the Amazon S3 location of the image to be inserted into the output. Use a 32 bit BMP, PNG, or TGA file that fits inside the video frame.
--
-- * 'iiFadeIn' - Use Fade in (FadeIut) to set the length, in milliseconds, of the inserted image fade in. If you don't specify a value for Fade in, the image will appear abruptly at the Start time.
insertableImage
    :: InsertableImage
insertableImage =
  InsertableImage'
    { _iiImageX = Nothing
    , _iiHeight = Nothing
    , _iiStartTime = Nothing
    , _iiFadeOut = Nothing
    , _iiWidth = Nothing
    , _iiOpacity = Nothing
    , _iiLayer = Nothing
    , _iiDuration = Nothing
    , _iiImageY = Nothing
    , _iiImageInserterInput = Nothing
    , _iiFadeIn = Nothing
    }


-- | Use Left (ImageX) to set the distance, in pixels, between the inserted image and the left edge of the frame. Required for BMP, PNG and TGA input.
iiImageX :: Lens' InsertableImage (Maybe Int)
iiImageX = lens _iiImageX (\ s a -> s{_iiImageX = a})

-- | Specify the Height (Height) of the inserted image. Use a value that is less than or equal to the video resolution height. Leave this setting blank to use the native height of the image.
iiHeight :: Lens' InsertableImage (Maybe Int)
iiHeight = lens _iiHeight (\ s a -> s{_iiHeight = a})

-- | Use Start time (StartTime) to specify the video timecode when the image is inserted in the output. This must be in timecode format (HH:MM:SS:FF)
iiStartTime :: Lens' InsertableImage (Maybe Text)
iiStartTime = lens _iiStartTime (\ s a -> s{_iiStartTime = a})

-- | Use Fade out (FadeOut) to set the length, in milliseconds, of the inserted image fade out. If you don't specify a value for Fade out, the image will disappear abruptly at the end of the inserted image duration.
iiFadeOut :: Lens' InsertableImage (Maybe Int)
iiFadeOut = lens _iiFadeOut (\ s a -> s{_iiFadeOut = a})

-- | Specify the Width (Width) of the inserted image. Use a value that is less than or equal to the video resolution width. Leave this setting blank to use the native width of the image.
iiWidth :: Lens' InsertableImage (Maybe Int)
iiWidth = lens _iiWidth (\ s a -> s{_iiWidth = a})

-- | Use Opacity (Opacity) to specify how much of the underlying video shows through the inserted image. 0 is transparent and 100 is fully opaque. Default is 50.
iiOpacity :: Lens' InsertableImage (Maybe Int)
iiOpacity = lens _iiOpacity (\ s a -> s{_iiOpacity = a})

-- | Use Layer (Layer) to specify how overlapping inserted images appear. Images with higher values of layer appear on top of images with lower values of layer.
iiLayer :: Lens' InsertableImage (Maybe Int)
iiLayer = lens _iiLayer (\ s a -> s{_iiLayer = a})

-- | Use Duration (Duration) to set the time, in milliseconds, for the image to remain on the output video.
iiDuration :: Lens' InsertableImage (Maybe Int)
iiDuration = lens _iiDuration (\ s a -> s{_iiDuration = a})

-- | Use Top (ImageY) to set the distance, in pixels, between the inserted image and the top edge of the video frame. Required for BMP, PNG and TGA input.
iiImageY :: Lens' InsertableImage (Maybe Int)
iiImageY = lens _iiImageY (\ s a -> s{_iiImageY = a})

-- | Use Image location (imageInserterInput) to specify the Amazon S3 location of the image to be inserted into the output. Use a 32 bit BMP, PNG, or TGA file that fits inside the video frame.
iiImageInserterInput :: Lens' InsertableImage (Maybe Text)
iiImageInserterInput = lens _iiImageInserterInput (\ s a -> s{_iiImageInserterInput = a})

-- | Use Fade in (FadeIut) to set the length, in milliseconds, of the inserted image fade in. If you don't specify a value for Fade in, the image will appear abruptly at the Start time.
iiFadeIn :: Lens' InsertableImage (Maybe Int)
iiFadeIn = lens _iiFadeIn (\ s a -> s{_iiFadeIn = a})

instance FromJSON InsertableImage where
        parseJSON
          = withObject "InsertableImage"
              (\ x ->
                 InsertableImage' <$>
                   (x .:? "imageX") <*> (x .:? "height") <*>
                     (x .:? "startTime")
                     <*> (x .:? "fadeOut")
                     <*> (x .:? "width")
                     <*> (x .:? "opacity")
                     <*> (x .:? "layer")
                     <*> (x .:? "duration")
                     <*> (x .:? "imageY")
                     <*> (x .:? "imageInserterInput")
                     <*> (x .:? "fadeIn"))

instance Hashable InsertableImage where

instance NFData InsertableImage where

instance ToJSON InsertableImage where
        toJSON InsertableImage'{..}
          = object
              (catMaybes
                 [("imageX" .=) <$> _iiImageX,
                  ("height" .=) <$> _iiHeight,
                  ("startTime" .=) <$> _iiStartTime,
                  ("fadeOut" .=) <$> _iiFadeOut,
                  ("width" .=) <$> _iiWidth,
                  ("opacity" .=) <$> _iiOpacity,
                  ("layer" .=) <$> _iiLayer,
                  ("duration" .=) <$> _iiDuration,
                  ("imageY" .=) <$> _iiImageY,
                  ("imageInserterInput" .=) <$> _iiImageInserterInput,
                  ("fadeIn" .=) <$> _iiFadeIn])

-- | Each job converts an input file into an output file or files. For more information, see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
--
-- /See:/ 'job' smart constructor.
data Job = Job'
  { _jStatus             :: !(Maybe JobStatus)
  , _jJobTemplate        :: !(Maybe Text)
  , _jSettings           :: !(Maybe JobSettings)
  , _jARN                :: !(Maybe Text)
  , _jCreatedAt          :: !(Maybe POSIX)
  , _jQueue              :: !(Maybe Text)
  , _jUserMetadata       :: !(Maybe (Map Text Text))
  , _jRole               :: !(Maybe Text)
  , _jOutputGroupDetails :: !(Maybe [OutputGroupDetail])
  , _jErrorCode          :: !(Maybe Int)
  , _jId                 :: !(Maybe Text)
  , _jTiming             :: !(Maybe Timing)
  , _jErrorMessage       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jStatus' - Undocumented member.
--
-- * 'jJobTemplate' - The job template that the job is created from, if it is created from a job template.
--
-- * 'jSettings' - Undocumented member.
--
-- * 'jARN' - An identifier for this resource that is unique within all of AWS.
--
-- * 'jCreatedAt' - The time, in Unix epoch format in seconds, when the job got created.
--
-- * 'jQueue' - Optional. When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
--
-- * 'jUserMetadata' - User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.
--
-- * 'jRole' - The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html
--
-- * 'jOutputGroupDetails' - List of output group details
--
-- * 'jErrorCode' - Error code for the job
--
-- * 'jId' - A portion of the job's ARN, unique within your AWS Elemental MediaConvert resources
--
-- * 'jTiming' - Undocumented member.
--
-- * 'jErrorMessage' - Error message of Job
job
    :: Job
job =
  Job'
    { _jStatus = Nothing
    , _jJobTemplate = Nothing
    , _jSettings = Nothing
    , _jARN = Nothing
    , _jCreatedAt = Nothing
    , _jQueue = Nothing
    , _jUserMetadata = Nothing
    , _jRole = Nothing
    , _jOutputGroupDetails = Nothing
    , _jErrorCode = Nothing
    , _jId = Nothing
    , _jTiming = Nothing
    , _jErrorMessage = Nothing
    }


-- | Undocumented member.
jStatus :: Lens' Job (Maybe JobStatus)
jStatus = lens _jStatus (\ s a -> s{_jStatus = a})

-- | The job template that the job is created from, if it is created from a job template.
jJobTemplate :: Lens' Job (Maybe Text)
jJobTemplate = lens _jJobTemplate (\ s a -> s{_jJobTemplate = a})

-- | Undocumented member.
jSettings :: Lens' Job (Maybe JobSettings)
jSettings = lens _jSettings (\ s a -> s{_jSettings = a})

-- | An identifier for this resource that is unique within all of AWS.
jARN :: Lens' Job (Maybe Text)
jARN = lens _jARN (\ s a -> s{_jARN = a})

-- | The time, in Unix epoch format in seconds, when the job got created.
jCreatedAt :: Lens' Job (Maybe UTCTime)
jCreatedAt = lens _jCreatedAt (\ s a -> s{_jCreatedAt = a}) . mapping _Time

-- | Optional. When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
jQueue :: Lens' Job (Maybe Text)
jQueue = lens _jQueue (\ s a -> s{_jQueue = a})

-- | User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.
jUserMetadata :: Lens' Job (HashMap Text Text)
jUserMetadata = lens _jUserMetadata (\ s a -> s{_jUserMetadata = a}) . _Default . _Map

-- | The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html
jRole :: Lens' Job (Maybe Text)
jRole = lens _jRole (\ s a -> s{_jRole = a})

-- | List of output group details
jOutputGroupDetails :: Lens' Job [OutputGroupDetail]
jOutputGroupDetails = lens _jOutputGroupDetails (\ s a -> s{_jOutputGroupDetails = a}) . _Default . _Coerce

-- | Error code for the job
jErrorCode :: Lens' Job (Maybe Int)
jErrorCode = lens _jErrorCode (\ s a -> s{_jErrorCode = a})

-- | A portion of the job's ARN, unique within your AWS Elemental MediaConvert resources
jId :: Lens' Job (Maybe Text)
jId = lens _jId (\ s a -> s{_jId = a})

-- | Undocumented member.
jTiming :: Lens' Job (Maybe Timing)
jTiming = lens _jTiming (\ s a -> s{_jTiming = a})

-- | Error message of Job
jErrorMessage :: Lens' Job (Maybe Text)
jErrorMessage = lens _jErrorMessage (\ s a -> s{_jErrorMessage = a})

instance FromJSON Job where
        parseJSON
          = withObject "Job"
              (\ x ->
                 Job' <$>
                   (x .:? "status") <*> (x .:? "jobTemplate") <*>
                     (x .:? "settings")
                     <*> (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "queue")
                     <*> (x .:? "userMetadata" .!= mempty)
                     <*> (x .:? "role")
                     <*> (x .:? "outputGroupDetails" .!= mempty)
                     <*> (x .:? "errorCode")
                     <*> (x .:? "id")
                     <*> (x .:? "timing")
                     <*> (x .:? "errorMessage"))

instance Hashable Job where

instance NFData Job where

-- | JobSettings contains all the transcode settings for a job.
--
-- /See:/ 'jobSettings' smart constructor.
data JobSettings = JobSettings'
  { _jsInputs                 :: !(Maybe [Input])
  , _jsTimedMetadataInsertion :: !(Maybe TimedMetadataInsertion)
  , _jsNielsenConfiguration   :: !(Maybe NielsenConfiguration)
  , _jsAvailBlanking          :: !(Maybe AvailBlanking)
  , _jsTimecodeConfig         :: !(Maybe TimecodeConfig)
  , _jsOutputGroups           :: !(Maybe [OutputGroup])
  , _jsAdAvailOffset          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jsInputs' - Use Inputs (inputs) to define source file used in the transcode job. There can be multiple inputs add in a job. These inputs will be concantenated together to create the output.
--
-- * 'jsTimedMetadataInsertion' - Undocumented member.
--
-- * 'jsNielsenConfiguration' - Undocumented member.
--
-- * 'jsAvailBlanking' - Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
--
-- * 'jsTimecodeConfig' - Contains settings used to acquire and adjust timecode information from inputs.
--
-- * 'jsOutputGroups' - **!!**(OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings
--
-- * 'jsAdAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
jobSettings
    :: JobSettings
jobSettings =
  JobSettings'
    { _jsInputs = Nothing
    , _jsTimedMetadataInsertion = Nothing
    , _jsNielsenConfiguration = Nothing
    , _jsAvailBlanking = Nothing
    , _jsTimecodeConfig = Nothing
    , _jsOutputGroups = Nothing
    , _jsAdAvailOffset = Nothing
    }


-- | Use Inputs (inputs) to define source file used in the transcode job. There can be multiple inputs add in a job. These inputs will be concantenated together to create the output.
jsInputs :: Lens' JobSettings [Input]
jsInputs = lens _jsInputs (\ s a -> s{_jsInputs = a}) . _Default . _Coerce

-- | Undocumented member.
jsTimedMetadataInsertion :: Lens' JobSettings (Maybe TimedMetadataInsertion)
jsTimedMetadataInsertion = lens _jsTimedMetadataInsertion (\ s a -> s{_jsTimedMetadataInsertion = a})

-- | Undocumented member.
jsNielsenConfiguration :: Lens' JobSettings (Maybe NielsenConfiguration)
jsNielsenConfiguration = lens _jsNielsenConfiguration (\ s a -> s{_jsNielsenConfiguration = a})

-- | Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
jsAvailBlanking :: Lens' JobSettings (Maybe AvailBlanking)
jsAvailBlanking = lens _jsAvailBlanking (\ s a -> s{_jsAvailBlanking = a})

-- | Contains settings used to acquire and adjust timecode information from inputs.
jsTimecodeConfig :: Lens' JobSettings (Maybe TimecodeConfig)
jsTimecodeConfig = lens _jsTimecodeConfig (\ s a -> s{_jsTimecodeConfig = a})

-- | **!!**(OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings
jsOutputGroups :: Lens' JobSettings [OutputGroup]
jsOutputGroups = lens _jsOutputGroups (\ s a -> s{_jsOutputGroups = a}) . _Default . _Coerce

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
jsAdAvailOffset :: Lens' JobSettings (Maybe Int)
jsAdAvailOffset = lens _jsAdAvailOffset (\ s a -> s{_jsAdAvailOffset = a})

instance FromJSON JobSettings where
        parseJSON
          = withObject "JobSettings"
              (\ x ->
                 JobSettings' <$>
                   (x .:? "inputs" .!= mempty) <*>
                     (x .:? "timedMetadataInsertion")
                     <*> (x .:? "nielsenConfiguration")
                     <*> (x .:? "availBlanking")
                     <*> (x .:? "timecodeConfig")
                     <*> (x .:? "outputGroups" .!= mempty)
                     <*> (x .:? "adAvailOffset"))

instance Hashable JobSettings where

instance NFData JobSettings where

instance ToJSON JobSettings where
        toJSON JobSettings'{..}
          = object
              (catMaybes
                 [("inputs" .=) <$> _jsInputs,
                  ("timedMetadataInsertion" .=) <$>
                    _jsTimedMetadataInsertion,
                  ("nielsenConfiguration" .=) <$>
                    _jsNielsenConfiguration,
                  ("availBlanking" .=) <$> _jsAvailBlanking,
                  ("timecodeConfig" .=) <$> _jsTimecodeConfig,
                  ("outputGroups" .=) <$> _jsOutputGroups,
                  ("adAvailOffset" .=) <$> _jsAdAvailOffset])

-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
--
-- /See:/ 'jobTemplate' smart constructor.
data JobTemplate = JobTemplate'
  { _jtLastUpdated :: !(Maybe POSIX)
  , _jtSettings    :: !(Maybe JobTemplateSettings)
  , _jtARN         :: !(Maybe Text)
  , _jtCreatedAt   :: !(Maybe POSIX)
  , _jtCategory    :: !(Maybe Text)
  , _jtQueue       :: !(Maybe Text)
  , _jtName        :: !(Maybe Text)
  , _jtType        :: !(Maybe Type)
  , _jtDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jtLastUpdated' - The timestamp in epoch seconds when the Job template was last updated.
--
-- * 'jtSettings' - Undocumented member.
--
-- * 'jtARN' - An identifier for this resource that is unique within all of AWS.
--
-- * 'jtCreatedAt' - The timestamp in epoch seconds for Job template creation.
--
-- * 'jtCategory' - An optional category you create to organize your job templates.
--
-- * 'jtQueue' - Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
--
-- * 'jtName' - A name you create for each job template. Each name must be unique within your account.
--
-- * 'jtType' - A job template can be of two types: system or custom. System or built-in job templates can't be modified or deleted by the user.
--
-- * 'jtDescription' - An optional description you create for each job template.
jobTemplate
    :: JobTemplate
jobTemplate =
  JobTemplate'
    { _jtLastUpdated = Nothing
    , _jtSettings = Nothing
    , _jtARN = Nothing
    , _jtCreatedAt = Nothing
    , _jtCategory = Nothing
    , _jtQueue = Nothing
    , _jtName = Nothing
    , _jtType = Nothing
    , _jtDescription = Nothing
    }


-- | The timestamp in epoch seconds when the Job template was last updated.
jtLastUpdated :: Lens' JobTemplate (Maybe UTCTime)
jtLastUpdated = lens _jtLastUpdated (\ s a -> s{_jtLastUpdated = a}) . mapping _Time

-- | Undocumented member.
jtSettings :: Lens' JobTemplate (Maybe JobTemplateSettings)
jtSettings = lens _jtSettings (\ s a -> s{_jtSettings = a})

-- | An identifier for this resource that is unique within all of AWS.
jtARN :: Lens' JobTemplate (Maybe Text)
jtARN = lens _jtARN (\ s a -> s{_jtARN = a})

-- | The timestamp in epoch seconds for Job template creation.
jtCreatedAt :: Lens' JobTemplate (Maybe UTCTime)
jtCreatedAt = lens _jtCreatedAt (\ s a -> s{_jtCreatedAt = a}) . mapping _Time

-- | An optional category you create to organize your job templates.
jtCategory :: Lens' JobTemplate (Maybe Text)
jtCategory = lens _jtCategory (\ s a -> s{_jtCategory = a})

-- | Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
jtQueue :: Lens' JobTemplate (Maybe Text)
jtQueue = lens _jtQueue (\ s a -> s{_jtQueue = a})

-- | A name you create for each job template. Each name must be unique within your account.
jtName :: Lens' JobTemplate (Maybe Text)
jtName = lens _jtName (\ s a -> s{_jtName = a})

-- | A job template can be of two types: system or custom. System or built-in job templates can't be modified or deleted by the user.
jtType :: Lens' JobTemplate (Maybe Type)
jtType = lens _jtType (\ s a -> s{_jtType = a})

-- | An optional description you create for each job template.
jtDescription :: Lens' JobTemplate (Maybe Text)
jtDescription = lens _jtDescription (\ s a -> s{_jtDescription = a})

instance FromJSON JobTemplate where
        parseJSON
          = withObject "JobTemplate"
              (\ x ->
                 JobTemplate' <$>
                   (x .:? "lastUpdated") <*> (x .:? "settings") <*>
                     (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "category")
                     <*> (x .:? "queue")
                     <*> (x .:? "name")
                     <*> (x .:? "type")
                     <*> (x .:? "description"))

instance Hashable JobTemplate where

instance NFData JobTemplate where

-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
--
-- /See:/ 'jobTemplateSettings' smart constructor.
data JobTemplateSettings = JobTemplateSettings'
  { _jtsInputs                 :: !(Maybe [InputTemplate])
  , _jtsTimedMetadataInsertion :: !(Maybe TimedMetadataInsertion)
  , _jtsNielsenConfiguration   :: !(Maybe NielsenConfiguration)
  , _jtsAvailBlanking          :: !(Maybe AvailBlanking)
  , _jtsTimecodeConfig         :: !(Maybe TimecodeConfig)
  , _jtsOutputGroups           :: !(Maybe [OutputGroup])
  , _jtsAdAvailOffset          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobTemplateSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jtsInputs' - Use Inputs (inputs) to define the source file used in the transcode job. There can only be one input in a job template.  Using the API, you can include multiple inputs when referencing a job template.
--
-- * 'jtsTimedMetadataInsertion' - Undocumented member.
--
-- * 'jtsNielsenConfiguration' - Undocumented member.
--
-- * 'jtsAvailBlanking' - Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
--
-- * 'jtsTimecodeConfig' - Contains settings used to acquire and adjust timecode information from inputs.
--
-- * 'jtsOutputGroups' - **!!**(OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings
--
-- * 'jtsAdAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
jobTemplateSettings
    :: JobTemplateSettings
jobTemplateSettings =
  JobTemplateSettings'
    { _jtsInputs = Nothing
    , _jtsTimedMetadataInsertion = Nothing
    , _jtsNielsenConfiguration = Nothing
    , _jtsAvailBlanking = Nothing
    , _jtsTimecodeConfig = Nothing
    , _jtsOutputGroups = Nothing
    , _jtsAdAvailOffset = Nothing
    }


-- | Use Inputs (inputs) to define the source file used in the transcode job. There can only be one input in a job template.  Using the API, you can include multiple inputs when referencing a job template.
jtsInputs :: Lens' JobTemplateSettings [InputTemplate]
jtsInputs = lens _jtsInputs (\ s a -> s{_jtsInputs = a}) . _Default . _Coerce

-- | Undocumented member.
jtsTimedMetadataInsertion :: Lens' JobTemplateSettings (Maybe TimedMetadataInsertion)
jtsTimedMetadataInsertion = lens _jtsTimedMetadataInsertion (\ s a -> s{_jtsTimedMetadataInsertion = a})

-- | Undocumented member.
jtsNielsenConfiguration :: Lens' JobTemplateSettings (Maybe NielsenConfiguration)
jtsNielsenConfiguration = lens _jtsNielsenConfiguration (\ s a -> s{_jtsNielsenConfiguration = a})

-- | Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
jtsAvailBlanking :: Lens' JobTemplateSettings (Maybe AvailBlanking)
jtsAvailBlanking = lens _jtsAvailBlanking (\ s a -> s{_jtsAvailBlanking = a})

-- | Contains settings used to acquire and adjust timecode information from inputs.
jtsTimecodeConfig :: Lens' JobTemplateSettings (Maybe TimecodeConfig)
jtsTimecodeConfig = lens _jtsTimecodeConfig (\ s a -> s{_jtsTimecodeConfig = a})

-- | **!!**(OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings
jtsOutputGroups :: Lens' JobTemplateSettings [OutputGroup]
jtsOutputGroups = lens _jtsOutputGroups (\ s a -> s{_jtsOutputGroups = a}) . _Default . _Coerce

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
jtsAdAvailOffset :: Lens' JobTemplateSettings (Maybe Int)
jtsAdAvailOffset = lens _jtsAdAvailOffset (\ s a -> s{_jtsAdAvailOffset = a})

instance FromJSON JobTemplateSettings where
        parseJSON
          = withObject "JobTemplateSettings"
              (\ x ->
                 JobTemplateSettings' <$>
                   (x .:? "inputs" .!= mempty) <*>
                     (x .:? "timedMetadataInsertion")
                     <*> (x .:? "nielsenConfiguration")
                     <*> (x .:? "availBlanking")
                     <*> (x .:? "timecodeConfig")
                     <*> (x .:? "outputGroups" .!= mempty)
                     <*> (x .:? "adAvailOffset"))

instance Hashable JobTemplateSettings where

instance NFData JobTemplateSettings where

instance ToJSON JobTemplateSettings where
        toJSON JobTemplateSettings'{..}
          = object
              (catMaybes
                 [("inputs" .=) <$> _jtsInputs,
                  ("timedMetadataInsertion" .=) <$>
                    _jtsTimedMetadataInsertion,
                  ("nielsenConfiguration" .=) <$>
                    _jtsNielsenConfiguration,
                  ("availBlanking" .=) <$> _jtsAvailBlanking,
                  ("timecodeConfig" .=) <$> _jtsTimecodeConfig,
                  ("outputGroups" .=) <$> _jtsOutputGroups,
                  ("adAvailOffset" .=) <$> _jtsAdAvailOffset])

-- | Settings for M2TS Container.
--
-- /See:/ 'm2tsSettings' smart constructor.
data M2tsSettings = M2tsSettings'
  { _mPmtPid              :: !(Maybe Int)
  , _mVideoPid            :: !(Maybe Int)
  , _mBufferModel         :: !(Maybe M2tsBufferModel)
  , _mProgramNumber       :: !(Maybe Int)
  , _mScte35Pid           :: !(Maybe Int)
  , _mMinEbpInterval      :: !(Maybe Int)
  , _mTransportStreamId   :: !(Maybe Int)
  , _mMaxPcrInterval      :: !(Maybe Int)
  , _mFragmentTime        :: !(Maybe Double)
  , _mPrivateMetadataPid  :: !(Maybe Int)
  , _mPmtInterval         :: !(Maybe Int)
  , _mDvbSdtSettings      :: !(Maybe DvbSdtSettings)
  , _mNullPacketBitrate   :: !(Maybe Double)
  , _mAudioBufferModel    :: !(Maybe M2tsAudioBufferModel)
  , _mTimedMetadataPid    :: !(Maybe Int)
  , _mAudioFramesPerPes   :: !(Maybe Int)
  , _mPcrPid              :: !(Maybe Int)
  , _mSegmentationMarkers :: !(Maybe M2tsSegmentationMarkers)
  , _mDvbSubPids          :: !(Maybe [Int])
  , _mScte35Source        :: !(Maybe M2tsScte35Source)
  , _mPatInterval         :: !(Maybe Int)
  , _mEsRateInPes         :: !(Maybe M2tsEsRateInPes)
  , _mBitrate             :: !(Maybe Int)
  , _mAudioPids           :: !(Maybe [Int])
  , _mDvbTeletextPid      :: !(Maybe Int)
  , _mNielsenId3          :: !(Maybe M2tsNielsenId3)
  , _mSegmentationTime    :: !(Maybe Double)
  , _mEbpAudioInterval    :: !(Maybe M2tsEbpAudioInterval)
  , _mDvbNitSettings      :: !(Maybe DvbNitSettings)
  , _mPcrControl          :: !(Maybe M2tsPcrControl)
  , _mEbpPlacement        :: !(Maybe M2tsEbpPlacement)
  , _mRateMode            :: !(Maybe M2tsRateMode)
  , _mSegmentationStyle   :: !(Maybe M2tsSegmentationStyle)
  , _mDvbTdtSettings      :: !(Maybe DvbTdtSettings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'M2tsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mPmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream.
--
-- * 'mVideoPid' - Packet Identifier (PID) of the elementary video stream in the transport stream.
--
-- * 'mBufferModel' - Undocumented member.
--
-- * 'mProgramNumber' - The value of the program number field in the Program Map Table.
--
-- * 'mScte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
--
-- * 'mMinEbpInterval' - When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker. The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
--
-- * 'mTransportStreamId' - The value of the transport stream ID field in the Program Map Table.
--
-- * 'mMaxPcrInterval' - Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
--
-- * 'mFragmentTime' - The length in seconds of each fragment. Only used with EBP markers.
--
-- * 'mPrivateMetadataPid' - Packet Identifier (PID) of the private metadata stream in the transport stream.
--
-- * 'mPmtInterval' - The number of milliseconds between instances of this table in the output transport stream.
--
-- * 'mDvbSdtSettings' - Undocumented member.
--
-- * 'mNullPacketBitrate' - Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
--
-- * 'mAudioBufferModel' - Undocumented member.
--
-- * 'mTimedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport stream.
--
-- * 'mAudioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- * 'mPcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID.
--
-- * 'mSegmentationMarkers' - Undocumented member.
--
-- * 'mDvbSubPids' - Packet Identifier (PID) for input source DVB Subtitle data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation.
--
-- * 'mScte35Source' - Undocumented member.
--
-- * 'mPatInterval' - The number of milliseconds between instances of this table in the output transport stream.
--
-- * 'mEsRateInPes' - Undocumented member.
--
-- * 'mBitrate' - The output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate. Other common values are 3750000, 7500000, and 15000000.
--
-- * 'mAudioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation.
--
-- * 'mDvbTeletextPid' - Packet Identifier (PID) for input source DVB Teletext data to this output.
--
-- * 'mNielsenId3' - Undocumented member.
--
-- * 'mSegmentationTime' - The length in seconds of each segment. Required unless markers is set to _none_.
--
-- * 'mEbpAudioInterval' - Undocumented member.
--
-- * 'mDvbNitSettings' - Undocumented member.
--
-- * 'mPcrControl' - Undocumented member.
--
-- * 'mEbpPlacement' - Undocumented member.
--
-- * 'mRateMode' - Undocumented member.
--
-- * 'mSegmentationStyle' - Undocumented member.
--
-- * 'mDvbTdtSettings' - Undocumented member.
m2tsSettings
    :: M2tsSettings
m2tsSettings =
  M2tsSettings'
    { _mPmtPid = Nothing
    , _mVideoPid = Nothing
    , _mBufferModel = Nothing
    , _mProgramNumber = Nothing
    , _mScte35Pid = Nothing
    , _mMinEbpInterval = Nothing
    , _mTransportStreamId = Nothing
    , _mMaxPcrInterval = Nothing
    , _mFragmentTime = Nothing
    , _mPrivateMetadataPid = Nothing
    , _mPmtInterval = Nothing
    , _mDvbSdtSettings = Nothing
    , _mNullPacketBitrate = Nothing
    , _mAudioBufferModel = Nothing
    , _mTimedMetadataPid = Nothing
    , _mAudioFramesPerPes = Nothing
    , _mPcrPid = Nothing
    , _mSegmentationMarkers = Nothing
    , _mDvbSubPids = Nothing
    , _mScte35Source = Nothing
    , _mPatInterval = Nothing
    , _mEsRateInPes = Nothing
    , _mBitrate = Nothing
    , _mAudioPids = Nothing
    , _mDvbTeletextPid = Nothing
    , _mNielsenId3 = Nothing
    , _mSegmentationTime = Nothing
    , _mEbpAudioInterval = Nothing
    , _mDvbNitSettings = Nothing
    , _mPcrControl = Nothing
    , _mEbpPlacement = Nothing
    , _mRateMode = Nothing
    , _mSegmentationStyle = Nothing
    , _mDvbTdtSettings = Nothing
    }


-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream.
mPmtPid :: Lens' M2tsSettings (Maybe Int)
mPmtPid = lens _mPmtPid (\ s a -> s{_mPmtPid = a})

-- | Packet Identifier (PID) of the elementary video stream in the transport stream.
mVideoPid :: Lens' M2tsSettings (Maybe Int)
mVideoPid = lens _mVideoPid (\ s a -> s{_mVideoPid = a})

-- | Undocumented member.
mBufferModel :: Lens' M2tsSettings (Maybe M2tsBufferModel)
mBufferModel = lens _mBufferModel (\ s a -> s{_mBufferModel = a})

-- | The value of the program number field in the Program Map Table.
mProgramNumber :: Lens' M2tsSettings (Maybe Int)
mProgramNumber = lens _mProgramNumber (\ s a -> s{_mProgramNumber = a})

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
mScte35Pid :: Lens' M2tsSettings (Maybe Int)
mScte35Pid = lens _mScte35Pid (\ s a -> s{_mScte35Pid = a})

-- | When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker. The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
mMinEbpInterval :: Lens' M2tsSettings (Maybe Int)
mMinEbpInterval = lens _mMinEbpInterval (\ s a -> s{_mMinEbpInterval = a})

-- | The value of the transport stream ID field in the Program Map Table.
mTransportStreamId :: Lens' M2tsSettings (Maybe Int)
mTransportStreamId = lens _mTransportStreamId (\ s a -> s{_mTransportStreamId = a})

-- | Maximum time in milliseconds between Program Clock References (PCRs) inserted into the transport stream.
mMaxPcrInterval :: Lens' M2tsSettings (Maybe Int)
mMaxPcrInterval = lens _mMaxPcrInterval (\ s a -> s{_mMaxPcrInterval = a})

-- | The length in seconds of each fragment. Only used with EBP markers.
mFragmentTime :: Lens' M2tsSettings (Maybe Double)
mFragmentTime = lens _mFragmentTime (\ s a -> s{_mFragmentTime = a})

-- | Packet Identifier (PID) of the private metadata stream in the transport stream.
mPrivateMetadataPid :: Lens' M2tsSettings (Maybe Int)
mPrivateMetadataPid = lens _mPrivateMetadataPid (\ s a -> s{_mPrivateMetadataPid = a})

-- | The number of milliseconds between instances of this table in the output transport stream.
mPmtInterval :: Lens' M2tsSettings (Maybe Int)
mPmtInterval = lens _mPmtInterval (\ s a -> s{_mPmtInterval = a})

-- | Undocumented member.
mDvbSdtSettings :: Lens' M2tsSettings (Maybe DvbSdtSettings)
mDvbSdtSettings = lens _mDvbSdtSettings (\ s a -> s{_mDvbSdtSettings = a})

-- | Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
mNullPacketBitrate :: Lens' M2tsSettings (Maybe Double)
mNullPacketBitrate = lens _mNullPacketBitrate (\ s a -> s{_mNullPacketBitrate = a})

-- | Undocumented member.
mAudioBufferModel :: Lens' M2tsSettings (Maybe M2tsAudioBufferModel)
mAudioBufferModel = lens _mAudioBufferModel (\ s a -> s{_mAudioBufferModel = a})

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream.
mTimedMetadataPid :: Lens' M2tsSettings (Maybe Int)
mTimedMetadataPid = lens _mTimedMetadataPid (\ s a -> s{_mTimedMetadataPid = a})

-- | The number of audio frames to insert for each PES packet.
mAudioFramesPerPes :: Lens' M2tsSettings (Maybe Int)
mAudioFramesPerPes = lens _mAudioFramesPerPes (\ s a -> s{_mAudioFramesPerPes = a})

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID.
mPcrPid :: Lens' M2tsSettings (Maybe Int)
mPcrPid = lens _mPcrPid (\ s a -> s{_mPcrPid = a})

-- | Undocumented member.
mSegmentationMarkers :: Lens' M2tsSettings (Maybe M2tsSegmentationMarkers)
mSegmentationMarkers = lens _mSegmentationMarkers (\ s a -> s{_mSegmentationMarkers = a})

-- | Packet Identifier (PID) for input source DVB Subtitle data to this output. Multiple values are accepted, and can be entered in ranges and/or by comma separation.
mDvbSubPids :: Lens' M2tsSettings [Int]
mDvbSubPids = lens _mDvbSubPids (\ s a -> s{_mDvbSubPids = a}) . _Default . _Coerce

-- | Undocumented member.
mScte35Source :: Lens' M2tsSettings (Maybe M2tsScte35Source)
mScte35Source = lens _mScte35Source (\ s a -> s{_mScte35Source = a})

-- | The number of milliseconds between instances of this table in the output transport stream.
mPatInterval :: Lens' M2tsSettings (Maybe Int)
mPatInterval = lens _mPatInterval (\ s a -> s{_mPatInterval = a})

-- | Undocumented member.
mEsRateInPes :: Lens' M2tsSettings (Maybe M2tsEsRateInPes)
mEsRateInPes = lens _mEsRateInPes (\ s a -> s{_mEsRateInPes = a})

-- | The output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate. Other common values are 3750000, 7500000, and 15000000.
mBitrate :: Lens' M2tsSettings (Maybe Int)
mBitrate = lens _mBitrate (\ s a -> s{_mBitrate = a})

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation.
mAudioPids :: Lens' M2tsSettings [Int]
mAudioPids = lens _mAudioPids (\ s a -> s{_mAudioPids = a}) . _Default . _Coerce

-- | Packet Identifier (PID) for input source DVB Teletext data to this output.
mDvbTeletextPid :: Lens' M2tsSettings (Maybe Int)
mDvbTeletextPid = lens _mDvbTeletextPid (\ s a -> s{_mDvbTeletextPid = a})

-- | Undocumented member.
mNielsenId3 :: Lens' M2tsSettings (Maybe M2tsNielsenId3)
mNielsenId3 = lens _mNielsenId3 (\ s a -> s{_mNielsenId3 = a})

-- | The length in seconds of each segment. Required unless markers is set to _none_.
mSegmentationTime :: Lens' M2tsSettings (Maybe Double)
mSegmentationTime = lens _mSegmentationTime (\ s a -> s{_mSegmentationTime = a})

-- | Undocumented member.
mEbpAudioInterval :: Lens' M2tsSettings (Maybe M2tsEbpAudioInterval)
mEbpAudioInterval = lens _mEbpAudioInterval (\ s a -> s{_mEbpAudioInterval = a})

-- | Undocumented member.
mDvbNitSettings :: Lens' M2tsSettings (Maybe DvbNitSettings)
mDvbNitSettings = lens _mDvbNitSettings (\ s a -> s{_mDvbNitSettings = a})

-- | Undocumented member.
mPcrControl :: Lens' M2tsSettings (Maybe M2tsPcrControl)
mPcrControl = lens _mPcrControl (\ s a -> s{_mPcrControl = a})

-- | Undocumented member.
mEbpPlacement :: Lens' M2tsSettings (Maybe M2tsEbpPlacement)
mEbpPlacement = lens _mEbpPlacement (\ s a -> s{_mEbpPlacement = a})

-- | Undocumented member.
mRateMode :: Lens' M2tsSettings (Maybe M2tsRateMode)
mRateMode = lens _mRateMode (\ s a -> s{_mRateMode = a})

-- | Undocumented member.
mSegmentationStyle :: Lens' M2tsSettings (Maybe M2tsSegmentationStyle)
mSegmentationStyle = lens _mSegmentationStyle (\ s a -> s{_mSegmentationStyle = a})

-- | Undocumented member.
mDvbTdtSettings :: Lens' M2tsSettings (Maybe DvbTdtSettings)
mDvbTdtSettings = lens _mDvbTdtSettings (\ s a -> s{_mDvbTdtSettings = a})

instance FromJSON M2tsSettings where
        parseJSON
          = withObject "M2tsSettings"
              (\ x ->
                 M2tsSettings' <$>
                   (x .:? "pmtPid") <*> (x .:? "videoPid") <*>
                     (x .:? "bufferModel")
                     <*> (x .:? "programNumber")
                     <*> (x .:? "scte35Pid")
                     <*> (x .:? "minEbpInterval")
                     <*> (x .:? "transportStreamId")
                     <*> (x .:? "maxPcrInterval")
                     <*> (x .:? "fragmentTime")
                     <*> (x .:? "privateMetadataPid")
                     <*> (x .:? "pmtInterval")
                     <*> (x .:? "dvbSdtSettings")
                     <*> (x .:? "nullPacketBitrate")
                     <*> (x .:? "audioBufferModel")
                     <*> (x .:? "timedMetadataPid")
                     <*> (x .:? "audioFramesPerPes")
                     <*> (x .:? "pcrPid")
                     <*> (x .:? "segmentationMarkers")
                     <*> (x .:? "dvbSubPids" .!= mempty)
                     <*> (x .:? "scte35Source")
                     <*> (x .:? "patInterval")
                     <*> (x .:? "esRateInPes")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "audioPids" .!= mempty)
                     <*> (x .:? "dvbTeletextPid")
                     <*> (x .:? "nielsenId3")
                     <*> (x .:? "segmentationTime")
                     <*> (x .:? "ebpAudioInterval")
                     <*> (x .:? "dvbNitSettings")
                     <*> (x .:? "pcrControl")
                     <*> (x .:? "ebpPlacement")
                     <*> (x .:? "rateMode")
                     <*> (x .:? "segmentationStyle")
                     <*> (x .:? "dvbTdtSettings"))

instance Hashable M2tsSettings where

instance NFData M2tsSettings where

instance ToJSON M2tsSettings where
        toJSON M2tsSettings'{..}
          = object
              (catMaybes
                 [("pmtPid" .=) <$> _mPmtPid,
                  ("videoPid" .=) <$> _mVideoPid,
                  ("bufferModel" .=) <$> _mBufferModel,
                  ("programNumber" .=) <$> _mProgramNumber,
                  ("scte35Pid" .=) <$> _mScte35Pid,
                  ("minEbpInterval" .=) <$> _mMinEbpInterval,
                  ("transportStreamId" .=) <$> _mTransportStreamId,
                  ("maxPcrInterval" .=) <$> _mMaxPcrInterval,
                  ("fragmentTime" .=) <$> _mFragmentTime,
                  ("privateMetadataPid" .=) <$> _mPrivateMetadataPid,
                  ("pmtInterval" .=) <$> _mPmtInterval,
                  ("dvbSdtSettings" .=) <$> _mDvbSdtSettings,
                  ("nullPacketBitrate" .=) <$> _mNullPacketBitrate,
                  ("audioBufferModel" .=) <$> _mAudioBufferModel,
                  ("timedMetadataPid" .=) <$> _mTimedMetadataPid,
                  ("audioFramesPerPes" .=) <$> _mAudioFramesPerPes,
                  ("pcrPid" .=) <$> _mPcrPid,
                  ("segmentationMarkers" .=) <$> _mSegmentationMarkers,
                  ("dvbSubPids" .=) <$> _mDvbSubPids,
                  ("scte35Source" .=) <$> _mScte35Source,
                  ("patInterval" .=) <$> _mPatInterval,
                  ("esRateInPes" .=) <$> _mEsRateInPes,
                  ("bitrate" .=) <$> _mBitrate,
                  ("audioPids" .=) <$> _mAudioPids,
                  ("dvbTeletextPid" .=) <$> _mDvbTeletextPid,
                  ("nielsenId3" .=) <$> _mNielsenId3,
                  ("segmentationTime" .=) <$> _mSegmentationTime,
                  ("ebpAudioInterval" .=) <$> _mEbpAudioInterval,
                  ("dvbNitSettings" .=) <$> _mDvbNitSettings,
                  ("pcrControl" .=) <$> _mPcrControl,
                  ("ebpPlacement" .=) <$> _mEbpPlacement,
                  ("rateMode" .=) <$> _mRateMode,
                  ("segmentationStyle" .=) <$> _mSegmentationStyle,
                  ("dvbTdtSettings" .=) <$> _mDvbTdtSettings])

-- | Settings for TS segments in HLS
--
-- /See:/ 'm3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'
  { _msPmtPid             :: !(Maybe Int)
  , _msVideoPid           :: !(Maybe Int)
  , _msProgramNumber      :: !(Maybe Int)
  , _msScte35Pid          :: !(Maybe Int)
  , _msTransportStreamId  :: !(Maybe Int)
  , _msPrivateMetadataPid :: !(Maybe Int)
  , _msPmtInterval        :: !(Maybe Int)
  , _msTimedMetadataPid   :: !(Maybe Int)
  , _msAudioFramesPerPes  :: !(Maybe Int)
  , _msPcrPid             :: !(Maybe Int)
  , _msTimedMetadata      :: !(Maybe TimedMetadata)
  , _msScte35Source       :: !(Maybe M3u8Scte35Source)
  , _msPatInterval        :: !(Maybe Int)
  , _msAudioPids          :: !(Maybe [Int])
  , _msNielsenId3         :: !(Maybe M3u8NielsenId3)
  , _msPcrControl         :: !(Maybe M3u8PcrControl)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'M3u8Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msPmtPid' - Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream.
--
-- * 'msVideoPid' - Packet Identifier (PID) of the elementary video stream in the transport stream.
--
-- * 'msProgramNumber' - The value of the program number field in the Program Map Table.
--
-- * 'msScte35Pid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
--
-- * 'msTransportStreamId' - The value of the transport stream ID field in the Program Map Table.
--
-- * 'msPrivateMetadataPid' - Packet Identifier (PID) of the private metadata stream in the transport stream.
--
-- * 'msPmtInterval' - The number of milliseconds between instances of this table in the output transport stream.
--
-- * 'msTimedMetadataPid' - Packet Identifier (PID) of the timed metadata stream in the transport stream.
--
-- * 'msAudioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- * 'msPcrPid' - Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID.
--
-- * 'msTimedMetadata' - Undocumented member.
--
-- * 'msScte35Source' - Undocumented member.
--
-- * 'msPatInterval' - The number of milliseconds between instances of this table in the output transport stream.
--
-- * 'msAudioPids' - Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation.
--
-- * 'msNielsenId3' - Undocumented member.
--
-- * 'msPcrControl' - Undocumented member.
m3u8Settings
    :: M3u8Settings
m3u8Settings =
  M3u8Settings'
    { _msPmtPid = Nothing
    , _msVideoPid = Nothing
    , _msProgramNumber = Nothing
    , _msScte35Pid = Nothing
    , _msTransportStreamId = Nothing
    , _msPrivateMetadataPid = Nothing
    , _msPmtInterval = Nothing
    , _msTimedMetadataPid = Nothing
    , _msAudioFramesPerPes = Nothing
    , _msPcrPid = Nothing
    , _msTimedMetadata = Nothing
    , _msScte35Source = Nothing
    , _msPatInterval = Nothing
    , _msAudioPids = Nothing
    , _msNielsenId3 = Nothing
    , _msPcrControl = Nothing
    }


-- | Packet Identifier (PID) for the Program Map Table (PMT) in the transport stream.
msPmtPid :: Lens' M3u8Settings (Maybe Int)
msPmtPid = lens _msPmtPid (\ s a -> s{_msPmtPid = a})

-- | Packet Identifier (PID) of the elementary video stream in the transport stream.
msVideoPid :: Lens' M3u8Settings (Maybe Int)
msVideoPid = lens _msVideoPid (\ s a -> s{_msVideoPid = a})

-- | The value of the program number field in the Program Map Table.
msProgramNumber :: Lens' M3u8Settings (Maybe Int)
msProgramNumber = lens _msProgramNumber (\ s a -> s{_msProgramNumber = a})

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
msScte35Pid :: Lens' M3u8Settings (Maybe Int)
msScte35Pid = lens _msScte35Pid (\ s a -> s{_msScte35Pid = a})

-- | The value of the transport stream ID field in the Program Map Table.
msTransportStreamId :: Lens' M3u8Settings (Maybe Int)
msTransportStreamId = lens _msTransportStreamId (\ s a -> s{_msTransportStreamId = a})

-- | Packet Identifier (PID) of the private metadata stream in the transport stream.
msPrivateMetadataPid :: Lens' M3u8Settings (Maybe Int)
msPrivateMetadataPid = lens _msPrivateMetadataPid (\ s a -> s{_msPrivateMetadataPid = a})

-- | The number of milliseconds between instances of this table in the output transport stream.
msPmtInterval :: Lens' M3u8Settings (Maybe Int)
msPmtInterval = lens _msPmtInterval (\ s a -> s{_msPmtInterval = a})

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream.
msTimedMetadataPid :: Lens' M3u8Settings (Maybe Int)
msTimedMetadataPid = lens _msTimedMetadataPid (\ s a -> s{_msTimedMetadataPid = a})

-- | The number of audio frames to insert for each PES packet.
msAudioFramesPerPes :: Lens' M3u8Settings (Maybe Int)
msAudioFramesPerPes = lens _msAudioFramesPerPes (\ s a -> s{_msAudioFramesPerPes = a})

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID.
msPcrPid :: Lens' M3u8Settings (Maybe Int)
msPcrPid = lens _msPcrPid (\ s a -> s{_msPcrPid = a})

-- | Undocumented member.
msTimedMetadata :: Lens' M3u8Settings (Maybe TimedMetadata)
msTimedMetadata = lens _msTimedMetadata (\ s a -> s{_msTimedMetadata = a})

-- | Undocumented member.
msScte35Source :: Lens' M3u8Settings (Maybe M3u8Scte35Source)
msScte35Source = lens _msScte35Source (\ s a -> s{_msScte35Source = a})

-- | The number of milliseconds between instances of this table in the output transport stream.
msPatInterval :: Lens' M3u8Settings (Maybe Int)
msPatInterval = lens _msPatInterval (\ s a -> s{_msPatInterval = a})

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation.
msAudioPids :: Lens' M3u8Settings [Int]
msAudioPids = lens _msAudioPids (\ s a -> s{_msAudioPids = a}) . _Default . _Coerce

-- | Undocumented member.
msNielsenId3 :: Lens' M3u8Settings (Maybe M3u8NielsenId3)
msNielsenId3 = lens _msNielsenId3 (\ s a -> s{_msNielsenId3 = a})

-- | Undocumented member.
msPcrControl :: Lens' M3u8Settings (Maybe M3u8PcrControl)
msPcrControl = lens _msPcrControl (\ s a -> s{_msPcrControl = a})

instance FromJSON M3u8Settings where
        parseJSON
          = withObject "M3u8Settings"
              (\ x ->
                 M3u8Settings' <$>
                   (x .:? "pmtPid") <*> (x .:? "videoPid") <*>
                     (x .:? "programNumber")
                     <*> (x .:? "scte35Pid")
                     <*> (x .:? "transportStreamId")
                     <*> (x .:? "privateMetadataPid")
                     <*> (x .:? "pmtInterval")
                     <*> (x .:? "timedMetadataPid")
                     <*> (x .:? "audioFramesPerPes")
                     <*> (x .:? "pcrPid")
                     <*> (x .:? "timedMetadata")
                     <*> (x .:? "scte35Source")
                     <*> (x .:? "patInterval")
                     <*> (x .:? "audioPids" .!= mempty)
                     <*> (x .:? "nielsenId3")
                     <*> (x .:? "pcrControl"))

instance Hashable M3u8Settings where

instance NFData M3u8Settings where

instance ToJSON M3u8Settings where
        toJSON M3u8Settings'{..}
          = object
              (catMaybes
                 [("pmtPid" .=) <$> _msPmtPid,
                  ("videoPid" .=) <$> _msVideoPid,
                  ("programNumber" .=) <$> _msProgramNumber,
                  ("scte35Pid" .=) <$> _msScte35Pid,
                  ("transportStreamId" .=) <$> _msTransportStreamId,
                  ("privateMetadataPid" .=) <$> _msPrivateMetadataPid,
                  ("pmtInterval" .=) <$> _msPmtInterval,
                  ("timedMetadataPid" .=) <$> _msTimedMetadataPid,
                  ("audioFramesPerPes" .=) <$> _msAudioFramesPerPes,
                  ("pcrPid" .=) <$> _msPcrPid,
                  ("timedMetadata" .=) <$> _msTimedMetadata,
                  ("scte35Source" .=) <$> _msScte35Source,
                  ("patInterval" .=) <$> _msPatInterval,
                  ("audioPids" .=) <$> _msAudioPids,
                  ("nielsenId3" .=) <$> _msNielsenId3,
                  ("pcrControl" .=) <$> _msPcrControl])

-- | Settings for MOV Container.
--
-- /See:/ 'movSettings' smart constructor.
data MovSettings = MovSettings'
  { _msReference          :: !(Maybe MovReference)
  , _msCslgAtom           :: !(Maybe MovCslgAtom)
  , _msMpeg2FourCCControl :: !(Maybe MovMpeg2FourCCControl)
  , _msPaddingControl     :: !(Maybe MovPaddingControl)
  , _msClapAtom           :: !(Maybe MovClapAtom)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MovSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msReference' - Undocumented member.
--
-- * 'msCslgAtom' - Undocumented member.
--
-- * 'msMpeg2FourCCControl' - Undocumented member.
--
-- * 'msPaddingControl' - Undocumented member.
--
-- * 'msClapAtom' - Undocumented member.
movSettings
    :: MovSettings
movSettings =
  MovSettings'
    { _msReference = Nothing
    , _msCslgAtom = Nothing
    , _msMpeg2FourCCControl = Nothing
    , _msPaddingControl = Nothing
    , _msClapAtom = Nothing
    }


-- | Undocumented member.
msReference :: Lens' MovSettings (Maybe MovReference)
msReference = lens _msReference (\ s a -> s{_msReference = a})

-- | Undocumented member.
msCslgAtom :: Lens' MovSettings (Maybe MovCslgAtom)
msCslgAtom = lens _msCslgAtom (\ s a -> s{_msCslgAtom = a})

-- | Undocumented member.
msMpeg2FourCCControl :: Lens' MovSettings (Maybe MovMpeg2FourCCControl)
msMpeg2FourCCControl = lens _msMpeg2FourCCControl (\ s a -> s{_msMpeg2FourCCControl = a})

-- | Undocumented member.
msPaddingControl :: Lens' MovSettings (Maybe MovPaddingControl)
msPaddingControl = lens _msPaddingControl (\ s a -> s{_msPaddingControl = a})

-- | Undocumented member.
msClapAtom :: Lens' MovSettings (Maybe MovClapAtom)
msClapAtom = lens _msClapAtom (\ s a -> s{_msClapAtom = a})

instance FromJSON MovSettings where
        parseJSON
          = withObject "MovSettings"
              (\ x ->
                 MovSettings' <$>
                   (x .:? "reference") <*> (x .:? "cslgAtom") <*>
                     (x .:? "mpeg2FourCCControl")
                     <*> (x .:? "paddingControl")
                     <*> (x .:? "clapAtom"))

instance Hashable MovSettings where

instance NFData MovSettings where

instance ToJSON MovSettings where
        toJSON MovSettings'{..}
          = object
              (catMaybes
                 [("reference" .=) <$> _msReference,
                  ("cslgAtom" .=) <$> _msCslgAtom,
                  ("mpeg2FourCCControl" .=) <$> _msMpeg2FourCCControl,
                  ("paddingControl" .=) <$> _msPaddingControl,
                  ("clapAtom" .=) <$> _msClapAtom])

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value MP2.
--
-- /See:/ 'mp2Settings' smart constructor.
data Mp2Settings = Mp2Settings'
  { _mssChannels   :: !(Maybe Int)
  , _mssSampleRate :: !(Maybe Int)
  , _mssBitrate    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Mp2Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mssChannels' - Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
--
-- * 'mssSampleRate' - Sample rate in hz.
--
-- * 'mssBitrate' - Average bitrate in bits/second.
mp2Settings
    :: Mp2Settings
mp2Settings =
  Mp2Settings'
    {_mssChannels = Nothing, _mssSampleRate = Nothing, _mssBitrate = Nothing}


-- | Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
mssChannels :: Lens' Mp2Settings (Maybe Int)
mssChannels = lens _mssChannels (\ s a -> s{_mssChannels = a})

-- | Sample rate in hz.
mssSampleRate :: Lens' Mp2Settings (Maybe Int)
mssSampleRate = lens _mssSampleRate (\ s a -> s{_mssSampleRate = a})

-- | Average bitrate in bits/second.
mssBitrate :: Lens' Mp2Settings (Maybe Int)
mssBitrate = lens _mssBitrate (\ s a -> s{_mssBitrate = a})

instance FromJSON Mp2Settings where
        parseJSON
          = withObject "Mp2Settings"
              (\ x ->
                 Mp2Settings' <$>
                   (x .:? "channels") <*> (x .:? "sampleRate") <*>
                     (x .:? "bitrate"))

instance Hashable Mp2Settings where

instance NFData Mp2Settings where

instance ToJSON Mp2Settings where
        toJSON Mp2Settings'{..}
          = object
              (catMaybes
                 [("channels" .=) <$> _mssChannels,
                  ("sampleRate" .=) <$> _mssSampleRate,
                  ("bitrate" .=) <$> _mssBitrate])

-- | Settings for MP4 Container
--
-- /See:/ 'mp4Settings' smart constructor.
data Mp4Settings = Mp4Settings'
  { _mMoovPlacement :: !(Maybe Mp4MoovPlacement)
  , _mFreeSpaceBox  :: !(Maybe Mp4FreeSpaceBox)
  , _mMp4MajorBrand :: !(Maybe Text)
  , _mCslgAtom      :: !(Maybe Mp4CslgAtom)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Mp4Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMoovPlacement' - Undocumented member.
--
-- * 'mFreeSpaceBox' - Undocumented member.
--
-- * 'mMp4MajorBrand' - Overrides the "Major Brand" field in the output file. Usually not necessary to specify.
--
-- * 'mCslgAtom' - Undocumented member.
mp4Settings
    :: Mp4Settings
mp4Settings =
  Mp4Settings'
    { _mMoovPlacement = Nothing
    , _mFreeSpaceBox = Nothing
    , _mMp4MajorBrand = Nothing
    , _mCslgAtom = Nothing
    }


-- | Undocumented member.
mMoovPlacement :: Lens' Mp4Settings (Maybe Mp4MoovPlacement)
mMoovPlacement = lens _mMoovPlacement (\ s a -> s{_mMoovPlacement = a})

-- | Undocumented member.
mFreeSpaceBox :: Lens' Mp4Settings (Maybe Mp4FreeSpaceBox)
mFreeSpaceBox = lens _mFreeSpaceBox (\ s a -> s{_mFreeSpaceBox = a})

-- | Overrides the "Major Brand" field in the output file. Usually not necessary to specify.
mMp4MajorBrand :: Lens' Mp4Settings (Maybe Text)
mMp4MajorBrand = lens _mMp4MajorBrand (\ s a -> s{_mMp4MajorBrand = a})

-- | Undocumented member.
mCslgAtom :: Lens' Mp4Settings (Maybe Mp4CslgAtom)
mCslgAtom = lens _mCslgAtom (\ s a -> s{_mCslgAtom = a})

instance FromJSON Mp4Settings where
        parseJSON
          = withObject "Mp4Settings"
              (\ x ->
                 Mp4Settings' <$>
                   (x .:? "moovPlacement") <*> (x .:? "freeSpaceBox")
                     <*> (x .:? "mp4MajorBrand")
                     <*> (x .:? "cslgAtom"))

instance Hashable Mp4Settings where

instance NFData Mp4Settings where

instance ToJSON Mp4Settings where
        toJSON Mp4Settings'{..}
          = object
              (catMaybes
                 [("moovPlacement" .=) <$> _mMoovPlacement,
                  ("freeSpaceBox" .=) <$> _mFreeSpaceBox,
                  ("mp4MajorBrand" .=) <$> _mMp4MajorBrand,
                  ("cslgAtom" .=) <$> _mCslgAtom])

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.
--
-- /See:/ 'mpeg2Settings' smart constructor.
data Mpeg2Settings = Mpeg2Settings'
  { _msQualityTuningLevel :: !(Maybe Mpeg2QualityTuningLevel)
  , _msTemporalAdaptiveQuantization :: !(Maybe Mpeg2TemporalAdaptiveQuantization)
  , _msSceneChangeDetect :: !(Maybe Mpeg2SceneChangeDetect)
  , _msHrdBufferInitialFillPercentage :: !(Maybe Int)
  , _msSlowPal :: !(Maybe Mpeg2SlowPal)
  , _msParNumerator :: !(Maybe Int)
  , _msGopSize :: !(Maybe Double)
  , _msNumberBFramesBetweenReferenceFrames :: !(Maybe Int)
  , _msGopSizeUnits :: !(Maybe Mpeg2GopSizeUnits)
  , _msHrdBufferSize :: !(Maybe Int)
  , _msRateControlMode :: !(Maybe Mpeg2RateControlMode)
  , _msTelecine :: !(Maybe Mpeg2Telecine)
  , _msIntraDcPrecision :: !(Maybe Mpeg2IntraDcPrecision)
  , _msMinIInterval :: !(Maybe Int)
  , _msInterlaceMode :: !(Maybe Mpeg2InterlaceMode)
  , _msParControl :: !(Maybe Mpeg2ParControl)
  , _msSoftness :: !(Maybe Int)
  , _msCodecProfile :: !(Maybe Mpeg2CodecProfile)
  , _msBitrate :: !(Maybe Int)
  , _msFramerateDenominator :: !(Maybe Int)
  , _msFramerateConversionAlgorithm :: !(Maybe Mpeg2FramerateConversionAlgorithm)
  , _msCodecLevel :: !(Maybe Mpeg2CodecLevel)
  , _msFramerateControl :: !(Maybe Mpeg2FramerateControl)
  , _msAdaptiveQuantization :: !(Maybe Mpeg2AdaptiveQuantization)
  , _msFramerateNumerator :: !(Maybe Int)
  , _msMaxBitrate :: !(Maybe Int)
  , _msSyntax :: !(Maybe Mpeg2Syntax)
  , _msGopClosedCadence :: !(Maybe Int)
  , _msParDenominator :: !(Maybe Int)
  , _msSpatialAdaptiveQuantization :: !(Maybe Mpeg2SpatialAdaptiveQuantization)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Mpeg2Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msQualityTuningLevel' - Undocumented member.
--
-- * 'msTemporalAdaptiveQuantization' - Undocumented member.
--
-- * 'msSceneChangeDetect' - Undocumented member.
--
-- * 'msHrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- * 'msSlowPal' - Undocumented member.
--
-- * 'msParNumerator' - Pixel Aspect Ratio numerator.
--
-- * 'msGopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- * 'msNumberBFramesBetweenReferenceFrames' - Number of B-frames between reference frames.
--
-- * 'msGopSizeUnits' - Undocumented member.
--
-- * 'msHrdBufferSize' - Size of buffer (HRD buffer model). Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m.
--
-- * 'msRateControlMode' - Undocumented member.
--
-- * 'msTelecine' - Undocumented member.
--
-- * 'msIntraDcPrecision' - Undocumented member.
--
-- * 'msMinIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'msInterlaceMode' - Undocumented member.
--
-- * 'msParControl' - Undocumented member.
--
-- * 'msSoftness' - Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
--
-- * 'msCodecProfile' - Undocumented member.
--
-- * 'msBitrate' - Average bitrate in bits/second. Required for VBR, CBR, and ABR. Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- * 'msFramerateDenominator' - Framerate denominator.
--
-- * 'msFramerateConversionAlgorithm' - Undocumented member.
--
-- * 'msCodecLevel' - Undocumented member.
--
-- * 'msFramerateControl' - Undocumented member.
--
-- * 'msAdaptiveQuantization' - Undocumented member.
--
-- * 'msFramerateNumerator' - Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- * 'msMaxBitrate' - Maximum bitrate in bits/second (for VBR mode only). Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m.
--
-- * 'msSyntax' - Undocumented member.
--
-- * 'msGopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- * 'msParDenominator' - Pixel Aspect Ratio denominator.
--
-- * 'msSpatialAdaptiveQuantization' - Undocumented member.
mpeg2Settings
    :: Mpeg2Settings
mpeg2Settings =
  Mpeg2Settings'
    { _msQualityTuningLevel = Nothing
    , _msTemporalAdaptiveQuantization = Nothing
    , _msSceneChangeDetect = Nothing
    , _msHrdBufferInitialFillPercentage = Nothing
    , _msSlowPal = Nothing
    , _msParNumerator = Nothing
    , _msGopSize = Nothing
    , _msNumberBFramesBetweenReferenceFrames = Nothing
    , _msGopSizeUnits = Nothing
    , _msHrdBufferSize = Nothing
    , _msRateControlMode = Nothing
    , _msTelecine = Nothing
    , _msIntraDcPrecision = Nothing
    , _msMinIInterval = Nothing
    , _msInterlaceMode = Nothing
    , _msParControl = Nothing
    , _msSoftness = Nothing
    , _msCodecProfile = Nothing
    , _msBitrate = Nothing
    , _msFramerateDenominator = Nothing
    , _msFramerateConversionAlgorithm = Nothing
    , _msCodecLevel = Nothing
    , _msFramerateControl = Nothing
    , _msAdaptiveQuantization = Nothing
    , _msFramerateNumerator = Nothing
    , _msMaxBitrate = Nothing
    , _msSyntax = Nothing
    , _msGopClosedCadence = Nothing
    , _msParDenominator = Nothing
    , _msSpatialAdaptiveQuantization = Nothing
    }


-- | Undocumented member.
msQualityTuningLevel :: Lens' Mpeg2Settings (Maybe Mpeg2QualityTuningLevel)
msQualityTuningLevel = lens _msQualityTuningLevel (\ s a -> s{_msQualityTuningLevel = a})

-- | Undocumented member.
msTemporalAdaptiveQuantization :: Lens' Mpeg2Settings (Maybe Mpeg2TemporalAdaptiveQuantization)
msTemporalAdaptiveQuantization = lens _msTemporalAdaptiveQuantization (\ s a -> s{_msTemporalAdaptiveQuantization = a})

-- | Undocumented member.
msSceneChangeDetect :: Lens' Mpeg2Settings (Maybe Mpeg2SceneChangeDetect)
msSceneChangeDetect = lens _msSceneChangeDetect (\ s a -> s{_msSceneChangeDetect = a})

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
msHrdBufferInitialFillPercentage :: Lens' Mpeg2Settings (Maybe Int)
msHrdBufferInitialFillPercentage = lens _msHrdBufferInitialFillPercentage (\ s a -> s{_msHrdBufferInitialFillPercentage = a})

-- | Undocumented member.
msSlowPal :: Lens' Mpeg2Settings (Maybe Mpeg2SlowPal)
msSlowPal = lens _msSlowPal (\ s a -> s{_msSlowPal = a})

-- | Pixel Aspect Ratio numerator.
msParNumerator :: Lens' Mpeg2Settings (Maybe Int)
msParNumerator = lens _msParNumerator (\ s a -> s{_msParNumerator = a})

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
msGopSize :: Lens' Mpeg2Settings (Maybe Double)
msGopSize = lens _msGopSize (\ s a -> s{_msGopSize = a})

-- | Number of B-frames between reference frames.
msNumberBFramesBetweenReferenceFrames :: Lens' Mpeg2Settings (Maybe Int)
msNumberBFramesBetweenReferenceFrames = lens _msNumberBFramesBetweenReferenceFrames (\ s a -> s{_msNumberBFramesBetweenReferenceFrames = a})

-- | Undocumented member.
msGopSizeUnits :: Lens' Mpeg2Settings (Maybe Mpeg2GopSizeUnits)
msGopSizeUnits = lens _msGopSizeUnits (\ s a -> s{_msGopSizeUnits = a})

-- | Size of buffer (HRD buffer model). Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m.
msHrdBufferSize :: Lens' Mpeg2Settings (Maybe Int)
msHrdBufferSize = lens _msHrdBufferSize (\ s a -> s{_msHrdBufferSize = a})

-- | Undocumented member.
msRateControlMode :: Lens' Mpeg2Settings (Maybe Mpeg2RateControlMode)
msRateControlMode = lens _msRateControlMode (\ s a -> s{_msRateControlMode = a})

-- | Undocumented member.
msTelecine :: Lens' Mpeg2Settings (Maybe Mpeg2Telecine)
msTelecine = lens _msTelecine (\ s a -> s{_msTelecine = a})

-- | Undocumented member.
msIntraDcPrecision :: Lens' Mpeg2Settings (Maybe Mpeg2IntraDcPrecision)
msIntraDcPrecision = lens _msIntraDcPrecision (\ s a -> s{_msIntraDcPrecision = a})

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
msMinIInterval :: Lens' Mpeg2Settings (Maybe Int)
msMinIInterval = lens _msMinIInterval (\ s a -> s{_msMinIInterval = a})

-- | Undocumented member.
msInterlaceMode :: Lens' Mpeg2Settings (Maybe Mpeg2InterlaceMode)
msInterlaceMode = lens _msInterlaceMode (\ s a -> s{_msInterlaceMode = a})

-- | Undocumented member.
msParControl :: Lens' Mpeg2Settings (Maybe Mpeg2ParControl)
msParControl = lens _msParControl (\ s a -> s{_msParControl = a})

-- | Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
msSoftness :: Lens' Mpeg2Settings (Maybe Int)
msSoftness = lens _msSoftness (\ s a -> s{_msSoftness = a})

-- | Undocumented member.
msCodecProfile :: Lens' Mpeg2Settings (Maybe Mpeg2CodecProfile)
msCodecProfile = lens _msCodecProfile (\ s a -> s{_msCodecProfile = a})

-- | Average bitrate in bits/second. Required for VBR, CBR, and ABR. Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
msBitrate :: Lens' Mpeg2Settings (Maybe Int)
msBitrate = lens _msBitrate (\ s a -> s{_msBitrate = a})

-- | Framerate denominator.
msFramerateDenominator :: Lens' Mpeg2Settings (Maybe Int)
msFramerateDenominator = lens _msFramerateDenominator (\ s a -> s{_msFramerateDenominator = a})

-- | Undocumented member.
msFramerateConversionAlgorithm :: Lens' Mpeg2Settings (Maybe Mpeg2FramerateConversionAlgorithm)
msFramerateConversionAlgorithm = lens _msFramerateConversionAlgorithm (\ s a -> s{_msFramerateConversionAlgorithm = a})

-- | Undocumented member.
msCodecLevel :: Lens' Mpeg2Settings (Maybe Mpeg2CodecLevel)
msCodecLevel = lens _msCodecLevel (\ s a -> s{_msCodecLevel = a})

-- | Undocumented member.
msFramerateControl :: Lens' Mpeg2Settings (Maybe Mpeg2FramerateControl)
msFramerateControl = lens _msFramerateControl (\ s a -> s{_msFramerateControl = a})

-- | Undocumented member.
msAdaptiveQuantization :: Lens' Mpeg2Settings (Maybe Mpeg2AdaptiveQuantization)
msAdaptiveQuantization = lens _msAdaptiveQuantization (\ s a -> s{_msAdaptiveQuantization = a})

-- | Framerate numerator - framerate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
msFramerateNumerator :: Lens' Mpeg2Settings (Maybe Int)
msFramerateNumerator = lens _msFramerateNumerator (\ s a -> s{_msFramerateNumerator = a})

-- | Maximum bitrate in bits/second (for VBR mode only). Five megabits can be entered as 5000000 or 5m. Five hundred kilobits can be entered as 500000 or 0.5m.
msMaxBitrate :: Lens' Mpeg2Settings (Maybe Int)
msMaxBitrate = lens _msMaxBitrate (\ s a -> s{_msMaxBitrate = a})

-- | Undocumented member.
msSyntax :: Lens' Mpeg2Settings (Maybe Mpeg2Syntax)
msSyntax = lens _msSyntax (\ s a -> s{_msSyntax = a})

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
msGopClosedCadence :: Lens' Mpeg2Settings (Maybe Int)
msGopClosedCadence = lens _msGopClosedCadence (\ s a -> s{_msGopClosedCadence = a})

-- | Pixel Aspect Ratio denominator.
msParDenominator :: Lens' Mpeg2Settings (Maybe Int)
msParDenominator = lens _msParDenominator (\ s a -> s{_msParDenominator = a})

-- | Undocumented member.
msSpatialAdaptiveQuantization :: Lens' Mpeg2Settings (Maybe Mpeg2SpatialAdaptiveQuantization)
msSpatialAdaptiveQuantization = lens _msSpatialAdaptiveQuantization (\ s a -> s{_msSpatialAdaptiveQuantization = a})

instance FromJSON Mpeg2Settings where
        parseJSON
          = withObject "Mpeg2Settings"
              (\ x ->
                 Mpeg2Settings' <$>
                   (x .:? "qualityTuningLevel") <*>
                     (x .:? "temporalAdaptiveQuantization")
                     <*> (x .:? "sceneChangeDetect")
                     <*> (x .:? "hrdBufferInitialFillPercentage")
                     <*> (x .:? "slowPal")
                     <*> (x .:? "parNumerator")
                     <*> (x .:? "gopSize")
                     <*> (x .:? "numberBFramesBetweenReferenceFrames")
                     <*> (x .:? "gopSizeUnits")
                     <*> (x .:? "hrdBufferSize")
                     <*> (x .:? "rateControlMode")
                     <*> (x .:? "telecine")
                     <*> (x .:? "intraDcPrecision")
                     <*> (x .:? "minIInterval")
                     <*> (x .:? "interlaceMode")
                     <*> (x .:? "parControl")
                     <*> (x .:? "softness")
                     <*> (x .:? "codecProfile")
                     <*> (x .:? "bitrate")
                     <*> (x .:? "framerateDenominator")
                     <*> (x .:? "framerateConversionAlgorithm")
                     <*> (x .:? "codecLevel")
                     <*> (x .:? "framerateControl")
                     <*> (x .:? "adaptiveQuantization")
                     <*> (x .:? "framerateNumerator")
                     <*> (x .:? "maxBitrate")
                     <*> (x .:? "syntax")
                     <*> (x .:? "gopClosedCadence")
                     <*> (x .:? "parDenominator")
                     <*> (x .:? "spatialAdaptiveQuantization"))

instance Hashable Mpeg2Settings where

instance NFData Mpeg2Settings where

instance ToJSON Mpeg2Settings where
        toJSON Mpeg2Settings'{..}
          = object
              (catMaybes
                 [("qualityTuningLevel" .=) <$> _msQualityTuningLevel,
                  ("temporalAdaptiveQuantization" .=) <$>
                    _msTemporalAdaptiveQuantization,
                  ("sceneChangeDetect" .=) <$> _msSceneChangeDetect,
                  ("hrdBufferInitialFillPercentage" .=) <$>
                    _msHrdBufferInitialFillPercentage,
                  ("slowPal" .=) <$> _msSlowPal,
                  ("parNumerator" .=) <$> _msParNumerator,
                  ("gopSize" .=) <$> _msGopSize,
                  ("numberBFramesBetweenReferenceFrames" .=) <$>
                    _msNumberBFramesBetweenReferenceFrames,
                  ("gopSizeUnits" .=) <$> _msGopSizeUnits,
                  ("hrdBufferSize" .=) <$> _msHrdBufferSize,
                  ("rateControlMode" .=) <$> _msRateControlMode,
                  ("telecine" .=) <$> _msTelecine,
                  ("intraDcPrecision" .=) <$> _msIntraDcPrecision,
                  ("minIInterval" .=) <$> _msMinIInterval,
                  ("interlaceMode" .=) <$> _msInterlaceMode,
                  ("parControl" .=) <$> _msParControl,
                  ("softness" .=) <$> _msSoftness,
                  ("codecProfile" .=) <$> _msCodecProfile,
                  ("bitrate" .=) <$> _msBitrate,
                  ("framerateDenominator" .=) <$>
                    _msFramerateDenominator,
                  ("framerateConversionAlgorithm" .=) <$>
                    _msFramerateConversionAlgorithm,
                  ("codecLevel" .=) <$> _msCodecLevel,
                  ("framerateControl" .=) <$> _msFramerateControl,
                  ("adaptiveQuantization" .=) <$>
                    _msAdaptiveQuantization,
                  ("framerateNumerator" .=) <$> _msFramerateNumerator,
                  ("maxBitrate" .=) <$> _msMaxBitrate,
                  ("syntax" .=) <$> _msSyntax,
                  ("gopClosedCadence" .=) <$> _msGopClosedCadence,
                  ("parDenominator" .=) <$> _msParDenominator,
                  ("spatialAdaptiveQuantization" .=) <$>
                    _msSpatialAdaptiveQuantization])

-- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to specify the value SpekeKeyProvider.
--
-- /See:/ 'msSmoothEncryptionSettings' smart constructor.
newtype MsSmoothEncryptionSettings = MsSmoothEncryptionSettings'
  { _msesSpekeKeyProvider :: Maybe SpekeKeyProvider
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MsSmoothEncryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msesSpekeKeyProvider' - Undocumented member.
msSmoothEncryptionSettings
    :: MsSmoothEncryptionSettings
msSmoothEncryptionSettings =
  MsSmoothEncryptionSettings' {_msesSpekeKeyProvider = Nothing}


-- | Undocumented member.
msesSpekeKeyProvider :: Lens' MsSmoothEncryptionSettings (Maybe SpekeKeyProvider)
msesSpekeKeyProvider = lens _msesSpekeKeyProvider (\ s a -> s{_msesSpekeKeyProvider = a})

instance FromJSON MsSmoothEncryptionSettings where
        parseJSON
          = withObject "MsSmoothEncryptionSettings"
              (\ x ->
                 MsSmoothEncryptionSettings' <$>
                   (x .:? "spekeKeyProvider"))

instance Hashable MsSmoothEncryptionSettings where

instance NFData MsSmoothEncryptionSettings where

instance ToJSON MsSmoothEncryptionSettings where
        toJSON MsSmoothEncryptionSettings'{..}
          = object
              (catMaybes
                 [("spekeKeyProvider" .=) <$> _msesSpekeKeyProvider])

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to MS_SMOOTH_GROUP_SETTINGS.
--
-- /See:/ 'msSmoothGroupSettings' smart constructor.
data MsSmoothGroupSettings = MsSmoothGroupSettings'
  { _msgsFragmentLength     :: !(Maybe Int)
  , _msgsManifestEncoding   :: !(Maybe MsSmoothManifestEncoding)
  , _msgsDestination        :: !(Maybe Text)
  , _msgsAudioDeduplication :: !(Maybe MsSmoothAudioDeduplication)
  , _msgsEncryption         :: !(Maybe MsSmoothEncryptionSettings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MsSmoothGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msgsFragmentLength' - Use Fragment length (FragmentLength) to specify the mp4 fragment sizes in seconds. Fragment length must be compatible with GOP size and framerate.
--
-- * 'msgsManifestEncoding' - Undocumented member.
--
-- * 'msgsDestination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- * 'msgsAudioDeduplication' - Undocumented member.
--
-- * 'msgsEncryption' - Undocumented member.
msSmoothGroupSettings
    :: MsSmoothGroupSettings
msSmoothGroupSettings =
  MsSmoothGroupSettings'
    { _msgsFragmentLength = Nothing
    , _msgsManifestEncoding = Nothing
    , _msgsDestination = Nothing
    , _msgsAudioDeduplication = Nothing
    , _msgsEncryption = Nothing
    }


-- | Use Fragment length (FragmentLength) to specify the mp4 fragment sizes in seconds. Fragment length must be compatible with GOP size and framerate.
msgsFragmentLength :: Lens' MsSmoothGroupSettings (Maybe Int)
msgsFragmentLength = lens _msgsFragmentLength (\ s a -> s{_msgsFragmentLength = a})

-- | Undocumented member.
msgsManifestEncoding :: Lens' MsSmoothGroupSettings (Maybe MsSmoothManifestEncoding)
msgsManifestEncoding = lens _msgsManifestEncoding (\ s a -> s{_msgsManifestEncoding = a})

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
msgsDestination :: Lens' MsSmoothGroupSettings (Maybe Text)
msgsDestination = lens _msgsDestination (\ s a -> s{_msgsDestination = a})

-- | Undocumented member.
msgsAudioDeduplication :: Lens' MsSmoothGroupSettings (Maybe MsSmoothAudioDeduplication)
msgsAudioDeduplication = lens _msgsAudioDeduplication (\ s a -> s{_msgsAudioDeduplication = a})

-- | Undocumented member.
msgsEncryption :: Lens' MsSmoothGroupSettings (Maybe MsSmoothEncryptionSettings)
msgsEncryption = lens _msgsEncryption (\ s a -> s{_msgsEncryption = a})

instance FromJSON MsSmoothGroupSettings where
        parseJSON
          = withObject "MsSmoothGroupSettings"
              (\ x ->
                 MsSmoothGroupSettings' <$>
                   (x .:? "fragmentLength") <*>
                     (x .:? "manifestEncoding")
                     <*> (x .:? "destination")
                     <*> (x .:? "audioDeduplication")
                     <*> (x .:? "encryption"))

instance Hashable MsSmoothGroupSettings where

instance NFData MsSmoothGroupSettings where

instance ToJSON MsSmoothGroupSettings where
        toJSON MsSmoothGroupSettings'{..}
          = object
              (catMaybes
                 [("fragmentLength" .=) <$> _msgsFragmentLength,
                  ("manifestEncoding" .=) <$> _msgsManifestEncoding,
                  ("destination" .=) <$> _msgsDestination,
                  ("audioDeduplication" .=) <$>
                    _msgsAudioDeduplication,
                  ("encryption" .=) <$> _msgsEncryption])

-- | Settings for Nielsen Configuration
--
-- /See:/ 'nielsenConfiguration' smart constructor.
data NielsenConfiguration = NielsenConfiguration'
  { _ncBreakoutCode  :: !(Maybe Int)
  , _ncDistributorId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NielsenConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncBreakoutCode' - Use Nielsen Configuration (NielsenConfiguration) to set the Nielsen measurement system breakout code. Supported values are 0, 3, 7, and 9.
--
-- * 'ncDistributorId' - Use Distributor ID (DistributorID) to specify the distributor ID that is assigned to your organization by Neilsen.
nielsenConfiguration
    :: NielsenConfiguration
nielsenConfiguration =
  NielsenConfiguration' {_ncBreakoutCode = Nothing, _ncDistributorId = Nothing}


-- | Use Nielsen Configuration (NielsenConfiguration) to set the Nielsen measurement system breakout code. Supported values are 0, 3, 7, and 9.
ncBreakoutCode :: Lens' NielsenConfiguration (Maybe Int)
ncBreakoutCode = lens _ncBreakoutCode (\ s a -> s{_ncBreakoutCode = a})

-- | Use Distributor ID (DistributorID) to specify the distributor ID that is assigned to your organization by Neilsen.
ncDistributorId :: Lens' NielsenConfiguration (Maybe Text)
ncDistributorId = lens _ncDistributorId (\ s a -> s{_ncDistributorId = a})

instance FromJSON NielsenConfiguration where
        parseJSON
          = withObject "NielsenConfiguration"
              (\ x ->
                 NielsenConfiguration' <$>
                   (x .:? "breakoutCode") <*> (x .:? "distributorId"))

instance Hashable NielsenConfiguration where

instance NFData NielsenConfiguration where

instance ToJSON NielsenConfiguration where
        toJSON NielsenConfiguration'{..}
          = object
              (catMaybes
                 [("breakoutCode" .=) <$> _ncBreakoutCode,
                  ("distributorId" .=) <$> _ncDistributorId])

-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default. When you enable Noise reducer (NoiseReducer), you must also select a value for Noise reducer filter (NoiseReducerFilter).
--
-- /See:/ 'noiseReducer' smart constructor.
data NoiseReducer = NoiseReducer'
  { _nrSpatialFilterSettings :: !(Maybe NoiseReducerSpatialFilterSettings)
  , _nrFilterSettings        :: !(Maybe NoiseReducerFilterSettings)
  , _nrFilter                :: !(Maybe NoiseReducerFilter)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NoiseReducer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nrSpatialFilterSettings' - Undocumented member.
--
-- * 'nrFilterSettings' - Undocumented member.
--
-- * 'nrFilter' - Undocumented member.
noiseReducer
    :: NoiseReducer
noiseReducer =
  NoiseReducer'
    { _nrSpatialFilterSettings = Nothing
    , _nrFilterSettings = Nothing
    , _nrFilter = Nothing
    }


-- | Undocumented member.
nrSpatialFilterSettings :: Lens' NoiseReducer (Maybe NoiseReducerSpatialFilterSettings)
nrSpatialFilterSettings = lens _nrSpatialFilterSettings (\ s a -> s{_nrSpatialFilterSettings = a})

-- | Undocumented member.
nrFilterSettings :: Lens' NoiseReducer (Maybe NoiseReducerFilterSettings)
nrFilterSettings = lens _nrFilterSettings (\ s a -> s{_nrFilterSettings = a})

-- | Undocumented member.
nrFilter :: Lens' NoiseReducer (Maybe NoiseReducerFilter)
nrFilter = lens _nrFilter (\ s a -> s{_nrFilter = a})

instance FromJSON NoiseReducer where
        parseJSON
          = withObject "NoiseReducer"
              (\ x ->
                 NoiseReducer' <$>
                   (x .:? "spatialFilterSettings") <*>
                     (x .:? "filterSettings")
                     <*> (x .:? "filter"))

instance Hashable NoiseReducer where

instance NFData NoiseReducer where

instance ToJSON NoiseReducer where
        toJSON NoiseReducer'{..}
          = object
              (catMaybes
                 [("spatialFilterSettings" .=) <$>
                    _nrSpatialFilterSettings,
                  ("filterSettings" .=) <$> _nrFilterSettings,
                  ("filter" .=) <$> _nrFilter])

-- | Settings for a noise reducer filter
--
-- /See:/ 'noiseReducerFilterSettings' smart constructor.
newtype NoiseReducerFilterSettings = NoiseReducerFilterSettings'
  { _nrfsStrength :: Maybe Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NoiseReducerFilterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nrfsStrength' - Relative strength of noise reducing filter. Higher values produce stronger filtering.
noiseReducerFilterSettings
    :: NoiseReducerFilterSettings
noiseReducerFilterSettings =
  NoiseReducerFilterSettings' {_nrfsStrength = Nothing}


-- | Relative strength of noise reducing filter. Higher values produce stronger filtering.
nrfsStrength :: Lens' NoiseReducerFilterSettings (Maybe Int)
nrfsStrength = lens _nrfsStrength (\ s a -> s{_nrfsStrength = a})

instance FromJSON NoiseReducerFilterSettings where
        parseJSON
          = withObject "NoiseReducerFilterSettings"
              (\ x ->
                 NoiseReducerFilterSettings' <$> (x .:? "strength"))

instance Hashable NoiseReducerFilterSettings where

instance NFData NoiseReducerFilterSettings where

instance ToJSON NoiseReducerFilterSettings where
        toJSON NoiseReducerFilterSettings'{..}
          = object
              (catMaybes [("strength" .=) <$> _nrfsStrength])

-- | Noise reducer filter settings for spatial filter.
--
-- /See:/ 'noiseReducerSpatialFilterSettings' smart constructor.
data NoiseReducerSpatialFilterSettings = NoiseReducerSpatialFilterSettings'
  { _nrsfsStrength                  :: !(Maybe Int)
  , _nrsfsPostFilterSharpenStrength :: !(Maybe Int)
  , _nrsfsSpeed                     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NoiseReducerSpatialFilterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nrsfsStrength' - Relative strength of noise reducing filter. Higher values produce stronger filtering.
--
-- * 'nrsfsPostFilterSharpenStrength' - Specify strength of post noise reduction sharpening filter, with 0 disabling the filter and 3 enabling it at maximum strength.
--
-- * 'nrsfsSpeed' - The speed of the filter, from -2 (lower speed) to 3 (higher speed), with 0 being the nominal value.
noiseReducerSpatialFilterSettings
    :: NoiseReducerSpatialFilterSettings
noiseReducerSpatialFilterSettings =
  NoiseReducerSpatialFilterSettings'
    { _nrsfsStrength = Nothing
    , _nrsfsPostFilterSharpenStrength = Nothing
    , _nrsfsSpeed = Nothing
    }


-- | Relative strength of noise reducing filter. Higher values produce stronger filtering.
nrsfsStrength :: Lens' NoiseReducerSpatialFilterSettings (Maybe Int)
nrsfsStrength = lens _nrsfsStrength (\ s a -> s{_nrsfsStrength = a})

-- | Specify strength of post noise reduction sharpening filter, with 0 disabling the filter and 3 enabling it at maximum strength.
nrsfsPostFilterSharpenStrength :: Lens' NoiseReducerSpatialFilterSettings (Maybe Int)
nrsfsPostFilterSharpenStrength = lens _nrsfsPostFilterSharpenStrength (\ s a -> s{_nrsfsPostFilterSharpenStrength = a})

-- | The speed of the filter, from -2 (lower speed) to 3 (higher speed), with 0 being the nominal value.
nrsfsSpeed :: Lens' NoiseReducerSpatialFilterSettings (Maybe Int)
nrsfsSpeed = lens _nrsfsSpeed (\ s a -> s{_nrsfsSpeed = a})

instance FromJSON NoiseReducerSpatialFilterSettings
         where
        parseJSON
          = withObject "NoiseReducerSpatialFilterSettings"
              (\ x ->
                 NoiseReducerSpatialFilterSettings' <$>
                   (x .:? "strength") <*>
                     (x .:? "postFilterSharpenStrength")
                     <*> (x .:? "speed"))

instance Hashable NoiseReducerSpatialFilterSettings
         where

instance NFData NoiseReducerSpatialFilterSettings
         where

instance ToJSON NoiseReducerSpatialFilterSettings
         where
        toJSON NoiseReducerSpatialFilterSettings'{..}
          = object
              (catMaybes
                 [("strength" .=) <$> _nrsfsStrength,
                  ("postFilterSharpenStrength" .=) <$>
                    _nrsfsPostFilterSharpenStrength,
                  ("speed" .=) <$> _nrsfsSpeed])

-- | An output object describes the settings for a single output file or stream in an output group.
--
-- /See:/ 'output' smart constructor.
data Output = Output'
  { _oCaptionDescriptions :: !(Maybe [CaptionDescription])
  , _oExtension           :: !(Maybe Text)
  , _oVideoDescription    :: !(Maybe VideoDescription)
  , _oContainerSettings   :: !(Maybe ContainerSettings)
  , _oOutputSettings      :: !(Maybe OutputSettings)
  , _oPreset              :: !(Maybe Text)
  , _oNameModifier        :: !(Maybe Text)
  , _oAudioDescriptions   :: !(Maybe [AudioDescription])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Output' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oCaptionDescriptions' - (CaptionDescriptions) contains groups of captions settings. For each output that has captions, include one instance of (CaptionDescriptions). (CaptionDescriptions) can contain multiple groups of captions settings.
--
-- * 'oExtension' - Use Extension (Extension) to specify the file extension for outputs in File output groups. If you do not specify a value, the service will use default extensions by container type as follows * MPEG-2 transport stream, m2ts * Quicktime, mov * MXF container, mxf * MPEG-4 container, mp4 * No Container, the service will use codec extensions (e.g. AAC, H265, H265, AC3)
--
-- * 'oVideoDescription' - (VideoDescription) contains a group of video encoding settings. The specific video settings depend on the video codec you choose when you specify a value for Video codec (codec). Include one instance of (VideoDescription) per output.
--
-- * 'oContainerSettings' - Undocumented member.
--
-- * 'oOutputSettings' - Undocumented member.
--
-- * 'oPreset' - Use Preset (Preset) to specifiy a preset for your transcoding settings. Provide the system or custom preset name. You can specify either Preset (Preset) or Container settings (ContainerSettings), but not both.
--
-- * 'oNameModifier' - Use Name modifier (NameModifier) to have the service add a string to the end of each output filename. You specify the base filename as part of your destination URI. When you create multiple outputs in the same output group, Name modifier (NameModifier) is required. Name modifier also accepts format identifiers. For DASH ISO outputs, if you use the format identifiers $Number$ or $Time$ in one output, you must use them in the same way in all outputs of the output group.
--
-- * 'oAudioDescriptions' - (AudioDescriptions) contains groups of audio encoding settings organized by audio codec. Include one instance of (AudioDescriptions) per output. (AudioDescriptions) can contain multiple groups of encoding settings.
output
    :: Output
output =
  Output'
    { _oCaptionDescriptions = Nothing
    , _oExtension = Nothing
    , _oVideoDescription = Nothing
    , _oContainerSettings = Nothing
    , _oOutputSettings = Nothing
    , _oPreset = Nothing
    , _oNameModifier = Nothing
    , _oAudioDescriptions = Nothing
    }


-- | (CaptionDescriptions) contains groups of captions settings. For each output that has captions, include one instance of (CaptionDescriptions). (CaptionDescriptions) can contain multiple groups of captions settings.
oCaptionDescriptions :: Lens' Output [CaptionDescription]
oCaptionDescriptions = lens _oCaptionDescriptions (\ s a -> s{_oCaptionDescriptions = a}) . _Default . _Coerce

-- | Use Extension (Extension) to specify the file extension for outputs in File output groups. If you do not specify a value, the service will use default extensions by container type as follows * MPEG-2 transport stream, m2ts * Quicktime, mov * MXF container, mxf * MPEG-4 container, mp4 * No Container, the service will use codec extensions (e.g. AAC, H265, H265, AC3)
oExtension :: Lens' Output (Maybe Text)
oExtension = lens _oExtension (\ s a -> s{_oExtension = a})

-- | (VideoDescription) contains a group of video encoding settings. The specific video settings depend on the video codec you choose when you specify a value for Video codec (codec). Include one instance of (VideoDescription) per output.
oVideoDescription :: Lens' Output (Maybe VideoDescription)
oVideoDescription = lens _oVideoDescription (\ s a -> s{_oVideoDescription = a})

-- | Undocumented member.
oContainerSettings :: Lens' Output (Maybe ContainerSettings)
oContainerSettings = lens _oContainerSettings (\ s a -> s{_oContainerSettings = a})

-- | Undocumented member.
oOutputSettings :: Lens' Output (Maybe OutputSettings)
oOutputSettings = lens _oOutputSettings (\ s a -> s{_oOutputSettings = a})

-- | Use Preset (Preset) to specifiy a preset for your transcoding settings. Provide the system or custom preset name. You can specify either Preset (Preset) or Container settings (ContainerSettings), but not both.
oPreset :: Lens' Output (Maybe Text)
oPreset = lens _oPreset (\ s a -> s{_oPreset = a})

-- | Use Name modifier (NameModifier) to have the service add a string to the end of each output filename. You specify the base filename as part of your destination URI. When you create multiple outputs in the same output group, Name modifier (NameModifier) is required. Name modifier also accepts format identifiers. For DASH ISO outputs, if you use the format identifiers $Number$ or $Time$ in one output, you must use them in the same way in all outputs of the output group.
oNameModifier :: Lens' Output (Maybe Text)
oNameModifier = lens _oNameModifier (\ s a -> s{_oNameModifier = a})

-- | (AudioDescriptions) contains groups of audio encoding settings organized by audio codec. Include one instance of (AudioDescriptions) per output. (AudioDescriptions) can contain multiple groups of encoding settings.
oAudioDescriptions :: Lens' Output [AudioDescription]
oAudioDescriptions = lens _oAudioDescriptions (\ s a -> s{_oAudioDescriptions = a}) . _Default . _Coerce

instance FromJSON Output where
        parseJSON
          = withObject "Output"
              (\ x ->
                 Output' <$>
                   (x .:? "captionDescriptions" .!= mempty) <*>
                     (x .:? "extension")
                     <*> (x .:? "videoDescription")
                     <*> (x .:? "containerSettings")
                     <*> (x .:? "outputSettings")
                     <*> (x .:? "preset")
                     <*> (x .:? "nameModifier")
                     <*> (x .:? "audioDescriptions" .!= mempty))

instance Hashable Output where

instance NFData Output where

instance ToJSON Output where
        toJSON Output'{..}
          = object
              (catMaybes
                 [("captionDescriptions" .=) <$>
                    _oCaptionDescriptions,
                  ("extension" .=) <$> _oExtension,
                  ("videoDescription" .=) <$> _oVideoDescription,
                  ("containerSettings" .=) <$> _oContainerSettings,
                  ("outputSettings" .=) <$> _oOutputSettings,
                  ("preset" .=) <$> _oPreset,
                  ("nameModifier" .=) <$> _oNameModifier,
                  ("audioDescriptions" .=) <$> _oAudioDescriptions])

-- | OutputChannel mapping settings.
--
-- /See:/ 'outputChannelMapping' smart constructor.
newtype OutputChannelMapping = OutputChannelMapping'
  { _ocmInputChannels :: Maybe [Int]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputChannelMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocmInputChannels' - List of input channels
outputChannelMapping
    :: OutputChannelMapping
outputChannelMapping = OutputChannelMapping' {_ocmInputChannels = Nothing}


-- | List of input channels
ocmInputChannels :: Lens' OutputChannelMapping [Int]
ocmInputChannels = lens _ocmInputChannels (\ s a -> s{_ocmInputChannels = a}) . _Default . _Coerce

instance FromJSON OutputChannelMapping where
        parseJSON
          = withObject "OutputChannelMapping"
              (\ x ->
                 OutputChannelMapping' <$>
                   (x .:? "inputChannels" .!= mempty))

instance Hashable OutputChannelMapping where

instance NFData OutputChannelMapping where

instance ToJSON OutputChannelMapping where
        toJSON OutputChannelMapping'{..}
          = object
              (catMaybes
                 [("inputChannels" .=) <$> _ocmInputChannels])

-- | Details regarding output
--
-- /See:/ 'outputDetail' smart constructor.
data OutputDetail = OutputDetail'
  { _odVideoDetails :: !(Maybe VideoDetail)
  , _odDurationInMs :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odVideoDetails' - Undocumented member.
--
-- * 'odDurationInMs' - Duration in milliseconds
outputDetail
    :: OutputDetail
outputDetail =
  OutputDetail' {_odVideoDetails = Nothing, _odDurationInMs = Nothing}


-- | Undocumented member.
odVideoDetails :: Lens' OutputDetail (Maybe VideoDetail)
odVideoDetails = lens _odVideoDetails (\ s a -> s{_odVideoDetails = a})

-- | Duration in milliseconds
odDurationInMs :: Lens' OutputDetail (Maybe Int)
odDurationInMs = lens _odDurationInMs (\ s a -> s{_odDurationInMs = a})

instance FromJSON OutputDetail where
        parseJSON
          = withObject "OutputDetail"
              (\ x ->
                 OutputDetail' <$>
                   (x .:? "videoDetails") <*> (x .:? "durationInMs"))

instance Hashable OutputDetail where

instance NFData OutputDetail where

-- | Group of outputs
--
-- /See:/ 'outputGroup' smart constructor.
data OutputGroup = OutputGroup'
  { _ogOutputGroupSettings :: !(Maybe OutputGroupSettings)
  , _ogOutputs             :: !(Maybe [Output])
  , _ogCustomName          :: !(Maybe Text)
  , _ogName                :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogOutputGroupSettings' - Undocumented member.
--
-- * 'ogOutputs' - This object holds groups of encoding settings, one group of settings per output.
--
-- * 'ogCustomName' - Use Custom Group Name (CustomName) to specify a name for the output group. This value is displayed on the console and can make your job settings JSON more human-readable. It does not affect your outputs. Use up to twelve characters that are either letters, numbers, spaces, or underscores.
--
-- * 'ogName' - Name of the output group
outputGroup
    :: OutputGroup
outputGroup =
  OutputGroup'
    { _ogOutputGroupSettings = Nothing
    , _ogOutputs = Nothing
    , _ogCustomName = Nothing
    , _ogName = Nothing
    }


-- | Undocumented member.
ogOutputGroupSettings :: Lens' OutputGroup (Maybe OutputGroupSettings)
ogOutputGroupSettings = lens _ogOutputGroupSettings (\ s a -> s{_ogOutputGroupSettings = a})

-- | This object holds groups of encoding settings, one group of settings per output.
ogOutputs :: Lens' OutputGroup [Output]
ogOutputs = lens _ogOutputs (\ s a -> s{_ogOutputs = a}) . _Default . _Coerce

-- | Use Custom Group Name (CustomName) to specify a name for the output group. This value is displayed on the console and can make your job settings JSON more human-readable. It does not affect your outputs. Use up to twelve characters that are either letters, numbers, spaces, or underscores.
ogCustomName :: Lens' OutputGroup (Maybe Text)
ogCustomName = lens _ogCustomName (\ s a -> s{_ogCustomName = a})

-- | Name of the output group
ogName :: Lens' OutputGroup (Maybe Text)
ogName = lens _ogName (\ s a -> s{_ogName = a})

instance FromJSON OutputGroup where
        parseJSON
          = withObject "OutputGroup"
              (\ x ->
                 OutputGroup' <$>
                   (x .:? "outputGroupSettings") <*>
                     (x .:? "outputs" .!= mempty)
                     <*> (x .:? "customName")
                     <*> (x .:? "name"))

instance Hashable OutputGroup where

instance NFData OutputGroup where

instance ToJSON OutputGroup where
        toJSON OutputGroup'{..}
          = object
              (catMaybes
                 [("outputGroupSettings" .=) <$>
                    _ogOutputGroupSettings,
                  ("outputs" .=) <$> _ogOutputs,
                  ("customName" .=) <$> _ogCustomName,
                  ("name" .=) <$> _ogName])

-- | Contains details about the output groups specified in the job settings.
--
-- /See:/ 'outputGroupDetail' smart constructor.
newtype OutputGroupDetail = OutputGroupDetail'
  { _ogdOutputDetails :: Maybe [OutputDetail]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputGroupDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogdOutputDetails' - Details about the output
outputGroupDetail
    :: OutputGroupDetail
outputGroupDetail = OutputGroupDetail' {_ogdOutputDetails = Nothing}


-- | Details about the output
ogdOutputDetails :: Lens' OutputGroupDetail [OutputDetail]
ogdOutputDetails = lens _ogdOutputDetails (\ s a -> s{_ogdOutputDetails = a}) . _Default . _Coerce

instance FromJSON OutputGroupDetail where
        parseJSON
          = withObject "OutputGroupDetail"
              (\ x ->
                 OutputGroupDetail' <$>
                   (x .:? "outputDetails" .!= mempty))

instance Hashable OutputGroupDetail where

instance NFData OutputGroupDetail where

-- | Output Group settings, including type
--
-- /See:/ 'outputGroupSettings' smart constructor.
data OutputGroupSettings = OutputGroupSettings'
  { _ogsFileGroupSettings     :: !(Maybe FileGroupSettings)
  , _ogsMsSmoothGroupSettings :: !(Maybe MsSmoothGroupSettings)
  , _ogsHlsGroupSettings      :: !(Maybe HlsGroupSettings)
  , _ogsType                  :: !(Maybe OutputGroupType)
  , _ogsDashIsoGroupSettings  :: !(Maybe DashIsoGroupSettings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogsFileGroupSettings' - Undocumented member.
--
-- * 'ogsMsSmoothGroupSettings' - Undocumented member.
--
-- * 'ogsHlsGroupSettings' - Undocumented member.
--
-- * 'ogsType' - Undocumented member.
--
-- * 'ogsDashIsoGroupSettings' - Undocumented member.
outputGroupSettings
    :: OutputGroupSettings
outputGroupSettings =
  OutputGroupSettings'
    { _ogsFileGroupSettings = Nothing
    , _ogsMsSmoothGroupSettings = Nothing
    , _ogsHlsGroupSettings = Nothing
    , _ogsType = Nothing
    , _ogsDashIsoGroupSettings = Nothing
    }


-- | Undocumented member.
ogsFileGroupSettings :: Lens' OutputGroupSettings (Maybe FileGroupSettings)
ogsFileGroupSettings = lens _ogsFileGroupSettings (\ s a -> s{_ogsFileGroupSettings = a})

-- | Undocumented member.
ogsMsSmoothGroupSettings :: Lens' OutputGroupSettings (Maybe MsSmoothGroupSettings)
ogsMsSmoothGroupSettings = lens _ogsMsSmoothGroupSettings (\ s a -> s{_ogsMsSmoothGroupSettings = a})

-- | Undocumented member.
ogsHlsGroupSettings :: Lens' OutputGroupSettings (Maybe HlsGroupSettings)
ogsHlsGroupSettings = lens _ogsHlsGroupSettings (\ s a -> s{_ogsHlsGroupSettings = a})

-- | Undocumented member.
ogsType :: Lens' OutputGroupSettings (Maybe OutputGroupType)
ogsType = lens _ogsType (\ s a -> s{_ogsType = a})

-- | Undocumented member.
ogsDashIsoGroupSettings :: Lens' OutputGroupSettings (Maybe DashIsoGroupSettings)
ogsDashIsoGroupSettings = lens _ogsDashIsoGroupSettings (\ s a -> s{_ogsDashIsoGroupSettings = a})

instance FromJSON OutputGroupSettings where
        parseJSON
          = withObject "OutputGroupSettings"
              (\ x ->
                 OutputGroupSettings' <$>
                   (x .:? "fileGroupSettings") <*>
                     (x .:? "msSmoothGroupSettings")
                     <*> (x .:? "hlsGroupSettings")
                     <*> (x .:? "type")
                     <*> (x .:? "dashIsoGroupSettings"))

instance Hashable OutputGroupSettings where

instance NFData OutputGroupSettings where

instance ToJSON OutputGroupSettings where
        toJSON OutputGroupSettings'{..}
          = object
              (catMaybes
                 [("fileGroupSettings" .=) <$> _ogsFileGroupSettings,
                  ("msSmoothGroupSettings" .=) <$>
                    _ogsMsSmoothGroupSettings,
                  ("hlsGroupSettings" .=) <$> _ogsHlsGroupSettings,
                  ("type" .=) <$> _ogsType,
                  ("dashIsoGroupSettings" .=) <$>
                    _ogsDashIsoGroupSettings])

-- | Specific settings for this type of output.
--
-- /See:/ 'outputSettings' smart constructor.
newtype OutputSettings = OutputSettings'
  { _osHlsSettings :: Maybe HlsSettings
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osHlsSettings' - Undocumented member.
outputSettings
    :: OutputSettings
outputSettings = OutputSettings' {_osHlsSettings = Nothing}


-- | Undocumented member.
osHlsSettings :: Lens' OutputSettings (Maybe HlsSettings)
osHlsSettings = lens _osHlsSettings (\ s a -> s{_osHlsSettings = a})

instance FromJSON OutputSettings where
        parseJSON
          = withObject "OutputSettings"
              (\ x -> OutputSettings' <$> (x .:? "hlsSettings"))

instance Hashable OutputSettings where

instance NFData OutputSettings where

instance ToJSON OutputSettings where
        toJSON OutputSettings'{..}
          = object
              (catMaybes [("hlsSettings" .=) <$> _osHlsSettings])

-- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
--
-- /See:/ 'preset' smart constructor.
data Preset = Preset'
  { _pLastUpdated :: !(Maybe POSIX)
  , _pSettings    :: !(Maybe PresetSettings)
  , _pARN         :: !(Maybe Text)
  , _pCreatedAt   :: !(Maybe POSIX)
  , _pCategory    :: !(Maybe Text)
  , _pName        :: !(Maybe Text)
  , _pType        :: !(Maybe Type)
  , _pDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Preset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pLastUpdated' - The timestamp in epoch seconds when the preset was last updated.
--
-- * 'pSettings' - Undocumented member.
--
-- * 'pARN' - An identifier for this resource that is unique within all of AWS.
--
-- * 'pCreatedAt' - The timestamp in epoch seconds for preset creation.
--
-- * 'pCategory' - An optional category you create to organize your presets.
--
-- * 'pName' - A name you create for each preset. Each name must be unique within your account.
--
-- * 'pType' - A preset can be of two types: system or custom. System or built-in preset can't be modified or deleted by the user.
--
-- * 'pDescription' - An optional description you create for each preset.
preset
    :: Preset
preset =
  Preset'
    { _pLastUpdated = Nothing
    , _pSettings = Nothing
    , _pARN = Nothing
    , _pCreatedAt = Nothing
    , _pCategory = Nothing
    , _pName = Nothing
    , _pType = Nothing
    , _pDescription = Nothing
    }


-- | The timestamp in epoch seconds when the preset was last updated.
pLastUpdated :: Lens' Preset (Maybe UTCTime)
pLastUpdated = lens _pLastUpdated (\ s a -> s{_pLastUpdated = a}) . mapping _Time

-- | Undocumented member.
pSettings :: Lens' Preset (Maybe PresetSettings)
pSettings = lens _pSettings (\ s a -> s{_pSettings = a})

-- | An identifier for this resource that is unique within all of AWS.
pARN :: Lens' Preset (Maybe Text)
pARN = lens _pARN (\ s a -> s{_pARN = a})

-- | The timestamp in epoch seconds for preset creation.
pCreatedAt :: Lens' Preset (Maybe UTCTime)
pCreatedAt = lens _pCreatedAt (\ s a -> s{_pCreatedAt = a}) . mapping _Time

-- | An optional category you create to organize your presets.
pCategory :: Lens' Preset (Maybe Text)
pCategory = lens _pCategory (\ s a -> s{_pCategory = a})

-- | A name you create for each preset. Each name must be unique within your account.
pName :: Lens' Preset (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a})

-- | A preset can be of two types: system or custom. System or built-in preset can't be modified or deleted by the user.
pType :: Lens' Preset (Maybe Type)
pType = lens _pType (\ s a -> s{_pType = a})

-- | An optional description you create for each preset.
pDescription :: Lens' Preset (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a})

instance FromJSON Preset where
        parseJSON
          = withObject "Preset"
              (\ x ->
                 Preset' <$>
                   (x .:? "lastUpdated") <*> (x .:? "settings") <*>
                     (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "category")
                     <*> (x .:? "name")
                     <*> (x .:? "type")
                     <*> (x .:? "description"))

instance Hashable Preset where

instance NFData Preset where

-- | Settings for preset
--
-- /See:/ 'presetSettings' smart constructor.
data PresetSettings = PresetSettings'
  { _psCaptionDescriptions :: !(Maybe [CaptionDescriptionPreset])
  , _psVideoDescription    :: !(Maybe VideoDescription)
  , _psContainerSettings   :: !(Maybe ContainerSettings)
  , _psAudioDescriptions   :: !(Maybe [AudioDescription])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PresetSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psCaptionDescriptions' - Caption settings for this preset. There can be multiple caption settings in a single output.
--
-- * 'psVideoDescription' - (VideoDescription) contains a group of video encoding settings. The specific video settings depend on the video codec you choose when you specify a value for Video codec (codec). Include one instance of (VideoDescription) per output.
--
-- * 'psContainerSettings' - Undocumented member.
--
-- * 'psAudioDescriptions' - (AudioDescriptions) contains groups of audio encoding settings organized by audio codec. Include one instance of (AudioDescriptions) per output. (AudioDescriptions) can contain multiple groups of encoding settings.
presetSettings
    :: PresetSettings
presetSettings =
  PresetSettings'
    { _psCaptionDescriptions = Nothing
    , _psVideoDescription = Nothing
    , _psContainerSettings = Nothing
    , _psAudioDescriptions = Nothing
    }


-- | Caption settings for this preset. There can be multiple caption settings in a single output.
psCaptionDescriptions :: Lens' PresetSettings [CaptionDescriptionPreset]
psCaptionDescriptions = lens _psCaptionDescriptions (\ s a -> s{_psCaptionDescriptions = a}) . _Default . _Coerce

-- | (VideoDescription) contains a group of video encoding settings. The specific video settings depend on the video codec you choose when you specify a value for Video codec (codec). Include one instance of (VideoDescription) per output.
psVideoDescription :: Lens' PresetSettings (Maybe VideoDescription)
psVideoDescription = lens _psVideoDescription (\ s a -> s{_psVideoDescription = a})

-- | Undocumented member.
psContainerSettings :: Lens' PresetSettings (Maybe ContainerSettings)
psContainerSettings = lens _psContainerSettings (\ s a -> s{_psContainerSettings = a})

-- | (AudioDescriptions) contains groups of audio encoding settings organized by audio codec. Include one instance of (AudioDescriptions) per output. (AudioDescriptions) can contain multiple groups of encoding settings.
psAudioDescriptions :: Lens' PresetSettings [AudioDescription]
psAudioDescriptions = lens _psAudioDescriptions (\ s a -> s{_psAudioDescriptions = a}) . _Default . _Coerce

instance FromJSON PresetSettings where
        parseJSON
          = withObject "PresetSettings"
              (\ x ->
                 PresetSettings' <$>
                   (x .:? "captionDescriptions" .!= mempty) <*>
                     (x .:? "videoDescription")
                     <*> (x .:? "containerSettings")
                     <*> (x .:? "audioDescriptions" .!= mempty))

instance Hashable PresetSettings where

instance NFData PresetSettings where

instance ToJSON PresetSettings where
        toJSON PresetSettings'{..}
          = object
              (catMaybes
                 [("captionDescriptions" .=) <$>
                    _psCaptionDescriptions,
                  ("videoDescription" .=) <$> _psVideoDescription,
                  ("containerSettings" .=) <$> _psContainerSettings,
                  ("audioDescriptions" .=) <$> _psAudioDescriptions])

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.
--
-- /See:/ 'proresSettings' smart constructor.
data ProresSettings = ProresSettings'
  { _psSlowPal :: !(Maybe ProresSlowPal)
  , _psParNumerator :: !(Maybe Int)
  , _psTelecine :: !(Maybe ProresTelecine)
  , _psInterlaceMode :: !(Maybe ProresInterlaceMode)
  , _psParControl :: !(Maybe ProresParControl)
  , _psCodecProfile :: !(Maybe ProresCodecProfile)
  , _psFramerateDenominator :: !(Maybe Int)
  , _psFramerateConversionAlgorithm :: !(Maybe ProresFramerateConversionAlgorithm)
  , _psFramerateControl :: !(Maybe ProresFramerateControl)
  , _psFramerateNumerator :: !(Maybe Int)
  , _psParDenominator :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProresSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psSlowPal' - Undocumented member.
--
-- * 'psParNumerator' - Pixel Aspect Ratio numerator.
--
-- * 'psTelecine' - Undocumented member.
--
-- * 'psInterlaceMode' - Undocumented member.
--
-- * 'psParControl' - Undocumented member.
--
-- * 'psCodecProfile' - Undocumented member.
--
-- * 'psFramerateDenominator' - Framerate denominator.
--
-- * 'psFramerateConversionAlgorithm' - Undocumented member.
--
-- * 'psFramerateControl' - Undocumented member.
--
-- * 'psFramerateNumerator' - When you use the API for transcode jobs that use framerate conversion, specify the framerate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator.
--
-- * 'psParDenominator' - Pixel Aspect Ratio denominator.
proresSettings
    :: ProresSettings
proresSettings =
  ProresSettings'
    { _psSlowPal = Nothing
    , _psParNumerator = Nothing
    , _psTelecine = Nothing
    , _psInterlaceMode = Nothing
    , _psParControl = Nothing
    , _psCodecProfile = Nothing
    , _psFramerateDenominator = Nothing
    , _psFramerateConversionAlgorithm = Nothing
    , _psFramerateControl = Nothing
    , _psFramerateNumerator = Nothing
    , _psParDenominator = Nothing
    }


-- | Undocumented member.
psSlowPal :: Lens' ProresSettings (Maybe ProresSlowPal)
psSlowPal = lens _psSlowPal (\ s a -> s{_psSlowPal = a})

-- | Pixel Aspect Ratio numerator.
psParNumerator :: Lens' ProresSettings (Maybe Int)
psParNumerator = lens _psParNumerator (\ s a -> s{_psParNumerator = a})

-- | Undocumented member.
psTelecine :: Lens' ProresSettings (Maybe ProresTelecine)
psTelecine = lens _psTelecine (\ s a -> s{_psTelecine = a})

-- | Undocumented member.
psInterlaceMode :: Lens' ProresSettings (Maybe ProresInterlaceMode)
psInterlaceMode = lens _psInterlaceMode (\ s a -> s{_psInterlaceMode = a})

-- | Undocumented member.
psParControl :: Lens' ProresSettings (Maybe ProresParControl)
psParControl = lens _psParControl (\ s a -> s{_psParControl = a})

-- | Undocumented member.
psCodecProfile :: Lens' ProresSettings (Maybe ProresCodecProfile)
psCodecProfile = lens _psCodecProfile (\ s a -> s{_psCodecProfile = a})

-- | Framerate denominator.
psFramerateDenominator :: Lens' ProresSettings (Maybe Int)
psFramerateDenominator = lens _psFramerateDenominator (\ s a -> s{_psFramerateDenominator = a})

-- | Undocumented member.
psFramerateConversionAlgorithm :: Lens' ProresSettings (Maybe ProresFramerateConversionAlgorithm)
psFramerateConversionAlgorithm = lens _psFramerateConversionAlgorithm (\ s a -> s{_psFramerateConversionAlgorithm = a})

-- | Undocumented member.
psFramerateControl :: Lens' ProresSettings (Maybe ProresFramerateControl)
psFramerateControl = lens _psFramerateControl (\ s a -> s{_psFramerateControl = a})

-- | When you use the API for transcode jobs that use framerate conversion, specify the framerate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator.
psFramerateNumerator :: Lens' ProresSettings (Maybe Int)
psFramerateNumerator = lens _psFramerateNumerator (\ s a -> s{_psFramerateNumerator = a})

-- | Pixel Aspect Ratio denominator.
psParDenominator :: Lens' ProresSettings (Maybe Int)
psParDenominator = lens _psParDenominator (\ s a -> s{_psParDenominator = a})

instance FromJSON ProresSettings where
        parseJSON
          = withObject "ProresSettings"
              (\ x ->
                 ProresSettings' <$>
                   (x .:? "slowPal") <*> (x .:? "parNumerator") <*>
                     (x .:? "telecine")
                     <*> (x .:? "interlaceMode")
                     <*> (x .:? "parControl")
                     <*> (x .:? "codecProfile")
                     <*> (x .:? "framerateDenominator")
                     <*> (x .:? "framerateConversionAlgorithm")
                     <*> (x .:? "framerateControl")
                     <*> (x .:? "framerateNumerator")
                     <*> (x .:? "parDenominator"))

instance Hashable ProresSettings where

instance NFData ProresSettings where

instance ToJSON ProresSettings where
        toJSON ProresSettings'{..}
          = object
              (catMaybes
                 [("slowPal" .=) <$> _psSlowPal,
                  ("parNumerator" .=) <$> _psParNumerator,
                  ("telecine" .=) <$> _psTelecine,
                  ("interlaceMode" .=) <$> _psInterlaceMode,
                  ("parControl" .=) <$> _psParControl,
                  ("codecProfile" .=) <$> _psCodecProfile,
                  ("framerateDenominator" .=) <$>
                    _psFramerateDenominator,
                  ("framerateConversionAlgorithm" .=) <$>
                    _psFramerateConversionAlgorithm,
                  ("framerateControl" .=) <$> _psFramerateControl,
                  ("framerateNumerator" .=) <$> _psFramerateNumerator,
                  ("parDenominator" .=) <$> _psParDenominator])

-- | MediaConvert jobs are submitted to a queue. Unless specified otherwise jobs are submitted to a built-in default queue. User can create additional queues to separate the jobs of different categories or priority.
--
-- /See:/ 'queue' smart constructor.
data Queue = Queue'
  { _qStatus      :: !(Maybe QueueStatus)
  , _qLastUpdated :: !(Maybe POSIX)
  , _qARN         :: !(Maybe Text)
  , _qCreatedAt   :: !(Maybe POSIX)
  , _qName        :: !(Maybe Text)
  , _qType        :: !(Maybe Type)
  , _qDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Queue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qStatus' - Undocumented member.
--
-- * 'qLastUpdated' - The timestamp in epoch seconds when the queue was last updated.
--
-- * 'qARN' - An identifier for this resource that is unique within all of AWS.
--
-- * 'qCreatedAt' - The timestamp in epoch seconds for queue creation.
--
-- * 'qName' - A name you create for each queue. Each name must be unique within your account.
--
-- * 'qType' - A queue can be of two types: system or custom. System or built-in queues can't be modified or deleted by the user.
--
-- * 'qDescription' - An optional description you create for each queue.
queue
    :: Queue
queue =
  Queue'
    { _qStatus = Nothing
    , _qLastUpdated = Nothing
    , _qARN = Nothing
    , _qCreatedAt = Nothing
    , _qName = Nothing
    , _qType = Nothing
    , _qDescription = Nothing
    }


-- | Undocumented member.
qStatus :: Lens' Queue (Maybe QueueStatus)
qStatus = lens _qStatus (\ s a -> s{_qStatus = a})

-- | The timestamp in epoch seconds when the queue was last updated.
qLastUpdated :: Lens' Queue (Maybe UTCTime)
qLastUpdated = lens _qLastUpdated (\ s a -> s{_qLastUpdated = a}) . mapping _Time

-- | An identifier for this resource that is unique within all of AWS.
qARN :: Lens' Queue (Maybe Text)
qARN = lens _qARN (\ s a -> s{_qARN = a})

-- | The timestamp in epoch seconds for queue creation.
qCreatedAt :: Lens' Queue (Maybe UTCTime)
qCreatedAt = lens _qCreatedAt (\ s a -> s{_qCreatedAt = a}) . mapping _Time

-- | A name you create for each queue. Each name must be unique within your account.
qName :: Lens' Queue (Maybe Text)
qName = lens _qName (\ s a -> s{_qName = a})

-- | A queue can be of two types: system or custom. System or built-in queues can't be modified or deleted by the user.
qType :: Lens' Queue (Maybe Type)
qType = lens _qType (\ s a -> s{_qType = a})

-- | An optional description you create for each queue.
qDescription :: Lens' Queue (Maybe Text)
qDescription = lens _qDescription (\ s a -> s{_qDescription = a})

instance FromJSON Queue where
        parseJSON
          = withObject "Queue"
              (\ x ->
                 Queue' <$>
                   (x .:? "status") <*> (x .:? "lastUpdated") <*>
                     (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "name")
                     <*> (x .:? "type")
                     <*> (x .:? "description"))

instance Hashable Queue where

instance NFData Queue where

-- | Use Rectangle to identify a specific area of the video frame.
--
-- /See:/ 'rectangle' smart constructor.
data Rectangle = Rectangle'
  { _rHeight :: !(Maybe Int)
  , _rWidth  :: !(Maybe Int)
  , _rX      :: !(Maybe Int)
  , _rY      :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Rectangle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rHeight' - Height of rectangle in pixels.
--
-- * 'rWidth' - Width of rectangle in pixels.
--
-- * 'rX' - The distance, in pixels, between the rectangle and the left edge of the video frame.
--
-- * 'rY' - The distance, in pixels, between the rectangle and the top edge of the video frame.
rectangle
    :: Rectangle
rectangle =
  Rectangle'
    {_rHeight = Nothing, _rWidth = Nothing, _rX = Nothing, _rY = Nothing}


-- | Height of rectangle in pixels.
rHeight :: Lens' Rectangle (Maybe Int)
rHeight = lens _rHeight (\ s a -> s{_rHeight = a})

-- | Width of rectangle in pixels.
rWidth :: Lens' Rectangle (Maybe Int)
rWidth = lens _rWidth (\ s a -> s{_rWidth = a})

-- | The distance, in pixels, between the rectangle and the left edge of the video frame.
rX :: Lens' Rectangle (Maybe Int)
rX = lens _rX (\ s a -> s{_rX = a})

-- | The distance, in pixels, between the rectangle and the top edge of the video frame.
rY :: Lens' Rectangle (Maybe Int)
rY = lens _rY (\ s a -> s{_rY = a})

instance FromJSON Rectangle where
        parseJSON
          = withObject "Rectangle"
              (\ x ->
                 Rectangle' <$>
                   (x .:? "height") <*> (x .:? "width") <*> (x .:? "x")
                     <*> (x .:? "y"))

instance Hashable Rectangle where

instance NFData Rectangle where

instance ToJSON Rectangle where
        toJSON Rectangle'{..}
          = object
              (catMaybes
                 [("height" .=) <$> _rHeight,
                  ("width" .=) <$> _rWidth, ("x" .=) <$> _rX,
                  ("y" .=) <$> _rY])

-- | Use Manual audio remixing (RemixSettings) to adjust audio levels for each output channel. With audio remixing, you can output more or fewer audio channels than your input audio source provides.
--
-- /See:/ 'remixSettings' smart constructor.
data RemixSettings = RemixSettings'
  { _rsChannelMapping :: !(Maybe ChannelMapping)
  , _rsChannelsIn     :: !(Maybe Int)
  , _rsChannelsOut    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemixSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsChannelMapping' - Undocumented member.
--
-- * 'rsChannelsIn' - Specify the number of audio channels from your input that you want to use in your output. With remixing, you might combine or split the data in these channels, so the number of channels in your final output might be different.
--
-- * 'rsChannelsOut' - Specify the number of channels in this output after remixing. Valid values: 1, 2, 4, 6, 8
remixSettings
    :: RemixSettings
remixSettings =
  RemixSettings'
    { _rsChannelMapping = Nothing
    , _rsChannelsIn = Nothing
    , _rsChannelsOut = Nothing
    }


-- | Undocumented member.
rsChannelMapping :: Lens' RemixSettings (Maybe ChannelMapping)
rsChannelMapping = lens _rsChannelMapping (\ s a -> s{_rsChannelMapping = a})

-- | Specify the number of audio channels from your input that you want to use in your output. With remixing, you might combine or split the data in these channels, so the number of channels in your final output might be different.
rsChannelsIn :: Lens' RemixSettings (Maybe Int)
rsChannelsIn = lens _rsChannelsIn (\ s a -> s{_rsChannelsIn = a})

-- | Specify the number of channels in this output after remixing. Valid values: 1, 2, 4, 6, 8
rsChannelsOut :: Lens' RemixSettings (Maybe Int)
rsChannelsOut = lens _rsChannelsOut (\ s a -> s{_rsChannelsOut = a})

instance FromJSON RemixSettings where
        parseJSON
          = withObject "RemixSettings"
              (\ x ->
                 RemixSettings' <$>
                   (x .:? "channelMapping") <*> (x .:? "channelsIn") <*>
                     (x .:? "channelsOut"))

instance Hashable RemixSettings where

instance NFData RemixSettings where

instance ToJSON RemixSettings where
        toJSON RemixSettings'{..}
          = object
              (catMaybes
                 [("channelMapping" .=) <$> _rsChannelMapping,
                  ("channelsIn" .=) <$> _rsChannelsIn,
                  ("channelsOut" .=) <$> _rsChannelsOut])

-- | Settings for SCC caption output.
--
-- /See:/ 'sccDestinationSettings' smart constructor.
newtype SccDestinationSettings = SccDestinationSettings'
  { _sdsFramerate :: Maybe SccDestinationFramerate
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SccDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsFramerate' - Undocumented member.
sccDestinationSettings
    :: SccDestinationSettings
sccDestinationSettings = SccDestinationSettings' {_sdsFramerate = Nothing}


-- | Undocumented member.
sdsFramerate :: Lens' SccDestinationSettings (Maybe SccDestinationFramerate)
sdsFramerate = lens _sdsFramerate (\ s a -> s{_sdsFramerate = a})

instance FromJSON SccDestinationSettings where
        parseJSON
          = withObject "SccDestinationSettings"
              (\ x ->
                 SccDestinationSettings' <$> (x .:? "framerate"))

instance Hashable SccDestinationSettings where

instance NFData SccDestinationSettings where

instance ToJSON SccDestinationSettings where
        toJSON SccDestinationSettings'{..}
          = object
              (catMaybes [("framerate" .=) <$> _sdsFramerate])

-- | Settings for use with a SPEKE key provider
--
-- /See:/ 'spekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { _skpResourceId :: !(Maybe Text)
  , _skpURL        :: !(Maybe Text)
  , _skpSystemIds  :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpekeKeyProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skpResourceId' - The SPEKE-compliant server uses Resource ID (ResourceId) to identify content.
--
-- * 'skpURL' - Use URL (Url) to specify the SPEKE-compliant server that will provide keys for content.
--
-- * 'skpSystemIds' - Relates to SPEKE implementation. DRM system identifiers. DASH output groups support a max of two system ids. Other group types support one system id.
spekeKeyProvider
    :: SpekeKeyProvider
spekeKeyProvider =
  SpekeKeyProvider'
    {_skpResourceId = Nothing, _skpURL = Nothing, _skpSystemIds = Nothing}


-- | The SPEKE-compliant server uses Resource ID (ResourceId) to identify content.
skpResourceId :: Lens' SpekeKeyProvider (Maybe Text)
skpResourceId = lens _skpResourceId (\ s a -> s{_skpResourceId = a})

-- | Use URL (Url) to specify the SPEKE-compliant server that will provide keys for content.
skpURL :: Lens' SpekeKeyProvider (Maybe Text)
skpURL = lens _skpURL (\ s a -> s{_skpURL = a})

-- | Relates to SPEKE implementation. DRM system identifiers. DASH output groups support a max of two system ids. Other group types support one system id.
skpSystemIds :: Lens' SpekeKeyProvider [Text]
skpSystemIds = lens _skpSystemIds (\ s a -> s{_skpSystemIds = a}) . _Default . _Coerce

instance FromJSON SpekeKeyProvider where
        parseJSON
          = withObject "SpekeKeyProvider"
              (\ x ->
                 SpekeKeyProvider' <$>
                   (x .:? "resourceId") <*> (x .:? "url") <*>
                     (x .:? "systemIds" .!= mempty))

instance Hashable SpekeKeyProvider where

instance NFData SpekeKeyProvider where

instance ToJSON SpekeKeyProvider where
        toJSON SpekeKeyProvider'{..}
          = object
              (catMaybes
                 [("resourceId" .=) <$> _skpResourceId,
                  ("url" .=) <$> _skpURL,
                  ("systemIds" .=) <$> _skpSystemIds])

-- | Settings for use with a SPEKE key provider.
--
-- /See:/ 'staticKeyProvider' smart constructor.
data StaticKeyProvider = StaticKeyProvider'
  { _sStaticKeyValue    :: !(Maybe Text)
  , _sURL               :: !(Maybe Text)
  , _sKeyFormat         :: !(Maybe Text)
  , _sKeyFormatVersions :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StaticKeyProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStaticKeyValue' - Relates to DRM implementation. Use a 32-character hexidecimal string to specify Key Value (StaticKeyValue).
--
-- * 'sURL' - Relates to DRM implementation. The location of the license server used for protecting content.
--
-- * 'sKeyFormat' - Relates to DRM implementation. Sets the value of the KEYFORMAT attribute. Must be 'identity' or a reverse DNS string. May be omitted to indicate an implicit value of 'identity'.
--
-- * 'sKeyFormatVersions' - Relates to DRM implementation. Either a single positive integer version value or a slash delimited list of version values (1/2/3).
staticKeyProvider
    :: StaticKeyProvider
staticKeyProvider =
  StaticKeyProvider'
    { _sStaticKeyValue = Nothing
    , _sURL = Nothing
    , _sKeyFormat = Nothing
    , _sKeyFormatVersions = Nothing
    }


-- | Relates to DRM implementation. Use a 32-character hexidecimal string to specify Key Value (StaticKeyValue).
sStaticKeyValue :: Lens' StaticKeyProvider (Maybe Text)
sStaticKeyValue = lens _sStaticKeyValue (\ s a -> s{_sStaticKeyValue = a})

-- | Relates to DRM implementation. The location of the license server used for protecting content.
sURL :: Lens' StaticKeyProvider (Maybe Text)
sURL = lens _sURL (\ s a -> s{_sURL = a})

-- | Relates to DRM implementation. Sets the value of the KEYFORMAT attribute. Must be 'identity' or a reverse DNS string. May be omitted to indicate an implicit value of 'identity'.
sKeyFormat :: Lens' StaticKeyProvider (Maybe Text)
sKeyFormat = lens _sKeyFormat (\ s a -> s{_sKeyFormat = a})

-- | Relates to DRM implementation. Either a single positive integer version value or a slash delimited list of version values (1/2/3).
sKeyFormatVersions :: Lens' StaticKeyProvider (Maybe Text)
sKeyFormatVersions = lens _sKeyFormatVersions (\ s a -> s{_sKeyFormatVersions = a})

instance FromJSON StaticKeyProvider where
        parseJSON
          = withObject "StaticKeyProvider"
              (\ x ->
                 StaticKeyProvider' <$>
                   (x .:? "staticKeyValue") <*> (x .:? "url") <*>
                     (x .:? "keyFormat")
                     <*> (x .:? "keyFormatVersions"))

instance Hashable StaticKeyProvider where

instance NFData StaticKeyProvider where

instance ToJSON StaticKeyProvider where
        toJSON StaticKeyProvider'{..}
          = object
              (catMaybes
                 [("staticKeyValue" .=) <$> _sStaticKeyValue,
                  ("url" .=) <$> _sURL,
                  ("keyFormat" .=) <$> _sKeyFormat,
                  ("keyFormatVersions" .=) <$> _sKeyFormatVersions])

-- | Settings for Teletext caption output
--
-- /See:/ 'teletextDestinationSettings' smart constructor.
newtype TeletextDestinationSettings = TeletextDestinationSettings'
  { _tdsPageNumber :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TeletextDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdsPageNumber' - Set pageNumber to the Teletext page number for the destination captions for this output. This value must be a three-digit hexadecimal string; strings ending in -FF are invalid. If you are passing through the entire set of Teletext data, do not use this field.
teletextDestinationSettings
    :: TeletextDestinationSettings
teletextDestinationSettings =
  TeletextDestinationSettings' {_tdsPageNumber = Nothing}


-- | Set pageNumber to the Teletext page number for the destination captions for this output. This value must be a three-digit hexadecimal string; strings ending in -FF are invalid. If you are passing through the entire set of Teletext data, do not use this field.
tdsPageNumber :: Lens' TeletextDestinationSettings (Maybe Text)
tdsPageNumber = lens _tdsPageNumber (\ s a -> s{_tdsPageNumber = a})

instance FromJSON TeletextDestinationSettings where
        parseJSON
          = withObject "TeletextDestinationSettings"
              (\ x ->
                 TeletextDestinationSettings' <$>
                   (x .:? "pageNumber"))

instance Hashable TeletextDestinationSettings where

instance NFData TeletextDestinationSettings where

instance ToJSON TeletextDestinationSettings where
        toJSON TeletextDestinationSettings'{..}
          = object
              (catMaybes [("pageNumber" .=) <$> _tdsPageNumber])

-- | Settings specific to Teletext caption sources, including Page number.
--
-- /See:/ 'teletextSourceSettings' smart constructor.
newtype TeletextSourceSettings = TeletextSourceSettings'
  { _tssPageNumber :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TeletextSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tssPageNumber' - Use Page Number (PageNumber) to specify the three-digit hexadecimal page number that will be used for Teletext captions. Do not use this setting if you are passing through teletext from the input source to output.
teletextSourceSettings
    :: TeletextSourceSettings
teletextSourceSettings = TeletextSourceSettings' {_tssPageNumber = Nothing}


-- | Use Page Number (PageNumber) to specify the three-digit hexadecimal page number that will be used for Teletext captions. Do not use this setting if you are passing through teletext from the input source to output.
tssPageNumber :: Lens' TeletextSourceSettings (Maybe Text)
tssPageNumber = lens _tssPageNumber (\ s a -> s{_tssPageNumber = a})

instance FromJSON TeletextSourceSettings where
        parseJSON
          = withObject "TeletextSourceSettings"
              (\ x ->
                 TeletextSourceSettings' <$> (x .:? "pageNumber"))

instance Hashable TeletextSourceSettings where

instance NFData TeletextSourceSettings where

instance ToJSON TeletextSourceSettings where
        toJSON TeletextSourceSettings'{..}
          = object
              (catMaybes [("pageNumber" .=) <$> _tssPageNumber])

-- | Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
--
-- /See:/ 'timecodeBurnin' smart constructor.
data TimecodeBurnin = TimecodeBurnin'
  { _tbPrefix   :: !(Maybe Text)
  , _tbFontSize :: !(Maybe Int)
  , _tbPosition :: !(Maybe TimecodeBurninPosition)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimecodeBurnin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbPrefix' - Use Prefix (Prefix) to place ASCII characters before any burned-in timecode. For example, a prefix of "EZ-" will result in the timecode "EZ-00:00:00:00". Provide either the characters themselves or the ASCII code equivalents. The supported range of characters is 0x20 through 0x7e. This includes letters, numbers, and all special characters represented on a standard English keyboard.
--
-- * 'tbFontSize' - Use Font Size (FontSize) to set the font size of any burned-in timecode. Valid values are 10, 16, 32, 48.
--
-- * 'tbPosition' - Undocumented member.
timecodeBurnin
    :: TimecodeBurnin
timecodeBurnin =
  TimecodeBurnin'
    {_tbPrefix = Nothing, _tbFontSize = Nothing, _tbPosition = Nothing}


-- | Use Prefix (Prefix) to place ASCII characters before any burned-in timecode. For example, a prefix of "EZ-" will result in the timecode "EZ-00:00:00:00". Provide either the characters themselves or the ASCII code equivalents. The supported range of characters is 0x20 through 0x7e. This includes letters, numbers, and all special characters represented on a standard English keyboard.
tbPrefix :: Lens' TimecodeBurnin (Maybe Text)
tbPrefix = lens _tbPrefix (\ s a -> s{_tbPrefix = a})

-- | Use Font Size (FontSize) to set the font size of any burned-in timecode. Valid values are 10, 16, 32, 48.
tbFontSize :: Lens' TimecodeBurnin (Maybe Int)
tbFontSize = lens _tbFontSize (\ s a -> s{_tbFontSize = a})

-- | Undocumented member.
tbPosition :: Lens' TimecodeBurnin (Maybe TimecodeBurninPosition)
tbPosition = lens _tbPosition (\ s a -> s{_tbPosition = a})

instance FromJSON TimecodeBurnin where
        parseJSON
          = withObject "TimecodeBurnin"
              (\ x ->
                 TimecodeBurnin' <$>
                   (x .:? "prefix") <*> (x .:? "fontSize") <*>
                     (x .:? "position"))

instance Hashable TimecodeBurnin where

instance NFData TimecodeBurnin where

instance ToJSON TimecodeBurnin where
        toJSON TimecodeBurnin'{..}
          = object
              (catMaybes
                 [("prefix" .=) <$> _tbPrefix,
                  ("fontSize" .=) <$> _tbFontSize,
                  ("position" .=) <$> _tbPosition])

-- | Contains settings used to acquire and adjust timecode information from inputs.
--
-- /See:/ 'timecodeConfig' smart constructor.
data TimecodeConfig = TimecodeConfig'
  { _tcStart           :: !(Maybe Text)
  , _tcTimestampOffset :: !(Maybe Text)
  , _tcAnchor          :: !(Maybe Text)
  , _tcSource          :: !(Maybe TimecodeSource)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimecodeConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcStart' - Only use when you set Timecode Source (TimecodeSource) to Specified Start (SPECIFIEDSTART). Use Start timecode (Start) to specify the timecode for the initial frame. Use 24-hour format with frame number, (HH:MM:SS:FF) or (HH:MM:SS;FF).
--
-- * 'tcTimestampOffset' - Only applies to outputs that support program-date-time stamp. Use Time stamp offset (TimestampOffset) to overwrite the timecode date without affecting the time and frame number. Provide the new date as a string in the format "yyyy-mm-dd".  To use Time stamp offset, you must also enable Insert program-date-time (InsertProgramDateTime) in the output settings.
--
-- * 'tcAnchor' - If you use an editing platform that relies on an anchor timecode, use Anchor Timecode (Anchor) to specify a timecode that will match the input video frame to the output video frame. Use 24-hour format with frame number, (HH:MM:SS:FF) or (HH:MM:SS;FF). This setting ignores framerate conversion. System behavior for Anchor Timecode varies depending on your setting for Timecode source (TimecodeSource). * If Timecode source (TimecodeSource) is set to Specified Start (specifiedstart), the first input frame is the specified value in Start Timecode (Start). Anchor Timecode (Anchor) and Start Timecode (Start) are used calculate output timecode. * If Timecode source (TimecodeSource) is set to Start at 0 (zerobased)  the  first frame is 00:00:00:00. * If Timecode source (TimecodeSource) is set to Embedded (embedded), the  first frame is the timecode value on the first input frame of the input.
--
-- * 'tcSource' - Undocumented member.
timecodeConfig
    :: TimecodeConfig
timecodeConfig =
  TimecodeConfig'
    { _tcStart = Nothing
    , _tcTimestampOffset = Nothing
    , _tcAnchor = Nothing
    , _tcSource = Nothing
    }


-- | Only use when you set Timecode Source (TimecodeSource) to Specified Start (SPECIFIEDSTART). Use Start timecode (Start) to specify the timecode for the initial frame. Use 24-hour format with frame number, (HH:MM:SS:FF) or (HH:MM:SS;FF).
tcStart :: Lens' TimecodeConfig (Maybe Text)
tcStart = lens _tcStart (\ s a -> s{_tcStart = a})

-- | Only applies to outputs that support program-date-time stamp. Use Time stamp offset (TimestampOffset) to overwrite the timecode date without affecting the time and frame number. Provide the new date as a string in the format "yyyy-mm-dd".  To use Time stamp offset, you must also enable Insert program-date-time (InsertProgramDateTime) in the output settings.
tcTimestampOffset :: Lens' TimecodeConfig (Maybe Text)
tcTimestampOffset = lens _tcTimestampOffset (\ s a -> s{_tcTimestampOffset = a})

-- | If you use an editing platform that relies on an anchor timecode, use Anchor Timecode (Anchor) to specify a timecode that will match the input video frame to the output video frame. Use 24-hour format with frame number, (HH:MM:SS:FF) or (HH:MM:SS;FF). This setting ignores framerate conversion. System behavior for Anchor Timecode varies depending on your setting for Timecode source (TimecodeSource). * If Timecode source (TimecodeSource) is set to Specified Start (specifiedstart), the first input frame is the specified value in Start Timecode (Start). Anchor Timecode (Anchor) and Start Timecode (Start) are used calculate output timecode. * If Timecode source (TimecodeSource) is set to Start at 0 (zerobased)  the  first frame is 00:00:00:00. * If Timecode source (TimecodeSource) is set to Embedded (embedded), the  first frame is the timecode value on the first input frame of the input.
tcAnchor :: Lens' TimecodeConfig (Maybe Text)
tcAnchor = lens _tcAnchor (\ s a -> s{_tcAnchor = a})

-- | Undocumented member.
tcSource :: Lens' TimecodeConfig (Maybe TimecodeSource)
tcSource = lens _tcSource (\ s a -> s{_tcSource = a})

instance FromJSON TimecodeConfig where
        parseJSON
          = withObject "TimecodeConfig"
              (\ x ->
                 TimecodeConfig' <$>
                   (x .:? "start") <*> (x .:? "timestampOffset") <*>
                     (x .:? "anchor")
                     <*> (x .:? "source"))

instance Hashable TimecodeConfig where

instance NFData TimecodeConfig where

instance ToJSON TimecodeConfig where
        toJSON TimecodeConfig'{..}
          = object
              (catMaybes
                 [("start" .=) <$> _tcStart,
                  ("timestampOffset" .=) <$> _tcTimestampOffset,
                  ("anchor" .=) <$> _tcAnchor,
                  ("source" .=) <$> _tcSource])

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in your job. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
--
-- /See:/ 'timedMetadataInsertion' smart constructor.
newtype TimedMetadataInsertion = TimedMetadataInsertion'
  { _tmiId3Insertions :: Maybe [Id3Insertion]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimedMetadataInsertion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmiId3Insertions' - Id3Insertions contains the array of Id3Insertion instances.
timedMetadataInsertion
    :: TimedMetadataInsertion
timedMetadataInsertion = TimedMetadataInsertion' {_tmiId3Insertions = Nothing}


-- | Id3Insertions contains the array of Id3Insertion instances.
tmiId3Insertions :: Lens' TimedMetadataInsertion [Id3Insertion]
tmiId3Insertions = lens _tmiId3Insertions (\ s a -> s{_tmiId3Insertions = a}) . _Default . _Coerce

instance FromJSON TimedMetadataInsertion where
        parseJSON
          = withObject "TimedMetadataInsertion"
              (\ x ->
                 TimedMetadataInsertion' <$>
                   (x .:? "id3Insertions" .!= mempty))

instance Hashable TimedMetadataInsertion where

instance NFData TimedMetadataInsertion where

instance ToJSON TimedMetadataInsertion where
        toJSON TimedMetadataInsertion'{..}
          = object
              (catMaybes
                 [("id3Insertions" .=) <$> _tmiId3Insertions])

-- | Information about when jobs are submitted, started, and finished is specified in Unix epoch format in seconds.
--
-- /See:/ 'timing' smart constructor.
data Timing = Timing'
  { _tStartTime  :: !(Maybe POSIX)
  , _tFinishTime :: !(Maybe POSIX)
  , _tSubmitTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Timing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tStartTime' - The time, in Unix epoch format, that transcoding for the job began.
--
-- * 'tFinishTime' - The time, in Unix epoch format, that the transcoding job finished
--
-- * 'tSubmitTime' - The time, in Unix epoch format, that you submitted the job.
timing
    :: Timing
timing =
  Timing'
    {_tStartTime = Nothing, _tFinishTime = Nothing, _tSubmitTime = Nothing}


-- | The time, in Unix epoch format, that transcoding for the job began.
tStartTime :: Lens' Timing (Maybe UTCTime)
tStartTime = lens _tStartTime (\ s a -> s{_tStartTime = a}) . mapping _Time

-- | The time, in Unix epoch format, that the transcoding job finished
tFinishTime :: Lens' Timing (Maybe UTCTime)
tFinishTime = lens _tFinishTime (\ s a -> s{_tFinishTime = a}) . mapping _Time

-- | The time, in Unix epoch format, that you submitted the job.
tSubmitTime :: Lens' Timing (Maybe UTCTime)
tSubmitTime = lens _tSubmitTime (\ s a -> s{_tSubmitTime = a}) . mapping _Time

instance FromJSON Timing where
        parseJSON
          = withObject "Timing"
              (\ x ->
                 Timing' <$>
                   (x .:? "startTime") <*> (x .:? "finishTime") <*>
                     (x .:? "submitTime"))

instance Hashable Timing where

instance NFData Timing where

-- | Settings specific to TTML caption outputs, including Pass style information (TtmlStylePassthrough).
--
-- /See:/ 'ttmlDestinationSettings' smart constructor.
newtype TtmlDestinationSettings = TtmlDestinationSettings'
  { _tdsStylePassthrough :: Maybe TtmlStylePassthrough
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TtmlDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdsStylePassthrough' - Undocumented member.
ttmlDestinationSettings
    :: TtmlDestinationSettings
ttmlDestinationSettings =
  TtmlDestinationSettings' {_tdsStylePassthrough = Nothing}


-- | Undocumented member.
tdsStylePassthrough :: Lens' TtmlDestinationSettings (Maybe TtmlStylePassthrough)
tdsStylePassthrough = lens _tdsStylePassthrough (\ s a -> s{_tdsStylePassthrough = a})

instance FromJSON TtmlDestinationSettings where
        parseJSON
          = withObject "TtmlDestinationSettings"
              (\ x ->
                 TtmlDestinationSettings' <$>
                   (x .:? "stylePassthrough"))

instance Hashable TtmlDestinationSettings where

instance NFData TtmlDestinationSettings where

instance ToJSON TtmlDestinationSettings where
        toJSON TtmlDestinationSettings'{..}
          = object
              (catMaybes
                 [("stylePassthrough" .=) <$> _tdsStylePassthrough])

-- | Video codec settings, (CodecSettings) under (VideoDescription), contains the group of settings related to video encoding. The settings in this group vary depending on the value you choose for Video codec (Codec). For each codec enum you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * H_264, H264Settings * H_265, H265Settings * MPEG2, Mpeg2Settings * PRORES, ProresSettings * FRAME_CAPTURE, FrameCaptureSettings
--
-- /See:/ 'videoCodecSettings' smart constructor.
data VideoCodecSettings = VideoCodecSettings'
  { _vcsFrameCaptureSettings :: !(Maybe FrameCaptureSettings)
  , _vcsCodec                :: !(Maybe VideoCodec)
  , _vcsH265Settings         :: !(Maybe H265Settings)
  , _vcsProresSettings       :: !(Maybe ProresSettings)
  , _vcsH264Settings         :: !(Maybe H264Settings)
  , _vcsMpeg2Settings        :: !(Maybe Mpeg2Settings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VideoCodecSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcsFrameCaptureSettings' - Undocumented member.
--
-- * 'vcsCodec' - Undocumented member.
--
-- * 'vcsH265Settings' - Undocumented member.
--
-- * 'vcsProresSettings' - Undocumented member.
--
-- * 'vcsH264Settings' - Undocumented member.
--
-- * 'vcsMpeg2Settings' - Undocumented member.
videoCodecSettings
    :: VideoCodecSettings
videoCodecSettings =
  VideoCodecSettings'
    { _vcsFrameCaptureSettings = Nothing
    , _vcsCodec = Nothing
    , _vcsH265Settings = Nothing
    , _vcsProresSettings = Nothing
    , _vcsH264Settings = Nothing
    , _vcsMpeg2Settings = Nothing
    }


-- | Undocumented member.
vcsFrameCaptureSettings :: Lens' VideoCodecSettings (Maybe FrameCaptureSettings)
vcsFrameCaptureSettings = lens _vcsFrameCaptureSettings (\ s a -> s{_vcsFrameCaptureSettings = a})

-- | Undocumented member.
vcsCodec :: Lens' VideoCodecSettings (Maybe VideoCodec)
vcsCodec = lens _vcsCodec (\ s a -> s{_vcsCodec = a})

-- | Undocumented member.
vcsH265Settings :: Lens' VideoCodecSettings (Maybe H265Settings)
vcsH265Settings = lens _vcsH265Settings (\ s a -> s{_vcsH265Settings = a})

-- | Undocumented member.
vcsProresSettings :: Lens' VideoCodecSettings (Maybe ProresSettings)
vcsProresSettings = lens _vcsProresSettings (\ s a -> s{_vcsProresSettings = a})

-- | Undocumented member.
vcsH264Settings :: Lens' VideoCodecSettings (Maybe H264Settings)
vcsH264Settings = lens _vcsH264Settings (\ s a -> s{_vcsH264Settings = a})

-- | Undocumented member.
vcsMpeg2Settings :: Lens' VideoCodecSettings (Maybe Mpeg2Settings)
vcsMpeg2Settings = lens _vcsMpeg2Settings (\ s a -> s{_vcsMpeg2Settings = a})

instance FromJSON VideoCodecSettings where
        parseJSON
          = withObject "VideoCodecSettings"
              (\ x ->
                 VideoCodecSettings' <$>
                   (x .:? "frameCaptureSettings") <*> (x .:? "codec")
                     <*> (x .:? "h265Settings")
                     <*> (x .:? "proresSettings")
                     <*> (x .:? "h264Settings")
                     <*> (x .:? "mpeg2Settings"))

instance Hashable VideoCodecSettings where

instance NFData VideoCodecSettings where

instance ToJSON VideoCodecSettings where
        toJSON VideoCodecSettings'{..}
          = object
              (catMaybes
                 [("frameCaptureSettings" .=) <$>
                    _vcsFrameCaptureSettings,
                  ("codec" .=) <$> _vcsCodec,
                  ("h265Settings" .=) <$> _vcsH265Settings,
                  ("proresSettings" .=) <$> _vcsProresSettings,
                  ("h264Settings" .=) <$> _vcsH264Settings,
                  ("mpeg2Settings" .=) <$> _vcsMpeg2Settings])

-- | Settings for video outputs
--
-- /See:/ 'videoDescription' smart constructor.
data VideoDescription = VideoDescription'
  { _vdTimecodeInsertion  :: !(Maybe VideoTimecodeInsertion)
  , _vdHeight             :: !(Maybe Int)
  , _vdAfdSignaling       :: !(Maybe AfdSignaling)
  , _vdSharpness          :: !(Maybe Int)
  , _vdCrop               :: !(Maybe Rectangle)
  , _vdWidth              :: !(Maybe Int)
  , _vdScalingBehavior    :: !(Maybe ScalingBehavior)
  , _vdRespondToAfd       :: !(Maybe RespondToAfd)
  , _vdDropFrameTimecode  :: !(Maybe DropFrameTimecode)
  , _vdAntiAlias          :: !(Maybe AntiAlias)
  , _vdFixedAfd           :: !(Maybe Int)
  , _vdColorMetadata      :: !(Maybe ColorMetadata)
  , _vdCodecSettings      :: !(Maybe VideoCodecSettings)
  , _vdVideoPreprocessors :: !(Maybe VideoPreprocessor)
  , _vdPosition           :: !(Maybe Rectangle)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VideoDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdTimecodeInsertion' - Undocumented member.
--
-- * 'vdHeight' - Use the Height (Height) setting to define the video resolution height for this output. Specify in pixels. If you don't provide a value here, the service will use the input height.
--
-- * 'vdAfdSignaling' - Undocumented member.
--
-- * 'vdSharpness' - Use Sharpness (Sharpness)setting to specify the strength of anti-aliasing. This setting changes the width of the anti-alias filter kernel used for scaling. Sharpness only applies if your output resolution is different from your input resolution, and if you set Anti-alias (AntiAlias) to ENABLED. 0 is the softest setting, 100 the sharpest, and 50 recommended for most content.
--
-- * 'vdCrop' - Applies only if your input aspect ratio is different from your output aspect ratio. Use Input cropping rectangle (Crop) to specify the  video area the service will include in the output. This will crop the input source, causing video pixels to be removed on encode. Do not use this setting if you have enabled Stretch to output (stretchToOutput) in your output settings.
--
-- * 'vdWidth' - Use Width (Width) to define the video resolution width, in pixels, for this output. If you don't provide a value here, the service will use the input width.
--
-- * 'vdScalingBehavior' - Undocumented member.
--
-- * 'vdRespondToAfd' - Undocumented member.
--
-- * 'vdDropFrameTimecode' - Undocumented member.
--
-- * 'vdAntiAlias' - Undocumented member.
--
-- * 'vdFixedAfd' - Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED). Use Fixed (FixedAfd) to specify a four-bit AFD value which the service will write on all  frames of this video output.
--
-- * 'vdColorMetadata' - Undocumented member.
--
-- * 'vdCodecSettings' - Undocumented member.
--
-- * 'vdVideoPreprocessors' - Find additional transcoding features under Preprocessors (VideoPreprocessors). Enable the features at each output individually. These features are disabled by default.
--
-- * 'vdPosition' - Use Position (Position) to point to a rectangle object to define your position. This setting overrides any other aspect ratio.
videoDescription
    :: VideoDescription
videoDescription =
  VideoDescription'
    { _vdTimecodeInsertion = Nothing
    , _vdHeight = Nothing
    , _vdAfdSignaling = Nothing
    , _vdSharpness = Nothing
    , _vdCrop = Nothing
    , _vdWidth = Nothing
    , _vdScalingBehavior = Nothing
    , _vdRespondToAfd = Nothing
    , _vdDropFrameTimecode = Nothing
    , _vdAntiAlias = Nothing
    , _vdFixedAfd = Nothing
    , _vdColorMetadata = Nothing
    , _vdCodecSettings = Nothing
    , _vdVideoPreprocessors = Nothing
    , _vdPosition = Nothing
    }


-- | Undocumented member.
vdTimecodeInsertion :: Lens' VideoDescription (Maybe VideoTimecodeInsertion)
vdTimecodeInsertion = lens _vdTimecodeInsertion (\ s a -> s{_vdTimecodeInsertion = a})

-- | Use the Height (Height) setting to define the video resolution height for this output. Specify in pixels. If you don't provide a value here, the service will use the input height.
vdHeight :: Lens' VideoDescription (Maybe Int)
vdHeight = lens _vdHeight (\ s a -> s{_vdHeight = a})

-- | Undocumented member.
vdAfdSignaling :: Lens' VideoDescription (Maybe AfdSignaling)
vdAfdSignaling = lens _vdAfdSignaling (\ s a -> s{_vdAfdSignaling = a})

-- | Use Sharpness (Sharpness)setting to specify the strength of anti-aliasing. This setting changes the width of the anti-alias filter kernel used for scaling. Sharpness only applies if your output resolution is different from your input resolution, and if you set Anti-alias (AntiAlias) to ENABLED. 0 is the softest setting, 100 the sharpest, and 50 recommended for most content.
vdSharpness :: Lens' VideoDescription (Maybe Int)
vdSharpness = lens _vdSharpness (\ s a -> s{_vdSharpness = a})

-- | Applies only if your input aspect ratio is different from your output aspect ratio. Use Input cropping rectangle (Crop) to specify the  video area the service will include in the output. This will crop the input source, causing video pixels to be removed on encode. Do not use this setting if you have enabled Stretch to output (stretchToOutput) in your output settings.
vdCrop :: Lens' VideoDescription (Maybe Rectangle)
vdCrop = lens _vdCrop (\ s a -> s{_vdCrop = a})

-- | Use Width (Width) to define the video resolution width, in pixels, for this output. If you don't provide a value here, the service will use the input width.
vdWidth :: Lens' VideoDescription (Maybe Int)
vdWidth = lens _vdWidth (\ s a -> s{_vdWidth = a})

-- | Undocumented member.
vdScalingBehavior :: Lens' VideoDescription (Maybe ScalingBehavior)
vdScalingBehavior = lens _vdScalingBehavior (\ s a -> s{_vdScalingBehavior = a})

-- | Undocumented member.
vdRespondToAfd :: Lens' VideoDescription (Maybe RespondToAfd)
vdRespondToAfd = lens _vdRespondToAfd (\ s a -> s{_vdRespondToAfd = a})

-- | Undocumented member.
vdDropFrameTimecode :: Lens' VideoDescription (Maybe DropFrameTimecode)
vdDropFrameTimecode = lens _vdDropFrameTimecode (\ s a -> s{_vdDropFrameTimecode = a})

-- | Undocumented member.
vdAntiAlias :: Lens' VideoDescription (Maybe AntiAlias)
vdAntiAlias = lens _vdAntiAlias (\ s a -> s{_vdAntiAlias = a})

-- | Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED). Use Fixed (FixedAfd) to specify a four-bit AFD value which the service will write on all  frames of this video output.
vdFixedAfd :: Lens' VideoDescription (Maybe Int)
vdFixedAfd = lens _vdFixedAfd (\ s a -> s{_vdFixedAfd = a})

-- | Undocumented member.
vdColorMetadata :: Lens' VideoDescription (Maybe ColorMetadata)
vdColorMetadata = lens _vdColorMetadata (\ s a -> s{_vdColorMetadata = a})

-- | Undocumented member.
vdCodecSettings :: Lens' VideoDescription (Maybe VideoCodecSettings)
vdCodecSettings = lens _vdCodecSettings (\ s a -> s{_vdCodecSettings = a})

-- | Find additional transcoding features under Preprocessors (VideoPreprocessors). Enable the features at each output individually. These features are disabled by default.
vdVideoPreprocessors :: Lens' VideoDescription (Maybe VideoPreprocessor)
vdVideoPreprocessors = lens _vdVideoPreprocessors (\ s a -> s{_vdVideoPreprocessors = a})

-- | Use Position (Position) to point to a rectangle object to define your position. This setting overrides any other aspect ratio.
vdPosition :: Lens' VideoDescription (Maybe Rectangle)
vdPosition = lens _vdPosition (\ s a -> s{_vdPosition = a})

instance FromJSON VideoDescription where
        parseJSON
          = withObject "VideoDescription"
              (\ x ->
                 VideoDescription' <$>
                   (x .:? "timecodeInsertion") <*> (x .:? "height") <*>
                     (x .:? "afdSignaling")
                     <*> (x .:? "sharpness")
                     <*> (x .:? "crop")
                     <*> (x .:? "width")
                     <*> (x .:? "scalingBehavior")
                     <*> (x .:? "respondToAfd")
                     <*> (x .:? "dropFrameTimecode")
                     <*> (x .:? "antiAlias")
                     <*> (x .:? "fixedAfd")
                     <*> (x .:? "colorMetadata")
                     <*> (x .:? "codecSettings")
                     <*> (x .:? "videoPreprocessors")
                     <*> (x .:? "position"))

instance Hashable VideoDescription where

instance NFData VideoDescription where

instance ToJSON VideoDescription where
        toJSON VideoDescription'{..}
          = object
              (catMaybes
                 [("timecodeInsertion" .=) <$> _vdTimecodeInsertion,
                  ("height" .=) <$> _vdHeight,
                  ("afdSignaling" .=) <$> _vdAfdSignaling,
                  ("sharpness" .=) <$> _vdSharpness,
                  ("crop" .=) <$> _vdCrop, ("width" .=) <$> _vdWidth,
                  ("scalingBehavior" .=) <$> _vdScalingBehavior,
                  ("respondToAfd" .=) <$> _vdRespondToAfd,
                  ("dropFrameTimecode" .=) <$> _vdDropFrameTimecode,
                  ("antiAlias" .=) <$> _vdAntiAlias,
                  ("fixedAfd" .=) <$> _vdFixedAfd,
                  ("colorMetadata" .=) <$> _vdColorMetadata,
                  ("codecSettings" .=) <$> _vdCodecSettings,
                  ("videoPreprocessors" .=) <$> _vdVideoPreprocessors,
                  ("position" .=) <$> _vdPosition])

-- | Contains details about the output's video stream
--
-- /See:/ 'videoDetail' smart constructor.
data VideoDetail = VideoDetail'
  { _vdHeightInPx :: !(Maybe Int)
  , _vdWidthInPx  :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VideoDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdHeightInPx' - Height in pixels for the output
--
-- * 'vdWidthInPx' - Width in pixels for the output
videoDetail
    :: VideoDetail
videoDetail = VideoDetail' {_vdHeightInPx = Nothing, _vdWidthInPx = Nothing}


-- | Height in pixels for the output
vdHeightInPx :: Lens' VideoDetail (Maybe Int)
vdHeightInPx = lens _vdHeightInPx (\ s a -> s{_vdHeightInPx = a})

-- | Width in pixels for the output
vdWidthInPx :: Lens' VideoDetail (Maybe Int)
vdWidthInPx = lens _vdWidthInPx (\ s a -> s{_vdWidthInPx = a})

instance FromJSON VideoDetail where
        parseJSON
          = withObject "VideoDetail"
              (\ x ->
                 VideoDetail' <$>
                   (x .:? "heightInPx") <*> (x .:? "widthInPx"))

instance Hashable VideoDetail where

instance NFData VideoDetail where

-- | Find additional transcoding features under Preprocessors (VideoPreprocessors). Enable the features at each output individually. These features are disabled by default.
--
-- /See:/ 'videoPreprocessor' smart constructor.
data VideoPreprocessor = VideoPreprocessor'
  { _vpTimecodeBurnin :: !(Maybe TimecodeBurnin)
  , _vpColorCorrector :: !(Maybe ColorCorrector)
  , _vpDeinterlacer   :: !(Maybe Deinterlacer)
  , _vpNoiseReducer   :: !(Maybe NoiseReducer)
  , _vpImageInserter  :: !(Maybe ImageInserter)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VideoPreprocessor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpTimecodeBurnin' - Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
--
-- * 'vpColorCorrector' - Enable the Color corrector (ColorCorrector) feature if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
--
-- * 'vpDeinterlacer' - Use Deinterlacer (Deinterlacer) to produce smoother motion and a clearer picture.
--
-- * 'vpNoiseReducer' - Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
--
-- * 'vpImageInserter' - Enable the Image inserter (ImageInserter) feature to include a graphic overlay on your video. Enable or disable this feature for each output individually. This setting is disabled by default.
videoPreprocessor
    :: VideoPreprocessor
videoPreprocessor =
  VideoPreprocessor'
    { _vpTimecodeBurnin = Nothing
    , _vpColorCorrector = Nothing
    , _vpDeinterlacer = Nothing
    , _vpNoiseReducer = Nothing
    , _vpImageInserter = Nothing
    }


-- | Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
vpTimecodeBurnin :: Lens' VideoPreprocessor (Maybe TimecodeBurnin)
vpTimecodeBurnin = lens _vpTimecodeBurnin (\ s a -> s{_vpTimecodeBurnin = a})

-- | Enable the Color corrector (ColorCorrector) feature if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
vpColorCorrector :: Lens' VideoPreprocessor (Maybe ColorCorrector)
vpColorCorrector = lens _vpColorCorrector (\ s a -> s{_vpColorCorrector = a})

-- | Use Deinterlacer (Deinterlacer) to produce smoother motion and a clearer picture.
vpDeinterlacer :: Lens' VideoPreprocessor (Maybe Deinterlacer)
vpDeinterlacer = lens _vpDeinterlacer (\ s a -> s{_vpDeinterlacer = a})

-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default.
vpNoiseReducer :: Lens' VideoPreprocessor (Maybe NoiseReducer)
vpNoiseReducer = lens _vpNoiseReducer (\ s a -> s{_vpNoiseReducer = a})

-- | Enable the Image inserter (ImageInserter) feature to include a graphic overlay on your video. Enable or disable this feature for each output individually. This setting is disabled by default.
vpImageInserter :: Lens' VideoPreprocessor (Maybe ImageInserter)
vpImageInserter = lens _vpImageInserter (\ s a -> s{_vpImageInserter = a})

instance FromJSON VideoPreprocessor where
        parseJSON
          = withObject "VideoPreprocessor"
              (\ x ->
                 VideoPreprocessor' <$>
                   (x .:? "timecodeBurnin") <*> (x .:? "colorCorrector")
                     <*> (x .:? "deinterlacer")
                     <*> (x .:? "noiseReducer")
                     <*> (x .:? "imageInserter"))

instance Hashable VideoPreprocessor where

instance NFData VideoPreprocessor where

instance ToJSON VideoPreprocessor where
        toJSON VideoPreprocessor'{..}
          = object
              (catMaybes
                 [("timecodeBurnin" .=) <$> _vpTimecodeBurnin,
                  ("colorCorrector" .=) <$> _vpColorCorrector,
                  ("deinterlacer" .=) <$> _vpDeinterlacer,
                  ("noiseReducer" .=) <$> _vpNoiseReducer,
                  ("imageInserter" .=) <$> _vpImageInserter])

-- | Selector for video.
--
-- /See:/ 'videoSelector' smart constructor.
data VideoSelector = VideoSelector'
  { _vsProgramNumber   :: !(Maybe Int)
  , _vsColorSpaceUsage :: !(Maybe ColorSpaceUsage)
  , _vsHdr10Metadata   :: !(Maybe Hdr10Metadata)
  , _vsPid             :: !(Maybe Int)
  , _vsColorSpace      :: !(Maybe ColorSpace)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VideoSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsProgramNumber' - Selects a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported.
--
-- * 'vsColorSpaceUsage' - Undocumented member.
--
-- * 'vsHdr10Metadata' - Undocumented member.
--
-- * 'vsPid' - Use PID (Pid) to select specific video data from an input file. Specify this value as an integer; the system automatically converts it to the hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet identifier, is an identifier for a set of data in an MPEG-2 transport stream container.
--
-- * 'vsColorSpace' - Undocumented member.
videoSelector
    :: VideoSelector
videoSelector =
  VideoSelector'
    { _vsProgramNumber = Nothing
    , _vsColorSpaceUsage = Nothing
    , _vsHdr10Metadata = Nothing
    , _vsPid = Nothing
    , _vsColorSpace = Nothing
    }


-- | Selects a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported.
vsProgramNumber :: Lens' VideoSelector (Maybe Int)
vsProgramNumber = lens _vsProgramNumber (\ s a -> s{_vsProgramNumber = a})

-- | Undocumented member.
vsColorSpaceUsage :: Lens' VideoSelector (Maybe ColorSpaceUsage)
vsColorSpaceUsage = lens _vsColorSpaceUsage (\ s a -> s{_vsColorSpaceUsage = a})

-- | Undocumented member.
vsHdr10Metadata :: Lens' VideoSelector (Maybe Hdr10Metadata)
vsHdr10Metadata = lens _vsHdr10Metadata (\ s a -> s{_vsHdr10Metadata = a})

-- | Use PID (Pid) to select specific video data from an input file. Specify this value as an integer; the system automatically converts it to the hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet identifier, is an identifier for a set of data in an MPEG-2 transport stream container.
vsPid :: Lens' VideoSelector (Maybe Int)
vsPid = lens _vsPid (\ s a -> s{_vsPid = a})

-- | Undocumented member.
vsColorSpace :: Lens' VideoSelector (Maybe ColorSpace)
vsColorSpace = lens _vsColorSpace (\ s a -> s{_vsColorSpace = a})

instance FromJSON VideoSelector where
        parseJSON
          = withObject "VideoSelector"
              (\ x ->
                 VideoSelector' <$>
                   (x .:? "programNumber") <*> (x .:? "colorSpaceUsage")
                     <*> (x .:? "hdr10Metadata")
                     <*> (x .:? "pid")
                     <*> (x .:? "colorSpace"))

instance Hashable VideoSelector where

instance NFData VideoSelector where

instance ToJSON VideoSelector where
        toJSON VideoSelector'{..}
          = object
              (catMaybes
                 [("programNumber" .=) <$> _vsProgramNumber,
                  ("colorSpaceUsage" .=) <$> _vsColorSpaceUsage,
                  ("hdr10Metadata" .=) <$> _vsHdr10Metadata,
                  ("pid" .=) <$> _vsPid,
                  ("colorSpace" .=) <$> _vsColorSpace])

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value WAV.
--
-- /See:/ 'wavSettings' smart constructor.
data WavSettings = WavSettings'
  { _wsBitDepth   :: !(Maybe Int)
  , _wsChannels   :: !(Maybe Int)
  , _wsSampleRate :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WavSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wsBitDepth' - Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
--
-- * 'wsChannels' - Set Channels to specify the number of channels in this output audio track. With WAV, valid values 1, 2, 4, and 8. In the console, these values are Mono, Stereo, 4-Channel, and 8-Channel, respectively.
--
-- * 'wsSampleRate' - Sample rate in Hz.
wavSettings
    :: WavSettings
wavSettings =
  WavSettings'
    {_wsBitDepth = Nothing, _wsChannels = Nothing, _wsSampleRate = Nothing}


-- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
wsBitDepth :: Lens' WavSettings (Maybe Int)
wsBitDepth = lens _wsBitDepth (\ s a -> s{_wsBitDepth = a})

-- | Set Channels to specify the number of channels in this output audio track. With WAV, valid values 1, 2, 4, and 8. In the console, these values are Mono, Stereo, 4-Channel, and 8-Channel, respectively.
wsChannels :: Lens' WavSettings (Maybe Int)
wsChannels = lens _wsChannels (\ s a -> s{_wsChannels = a})

-- | Sample rate in Hz.
wsSampleRate :: Lens' WavSettings (Maybe Int)
wsSampleRate = lens _wsSampleRate (\ s a -> s{_wsSampleRate = a})

instance FromJSON WavSettings where
        parseJSON
          = withObject "WavSettings"
              (\ x ->
                 WavSettings' <$>
                   (x .:? "bitDepth") <*> (x .:? "channels") <*>
                     (x .:? "sampleRate"))

instance Hashable WavSettings where

instance NFData WavSettings where

instance ToJSON WavSettings where
        toJSON WavSettings'{..}
          = object
              (catMaybes
                 [("bitDepth" .=) <$> _wsBitDepth,
                  ("channels" .=) <$> _wsChannels,
                  ("sampleRate" .=) <$> _wsSampleRate])
