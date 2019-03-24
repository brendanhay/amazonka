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

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC. The service accepts one of two mutually exclusive groups of AAC settings--VBR and CBR. To select one of these modes, set the value of Bitrate control mode (rateControlMode) to "VBR" or "CBR".  In VBR mode, you control the audio quality with the setting VBR quality (vbrQuality). In CBR mode, you use the setting Bitrate (bitrate). Defaults and valid values depend on the rate control mode.
--
-- /See:/ 'aacSettings' smart constructor.
data AacSettings = AacSettings'
  { _assAudioDescriptionBroadcasterMix :: !(Maybe AacAudioDescriptionBroadcasterMix)
  , _assRawFormat :: !(Maybe AacRawFormat)
  , _assCodingMode :: !(Maybe AacCodingMode)
  , _assRateControlMode :: !(Maybe AacRateControlMode)
  , _assSampleRate :: !(Maybe Nat)
  , _assSpecification :: !(Maybe AacSpecification)
  , _assCodecProfile :: !(Maybe AacCodecProfile)
  , _assBitrate :: !(Maybe Nat)
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
-- * 'assBitrate' - Average bitrate in bits/second. The set of valid values for this setting is: 6000, 8000, 10000, 12000, 14000, 16000, 20000, 24000, 28000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 288000, 320000, 384000, 448000, 512000, 576000, 640000, 768000, 896000, 1024000. The value you set is also constrained by the values you choose for Profile (codecProfile), Bitrate control mode (codingMode), and Sample rate (sampleRate). Default values depend on Bitrate control mode and Profile.
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
assSampleRate :: Lens' AacSettings (Maybe Natural)
assSampleRate = lens _assSampleRate (\ s a -> s{_assSampleRate = a}) . mapping _Nat

-- | Undocumented member.
assSpecification :: Lens' AacSettings (Maybe AacSpecification)
assSpecification = lens _assSpecification (\ s a -> s{_assSpecification = a})

-- | Undocumented member.
assCodecProfile :: Lens' AacSettings (Maybe AacCodecProfile)
assCodecProfile = lens _assCodecProfile (\ s a -> s{_assCodecProfile = a})

-- | Average bitrate in bits/second. The set of valid values for this setting is: 6000, 8000, 10000, 12000, 14000, 16000, 20000, 24000, 28000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 288000, 320000, 384000, 448000, 512000, 576000, 640000, 768000, 896000, 1024000. The value you set is also constrained by the values you choose for Profile (codecProfile), Bitrate control mode (codingMode), and Sample rate (sampleRate). Default values depend on Bitrate control mode and Profile.
assBitrate :: Lens' AacSettings (Maybe Natural)
assBitrate = lens _assBitrate (\ s a -> s{_assBitrate = a}) . mapping _Nat

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
  , _aSampleRate :: !(Maybe Nat)
  , _aDynamicRangeCompressionProfile :: !(Maybe Ac3DynamicRangeCompressionProfile)
  , _aBitrate :: !(Maybe Nat)
  , _aDialnorm :: !(Maybe Nat)
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
aSampleRate :: Lens' Ac3Settings (Maybe Natural)
aSampleRate = lens _aSampleRate (\ s a -> s{_aSampleRate = a}) . mapping _Nat

-- | Undocumented member.
aDynamicRangeCompressionProfile :: Lens' Ac3Settings (Maybe Ac3DynamicRangeCompressionProfile)
aDynamicRangeCompressionProfile = lens _aDynamicRangeCompressionProfile (\ s a -> s{_aDynamicRangeCompressionProfile = a})

-- | Average bitrate in bits/second. Valid bitrates depend on the coding mode.
aBitrate :: Lens' Ac3Settings (Maybe Natural)
aBitrate = lens _aBitrate (\ s a -> s{_aBitrate = a}) . mapping _Nat

-- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital, dialnorm will be passed through.
aDialnorm :: Lens' Ac3Settings (Maybe Natural)
aDialnorm = lens _aDialnorm (\ s a -> s{_aDialnorm = a}) . mapping _Nat

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

-- | Acceleration settings for job execution.
--
-- /See:/ 'accelerationSettings' smart constructor.
newtype AccelerationSettings = AccelerationSettings'
  { _asMode :: AccelerationMode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccelerationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asMode' - Acceleration configuration for the job.
accelerationSettings
    :: AccelerationMode -- ^ 'asMode'
    -> AccelerationSettings
accelerationSettings pMode_ = AccelerationSettings' {_asMode = pMode_}


-- | Acceleration configuration for the job.
asMode :: Lens' AccelerationSettings AccelerationMode
asMode = lens _asMode (\ s a -> s{_asMode = a})

instance FromJSON AccelerationSettings where
        parseJSON
          = withObject "AccelerationSettings"
              (\ x -> AccelerationSettings' <$> (x .: "mode"))

instance Hashable AccelerationSettings where

instance NFData AccelerationSettings where

instance ToJSON AccelerationSettings where
        toJSON AccelerationSettings'{..}
          = object (catMaybes [Just ("mode" .= _asMode)])

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.
--
-- /See:/ 'aiffSettings' smart constructor.
data AiffSettings = AiffSettings'
  { _asBitDepth   :: !(Maybe Nat)
  , _asChannels   :: !(Maybe Nat)
  , _asSampleRate :: !(Maybe Nat)
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
asBitDepth :: Lens' AiffSettings (Maybe Natural)
asBitDepth = lens _asBitDepth (\ s a -> s{_asBitDepth = a}) . mapping _Nat

-- | Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
asChannels :: Lens' AiffSettings (Maybe Natural)
asChannels = lens _asChannels (\ s a -> s{_asChannels = a}) . mapping _Nat

-- | Sample rate in hz.
asSampleRate :: Lens' AiffSettings (Maybe Natural)
asSampleRate = lens _asSampleRate (\ s a -> s{_asSampleRate = a}) . mapping _Nat

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
  { _assSourceAncillaryChannelNumber :: Maybe Nat
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
assSourceAncillaryChannelNumber :: Lens' AncillarySourceSettings (Maybe Natural)
assSourceAncillaryChannelNumber = lens _assSourceAncillaryChannelNumber (\ s a -> s{_assSourceAncillaryChannelNumber = a}) . mapping _Nat

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
  , _adCustomLanguageCode         :: !(Maybe Text)
  , _adLanguageCode               :: !(Maybe LanguageCode)
  , _adAudioType                  :: !(Maybe Nat)
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
-- * 'adCustomLanguageCode' - Specify the language for this audio output track, using the ISO 639-2 or ISO 639-3 three-letter language code. The language specified will be used when 'Follow Input Language Code' is not selected or when 'Follow Input Language Code' is selected but there is no ISO 639 language code specified by the input.
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
    , _adCustomLanguageCode = Nothing
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

-- | Specify the language for this audio output track, using the ISO 639-2 or ISO 639-3 three-letter language code. The language specified will be used when 'Follow Input Language Code' is not selected or when 'Follow Input Language Code' is selected but there is no ISO 639 language code specified by the input.
adCustomLanguageCode :: Lens' AudioDescription (Maybe Text)
adCustomLanguageCode = lens _adCustomLanguageCode (\ s a -> s{_adCustomLanguageCode = a})

-- | Indicates the language of the audio output track. The ISO 639 language specified in the 'Language Code' drop down will be used when 'Follow Input Language Code' is not selected or when 'Follow Input Language Code' is selected but there is no ISO 639 language code specified by the input.
adLanguageCode :: Lens' AudioDescription (Maybe LanguageCode)
adLanguageCode = lens _adLanguageCode (\ s a -> s{_adLanguageCode = a})

-- | Applies only if Follow Input Audio Type is unchecked (false). A number between 0 and 255. The following are defined in ISO-IEC 13818-1: 0 = Undefined, 1 = Clean Effects, 2 = Hearing Impaired, 3 = Visually Impaired Commentary, 4-255 = Reserved.
adAudioType :: Lens' AudioDescription (Maybe Natural)
adAudioType = lens _adAudioType (\ s a -> s{_adAudioType = a}) . mapping _Nat

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
                   (x .:? "audioSourceName") <*>
                     (x .:? "customLanguageCode")
                     <*> (x .:? "languageCode")
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
                  ("customLanguageCode" .=) <$> _adCustomLanguageCode,
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
  { _asTracks                 :: !(Maybe [Nat])
  , _asCustomLanguageCode     :: !(Maybe Text)
  , _asProgramSelection       :: !(Maybe Nat)
  , _asLanguageCode           :: !(Maybe LanguageCode)
  , _asOffset                 :: !(Maybe Int)
  , _asDefaultSelection       :: !(Maybe AudioDefaultSelection)
  , _asPids                   :: !(Maybe [Nat])
  , _asSelectorType           :: !(Maybe AudioSelectorType)
  , _asExternalAudioFileInput :: !(Maybe Text)
  , _asRemixSettings          :: !(Maybe RemixSettings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AudioSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asTracks' - Identify a track from the input audio to include in this selector by entering the track index number. To include several tracks in a single audio selector, specify multiple tracks as follows. Using the console, enter a comma-separated list. For examle, type "1,2,3" to include tracks 1 through 3. Specifying directly in your JSON job file, provide the track numbers in an array. For example, "tracks": [1,2,3].
--
-- * 'asCustomLanguageCode' - Selects a specific language code from within an audio source, using the ISO 639-2 or ISO 639-3 three-letter language code
--
-- * 'asProgramSelection' - Use this setting for input streams that contain Dolby E, to have the service extract specific program data from the track. To select multiple programs, create multiple selectors with the same Track and different Program numbers. In the console, this setting is visible when you set Selector type to Track. Choose the program number from the dropdown list. If you are sending a JSON file, provide the program ID, which is part of the audio metadata. If your input file has incorrect metadata, you can choose All channels instead of a program number to have the service ignore the program IDs and include all the programs in the track.
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
-- * 'asRemixSettings' - Use these settings to reorder the audio channels of one input to match those of another input. This allows you to combine the two files into a single output, one after the other.
audioSelector
    :: AudioSelector
audioSelector =
  AudioSelector'
    { _asTracks = Nothing
    , _asCustomLanguageCode = Nothing
    , _asProgramSelection = Nothing
    , _asLanguageCode = Nothing
    , _asOffset = Nothing
    , _asDefaultSelection = Nothing
    , _asPids = Nothing
    , _asSelectorType = Nothing
    , _asExternalAudioFileInput = Nothing
    , _asRemixSettings = Nothing
    }


-- | Identify a track from the input audio to include in this selector by entering the track index number. To include several tracks in a single audio selector, specify multiple tracks as follows. Using the console, enter a comma-separated list. For examle, type "1,2,3" to include tracks 1 through 3. Specifying directly in your JSON job file, provide the track numbers in an array. For example, "tracks": [1,2,3].
asTracks :: Lens' AudioSelector [Natural]
asTracks = lens _asTracks (\ s a -> s{_asTracks = a}) . _Default . _Coerce

-- | Selects a specific language code from within an audio source, using the ISO 639-2 or ISO 639-3 three-letter language code
asCustomLanguageCode :: Lens' AudioSelector (Maybe Text)
asCustomLanguageCode = lens _asCustomLanguageCode (\ s a -> s{_asCustomLanguageCode = a})

-- | Use this setting for input streams that contain Dolby E, to have the service extract specific program data from the track. To select multiple programs, create multiple selectors with the same Track and different Program numbers. In the console, this setting is visible when you set Selector type to Track. Choose the program number from the dropdown list. If you are sending a JSON file, provide the program ID, which is part of the audio metadata. If your input file has incorrect metadata, you can choose All channels instead of a program number to have the service ignore the program IDs and include all the programs in the track.
asProgramSelection :: Lens' AudioSelector (Maybe Natural)
asProgramSelection = lens _asProgramSelection (\ s a -> s{_asProgramSelection = a}) . mapping _Nat

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
asPids :: Lens' AudioSelector [Natural]
asPids = lens _asPids (\ s a -> s{_asPids = a}) . _Default . _Coerce

-- | Undocumented member.
asSelectorType :: Lens' AudioSelector (Maybe AudioSelectorType)
asSelectorType = lens _asSelectorType (\ s a -> s{_asSelectorType = a})

-- | Specifies audio data from an external file source.
asExternalAudioFileInput :: Lens' AudioSelector (Maybe Text)
asExternalAudioFileInput = lens _asExternalAudioFileInput (\ s a -> s{_asExternalAudioFileInput = a})

-- | Use these settings to reorder the audio channels of one input to match those of another input. This allows you to combine the two files into a single output, one after the other.
asRemixSettings :: Lens' AudioSelector (Maybe RemixSettings)
asRemixSettings = lens _asRemixSettings (\ s a -> s{_asRemixSettings = a})

instance FromJSON AudioSelector where
        parseJSON
          = withObject "AudioSelector"
              (\ x ->
                 AudioSelector' <$>
                   (x .:? "tracks" .!= mempty) <*>
                     (x .:? "customLanguageCode")
                     <*> (x .:? "programSelection")
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
                  ("customLanguageCode" .=) <$> _asCustomLanguageCode,
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
-- * 'asgAudioSelectorNames' - Name of an Audio Selector within the same input to include in the group.  Audio selector names are standardized, based on their order within the input (e.g., "Audio Selector 1"). The audio selector name parameter can be repeated to add any number of audio selectors to the group.
audioSelectorGroup
    :: AudioSelectorGroup
audioSelectorGroup = AudioSelectorGroup' {_asgAudioSelectorNames = Nothing}


-- | Name of an Audio Selector within the same input to include in the group.  Audio selector names are standardized, based on their order within the input (e.g., "Audio Selector 1"). The audio selector name parameter can be repeated to add any number of audio selectors to the group.
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
  { _bdsBackgroundOpacity :: !(Maybe Nat)
  , _bdsFontOpacity       :: !(Maybe Nat)
  , _bdsShadowYOffset     :: !(Maybe Int)
  , _bdsFontResolution    :: !(Maybe Nat)
  , _bdsYPosition         :: !(Maybe Nat)
  , _bdsBackgroundColor   :: !(Maybe BurninSubtitleBackgroundColor)
  , _bdsShadowXOffset     :: !(Maybe Int)
  , _bdsFontSize          :: !(Maybe Nat)
  , _bdsXPosition         :: !(Maybe Nat)
  , _bdsTeletextSpacing   :: !(Maybe BurninSubtitleTeletextSpacing)
  , _bdsFontScript        :: !(Maybe FontScript)
  , _bdsAlignment         :: !(Maybe BurninSubtitleAlignment)
  , _bdsShadowOpacity     :: !(Maybe Nat)
  , _bdsOutlineColor      :: !(Maybe BurninSubtitleOutlineColor)
  , _bdsOutlineSize       :: !(Maybe Nat)
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
-- * 'bdsFontScript' - Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering burn-in captions.
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
    , _bdsFontScript = Nothing
    , _bdsAlignment = Nothing
    , _bdsShadowOpacity = Nothing
    , _bdsOutlineColor = Nothing
    , _bdsOutlineSize = Nothing
    , _bdsShadowColor = Nothing
    , _bdsFontColor = Nothing
    }


-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
bdsBackgroundOpacity :: Lens' BurninDestinationSettings (Maybe Natural)
bdsBackgroundOpacity = lens _bdsBackgroundOpacity (\ s a -> s{_bdsBackgroundOpacity = a}) . mapping _Nat

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent. All burn-in and DVB-Sub font settings must match.
bdsFontOpacity :: Lens' BurninDestinationSettings (Maybe Natural)
bdsFontOpacity = lens _bdsFontOpacity (\ s a -> s{_bdsFontOpacity = a}) . mapping _Nat

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
bdsShadowYOffset :: Lens' BurninDestinationSettings (Maybe Int)
bdsShadowYOffset = lens _bdsShadowYOffset (\ s a -> s{_bdsShadowYOffset = a})

-- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in and DVB-Sub font settings must match.
bdsFontResolution :: Lens' BurninDestinationSettings (Maybe Natural)
bdsFontResolution = lens _bdsFontResolution (\ s a -> s{_bdsFontResolution = a}) . mapping _Nat

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bdsYPosition :: Lens' BurninDestinationSettings (Maybe Natural)
bdsYPosition = lens _bdsYPosition (\ s a -> s{_bdsYPosition = a}) . mapping _Nat

-- | Undocumented member.
bdsBackgroundColor :: Lens' BurninDestinationSettings (Maybe BurninSubtitleBackgroundColor)
bdsBackgroundColor = lens _bdsBackgroundColor (\ s a -> s{_bdsBackgroundColor = a})

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
bdsShadowXOffset :: Lens' BurninDestinationSettings (Maybe Int)
bdsShadowXOffset = lens _bdsShadowXOffset (\ s a -> s{_bdsShadowXOffset = a})

-- | A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
bdsFontSize :: Lens' BurninDestinationSettings (Maybe Natural)
bdsFontSize = lens _bdsFontSize (\ s a -> s{_bdsFontSize = a}) . mapping _Nat

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bdsXPosition :: Lens' BurninDestinationSettings (Maybe Natural)
bdsXPosition = lens _bdsXPosition (\ s a -> s{_bdsXPosition = a}) . mapping _Nat

-- | Undocumented member.
bdsTeletextSpacing :: Lens' BurninDestinationSettings (Maybe BurninSubtitleTeletextSpacing)
bdsTeletextSpacing = lens _bdsTeletextSpacing (\ s a -> s{_bdsTeletextSpacing = a})

-- | Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering burn-in captions.
bdsFontScript :: Lens' BurninDestinationSettings (Maybe FontScript)
bdsFontScript = lens _bdsFontScript (\ s a -> s{_bdsFontScript = a})

-- | Undocumented member.
bdsAlignment :: Lens' BurninDestinationSettings (Maybe BurninSubtitleAlignment)
bdsAlignment = lens _bdsAlignment (\ s a -> s{_bdsAlignment = a})

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
bdsShadowOpacity :: Lens' BurninDestinationSettings (Maybe Natural)
bdsShadowOpacity = lens _bdsShadowOpacity (\ s a -> s{_bdsShadowOpacity = a}) . mapping _Nat

-- | Undocumented member.
bdsOutlineColor :: Lens' BurninDestinationSettings (Maybe BurninSubtitleOutlineColor)
bdsOutlineColor = lens _bdsOutlineColor (\ s a -> s{_bdsOutlineColor = a})

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
bdsOutlineSize :: Lens' BurninDestinationSettings (Maybe Natural)
bdsOutlineSize = lens _bdsOutlineSize (\ s a -> s{_bdsOutlineSize = a}) . mapping _Nat

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
                     <*> (x .:? "fontScript")
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
                  ("fontScript" .=) <$> _bdsFontScript,
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
  , _cdCustomLanguageCode  :: !(Maybe Text)
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
-- * 'cdCustomLanguageCode' - Indicates the language of the caption output track, using the ISO 639-2 or ISO 639-3 three-letter language code. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
--
-- * 'cdLanguageCode' - Specify the language of this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
--
-- * 'cdDestinationSettings' - Undocumented member.
--
-- * 'cdLanguageDescription' - Human readable information to indicate captions available for players (eg. English, or Spanish). Alphanumeric characters, spaces, and underscore are legal.
captionDescription
    :: CaptionDescription
captionDescription =
  CaptionDescription'
    { _cdCaptionSelectorName = Nothing
    , _cdCustomLanguageCode = Nothing
    , _cdLanguageCode = Nothing
    , _cdDestinationSettings = Nothing
    , _cdLanguageDescription = Nothing
    }


-- | <N>", which denotes that the Nth Caption Selector will be used from each input.
cdCaptionSelectorName :: Lens' CaptionDescription (Maybe Text)
cdCaptionSelectorName = lens _cdCaptionSelectorName (\ s a -> s{_cdCaptionSelectorName = a})

-- | Indicates the language of the caption output track, using the ISO 639-2 or ISO 639-3 three-letter language code. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
cdCustomLanguageCode :: Lens' CaptionDescription (Maybe Text)
cdCustomLanguageCode = lens _cdCustomLanguageCode (\ s a -> s{_cdCustomLanguageCode = a})

-- | Specify the language of this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
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
                     (x .:? "customLanguageCode")
                     <*> (x .:? "languageCode")
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
                  ("customLanguageCode" .=) <$> _cdCustomLanguageCode,
                  ("languageCode" .=) <$> _cdLanguageCode,
                  ("destinationSettings" .=) <$>
                    _cdDestinationSettings,
                  ("languageDescription" .=) <$>
                    _cdLanguageDescription])

-- | Caption Description for preset
--
-- /See:/ 'captionDescriptionPreset' smart constructor.
data CaptionDescriptionPreset = CaptionDescriptionPreset'
  { _cdpCustomLanguageCode  :: !(Maybe Text)
  , _cdpLanguageCode        :: !(Maybe LanguageCode)
  , _cdpDestinationSettings :: !(Maybe CaptionDestinationSettings)
  , _cdpLanguageDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CaptionDescriptionPreset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdpCustomLanguageCode' - Indicates the language of the caption output track, using the ISO 639-2 or ISO 639-3 three-letter language code. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
--
-- * 'cdpLanguageCode' - Specify the language of this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
--
-- * 'cdpDestinationSettings' - Undocumented member.
--
-- * 'cdpLanguageDescription' - Human readable information to indicate captions available for players (eg. English, or Spanish). Alphanumeric characters, spaces, and underscore are legal.
captionDescriptionPreset
    :: CaptionDescriptionPreset
captionDescriptionPreset =
  CaptionDescriptionPreset'
    { _cdpCustomLanguageCode = Nothing
    , _cdpLanguageCode = Nothing
    , _cdpDestinationSettings = Nothing
    , _cdpLanguageDescription = Nothing
    }


-- | Indicates the language of the caption output track, using the ISO 639-2 or ISO 639-3 three-letter language code. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
cdpCustomLanguageCode :: Lens' CaptionDescriptionPreset (Maybe Text)
cdpCustomLanguageCode = lens _cdpCustomLanguageCode (\ s a -> s{_cdpCustomLanguageCode = a})

-- | Specify the language of this captions output track. For most captions output formats, the encoder puts this language information in the output captions metadata. If your output captions format is DVB-Sub or Burn in, the encoder uses this language information to choose the font language for rendering the captions text.
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
                   (x .:? "customLanguageCode") <*>
                     (x .:? "languageCode")
                     <*> (x .:? "destinationSettings")
                     <*> (x .:? "languageDescription"))

instance Hashable CaptionDescriptionPreset where

instance NFData CaptionDescriptionPreset where

instance ToJSON CaptionDescriptionPreset where
        toJSON CaptionDescriptionPreset'{..}
          = object
              (catMaybes
                 [("customLanguageCode" .=) <$>
                    _cdpCustomLanguageCode,
                  ("languageCode" .=) <$> _cdpLanguageCode,
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
  , _cdsEmbeddedDestinationSettings :: !(Maybe EmbeddedDestinationSettings)
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
-- * 'cdsEmbeddedDestinationSettings' - Undocumented member.
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
    , _cdsEmbeddedDestinationSettings = Nothing
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
cdsEmbeddedDestinationSettings :: Lens' CaptionDestinationSettings (Maybe EmbeddedDestinationSettings)
cdsEmbeddedDestinationSettings = lens _cdsEmbeddedDestinationSettings (\ s a -> s{_cdsEmbeddedDestinationSettings = a})

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
                     <*> (x .:? "embeddedDestinationSettings")
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
                  ("embeddedDestinationSettings" .=) <$>
                    _cdsEmbeddedDestinationSettings,
                  ("sccDestinationSettings" .=) <$>
                    _cdsSccDestinationSettings,
                  ("burninDestinationSettings" .=) <$>
                    _cdsBurninDestinationSettings])

-- | Set up captions in your outputs by first selecting them from your input here.
--
-- /See:/ 'captionSelector' smart constructor.
data CaptionSelector = CaptionSelector'
  { _csCustomLanguageCode :: !(Maybe Text)
  , _csLanguageCode       :: !(Maybe LanguageCode)
  , _csSourceSettings     :: !(Maybe CaptionSourceSettings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CaptionSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCustomLanguageCode' - The specific language to extract from source, using the ISO 639-2 or ISO 639-3 three-letter language code. If input is SCTE-27, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub that is being passed through, omit this field (and PID field); there is no way to extract a specific language with pass-through captions.
--
-- * 'csLanguageCode' - The specific language to extract from source. If input is SCTE-27, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub that is being passed through, omit this field (and PID field); there is no way to extract a specific language with pass-through captions.
--
-- * 'csSourceSettings' - Undocumented member.
captionSelector
    :: CaptionSelector
captionSelector =
  CaptionSelector'
    { _csCustomLanguageCode = Nothing
    , _csLanguageCode = Nothing
    , _csSourceSettings = Nothing
    }


-- | The specific language to extract from source, using the ISO 639-2 or ISO 639-3 three-letter language code. If input is SCTE-27, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub and output is Burn-in or SMPTE-TT, complete this field and/or PID to select the caption language to extract. If input is DVB-Sub that is being passed through, omit this field (and PID field); there is no way to extract a specific language with pass-through captions.
csCustomLanguageCode :: Lens' CaptionSelector (Maybe Text)
csCustomLanguageCode = lens _csCustomLanguageCode (\ s a -> s{_csCustomLanguageCode = a})

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
                   (x .:? "customLanguageCode") <*>
                     (x .:? "languageCode")
                     <*> (x .:? "sourceSettings"))

instance Hashable CaptionSelector where

instance NFData CaptionSelector where

instance ToJSON CaptionSelector where
        toJSON CaptionSelector'{..}
          = object
              (catMaybes
                 [("customLanguageCode" .=) <$> _csCustomLanguageCode,
                  ("languageCode" .=) <$> _csLanguageCode,
                  ("sourceSettings" .=) <$> _csSourceSettings])

-- | Source settings (SourceSettings) contains the group of settings for captions in the input.
--
-- /See:/ 'captionSourceSettings' smart constructor.
data CaptionSourceSettings = CaptionSourceSettings'
  { _cssTeletextSourceSettings  :: !(Maybe TeletextSourceSettings)
  , _cssSourceType              :: !(Maybe CaptionSourceType)
  , _cssFileSourceSettings      :: !(Maybe FileSourceSettings)
  , _cssDvbSubSourceSettings    :: !(Maybe DvbSubSourceSettings)
  , _cssTrackSourceSettings     :: !(Maybe TrackSourceSettings)
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
-- * 'cssTrackSourceSettings' - Undocumented member.
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
    , _cssTrackSourceSettings = Nothing
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
cssTrackSourceSettings :: Lens' CaptionSourceSettings (Maybe TrackSourceSettings)
cssTrackSourceSettings = lens _cssTrackSourceSettings (\ s a -> s{_cssTrackSourceSettings = a})

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
                     <*> (x .:? "trackSourceSettings")
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
                  ("trackSourceSettings" .=) <$>
                    _cssTrackSourceSettings,
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

-- | Settings for CMAF encryption
--
-- /See:/ 'cmafEncryptionSettings' smart constructor.
data CmafEncryptionSettings = CmafEncryptionSettings'
  { _cesEncryptionMethod :: !(Maybe CmafEncryptionType)
  , _cesConstantInitializationVector :: !(Maybe Text)
  , _cesType :: !(Maybe CmafKeyProviderType)
  , _cesStaticKeyProvider :: !(Maybe StaticKeyProvider)
  , _cesInitializationVectorInManifest :: !(Maybe CmafInitializationVectorInManifest)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CmafEncryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cesEncryptionMethod' - Undocumented member.
--
-- * 'cesConstantInitializationVector' - This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
--
-- * 'cesType' - Undocumented member.
--
-- * 'cesStaticKeyProvider' - Undocumented member.
--
-- * 'cesInitializationVectorInManifest' - Undocumented member.
cmafEncryptionSettings
    :: CmafEncryptionSettings
cmafEncryptionSettings =
  CmafEncryptionSettings'
    { _cesEncryptionMethod = Nothing
    , _cesConstantInitializationVector = Nothing
    , _cesType = Nothing
    , _cesStaticKeyProvider = Nothing
    , _cesInitializationVectorInManifest = Nothing
    }


-- | Undocumented member.
cesEncryptionMethod :: Lens' CmafEncryptionSettings (Maybe CmafEncryptionType)
cesEncryptionMethod = lens _cesEncryptionMethod (\ s a -> s{_cesEncryptionMethod = a})

-- | This is a 128-bit, 16-byte hex value represented by a 32-character text string. If this parameter is not set then the Initialization Vector will follow the segment number by default.
cesConstantInitializationVector :: Lens' CmafEncryptionSettings (Maybe Text)
cesConstantInitializationVector = lens _cesConstantInitializationVector (\ s a -> s{_cesConstantInitializationVector = a})

-- | Undocumented member.
cesType :: Lens' CmafEncryptionSettings (Maybe CmafKeyProviderType)
cesType = lens _cesType (\ s a -> s{_cesType = a})

-- | Undocumented member.
cesStaticKeyProvider :: Lens' CmafEncryptionSettings (Maybe StaticKeyProvider)
cesStaticKeyProvider = lens _cesStaticKeyProvider (\ s a -> s{_cesStaticKeyProvider = a})

-- | Undocumented member.
cesInitializationVectorInManifest :: Lens' CmafEncryptionSettings (Maybe CmafInitializationVectorInManifest)
cesInitializationVectorInManifest = lens _cesInitializationVectorInManifest (\ s a -> s{_cesInitializationVectorInManifest = a})

instance FromJSON CmafEncryptionSettings where
        parseJSON
          = withObject "CmafEncryptionSettings"
              (\ x ->
                 CmafEncryptionSettings' <$>
                   (x .:? "encryptionMethod") <*>
                     (x .:? "constantInitializationVector")
                     <*> (x .:? "type")
                     <*> (x .:? "staticKeyProvider")
                     <*> (x .:? "initializationVectorInManifest"))

instance Hashable CmafEncryptionSettings where

instance NFData CmafEncryptionSettings where

instance ToJSON CmafEncryptionSettings where
        toJSON CmafEncryptionSettings'{..}
          = object
              (catMaybes
                 [("encryptionMethod" .=) <$> _cesEncryptionMethod,
                  ("constantInitializationVector" .=) <$>
                    _cesConstantInitializationVector,
                  ("type" .=) <$> _cesType,
                  ("staticKeyProvider" .=) <$> _cesStaticKeyProvider,
                  ("initializationVectorInManifest" .=) <$>
                    _cesInitializationVectorInManifest])

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only contain a single video, audio, or caption output.
--
-- /See:/ 'cmafGroupSettings' smart constructor.
data CmafGroupSettings = CmafGroupSettings'
  { _cgsFragmentLength         :: !(Maybe Nat)
  , _cgsSegmentControl         :: !(Maybe CmafSegmentControl)
  , _cgsDestination            :: !(Maybe Text)
  , _cgsMinBufferTime          :: !(Maybe Nat)
  , _cgsWriteHlsManifest       :: !(Maybe CmafWriteHLSManifest)
  , _cgsCodecSpecification     :: !(Maybe CmafCodecSpecification)
  , _cgsBaseURL                :: !(Maybe Text)
  , _cgsMinFinalSegmentLength  :: !(Maybe Double)
  , _cgsWriteDashManifest      :: !(Maybe CmafWriteDASHManifest)
  , _cgsEncryption             :: !(Maybe CmafEncryptionSettings)
  , _cgsSegmentLength          :: !(Maybe Nat)
  , _cgsManifestDurationFormat :: !(Maybe CmafManifestDurationFormat)
  , _cgsClientCache            :: !(Maybe CmafClientCache)
  , _cgsStreamInfResolution    :: !(Maybe CmafStreamInfResolution)
  , _cgsManifestCompression    :: !(Maybe CmafManifestCompression)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CmafGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsFragmentLength' - Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
--
-- * 'cgsSegmentControl' - Undocumented member.
--
-- * 'cgsDestination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- * 'cgsMinBufferTime' - Minimum time of initially buffered media that is needed to ensure smooth playout.
--
-- * 'cgsWriteHlsManifest' - Undocumented member.
--
-- * 'cgsCodecSpecification' - Undocumented member.
--
-- * 'cgsBaseURL' - A partial URI prefix that will be put in the manifest file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
--
-- * 'cgsMinFinalSegmentLength' - Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
--
-- * 'cgsWriteDashManifest' - Undocumented member.
--
-- * 'cgsEncryption' - DRM settings.
--
-- * 'cgsSegmentLength' - Use this setting to specify the length, in seconds, of each individual CMAF segment. This value applies to the whole package; that is, to every output in the output group. Note that segments end on the first keyframe after this number of seconds, so the actual segment length might be slightly longer. If you set Segment control (CmafSegmentControl) to single file, the service puts the content of each output in a single file that has metadata that marks these segments. If you set it to segmented files, the service creates multiple files for each output, each with the content of one segment.
--
-- * 'cgsManifestDurationFormat' - Undocumented member.
--
-- * 'cgsClientCache' - Undocumented member.
--
-- * 'cgsStreamInfResolution' - Undocumented member.
--
-- * 'cgsManifestCompression' - Undocumented member.
cmafGroupSettings
    :: CmafGroupSettings
cmafGroupSettings =
  CmafGroupSettings'
    { _cgsFragmentLength = Nothing
    , _cgsSegmentControl = Nothing
    , _cgsDestination = Nothing
    , _cgsMinBufferTime = Nothing
    , _cgsWriteHlsManifest = Nothing
    , _cgsCodecSpecification = Nothing
    , _cgsBaseURL = Nothing
    , _cgsMinFinalSegmentLength = Nothing
    , _cgsWriteDashManifest = Nothing
    , _cgsEncryption = Nothing
    , _cgsSegmentLength = Nothing
    , _cgsManifestDurationFormat = Nothing
    , _cgsClientCache = Nothing
    , _cgsStreamInfResolution = Nothing
    , _cgsManifestCompression = Nothing
    }


-- | Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
cgsFragmentLength :: Lens' CmafGroupSettings (Maybe Natural)
cgsFragmentLength = lens _cgsFragmentLength (\ s a -> s{_cgsFragmentLength = a}) . mapping _Nat

-- | Undocumented member.
cgsSegmentControl :: Lens' CmafGroupSettings (Maybe CmafSegmentControl)
cgsSegmentControl = lens _cgsSegmentControl (\ s a -> s{_cgsSegmentControl = a})

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
cgsDestination :: Lens' CmafGroupSettings (Maybe Text)
cgsDestination = lens _cgsDestination (\ s a -> s{_cgsDestination = a})

-- | Minimum time of initially buffered media that is needed to ensure smooth playout.
cgsMinBufferTime :: Lens' CmafGroupSettings (Maybe Natural)
cgsMinBufferTime = lens _cgsMinBufferTime (\ s a -> s{_cgsMinBufferTime = a}) . mapping _Nat

-- | Undocumented member.
cgsWriteHlsManifest :: Lens' CmafGroupSettings (Maybe CmafWriteHLSManifest)
cgsWriteHlsManifest = lens _cgsWriteHlsManifest (\ s a -> s{_cgsWriteHlsManifest = a})

-- | Undocumented member.
cgsCodecSpecification :: Lens' CmafGroupSettings (Maybe CmafCodecSpecification)
cgsCodecSpecification = lens _cgsCodecSpecification (\ s a -> s{_cgsCodecSpecification = a})

-- | A partial URI prefix that will be put in the manifest file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
cgsBaseURL :: Lens' CmafGroupSettings (Maybe Text)
cgsBaseURL = lens _cgsBaseURL (\ s a -> s{_cgsBaseURL = a})

-- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
cgsMinFinalSegmentLength :: Lens' CmafGroupSettings (Maybe Double)
cgsMinFinalSegmentLength = lens _cgsMinFinalSegmentLength (\ s a -> s{_cgsMinFinalSegmentLength = a})

-- | Undocumented member.
cgsWriteDashManifest :: Lens' CmafGroupSettings (Maybe CmafWriteDASHManifest)
cgsWriteDashManifest = lens _cgsWriteDashManifest (\ s a -> s{_cgsWriteDashManifest = a})

-- | DRM settings.
cgsEncryption :: Lens' CmafGroupSettings (Maybe CmafEncryptionSettings)
cgsEncryption = lens _cgsEncryption (\ s a -> s{_cgsEncryption = a})

-- | Use this setting to specify the length, in seconds, of each individual CMAF segment. This value applies to the whole package; that is, to every output in the output group. Note that segments end on the first keyframe after this number of seconds, so the actual segment length might be slightly longer. If you set Segment control (CmafSegmentControl) to single file, the service puts the content of each output in a single file that has metadata that marks these segments. If you set it to segmented files, the service creates multiple files for each output, each with the content of one segment.
cgsSegmentLength :: Lens' CmafGroupSettings (Maybe Natural)
cgsSegmentLength = lens _cgsSegmentLength (\ s a -> s{_cgsSegmentLength = a}) . mapping _Nat

-- | Undocumented member.
cgsManifestDurationFormat :: Lens' CmafGroupSettings (Maybe CmafManifestDurationFormat)
cgsManifestDurationFormat = lens _cgsManifestDurationFormat (\ s a -> s{_cgsManifestDurationFormat = a})

-- | Undocumented member.
cgsClientCache :: Lens' CmafGroupSettings (Maybe CmafClientCache)
cgsClientCache = lens _cgsClientCache (\ s a -> s{_cgsClientCache = a})

-- | Undocumented member.
cgsStreamInfResolution :: Lens' CmafGroupSettings (Maybe CmafStreamInfResolution)
cgsStreamInfResolution = lens _cgsStreamInfResolution (\ s a -> s{_cgsStreamInfResolution = a})

-- | Undocumented member.
cgsManifestCompression :: Lens' CmafGroupSettings (Maybe CmafManifestCompression)
cgsManifestCompression = lens _cgsManifestCompression (\ s a -> s{_cgsManifestCompression = a})

instance FromJSON CmafGroupSettings where
        parseJSON
          = withObject "CmafGroupSettings"
              (\ x ->
                 CmafGroupSettings' <$>
                   (x .:? "fragmentLength") <*> (x .:? "segmentControl")
                     <*> (x .:? "destination")
                     <*> (x .:? "minBufferTime")
                     <*> (x .:? "writeHlsManifest")
                     <*> (x .:? "codecSpecification")
                     <*> (x .:? "baseUrl")
                     <*> (x .:? "minFinalSegmentLength")
                     <*> (x .:? "writeDashManifest")
                     <*> (x .:? "encryption")
                     <*> (x .:? "segmentLength")
                     <*> (x .:? "manifestDurationFormat")
                     <*> (x .:? "clientCache")
                     <*> (x .:? "streamInfResolution")
                     <*> (x .:? "manifestCompression"))

instance Hashable CmafGroupSettings where

instance NFData CmafGroupSettings where

instance ToJSON CmafGroupSettings where
        toJSON CmafGroupSettings'{..}
          = object
              (catMaybes
                 [("fragmentLength" .=) <$> _cgsFragmentLength,
                  ("segmentControl" .=) <$> _cgsSegmentControl,
                  ("destination" .=) <$> _cgsDestination,
                  ("minBufferTime" .=) <$> _cgsMinBufferTime,
                  ("writeHlsManifest" .=) <$> _cgsWriteHlsManifest,
                  ("codecSpecification" .=) <$> _cgsCodecSpecification,
                  ("baseUrl" .=) <$> _cgsBaseURL,
                  ("minFinalSegmentLength" .=) <$>
                    _cgsMinFinalSegmentLength,
                  ("writeDashManifest" .=) <$> _cgsWriteDashManifest,
                  ("encryption" .=) <$> _cgsEncryption,
                  ("segmentLength" .=) <$> _cgsSegmentLength,
                  ("manifestDurationFormat" .=) <$>
                    _cgsManifestDurationFormat,
                  ("clientCache" .=) <$> _cgsClientCache,
                  ("streamInfResolution" .=) <$>
                    _cgsStreamInfResolution,
                  ("manifestCompression" .=) <$>
                    _cgsManifestCompression])

-- | Settings for color correction.
--
-- /See:/ 'colorCorrector' smart constructor.
data ColorCorrector = ColorCorrector'
  { _ccSaturation           :: !(Maybe Nat)
  , _ccHue                  :: !(Maybe Int)
  , _ccColorSpaceConversion :: !(Maybe ColorSpaceConversion)
  , _ccHdr10Metadata        :: !(Maybe Hdr10Metadata)
  , _ccContrast             :: !(Maybe Nat)
  , _ccBrightness           :: !(Maybe Nat)
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
-- * 'ccHdr10Metadata' - Use the HDR master display (Hdr10Metadata) settings to correct HDR metadata or to provide missing metadata. Note that these settings are not color correction.
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
ccSaturation :: Lens' ColorCorrector (Maybe Natural)
ccSaturation = lens _ccSaturation (\ s a -> s{_ccSaturation = a}) . mapping _Nat

-- | Hue in degrees.
ccHue :: Lens' ColorCorrector (Maybe Int)
ccHue = lens _ccHue (\ s a -> s{_ccHue = a})

-- | Undocumented member.
ccColorSpaceConversion :: Lens' ColorCorrector (Maybe ColorSpaceConversion)
ccColorSpaceConversion = lens _ccColorSpaceConversion (\ s a -> s{_ccColorSpaceConversion = a})

-- | Use the HDR master display (Hdr10Metadata) settings to correct HDR metadata or to provide missing metadata. Note that these settings are not color correction.
ccHdr10Metadata :: Lens' ColorCorrector (Maybe Hdr10Metadata)
ccHdr10Metadata = lens _ccHdr10Metadata (\ s a -> s{_ccHdr10Metadata = a})

-- | Contrast level.
ccContrast :: Lens' ColorCorrector (Maybe Natural)
ccContrast = lens _ccContrast (\ s a -> s{_ccContrast = a}) . mapping _Nat

-- | Brightness level.
ccBrightness :: Lens' ColorCorrector (Maybe Natural)
ccBrightness = lens _ccBrightness (\ s a -> s{_ccBrightness = a}) . mapping _Nat

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
  { _digsFragmentLength :: !(Maybe Nat)
  , _digsSegmentControl :: !(Maybe DashIsoSegmentControl)
  , _digsDestination :: !(Maybe Text)
  , _digsHbbtvCompliance :: !(Maybe DashIsoHbbtvCompliance)
  , _digsMinBufferTime :: !(Maybe Nat)
  , _digsBaseURL :: !(Maybe Text)
  , _digsEncryption :: !(Maybe DashIsoEncryptionSettings)
  , _digsSegmentLength :: !(Maybe Nat)
  , _digsWriteSegmentTimelineInRepresentation :: !(Maybe DashIsoWriteSegmentTimelineInRepresentation)
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
--
-- * 'digsWriteSegmentTimelineInRepresentation' - When you enable Precise segment duration in manifests (writeSegmentTimelineInRepresentation), your DASH manifest shows precise segment durations. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When this feature isn't enabled, the segment durations in your DASH manifest are approximate. The segment duration information appears in the duration attribute of the SegmentTemplate element.
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
    , _digsWriteSegmentTimelineInRepresentation = Nothing
    }


-- | Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
digsFragmentLength :: Lens' DashIsoGroupSettings (Maybe Natural)
digsFragmentLength = lens _digsFragmentLength (\ s a -> s{_digsFragmentLength = a}) . mapping _Nat

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
digsMinBufferTime :: Lens' DashIsoGroupSettings (Maybe Natural)
digsMinBufferTime = lens _digsMinBufferTime (\ s a -> s{_digsMinBufferTime = a}) . mapping _Nat

-- | A partial URI prefix that will be put in the manifest (.mpd) file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
digsBaseURL :: Lens' DashIsoGroupSettings (Maybe Text)
digsBaseURL = lens _digsBaseURL (\ s a -> s{_digsBaseURL = a})

-- | DRM settings.
digsEncryption :: Lens' DashIsoGroupSettings (Maybe DashIsoEncryptionSettings)
digsEncryption = lens _digsEncryption (\ s a -> s{_digsEncryption = a})

-- | Length of mpd segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer. When Emit Single File is checked, the segmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
digsSegmentLength :: Lens' DashIsoGroupSettings (Maybe Natural)
digsSegmentLength = lens _digsSegmentLength (\ s a -> s{_digsSegmentLength = a}) . mapping _Nat

-- | When you enable Precise segment duration in manifests (writeSegmentTimelineInRepresentation), your DASH manifest shows precise segment durations. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When this feature isn't enabled, the segment durations in your DASH manifest are approximate. The segment duration information appears in the duration attribute of the SegmentTemplate element.
digsWriteSegmentTimelineInRepresentation :: Lens' DashIsoGroupSettings (Maybe DashIsoWriteSegmentTimelineInRepresentation)
digsWriteSegmentTimelineInRepresentation = lens _digsWriteSegmentTimelineInRepresentation (\ s a -> s{_digsWriteSegmentTimelineInRepresentation = a})

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
                     <*> (x .:? "segmentLength")
                     <*> (x .:? "writeSegmentTimelineInRepresentation"))

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
                  ("segmentLength" .=) <$> _digsSegmentLength,
                  ("writeSegmentTimelineInRepresentation" .=) <$>
                    _digsWriteSegmentTimelineInRepresentation])

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
  { _dnsNetworkId   :: !(Maybe Nat)
  , _dnsNetworkName :: !(Maybe Text)
  , _dnsNitInterval :: !(Maybe Nat)
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
dnsNetworkId :: Lens' DvbNitSettings (Maybe Natural)
dnsNetworkId = lens _dnsNetworkId (\ s a -> s{_dnsNetworkId = a}) . mapping _Nat

-- | The network name text placed in the network_name_descriptor inside the Network Information Table. Maximum length is 256 characters.
dnsNetworkName :: Lens' DvbNitSettings (Maybe Text)
dnsNetworkName = lens _dnsNetworkName (\ s a -> s{_dnsNetworkName = a})

-- | The number of milliseconds between instances of this table in the output transport stream.
dnsNitInterval :: Lens' DvbNitSettings (Maybe Natural)
dnsNitInterval = lens _dnsNitInterval (\ s a -> s{_dnsNitInterval = a}) . mapping _Nat

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
  { _dssSdtInterval         :: !(Maybe Nat)
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
dssSdtInterval :: Lens' DvbSdtSettings (Maybe Natural)
dssSdtInterval = lens _dssSdtInterval (\ s a -> s{_dssSdtInterval = a}) . mapping _Nat

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
  { _dsdsBackgroundOpacity :: !(Maybe Nat)
  , _dsdsFontOpacity       :: !(Maybe Nat)
  , _dsdsShadowYOffset     :: !(Maybe Int)
  , _dsdsFontResolution    :: !(Maybe Nat)
  , _dsdsYPosition         :: !(Maybe Nat)
  , _dsdsBackgroundColor   :: !(Maybe DvbSubtitleBackgroundColor)
  , _dsdsShadowXOffset     :: !(Maybe Int)
  , _dsdsFontSize          :: !(Maybe Nat)
  , _dsdsXPosition         :: !(Maybe Nat)
  , _dsdsTeletextSpacing   :: !(Maybe DvbSubtitleTeletextSpacing)
  , _dsdsFontScript        :: !(Maybe FontScript)
  , _dsdsAlignment         :: !(Maybe DvbSubtitleAlignment)
  , _dsdsShadowOpacity     :: !(Maybe Nat)
  , _dsdsOutlineColor      :: !(Maybe DvbSubtitleOutlineColor)
  , _dsdsOutlineSize       :: !(Maybe Nat)
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
-- * 'dsdsFontScript' - Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering DVB-Sub captions.
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
    , _dsdsFontScript = Nothing
    , _dsdsAlignment = Nothing
    , _dsdsShadowOpacity = Nothing
    , _dsdsOutlineColor = Nothing
    , _dsdsOutlineSize = Nothing
    , _dsdsShadowColor = Nothing
    , _dsdsFontColor = Nothing
    }


-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
dsdsBackgroundOpacity :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsBackgroundOpacity = lens _dsdsBackgroundOpacity (\ s a -> s{_dsdsBackgroundOpacity = a}) . mapping _Nat

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is transparent. All burn-in and DVB-Sub font settings must match.
dsdsFontOpacity :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsFontOpacity = lens _dsdsFontOpacity (\ s a -> s{_dsdsFontOpacity = a}) . mapping _Nat

-- | Specifies the vertical offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels above the text. All burn-in and DVB-Sub font settings must match.
dsdsShadowYOffset :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsShadowYOffset = lens _dsdsShadowYOffset (\ s a -> s{_dsdsShadowYOffset = a})

-- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in and DVB-Sub font settings must match.
dsdsFontResolution :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsFontResolution = lens _dsdsFontResolution (\ s a -> s{_dsdsFontResolution = a}) . mapping _Nat

-- | Specifies the vertical position of the caption relative to the top of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the top of the output. If no explicit y_position is provided, the caption will be positioned towards the bottom of the output. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsYPosition :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsYPosition = lens _dsdsYPosition (\ s a -> s{_dsdsYPosition = a}) . mapping _Nat

-- | Undocumented member.
dsdsBackgroundColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleBackgroundColor)
dsdsBackgroundColor = lens _dsdsBackgroundColor (\ s a -> s{_dsdsBackgroundColor = a})

-- | Specifies the horizontal offset of the shadow relative to the captions in pixels. A value of -2 would result in a shadow offset 2 pixels to the left. All burn-in and DVB-Sub font settings must match.
dsdsShadowXOffset :: Lens' DvbSubDestinationSettings (Maybe Int)
dsdsShadowXOffset = lens _dsdsShadowXOffset (\ s a -> s{_dsdsShadowXOffset = a})

-- | A positive integer indicates the exact font size in points. Set to 0 for automatic font size selection. All burn-in and DVB-Sub font settings must match.
dsdsFontSize :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsFontSize = lens _dsdsFontSize (\ s a -> s{_dsdsFontSize = a}) . mapping _Nat

-- | Specifies the horizontal position of the caption relative to the left side of the output in pixels. A value of 10 would result in the captions starting 10 pixels from the left of the output. If no explicit x_position is provided, the horizontal caption position will be determined by the alignment parameter. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsXPosition :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsXPosition = lens _dsdsXPosition (\ s a -> s{_dsdsXPosition = a}) . mapping _Nat

-- | Undocumented member.
dsdsTeletextSpacing :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleTeletextSpacing)
dsdsTeletextSpacing = lens _dsdsTeletextSpacing (\ s a -> s{_dsdsTeletextSpacing = a})

-- | Provide the font script, using an ISO 15924 script code, if the LanguageCode is not sufficient for determining the script type. Where LanguageCode or CustomLanguageCode is sufficient, use "AUTOMATIC" or leave unset. This is used to help determine the appropriate font for rendering DVB-Sub captions.
dsdsFontScript :: Lens' DvbSubDestinationSettings (Maybe FontScript)
dsdsFontScript = lens _dsdsFontScript (\ s a -> s{_dsdsFontScript = a})

-- | Undocumented member.
dsdsAlignment :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleAlignment)
dsdsAlignment = lens _dsdsAlignment (\ s a -> s{_dsdsAlignment = a})

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent. Leaving this parameter blank is equivalent to setting it to 0 (transparent). All burn-in and DVB-Sub font settings must match.
dsdsShadowOpacity :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsShadowOpacity = lens _dsdsShadowOpacity (\ s a -> s{_dsdsShadowOpacity = a}) . mapping _Nat

-- | Undocumented member.
dsdsOutlineColor :: Lens' DvbSubDestinationSettings (Maybe DvbSubtitleOutlineColor)
dsdsOutlineColor = lens _dsdsOutlineColor (\ s a -> s{_dsdsOutlineColor = a})

-- | Specifies font outline size in pixels. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
dsdsOutlineSize :: Lens' DvbSubDestinationSettings (Maybe Natural)
dsdsOutlineSize = lens _dsdsOutlineSize (\ s a -> s{_dsdsOutlineSize = a}) . mapping _Nat

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
                     <*> (x .:? "fontScript")
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
                  ("fontScript" .=) <$> _dsdsFontScript,
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
  { _dsssPid :: Maybe Nat
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
dsssPid :: Lens' DvbSubSourceSettings (Maybe Natural)
dsssPid = lens _dsssPid (\ s a -> s{_dsssPid = a}) . mapping _Nat

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
  { _dtsTdtInterval :: Maybe Nat
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
dtsTdtInterval :: Lens' DvbTdtSettings (Maybe Natural)
dtsTdtInterval = lens _dtsTdtInterval (\ s a -> s{_dtsTdtInterval = a}) . mapping _Nat

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
  , _esSampleRate                  :: !(Maybe Nat)
  , _esDcFilter                    :: !(Maybe Eac3DcFilter)
  , _esBitrate                     :: !(Maybe Nat)
  , _esPhaseControl                :: !(Maybe Eac3PhaseControl)
  , _esSurroundExMode              :: !(Maybe Eac3SurroundExMode)
  , _esDialnorm                    :: !(Maybe Nat)
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
esSampleRate :: Lens' Eac3Settings (Maybe Natural)
esSampleRate = lens _esSampleRate (\ s a -> s{_esSampleRate = a}) . mapping _Nat

-- | Undocumented member.
esDcFilter :: Lens' Eac3Settings (Maybe Eac3DcFilter)
esDcFilter = lens _esDcFilter (\ s a -> s{_esDcFilter = a})

-- | Average bitrate in bits/second. Valid bitrates depend on the coding mode.
esBitrate :: Lens' Eac3Settings (Maybe Natural)
esBitrate = lens _esBitrate (\ s a -> s{_esBitrate = a}) . mapping _Nat

-- | Undocumented member.
esPhaseControl :: Lens' Eac3Settings (Maybe Eac3PhaseControl)
esPhaseControl = lens _esPhaseControl (\ s a -> s{_esPhaseControl = a})

-- | Undocumented member.
esSurroundExMode :: Lens' Eac3Settings (Maybe Eac3SurroundExMode)
esSurroundExMode = lens _esSurroundExMode (\ s a -> s{_esSurroundExMode = a})

-- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
esDialnorm :: Lens' Eac3Settings (Maybe Natural)
esDialnorm = lens _esDialnorm (\ s a -> s{_esDialnorm = a}) . mapping _Nat

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

-- | Settings specific to embedded/ancillary caption outputs, including 608/708 Channel destination number.
--
-- /See:/ 'embeddedDestinationSettings' smart constructor.
newtype EmbeddedDestinationSettings = EmbeddedDestinationSettings'
  { _edsDestination608ChannelNumber :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EmbeddedDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edsDestination608ChannelNumber' - Ignore this setting unless your input captions are SCC format and your output container is MXF. With this combination of input captions format and output container, you can optionally use this setting to replace the input channel number with the track number that you specify. Specify a different number for each output captions track. If you don't specify an output track number, the system uses the input channel number for the output channel number. This setting applies to each output individually. You can optionally combine two captions channels in your output. The two output channel numbers can be one of the following pairs: 1,3; 2,4; 1,4; or 2,3.
embeddedDestinationSettings
    :: EmbeddedDestinationSettings
embeddedDestinationSettings =
  EmbeddedDestinationSettings' {_edsDestination608ChannelNumber = Nothing}


-- | Ignore this setting unless your input captions are SCC format and your output container is MXF. With this combination of input captions format and output container, you can optionally use this setting to replace the input channel number with the track number that you specify. Specify a different number for each output captions track. If you don't specify an output track number, the system uses the input channel number for the output channel number. This setting applies to each output individually. You can optionally combine two captions channels in your output. The two output channel numbers can be one of the following pairs: 1,3; 2,4; 1,4; or 2,3.
edsDestination608ChannelNumber :: Lens' EmbeddedDestinationSettings (Maybe Natural)
edsDestination608ChannelNumber = lens _edsDestination608ChannelNumber (\ s a -> s{_edsDestination608ChannelNumber = a}) . mapping _Nat

instance FromJSON EmbeddedDestinationSettings where
        parseJSON
          = withObject "EmbeddedDestinationSettings"
              (\ x ->
                 EmbeddedDestinationSettings' <$>
                   (x .:? "destination608ChannelNumber"))

instance Hashable EmbeddedDestinationSettings where

instance NFData EmbeddedDestinationSettings where

instance ToJSON EmbeddedDestinationSettings where
        toJSON EmbeddedDestinationSettings'{..}
          = object
              (catMaybes
                 [("destination608ChannelNumber" .=) <$>
                    _edsDestination608ChannelNumber])

-- | Settings for embedded captions Source
--
-- /See:/ 'embeddedSourceSettings' smart constructor.
data EmbeddedSourceSettings = EmbeddedSourceSettings'
  { _essConvert608To708        :: !(Maybe EmbeddedConvert608To708)
  , _essSource608TrackNumber   :: !(Maybe Nat)
  , _essSource608ChannelNumber :: !(Maybe Nat)
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
essSource608TrackNumber :: Lens' EmbeddedSourceSettings (Maybe Natural)
essSource608TrackNumber = lens _essSource608TrackNumber (\ s a -> s{_essSource608TrackNumber = a}) . mapping _Nat

-- | Specifies the 608/708 channel number within the video track from which to extract captions. Unused for passthrough.
essSource608ChannelNumber :: Lens' EmbeddedSourceSettings (Maybe Natural)
essSource608ChannelNumber = lens _essSource608ChannelNumber (\ s a -> s{_essSource608ChannelNumber = a}) . mapping _Nat

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

-- | Describes an account-specific API endpoint.
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

-- | ESAM ManifestConfirmConditionNotification defined by OC-SP-ESAM-API-I03-131025.
--
-- /See:/ 'esamManifestConfirmConditionNotification' smart constructor.
newtype EsamManifestConfirmConditionNotification = EsamManifestConfirmConditionNotification'
  { _emccnMccXML :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EsamManifestConfirmConditionNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emccnMccXML' - Provide your ESAM ManifestConfirmConditionNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the Manifest Conditioning instructions in the message that you supply.
esamManifestConfirmConditionNotification
    :: EsamManifestConfirmConditionNotification
esamManifestConfirmConditionNotification =
  EsamManifestConfirmConditionNotification' {_emccnMccXML = Nothing}


-- | Provide your ESAM ManifestConfirmConditionNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the Manifest Conditioning instructions in the message that you supply.
emccnMccXML :: Lens' EsamManifestConfirmConditionNotification (Maybe Text)
emccnMccXML = lens _emccnMccXML (\ s a -> s{_emccnMccXML = a})

instance FromJSON
           EsamManifestConfirmConditionNotification
         where
        parseJSON
          = withObject
              "EsamManifestConfirmConditionNotification"
              (\ x ->
                 EsamManifestConfirmConditionNotification' <$>
                   (x .:? "mccXml"))

instance Hashable
           EsamManifestConfirmConditionNotification
         where

instance NFData
           EsamManifestConfirmConditionNotification
         where

instance ToJSON
           EsamManifestConfirmConditionNotification
         where
        toJSON EsamManifestConfirmConditionNotification'{..}
          = object (catMaybes [("mccXml" .=) <$> _emccnMccXML])

-- | Settings for Event Signaling And Messaging (ESAM). If you don't do ad insertion, you can ignore these settings.
--
-- /See:/ 'esamSettings' smart constructor.
data EsamSettings = EsamSettings'
  { _esManifestConfirmConditionNotification :: !(Maybe EsamManifestConfirmConditionNotification)
  , _esResponseSignalPreroll :: !(Maybe Nat)
  , _esSignalProcessingNotification :: !(Maybe EsamSignalProcessingNotification)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EsamSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esManifestConfirmConditionNotification' - Specifies an ESAM ManifestConfirmConditionNotification XML as per OC-SP-ESAM-API-I03-131025. The transcoder uses the manifest conditioning instructions that you provide in the setting MCC XML (mccXml).
--
-- * 'esResponseSignalPreroll' - Specifies the stream distance, in milliseconds, between the SCTE 35 messages that the transcoder places and the splice points that they refer to. If the time between the start of the asset and the SCTE-35 message is less than this value, then the transcoder places the SCTE-35 marker at the beginning of the stream.
--
-- * 'esSignalProcessingNotification' - Specifies an ESAM SignalProcessingNotification XML as per OC-SP-ESAM-API-I03-131025. The transcoder uses the signal processing instructions that you provide in the setting SCC XML (sccXml).
esamSettings
    :: EsamSettings
esamSettings =
  EsamSettings'
    { _esManifestConfirmConditionNotification = Nothing
    , _esResponseSignalPreroll = Nothing
    , _esSignalProcessingNotification = Nothing
    }


-- | Specifies an ESAM ManifestConfirmConditionNotification XML as per OC-SP-ESAM-API-I03-131025. The transcoder uses the manifest conditioning instructions that you provide in the setting MCC XML (mccXml).
esManifestConfirmConditionNotification :: Lens' EsamSettings (Maybe EsamManifestConfirmConditionNotification)
esManifestConfirmConditionNotification = lens _esManifestConfirmConditionNotification (\ s a -> s{_esManifestConfirmConditionNotification = a})

-- | Specifies the stream distance, in milliseconds, between the SCTE 35 messages that the transcoder places and the splice points that they refer to. If the time between the start of the asset and the SCTE-35 message is less than this value, then the transcoder places the SCTE-35 marker at the beginning of the stream.
esResponseSignalPreroll :: Lens' EsamSettings (Maybe Natural)
esResponseSignalPreroll = lens _esResponseSignalPreroll (\ s a -> s{_esResponseSignalPreroll = a}) . mapping _Nat

-- | Specifies an ESAM SignalProcessingNotification XML as per OC-SP-ESAM-API-I03-131025. The transcoder uses the signal processing instructions that you provide in the setting SCC XML (sccXml).
esSignalProcessingNotification :: Lens' EsamSettings (Maybe EsamSignalProcessingNotification)
esSignalProcessingNotification = lens _esSignalProcessingNotification (\ s a -> s{_esSignalProcessingNotification = a})

instance FromJSON EsamSettings where
        parseJSON
          = withObject "EsamSettings"
              (\ x ->
                 EsamSettings' <$>
                   (x .:? "manifestConfirmConditionNotification") <*>
                     (x .:? "responseSignalPreroll")
                     <*> (x .:? "signalProcessingNotification"))

instance Hashable EsamSettings where

instance NFData EsamSettings where

instance ToJSON EsamSettings where
        toJSON EsamSettings'{..}
          = object
              (catMaybes
                 [("manifestConfirmConditionNotification" .=) <$>
                    _esManifestConfirmConditionNotification,
                  ("responseSignalPreroll" .=) <$>
                    _esResponseSignalPreroll,
                  ("signalProcessingNotification" .=) <$>
                    _esSignalProcessingNotification])

-- | ESAM SignalProcessingNotification data defined by OC-SP-ESAM-API-I03-131025.
--
-- /See:/ 'esamSignalProcessingNotification' smart constructor.
newtype EsamSignalProcessingNotification = EsamSignalProcessingNotification'
  { _espnSccXML :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EsamSignalProcessingNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'espnSccXML' - Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the signal processing instructions in the message that you supply. Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. If you want the service to place SCTE-35 markers at the insertion points you specify in the XML document, you must also enable SCTE-35 ESAM (scte35Esam). Note that you can either specify an ESAM XML document or enable SCTE-35 passthrough. You can't do both.
esamSignalProcessingNotification
    :: EsamSignalProcessingNotification
esamSignalProcessingNotification =
  EsamSignalProcessingNotification' {_espnSccXML = Nothing}


-- | Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. Form the XML document as per OC-SP-ESAM-API-I03-131025. The transcoder will use the signal processing instructions in the message that you supply. Provide your ESAM SignalProcessingNotification XML document inside your JSON job settings. If you want the service to place SCTE-35 markers at the insertion points you specify in the XML document, you must also enable SCTE-35 ESAM (scte35Esam). Note that you can either specify an ESAM XML document or enable SCTE-35 passthrough. You can't do both.
espnSccXML :: Lens' EsamSignalProcessingNotification (Maybe Text)
espnSccXML = lens _espnSccXML (\ s a -> s{_espnSccXML = a})

instance FromJSON EsamSignalProcessingNotification
         where
        parseJSON
          = withObject "EsamSignalProcessingNotification"
              (\ x ->
                 EsamSignalProcessingNotification' <$>
                   (x .:? "sccXml"))

instance Hashable EsamSignalProcessingNotification
         where

instance NFData EsamSignalProcessingNotification
         where

instance ToJSON EsamSignalProcessingNotification
         where
        toJSON EsamSignalProcessingNotification'{..}
          = object (catMaybes [("sccXml" .=) <$> _espnSccXML])

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
  { _fcsQuality              :: !(Maybe Nat)
  , _fcsFramerateDenominator :: !(Maybe Nat)
  , _fcsMaxCaptures          :: !(Maybe Nat)
  , _fcsFramerateNumerator   :: !(Maybe Nat)
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
fcsQuality :: Lens' FrameCaptureSettings (Maybe Natural)
fcsQuality = lens _fcsQuality (\ s a -> s{_fcsQuality = a}) . mapping _Nat

-- | Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.n.jpg where n is the 0-based sequence number of each Capture.
fcsFramerateDenominator :: Lens' FrameCaptureSettings (Maybe Natural)
fcsFramerateDenominator = lens _fcsFramerateDenominator (\ s a -> s{_fcsFramerateDenominator = a}) . mapping _Nat

-- | Maximum number of captures (encoded jpg output files).
fcsMaxCaptures :: Lens' FrameCaptureSettings (Maybe Natural)
fcsMaxCaptures = lens _fcsMaxCaptures (\ s a -> s{_fcsMaxCaptures = a}) . mapping _Nat

-- | Frame capture will encode the first frame of the output stream, then one frame every framerateDenominator/framerateNumerator seconds. For example, settings of framerateNumerator = 1 and framerateDenominator = 3 (a rate of 1/3 frame per second) will capture the first frame, then 1 frame every 3s. Files will be named as filename.NNNNNNN.jpg where N is the 0-based frame sequence number zero padded to 7 decimal places.
fcsFramerateNumerator :: Lens' FrameCaptureSettings (Maybe Natural)
fcsFramerateNumerator = lens _fcsFramerateNumerator (\ s a -> s{_fcsFramerateNumerator = a}) . mapping _Nat

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

-- | Settings for quality-defined variable bitrate encoding with the H.264 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- /See:/ 'h264QvbrSettings' smart constructor.
data H264QvbrSettings = H264QvbrSettings'
  { _hMaxAverageBitrate :: !(Maybe Nat)
  , _hQvbrQualityLevel  :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'H264QvbrSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hMaxAverageBitrate' - Use this setting only when Rate control mode is QVBR and Quality tuning level is Multi-pass HQ. For Max average bitrate values suited to the complexity of your input video, the service limits the average bitrate of the video part of this output to the value you choose. That is, the total size of the video element is less than or equal to the value you set multiplied by the number of seconds of encoded output.
--
-- * 'hQvbrQualityLevel' - Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within h264Settings. Specify the target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9.
h264QvbrSettings
    :: H264QvbrSettings
h264QvbrSettings =
  H264QvbrSettings'
    {_hMaxAverageBitrate = Nothing, _hQvbrQualityLevel = Nothing}


-- | Use this setting only when Rate control mode is QVBR and Quality tuning level is Multi-pass HQ. For Max average bitrate values suited to the complexity of your input video, the service limits the average bitrate of the video part of this output to the value you choose. That is, the total size of the video element is less than or equal to the value you set multiplied by the number of seconds of encoded output.
hMaxAverageBitrate :: Lens' H264QvbrSettings (Maybe Natural)
hMaxAverageBitrate = lens _hMaxAverageBitrate (\ s a -> s{_hMaxAverageBitrate = a}) . mapping _Nat

-- | Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within h264Settings. Specify the target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9.
hQvbrQualityLevel :: Lens' H264QvbrSettings (Maybe Natural)
hQvbrQualityLevel = lens _hQvbrQualityLevel (\ s a -> s{_hQvbrQualityLevel = a}) . mapping _Nat

instance FromJSON H264QvbrSettings where
        parseJSON
          = withObject "H264QvbrSettings"
              (\ x ->
                 H264QvbrSettings' <$>
                   (x .:? "maxAverageBitrate") <*>
                     (x .:? "qvbrQualityLevel"))

instance Hashable H264QvbrSettings where

instance NFData H264QvbrSettings where

instance ToJSON H264QvbrSettings where
        toJSON H264QvbrSettings'{..}
          = object
              (catMaybes
                 [("maxAverageBitrate" .=) <$> _hMaxAverageBitrate,
                  ("qvbrQualityLevel" .=) <$> _hQvbrQualityLevel])

-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
--
-- /See:/ 'h264Settings' smart constructor.
data H264Settings = H264Settings'
  { _hssUnregisteredSeiTimecode :: !(Maybe H264UnregisteredSeiTimecode)
  , _hssQualityTuningLevel :: !(Maybe H264QualityTuningLevel)
  , _hssTemporalAdaptiveQuantization :: !(Maybe H264TemporalAdaptiveQuantization)
  , _hssSceneChangeDetect :: !(Maybe H264SceneChangeDetect)
  , _hssHrdBufferInitialFillPercentage :: !(Maybe Nat)
  , _hssSlowPal :: !(Maybe H264SlowPal)
  , _hssParNumerator :: !(Maybe Nat)
  , _hssGopSize :: !(Maybe Double)
  , _hssNumberBFramesBetweenReferenceFrames :: !(Maybe Nat)
  , _hssGopSizeUnits :: !(Maybe H264GopSizeUnits)
  , _hssHrdBufferSize :: !(Maybe Nat)
  , _hssSlices :: !(Maybe Nat)
  , _hssRateControlMode :: !(Maybe H264RateControlMode)
  , _hssNumberReferenceFrames :: !(Maybe Nat)
  , _hssTelecine :: !(Maybe H264Telecine)
  , _hssDynamicSubGop :: !(Maybe H264DynamicSubGop)
  , _hssMinIInterval :: !(Maybe Nat)
  , _hssInterlaceMode :: !(Maybe H264InterlaceMode)
  , _hssParControl :: !(Maybe H264ParControl)
  , _hssRepeatPps :: !(Maybe H264RepeatPps)
  , _hssFlickerAdaptiveQuantization :: !(Maybe H264FlickerAdaptiveQuantization)
  , _hssQvbrSettings :: !(Maybe H264QvbrSettings)
  , _hssSoftness :: !(Maybe Nat)
  , _hssCodecProfile :: !(Maybe H264CodecProfile)
  , _hssBitrate :: !(Maybe Nat)
  , _hssFramerateDenominator :: !(Maybe Nat)
  , _hssFramerateConversionAlgorithm :: !(Maybe H264FramerateConversionAlgorithm)
  , _hssCodecLevel :: !(Maybe H264CodecLevel)
  , _hssEntropyEncoding :: !(Maybe H264EntropyEncoding)
  , _hssFramerateControl :: !(Maybe H264FramerateControl)
  , _hssAdaptiveQuantization :: !(Maybe H264AdaptiveQuantization)
  , _hssFramerateNumerator :: !(Maybe Nat)
  , _hssGopBReference :: !(Maybe H264GopBReference)
  , _hssMaxBitrate :: !(Maybe Nat)
  , _hssSyntax :: !(Maybe H264Syntax)
  , _hssFieldEncoding :: !(Maybe H264FieldEncoding)
  , _hssGopClosedCadence :: !(Maybe Nat)
  , _hssParDenominator :: !(Maybe Nat)
  , _hssSpatialAdaptiveQuantization :: !(Maybe H264SpatialAdaptiveQuantization)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'H264Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hssUnregisteredSeiTimecode' - Undocumented member.
--
-- * 'hssQualityTuningLevel' - Undocumented member.
--
-- * 'hssTemporalAdaptiveQuantization' - Undocumented member.
--
-- * 'hssSceneChangeDetect' - Undocumented member.
--
-- * 'hssHrdBufferInitialFillPercentage' - Percentage of the buffer that should initially be filled (HRD buffer model).
--
-- * 'hssSlowPal' - Undocumented member.
--
-- * 'hssParNumerator' - Pixel Aspect Ratio numerator.
--
-- * 'hssGopSize' - GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
--
-- * 'hssNumberBFramesBetweenReferenceFrames' - Number of B-frames between reference frames.
--
-- * 'hssGopSizeUnits' - Undocumented member.
--
-- * 'hssHrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- * 'hssSlices' - Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
--
-- * 'hssRateControlMode' - Undocumented member.
--
-- * 'hssNumberReferenceFrames' - Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
--
-- * 'hssTelecine' - Undocumented member.
--
-- * 'hssDynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
--
-- * 'hssMinIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'hssInterlaceMode' - Undocumented member.
--
-- * 'hssParControl' - Undocumented member.
--
-- * 'hssRepeatPps' - Undocumented member.
--
-- * 'hssFlickerAdaptiveQuantization' - Undocumented member.
--
-- * 'hssQvbrSettings' - Settings for quality-defined variable bitrate encoding with the H.264 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- * 'hssSoftness' - Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
--
-- * 'hssCodecProfile' - Undocumented member.
--
-- * 'hssBitrate' - Average bitrate in bits/second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- * 'hssFramerateDenominator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
--
-- * 'hssFramerateConversionAlgorithm' - Undocumented member.
--
-- * 'hssCodecLevel' - Undocumented member.
--
-- * 'hssEntropyEncoding' - Undocumented member.
--
-- * 'hssFramerateControl' - Undocumented member.
--
-- * 'hssAdaptiveQuantization' - Undocumented member.
--
-- * 'hssFramerateNumerator' - Frame rate numerator - frame rate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- * 'hssGopBReference' - Undocumented member.
--
-- * 'hssMaxBitrate' - Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
--
-- * 'hssSyntax' - Undocumented member.
--
-- * 'hssFieldEncoding' - Undocumented member.
--
-- * 'hssGopClosedCadence' - Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
--
-- * 'hssParDenominator' - Pixel Aspect Ratio denominator.
--
-- * 'hssSpatialAdaptiveQuantization' - Undocumented member.
h264Settings
    :: H264Settings
h264Settings =
  H264Settings'
    { _hssUnregisteredSeiTimecode = Nothing
    , _hssQualityTuningLevel = Nothing
    , _hssTemporalAdaptiveQuantization = Nothing
    , _hssSceneChangeDetect = Nothing
    , _hssHrdBufferInitialFillPercentage = Nothing
    , _hssSlowPal = Nothing
    , _hssParNumerator = Nothing
    , _hssGopSize = Nothing
    , _hssNumberBFramesBetweenReferenceFrames = Nothing
    , _hssGopSizeUnits = Nothing
    , _hssHrdBufferSize = Nothing
    , _hssSlices = Nothing
    , _hssRateControlMode = Nothing
    , _hssNumberReferenceFrames = Nothing
    , _hssTelecine = Nothing
    , _hssDynamicSubGop = Nothing
    , _hssMinIInterval = Nothing
    , _hssInterlaceMode = Nothing
    , _hssParControl = Nothing
    , _hssRepeatPps = Nothing
    , _hssFlickerAdaptiveQuantization = Nothing
    , _hssQvbrSettings = Nothing
    , _hssSoftness = Nothing
    , _hssCodecProfile = Nothing
    , _hssBitrate = Nothing
    , _hssFramerateDenominator = Nothing
    , _hssFramerateConversionAlgorithm = Nothing
    , _hssCodecLevel = Nothing
    , _hssEntropyEncoding = Nothing
    , _hssFramerateControl = Nothing
    , _hssAdaptiveQuantization = Nothing
    , _hssFramerateNumerator = Nothing
    , _hssGopBReference = Nothing
    , _hssMaxBitrate = Nothing
    , _hssSyntax = Nothing
    , _hssFieldEncoding = Nothing
    , _hssGopClosedCadence = Nothing
    , _hssParDenominator = Nothing
    , _hssSpatialAdaptiveQuantization = Nothing
    }


-- | Undocumented member.
hssUnregisteredSeiTimecode :: Lens' H264Settings (Maybe H264UnregisteredSeiTimecode)
hssUnregisteredSeiTimecode = lens _hssUnregisteredSeiTimecode (\ s a -> s{_hssUnregisteredSeiTimecode = a})

-- | Undocumented member.
hssQualityTuningLevel :: Lens' H264Settings (Maybe H264QualityTuningLevel)
hssQualityTuningLevel = lens _hssQualityTuningLevel (\ s a -> s{_hssQualityTuningLevel = a})

-- | Undocumented member.
hssTemporalAdaptiveQuantization :: Lens' H264Settings (Maybe H264TemporalAdaptiveQuantization)
hssTemporalAdaptiveQuantization = lens _hssTemporalAdaptiveQuantization (\ s a -> s{_hssTemporalAdaptiveQuantization = a})

-- | Undocumented member.
hssSceneChangeDetect :: Lens' H264Settings (Maybe H264SceneChangeDetect)
hssSceneChangeDetect = lens _hssSceneChangeDetect (\ s a -> s{_hssSceneChangeDetect = a})

-- | Percentage of the buffer that should initially be filled (HRD buffer model).
hssHrdBufferInitialFillPercentage :: Lens' H264Settings (Maybe Natural)
hssHrdBufferInitialFillPercentage = lens _hssHrdBufferInitialFillPercentage (\ s a -> s{_hssHrdBufferInitialFillPercentage = a}) . mapping _Nat

-- | Undocumented member.
hssSlowPal :: Lens' H264Settings (Maybe H264SlowPal)
hssSlowPal = lens _hssSlowPal (\ s a -> s{_hssSlowPal = a})

-- | Pixel Aspect Ratio numerator.
hssParNumerator :: Lens' H264Settings (Maybe Natural)
hssParNumerator = lens _hssParNumerator (\ s a -> s{_hssParNumerator = a}) . mapping _Nat

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
hssGopSize :: Lens' H264Settings (Maybe Double)
hssGopSize = lens _hssGopSize (\ s a -> s{_hssGopSize = a})

-- | Number of B-frames between reference frames.
hssNumberBFramesBetweenReferenceFrames :: Lens' H264Settings (Maybe Natural)
hssNumberBFramesBetweenReferenceFrames = lens _hssNumberBFramesBetweenReferenceFrames (\ s a -> s{_hssNumberBFramesBetweenReferenceFrames = a}) . mapping _Nat

-- | Undocumented member.
hssGopSizeUnits :: Lens' H264Settings (Maybe H264GopSizeUnits)
hssGopSizeUnits = lens _hssGopSizeUnits (\ s a -> s{_hssGopSizeUnits = a})

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
hssHrdBufferSize :: Lens' H264Settings (Maybe Natural)
hssHrdBufferSize = lens _hssHrdBufferSize (\ s a -> s{_hssHrdBufferSize = a}) . mapping _Nat

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
hssSlices :: Lens' H264Settings (Maybe Natural)
hssSlices = lens _hssSlices (\ s a -> s{_hssSlices = a}) . mapping _Nat

-- | Undocumented member.
hssRateControlMode :: Lens' H264Settings (Maybe H264RateControlMode)
hssRateControlMode = lens _hssRateControlMode (\ s a -> s{_hssRateControlMode = a})

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
hssNumberReferenceFrames :: Lens' H264Settings (Maybe Natural)
hssNumberReferenceFrames = lens _hssNumberReferenceFrames (\ s a -> s{_hssNumberReferenceFrames = a}) . mapping _Nat

-- | Undocumented member.
hssTelecine :: Lens' H264Settings (Maybe H264Telecine)
hssTelecine = lens _hssTelecine (\ s a -> s{_hssTelecine = a})

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
hssDynamicSubGop :: Lens' H264Settings (Maybe H264DynamicSubGop)
hssDynamicSubGop = lens _hssDynamicSubGop (\ s a -> s{_hssDynamicSubGop = a})

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
hssMinIInterval :: Lens' H264Settings (Maybe Natural)
hssMinIInterval = lens _hssMinIInterval (\ s a -> s{_hssMinIInterval = a}) . mapping _Nat

-- | Undocumented member.
hssInterlaceMode :: Lens' H264Settings (Maybe H264InterlaceMode)
hssInterlaceMode = lens _hssInterlaceMode (\ s a -> s{_hssInterlaceMode = a})

-- | Undocumented member.
hssParControl :: Lens' H264Settings (Maybe H264ParControl)
hssParControl = lens _hssParControl (\ s a -> s{_hssParControl = a})

-- | Undocumented member.
hssRepeatPps :: Lens' H264Settings (Maybe H264RepeatPps)
hssRepeatPps = lens _hssRepeatPps (\ s a -> s{_hssRepeatPps = a})

-- | Undocumented member.
hssFlickerAdaptiveQuantization :: Lens' H264Settings (Maybe H264FlickerAdaptiveQuantization)
hssFlickerAdaptiveQuantization = lens _hssFlickerAdaptiveQuantization (\ s a -> s{_hssFlickerAdaptiveQuantization = a})

-- | Settings for quality-defined variable bitrate encoding with the H.264 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
hssQvbrSettings :: Lens' H264Settings (Maybe H264QvbrSettings)
hssQvbrSettings = lens _hssQvbrSettings (\ s a -> s{_hssQvbrSettings = a})

-- | Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
hssSoftness :: Lens' H264Settings (Maybe Natural)
hssSoftness = lens _hssSoftness (\ s a -> s{_hssSoftness = a}) . mapping _Nat

-- | Undocumented member.
hssCodecProfile :: Lens' H264Settings (Maybe H264CodecProfile)
hssCodecProfile = lens _hssCodecProfile (\ s a -> s{_hssCodecProfile = a})

-- | Average bitrate in bits/second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
hssBitrate :: Lens' H264Settings (Maybe Natural)
hssBitrate = lens _hssBitrate (\ s a -> s{_hssBitrate = a}) . mapping _Nat

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateDenominator to specify the denominator of this fraction. In this example, use 1001 for the value of FramerateDenominator. When you use the console for transcode jobs that use frame rate conversion, provide the value as a decimal number for Framerate. In this example, specify 23.976.
hssFramerateDenominator :: Lens' H264Settings (Maybe Natural)
hssFramerateDenominator = lens _hssFramerateDenominator (\ s a -> s{_hssFramerateDenominator = a}) . mapping _Nat

-- | Undocumented member.
hssFramerateConversionAlgorithm :: Lens' H264Settings (Maybe H264FramerateConversionAlgorithm)
hssFramerateConversionAlgorithm = lens _hssFramerateConversionAlgorithm (\ s a -> s{_hssFramerateConversionAlgorithm = a})

-- | Undocumented member.
hssCodecLevel :: Lens' H264Settings (Maybe H264CodecLevel)
hssCodecLevel = lens _hssCodecLevel (\ s a -> s{_hssCodecLevel = a})

-- | Undocumented member.
hssEntropyEncoding :: Lens' H264Settings (Maybe H264EntropyEncoding)
hssEntropyEncoding = lens _hssEntropyEncoding (\ s a -> s{_hssEntropyEncoding = a})

-- | Undocumented member.
hssFramerateControl :: Lens' H264Settings (Maybe H264FramerateControl)
hssFramerateControl = lens _hssFramerateControl (\ s a -> s{_hssFramerateControl = a})

-- | Undocumented member.
hssAdaptiveQuantization :: Lens' H264Settings (Maybe H264AdaptiveQuantization)
hssAdaptiveQuantization = lens _hssAdaptiveQuantization (\ s a -> s{_hssAdaptiveQuantization = a})

-- | Frame rate numerator - frame rate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
hssFramerateNumerator :: Lens' H264Settings (Maybe Natural)
hssFramerateNumerator = lens _hssFramerateNumerator (\ s a -> s{_hssFramerateNumerator = a}) . mapping _Nat

-- | Undocumented member.
hssGopBReference :: Lens' H264Settings (Maybe H264GopBReference)
hssGopBReference = lens _hssGopBReference (\ s a -> s{_hssGopBReference = a})

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
hssMaxBitrate :: Lens' H264Settings (Maybe Natural)
hssMaxBitrate = lens _hssMaxBitrate (\ s a -> s{_hssMaxBitrate = a}) . mapping _Nat

-- | Undocumented member.
hssSyntax :: Lens' H264Settings (Maybe H264Syntax)
hssSyntax = lens _hssSyntax (\ s a -> s{_hssSyntax = a})

-- | Undocumented member.
hssFieldEncoding :: Lens' H264Settings (Maybe H264FieldEncoding)
hssFieldEncoding = lens _hssFieldEncoding (\ s a -> s{_hssFieldEncoding = a})

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
hssGopClosedCadence :: Lens' H264Settings (Maybe Natural)
hssGopClosedCadence = lens _hssGopClosedCadence (\ s a -> s{_hssGopClosedCadence = a}) . mapping _Nat

-- | Pixel Aspect Ratio denominator.
hssParDenominator :: Lens' H264Settings (Maybe Natural)
hssParDenominator = lens _hssParDenominator (\ s a -> s{_hssParDenominator = a}) . mapping _Nat

-- | Undocumented member.
hssSpatialAdaptiveQuantization :: Lens' H264Settings (Maybe H264SpatialAdaptiveQuantization)
hssSpatialAdaptiveQuantization = lens _hssSpatialAdaptiveQuantization (\ s a -> s{_hssSpatialAdaptiveQuantization = a})

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
                     <*> (x .:? "spatialAdaptiveQuantization"))

instance Hashable H264Settings where

instance NFData H264Settings where

instance ToJSON H264Settings where
        toJSON H264Settings'{..}
          = object
              (catMaybes
                 [("unregisteredSeiTimecode" .=) <$>
                    _hssUnregisteredSeiTimecode,
                  ("qualityTuningLevel" .=) <$> _hssQualityTuningLevel,
                  ("temporalAdaptiveQuantization" .=) <$>
                    _hssTemporalAdaptiveQuantization,
                  ("sceneChangeDetect" .=) <$> _hssSceneChangeDetect,
                  ("hrdBufferInitialFillPercentage" .=) <$>
                    _hssHrdBufferInitialFillPercentage,
                  ("slowPal" .=) <$> _hssSlowPal,
                  ("parNumerator" .=) <$> _hssParNumerator,
                  ("gopSize" .=) <$> _hssGopSize,
                  ("numberBFramesBetweenReferenceFrames" .=) <$>
                    _hssNumberBFramesBetweenReferenceFrames,
                  ("gopSizeUnits" .=) <$> _hssGopSizeUnits,
                  ("hrdBufferSize" .=) <$> _hssHrdBufferSize,
                  ("slices" .=) <$> _hssSlices,
                  ("rateControlMode" .=) <$> _hssRateControlMode,
                  ("numberReferenceFrames" .=) <$>
                    _hssNumberReferenceFrames,
                  ("telecine" .=) <$> _hssTelecine,
                  ("dynamicSubGop" .=) <$> _hssDynamicSubGop,
                  ("minIInterval" .=) <$> _hssMinIInterval,
                  ("interlaceMode" .=) <$> _hssInterlaceMode,
                  ("parControl" .=) <$> _hssParControl,
                  ("repeatPps" .=) <$> _hssRepeatPps,
                  ("flickerAdaptiveQuantization" .=) <$>
                    _hssFlickerAdaptiveQuantization,
                  ("qvbrSettings" .=) <$> _hssQvbrSettings,
                  ("softness" .=) <$> _hssSoftness,
                  ("codecProfile" .=) <$> _hssCodecProfile,
                  ("bitrate" .=) <$> _hssBitrate,
                  ("framerateDenominator" .=) <$>
                    _hssFramerateDenominator,
                  ("framerateConversionAlgorithm" .=) <$>
                    _hssFramerateConversionAlgorithm,
                  ("codecLevel" .=) <$> _hssCodecLevel,
                  ("entropyEncoding" .=) <$> _hssEntropyEncoding,
                  ("framerateControl" .=) <$> _hssFramerateControl,
                  ("adaptiveQuantization" .=) <$>
                    _hssAdaptiveQuantization,
                  ("framerateNumerator" .=) <$> _hssFramerateNumerator,
                  ("gopBReference" .=) <$> _hssGopBReference,
                  ("maxBitrate" .=) <$> _hssMaxBitrate,
                  ("syntax" .=) <$> _hssSyntax,
                  ("fieldEncoding" .=) <$> _hssFieldEncoding,
                  ("gopClosedCadence" .=) <$> _hssGopClosedCadence,
                  ("parDenominator" .=) <$> _hssParDenominator,
                  ("spatialAdaptiveQuantization" .=) <$>
                    _hssSpatialAdaptiveQuantization])

-- | Settings for quality-defined variable bitrate encoding with the H.265 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- /See:/ 'h265QvbrSettings' smart constructor.
data H265QvbrSettings = H265QvbrSettings'
  { _hqsMaxAverageBitrate :: !(Maybe Nat)
  , _hqsQvbrQualityLevel  :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'H265QvbrSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hqsMaxAverageBitrate' - Use this setting only when Rate control mode is QVBR and Quality tuning level is Multi-pass HQ. For Max average bitrate values suited to the complexity of your input video, the service limits the average bitrate of the video part of this output to the value you choose. That is, the total size of the video element is less than or equal to the value you set multiplied by the number of seconds of encoded output.
--
-- * 'hqsQvbrQualityLevel' - Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within h265Settings. Specify the target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9.
h265QvbrSettings
    :: H265QvbrSettings
h265QvbrSettings =
  H265QvbrSettings'
    {_hqsMaxAverageBitrate = Nothing, _hqsQvbrQualityLevel = Nothing}


-- | Use this setting only when Rate control mode is QVBR and Quality tuning level is Multi-pass HQ. For Max average bitrate values suited to the complexity of your input video, the service limits the average bitrate of the video part of this output to the value you choose. That is, the total size of the video element is less than or equal to the value you set multiplied by the number of seconds of encoded output.
hqsMaxAverageBitrate :: Lens' H265QvbrSettings (Maybe Natural)
hqsMaxAverageBitrate = lens _hqsMaxAverageBitrate (\ s a -> s{_hqsMaxAverageBitrate = a}) . mapping _Nat

-- | Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within h265Settings. Specify the target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9.
hqsQvbrQualityLevel :: Lens' H265QvbrSettings (Maybe Natural)
hqsQvbrQualityLevel = lens _hqsQvbrQualityLevel (\ s a -> s{_hqsQvbrQualityLevel = a}) . mapping _Nat

instance FromJSON H265QvbrSettings where
        parseJSON
          = withObject "H265QvbrSettings"
              (\ x ->
                 H265QvbrSettings' <$>
                   (x .:? "maxAverageBitrate") <*>
                     (x .:? "qvbrQualityLevel"))

instance Hashable H265QvbrSettings where

instance NFData H265QvbrSettings where

instance ToJSON H265QvbrSettings where
        toJSON H265QvbrSettings'{..}
          = object
              (catMaybes
                 [("maxAverageBitrate" .=) <$> _hqsMaxAverageBitrate,
                  ("qvbrQualityLevel" .=) <$> _hqsQvbrQualityLevel])

-- | Settings for H265 codec
--
-- /See:/ 'h265Settings' smart constructor.
data H265Settings = H265Settings'
  { _hsUnregisteredSeiTimecode :: !(Maybe H265UnregisteredSeiTimecode)
  , _hsQualityTuningLevel :: !(Maybe H265QualityTuningLevel)
  , _hsTemporalAdaptiveQuantization :: !(Maybe H265TemporalAdaptiveQuantization)
  , _hsSceneChangeDetect :: !(Maybe H265SceneChangeDetect)
  , _hsHrdBufferInitialFillPercentage :: !(Maybe Nat)
  , _hsTiles :: !(Maybe H265Tiles)
  , _hsSlowPal :: !(Maybe H265SlowPal)
  , _hsTemporalIds :: !(Maybe H265TemporalIds)
  , _hsParNumerator :: !(Maybe Nat)
  , _hsGopSize :: !(Maybe Double)
  , _hsNumberBFramesBetweenReferenceFrames :: !(Maybe Nat)
  , _hsGopSizeUnits :: !(Maybe H265GopSizeUnits)
  , _hsHrdBufferSize :: !(Maybe Nat)
  , _hsSlices :: !(Maybe Nat)
  , _hsAlternateTransferFunctionSei :: !(Maybe H265AlternateTransferFunctionSei)
  , _hsRateControlMode :: !(Maybe H265RateControlMode)
  , _hsNumberReferenceFrames :: !(Maybe Nat)
  , _hsTelecine :: !(Maybe H265Telecine)
  , _hsDynamicSubGop :: !(Maybe H265DynamicSubGop)
  , _hsMinIInterval :: !(Maybe Nat)
  , _hsInterlaceMode :: !(Maybe H265InterlaceMode)
  , _hsParControl :: !(Maybe H265ParControl)
  , _hsFlickerAdaptiveQuantization :: !(Maybe H265FlickerAdaptiveQuantization)
  , _hsQvbrSettings :: !(Maybe H265QvbrSettings)
  , _hsSampleAdaptiveOffsetFilterMode :: !(Maybe H265SampleAdaptiveOffsetFilterMode)
  , _hsCodecProfile :: !(Maybe H265CodecProfile)
  , _hsBitrate :: !(Maybe Nat)
  , _hsFramerateDenominator :: !(Maybe Nat)
  , _hsFramerateConversionAlgorithm :: !(Maybe H265FramerateConversionAlgorithm)
  , _hsCodecLevel :: !(Maybe H265CodecLevel)
  , _hsFramerateControl :: !(Maybe H265FramerateControl)
  , _hsWriteMp4PackagingType :: !(Maybe H265WriteMp4PackagingType)
  , _hsAdaptiveQuantization :: !(Maybe H265AdaptiveQuantization)
  , _hsFramerateNumerator :: !(Maybe Nat)
  , _hsGopBReference :: !(Maybe H265GopBReference)
  , _hsMaxBitrate :: !(Maybe Nat)
  , _hsGopClosedCadence :: !(Maybe Nat)
  , _hsParDenominator :: !(Maybe Nat)
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
-- * 'hsHrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
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
-- * 'hsDynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
--
-- * 'hsMinIInterval' - Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
--
-- * 'hsInterlaceMode' - Undocumented member.
--
-- * 'hsParControl' - Undocumented member.
--
-- * 'hsFlickerAdaptiveQuantization' - Undocumented member.
--
-- * 'hsQvbrSettings' - Settings for quality-defined variable bitrate encoding with the H.265 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- * 'hsSampleAdaptiveOffsetFilterMode' - Undocumented member.
--
-- * 'hsCodecProfile' - Undocumented member.
--
-- * 'hsBitrate' - Average bitrate in bits/second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- * 'hsFramerateDenominator' - Frame rate denominator.
--
-- * 'hsFramerateConversionAlgorithm' - Undocumented member.
--
-- * 'hsCodecLevel' - Undocumented member.
--
-- * 'hsFramerateControl' - Undocumented member.
--
-- * 'hsWriteMp4PackagingType' - Use this setting only for outputs encoded with H.265 that are in CMAF or DASH output groups. If you include writeMp4PackagingType in your JSON job specification for other outputs, your video might not work properly with downstream systems and video players. If the location of parameter set NAL units don't matter in your workflow, ignore this setting. The service defaults to marking your output as HEV1. Choose HVC1 to mark your output as HVC1. This makes your output compliant with this specification: ISO IECJTC1 SC29 N13798 Text ISO/IEC FDIS 14496-15 3rd Edition. For these outputs, the service stores parameter set NAL units in the sample headers but not in the samples directly. Keep the default HEV1 to mark your output as HEV1. For these outputs, the service writes parameter set NAL units directly into the samples.
--
-- * 'hsAdaptiveQuantization' - Undocumented member.
--
-- * 'hsFramerateNumerator' - Frame rate numerator - frame rate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- * 'hsGopBReference' - Undocumented member.
--
-- * 'hsMaxBitrate' - Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
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
    , _hsDynamicSubGop = Nothing
    , _hsMinIInterval = Nothing
    , _hsInterlaceMode = Nothing
    , _hsParControl = Nothing
    , _hsFlickerAdaptiveQuantization = Nothing
    , _hsQvbrSettings = Nothing
    , _hsSampleAdaptiveOffsetFilterMode = Nothing
    , _hsCodecProfile = Nothing
    , _hsBitrate = Nothing
    , _hsFramerateDenominator = Nothing
    , _hsFramerateConversionAlgorithm = Nothing
    , _hsCodecLevel = Nothing
    , _hsFramerateControl = Nothing
    , _hsWriteMp4PackagingType = Nothing
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
hsHrdBufferInitialFillPercentage :: Lens' H265Settings (Maybe Natural)
hsHrdBufferInitialFillPercentage = lens _hsHrdBufferInitialFillPercentage (\ s a -> s{_hsHrdBufferInitialFillPercentage = a}) . mapping _Nat

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
hsParNumerator :: Lens' H265Settings (Maybe Natural)
hsParNumerator = lens _hsParNumerator (\ s a -> s{_hsParNumerator = a}) . mapping _Nat

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
hsGopSize :: Lens' H265Settings (Maybe Double)
hsGopSize = lens _hsGopSize (\ s a -> s{_hsGopSize = a})

-- | Number of B-frames between reference frames.
hsNumberBFramesBetweenReferenceFrames :: Lens' H265Settings (Maybe Natural)
hsNumberBFramesBetweenReferenceFrames = lens _hsNumberBFramesBetweenReferenceFrames (\ s a -> s{_hsNumberBFramesBetweenReferenceFrames = a}) . mapping _Nat

-- | Undocumented member.
hsGopSizeUnits :: Lens' H265Settings (Maybe H265GopSizeUnits)
hsGopSizeUnits = lens _hsGopSizeUnits (\ s a -> s{_hsGopSizeUnits = a})

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
hsHrdBufferSize :: Lens' H265Settings (Maybe Natural)
hsHrdBufferSize = lens _hsHrdBufferSize (\ s a -> s{_hsHrdBufferSize = a}) . mapping _Nat

-- | Number of slices per picture. Must be less than or equal to the number of macroblock rows for progressive pictures, and less than or equal to half the number of macroblock rows for interlaced pictures.
hsSlices :: Lens' H265Settings (Maybe Natural)
hsSlices = lens _hsSlices (\ s a -> s{_hsSlices = a}) . mapping _Nat

-- | Undocumented member.
hsAlternateTransferFunctionSei :: Lens' H265Settings (Maybe H265AlternateTransferFunctionSei)
hsAlternateTransferFunctionSei = lens _hsAlternateTransferFunctionSei (\ s a -> s{_hsAlternateTransferFunctionSei = a})

-- | Undocumented member.
hsRateControlMode :: Lens' H265Settings (Maybe H265RateControlMode)
hsRateControlMode = lens _hsRateControlMode (\ s a -> s{_hsRateControlMode = a})

-- | Number of reference frames to use. The encoder may use more than requested if using B-frames and/or interlaced encoding.
hsNumberReferenceFrames :: Lens' H265Settings (Maybe Natural)
hsNumberReferenceFrames = lens _hsNumberReferenceFrames (\ s a -> s{_hsNumberReferenceFrames = a}) . mapping _Nat

-- | Undocumented member.
hsTelecine :: Lens' H265Settings (Maybe H265Telecine)
hsTelecine = lens _hsTelecine (\ s a -> s{_hsTelecine = a})

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
hsDynamicSubGop :: Lens' H265Settings (Maybe H265DynamicSubGop)
hsDynamicSubGop = lens _hsDynamicSubGop (\ s a -> s{_hsDynamicSubGop = a})

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
hsMinIInterval :: Lens' H265Settings (Maybe Natural)
hsMinIInterval = lens _hsMinIInterval (\ s a -> s{_hsMinIInterval = a}) . mapping _Nat

-- | Undocumented member.
hsInterlaceMode :: Lens' H265Settings (Maybe H265InterlaceMode)
hsInterlaceMode = lens _hsInterlaceMode (\ s a -> s{_hsInterlaceMode = a})

-- | Undocumented member.
hsParControl :: Lens' H265Settings (Maybe H265ParControl)
hsParControl = lens _hsParControl (\ s a -> s{_hsParControl = a})

-- | Undocumented member.
hsFlickerAdaptiveQuantization :: Lens' H265Settings (Maybe H265FlickerAdaptiveQuantization)
hsFlickerAdaptiveQuantization = lens _hsFlickerAdaptiveQuantization (\ s a -> s{_hsFlickerAdaptiveQuantization = a})

-- | Settings for quality-defined variable bitrate encoding with the H.265 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
hsQvbrSettings :: Lens' H265Settings (Maybe H265QvbrSettings)
hsQvbrSettings = lens _hsQvbrSettings (\ s a -> s{_hsQvbrSettings = a})

-- | Undocumented member.
hsSampleAdaptiveOffsetFilterMode :: Lens' H265Settings (Maybe H265SampleAdaptiveOffsetFilterMode)
hsSampleAdaptiveOffsetFilterMode = lens _hsSampleAdaptiveOffsetFilterMode (\ s a -> s{_hsSampleAdaptiveOffsetFilterMode = a})

-- | Undocumented member.
hsCodecProfile :: Lens' H265Settings (Maybe H265CodecProfile)
hsCodecProfile = lens _hsCodecProfile (\ s a -> s{_hsCodecProfile = a})

-- | Average bitrate in bits/second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
hsBitrate :: Lens' H265Settings (Maybe Natural)
hsBitrate = lens _hsBitrate (\ s a -> s{_hsBitrate = a}) . mapping _Nat

-- | Frame rate denominator.
hsFramerateDenominator :: Lens' H265Settings (Maybe Natural)
hsFramerateDenominator = lens _hsFramerateDenominator (\ s a -> s{_hsFramerateDenominator = a}) . mapping _Nat

-- | Undocumented member.
hsFramerateConversionAlgorithm :: Lens' H265Settings (Maybe H265FramerateConversionAlgorithm)
hsFramerateConversionAlgorithm = lens _hsFramerateConversionAlgorithm (\ s a -> s{_hsFramerateConversionAlgorithm = a})

-- | Undocumented member.
hsCodecLevel :: Lens' H265Settings (Maybe H265CodecLevel)
hsCodecLevel = lens _hsCodecLevel (\ s a -> s{_hsCodecLevel = a})

-- | Undocumented member.
hsFramerateControl :: Lens' H265Settings (Maybe H265FramerateControl)
hsFramerateControl = lens _hsFramerateControl (\ s a -> s{_hsFramerateControl = a})

-- | Use this setting only for outputs encoded with H.265 that are in CMAF or DASH output groups. If you include writeMp4PackagingType in your JSON job specification for other outputs, your video might not work properly with downstream systems and video players. If the location of parameter set NAL units don't matter in your workflow, ignore this setting. The service defaults to marking your output as HEV1. Choose HVC1 to mark your output as HVC1. This makes your output compliant with this specification: ISO IECJTC1 SC29 N13798 Text ISO/IEC FDIS 14496-15 3rd Edition. For these outputs, the service stores parameter set NAL units in the sample headers but not in the samples directly. Keep the default HEV1 to mark your output as HEV1. For these outputs, the service writes parameter set NAL units directly into the samples.
hsWriteMp4PackagingType :: Lens' H265Settings (Maybe H265WriteMp4PackagingType)
hsWriteMp4PackagingType = lens _hsWriteMp4PackagingType (\ s a -> s{_hsWriteMp4PackagingType = a})

-- | Undocumented member.
hsAdaptiveQuantization :: Lens' H265Settings (Maybe H265AdaptiveQuantization)
hsAdaptiveQuantization = lens _hsAdaptiveQuantization (\ s a -> s{_hsAdaptiveQuantization = a})

-- | Frame rate numerator - frame rate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
hsFramerateNumerator :: Lens' H265Settings (Maybe Natural)
hsFramerateNumerator = lens _hsFramerateNumerator (\ s a -> s{_hsFramerateNumerator = a}) . mapping _Nat

-- | Undocumented member.
hsGopBReference :: Lens' H265Settings (Maybe H265GopBReference)
hsGopBReference = lens _hsGopBReference (\ s a -> s{_hsGopBReference = a})

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000. Required when Rate control mode is QVBR.
hsMaxBitrate :: Lens' H265Settings (Maybe Natural)
hsMaxBitrate = lens _hsMaxBitrate (\ s a -> s{_hsMaxBitrate = a}) . mapping _Nat

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
hsGopClosedCadence :: Lens' H265Settings (Maybe Natural)
hsGopClosedCadence = lens _hsGopClosedCadence (\ s a -> s{_hsGopClosedCadence = a}) . mapping _Nat

-- | Pixel Aspect Ratio denominator.
hsParDenominator :: Lens' H265Settings (Maybe Natural)
hsParDenominator = lens _hsParDenominator (\ s a -> s{_hsParDenominator = a}) . mapping _Nat

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
                  ("dynamicSubGop" .=) <$> _hsDynamicSubGop,
                  ("minIInterval" .=) <$> _hsMinIInterval,
                  ("interlaceMode" .=) <$> _hsInterlaceMode,
                  ("parControl" .=) <$> _hsParControl,
                  ("flickerAdaptiveQuantization" .=) <$>
                    _hsFlickerAdaptiveQuantization,
                  ("qvbrSettings" .=) <$> _hsQvbrSettings,
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
                  ("writeMp4PackagingType" .=) <$>
                    _hsWriteMp4PackagingType,
                  ("adaptiveQuantization" .=) <$>
                    _hsAdaptiveQuantization,
                  ("framerateNumerator" .=) <$> _hsFramerateNumerator,
                  ("gopBReference" .=) <$> _hsGopBReference,
                  ("maxBitrate" .=) <$> _hsMaxBitrate,
                  ("gopClosedCadence" .=) <$> _hsGopClosedCadence,
                  ("parDenominator" .=) <$> _hsParDenominator,
                  ("spatialAdaptiveQuantization" .=) <$>
                    _hsSpatialAdaptiveQuantization])

-- | Use the "HDR master display information" (Hdr10Metadata) settings to correct HDR metadata or to provide missing metadata. These values vary depending on the input video and must be provided by a color grader. Range is 0 to 50,000; each increment represents 0.00002 in CIE1931 color coordinate. Note that these settings are not color correction. Note that if you are creating HDR outputs inside of an HLS CMAF package, to comply with the Apple specification, you must use the following settings. Set "MP4 packaging type" (writeMp4PackagingType) to HVC1 (HVC1). Set "Profile" (H265Settings > codecProfile) to Main10/High (MAIN10_HIGH). Set "Level" (H265Settings > codecLevel) to 5 (LEVEL_5).
--
-- /See:/ 'hdr10Metadata' smart constructor.
data Hdr10Metadata = Hdr10Metadata'
  { _hmRedPrimaryX               :: !(Maybe Nat)
  , _hmBluePrimaryX              :: !(Maybe Nat)
  , _hmMaxFrameAverageLightLevel :: !(Maybe Nat)
  , _hmWhitePointY               :: !(Maybe Nat)
  , _hmMaxContentLightLevel      :: !(Maybe Nat)
  , _hmWhitePointX               :: !(Maybe Nat)
  , _hmBluePrimaryY              :: !(Maybe Nat)
  , _hmGreenPrimaryY             :: !(Maybe Nat)
  , _hmGreenPrimaryX             :: !(Maybe Nat)
  , _hmMinLuminance              :: !(Maybe Nat)
  , _hmRedPrimaryY               :: !(Maybe Nat)
  , _hmMaxLuminance              :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Hdr10Metadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hmRedPrimaryX' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- * 'hmBluePrimaryX' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- * 'hmMaxFrameAverageLightLevel' - Maximum average light level of any frame in the coded video sequence, in units of candelas per square meter.
--
-- * 'hmWhitePointY' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- * 'hmMaxContentLightLevel' - Maximum light level among all samples in the coded video sequence, in units of candelas per square meter.
--
-- * 'hmWhitePointX' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- * 'hmBluePrimaryY' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- * 'hmGreenPrimaryY' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- * 'hmGreenPrimaryX' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
--
-- * 'hmMinLuminance' - Nominal minimum mastering display luminance in units of of 0.0001 candelas per square meter
--
-- * 'hmRedPrimaryY' - HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
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


-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
hmRedPrimaryX :: Lens' Hdr10Metadata (Maybe Natural)
hmRedPrimaryX = lens _hmRedPrimaryX (\ s a -> s{_hmRedPrimaryX = a}) . mapping _Nat

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
hmBluePrimaryX :: Lens' Hdr10Metadata (Maybe Natural)
hmBluePrimaryX = lens _hmBluePrimaryX (\ s a -> s{_hmBluePrimaryX = a}) . mapping _Nat

-- | Maximum average light level of any frame in the coded video sequence, in units of candelas per square meter.
hmMaxFrameAverageLightLevel :: Lens' Hdr10Metadata (Maybe Natural)
hmMaxFrameAverageLightLevel = lens _hmMaxFrameAverageLightLevel (\ s a -> s{_hmMaxFrameAverageLightLevel = a}) . mapping _Nat

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
hmWhitePointY :: Lens' Hdr10Metadata (Maybe Natural)
hmWhitePointY = lens _hmWhitePointY (\ s a -> s{_hmWhitePointY = a}) . mapping _Nat

-- | Maximum light level among all samples in the coded video sequence, in units of candelas per square meter.
hmMaxContentLightLevel :: Lens' Hdr10Metadata (Maybe Natural)
hmMaxContentLightLevel = lens _hmMaxContentLightLevel (\ s a -> s{_hmMaxContentLightLevel = a}) . mapping _Nat

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
hmWhitePointX :: Lens' Hdr10Metadata (Maybe Natural)
hmWhitePointX = lens _hmWhitePointX (\ s a -> s{_hmWhitePointX = a}) . mapping _Nat

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
hmBluePrimaryY :: Lens' Hdr10Metadata (Maybe Natural)
hmBluePrimaryY = lens _hmBluePrimaryY (\ s a -> s{_hmBluePrimaryY = a}) . mapping _Nat

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
hmGreenPrimaryY :: Lens' Hdr10Metadata (Maybe Natural)
hmGreenPrimaryY = lens _hmGreenPrimaryY (\ s a -> s{_hmGreenPrimaryY = a}) . mapping _Nat

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
hmGreenPrimaryX :: Lens' Hdr10Metadata (Maybe Natural)
hmGreenPrimaryX = lens _hmGreenPrimaryX (\ s a -> s{_hmGreenPrimaryX = a}) . mapping _Nat

-- | Nominal minimum mastering display luminance in units of of 0.0001 candelas per square meter
hmMinLuminance :: Lens' Hdr10Metadata (Maybe Natural)
hmMinLuminance = lens _hmMinLuminance (\ s a -> s{_hmMinLuminance = a}) . mapping _Nat

-- | HDR Master Display Information must be provided by a color grader, using color grading tools. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate. Note that this setting is not for color correction.
hmRedPrimaryY :: Lens' Hdr10Metadata (Maybe Natural)
hmRedPrimaryY = lens _hmRedPrimaryY (\ s a -> s{_hmRedPrimaryY = a}) . mapping _Nat

-- | Nominal maximum mastering display luminance in units of of 0.0001 candelas per square meter.
hmMaxLuminance :: Lens' Hdr10Metadata (Maybe Natural)
hmMaxLuminance = lens _hmMaxLuminance (\ s a -> s{_hmMaxLuminance = a}) . mapping _Nat

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
  { _hclmCustomLanguageCode  :: !(Maybe Text)
  , _hclmLanguageCode        :: !(Maybe LanguageCode)
  , _hclmLanguageDescription :: !(Maybe Text)
  , _hclmCaptionChannel      :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HlsCaptionLanguageMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hclmCustomLanguageCode' - Specify the language for this caption channel, using the ISO 639-2 or ISO 639-3 three-letter language code
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
    { _hclmCustomLanguageCode = Nothing
    , _hclmLanguageCode = Nothing
    , _hclmLanguageDescription = Nothing
    , _hclmCaptionChannel = Nothing
    }


-- | Specify the language for this caption channel, using the ISO 639-2 or ISO 639-3 three-letter language code
hclmCustomLanguageCode :: Lens' HlsCaptionLanguageMapping (Maybe Text)
hclmCustomLanguageCode = lens _hclmCustomLanguageCode (\ s a -> s{_hclmCustomLanguageCode = a})

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
                   (x .:? "customLanguageCode") <*>
                     (x .:? "languageCode")
                     <*> (x .:? "languageDescription")
                     <*> (x .:? "captionChannel"))

instance Hashable HlsCaptionLanguageMapping where

instance NFData HlsCaptionLanguageMapping where

instance ToJSON HlsCaptionLanguageMapping where
        toJSON HlsCaptionLanguageMapping'{..}
          = object
              (catMaybes
                 [("customLanguageCode" .=) <$>
                    _hclmCustomLanguageCode,
                  ("languageCode" .=) <$> _hclmLanguageCode,
                  ("languageDescription" .=) <$>
                    _hclmLanguageDescription,
                  ("captionChannel" .=) <$> _hclmCaptionChannel])

-- | Settings for HLS encryption
--
-- /See:/ 'hlsEncryptionSettings' smart constructor.
data HlsEncryptionSettings = HlsEncryptionSettings'
  { _hesOfflineEncrypted :: !(Maybe HlsOfflineEncrypted)
  , _hesEncryptionMethod :: !(Maybe HlsEncryptionType)
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
-- * 'hesOfflineEncrypted' - Undocumented member.
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
    { _hesOfflineEncrypted = Nothing
    , _hesEncryptionMethod = Nothing
    , _hesConstantInitializationVector = Nothing
    , _hesType = Nothing
    , _hesStaticKeyProvider = Nothing
    , _hesSpekeKeyProvider = Nothing
    , _hesInitializationVectorInManifest = Nothing
    }


-- | Undocumented member.
hesOfflineEncrypted :: Lens' HlsEncryptionSettings (Maybe HlsOfflineEncrypted)
hesOfflineEncrypted = lens _hesOfflineEncrypted (\ s a -> s{_hesOfflineEncrypted = a})

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
                   (x .:? "offlineEncrypted") <*>
                     (x .:? "encryptionMethod")
                     <*> (x .:? "constantInitializationVector")
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
                 [("offlineEncrypted" .=) <$> _hesOfflineEncrypted,
                  ("encryptionMethod" .=) <$> _hesEncryptionMethod,
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
  , _hgsMinSegmentLength           :: !(Maybe Nat)
  , _hgsProgramDateTime            :: !(Maybe HlsProgramDateTime)
  , _hgsProgramDateTimePeriod      :: !(Maybe Nat)
  , _hgsCodecSpecification         :: !(Maybe HlsCodecSpecification)
  , _hgsCaptionLanguageMappings    :: !(Maybe [HlsCaptionLanguageMapping])
  , _hgsBaseURL                    :: !(Maybe Text)
  , _hgsMinFinalSegmentLength      :: !(Maybe Double)
  , _hgsAdMarkers                  :: !(Maybe [HlsAdMarkers])
  , _hgsEncryption                 :: !(Maybe HlsEncryptionSettings)
  , _hgsSegmentLength              :: !(Maybe Nat)
  , _hgsTimedMetadataId3Frame      :: !(Maybe HlsTimedMetadataId3Frame)
  , _hgsOutputSelection            :: !(Maybe HlsOutputSelection)
  , _hgsCaptionLanguageSetting     :: !(Maybe HlsCaptionLanguageSetting)
  , _hgsSegmentsPerSubdirectory    :: !(Maybe Nat)
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
-- * 'hgsMinFinalSegmentLength' - Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
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
    , _hgsMinFinalSegmentLength = Nothing
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
hgsMinSegmentLength :: Lens' HlsGroupSettings (Maybe Natural)
hgsMinSegmentLength = lens _hgsMinSegmentLength (\ s a -> s{_hgsMinSegmentLength = a}) . mapping _Nat

-- | Undocumented member.
hgsProgramDateTime :: Lens' HlsGroupSettings (Maybe HlsProgramDateTime)
hgsProgramDateTime = lens _hgsProgramDateTime (\ s a -> s{_hgsProgramDateTime = a})

-- | Period of insertion of EXT-X-PROGRAM-DATE-TIME entry, in seconds.
hgsProgramDateTimePeriod :: Lens' HlsGroupSettings (Maybe Natural)
hgsProgramDateTimePeriod = lens _hgsProgramDateTimePeriod (\ s a -> s{_hgsProgramDateTimePeriod = a}) . mapping _Nat

-- | Undocumented member.
hgsCodecSpecification :: Lens' HlsGroupSettings (Maybe HlsCodecSpecification)
hgsCodecSpecification = lens _hgsCodecSpecification (\ s a -> s{_hgsCodecSpecification = a})

-- | Language to be used on Caption outputs
hgsCaptionLanguageMappings :: Lens' HlsGroupSettings [HlsCaptionLanguageMapping]
hgsCaptionLanguageMappings = lens _hgsCaptionLanguageMappings (\ s a -> s{_hgsCaptionLanguageMappings = a}) . _Default . _Coerce

-- | A partial URI prefix that will be prepended to each output in the media .m3u8 file. Can be used if base manifest is delivered from a different URL than the main .m3u8 file.
hgsBaseURL :: Lens' HlsGroupSettings (Maybe Text)
hgsBaseURL = lens _hgsBaseURL (\ s a -> s{_hgsBaseURL = a})

-- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
hgsMinFinalSegmentLength :: Lens' HlsGroupSettings (Maybe Double)
hgsMinFinalSegmentLength = lens _hgsMinFinalSegmentLength (\ s a -> s{_hgsMinFinalSegmentLength = a})

-- | Choose one or more ad marker types to pass SCTE35 signals through to this group of Apple HLS outputs.
hgsAdMarkers :: Lens' HlsGroupSettings [HlsAdMarkers]
hgsAdMarkers = lens _hgsAdMarkers (\ s a -> s{_hgsAdMarkers = a}) . _Default . _Coerce

-- | DRM settings.
hgsEncryption :: Lens' HlsGroupSettings (Maybe HlsEncryptionSettings)
hgsEncryption = lens _hgsEncryption (\ s a -> s{_hgsEncryption = a})

-- | Length of MPEG-2 Transport Stream segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer.
hgsSegmentLength :: Lens' HlsGroupSettings (Maybe Natural)
hgsSegmentLength = lens _hgsSegmentLength (\ s a -> s{_hgsSegmentLength = a}) . mapping _Nat

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
hgsSegmentsPerSubdirectory :: Lens' HlsGroupSettings (Maybe Natural)
hgsSegmentsPerSubdirectory = lens _hgsSegmentsPerSubdirectory (\ s a -> s{_hgsSegmentsPerSubdirectory = a}) . mapping _Nat

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
                     <*> (x .:? "minFinalSegmentLength")
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
                  ("minFinalSegmentLength" .=) <$>
                    _hgsMinFinalSegmentLength,
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

-- | To insert ID3 tags in your output, specify two values. Use ID3 tag (Id3) to specify the base 64 encoded string and use Timecode (TimeCode) to specify the time when the tag should be inserted. To insert multiple ID3 tags in your output, create multiple instances of ID3 insertion (Id3Insertion).
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

-- | Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input or output individually. This setting is disabled by default.
--
-- /See:/ 'imageInserter' smart constructor.
newtype ImageInserter = ImageInserter'
  { _iiInsertableImages :: Maybe [InsertableImage]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImageInserter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiInsertableImages' - Specify the images that you want to overlay on your video. The images must be PNG or TGA files.
imageInserter
    :: ImageInserter
imageInserter = ImageInserter' {_iiInsertableImages = Nothing}


-- | Specify the images that you want to overlay on your video. The images must be PNG or TGA files.
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
  , _iSupplementalImps    :: !(Maybe [Text])
  , _iProgramNumber       :: !(Maybe Nat)
  , _iAudioSelectorGroups :: !(Maybe (Map Text AudioSelectorGroup))
  , _iTimecodeSource      :: !(Maybe InputTimecodeSource)
  , _iAudioSelectors      :: !(Maybe (Map Text AudioSelector))
  , _iDecryptionSettings  :: !(Maybe InputDecryptionSettings)
  , _iDeblockFilter       :: !(Maybe InputDeblockFilter)
  , _iInputClippings      :: !(Maybe [InputClipping])
  , _iDenoiseFilter       :: !(Maybe InputDenoiseFilter)
  , _iImageInserter       :: !(Maybe ImageInserter)
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
-- * 'iSupplementalImps' - Provide a list of any necessary supplemental IMPs. You need supplemental IMPs if the CPL that you're using for your input is in an incomplete IMP. Specify either the supplemental IMP directories with a trailing slash or the ASSETMAP.xml files. For example ["s3://bucket/ov/", "s3://bucket/vf2/ASSETMAP.xml"]. You don't need to specify the IMP that contains your input CPL, because the service automatically detects it.
--
-- * 'iProgramNumber' - Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
--
-- * 'iAudioSelectorGroups' - Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
--
-- * 'iTimecodeSource' - Undocumented member.
--
-- * 'iAudioSelectors' - Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use mutiple Audio selectors per input.
--
-- * 'iDecryptionSettings' - Settings for decrypting any input files that are encrypted.
--
-- * 'iDeblockFilter' - Undocumented member.
--
-- * 'iInputClippings' - (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
--
-- * 'iDenoiseFilter' - Undocumented member.
--
-- * 'iImageInserter' - Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
--
-- * 'iFilterStrength' - Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
--
-- * 'iPsiControl' - Undocumented member.
--
-- * 'iCaptionSelectors' - Use Captions selectors (CaptionSelectors) to specify the captions data from the input that you will use in your outputs. You can use mutiple captions selectors per input.
--
-- * 'iFileInput' - Specify the source file for your transcoding job. You can use multiple inputs in a single job. The service concatenates these inputs, in the order that you specify them in the job, to create the outputs. If your input format is IMF, specify your input by providing the path to your CPL. For example, "s3://bucket/vf/cpl.xml". If the CPL is in an incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps) to specify any supplemental IMPs that contain assets referenced by the CPL.
--
-- * 'iFilterEnable' - Undocumented member.
input
    :: Input
input =
  Input'
    { _iVideoSelector = Nothing
    , _iSupplementalImps = Nothing
    , _iProgramNumber = Nothing
    , _iAudioSelectorGroups = Nothing
    , _iTimecodeSource = Nothing
    , _iAudioSelectors = Nothing
    , _iDecryptionSettings = Nothing
    , _iDeblockFilter = Nothing
    , _iInputClippings = Nothing
    , _iDenoiseFilter = Nothing
    , _iImageInserter = Nothing
    , _iFilterStrength = Nothing
    , _iPsiControl = Nothing
    , _iCaptionSelectors = Nothing
    , _iFileInput = Nothing
    , _iFilterEnable = Nothing
    }


-- | Undocumented member.
iVideoSelector :: Lens' Input (Maybe VideoSelector)
iVideoSelector = lens _iVideoSelector (\ s a -> s{_iVideoSelector = a})

-- | Provide a list of any necessary supplemental IMPs. You need supplemental IMPs if the CPL that you're using for your input is in an incomplete IMP. Specify either the supplemental IMP directories with a trailing slash or the ASSETMAP.xml files. For example ["s3://bucket/ov/", "s3://bucket/vf2/ASSETMAP.xml"]. You don't need to specify the IMP that contains your input CPL, because the service automatically detects it.
iSupplementalImps :: Lens' Input [Text]
iSupplementalImps = lens _iSupplementalImps (\ s a -> s{_iSupplementalImps = a}) . _Default . _Coerce

-- | Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
iProgramNumber :: Lens' Input (Maybe Natural)
iProgramNumber = lens _iProgramNumber (\ s a -> s{_iProgramNumber = a}) . mapping _Nat

-- | Specifies set of audio selectors within an input to combine. An input may have multiple audio selector groups. See "Audio Selector Group":#inputs-audio_selector_group for more information.
iAudioSelectorGroups :: Lens' Input (HashMap Text AudioSelectorGroup)
iAudioSelectorGroups = lens _iAudioSelectorGroups (\ s a -> s{_iAudioSelectorGroups = a}) . _Default . _Map

-- | Undocumented member.
iTimecodeSource :: Lens' Input (Maybe InputTimecodeSource)
iTimecodeSource = lens _iTimecodeSource (\ s a -> s{_iTimecodeSource = a})

-- | Use Audio selectors (AudioSelectors) to specify a track or set of tracks from the input that you will use in your outputs. You can use mutiple Audio selectors per input.
iAudioSelectors :: Lens' Input (HashMap Text AudioSelector)
iAudioSelectors = lens _iAudioSelectors (\ s a -> s{_iAudioSelectors = a}) . _Default . _Map

-- | Settings for decrypting any input files that are encrypted.
iDecryptionSettings :: Lens' Input (Maybe InputDecryptionSettings)
iDecryptionSettings = lens _iDecryptionSettings (\ s a -> s{_iDecryptionSettings = a})

-- | Undocumented member.
iDeblockFilter :: Lens' Input (Maybe InputDeblockFilter)
iDeblockFilter = lens _iDeblockFilter (\ s a -> s{_iDeblockFilter = a})

-- | (InputClippings) contains sets of start and end times that together specify a portion of the input to be used in the outputs. If you provide only a start time, the clip will be the entire input from that point to the end. If you provide only an end time, it will be the entire input up to that point. When you specify more than one input clip, the transcoding service creates the job outputs by stringing the clips together in the order you specify them.
iInputClippings :: Lens' Input [InputClipping]
iInputClippings = lens _iInputClippings (\ s a -> s{_iInputClippings = a}) . _Default . _Coerce

-- | Undocumented member.
iDenoiseFilter :: Lens' Input (Maybe InputDenoiseFilter)
iDenoiseFilter = lens _iDenoiseFilter (\ s a -> s{_iDenoiseFilter = a})

-- | Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
iImageInserter :: Lens' Input (Maybe ImageInserter)
iImageInserter = lens _iImageInserter (\ s a -> s{_iImageInserter = a})

-- | Use Filter strength (FilterStrength) to adjust the magnitude the input filter settings (Deblock and Denoise). The range is -5 to 5. Default is 0.
iFilterStrength :: Lens' Input (Maybe Int)
iFilterStrength = lens _iFilterStrength (\ s a -> s{_iFilterStrength = a})

-- | Undocumented member.
iPsiControl :: Lens' Input (Maybe InputPsiControl)
iPsiControl = lens _iPsiControl (\ s a -> s{_iPsiControl = a})

-- | Use Captions selectors (CaptionSelectors) to specify the captions data from the input that you will use in your outputs. You can use mutiple captions selectors per input.
iCaptionSelectors :: Lens' Input (HashMap Text CaptionSelector)
iCaptionSelectors = lens _iCaptionSelectors (\ s a -> s{_iCaptionSelectors = a}) . _Default . _Map

-- | Specify the source file for your transcoding job. You can use multiple inputs in a single job. The service concatenates these inputs, in the order that you specify them in the job, to create the outputs. If your input format is IMF, specify your input by providing the path to your CPL. For example, "s3://bucket/vf/cpl.xml". If the CPL is in an incomplete IMP, make sure to use *Supplemental IMPs* (SupplementalImps) to specify any supplemental IMPs that contain assets referenced by the CPL.
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
                   (x .:? "videoSelector") <*>
                     (x .:? "supplementalImps" .!= mempty)
                     <*> (x .:? "programNumber")
                     <*> (x .:? "audioSelectorGroups" .!= mempty)
                     <*> (x .:? "timecodeSource")
                     <*> (x .:? "audioSelectors" .!= mempty)
                     <*> (x .:? "decryptionSettings")
                     <*> (x .:? "deblockFilter")
                     <*> (x .:? "inputClippings" .!= mempty)
                     <*> (x .:? "denoiseFilter")
                     <*> (x .:? "imageInserter")
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
                  ("supplementalImps" .=) <$> _iSupplementalImps,
                  ("programNumber" .=) <$> _iProgramNumber,
                  ("audioSelectorGroups" .=) <$> _iAudioSelectorGroups,
                  ("timecodeSource" .=) <$> _iTimecodeSource,
                  ("audioSelectors" .=) <$> _iAudioSelectors,
                  ("decryptionSettings" .=) <$> _iDecryptionSettings,
                  ("deblockFilter" .=) <$> _iDeblockFilter,
                  ("inputClippings" .=) <$> _iInputClippings,
                  ("denoiseFilter" .=) <$> _iDenoiseFilter,
                  ("imageInserter" .=) <$> _iImageInserter,
                  ("filterStrength" .=) <$> _iFilterStrength,
                  ("psiControl" .=) <$> _iPsiControl,
                  ("captionSelectors" .=) <$> _iCaptionSelectors,
                  ("fileInput" .=) <$> _iFileInput,
                  ("filterEnable" .=) <$> _iFilterEnable])

-- | To transcode only portions of your input (clips), include one Input clipping (one instance of InputClipping in the JSON job file) for each input clip. All input clips you specify will be included in every output of the job.
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
-- * 'icEndTimecode' - Set End timecode (EndTimecode) to the end of the portion of the input you are clipping. The frame corresponding to the End timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second, and FF is the frame number. When choosing this value, take into account your setting for timecode source under input settings (InputTimecodeSource). For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to end six minutes into the video, use 01:06:00:00.
--
-- * 'icStartTimecode' - Set Start timecode (StartTimecode) to the beginning of the portion of the input you are clipping. The frame corresponding to the Start timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second, and FF is the frame number. When choosing this value, take into account your setting for Input timecode source. For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to begin five minutes into the video, use 01:05:00:00.
inputClipping
    :: InputClipping
inputClipping =
  InputClipping' {_icEndTimecode = Nothing, _icStartTimecode = Nothing}


-- | Set End timecode (EndTimecode) to the end of the portion of the input you are clipping. The frame corresponding to the End timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second, and FF is the frame number. When choosing this value, take into account your setting for timecode source under input settings (InputTimecodeSource). For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to end six minutes into the video, use 01:06:00:00.
icEndTimecode :: Lens' InputClipping (Maybe Text)
icEndTimecode = lens _icEndTimecode (\ s a -> s{_icEndTimecode = a})

-- | Set Start timecode (StartTimecode) to the beginning of the portion of the input you are clipping. The frame corresponding to the Start timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second, and FF is the frame number. When choosing this value, take into account your setting for Input timecode source. For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to begin five minutes into the video, use 01:05:00:00.
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

-- | Specify the decryption settings used to decrypt encrypted input
--
-- /See:/ 'inputDecryptionSettings' smart constructor.
data InputDecryptionSettings = InputDecryptionSettings'
  { _idsEncryptedDecryptionKey :: !(Maybe Text)
  , _idsKMSKeyRegion           :: !(Maybe Text)
  , _idsDecryptionMode         :: !(Maybe DecryptionMode)
  , _idsInitializationVector   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputDecryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idsEncryptedDecryptionKey' - Decryption key either 128 or 192 or 256 bits encrypted with KMS
--
-- * 'idsKMSKeyRegion' - The AWS region in which decryption key was encrypted with KMS
--
-- * 'idsDecryptionMode' - Undocumented member.
--
-- * 'idsInitializationVector' - Initialization Vector 96 bits (CTR/GCM mode only) or 128 bits.
inputDecryptionSettings
    :: InputDecryptionSettings
inputDecryptionSettings =
  InputDecryptionSettings'
    { _idsEncryptedDecryptionKey = Nothing
    , _idsKMSKeyRegion = Nothing
    , _idsDecryptionMode = Nothing
    , _idsInitializationVector = Nothing
    }


-- | Decryption key either 128 or 192 or 256 bits encrypted with KMS
idsEncryptedDecryptionKey :: Lens' InputDecryptionSettings (Maybe Text)
idsEncryptedDecryptionKey = lens _idsEncryptedDecryptionKey (\ s a -> s{_idsEncryptedDecryptionKey = a})

-- | The AWS region in which decryption key was encrypted with KMS
idsKMSKeyRegion :: Lens' InputDecryptionSettings (Maybe Text)
idsKMSKeyRegion = lens _idsKMSKeyRegion (\ s a -> s{_idsKMSKeyRegion = a})

-- | Undocumented member.
idsDecryptionMode :: Lens' InputDecryptionSettings (Maybe DecryptionMode)
idsDecryptionMode = lens _idsDecryptionMode (\ s a -> s{_idsDecryptionMode = a})

-- | Initialization Vector 96 bits (CTR/GCM mode only) or 128 bits.
idsInitializationVector :: Lens' InputDecryptionSettings (Maybe Text)
idsInitializationVector = lens _idsInitializationVector (\ s a -> s{_idsInitializationVector = a})

instance FromJSON InputDecryptionSettings where
        parseJSON
          = withObject "InputDecryptionSettings"
              (\ x ->
                 InputDecryptionSettings' <$>
                   (x .:? "encryptedDecryptionKey") <*>
                     (x .:? "kmsKeyRegion")
                     <*> (x .:? "decryptionMode")
                     <*> (x .:? "initializationVector"))

instance Hashable InputDecryptionSettings where

instance NFData InputDecryptionSettings where

instance ToJSON InputDecryptionSettings where
        toJSON InputDecryptionSettings'{..}
          = object
              (catMaybes
                 [("encryptedDecryptionKey" .=) <$>
                    _idsEncryptedDecryptionKey,
                  ("kmsKeyRegion" .=) <$> _idsKMSKeyRegion,
                  ("decryptionMode" .=) <$> _idsDecryptionMode,
                  ("initializationVector" .=) <$>
                    _idsInitializationVector])

-- | Specified video input in a template.
--
-- /See:/ 'inputTemplate' smart constructor.
data InputTemplate = InputTemplate'
  { _itVideoSelector       :: !(Maybe VideoSelector)
  , _itProgramNumber       :: !(Maybe Nat)
  , _itAudioSelectorGroups :: !(Maybe (Map Text AudioSelectorGroup))
  , _itTimecodeSource      :: !(Maybe InputTimecodeSource)
  , _itAudioSelectors      :: !(Maybe (Map Text AudioSelector))
  , _itDeblockFilter       :: !(Maybe InputDeblockFilter)
  , _itInputClippings      :: !(Maybe [InputClipping])
  , _itDenoiseFilter       :: !(Maybe InputDenoiseFilter)
  , _itImageInserter       :: !(Maybe ImageInserter)
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
-- * 'itImageInserter' - Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
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
    , _itImageInserter = Nothing
    , _itFilterStrength = Nothing
    , _itPsiControl = Nothing
    , _itCaptionSelectors = Nothing
    , _itFilterEnable = Nothing
    }


-- | Undocumented member.
itVideoSelector :: Lens' InputTemplate (Maybe VideoSelector)
itVideoSelector = lens _itVideoSelector (\ s a -> s{_itVideoSelector = a})

-- | Use Program (programNumber) to select a specific program from within a multi-program transport stream. Note that Quad 4K is not currently supported. Default is the first program within the transport stream. If the program you specify doesn't exist, the transcoding service will use this default.
itProgramNumber :: Lens' InputTemplate (Maybe Natural)
itProgramNumber = lens _itProgramNumber (\ s a -> s{_itProgramNumber = a}) . mapping _Nat

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

-- | Enable the image inserter feature to include a graphic overlay on your video. Enable or disable this feature for each input individually. This setting is disabled by default.
itImageInserter :: Lens' InputTemplate (Maybe ImageInserter)
itImageInserter = lens _itImageInserter (\ s a -> s{_itImageInserter = a})

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
                     <*> (x .:? "imageInserter")
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
                  ("imageInserter" .=) <$> _itImageInserter,
                  ("filterStrength" .=) <$> _itFilterStrength,
                  ("psiControl" .=) <$> _itPsiControl,
                  ("captionSelectors" .=) <$> _itCaptionSelectors,
                  ("filterEnable" .=) <$> _itFilterEnable])

-- | Settings that specify how your still graphic overlay appears.
--
-- /See:/ 'insertableImage' smart constructor.
data InsertableImage = InsertableImage'
  { _iiImageX             :: !(Maybe Nat)
  , _iiHeight             :: !(Maybe Nat)
  , _iiStartTime          :: !(Maybe Text)
  , _iiFadeOut            :: !(Maybe Nat)
  , _iiWidth              :: !(Maybe Nat)
  , _iiOpacity            :: !(Maybe Nat)
  , _iiLayer              :: !(Maybe Nat)
  , _iiDuration           :: !(Maybe Nat)
  , _iiImageY             :: !(Maybe Nat)
  , _iiImageInserterInput :: !(Maybe Text)
  , _iiFadeIn             :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InsertableImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiImageX' - Specify the distance, in pixels, between the inserted image and the left edge of the video frame. Required for any image overlay that you specify.
--
-- * 'iiHeight' - Specify the height of the inserted image in pixels. If you specify a value that's larger than the video resolution height, the service will crop your overlaid image to fit. To use the native height of the image, keep this setting blank.
--
-- * 'iiStartTime' - Specify the timecode of the frame that you want the overlay to first appear on. This must be in timecode (HH:MM:SS:FF or HH:MM:SS;FF) format. Remember to take into account your timecode source settings.
--
-- * 'iiFadeOut' - Specify the length of time, in milliseconds, between the end of the time that you have specified for the image overlay Duration and when the overlaid image has faded to total transparency. If you don't specify a value for Fade-out, the image will disappear abruptly at the end of the inserted image duration.
--
-- * 'iiWidth' - Specify the width of the inserted image in pixels. If you specify a value that's larger than the video resolution width, the service will crop your overlaid image to fit. To use the native width of the image, keep this setting blank.
--
-- * 'iiOpacity' - Use Opacity (Opacity) to specify how much of the underlying video shows through the inserted image. 0 is transparent and 100 is fully opaque. Default is 50.
--
-- * 'iiLayer' - Specify how overlapping inserted images appear. Images with higher values for Layer appear on top of images with lower values for Layer.
--
-- * 'iiDuration' - Specify the time, in milliseconds, for the image to remain on the output video. This duration includes fade-in time but not fade-out time.
--
-- * 'iiImageY' - Specify the distance, in pixels, between the overlaid image and the top edge of the video frame. Required for any image overlay that you specify.
--
-- * 'iiImageInserterInput' - Specify the Amazon S3 location of the image that you want to overlay on the video. Use a PNG or TGA file.
--
-- * 'iiFadeIn' - Specify the length of time, in milliseconds, between the Start time that you specify for the image insertion and the time that the image appears at full opacity. Full opacity is the level that you specify for the opacity setting. If you don't specify a value for Fade-in, the image will appear abruptly at the overlay start time.
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


-- | Specify the distance, in pixels, between the inserted image and the left edge of the video frame. Required for any image overlay that you specify.
iiImageX :: Lens' InsertableImage (Maybe Natural)
iiImageX = lens _iiImageX (\ s a -> s{_iiImageX = a}) . mapping _Nat

-- | Specify the height of the inserted image in pixels. If you specify a value that's larger than the video resolution height, the service will crop your overlaid image to fit. To use the native height of the image, keep this setting blank.
iiHeight :: Lens' InsertableImage (Maybe Natural)
iiHeight = lens _iiHeight (\ s a -> s{_iiHeight = a}) . mapping _Nat

-- | Specify the timecode of the frame that you want the overlay to first appear on. This must be in timecode (HH:MM:SS:FF or HH:MM:SS;FF) format. Remember to take into account your timecode source settings.
iiStartTime :: Lens' InsertableImage (Maybe Text)
iiStartTime = lens _iiStartTime (\ s a -> s{_iiStartTime = a})

-- | Specify the length of time, in milliseconds, between the end of the time that you have specified for the image overlay Duration and when the overlaid image has faded to total transparency. If you don't specify a value for Fade-out, the image will disappear abruptly at the end of the inserted image duration.
iiFadeOut :: Lens' InsertableImage (Maybe Natural)
iiFadeOut = lens _iiFadeOut (\ s a -> s{_iiFadeOut = a}) . mapping _Nat

-- | Specify the width of the inserted image in pixels. If you specify a value that's larger than the video resolution width, the service will crop your overlaid image to fit. To use the native width of the image, keep this setting blank.
iiWidth :: Lens' InsertableImage (Maybe Natural)
iiWidth = lens _iiWidth (\ s a -> s{_iiWidth = a}) . mapping _Nat

-- | Use Opacity (Opacity) to specify how much of the underlying video shows through the inserted image. 0 is transparent and 100 is fully opaque. Default is 50.
iiOpacity :: Lens' InsertableImage (Maybe Natural)
iiOpacity = lens _iiOpacity (\ s a -> s{_iiOpacity = a}) . mapping _Nat

-- | Specify how overlapping inserted images appear. Images with higher values for Layer appear on top of images with lower values for Layer.
iiLayer :: Lens' InsertableImage (Maybe Natural)
iiLayer = lens _iiLayer (\ s a -> s{_iiLayer = a}) . mapping _Nat

-- | Specify the time, in milliseconds, for the image to remain on the output video. This duration includes fade-in time but not fade-out time.
iiDuration :: Lens' InsertableImage (Maybe Natural)
iiDuration = lens _iiDuration (\ s a -> s{_iiDuration = a}) . mapping _Nat

-- | Specify the distance, in pixels, between the overlaid image and the top edge of the video frame. Required for any image overlay that you specify.
iiImageY :: Lens' InsertableImage (Maybe Natural)
iiImageY = lens _iiImageY (\ s a -> s{_iiImageY = a}) . mapping _Nat

-- | Specify the Amazon S3 location of the image that you want to overlay on the video. Use a PNG or TGA file.
iiImageInserterInput :: Lens' InsertableImage (Maybe Text)
iiImageInserterInput = lens _iiImageInserterInput (\ s a -> s{_iiImageInserterInput = a})

-- | Specify the length of time, in milliseconds, between the Start time that you specify for the image insertion and the time that the image appears at full opacity. Full opacity is the level that you specify for the opacity setting. If you don't specify a value for Fade-in, the image will appear abruptly at the overlay start time.
iiFadeIn :: Lens' InsertableImage (Maybe Natural)
iiFadeIn = lens _iiFadeIn (\ s a -> s{_iiFadeIn = a}) . mapping _Nat

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
  { _jStatus                     :: !(Maybe JobStatus)
  , _jJobTemplate                :: !(Maybe Text)
  , _jAccelerationSettings       :: !(Maybe AccelerationSettings)
  , _jARN                        :: !(Maybe Text)
  , _jCreatedAt                  :: !(Maybe POSIX)
  , _jQueue                      :: !(Maybe Text)
  , _jUserMetadata               :: !(Maybe (Map Text Text))
  , _jBillingTagsSource          :: !(Maybe BillingTagsSource)
  , _jOutputGroupDetails         :: !(Maybe [OutputGroupDetail])
  , _jErrorCode                  :: !(Maybe Int)
  , _jId                         :: !(Maybe Text)
  , _jTiming                     :: !(Maybe Timing)
  , _jErrorMessage               :: !(Maybe Text)
  , _jStatusUpdateIntervalInSecs :: !(Maybe Nat)
  , _jRole                       :: !Text
  , _jSettings                   :: !JobSettings
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jStatus' - Undocumented member.
--
-- * 'jJobTemplate' - The job template that the job is created from, if it is created from a job template.
--
-- * 'jAccelerationSettings' - Acceleration settings for job execution.
--
-- * 'jARN' - An identifier for this resource that is unique within all of AWS.
--
-- * 'jCreatedAt' - The time, in Unix epoch format in seconds, when the job got created.
--
-- * 'jQueue' - Optional. When you create a job, you can specify a queue to send it to. If you don't specify, the job will go to the default queue. For more about queues, see the User Guide topic at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
--
-- * 'jUserMetadata' - User-defined metadata that you want to associate with an MediaConvert job. You specify metadata in key/value pairs.
--
-- * 'jBillingTagsSource' - Undocumented member.
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
--
-- * 'jStatusUpdateIntervalInSecs' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- * 'jRole' - The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html
--
-- * 'jSettings' - Undocumented member.
job
    :: Text -- ^ 'jRole'
    -> JobSettings -- ^ 'jSettings'
    -> Job
job pRole_ pSettings_ =
  Job'
    { _jStatus = Nothing
    , _jJobTemplate = Nothing
    , _jAccelerationSettings = Nothing
    , _jARN = Nothing
    , _jCreatedAt = Nothing
    , _jQueue = Nothing
    , _jUserMetadata = Nothing
    , _jBillingTagsSource = Nothing
    , _jOutputGroupDetails = Nothing
    , _jErrorCode = Nothing
    , _jId = Nothing
    , _jTiming = Nothing
    , _jErrorMessage = Nothing
    , _jStatusUpdateIntervalInSecs = Nothing
    , _jRole = pRole_
    , _jSettings = pSettings_
    }


-- | Undocumented member.
jStatus :: Lens' Job (Maybe JobStatus)
jStatus = lens _jStatus (\ s a -> s{_jStatus = a})

-- | The job template that the job is created from, if it is created from a job template.
jJobTemplate :: Lens' Job (Maybe Text)
jJobTemplate = lens _jJobTemplate (\ s a -> s{_jJobTemplate = a})

-- | Acceleration settings for job execution.
jAccelerationSettings :: Lens' Job (Maybe AccelerationSettings)
jAccelerationSettings = lens _jAccelerationSettings (\ s a -> s{_jAccelerationSettings = a})

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

-- | Undocumented member.
jBillingTagsSource :: Lens' Job (Maybe BillingTagsSource)
jBillingTagsSource = lens _jBillingTagsSource (\ s a -> s{_jBillingTagsSource = a})

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

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
jStatusUpdateIntervalInSecs :: Lens' Job (Maybe Natural)
jStatusUpdateIntervalInSecs = lens _jStatusUpdateIntervalInSecs (\ s a -> s{_jStatusUpdateIntervalInSecs = a}) . mapping _Nat

-- | The IAM role you use for creating this job. For details about permissions, see the User Guide topic at the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/iam-role.html
jRole :: Lens' Job Text
jRole = lens _jRole (\ s a -> s{_jRole = a})

-- | Undocumented member.
jSettings :: Lens' Job JobSettings
jSettings = lens _jSettings (\ s a -> s{_jSettings = a})

instance FromJSON Job where
        parseJSON
          = withObject "Job"
              (\ x ->
                 Job' <$>
                   (x .:? "status") <*> (x .:? "jobTemplate") <*>
                     (x .:? "accelerationSettings")
                     <*> (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "queue")
                     <*> (x .:? "userMetadata" .!= mempty)
                     <*> (x .:? "billingTagsSource")
                     <*> (x .:? "outputGroupDetails" .!= mempty)
                     <*> (x .:? "errorCode")
                     <*> (x .:? "id")
                     <*> (x .:? "timing")
                     <*> (x .:? "errorMessage")
                     <*> (x .:? "statusUpdateIntervalInSecs")
                     <*> (x .: "role")
                     <*> (x .: "settings"))

instance Hashable Job where

instance NFData Job where

-- | JobSettings contains all the transcode settings for a job.
--
-- /See:/ 'jobSettings' smart constructor.
data JobSettings = JobSettings'
  { _jsEsam                   :: !(Maybe EsamSettings)
  , _jsInputs                 :: !(Maybe [Input])
  , _jsTimedMetadataInsertion :: !(Maybe TimedMetadataInsertion)
  , _jsNielsenConfiguration   :: !(Maybe NielsenConfiguration)
  , _jsAvailBlanking          :: !(Maybe AvailBlanking)
  , _jsMotionImageInserter    :: !(Maybe MotionImageInserter)
  , _jsTimecodeConfig         :: !(Maybe TimecodeConfig)
  , _jsOutputGroups           :: !(Maybe [OutputGroup])
  , _jsAdAvailOffset          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jsEsam' - Settings for Event Signaling And Messaging (ESAM).
--
-- * 'jsInputs' - Use Inputs (inputs) to define source file used in the transcode job. There can be multiple inputs add in a job. These inputs will be concantenated together to create the output.
--
-- * 'jsTimedMetadataInsertion' - Undocumented member.
--
-- * 'jsNielsenConfiguration' - Undocumented member.
--
-- * 'jsAvailBlanking' - Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
--
-- * 'jsMotionImageInserter' - Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
--
-- * 'jsTimecodeConfig' - Contains settings used to acquire and adjust timecode information from inputs.
--
-- * 'jsOutputGroups' - (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
--
-- * 'jsAdAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
jobSettings
    :: JobSettings
jobSettings =
  JobSettings'
    { _jsEsam = Nothing
    , _jsInputs = Nothing
    , _jsTimedMetadataInsertion = Nothing
    , _jsNielsenConfiguration = Nothing
    , _jsAvailBlanking = Nothing
    , _jsMotionImageInserter = Nothing
    , _jsTimecodeConfig = Nothing
    , _jsOutputGroups = Nothing
    , _jsAdAvailOffset = Nothing
    }


-- | Settings for Event Signaling And Messaging (ESAM).
jsEsam :: Lens' JobSettings (Maybe EsamSettings)
jsEsam = lens _jsEsam (\ s a -> s{_jsEsam = a})

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

-- | Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
jsMotionImageInserter :: Lens' JobSettings (Maybe MotionImageInserter)
jsMotionImageInserter = lens _jsMotionImageInserter (\ s a -> s{_jsMotionImageInserter = a})

-- | Contains settings used to acquire and adjust timecode information from inputs.
jsTimecodeConfig :: Lens' JobSettings (Maybe TimecodeConfig)
jsTimecodeConfig = lens _jsTimecodeConfig (\ s a -> s{_jsTimecodeConfig = a})

-- | (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
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
                   (x .:? "esam") <*> (x .:? "inputs" .!= mempty) <*>
                     (x .:? "timedMetadataInsertion")
                     <*> (x .:? "nielsenConfiguration")
                     <*> (x .:? "availBlanking")
                     <*> (x .:? "motionImageInserter")
                     <*> (x .:? "timecodeConfig")
                     <*> (x .:? "outputGroups" .!= mempty)
                     <*> (x .:? "adAvailOffset"))

instance Hashable JobSettings where

instance NFData JobSettings where

instance ToJSON JobSettings where
        toJSON JobSettings'{..}
          = object
              (catMaybes
                 [("esam" .=) <$> _jsEsam,
                  ("inputs" .=) <$> _jsInputs,
                  ("timedMetadataInsertion" .=) <$>
                    _jsTimedMetadataInsertion,
                  ("nielsenConfiguration" .=) <$>
                    _jsNielsenConfiguration,
                  ("availBlanking" .=) <$> _jsAvailBlanking,
                  ("motionImageInserter" .=) <$>
                    _jsMotionImageInserter,
                  ("timecodeConfig" .=) <$> _jsTimecodeConfig,
                  ("outputGroups" .=) <$> _jsOutputGroups,
                  ("adAvailOffset" .=) <$> _jsAdAvailOffset])

-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
--
-- /See:/ 'jobTemplate' smart constructor.
data JobTemplate = JobTemplate'
  { _jtAccelerationSettings       :: !(Maybe AccelerationSettings)
  , _jtLastUpdated                :: !(Maybe POSIX)
  , _jtARN                        :: !(Maybe Text)
  , _jtCreatedAt                  :: !(Maybe POSIX)
  , _jtCategory                   :: !(Maybe Text)
  , _jtQueue                      :: !(Maybe Text)
  , _jtType                       :: !(Maybe Type)
  , _jtStatusUpdateIntervalInSecs :: !(Maybe Nat)
  , _jtDescription                :: !(Maybe Text)
  , _jtSettings                   :: !JobTemplateSettings
  , _jtName                       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jtAccelerationSettings' - Acceleration settings for job execution.
--
-- * 'jtLastUpdated' - The timestamp in epoch seconds when the Job template was last updated.
--
-- * 'jtARN' - An identifier for this resource that is unique within all of AWS.
--
-- * 'jtCreatedAt' - The timestamp in epoch seconds for Job template creation.
--
-- * 'jtCategory' - An optional category you create to organize your job templates.
--
-- * 'jtQueue' - Optional. The queue that jobs created from this template are assigned to. If you don't specify this, jobs will go to the default queue.
--
-- * 'jtType' - A job template can be of two types: system or custom. System or built-in job templates can't be modified or deleted by the user.
--
-- * 'jtStatusUpdateIntervalInSecs' - Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
--
-- * 'jtDescription' - An optional description you create for each job template.
--
-- * 'jtSettings' - Undocumented member.
--
-- * 'jtName' - A name you create for each job template. Each name must be unique within your account.
jobTemplate
    :: JobTemplateSettings -- ^ 'jtSettings'
    -> Text -- ^ 'jtName'
    -> JobTemplate
jobTemplate pSettings_ pName_ =
  JobTemplate'
    { _jtAccelerationSettings = Nothing
    , _jtLastUpdated = Nothing
    , _jtARN = Nothing
    , _jtCreatedAt = Nothing
    , _jtCategory = Nothing
    , _jtQueue = Nothing
    , _jtType = Nothing
    , _jtStatusUpdateIntervalInSecs = Nothing
    , _jtDescription = Nothing
    , _jtSettings = pSettings_
    , _jtName = pName_
    }


-- | Acceleration settings for job execution.
jtAccelerationSettings :: Lens' JobTemplate (Maybe AccelerationSettings)
jtAccelerationSettings = lens _jtAccelerationSettings (\ s a -> s{_jtAccelerationSettings = a})

-- | The timestamp in epoch seconds when the Job template was last updated.
jtLastUpdated :: Lens' JobTemplate (Maybe UTCTime)
jtLastUpdated = lens _jtLastUpdated (\ s a -> s{_jtLastUpdated = a}) . mapping _Time

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

-- | A job template can be of two types: system or custom. System or built-in job templates can't be modified or deleted by the user.
jtType :: Lens' JobTemplate (Maybe Type)
jtType = lens _jtType (\ s a -> s{_jtType = a})

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
jtStatusUpdateIntervalInSecs :: Lens' JobTemplate (Maybe Natural)
jtStatusUpdateIntervalInSecs = lens _jtStatusUpdateIntervalInSecs (\ s a -> s{_jtStatusUpdateIntervalInSecs = a}) . mapping _Nat

-- | An optional description you create for each job template.
jtDescription :: Lens' JobTemplate (Maybe Text)
jtDescription = lens _jtDescription (\ s a -> s{_jtDescription = a})

-- | Undocumented member.
jtSettings :: Lens' JobTemplate JobTemplateSettings
jtSettings = lens _jtSettings (\ s a -> s{_jtSettings = a})

-- | A name you create for each job template. Each name must be unique within your account.
jtName :: Lens' JobTemplate Text
jtName = lens _jtName (\ s a -> s{_jtName = a})

instance FromJSON JobTemplate where
        parseJSON
          = withObject "JobTemplate"
              (\ x ->
                 JobTemplate' <$>
                   (x .:? "accelerationSettings") <*>
                     (x .:? "lastUpdated")
                     <*> (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "category")
                     <*> (x .:? "queue")
                     <*> (x .:? "type")
                     <*> (x .:? "statusUpdateIntervalInSecs")
                     <*> (x .:? "description")
                     <*> (x .: "settings")
                     <*> (x .: "name"))

instance Hashable JobTemplate where

instance NFData JobTemplate where

-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
--
-- /See:/ 'jobTemplateSettings' smart constructor.
data JobTemplateSettings = JobTemplateSettings'
  { _jtsEsam                   :: !(Maybe EsamSettings)
  , _jtsInputs                 :: !(Maybe [InputTemplate])
  , _jtsTimedMetadataInsertion :: !(Maybe TimedMetadataInsertion)
  , _jtsNielsenConfiguration   :: !(Maybe NielsenConfiguration)
  , _jtsAvailBlanking          :: !(Maybe AvailBlanking)
  , _jtsMotionImageInserter    :: !(Maybe MotionImageInserter)
  , _jtsTimecodeConfig         :: !(Maybe TimecodeConfig)
  , _jtsOutputGroups           :: !(Maybe [OutputGroup])
  , _jtsAdAvailOffset          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobTemplateSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jtsEsam' - Settings for Event Signaling And Messaging (ESAM).
--
-- * 'jtsInputs' - Use Inputs (inputs) to define the source file used in the transcode job. There can only be one input in a job template.  Using the API, you can include multiple inputs when referencing a job template.
--
-- * 'jtsTimedMetadataInsertion' - Undocumented member.
--
-- * 'jtsNielsenConfiguration' - Undocumented member.
--
-- * 'jtsAvailBlanking' - Settings for ad avail blanking.  Video can be blanked or overlaid with an image, and audio muted during SCTE-35 triggered ad avails.
--
-- * 'jtsMotionImageInserter' - Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
--
-- * 'jtsTimecodeConfig' - Contains settings used to acquire and adjust timecode information from inputs.
--
-- * 'jtsOutputGroups' - (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
--
-- * 'jtsAdAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time.
jobTemplateSettings
    :: JobTemplateSettings
jobTemplateSettings =
  JobTemplateSettings'
    { _jtsEsam = Nothing
    , _jtsInputs = Nothing
    , _jtsTimedMetadataInsertion = Nothing
    , _jtsNielsenConfiguration = Nothing
    , _jtsAvailBlanking = Nothing
    , _jtsMotionImageInserter = Nothing
    , _jtsTimecodeConfig = Nothing
    , _jtsOutputGroups = Nothing
    , _jtsAdAvailOffset = Nothing
    }


-- | Settings for Event Signaling And Messaging (ESAM).
jtsEsam :: Lens' JobTemplateSettings (Maybe EsamSettings)
jtsEsam = lens _jtsEsam (\ s a -> s{_jtsEsam = a})

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

-- | Overlay motion graphics on top of your video. The motion graphics that you specify here appear on all outputs in all output groups.
jtsMotionImageInserter :: Lens' JobTemplateSettings (Maybe MotionImageInserter)
jtsMotionImageInserter = lens _jtsMotionImageInserter (\ s a -> s{_jtsMotionImageInserter = a})

-- | Contains settings used to acquire and adjust timecode information from inputs.
jtsTimecodeConfig :: Lens' JobTemplateSettings (Maybe TimecodeConfig)
jtsTimecodeConfig = lens _jtsTimecodeConfig (\ s a -> s{_jtsTimecodeConfig = a})

-- | (OutputGroups) contains one group of settings for each set of outputs that share a common package type. All unpackaged files (MPEG-4, MPEG-2 TS, Quicktime, MXF, and no container) are grouped in a single output group as well. Required in (OutputGroups) is a group of settings that apply to the whole group. This required object depends on the value you set for (Type) under (OutputGroups)>(OutputGroupSettings). Type, settings object pairs are as follows. * FILE_GROUP_SETTINGS, FileGroupSettings * HLS_GROUP_SETTINGS, HlsGroupSettings * DASH_ISO_GROUP_SETTINGS, DashIsoGroupSettings * MS_SMOOTH_GROUP_SETTINGS, MsSmoothGroupSettings * CMAF_GROUP_SETTINGS, CmafGroupSettings
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
                   (x .:? "esam") <*> (x .:? "inputs" .!= mempty) <*>
                     (x .:? "timedMetadataInsertion")
                     <*> (x .:? "nielsenConfiguration")
                     <*> (x .:? "availBlanking")
                     <*> (x .:? "motionImageInserter")
                     <*> (x .:? "timecodeConfig")
                     <*> (x .:? "outputGroups" .!= mempty)
                     <*> (x .:? "adAvailOffset"))

instance Hashable JobTemplateSettings where

instance NFData JobTemplateSettings where

instance ToJSON JobTemplateSettings where
        toJSON JobTemplateSettings'{..}
          = object
              (catMaybes
                 [("esam" .=) <$> _jtsEsam,
                  ("inputs" .=) <$> _jtsInputs,
                  ("timedMetadataInsertion" .=) <$>
                    _jtsTimedMetadataInsertion,
                  ("nielsenConfiguration" .=) <$>
                    _jtsNielsenConfiguration,
                  ("availBlanking" .=) <$> _jtsAvailBlanking,
                  ("motionImageInserter" .=) <$>
                    _jtsMotionImageInserter,
                  ("timecodeConfig" .=) <$> _jtsTimecodeConfig,
                  ("outputGroups" .=) <$> _jtsOutputGroups,
                  ("adAvailOffset" .=) <$> _jtsAdAvailOffset])

-- | Settings for SCTE-35 signals from ESAM. Include this in your job settings to put SCTE-35 markers in your HLS and transport stream outputs at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
--
-- /See:/ 'm2tsScte35Esam' smart constructor.
newtype M2tsScte35Esam = M2tsScte35Esam'
  { _mseScte35EsamPid :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'M2tsScte35Esam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mseScte35EsamPid' - Packet Identifier (PID) of the SCTE-35 stream in the transport stream generated by ESAM.
m2tsScte35Esam
    :: M2tsScte35Esam
m2tsScte35Esam = M2tsScte35Esam' {_mseScte35EsamPid = Nothing}


-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream generated by ESAM.
mseScte35EsamPid :: Lens' M2tsScte35Esam (Maybe Natural)
mseScte35EsamPid = lens _mseScte35EsamPid (\ s a -> s{_mseScte35EsamPid = a}) . mapping _Nat

instance FromJSON M2tsScte35Esam where
        parseJSON
          = withObject "M2tsScte35Esam"
              (\ x -> M2tsScte35Esam' <$> (x .:? "scte35EsamPid"))

instance Hashable M2tsScte35Esam where

instance NFData M2tsScte35Esam where

instance ToJSON M2tsScte35Esam where
        toJSON M2tsScte35Esam'{..}
          = object
              (catMaybes
                 [("scte35EsamPid" .=) <$> _mseScte35EsamPid])

-- | MPEG-2 TS container settings. These apply to outputs in a File output group when the output's container (ContainerType) is MPEG-2 Transport Stream (M2TS). In these assets, data is organized by the program map table (PMT). Each transport stream program contains subsets of data, including audio, video, and metadata. Each of these subsets of data has a numerical label called a packet identifier (PID). Each transport stream program corresponds to one MediaConvert output. The PMT lists the types of data in a program along with their PID. Downstream systems and players use the program map table to look up the PID for each type of data it accesses and then uses the PIDs to locate specific data within the asset.
--
-- /See:/ 'm2tsSettings' smart constructor.
data M2tsSettings = M2tsSettings'
  { _mssPmtPid               :: !(Maybe Nat)
  , _mssVideoPid             :: !(Maybe Nat)
  , _mssBufferModel          :: !(Maybe M2tsBufferModel)
  , _mssProgramNumber        :: !(Maybe Nat)
  , _mssScte35Pid            :: !(Maybe Nat)
  , _mssMinEbpInterval       :: !(Maybe Nat)
  , _mssTransportStreamId    :: !(Maybe Nat)
  , _mssMaxPcrInterval       :: !(Maybe Nat)
  , _mssFragmentTime         :: !(Maybe Double)
  , _mssPrivateMetadataPid   :: !(Maybe Nat)
  , _mssScte35Esam           :: !(Maybe M2tsScte35Esam)
  , _mssPmtInterval          :: !(Maybe Nat)
  , _mssDvbSdtSettings       :: !(Maybe DvbSdtSettings)
  , _mssNullPacketBitrate    :: !(Maybe Double)
  , _mssAudioBufferModel     :: !(Maybe M2tsAudioBufferModel)
  , _mssTimedMetadataPid     :: !(Maybe Nat)
  , _mssAudioFramesPerPes    :: !(Maybe Nat)
  , _mssPcrPid               :: !(Maybe Nat)
  , _mssSegmentationMarkers  :: !(Maybe M2tsSegmentationMarkers)
  , _mssDvbSubPids           :: !(Maybe [Nat])
  , _mssScte35Source         :: !(Maybe M2tsScte35Source)
  , _mssPatInterval          :: !(Maybe Nat)
  , _mssForceTsVideoEbpOrder :: !(Maybe M2tsForceTsVideoEbpOrder)
  , _mssEsRateInPes          :: !(Maybe M2tsEsRateInPes)
  , _mssBitrate              :: !(Maybe Nat)
  , _mssAudioPids            :: !(Maybe [Nat])
  , _mssDvbTeletextPid       :: !(Maybe Nat)
  , _mssNielsenId3           :: !(Maybe M2tsNielsenId3)
  , _mssSegmentationTime     :: !(Maybe Double)
  , _mssEbpAudioInterval     :: !(Maybe M2tsEbpAudioInterval)
  , _mssDvbNitSettings       :: !(Maybe DvbNitSettings)
  , _mssPcrControl           :: !(Maybe M2tsPcrControl)
  , _mssEbpPlacement         :: !(Maybe M2tsEbpPlacement)
  , _mssRateMode             :: !(Maybe M2tsRateMode)
  , _mssSegmentationStyle    :: !(Maybe M2tsSegmentationStyle)
  , _mssDvbTdtSettings       :: !(Maybe DvbTdtSettings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'M2tsSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mssPmtPid' - Specify the packet identifier (PID) for the program map table (PMT) itself. Default is 480.
--
-- * 'mssVideoPid' - Specify the packet identifier (PID) of the elementary video stream in the transport stream.
--
-- * 'mssBufferModel' - Undocumented member.
--
-- * 'mssProgramNumber' - Use Program number (programNumber) to specify the program number used in the program map table (PMT) for this output. Default is 1. Program numbers and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
--
-- * 'mssScte35Pid' - Specify the packet identifier (PID) of the SCTE-35 stream in the transport stream.
--
-- * 'mssMinEbpInterval' - When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker. The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
--
-- * 'mssTransportStreamId' - Specify the ID for the transport stream itself in the program map table for this output. Transport stream IDs and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
--
-- * 'mssMaxPcrInterval' - Specify the maximum time, in milliseconds, between Program Clock References (PCRs) inserted into the transport stream.
--
-- * 'mssFragmentTime' - The length, in seconds, of each fragment. Only used with EBP markers.
--
-- * 'mssPrivateMetadataPid' - Specify the packet identifier (PID) of the private metadata stream. Default is 503.
--
-- * 'mssScte35Esam' - Include this in your job settings to put SCTE-35 markers in your HLS and transport stream outputs at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
--
-- * 'mssPmtInterval' - Specify the number of milliseconds between instances of the program map table (PMT) in the output transport stream.
--
-- * 'mssDvbSdtSettings' - Undocumented member.
--
-- * 'mssNullPacketBitrate' - Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
--
-- * 'mssAudioBufferModel' - Undocumented member.
--
-- * 'mssTimedMetadataPid' - Specify the packet identifier (PID) for timed metadata in this output. Default is 502.
--
-- * 'mssAudioFramesPerPes' - The number of audio frames to insert for each PES packet.
--
-- * 'mssPcrPid' - Specify the packet identifier (PID) for the program clock reference (PCR) in this output. If you do not specify a value, the service will use the value for Video PID (VideoPid).
--
-- * 'mssSegmentationMarkers' - Undocumented member.
--
-- * 'mssDvbSubPids' - Specify the packet identifiers (PIDs) for DVB subtitle data included in this output. Specify multiple PIDs as a JSON array. Default is the range 460-479.
--
-- * 'mssScte35Source' - Undocumented member.
--
-- * 'mssPatInterval' - The number of milliseconds between instances of this table in the output transport stream.
--
-- * 'mssForceTsVideoEbpOrder' - Keep the default value (DEFAULT) unless you know that your audio EBP markers are incorrectly appearing before your video EBP markers. To correct this problem, set this value to Force (FORCE).
--
-- * 'mssEsRateInPes' - Undocumented member.
--
-- * 'mssBitrate' - Specify the output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate. Other common values are 3750000, 7500000, and 15000000.
--
-- * 'mssAudioPids' - Specify the packet identifiers (PIDs) for any elementary audio streams you include in this output. Specify multiple PIDs as a JSON array. Default is the range 482-492.
--
-- * 'mssDvbTeletextPid' - Specify the packet identifier (PID) for DVB teletext data you include in this output. Default is 499.
--
-- * 'mssNielsenId3' - Undocumented member.
--
-- * 'mssSegmentationTime' - Specify the length, in seconds, of each segment. Required unless markers is set to _none_.
--
-- * 'mssEbpAudioInterval' - Undocumented member.
--
-- * 'mssDvbNitSettings' - Undocumented member.
--
-- * 'mssPcrControl' - Undocumented member.
--
-- * 'mssEbpPlacement' - Undocumented member.
--
-- * 'mssRateMode' - Undocumented member.
--
-- * 'mssSegmentationStyle' - Undocumented member.
--
-- * 'mssDvbTdtSettings' - Undocumented member.
m2tsSettings
    :: M2tsSettings
m2tsSettings =
  M2tsSettings'
    { _mssPmtPid = Nothing
    , _mssVideoPid = Nothing
    , _mssBufferModel = Nothing
    , _mssProgramNumber = Nothing
    , _mssScte35Pid = Nothing
    , _mssMinEbpInterval = Nothing
    , _mssTransportStreamId = Nothing
    , _mssMaxPcrInterval = Nothing
    , _mssFragmentTime = Nothing
    , _mssPrivateMetadataPid = Nothing
    , _mssScte35Esam = Nothing
    , _mssPmtInterval = Nothing
    , _mssDvbSdtSettings = Nothing
    , _mssNullPacketBitrate = Nothing
    , _mssAudioBufferModel = Nothing
    , _mssTimedMetadataPid = Nothing
    , _mssAudioFramesPerPes = Nothing
    , _mssPcrPid = Nothing
    , _mssSegmentationMarkers = Nothing
    , _mssDvbSubPids = Nothing
    , _mssScte35Source = Nothing
    , _mssPatInterval = Nothing
    , _mssForceTsVideoEbpOrder = Nothing
    , _mssEsRateInPes = Nothing
    , _mssBitrate = Nothing
    , _mssAudioPids = Nothing
    , _mssDvbTeletextPid = Nothing
    , _mssNielsenId3 = Nothing
    , _mssSegmentationTime = Nothing
    , _mssEbpAudioInterval = Nothing
    , _mssDvbNitSettings = Nothing
    , _mssPcrControl = Nothing
    , _mssEbpPlacement = Nothing
    , _mssRateMode = Nothing
    , _mssSegmentationStyle = Nothing
    , _mssDvbTdtSettings = Nothing
    }


-- | Specify the packet identifier (PID) for the program map table (PMT) itself. Default is 480.
mssPmtPid :: Lens' M2tsSettings (Maybe Natural)
mssPmtPid = lens _mssPmtPid (\ s a -> s{_mssPmtPid = a}) . mapping _Nat

-- | Specify the packet identifier (PID) of the elementary video stream in the transport stream.
mssVideoPid :: Lens' M2tsSettings (Maybe Natural)
mssVideoPid = lens _mssVideoPid (\ s a -> s{_mssVideoPid = a}) . mapping _Nat

-- | Undocumented member.
mssBufferModel :: Lens' M2tsSettings (Maybe M2tsBufferModel)
mssBufferModel = lens _mssBufferModel (\ s a -> s{_mssBufferModel = a})

-- | Use Program number (programNumber) to specify the program number used in the program map table (PMT) for this output. Default is 1. Program numbers and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
mssProgramNumber :: Lens' M2tsSettings (Maybe Natural)
mssProgramNumber = lens _mssProgramNumber (\ s a -> s{_mssProgramNumber = a}) . mapping _Nat

-- | Specify the packet identifier (PID) of the SCTE-35 stream in the transport stream.
mssScte35Pid :: Lens' M2tsSettings (Maybe Natural)
mssScte35Pid = lens _mssScte35Pid (\ s a -> s{_mssScte35Pid = a}) . mapping _Nat

-- | When set, enforces that Encoder Boundary Points do not come within the specified time interval of each other by looking ahead at input video. If another EBP is going to come in within the specified time interval, the current EBP is not emitted, and the segment is "stretched" to the next marker. The lookahead value does not add latency to the system. The Live Event must be configured elsewhere to create sufficient latency to make the lookahead accurate.
mssMinEbpInterval :: Lens' M2tsSettings (Maybe Natural)
mssMinEbpInterval = lens _mssMinEbpInterval (\ s a -> s{_mssMinEbpInterval = a}) . mapping _Nat

-- | Specify the ID for the transport stream itself in the program map table for this output. Transport stream IDs and program map tables are parts of MPEG-2 transport stream containers, used for organizing data.
mssTransportStreamId :: Lens' M2tsSettings (Maybe Natural)
mssTransportStreamId = lens _mssTransportStreamId (\ s a -> s{_mssTransportStreamId = a}) . mapping _Nat

-- | Specify the maximum time, in milliseconds, between Program Clock References (PCRs) inserted into the transport stream.
mssMaxPcrInterval :: Lens' M2tsSettings (Maybe Natural)
mssMaxPcrInterval = lens _mssMaxPcrInterval (\ s a -> s{_mssMaxPcrInterval = a}) . mapping _Nat

-- | The length, in seconds, of each fragment. Only used with EBP markers.
mssFragmentTime :: Lens' M2tsSettings (Maybe Double)
mssFragmentTime = lens _mssFragmentTime (\ s a -> s{_mssFragmentTime = a})

-- | Specify the packet identifier (PID) of the private metadata stream. Default is 503.
mssPrivateMetadataPid :: Lens' M2tsSettings (Maybe Natural)
mssPrivateMetadataPid = lens _mssPrivateMetadataPid (\ s a -> s{_mssPrivateMetadataPid = a}) . mapping _Nat

-- | Include this in your job settings to put SCTE-35 markers in your HLS and transport stream outputs at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
mssScte35Esam :: Lens' M2tsSettings (Maybe M2tsScte35Esam)
mssScte35Esam = lens _mssScte35Esam (\ s a -> s{_mssScte35Esam = a})

-- | Specify the number of milliseconds between instances of the program map table (PMT) in the output transport stream.
mssPmtInterval :: Lens' M2tsSettings (Maybe Natural)
mssPmtInterval = lens _mssPmtInterval (\ s a -> s{_mssPmtInterval = a}) . mapping _Nat

-- | Undocumented member.
mssDvbSdtSettings :: Lens' M2tsSettings (Maybe DvbSdtSettings)
mssDvbSdtSettings = lens _mssDvbSdtSettings (\ s a -> s{_mssDvbSdtSettings = a})

-- | Value in bits per second of extra null packets to insert into the transport stream. This can be used if a downstream encryption system requires periodic null packets.
mssNullPacketBitrate :: Lens' M2tsSettings (Maybe Double)
mssNullPacketBitrate = lens _mssNullPacketBitrate (\ s a -> s{_mssNullPacketBitrate = a})

-- | Undocumented member.
mssAudioBufferModel :: Lens' M2tsSettings (Maybe M2tsAudioBufferModel)
mssAudioBufferModel = lens _mssAudioBufferModel (\ s a -> s{_mssAudioBufferModel = a})

-- | Specify the packet identifier (PID) for timed metadata in this output. Default is 502.
mssTimedMetadataPid :: Lens' M2tsSettings (Maybe Natural)
mssTimedMetadataPid = lens _mssTimedMetadataPid (\ s a -> s{_mssTimedMetadataPid = a}) . mapping _Nat

-- | The number of audio frames to insert for each PES packet.
mssAudioFramesPerPes :: Lens' M2tsSettings (Maybe Natural)
mssAudioFramesPerPes = lens _mssAudioFramesPerPes (\ s a -> s{_mssAudioFramesPerPes = a}) . mapping _Nat

-- | Specify the packet identifier (PID) for the program clock reference (PCR) in this output. If you do not specify a value, the service will use the value for Video PID (VideoPid).
mssPcrPid :: Lens' M2tsSettings (Maybe Natural)
mssPcrPid = lens _mssPcrPid (\ s a -> s{_mssPcrPid = a}) . mapping _Nat

-- | Undocumented member.
mssSegmentationMarkers :: Lens' M2tsSettings (Maybe M2tsSegmentationMarkers)
mssSegmentationMarkers = lens _mssSegmentationMarkers (\ s a -> s{_mssSegmentationMarkers = a})

-- | Specify the packet identifiers (PIDs) for DVB subtitle data included in this output. Specify multiple PIDs as a JSON array. Default is the range 460-479.
mssDvbSubPids :: Lens' M2tsSettings [Natural]
mssDvbSubPids = lens _mssDvbSubPids (\ s a -> s{_mssDvbSubPids = a}) . _Default . _Coerce

-- | Undocumented member.
mssScte35Source :: Lens' M2tsSettings (Maybe M2tsScte35Source)
mssScte35Source = lens _mssScte35Source (\ s a -> s{_mssScte35Source = a})

-- | The number of milliseconds between instances of this table in the output transport stream.
mssPatInterval :: Lens' M2tsSettings (Maybe Natural)
mssPatInterval = lens _mssPatInterval (\ s a -> s{_mssPatInterval = a}) . mapping _Nat

-- | Keep the default value (DEFAULT) unless you know that your audio EBP markers are incorrectly appearing before your video EBP markers. To correct this problem, set this value to Force (FORCE).
mssForceTsVideoEbpOrder :: Lens' M2tsSettings (Maybe M2tsForceTsVideoEbpOrder)
mssForceTsVideoEbpOrder = lens _mssForceTsVideoEbpOrder (\ s a -> s{_mssForceTsVideoEbpOrder = a})

-- | Undocumented member.
mssEsRateInPes :: Lens' M2tsSettings (Maybe M2tsEsRateInPes)
mssEsRateInPes = lens _mssEsRateInPes (\ s a -> s{_mssEsRateInPes = a})

-- | Specify the output bitrate of the transport stream in bits per second. Setting to 0 lets the muxer automatically determine the appropriate bitrate. Other common values are 3750000, 7500000, and 15000000.
mssBitrate :: Lens' M2tsSettings (Maybe Natural)
mssBitrate = lens _mssBitrate (\ s a -> s{_mssBitrate = a}) . mapping _Nat

-- | Specify the packet identifiers (PIDs) for any elementary audio streams you include in this output. Specify multiple PIDs as a JSON array. Default is the range 482-492.
mssAudioPids :: Lens' M2tsSettings [Natural]
mssAudioPids = lens _mssAudioPids (\ s a -> s{_mssAudioPids = a}) . _Default . _Coerce

-- | Specify the packet identifier (PID) for DVB teletext data you include in this output. Default is 499.
mssDvbTeletextPid :: Lens' M2tsSettings (Maybe Natural)
mssDvbTeletextPid = lens _mssDvbTeletextPid (\ s a -> s{_mssDvbTeletextPid = a}) . mapping _Nat

-- | Undocumented member.
mssNielsenId3 :: Lens' M2tsSettings (Maybe M2tsNielsenId3)
mssNielsenId3 = lens _mssNielsenId3 (\ s a -> s{_mssNielsenId3 = a})

-- | Specify the length, in seconds, of each segment. Required unless markers is set to _none_.
mssSegmentationTime :: Lens' M2tsSettings (Maybe Double)
mssSegmentationTime = lens _mssSegmentationTime (\ s a -> s{_mssSegmentationTime = a})

-- | Undocumented member.
mssEbpAudioInterval :: Lens' M2tsSettings (Maybe M2tsEbpAudioInterval)
mssEbpAudioInterval = lens _mssEbpAudioInterval (\ s a -> s{_mssEbpAudioInterval = a})

-- | Undocumented member.
mssDvbNitSettings :: Lens' M2tsSettings (Maybe DvbNitSettings)
mssDvbNitSettings = lens _mssDvbNitSettings (\ s a -> s{_mssDvbNitSettings = a})

-- | Undocumented member.
mssPcrControl :: Lens' M2tsSettings (Maybe M2tsPcrControl)
mssPcrControl = lens _mssPcrControl (\ s a -> s{_mssPcrControl = a})

-- | Undocumented member.
mssEbpPlacement :: Lens' M2tsSettings (Maybe M2tsEbpPlacement)
mssEbpPlacement = lens _mssEbpPlacement (\ s a -> s{_mssEbpPlacement = a})

-- | Undocumented member.
mssRateMode :: Lens' M2tsSettings (Maybe M2tsRateMode)
mssRateMode = lens _mssRateMode (\ s a -> s{_mssRateMode = a})

-- | Undocumented member.
mssSegmentationStyle :: Lens' M2tsSettings (Maybe M2tsSegmentationStyle)
mssSegmentationStyle = lens _mssSegmentationStyle (\ s a -> s{_mssSegmentationStyle = a})

-- | Undocumented member.
mssDvbTdtSettings :: Lens' M2tsSettings (Maybe DvbTdtSettings)
mssDvbTdtSettings = lens _mssDvbTdtSettings (\ s a -> s{_mssDvbTdtSettings = a})

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
                     <*> (x .:? "scte35Esam")
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
                     <*> (x .:? "forceTsVideoEbpOrder")
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
                 [("pmtPid" .=) <$> _mssPmtPid,
                  ("videoPid" .=) <$> _mssVideoPid,
                  ("bufferModel" .=) <$> _mssBufferModel,
                  ("programNumber" .=) <$> _mssProgramNumber,
                  ("scte35Pid" .=) <$> _mssScte35Pid,
                  ("minEbpInterval" .=) <$> _mssMinEbpInterval,
                  ("transportStreamId" .=) <$> _mssTransportStreamId,
                  ("maxPcrInterval" .=) <$> _mssMaxPcrInterval,
                  ("fragmentTime" .=) <$> _mssFragmentTime,
                  ("privateMetadataPid" .=) <$> _mssPrivateMetadataPid,
                  ("scte35Esam" .=) <$> _mssScte35Esam,
                  ("pmtInterval" .=) <$> _mssPmtInterval,
                  ("dvbSdtSettings" .=) <$> _mssDvbSdtSettings,
                  ("nullPacketBitrate" .=) <$> _mssNullPacketBitrate,
                  ("audioBufferModel" .=) <$> _mssAudioBufferModel,
                  ("timedMetadataPid" .=) <$> _mssTimedMetadataPid,
                  ("audioFramesPerPes" .=) <$> _mssAudioFramesPerPes,
                  ("pcrPid" .=) <$> _mssPcrPid,
                  ("segmentationMarkers" .=) <$>
                    _mssSegmentationMarkers,
                  ("dvbSubPids" .=) <$> _mssDvbSubPids,
                  ("scte35Source" .=) <$> _mssScte35Source,
                  ("patInterval" .=) <$> _mssPatInterval,
                  ("forceTsVideoEbpOrder" .=) <$>
                    _mssForceTsVideoEbpOrder,
                  ("esRateInPes" .=) <$> _mssEsRateInPes,
                  ("bitrate" .=) <$> _mssBitrate,
                  ("audioPids" .=) <$> _mssAudioPids,
                  ("dvbTeletextPid" .=) <$> _mssDvbTeletextPid,
                  ("nielsenId3" .=) <$> _mssNielsenId3,
                  ("segmentationTime" .=) <$> _mssSegmentationTime,
                  ("ebpAudioInterval" .=) <$> _mssEbpAudioInterval,
                  ("dvbNitSettings" .=) <$> _mssDvbNitSettings,
                  ("pcrControl" .=) <$> _mssPcrControl,
                  ("ebpPlacement" .=) <$> _mssEbpPlacement,
                  ("rateMode" .=) <$> _mssRateMode,
                  ("segmentationStyle" .=) <$> _mssSegmentationStyle,
                  ("dvbTdtSettings" .=) <$> _mssDvbTdtSettings])

-- | Settings for TS segments in HLS
--
-- /See:/ 'm3u8Settings' smart constructor.
data M3u8Settings = M3u8Settings'
  { _msPmtPid             :: !(Maybe Nat)
  , _msVideoPid           :: !(Maybe Nat)
  , _msProgramNumber      :: !(Maybe Nat)
  , _msScte35Pid          :: !(Maybe Nat)
  , _msTransportStreamId  :: !(Maybe Nat)
  , _msPrivateMetadataPid :: !(Maybe Nat)
  , _msPmtInterval        :: !(Maybe Nat)
  , _msTimedMetadataPid   :: !(Maybe Nat)
  , _msAudioFramesPerPes  :: !(Maybe Nat)
  , _msPcrPid             :: !(Maybe Nat)
  , _msTimedMetadata      :: !(Maybe TimedMetadata)
  , _msScte35Source       :: !(Maybe M3u8Scte35Source)
  , _msPatInterval        :: !(Maybe Nat)
  , _msAudioPids          :: !(Maybe [Nat])
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
msPmtPid :: Lens' M3u8Settings (Maybe Natural)
msPmtPid = lens _msPmtPid (\ s a -> s{_msPmtPid = a}) . mapping _Nat

-- | Packet Identifier (PID) of the elementary video stream in the transport stream.
msVideoPid :: Lens' M3u8Settings (Maybe Natural)
msVideoPid = lens _msVideoPid (\ s a -> s{_msVideoPid = a}) . mapping _Nat

-- | The value of the program number field in the Program Map Table.
msProgramNumber :: Lens' M3u8Settings (Maybe Natural)
msProgramNumber = lens _msProgramNumber (\ s a -> s{_msProgramNumber = a}) . mapping _Nat

-- | Packet Identifier (PID) of the SCTE-35 stream in the transport stream.
msScte35Pid :: Lens' M3u8Settings (Maybe Natural)
msScte35Pid = lens _msScte35Pid (\ s a -> s{_msScte35Pid = a}) . mapping _Nat

-- | The value of the transport stream ID field in the Program Map Table.
msTransportStreamId :: Lens' M3u8Settings (Maybe Natural)
msTransportStreamId = lens _msTransportStreamId (\ s a -> s{_msTransportStreamId = a}) . mapping _Nat

-- | Packet Identifier (PID) of the private metadata stream in the transport stream.
msPrivateMetadataPid :: Lens' M3u8Settings (Maybe Natural)
msPrivateMetadataPid = lens _msPrivateMetadataPid (\ s a -> s{_msPrivateMetadataPid = a}) . mapping _Nat

-- | The number of milliseconds between instances of this table in the output transport stream.
msPmtInterval :: Lens' M3u8Settings (Maybe Natural)
msPmtInterval = lens _msPmtInterval (\ s a -> s{_msPmtInterval = a}) . mapping _Nat

-- | Packet Identifier (PID) of the timed metadata stream in the transport stream.
msTimedMetadataPid :: Lens' M3u8Settings (Maybe Natural)
msTimedMetadataPid = lens _msTimedMetadataPid (\ s a -> s{_msTimedMetadataPid = a}) . mapping _Nat

-- | The number of audio frames to insert for each PES packet.
msAudioFramesPerPes :: Lens' M3u8Settings (Maybe Natural)
msAudioFramesPerPes = lens _msAudioFramesPerPes (\ s a -> s{_msAudioFramesPerPes = a}) . mapping _Nat

-- | Packet Identifier (PID) of the Program Clock Reference (PCR) in the transport stream. When no value is given, the encoder will assign the same value as the Video PID.
msPcrPid :: Lens' M3u8Settings (Maybe Natural)
msPcrPid = lens _msPcrPid (\ s a -> s{_msPcrPid = a}) . mapping _Nat

-- | Undocumented member.
msTimedMetadata :: Lens' M3u8Settings (Maybe TimedMetadata)
msTimedMetadata = lens _msTimedMetadata (\ s a -> s{_msTimedMetadata = a})

-- | Undocumented member.
msScte35Source :: Lens' M3u8Settings (Maybe M3u8Scte35Source)
msScte35Source = lens _msScte35Source (\ s a -> s{_msScte35Source = a})

-- | The number of milliseconds between instances of this table in the output transport stream.
msPatInterval :: Lens' M3u8Settings (Maybe Natural)
msPatInterval = lens _msPatInterval (\ s a -> s{_msPatInterval = a}) . mapping _Nat

-- | Packet Identifier (PID) of the elementary audio stream(s) in the transport stream. Multiple values are accepted, and can be entered in ranges and/or by comma separation.
msAudioPids :: Lens' M3u8Settings [Natural]
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

-- | Overlay motion graphics on top of your video at the time that you specify.
--
-- /See:/ 'motionImageInserter' smart constructor.
data MotionImageInserter = MotionImageInserter'
  { _miiFramerate     :: !(Maybe MotionImageInsertionFramerate)
  , _miiStartTime     :: !(Maybe Text)
  , _miiOffset        :: !(Maybe MotionImageInsertionOffset)
  , _miiInput         :: !(Maybe Text)
  , _miiInsertionMode :: !(Maybe MotionImageInsertionMode)
  , _miiPlayback      :: !(Maybe MotionImagePlayback)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MotionImageInserter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miiFramerate' - If your motion graphic asset is a .mov file, keep this setting unspecified. If your motion graphic asset is a series of .png files, specify the frame rate of the overlay in frames per second, as a fraction. For example, specify 24 fps as 24/1. Make sure that the number of images in your series matches the frame rate and your intended overlay duration. For example, if you want a 30-second overlay at 30 fps, you should have 900 .png images. This overlay frame rate doesn't need to match the frame rate of the underlying video.
--
-- * 'miiStartTime' - Specify when the motion overlay begins. Use timecode format (HH:MM:SS:FF or HH:MM:SS;FF). Make sure that the timecode you provide here takes into account how you have set up your timecode configuration under both job settings and input settings. The simplest way to do that is to set both to start at 0. If you need to set up your job to follow timecodes embedded in your source that don't start at zero, make sure that you specify a start time that is after the first embedded timecode. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/setting-up-timecode.html Find job-wide and input timecode configuration settings in your JSON job settings specification at settings>timecodeConfig>source and settings>inputs>timecodeSource.
--
-- * 'miiOffset' - Use Offset to specify the placement of your motion graphic overlay on the video frame. Specify in pixels, from the upper-left corner of the frame. If you don't specify an offset, the service scales your overlay to the full size of the frame. Otherwise, the service inserts the overlay at its native resolution and scales the size up or down with any video scaling.
--
-- * 'miiInput' - Specify the .mov file or series of .png files that you want to overlay on your video. For .png files, provide the file name of the first file in the series. Make sure that the names of the .png files end with sequential numbers that specify the order that they are played in. For example, overlay_000.png, overlay_001.png, overlay_002.png, and so on. The sequence must start at zero, and each image file name must have the same number of digits. Pad your initial file names with enough zeros to complete the sequence. For example, if the first image is overlay_0.png, there can be only 10 images in the sequence, with the last image being overlay_9.png. But if the first image is overlay_00.png, there can be 100 images in the sequence.
--
-- * 'miiInsertionMode' - Choose the type of motion graphic asset that you are providing for your overlay. You can choose either a .mov file or a series of .png files.
--
-- * 'miiPlayback' - Specify whether your motion graphic overlay repeats on a loop or plays only once.
motionImageInserter
    :: MotionImageInserter
motionImageInserter =
  MotionImageInserter'
    { _miiFramerate = Nothing
    , _miiStartTime = Nothing
    , _miiOffset = Nothing
    , _miiInput = Nothing
    , _miiInsertionMode = Nothing
    , _miiPlayback = Nothing
    }


-- | If your motion graphic asset is a .mov file, keep this setting unspecified. If your motion graphic asset is a series of .png files, specify the frame rate of the overlay in frames per second, as a fraction. For example, specify 24 fps as 24/1. Make sure that the number of images in your series matches the frame rate and your intended overlay duration. For example, if you want a 30-second overlay at 30 fps, you should have 900 .png images. This overlay frame rate doesn't need to match the frame rate of the underlying video.
miiFramerate :: Lens' MotionImageInserter (Maybe MotionImageInsertionFramerate)
miiFramerate = lens _miiFramerate (\ s a -> s{_miiFramerate = a})

-- | Specify when the motion overlay begins. Use timecode format (HH:MM:SS:FF or HH:MM:SS;FF). Make sure that the timecode you provide here takes into account how you have set up your timecode configuration under both job settings and input settings. The simplest way to do that is to set both to start at 0. If you need to set up your job to follow timecodes embedded in your source that don't start at zero, make sure that you specify a start time that is after the first embedded timecode. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/setting-up-timecode.html Find job-wide and input timecode configuration settings in your JSON job settings specification at settings>timecodeConfig>source and settings>inputs>timecodeSource.
miiStartTime :: Lens' MotionImageInserter (Maybe Text)
miiStartTime = lens _miiStartTime (\ s a -> s{_miiStartTime = a})

-- | Use Offset to specify the placement of your motion graphic overlay on the video frame. Specify in pixels, from the upper-left corner of the frame. If you don't specify an offset, the service scales your overlay to the full size of the frame. Otherwise, the service inserts the overlay at its native resolution and scales the size up or down with any video scaling.
miiOffset :: Lens' MotionImageInserter (Maybe MotionImageInsertionOffset)
miiOffset = lens _miiOffset (\ s a -> s{_miiOffset = a})

-- | Specify the .mov file or series of .png files that you want to overlay on your video. For .png files, provide the file name of the first file in the series. Make sure that the names of the .png files end with sequential numbers that specify the order that they are played in. For example, overlay_000.png, overlay_001.png, overlay_002.png, and so on. The sequence must start at zero, and each image file name must have the same number of digits. Pad your initial file names with enough zeros to complete the sequence. For example, if the first image is overlay_0.png, there can be only 10 images in the sequence, with the last image being overlay_9.png. But if the first image is overlay_00.png, there can be 100 images in the sequence.
miiInput :: Lens' MotionImageInserter (Maybe Text)
miiInput = lens _miiInput (\ s a -> s{_miiInput = a})

-- | Choose the type of motion graphic asset that you are providing for your overlay. You can choose either a .mov file or a series of .png files.
miiInsertionMode :: Lens' MotionImageInserter (Maybe MotionImageInsertionMode)
miiInsertionMode = lens _miiInsertionMode (\ s a -> s{_miiInsertionMode = a})

-- | Specify whether your motion graphic overlay repeats on a loop or plays only once.
miiPlayback :: Lens' MotionImageInserter (Maybe MotionImagePlayback)
miiPlayback = lens _miiPlayback (\ s a -> s{_miiPlayback = a})

instance FromJSON MotionImageInserter where
        parseJSON
          = withObject "MotionImageInserter"
              (\ x ->
                 MotionImageInserter' <$>
                   (x .:? "framerate") <*> (x .:? "startTime") <*>
                     (x .:? "offset")
                     <*> (x .:? "input")
                     <*> (x .:? "insertionMode")
                     <*> (x .:? "playback"))

instance Hashable MotionImageInserter where

instance NFData MotionImageInserter where

instance ToJSON MotionImageInserter where
        toJSON MotionImageInserter'{..}
          = object
              (catMaybes
                 [("framerate" .=) <$> _miiFramerate,
                  ("startTime" .=) <$> _miiStartTime,
                  ("offset" .=) <$> _miiOffset,
                  ("input" .=) <$> _miiInput,
                  ("insertionMode" .=) <$> _miiInsertionMode,
                  ("playback" .=) <$> _miiPlayback])

-- | For motion overlays that don't have a built-in frame rate, specify the frame rate of the overlay in frames per second, as a fraction. For example, specify 24 fps as 24/1. The overlay frame rate doesn't need to match the frame rate of the underlying video.
--
-- /See:/ 'motionImageInsertionFramerate' smart constructor.
data MotionImageInsertionFramerate = MotionImageInsertionFramerate'
  { _miifFramerateDenominator :: !(Maybe Nat)
  , _miifFramerateNumerator   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MotionImageInsertionFramerate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miifFramerateDenominator' - The bottom of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 1.
--
-- * 'miifFramerateNumerator' - The top of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 24.
motionImageInsertionFramerate
    :: MotionImageInsertionFramerate
motionImageInsertionFramerate =
  MotionImageInsertionFramerate'
    {_miifFramerateDenominator = Nothing, _miifFramerateNumerator = Nothing}


-- | The bottom of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 1.
miifFramerateDenominator :: Lens' MotionImageInsertionFramerate (Maybe Natural)
miifFramerateDenominator = lens _miifFramerateDenominator (\ s a -> s{_miifFramerateDenominator = a}) . mapping _Nat

-- | The top of the fraction that expresses your overlay frame rate. For example, if your frame rate is 24 fps, set this value to 24.
miifFramerateNumerator :: Lens' MotionImageInsertionFramerate (Maybe Natural)
miifFramerateNumerator = lens _miifFramerateNumerator (\ s a -> s{_miifFramerateNumerator = a}) . mapping _Nat

instance FromJSON MotionImageInsertionFramerate where
        parseJSON
          = withObject "MotionImageInsertionFramerate"
              (\ x ->
                 MotionImageInsertionFramerate' <$>
                   (x .:? "framerateDenominator") <*>
                     (x .:? "framerateNumerator"))

instance Hashable MotionImageInsertionFramerate where

instance NFData MotionImageInsertionFramerate where

instance ToJSON MotionImageInsertionFramerate where
        toJSON MotionImageInsertionFramerate'{..}
          = object
              (catMaybes
                 [("framerateDenominator" .=) <$>
                    _miifFramerateDenominator,
                  ("framerateNumerator" .=) <$>
                    _miifFramerateNumerator])

-- | Specify the offset between the upper-left corner of the video frame and the top left corner of the overlay.
--
-- /See:/ 'motionImageInsertionOffset' smart constructor.
data MotionImageInsertionOffset = MotionImageInsertionOffset'
  { _miioImageX :: !(Maybe Nat)
  , _miioImageY :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MotionImageInsertionOffset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miioImageX' - Set the distance, in pixels, between the overlay and the left edge of the video frame.
--
-- * 'miioImageY' - Set the distance, in pixels, between the overlay and the top edge of the video frame.
motionImageInsertionOffset
    :: MotionImageInsertionOffset
motionImageInsertionOffset =
  MotionImageInsertionOffset' {_miioImageX = Nothing, _miioImageY = Nothing}


-- | Set the distance, in pixels, between the overlay and the left edge of the video frame.
miioImageX :: Lens' MotionImageInsertionOffset (Maybe Natural)
miioImageX = lens _miioImageX (\ s a -> s{_miioImageX = a}) . mapping _Nat

-- | Set the distance, in pixels, between the overlay and the top edge of the video frame.
miioImageY :: Lens' MotionImageInsertionOffset (Maybe Natural)
miioImageY = lens _miioImageY (\ s a -> s{_miioImageY = a}) . mapping _Nat

instance FromJSON MotionImageInsertionOffset where
        parseJSON
          = withObject "MotionImageInsertionOffset"
              (\ x ->
                 MotionImageInsertionOffset' <$>
                   (x .:? "imageX") <*> (x .:? "imageY"))

instance Hashable MotionImageInsertionOffset where

instance NFData MotionImageInsertionOffset where

instance ToJSON MotionImageInsertionOffset where
        toJSON MotionImageInsertionOffset'{..}
          = object
              (catMaybes
                 [("imageX" .=) <$> _miioImageX,
                  ("imageY" .=) <$> _miioImageY])

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
  { _mChannels   :: !(Maybe Nat)
  , _mSampleRate :: !(Maybe Nat)
  , _mBitrate    :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Mp2Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mChannels' - Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
--
-- * 'mSampleRate' - Sample rate in hz.
--
-- * 'mBitrate' - Average bitrate in bits/second.
mp2Settings
    :: Mp2Settings
mp2Settings =
  Mp2Settings'
    {_mChannels = Nothing, _mSampleRate = Nothing, _mBitrate = Nothing}


-- | Set Channels to specify the number of channels in this output audio track. Choosing Mono in the console will give you 1 output channel; choosing Stereo will give you 2. In the API, valid values are 1 and 2.
mChannels :: Lens' Mp2Settings (Maybe Natural)
mChannels = lens _mChannels (\ s a -> s{_mChannels = a}) . mapping _Nat

-- | Sample rate in hz.
mSampleRate :: Lens' Mp2Settings (Maybe Natural)
mSampleRate = lens _mSampleRate (\ s a -> s{_mSampleRate = a}) . mapping _Nat

-- | Average bitrate in bits/second.
mBitrate :: Lens' Mp2Settings (Maybe Natural)
mBitrate = lens _mBitrate (\ s a -> s{_mBitrate = a}) . mapping _Nat

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
                 [("channels" .=) <$> _mChannels,
                  ("sampleRate" .=) <$> _mSampleRate,
                  ("bitrate" .=) <$> _mBitrate])

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
  , _msHrdBufferInitialFillPercentage :: !(Maybe Nat)
  , _msSlowPal :: !(Maybe Mpeg2SlowPal)
  , _msParNumerator :: !(Maybe Nat)
  , _msGopSize :: !(Maybe Double)
  , _msNumberBFramesBetweenReferenceFrames :: !(Maybe Nat)
  , _msGopSizeUnits :: !(Maybe Mpeg2GopSizeUnits)
  , _msHrdBufferSize :: !(Maybe Nat)
  , _msRateControlMode :: !(Maybe Mpeg2RateControlMode)
  , _msTelecine :: !(Maybe Mpeg2Telecine)
  , _msIntraDcPrecision :: !(Maybe Mpeg2IntraDcPrecision)
  , _msDynamicSubGop :: !(Maybe Mpeg2DynamicSubGop)
  , _msMinIInterval :: !(Maybe Nat)
  , _msInterlaceMode :: !(Maybe Mpeg2InterlaceMode)
  , _msParControl :: !(Maybe Mpeg2ParControl)
  , _msSoftness :: !(Maybe Nat)
  , _msCodecProfile :: !(Maybe Mpeg2CodecProfile)
  , _msBitrate :: !(Maybe Nat)
  , _msFramerateDenominator :: !(Maybe Nat)
  , _msFramerateConversionAlgorithm :: !(Maybe Mpeg2FramerateConversionAlgorithm)
  , _msCodecLevel :: !(Maybe Mpeg2CodecLevel)
  , _msFramerateControl :: !(Maybe Mpeg2FramerateControl)
  , _msAdaptiveQuantization :: !(Maybe Mpeg2AdaptiveQuantization)
  , _msFramerateNumerator :: !(Maybe Nat)
  , _msMaxBitrate :: !(Maybe Nat)
  , _msSyntax :: !(Maybe Mpeg2Syntax)
  , _msGopClosedCadence :: !(Maybe Nat)
  , _msParDenominator :: !(Maybe Nat)
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
-- * 'msHrdBufferSize' - Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
--
-- * 'msRateControlMode' - Undocumented member.
--
-- * 'msTelecine' - Undocumented member.
--
-- * 'msIntraDcPrecision' - Undocumented member.
--
-- * 'msDynamicSubGop' - Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
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
-- * 'msBitrate' - Average bitrate in bits/second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
--
-- * 'msFramerateDenominator' - Frame rate denominator.
--
-- * 'msFramerateConversionAlgorithm' - Undocumented member.
--
-- * 'msCodecLevel' - Undocumented member.
--
-- * 'msFramerateControl' - Undocumented member.
--
-- * 'msAdaptiveQuantization' - Undocumented member.
--
-- * 'msFramerateNumerator' - Frame rate numerator - frame rate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
--
-- * 'msMaxBitrate' - Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000.
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
    , _msDynamicSubGop = Nothing
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
msHrdBufferInitialFillPercentage :: Lens' Mpeg2Settings (Maybe Natural)
msHrdBufferInitialFillPercentage = lens _msHrdBufferInitialFillPercentage (\ s a -> s{_msHrdBufferInitialFillPercentage = a}) . mapping _Nat

-- | Undocumented member.
msSlowPal :: Lens' Mpeg2Settings (Maybe Mpeg2SlowPal)
msSlowPal = lens _msSlowPal (\ s a -> s{_msSlowPal = a})

-- | Pixel Aspect Ratio numerator.
msParNumerator :: Lens' Mpeg2Settings (Maybe Natural)
msParNumerator = lens _msParNumerator (\ s a -> s{_msParNumerator = a}) . mapping _Nat

-- | GOP Length (keyframe interval) in frames or seconds. Must be greater than zero.
msGopSize :: Lens' Mpeg2Settings (Maybe Double)
msGopSize = lens _msGopSize (\ s a -> s{_msGopSize = a})

-- | Number of B-frames between reference frames.
msNumberBFramesBetweenReferenceFrames :: Lens' Mpeg2Settings (Maybe Natural)
msNumberBFramesBetweenReferenceFrames = lens _msNumberBFramesBetweenReferenceFrames (\ s a -> s{_msNumberBFramesBetweenReferenceFrames = a}) . mapping _Nat

-- | Undocumented member.
msGopSizeUnits :: Lens' Mpeg2Settings (Maybe Mpeg2GopSizeUnits)
msGopSizeUnits = lens _msGopSizeUnits (\ s a -> s{_msGopSizeUnits = a})

-- | Size of buffer (HRD buffer model) in bits. For example, enter five megabits as 5000000.
msHrdBufferSize :: Lens' Mpeg2Settings (Maybe Natural)
msHrdBufferSize = lens _msHrdBufferSize (\ s a -> s{_msHrdBufferSize = a}) . mapping _Nat

-- | Undocumented member.
msRateControlMode :: Lens' Mpeg2Settings (Maybe Mpeg2RateControlMode)
msRateControlMode = lens _msRateControlMode (\ s a -> s{_msRateControlMode = a})

-- | Undocumented member.
msTelecine :: Lens' Mpeg2Settings (Maybe Mpeg2Telecine)
msTelecine = lens _msTelecine (\ s a -> s{_msTelecine = a})

-- | Undocumented member.
msIntraDcPrecision :: Lens' Mpeg2Settings (Maybe Mpeg2IntraDcPrecision)
msIntraDcPrecision = lens _msIntraDcPrecision (\ s a -> s{_msIntraDcPrecision = a})

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
msDynamicSubGop :: Lens' Mpeg2Settings (Maybe Mpeg2DynamicSubGop)
msDynamicSubGop = lens _msDynamicSubGop (\ s a -> s{_msDynamicSubGop = a})

-- | Enforces separation between repeated (cadence) I-frames and I-frames inserted by Scene Change Detection. If a scene change I-frame is within I-interval frames of a cadence I-frame, the GOP is shrunk and/or stretched to the scene change I-frame. GOP stretch requires enabling lookahead as well as setting I-interval. The normal cadence resumes for the next GOP. This setting is only used when Scene Change Detect is enabled. Note: Maximum GOP stretch = GOP size + Min-I-interval - 1
msMinIInterval :: Lens' Mpeg2Settings (Maybe Natural)
msMinIInterval = lens _msMinIInterval (\ s a -> s{_msMinIInterval = a}) . mapping _Nat

-- | Undocumented member.
msInterlaceMode :: Lens' Mpeg2Settings (Maybe Mpeg2InterlaceMode)
msInterlaceMode = lens _msInterlaceMode (\ s a -> s{_msInterlaceMode = a})

-- | Undocumented member.
msParControl :: Lens' Mpeg2Settings (Maybe Mpeg2ParControl)
msParControl = lens _msParControl (\ s a -> s{_msParControl = a})

-- | Softness. Selects quantizer matrix, larger values reduce high-frequency content in the encoded image.
msSoftness :: Lens' Mpeg2Settings (Maybe Natural)
msSoftness = lens _msSoftness (\ s a -> s{_msSoftness = a}) . mapping _Nat

-- | Undocumented member.
msCodecProfile :: Lens' Mpeg2Settings (Maybe Mpeg2CodecProfile)
msCodecProfile = lens _msCodecProfile (\ s a -> s{_msCodecProfile = a})

-- | Average bitrate in bits/second. Required for VBR and CBR. For MS Smooth outputs, bitrates must be unique when rounded down to the nearest multiple of 1000.
msBitrate :: Lens' Mpeg2Settings (Maybe Natural)
msBitrate = lens _msBitrate (\ s a -> s{_msBitrate = a}) . mapping _Nat

-- | Frame rate denominator.
msFramerateDenominator :: Lens' Mpeg2Settings (Maybe Natural)
msFramerateDenominator = lens _msFramerateDenominator (\ s a -> s{_msFramerateDenominator = a}) . mapping _Nat

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

-- | Frame rate numerator - frame rate is a fraction, e.g. 24000 / 1001 = 23.976 fps.
msFramerateNumerator :: Lens' Mpeg2Settings (Maybe Natural)
msFramerateNumerator = lens _msFramerateNumerator (\ s a -> s{_msFramerateNumerator = a}) . mapping _Nat

-- | Maximum bitrate in bits/second. For example, enter five megabits per second as 5000000.
msMaxBitrate :: Lens' Mpeg2Settings (Maybe Natural)
msMaxBitrate = lens _msMaxBitrate (\ s a -> s{_msMaxBitrate = a}) . mapping _Nat

-- | Undocumented member.
msSyntax :: Lens' Mpeg2Settings (Maybe Mpeg2Syntax)
msSyntax = lens _msSyntax (\ s a -> s{_msSyntax = a})

-- | Frequency of closed GOPs. In streaming applications, it is recommended that this be set to 1 so a decoder joining mid-stream will receive an IDR frame as quickly as possible. Setting this value to 0 will break output segmenting.
msGopClosedCadence :: Lens' Mpeg2Settings (Maybe Natural)
msGopClosedCadence = lens _msGopClosedCadence (\ s a -> s{_msGopClosedCadence = a}) . mapping _Nat

-- | Pixel Aspect Ratio denominator.
msParDenominator :: Lens' Mpeg2Settings (Maybe Natural)
msParDenominator = lens _msParDenominator (\ s a -> s{_msParDenominator = a}) . mapping _Nat

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
                     <*> (x .:? "dynamicSubGop")
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
                  ("dynamicSubGop" .=) <$> _msDynamicSubGop,
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
  { _msgsFragmentLength     :: !(Maybe Nat)
  , _msgsManifestEncoding   :: !(Maybe MsSmoothManifestEncoding)
  , _msgsDestination        :: !(Maybe Text)
  , _msgsAudioDeduplication :: !(Maybe MsSmoothAudioDeduplication)
  , _msgsEncryption         :: !(Maybe MsSmoothEncryptionSettings)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MsSmoothGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msgsFragmentLength' - Use Fragment length (FragmentLength) to specify the mp4 fragment sizes in seconds. Fragment length must be compatible with GOP size and frame rate.
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


-- | Use Fragment length (FragmentLength) to specify the mp4 fragment sizes in seconds. Fragment length must be compatible with GOP size and frame rate.
msgsFragmentLength :: Lens' MsSmoothGroupSettings (Maybe Natural)
msgsFragmentLength = lens _msgsFragmentLength (\ s a -> s{_msgsFragmentLength = a}) . mapping _Nat

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
  { _ncBreakoutCode  :: !(Maybe Nat)
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
ncBreakoutCode :: Lens' NielsenConfiguration (Maybe Natural)
ncBreakoutCode = lens _ncBreakoutCode (\ s a -> s{_ncBreakoutCode = a}) . mapping _Nat

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
  { _nrfsStrength :: Maybe Nat
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
nrfsStrength :: Lens' NoiseReducerFilterSettings (Maybe Natural)
nrfsStrength = lens _nrfsStrength (\ s a -> s{_nrfsStrength = a}) . mapping _Nat

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
  { _nrsfsStrength                  :: !(Maybe Nat)
  , _nrsfsPostFilterSharpenStrength :: !(Maybe Nat)
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
nrsfsStrength :: Lens' NoiseReducerSpatialFilterSettings (Maybe Natural)
nrsfsStrength = lens _nrsfsStrength (\ s a -> s{_nrsfsStrength = a}) . mapping _Nat

-- | Specify strength of post noise reduction sharpening filter, with 0 disabling the filter and 3 enabling it at maximum strength.
nrsfsPostFilterSharpenStrength :: Lens' NoiseReducerSpatialFilterSettings (Maybe Natural)
nrsfsPostFilterSharpenStrength = lens _nrsfsPostFilterSharpenStrength (\ s a -> s{_nrsfsPostFilterSharpenStrength = a}) . mapping _Nat

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
  , _ogsCmafGroupSettings     :: !(Maybe CmafGroupSettings)
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
-- * 'ogsCmafGroupSettings' - Undocumented member.
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
    , _ogsCmafGroupSettings = Nothing
    , _ogsMsSmoothGroupSettings = Nothing
    , _ogsHlsGroupSettings = Nothing
    , _ogsType = Nothing
    , _ogsDashIsoGroupSettings = Nothing
    }


-- | Undocumented member.
ogsFileGroupSettings :: Lens' OutputGroupSettings (Maybe FileGroupSettings)
ogsFileGroupSettings = lens _ogsFileGroupSettings (\ s a -> s{_ogsFileGroupSettings = a})

-- | Undocumented member.
ogsCmafGroupSettings :: Lens' OutputGroupSettings (Maybe CmafGroupSettings)
ogsCmafGroupSettings = lens _ogsCmafGroupSettings (\ s a -> s{_ogsCmafGroupSettings = a})

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
                     (x .:? "cmafGroupSettings")
                     <*> (x .:? "msSmoothGroupSettings")
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
                  ("cmafGroupSettings" .=) <$> _ogsCmafGroupSettings,
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
  , _pARN         :: !(Maybe Text)
  , _pCreatedAt   :: !(Maybe POSIX)
  , _pCategory    :: !(Maybe Text)
  , _pType        :: !(Maybe Type)
  , _pDescription :: !(Maybe Text)
  , _pSettings    :: !PresetSettings
  , _pName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Preset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pLastUpdated' - The timestamp in epoch seconds when the preset was last updated.
--
-- * 'pARN' - An identifier for this resource that is unique within all of AWS.
--
-- * 'pCreatedAt' - The timestamp in epoch seconds for preset creation.
--
-- * 'pCategory' - An optional category you create to organize your presets.
--
-- * 'pType' - A preset can be of two types: system or custom. System or built-in preset can't be modified or deleted by the user.
--
-- * 'pDescription' - An optional description you create for each preset.
--
-- * 'pSettings' - Undocumented member.
--
-- * 'pName' - A name you create for each preset. Each name must be unique within your account.
preset
    :: PresetSettings -- ^ 'pSettings'
    -> Text -- ^ 'pName'
    -> Preset
preset pSettings_ pName_ =
  Preset'
    { _pLastUpdated = Nothing
    , _pARN = Nothing
    , _pCreatedAt = Nothing
    , _pCategory = Nothing
    , _pType = Nothing
    , _pDescription = Nothing
    , _pSettings = pSettings_
    , _pName = pName_
    }


-- | The timestamp in epoch seconds when the preset was last updated.
pLastUpdated :: Lens' Preset (Maybe UTCTime)
pLastUpdated = lens _pLastUpdated (\ s a -> s{_pLastUpdated = a}) . mapping _Time

-- | An identifier for this resource that is unique within all of AWS.
pARN :: Lens' Preset (Maybe Text)
pARN = lens _pARN (\ s a -> s{_pARN = a})

-- | The timestamp in epoch seconds for preset creation.
pCreatedAt :: Lens' Preset (Maybe UTCTime)
pCreatedAt = lens _pCreatedAt (\ s a -> s{_pCreatedAt = a}) . mapping _Time

-- | An optional category you create to organize your presets.
pCategory :: Lens' Preset (Maybe Text)
pCategory = lens _pCategory (\ s a -> s{_pCategory = a})

-- | A preset can be of two types: system or custom. System or built-in preset can't be modified or deleted by the user.
pType :: Lens' Preset (Maybe Type)
pType = lens _pType (\ s a -> s{_pType = a})

-- | An optional description you create for each preset.
pDescription :: Lens' Preset (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a})

-- | Undocumented member.
pSettings :: Lens' Preset PresetSettings
pSettings = lens _pSettings (\ s a -> s{_pSettings = a})

-- | A name you create for each preset. Each name must be unique within your account.
pName :: Lens' Preset Text
pName = lens _pName (\ s a -> s{_pName = a})

instance FromJSON Preset where
        parseJSON
          = withObject "Preset"
              (\ x ->
                 Preset' <$>
                   (x .:? "lastUpdated") <*> (x .:? "arn") <*>
                     (x .:? "createdAt")
                     <*> (x .:? "category")
                     <*> (x .:? "type")
                     <*> (x .:? "description")
                     <*> (x .: "settings")
                     <*> (x .: "name"))

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
  , _psParNumerator :: !(Maybe Nat)
  , _psTelecine :: !(Maybe ProresTelecine)
  , _psInterlaceMode :: !(Maybe ProresInterlaceMode)
  , _psParControl :: !(Maybe ProresParControl)
  , _psCodecProfile :: !(Maybe ProresCodecProfile)
  , _psFramerateDenominator :: !(Maybe Nat)
  , _psFramerateConversionAlgorithm :: !(Maybe ProresFramerateConversionAlgorithm)
  , _psFramerateControl :: !(Maybe ProresFramerateControl)
  , _psFramerateNumerator :: !(Maybe Nat)
  , _psParDenominator :: !(Maybe Nat)
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
-- * 'psFramerateDenominator' - Frame rate denominator.
--
-- * 'psFramerateConversionAlgorithm' - Undocumented member.
--
-- * 'psFramerateControl' - Undocumented member.
--
-- * 'psFramerateNumerator' - When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator.
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
psParNumerator :: Lens' ProresSettings (Maybe Natural)
psParNumerator = lens _psParNumerator (\ s a -> s{_psParNumerator = a}) . mapping _Nat

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

-- | Frame rate denominator.
psFramerateDenominator :: Lens' ProresSettings (Maybe Natural)
psFramerateDenominator = lens _psFramerateDenominator (\ s a -> s{_psFramerateDenominator = a}) . mapping _Nat

-- | Undocumented member.
psFramerateConversionAlgorithm :: Lens' ProresSettings (Maybe ProresFramerateConversionAlgorithm)
psFramerateConversionAlgorithm = lens _psFramerateConversionAlgorithm (\ s a -> s{_psFramerateConversionAlgorithm = a})

-- | Undocumented member.
psFramerateControl :: Lens' ProresSettings (Maybe ProresFramerateControl)
psFramerateControl = lens _psFramerateControl (\ s a -> s{_psFramerateControl = a})

-- | When you use the API for transcode jobs that use frame rate conversion, specify the frame rate as a fraction. For example,  24000 / 1001 = 23.976 fps. Use FramerateNumerator to specify the numerator of this fraction. In this example, use 24000 for the value of FramerateNumerator.
psFramerateNumerator :: Lens' ProresSettings (Maybe Natural)
psFramerateNumerator = lens _psFramerateNumerator (\ s a -> s{_psFramerateNumerator = a}) . mapping _Nat

-- | Pixel Aspect Ratio denominator.
psParDenominator :: Lens' ProresSettings (Maybe Natural)
psParDenominator = lens _psParDenominator (\ s a -> s{_psParDenominator = a}) . mapping _Nat

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

-- | You can use queues to manage the resources that are available to your AWS account for running multiple transcoding jobs at the same time. If you don't specify a queue, the service sends all jobs through the default queue. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/working-with-queues.html.
--
-- /See:/ 'queue' smart constructor.
data Queue = Queue'
  { _qStatus               :: !(Maybe QueueStatus)
  , _qLastUpdated          :: !(Maybe POSIX)
  , _qARN                  :: !(Maybe Text)
  , _qCreatedAt            :: !(Maybe POSIX)
  , _qReservationPlan      :: !(Maybe ReservationPlan)
  , _qPricingPlan          :: !(Maybe PricingPlan)
  , _qSubmittedJobsCount   :: !(Maybe Int)
  , _qProgressingJobsCount :: !(Maybe Int)
  , _qType                 :: !(Maybe Type)
  , _qDescription          :: !(Maybe Text)
  , _qName                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Queue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qStatus' - Queues can be ACTIVE or PAUSED. If you pause a queue, the service won't begin processing jobs in that queue. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
--
-- * 'qLastUpdated' - The timestamp in epoch seconds for when you most recently updated the queue.
--
-- * 'qARN' - An identifier for this resource that is unique within all of AWS.
--
-- * 'qCreatedAt' - The timestamp in epoch seconds for when you created the queue.
--
-- * 'qReservationPlan' - Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- * 'qPricingPlan' - Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment.
--
-- * 'qSubmittedJobsCount' - The estimated number of jobs with a SUBMITTED status.
--
-- * 'qProgressingJobsCount' - The estimated number of jobs with a PROGRESSING status.
--
-- * 'qType' - Specifies whether this on-demand queue is system or custom. System queues are built in. You can't modify or delete system queues. You can create and modify custom queues.
--
-- * 'qDescription' - An optional description that you create for each queue.
--
-- * 'qName' - A name that you create for each queue. Each name must be unique within your account.
queue
    :: Text -- ^ 'qName'
    -> Queue
queue pName_ =
  Queue'
    { _qStatus = Nothing
    , _qLastUpdated = Nothing
    , _qARN = Nothing
    , _qCreatedAt = Nothing
    , _qReservationPlan = Nothing
    , _qPricingPlan = Nothing
    , _qSubmittedJobsCount = Nothing
    , _qProgressingJobsCount = Nothing
    , _qType = Nothing
    , _qDescription = Nothing
    , _qName = pName_
    }


-- | Queues can be ACTIVE or PAUSED. If you pause a queue, the service won't begin processing jobs in that queue. Jobs that are running when you pause the queue continue to run until they finish or result in an error.
qStatus :: Lens' Queue (Maybe QueueStatus)
qStatus = lens _qStatus (\ s a -> s{_qStatus = a})

-- | The timestamp in epoch seconds for when you most recently updated the queue.
qLastUpdated :: Lens' Queue (Maybe UTCTime)
qLastUpdated = lens _qLastUpdated (\ s a -> s{_qLastUpdated = a}) . mapping _Time

-- | An identifier for this resource that is unique within all of AWS.
qARN :: Lens' Queue (Maybe Text)
qARN = lens _qARN (\ s a -> s{_qARN = a})

-- | The timestamp in epoch seconds for when you created the queue.
qCreatedAt :: Lens' Queue (Maybe UTCTime)
qCreatedAt = lens _qCreatedAt (\ s a -> s{_qCreatedAt = a}) . mapping _Time

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
qReservationPlan :: Lens' Queue (Maybe ReservationPlan)
qReservationPlan = lens _qReservationPlan (\ s a -> s{_qReservationPlan = a})

-- | Specifies whether the pricing plan for the queue is on-demand or reserved. For on-demand, you pay per minute, billed in increments of .01 minute. For reserved, you pay for the transcoding capacity of the entire queue, regardless of how much or how little you use it. Reserved pricing requires a 12-month commitment.
qPricingPlan :: Lens' Queue (Maybe PricingPlan)
qPricingPlan = lens _qPricingPlan (\ s a -> s{_qPricingPlan = a})

-- | The estimated number of jobs with a SUBMITTED status.
qSubmittedJobsCount :: Lens' Queue (Maybe Int)
qSubmittedJobsCount = lens _qSubmittedJobsCount (\ s a -> s{_qSubmittedJobsCount = a})

-- | The estimated number of jobs with a PROGRESSING status.
qProgressingJobsCount :: Lens' Queue (Maybe Int)
qProgressingJobsCount = lens _qProgressingJobsCount (\ s a -> s{_qProgressingJobsCount = a})

-- | Specifies whether this on-demand queue is system or custom. System queues are built in. You can't modify or delete system queues. You can create and modify custom queues.
qType :: Lens' Queue (Maybe Type)
qType = lens _qType (\ s a -> s{_qType = a})

-- | An optional description that you create for each queue.
qDescription :: Lens' Queue (Maybe Text)
qDescription = lens _qDescription (\ s a -> s{_qDescription = a})

-- | A name that you create for each queue. Each name must be unique within your account.
qName :: Lens' Queue Text
qName = lens _qName (\ s a -> s{_qName = a})

instance FromJSON Queue where
        parseJSON
          = withObject "Queue"
              (\ x ->
                 Queue' <$>
                   (x .:? "status") <*> (x .:? "lastUpdated") <*>
                     (x .:? "arn")
                     <*> (x .:? "createdAt")
                     <*> (x .:? "reservationPlan")
                     <*> (x .:? "pricingPlan")
                     <*> (x .:? "submittedJobsCount")
                     <*> (x .:? "progressingJobsCount")
                     <*> (x .:? "type")
                     <*> (x .:? "description")
                     <*> (x .: "name"))

instance Hashable Queue where

instance NFData Queue where

-- | Use Rectangle to identify a specific area of the video frame.
--
-- /See:/ 'rectangle' smart constructor.
data Rectangle = Rectangle'
  { _rHeight :: !(Maybe Nat)
  , _rWidth  :: !(Maybe Nat)
  , _rX      :: !(Maybe Nat)
  , _rY      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Rectangle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rHeight' - Height of rectangle in pixels. Specify only even numbers.
--
-- * 'rWidth' - Width of rectangle in pixels. Specify only even numbers.
--
-- * 'rX' - The distance, in pixels, between the rectangle and the left edge of the video frame. Specify only even numbers.
--
-- * 'rY' - The distance, in pixels, between the rectangle and the top edge of the video frame. Specify only even numbers.
rectangle
    :: Rectangle
rectangle =
  Rectangle'
    {_rHeight = Nothing, _rWidth = Nothing, _rX = Nothing, _rY = Nothing}


-- | Height of rectangle in pixels. Specify only even numbers.
rHeight :: Lens' Rectangle (Maybe Natural)
rHeight = lens _rHeight (\ s a -> s{_rHeight = a}) . mapping _Nat

-- | Width of rectangle in pixels. Specify only even numbers.
rWidth :: Lens' Rectangle (Maybe Natural)
rWidth = lens _rWidth (\ s a -> s{_rWidth = a}) . mapping _Nat

-- | The distance, in pixels, between the rectangle and the left edge of the video frame. Specify only even numbers.
rX :: Lens' Rectangle (Maybe Natural)
rX = lens _rX (\ s a -> s{_rX = a}) . mapping _Nat

-- | The distance, in pixels, between the rectangle and the top edge of the video frame. Specify only even numbers.
rY :: Lens' Rectangle (Maybe Natural)
rY = lens _rY (\ s a -> s{_rY = a}) . mapping _Nat

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

-- | Use Manual audio remixing (RemixSettings) to adjust audio levels for each audio channel in each output of your job. With audio remixing, you can output more or fewer audio channels than your input audio source provides.
--
-- /See:/ 'remixSettings' smart constructor.
data RemixSettings = RemixSettings'
  { _rsChannelMapping :: !(Maybe ChannelMapping)
  , _rsChannelsIn     :: !(Maybe Nat)
  , _rsChannelsOut    :: !(Maybe Nat)
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
rsChannelsIn :: Lens' RemixSettings (Maybe Natural)
rsChannelsIn = lens _rsChannelsIn (\ s a -> s{_rsChannelsIn = a}) . mapping _Nat

-- | Specify the number of channels in this output after remixing. Valid values: 1, 2, 4, 6, 8
rsChannelsOut :: Lens' RemixSettings (Maybe Natural)
rsChannelsOut = lens _rsChannelsOut (\ s a -> s{_rsChannelsOut = a}) . mapping _Nat

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

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- /See:/ 'reservationPlan' smart constructor.
data ReservationPlan = ReservationPlan'
  { _rpStatus        :: !(Maybe ReservationPlanStatus)
  , _rpExpiresAt     :: !(Maybe POSIX)
  , _rpPurchasedAt   :: !(Maybe POSIX)
  , _rpCommitment    :: !(Maybe Commitment)
  , _rpReservedSlots :: !(Maybe Int)
  , _rpRenewalType   :: !(Maybe RenewalType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservationPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpStatus' - Specifies whether the pricing plan for your reserved queue is ACTIVE or EXPIRED.
--
-- * 'rpExpiresAt' - The timestamp in epoch seconds for when the current pricing plan term for this reserved queue expires.
--
-- * 'rpPurchasedAt' - The timestamp in epoch seconds for when you set up the current pricing plan for this reserved queue.
--
-- * 'rpCommitment' - The length of the term of your reserved queue pricing plan commitment.
--
-- * 'rpReservedSlots' - Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. When you increase this number, you extend your existing commitment with a new 12-month commitment for a larger number of RTS. The new commitment begins when you purchase the additional capacity. You can't decrease the number of RTS in your reserved queue.
--
-- * 'rpRenewalType' - Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term.
reservationPlan
    :: ReservationPlan
reservationPlan =
  ReservationPlan'
    { _rpStatus = Nothing
    , _rpExpiresAt = Nothing
    , _rpPurchasedAt = Nothing
    , _rpCommitment = Nothing
    , _rpReservedSlots = Nothing
    , _rpRenewalType = Nothing
    }


-- | Specifies whether the pricing plan for your reserved queue is ACTIVE or EXPIRED.
rpStatus :: Lens' ReservationPlan (Maybe ReservationPlanStatus)
rpStatus = lens _rpStatus (\ s a -> s{_rpStatus = a})

-- | The timestamp in epoch seconds for when the current pricing plan term for this reserved queue expires.
rpExpiresAt :: Lens' ReservationPlan (Maybe UTCTime)
rpExpiresAt = lens _rpExpiresAt (\ s a -> s{_rpExpiresAt = a}) . mapping _Time

-- | The timestamp in epoch seconds for when you set up the current pricing plan for this reserved queue.
rpPurchasedAt :: Lens' ReservationPlan (Maybe UTCTime)
rpPurchasedAt = lens _rpPurchasedAt (\ s a -> s{_rpPurchasedAt = a}) . mapping _Time

-- | The length of the term of your reserved queue pricing plan commitment.
rpCommitment :: Lens' ReservationPlan (Maybe Commitment)
rpCommitment = lens _rpCommitment (\ s a -> s{_rpCommitment = a})

-- | Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. When you increase this number, you extend your existing commitment with a new 12-month commitment for a larger number of RTS. The new commitment begins when you purchase the additional capacity. You can't decrease the number of RTS in your reserved queue.
rpReservedSlots :: Lens' ReservationPlan (Maybe Int)
rpReservedSlots = lens _rpReservedSlots (\ s a -> s{_rpReservedSlots = a})

-- | Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term.
rpRenewalType :: Lens' ReservationPlan (Maybe RenewalType)
rpRenewalType = lens _rpRenewalType (\ s a -> s{_rpRenewalType = a})

instance FromJSON ReservationPlan where
        parseJSON
          = withObject "ReservationPlan"
              (\ x ->
                 ReservationPlan' <$>
                   (x .:? "status") <*> (x .:? "expiresAt") <*>
                     (x .:? "purchasedAt")
                     <*> (x .:? "commitment")
                     <*> (x .:? "reservedSlots")
                     <*> (x .:? "renewalType"))

instance Hashable ReservationPlan where

instance NFData ReservationPlan where

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- /See:/ 'reservationPlanSettings' smart constructor.
data ReservationPlanSettings = ReservationPlanSettings'
  { _rpsCommitment    :: !Commitment
  , _rpsReservedSlots :: !Int
  , _rpsRenewalType   :: !RenewalType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservationPlanSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpsCommitment' - The length of the term of your reserved queue pricing plan commitment.
--
-- * 'rpsReservedSlots' - Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. You can't decrease the number of RTS in your reserved queue. You can increase the number of RTS by extending your existing commitment with a new 12-month commitment for the larger number. The new commitment begins when you purchase the additional capacity. You can't cancel your commitment or revert to your original commitment after you increase the capacity.
--
-- * 'rpsRenewalType' - Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term. When your term is auto renewed, you extend your commitment by 12 months from the auto renew date. You can cancel this commitment.
reservationPlanSettings
    :: Commitment -- ^ 'rpsCommitment'
    -> Int -- ^ 'rpsReservedSlots'
    -> RenewalType -- ^ 'rpsRenewalType'
    -> ReservationPlanSettings
reservationPlanSettings pCommitment_ pReservedSlots_ pRenewalType_ =
  ReservationPlanSettings'
    { _rpsCommitment = pCommitment_
    , _rpsReservedSlots = pReservedSlots_
    , _rpsRenewalType = pRenewalType_
    }


-- | The length of the term of your reserved queue pricing plan commitment.
rpsCommitment :: Lens' ReservationPlanSettings Commitment
rpsCommitment = lens _rpsCommitment (\ s a -> s{_rpsCommitment = a})

-- | Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. You can't decrease the number of RTS in your reserved queue. You can increase the number of RTS by extending your existing commitment with a new 12-month commitment for the larger number. The new commitment begins when you purchase the additional capacity. You can't cancel your commitment or revert to your original commitment after you increase the capacity.
rpsReservedSlots :: Lens' ReservationPlanSettings Int
rpsReservedSlots = lens _rpsReservedSlots (\ s a -> s{_rpsReservedSlots = a})

-- | Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term. When your term is auto renewed, you extend your commitment by 12 months from the auto renew date. You can cancel this commitment.
rpsRenewalType :: Lens' ReservationPlanSettings RenewalType
rpsRenewalType = lens _rpsRenewalType (\ s a -> s{_rpsRenewalType = a})

instance Hashable ReservationPlanSettings where

instance NFData ReservationPlanSettings where

instance ToJSON ReservationPlanSettings where
        toJSON ReservationPlanSettings'{..}
          = object
              (catMaybes
                 [Just ("commitment" .= _rpsCommitment),
                  Just ("reservedSlots" .= _rpsReservedSlots),
                  Just ("renewalType" .= _rpsRenewalType)])

-- | The Amazon Resource Name (ARN) and tags for an AWS Elemental MediaConvert resource.
--
-- /See:/ 'resourceTags' smart constructor.
data ResourceTags = ResourceTags'
  { _rtARN  :: !(Maybe Text)
  , _rtTags :: !(Maybe (Map Text Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtARN' - The Amazon Resource Name (ARN) of the resource.
--
-- * 'rtTags' - The tags for the resource.
resourceTags
    :: ResourceTags
resourceTags = ResourceTags' {_rtARN = Nothing, _rtTags = Nothing}


-- | The Amazon Resource Name (ARN) of the resource.
rtARN :: Lens' ResourceTags (Maybe Text)
rtARN = lens _rtARN (\ s a -> s{_rtARN = a})

-- | The tags for the resource.
rtTags :: Lens' ResourceTags (HashMap Text Text)
rtTags = lens _rtTags (\ s a -> s{_rtTags = a}) . _Default . _Map

instance FromJSON ResourceTags where
        parseJSON
          = withObject "ResourceTags"
              (\ x ->
                 ResourceTags' <$>
                   (x .:? "arn") <*> (x .:? "tags" .!= mempty))

instance Hashable ResourceTags where

instance NFData ResourceTags where

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
  { _sResourceId     :: !(Maybe Text)
  , _sCertificateARN :: !(Maybe Text)
  , _sURL            :: !(Maybe Text)
  , _sSystemIds      :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SpekeKeyProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sResourceId' - The SPEKE-compliant server uses Resource ID (ResourceId) to identify content.
--
-- * 'sCertificateARN' - Optional AWS Certificate Manager ARN for a certificate to send to the keyprovider. The certificate holds a key used by the keyprovider to encrypt the keys in its response.
--
-- * 'sURL' - Use URL (Url) to specify the SPEKE-compliant server that will provide keys for content.
--
-- * 'sSystemIds' - Relates to SPEKE implementation. DRM system identifiers. DASH output groups support a max of two system ids. Other group types support one system id.
spekeKeyProvider
    :: SpekeKeyProvider
spekeKeyProvider =
  SpekeKeyProvider'
    { _sResourceId = Nothing
    , _sCertificateARN = Nothing
    , _sURL = Nothing
    , _sSystemIds = Nothing
    }


-- | The SPEKE-compliant server uses Resource ID (ResourceId) to identify content.
sResourceId :: Lens' SpekeKeyProvider (Maybe Text)
sResourceId = lens _sResourceId (\ s a -> s{_sResourceId = a})

-- | Optional AWS Certificate Manager ARN for a certificate to send to the keyprovider. The certificate holds a key used by the keyprovider to encrypt the keys in its response.
sCertificateARN :: Lens' SpekeKeyProvider (Maybe Text)
sCertificateARN = lens _sCertificateARN (\ s a -> s{_sCertificateARN = a})

-- | Use URL (Url) to specify the SPEKE-compliant server that will provide keys for content.
sURL :: Lens' SpekeKeyProvider (Maybe Text)
sURL = lens _sURL (\ s a -> s{_sURL = a})

-- | Relates to SPEKE implementation. DRM system identifiers. DASH output groups support a max of two system ids. Other group types support one system id.
sSystemIds :: Lens' SpekeKeyProvider [Text]
sSystemIds = lens _sSystemIds (\ s a -> s{_sSystemIds = a}) . _Default . _Coerce

instance FromJSON SpekeKeyProvider where
        parseJSON
          = withObject "SpekeKeyProvider"
              (\ x ->
                 SpekeKeyProvider' <$>
                   (x .:? "resourceId") <*> (x .:? "certificateArn") <*>
                     (x .:? "url")
                     <*> (x .:? "systemIds" .!= mempty))

instance Hashable SpekeKeyProvider where

instance NFData SpekeKeyProvider where

instance ToJSON SpekeKeyProvider where
        toJSON SpekeKeyProvider'{..}
          = object
              (catMaybes
                 [("resourceId" .=) <$> _sResourceId,
                  ("certificateArn" .=) <$> _sCertificateARN,
                  ("url" .=) <$> _sURL,
                  ("systemIds" .=) <$> _sSystemIds])

-- | Use these settings to set up encryption with a static key provider.
--
-- /See:/ 'staticKeyProvider' smart constructor.
data StaticKeyProvider = StaticKeyProvider'
  { _skpStaticKeyValue    :: !(Maybe Text)
  , _skpURL               :: !(Maybe Text)
  , _skpKeyFormat         :: !(Maybe Text)
  , _skpKeyFormatVersions :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StaticKeyProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skpStaticKeyValue' - Relates to DRM implementation. Use a 32-character hexidecimal string to specify Key Value (StaticKeyValue).
--
-- * 'skpURL' - Relates to DRM implementation. The location of the license server used for protecting content.
--
-- * 'skpKeyFormat' - Relates to DRM implementation. Sets the value of the KEYFORMAT attribute. Must be 'identity' or a reverse DNS string. May be omitted to indicate an implicit value of 'identity'.
--
-- * 'skpKeyFormatVersions' - Relates to DRM implementation. Either a single positive integer version value or a slash delimited list of version values (1/2/3).
staticKeyProvider
    :: StaticKeyProvider
staticKeyProvider =
  StaticKeyProvider'
    { _skpStaticKeyValue = Nothing
    , _skpURL = Nothing
    , _skpKeyFormat = Nothing
    , _skpKeyFormatVersions = Nothing
    }


-- | Relates to DRM implementation. Use a 32-character hexidecimal string to specify Key Value (StaticKeyValue).
skpStaticKeyValue :: Lens' StaticKeyProvider (Maybe Text)
skpStaticKeyValue = lens _skpStaticKeyValue (\ s a -> s{_skpStaticKeyValue = a})

-- | Relates to DRM implementation. The location of the license server used for protecting content.
skpURL :: Lens' StaticKeyProvider (Maybe Text)
skpURL = lens _skpURL (\ s a -> s{_skpURL = a})

-- | Relates to DRM implementation. Sets the value of the KEYFORMAT attribute. Must be 'identity' or a reverse DNS string. May be omitted to indicate an implicit value of 'identity'.
skpKeyFormat :: Lens' StaticKeyProvider (Maybe Text)
skpKeyFormat = lens _skpKeyFormat (\ s a -> s{_skpKeyFormat = a})

-- | Relates to DRM implementation. Either a single positive integer version value or a slash delimited list of version values (1/2/3).
skpKeyFormatVersions :: Lens' StaticKeyProvider (Maybe Text)
skpKeyFormatVersions = lens _skpKeyFormatVersions (\ s a -> s{_skpKeyFormatVersions = a})

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
                 [("staticKeyValue" .=) <$> _skpStaticKeyValue,
                  ("url" .=) <$> _skpURL,
                  ("keyFormat" .=) <$> _skpKeyFormat,
                  ("keyFormatVersions" .=) <$> _skpKeyFormatVersions])

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
  , _tbFontSize :: !(Maybe Nat)
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
tbFontSize :: Lens' TimecodeBurnin (Maybe Natural)
tbFontSize = lens _tbFontSize (\ s a -> s{_tbFontSize = a}) . mapping _Nat

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

-- | These settings control how the service handles timecodes throughout the job. These settings don't affect input clipping.
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
-- * 'tcStart' - Only use when you set Source (TimecodeSource) to Specified start (SPECIFIEDSTART). Use Start timecode (Start) to specify the timecode for the initial frame. Use 24-hour format with frame number, (HH:MM:SS:FF) or (HH:MM:SS;FF).
--
-- * 'tcTimestampOffset' - Only applies to outputs that support program-date-time stamp. Use Timestamp offset (TimestampOffset) to overwrite the timecode date without affecting the time and frame number. Provide the new date as a string in the format "yyyy-mm-dd".  To use Time stamp offset, you must also enable Insert program-date-time (InsertProgramDateTime) in the output settings. For example, if the date part of your timecodes is 2002-1-25 and you want to change it to one year later, set Timestamp offset (TimestampOffset) to 2003-1-25.
--
-- * 'tcAnchor' - If you use an editing platform that relies on an anchor timecode, use Anchor Timecode (Anchor) to specify a timecode that will match the input video frame to the output video frame. Use 24-hour format with frame number, (HH:MM:SS:FF) or (HH:MM:SS;FF). This setting ignores frame rate conversion. System behavior for Anchor Timecode varies depending on your setting for Source (TimecodeSource). * If Source (TimecodeSource) is set to Specified Start (SPECIFIEDSTART), the first input frame is the specified value in Start Timecode (Start). Anchor Timecode (Anchor) and Start Timecode (Start) are used calculate output timecode. * If Source (TimecodeSource) is set to Start at 0 (ZEROBASED)  the  first frame is 00:00:00:00. * If Source (TimecodeSource) is set to Embedded (EMBEDDED), the  first frame is the timecode value on the first input frame of the input.
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


-- | Only use when you set Source (TimecodeSource) to Specified start (SPECIFIEDSTART). Use Start timecode (Start) to specify the timecode for the initial frame. Use 24-hour format with frame number, (HH:MM:SS:FF) or (HH:MM:SS;FF).
tcStart :: Lens' TimecodeConfig (Maybe Text)
tcStart = lens _tcStart (\ s a -> s{_tcStart = a})

-- | Only applies to outputs that support program-date-time stamp. Use Timestamp offset (TimestampOffset) to overwrite the timecode date without affecting the time and frame number. Provide the new date as a string in the format "yyyy-mm-dd".  To use Time stamp offset, you must also enable Insert program-date-time (InsertProgramDateTime) in the output settings. For example, if the date part of your timecodes is 2002-1-25 and you want to change it to one year later, set Timestamp offset (TimestampOffset) to 2003-1-25.
tcTimestampOffset :: Lens' TimecodeConfig (Maybe Text)
tcTimestampOffset = lens _tcTimestampOffset (\ s a -> s{_tcTimestampOffset = a})

-- | If you use an editing platform that relies on an anchor timecode, use Anchor Timecode (Anchor) to specify a timecode that will match the input video frame to the output video frame. Use 24-hour format with frame number, (HH:MM:SS:FF) or (HH:MM:SS;FF). This setting ignores frame rate conversion. System behavior for Anchor Timecode varies depending on your setting for Source (TimecodeSource). * If Source (TimecodeSource) is set to Specified Start (SPECIFIEDSTART), the first input frame is the specified value in Start Timecode (Start). Anchor Timecode (Anchor) and Start Timecode (Start) are used calculate output timecode. * If Source (TimecodeSource) is set to Start at 0 (ZEROBASED)  the  first frame is 00:00:00:00. * If Source (TimecodeSource) is set to Embedded (EMBEDDED), the  first frame is the timecode value on the first input frame of the input.
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

-- | Settings specific to caption sources that are specfied by track number. Sources include IMSC in IMF.
--
-- /See:/ 'trackSourceSettings' smart constructor.
newtype TrackSourceSettings = TrackSourceSettings'
  { _tssTrackNumber :: Maybe Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TrackSourceSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tssTrackNumber' - Use this setting to select a single captions track from a source. Track numbers correspond to the order in the captions source file. For IMF sources, track numbering is based on the order that the captions appear in the CPL. For example, use 1 to select the captions asset that is listed first in the CPL. To include more than one captions track in your job outputs, create multiple input captions selectors. Specify one track per selector.
trackSourceSettings
    :: TrackSourceSettings
trackSourceSettings = TrackSourceSettings' {_tssTrackNumber = Nothing}


-- | Use this setting to select a single captions track from a source. Track numbers correspond to the order in the captions source file. For IMF sources, track numbering is based on the order that the captions appear in the CPL. For example, use 1 to select the captions asset that is listed first in the CPL. To include more than one captions track in your job outputs, create multiple input captions selectors. Specify one track per selector.
tssTrackNumber :: Lens' TrackSourceSettings (Maybe Natural)
tssTrackNumber = lens _tssTrackNumber (\ s a -> s{_tssTrackNumber = a}) . mapping _Nat

instance FromJSON TrackSourceSettings where
        parseJSON
          = withObject "TrackSourceSettings"
              (\ x ->
                 TrackSourceSettings' <$> (x .:? "trackNumber"))

instance Hashable TrackSourceSettings where

instance NFData TrackSourceSettings where

instance ToJSON TrackSourceSettings where
        toJSON TrackSourceSettings'{..}
          = object
              (catMaybes [("trackNumber" .=) <$> _tssTrackNumber])

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
-- * 'vcsCodec' - Specifies the video codec. This must be equal to one of the enum values defined by the object  VideoCodec.
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

-- | Specifies the video codec. This must be equal to one of the enum values defined by the object  VideoCodec.
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
  , _vdHeight             :: !(Maybe Nat)
  , _vdAfdSignaling       :: !(Maybe AfdSignaling)
  , _vdSharpness          :: !(Maybe Nat)
  , _vdCrop               :: !(Maybe Rectangle)
  , _vdWidth              :: !(Maybe Nat)
  , _vdScalingBehavior    :: !(Maybe ScalingBehavior)
  , _vdRespondToAfd       :: !(Maybe RespondToAfd)
  , _vdDropFrameTimecode  :: !(Maybe DropFrameTimecode)
  , _vdAntiAlias          :: !(Maybe AntiAlias)
  , _vdFixedAfd           :: !(Maybe Nat)
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
-- * 'vdSharpness' - Use Sharpness (Sharpness) setting to specify the strength of anti-aliasing. This setting changes the width of the anti-alias filter kernel used for scaling. Sharpness only applies if your output resolution is different from your input resolution. 0 is the softest setting, 100 the sharpest, and 50 recommended for most content.
--
-- * 'vdCrop' - Applies only if your input aspect ratio is different from your output aspect ratio. Use Input cropping rectangle (Crop) to specify the  video area the service will include in the output. This will crop the input source, causing video pixels to be removed on encode. If you crop your input frame size to smaller than your output frame size, make sure to specify the behavior you want in your output setting "Scaling behavior".
--
-- * 'vdWidth' - Use Width (Width) to define the video resolution width, in pixels, for this output. If you don't provide a value here, the service will use the input width.
--
-- * 'vdScalingBehavior' - Undocumented member.
--
-- * 'vdRespondToAfd' - Undocumented member.
--
-- * 'vdDropFrameTimecode' - Undocumented member.
--
-- * 'vdAntiAlias' - You no longer need to specify the anti-alias filter. It's now automatically applied to all outputs. This property is deprecated.
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
vdHeight :: Lens' VideoDescription (Maybe Natural)
vdHeight = lens _vdHeight (\ s a -> s{_vdHeight = a}) . mapping _Nat

-- | Undocumented member.
vdAfdSignaling :: Lens' VideoDescription (Maybe AfdSignaling)
vdAfdSignaling = lens _vdAfdSignaling (\ s a -> s{_vdAfdSignaling = a})

-- | Use Sharpness (Sharpness) setting to specify the strength of anti-aliasing. This setting changes the width of the anti-alias filter kernel used for scaling. Sharpness only applies if your output resolution is different from your input resolution. 0 is the softest setting, 100 the sharpest, and 50 recommended for most content.
vdSharpness :: Lens' VideoDescription (Maybe Natural)
vdSharpness = lens _vdSharpness (\ s a -> s{_vdSharpness = a}) . mapping _Nat

-- | Applies only if your input aspect ratio is different from your output aspect ratio. Use Input cropping rectangle (Crop) to specify the  video area the service will include in the output. This will crop the input source, causing video pixels to be removed on encode. If you crop your input frame size to smaller than your output frame size, make sure to specify the behavior you want in your output setting "Scaling behavior".
vdCrop :: Lens' VideoDescription (Maybe Rectangle)
vdCrop = lens _vdCrop (\ s a -> s{_vdCrop = a})

-- | Use Width (Width) to define the video resolution width, in pixels, for this output. If you don't provide a value here, the service will use the input width.
vdWidth :: Lens' VideoDescription (Maybe Natural)
vdWidth = lens _vdWidth (\ s a -> s{_vdWidth = a}) . mapping _Nat

-- | Undocumented member.
vdScalingBehavior :: Lens' VideoDescription (Maybe ScalingBehavior)
vdScalingBehavior = lens _vdScalingBehavior (\ s a -> s{_vdScalingBehavior = a})

-- | Undocumented member.
vdRespondToAfd :: Lens' VideoDescription (Maybe RespondToAfd)
vdRespondToAfd = lens _vdRespondToAfd (\ s a -> s{_vdRespondToAfd = a})

-- | Undocumented member.
vdDropFrameTimecode :: Lens' VideoDescription (Maybe DropFrameTimecode)
vdDropFrameTimecode = lens _vdDropFrameTimecode (\ s a -> s{_vdDropFrameTimecode = a})

-- | You no longer need to specify the anti-alias filter. It's now automatically applied to all outputs. This property is deprecated.
vdAntiAlias :: Lens' VideoDescription (Maybe AntiAlias)
vdAntiAlias = lens _vdAntiAlias (\ s a -> s{_vdAntiAlias = a})

-- | Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED). Use Fixed (FixedAfd) to specify a four-bit AFD value which the service will write on all  frames of this video output.
vdFixedAfd :: Lens' VideoDescription (Maybe Natural)
vdFixedAfd = lens _vdFixedAfd (\ s a -> s{_vdFixedAfd = a}) . mapping _Nat

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
  , _vsPid             :: !(Maybe Nat)
  , _vsRotate          :: !(Maybe InputRotate)
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
-- * 'vsRotate' - Undocumented member.
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
    , _vsRotate = Nothing
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
vsPid :: Lens' VideoSelector (Maybe Natural)
vsPid = lens _vsPid (\ s a -> s{_vsPid = a}) . mapping _Nat

-- | Undocumented member.
vsRotate :: Lens' VideoSelector (Maybe InputRotate)
vsRotate = lens _vsRotate (\ s a -> s{_vsRotate = a})

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
                     <*> (x .:? "rotate")
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
                  ("pid" .=) <$> _vsPid, ("rotate" .=) <$> _vsRotate,
                  ("colorSpace" .=) <$> _vsColorSpace])

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value WAV.
--
-- /See:/ 'wavSettings' smart constructor.
data WavSettings = WavSettings'
  { _wsBitDepth   :: !(Maybe Nat)
  , _wsChannels   :: !(Maybe Nat)
  , _wsFormat     :: !(Maybe WavFormat)
  , _wsSampleRate :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WavSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wsBitDepth' - Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
--
-- * 'wsChannels' - Set Channels to specify the number of channels in this output audio track. With WAV, valid values 1, 2, 4, and 8. In the console, these values are Mono, Stereo, 4-Channel, and 8-Channel, respectively.
--
-- * 'wsFormat' - Undocumented member.
--
-- * 'wsSampleRate' - Sample rate in Hz.
wavSettings
    :: WavSettings
wavSettings =
  WavSettings'
    { _wsBitDepth = Nothing
    , _wsChannels = Nothing
    , _wsFormat = Nothing
    , _wsSampleRate = Nothing
    }


-- | Specify Bit depth (BitDepth), in bits per sample, to choose the encoding quality for this audio track.
wsBitDepth :: Lens' WavSettings (Maybe Natural)
wsBitDepth = lens _wsBitDepth (\ s a -> s{_wsBitDepth = a}) . mapping _Nat

-- | Set Channels to specify the number of channels in this output audio track. With WAV, valid values 1, 2, 4, and 8. In the console, these values are Mono, Stereo, 4-Channel, and 8-Channel, respectively.
wsChannels :: Lens' WavSettings (Maybe Natural)
wsChannels = lens _wsChannels (\ s a -> s{_wsChannels = a}) . mapping _Nat

-- | Undocumented member.
wsFormat :: Lens' WavSettings (Maybe WavFormat)
wsFormat = lens _wsFormat (\ s a -> s{_wsFormat = a})

-- | Sample rate in Hz.
wsSampleRate :: Lens' WavSettings (Maybe Natural)
wsSampleRate = lens _wsSampleRate (\ s a -> s{_wsSampleRate = a}) . mapping _Nat

instance FromJSON WavSettings where
        parseJSON
          = withObject "WavSettings"
              (\ x ->
                 WavSettings' <$>
                   (x .:? "bitDepth") <*> (x .:? "channels") <*>
                     (x .:? "format")
                     <*> (x .:? "sampleRate"))

instance Hashable WavSettings where

instance NFData WavSettings where

instance ToJSON WavSettings where
        toJSON WavSettings'{..}
          = object
              (catMaybes
                 [("bitDepth" .=) <$> _wsBitDepth,
                  ("channels" .=) <$> _wsChannels,
                  ("format" .=) <$> _wsFormat,
                  ("sampleRate" .=) <$> _wsSampleRate])
