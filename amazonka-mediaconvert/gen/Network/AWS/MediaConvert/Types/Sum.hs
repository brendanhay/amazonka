{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Sum where

import Network.AWS.Prelude

-- | Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio + audio description (AD) as a stereo pair. The value for AudioType will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any values you provide in AudioType and  FollowInputAudioType. Choose NORMAL when the input does not contain pre-mixed audio + audio description (AD). In this case, the encoder will use any values you provide for AudioType and FollowInputAudioType.
data AacAudioDescriptionBroadcasterMix
  = AADBMBroadcasterMixedAd
  | AADBMNormal
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AacAudioDescriptionBroadcasterMix where
    parser = takeLowerText >>= \case
        "broadcaster_mixed_ad" -> pure AADBMBroadcasterMixedAd
        "normal" -> pure AADBMNormal
        e -> fromTextError $ "Failure parsing AacAudioDescriptionBroadcasterMix from value: '" <> e
           <> "'. Accepted values: broadcaster_mixed_ad, normal"

instance ToText AacAudioDescriptionBroadcasterMix where
    toText = \case
        AADBMBroadcasterMixedAd -> "BROADCASTER_MIXED_AD"
        AADBMNormal -> "NORMAL"

instance Hashable     AacAudioDescriptionBroadcasterMix
instance NFData       AacAudioDescriptionBroadcasterMix
instance ToByteString AacAudioDescriptionBroadcasterMix
instance ToQuery      AacAudioDescriptionBroadcasterMix
instance ToHeader     AacAudioDescriptionBroadcasterMix

instance ToJSON AacAudioDescriptionBroadcasterMix where
    toJSON = toJSONText

instance FromJSON AacAudioDescriptionBroadcasterMix where
    parseJSON = parseJSONText "AacAudioDescriptionBroadcasterMix"

-- | AAC Profile.
data AacCodecProfile
  = HEV1
  | HEV2
  | LC
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AacCodecProfile where
    parser = takeLowerText >>= \case
        "hev1" -> pure HEV1
        "hev2" -> pure HEV2
        "lc" -> pure LC
        e -> fromTextError $ "Failure parsing AacCodecProfile from value: '" <> e
           <> "'. Accepted values: hev1, hev2, lc"

instance ToText AacCodecProfile where
    toText = \case
        HEV1 -> "HEV1"
        HEV2 -> "HEV2"
        LC -> "LC"

instance Hashable     AacCodecProfile
instance NFData       AacCodecProfile
instance ToByteString AacCodecProfile
instance ToQuery      AacCodecProfile
instance ToHeader     AacCodecProfile

instance ToJSON AacCodecProfile where
    toJSON = toJSONText

instance FromJSON AacCodecProfile where
    parseJSON = parseJSONText "AacCodecProfile"

-- | Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. "1.0 - Audio Description (Receiver Mix)" setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
data AacCodingMode
  = ACMAdReceiverMix
  | ACMCodingMode10
  | ACMCodingMode11
  | ACMCodingMode20
  | ACMCodingMode51
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AacCodingMode where
    parser = takeLowerText >>= \case
        "ad_receiver_mix" -> pure ACMAdReceiverMix
        "coding_mode_1_0" -> pure ACMCodingMode10
        "coding_mode_1_1" -> pure ACMCodingMode11
        "coding_mode_2_0" -> pure ACMCodingMode20
        "coding_mode_5_1" -> pure ACMCodingMode51
        e -> fromTextError $ "Failure parsing AacCodingMode from value: '" <> e
           <> "'. Accepted values: ad_receiver_mix, coding_mode_1_0, coding_mode_1_1, coding_mode_2_0, coding_mode_5_1"

instance ToText AacCodingMode where
    toText = \case
        ACMAdReceiverMix -> "AD_RECEIVER_MIX"
        ACMCodingMode10 -> "CODING_MODE_1_0"
        ACMCodingMode11 -> "CODING_MODE_1_1"
        ACMCodingMode20 -> "CODING_MODE_2_0"
        ACMCodingMode51 -> "CODING_MODE_5_1"

instance Hashable     AacCodingMode
instance NFData       AacCodingMode
instance ToByteString AacCodingMode
instance ToQuery      AacCodingMode
instance ToHeader     AacCodingMode

instance ToJSON AacCodingMode where
    toJSON = toJSONText

instance FromJSON AacCodingMode where
    parseJSON = parseJSONText "AacCodingMode"

-- | Rate Control Mode.
data AacRateControlMode
  = ARCMCbr
  | ARCMVbr
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AacRateControlMode where
    parser = takeLowerText >>= \case
        "cbr" -> pure ARCMCbr
        "vbr" -> pure ARCMVbr
        e -> fromTextError $ "Failure parsing AacRateControlMode from value: '" <> e
           <> "'. Accepted values: cbr, vbr"

instance ToText AacRateControlMode where
    toText = \case
        ARCMCbr -> "CBR"
        ARCMVbr -> "VBR"

instance Hashable     AacRateControlMode
instance NFData       AacRateControlMode
instance ToByteString AacRateControlMode
instance ToQuery      AacRateControlMode
instance ToHeader     AacRateControlMode

instance ToJSON AacRateControlMode where
    toJSON = toJSONText

instance FromJSON AacRateControlMode where
    parseJSON = parseJSONText "AacRateControlMode"

-- | Enables LATM/LOAS AAC output. Note that if you use LATM/LOAS AAC in an output, you must choose "No container" for the output container.
data AacRawFormat
  = ARFLatmLoas
  | ARFNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AacRawFormat where
    parser = takeLowerText >>= \case
        "latm_loas" -> pure ARFLatmLoas
        "none" -> pure ARFNone
        e -> fromTextError $ "Failure parsing AacRawFormat from value: '" <> e
           <> "'. Accepted values: latm_loas, none"

instance ToText AacRawFormat where
    toText = \case
        ARFLatmLoas -> "LATM_LOAS"
        ARFNone -> "NONE"

instance Hashable     AacRawFormat
instance NFData       AacRawFormat
instance ToByteString AacRawFormat
instance ToQuery      AacRawFormat
instance ToHeader     AacRawFormat

instance ToJSON AacRawFormat where
    toJSON = toJSONText

instance FromJSON AacRawFormat where
    parseJSON = parseJSONText "AacRawFormat"

-- | Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
data AacSpecification
  = ASMPEG2
  | ASMPEG4
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AacSpecification where
    parser = takeLowerText >>= \case
        "mpeg2" -> pure ASMPEG2
        "mpeg4" -> pure ASMPEG4
        e -> fromTextError $ "Failure parsing AacSpecification from value: '" <> e
           <> "'. Accepted values: mpeg2, mpeg4"

instance ToText AacSpecification where
    toText = \case
        ASMPEG2 -> "MPEG2"
        ASMPEG4 -> "MPEG4"

instance Hashable     AacSpecification
instance NFData       AacSpecification
instance ToByteString AacSpecification
instance ToQuery      AacSpecification
instance ToHeader     AacSpecification

instance ToJSON AacSpecification where
    toJSON = toJSONText

instance FromJSON AacSpecification where
    parseJSON = parseJSONText "AacSpecification"

-- | VBR Quality Level - Only used if rate_control_mode is VBR.
data AacVbrQuality
  = AVQHigh
  | AVQLow
  | AVQMediumHigh
  | AVQMediumLow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AacVbrQuality where
    parser = takeLowerText >>= \case
        "high" -> pure AVQHigh
        "low" -> pure AVQLow
        "medium_high" -> pure AVQMediumHigh
        "medium_low" -> pure AVQMediumLow
        e -> fromTextError $ "Failure parsing AacVbrQuality from value: '" <> e
           <> "'. Accepted values: high, low, medium_high, medium_low"

instance ToText AacVbrQuality where
    toText = \case
        AVQHigh -> "HIGH"
        AVQLow -> "LOW"
        AVQMediumHigh -> "MEDIUM_HIGH"
        AVQMediumLow -> "MEDIUM_LOW"

instance Hashable     AacVbrQuality
instance NFData       AacVbrQuality
instance ToByteString AacVbrQuality
instance ToQuery      AacVbrQuality
instance ToHeader     AacVbrQuality

instance ToJSON AacVbrQuality where
    toJSON = toJSONText

instance FromJSON AacVbrQuality where
    parseJSON = parseJSONText "AacVbrQuality"

-- | Specifies the "Bitstream Mode" (bsmod) for the emitted AC-3 stream. See ATSC A/52-2012 for background on these values.
data Ac3BitstreamMode
  = Commentary
  | CompleteMain
  | Dialogue
  | Emergency
  | HearingImpaired
  | MusicAndEffects
  | VisuallyImpaired
  | VoiceOver
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Ac3BitstreamMode where
    parser = takeLowerText >>= \case
        "commentary" -> pure Commentary
        "complete_main" -> pure CompleteMain
        "dialogue" -> pure Dialogue
        "emergency" -> pure Emergency
        "hearing_impaired" -> pure HearingImpaired
        "music_and_effects" -> pure MusicAndEffects
        "visually_impaired" -> pure VisuallyImpaired
        "voice_over" -> pure VoiceOver
        e -> fromTextError $ "Failure parsing Ac3BitstreamMode from value: '" <> e
           <> "'. Accepted values: commentary, complete_main, dialogue, emergency, hearing_impaired, music_and_effects, visually_impaired, voice_over"

instance ToText Ac3BitstreamMode where
    toText = \case
        Commentary -> "COMMENTARY"
        CompleteMain -> "COMPLETE_MAIN"
        Dialogue -> "DIALOGUE"
        Emergency -> "EMERGENCY"
        HearingImpaired -> "HEARING_IMPAIRED"
        MusicAndEffects -> "MUSIC_AND_EFFECTS"
        VisuallyImpaired -> "VISUALLY_IMPAIRED"
        VoiceOver -> "VOICE_OVER"

instance Hashable     Ac3BitstreamMode
instance NFData       Ac3BitstreamMode
instance ToByteString Ac3BitstreamMode
instance ToQuery      Ac3BitstreamMode
instance ToHeader     Ac3BitstreamMode

instance ToJSON Ac3BitstreamMode where
    toJSON = toJSONText

instance FromJSON Ac3BitstreamMode where
    parseJSON = parseJSONText "Ac3BitstreamMode"

-- | Dolby Digital coding mode. Determines number of channels.
data Ac3CodingMode
  = CodingMode10
  | CodingMode11
  | CodingMode20
  | CodingMode32Lfe
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Ac3CodingMode where
    parser = takeLowerText >>= \case
        "coding_mode_1_0" -> pure CodingMode10
        "coding_mode_1_1" -> pure CodingMode11
        "coding_mode_2_0" -> pure CodingMode20
        "coding_mode_3_2_lfe" -> pure CodingMode32Lfe
        e -> fromTextError $ "Failure parsing Ac3CodingMode from value: '" <> e
           <> "'. Accepted values: coding_mode_1_0, coding_mode_1_1, coding_mode_2_0, coding_mode_3_2_lfe"

instance ToText Ac3CodingMode where
    toText = \case
        CodingMode10 -> "CODING_MODE_1_0"
        CodingMode11 -> "CODING_MODE_1_1"
        CodingMode20 -> "CODING_MODE_2_0"
        CodingMode32Lfe -> "CODING_MODE_3_2_LFE"

instance Hashable     Ac3CodingMode
instance NFData       Ac3CodingMode
instance ToByteString Ac3CodingMode
instance ToQuery      Ac3CodingMode
instance ToHeader     Ac3CodingMode

instance ToJSON Ac3CodingMode where
    toJSON = toJSONText

instance FromJSON Ac3CodingMode where
    parseJSON = parseJSONText "Ac3CodingMode"

-- | If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
data Ac3DynamicRangeCompressionProfile
  = ADRCPFilmStandard
  | ADRCPNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Ac3DynamicRangeCompressionProfile where
    parser = takeLowerText >>= \case
        "film_standard" -> pure ADRCPFilmStandard
        "none" -> pure ADRCPNone
        e -> fromTextError $ "Failure parsing Ac3DynamicRangeCompressionProfile from value: '" <> e
           <> "'. Accepted values: film_standard, none"

instance ToText Ac3DynamicRangeCompressionProfile where
    toText = \case
        ADRCPFilmStandard -> "FILM_STANDARD"
        ADRCPNone -> "NONE"

instance Hashable     Ac3DynamicRangeCompressionProfile
instance NFData       Ac3DynamicRangeCompressionProfile
instance ToByteString Ac3DynamicRangeCompressionProfile
instance ToQuery      Ac3DynamicRangeCompressionProfile
instance ToHeader     Ac3DynamicRangeCompressionProfile

instance ToJSON Ac3DynamicRangeCompressionProfile where
    toJSON = toJSONText

instance FromJSON Ac3DynamicRangeCompressionProfile where
    parseJSON = parseJSONText "Ac3DynamicRangeCompressionProfile"

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
data Ac3LfeFilter
  = ALFDisabled
  | ALFEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Ac3LfeFilter where
    parser = takeLowerText >>= \case
        "disabled" -> pure ALFDisabled
        "enabled" -> pure ALFEnabled
        e -> fromTextError $ "Failure parsing Ac3LfeFilter from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText Ac3LfeFilter where
    toText = \case
        ALFDisabled -> "DISABLED"
        ALFEnabled -> "ENABLED"

instance Hashable     Ac3LfeFilter
instance NFData       Ac3LfeFilter
instance ToByteString Ac3LfeFilter
instance ToQuery      Ac3LfeFilter
instance ToHeader     Ac3LfeFilter

instance ToJSON Ac3LfeFilter where
    toJSON = toJSONText

instance FromJSON Ac3LfeFilter where
    parseJSON = parseJSONText "Ac3LfeFilter"

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
data Ac3MetadataControl
  = AMCFollowInput
  | AMCUseConfigured
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Ac3MetadataControl where
    parser = takeLowerText >>= \case
        "follow_input" -> pure AMCFollowInput
        "use_configured" -> pure AMCUseConfigured
        e -> fromTextError $ "Failure parsing Ac3MetadataControl from value: '" <> e
           <> "'. Accepted values: follow_input, use_configured"

instance ToText Ac3MetadataControl where
    toText = \case
        AMCFollowInput -> "FOLLOW_INPUT"
        AMCUseConfigured -> "USE_CONFIGURED"

instance Hashable     Ac3MetadataControl
instance NFData       Ac3MetadataControl
instance ToByteString Ac3MetadataControl
instance ToQuery      Ac3MetadataControl
instance ToHeader     Ac3MetadataControl

instance ToJSON Ac3MetadataControl where
    toJSON = toJSONText

instance FromJSON Ac3MetadataControl where
    parseJSON = parseJSONText "Ac3MetadataControl"

-- | This setting only applies to H.264 and MPEG2 outputs. Use Insert AFD signaling (AfdSignaling) to whether there are AFD values in the output video data and what those values are. * Choose None to remove all AFD values from this output. * Choose Fixed to ignore input AFD values and instead encode the value specified in the job. * Choose Auto to calculate output AFD values based on the input AFD scaler data.
data AfdSignaling
  = ASAuto
  | ASFixed
  | ASNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AfdSignaling where
    parser = takeLowerText >>= \case
        "auto" -> pure ASAuto
        "fixed" -> pure ASFixed
        "none" -> pure ASNone
        e -> fromTextError $ "Failure parsing AfdSignaling from value: '" <> e
           <> "'. Accepted values: auto, fixed, none"

instance ToText AfdSignaling where
    toText = \case
        ASAuto -> "AUTO"
        ASFixed -> "FIXED"
        ASNone -> "NONE"

instance Hashable     AfdSignaling
instance NFData       AfdSignaling
instance ToByteString AfdSignaling
instance ToQuery      AfdSignaling
instance ToHeader     AfdSignaling

instance ToJSON AfdSignaling where
    toJSON = toJSONText

instance FromJSON AfdSignaling where
    parseJSON = parseJSONText "AfdSignaling"

-- | Enable Anti-alias (AntiAlias) to enhance sharp edges in video output when your input resolution is much larger than your output resolution. Default is enabled.
data AntiAlias
  = AADisabled
  | AAEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AntiAlias where
    parser = takeLowerText >>= \case
        "disabled" -> pure AADisabled
        "enabled" -> pure AAEnabled
        e -> fromTextError $ "Failure parsing AntiAlias from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText AntiAlias where
    toText = \case
        AADisabled -> "DISABLED"
        AAEnabled -> "ENABLED"

instance Hashable     AntiAlias
instance NFData       AntiAlias
instance ToByteString AntiAlias
instance ToQuery      AntiAlias
instance ToHeader     AntiAlias

instance ToJSON AntiAlias where
    toJSON = toJSONText

instance FromJSON AntiAlias where
    parseJSON = parseJSONText "AntiAlias"

-- | Type of Audio codec.
data AudioCodec
  = AC3
  | Aac
  | Aiff
  | EAC3
  | MP2
  | Passthrough
  | Wav
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioCodec where
    parser = takeLowerText >>= \case
        "ac3" -> pure AC3
        "aac" -> pure Aac
        "aiff" -> pure Aiff
        "eac3" -> pure EAC3
        "mp2" -> pure MP2
        "passthrough" -> pure Passthrough
        "wav" -> pure Wav
        e -> fromTextError $ "Failure parsing AudioCodec from value: '" <> e
           <> "'. Accepted values: ac3, aac, aiff, eac3, mp2, passthrough, wav"

instance ToText AudioCodec where
    toText = \case
        AC3 -> "AC3"
        Aac -> "AAC"
        Aiff -> "AIFF"
        EAC3 -> "EAC3"
        MP2 -> "MP2"
        Passthrough -> "PASSTHROUGH"
        Wav -> "WAV"

instance Hashable     AudioCodec
instance NFData       AudioCodec
instance ToByteString AudioCodec
instance ToQuery      AudioCodec
instance ToHeader     AudioCodec

instance ToJSON AudioCodec where
    toJSON = toJSONText

instance FromJSON AudioCodec where
    parseJSON = parseJSONText "AudioCodec"

-- | When an "Audio Description":#audio_description specifies an AudioSelector or AudioSelectorGroup  for which no matching source is found in the input, then the audio selector marked as DEFAULT will be used.  If none are marked as default, silence will be inserted for the duration of the input.
data AudioDefaultSelection
  = Default
  | NotDefault
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioDefaultSelection where
    parser = takeLowerText >>= \case
        "default" -> pure Default
        "not_default" -> pure NotDefault
        e -> fromTextError $ "Failure parsing AudioDefaultSelection from value: '" <> e
           <> "'. Accepted values: default, not_default"

instance ToText AudioDefaultSelection where
    toText = \case
        Default -> "DEFAULT"
        NotDefault -> "NOT_DEFAULT"

instance Hashable     AudioDefaultSelection
instance NFData       AudioDefaultSelection
instance ToByteString AudioDefaultSelection
instance ToQuery      AudioDefaultSelection
instance ToHeader     AudioDefaultSelection

instance ToJSON AudioDefaultSelection where
    toJSON = toJSONText

instance FromJSON AudioDefaultSelection where
    parseJSON = parseJSONText "AudioDefaultSelection"

-- | Choosing FOLLOW_INPUT will cause the ISO 639 language code of the output to follow the ISO 639 language code of the input. The language specified for languageCode' will be used when USE_CONFIGURED is selected or when FOLLOW_INPUT is selected but there is no ISO 639 language code specified by the input.
data AudioLanguageCodeControl
  = FollowInput
  | UseConfigured
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioLanguageCodeControl where
    parser = takeLowerText >>= \case
        "follow_input" -> pure FollowInput
        "use_configured" -> pure UseConfigured
        e -> fromTextError $ "Failure parsing AudioLanguageCodeControl from value: '" <> e
           <> "'. Accepted values: follow_input, use_configured"

instance ToText AudioLanguageCodeControl where
    toText = \case
        FollowInput -> "FOLLOW_INPUT"
        UseConfigured -> "USE_CONFIGURED"

instance Hashable     AudioLanguageCodeControl
instance NFData       AudioLanguageCodeControl
instance ToByteString AudioLanguageCodeControl
instance ToQuery      AudioLanguageCodeControl
instance ToHeader     AudioLanguageCodeControl

instance ToJSON AudioLanguageCodeControl where
    toJSON = toJSONText

instance FromJSON AudioLanguageCodeControl where
    parseJSON = parseJSONText "AudioLanguageCodeControl"

-- | Audio normalization algorithm to use. 1770-1 conforms to the CALM Act specification, 1770-2 conforms to the EBU R-128 specification.
data AudioNormalizationAlgorithm
  = ItuBs17701
  | ItuBs17702
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioNormalizationAlgorithm where
    parser = takeLowerText >>= \case
        "itu_bs_1770_1" -> pure ItuBs17701
        "itu_bs_1770_2" -> pure ItuBs17702
        e -> fromTextError $ "Failure parsing AudioNormalizationAlgorithm from value: '" <> e
           <> "'. Accepted values: itu_bs_1770_1, itu_bs_1770_2"

instance ToText AudioNormalizationAlgorithm where
    toText = \case
        ItuBs17701 -> "ITU_BS_1770_1"
        ItuBs17702 -> "ITU_BS_1770_2"

instance Hashable     AudioNormalizationAlgorithm
instance NFData       AudioNormalizationAlgorithm
instance ToByteString AudioNormalizationAlgorithm
instance ToQuery      AudioNormalizationAlgorithm
instance ToHeader     AudioNormalizationAlgorithm

instance ToJSON AudioNormalizationAlgorithm where
    toJSON = toJSONText

instance FromJSON AudioNormalizationAlgorithm where
    parseJSON = parseJSONText "AudioNormalizationAlgorithm"

-- | When enabled the output audio is corrected using the chosen algorithm. If disabled, the audio will be measured but not adjusted.
data AudioNormalizationAlgorithmControl
  = CorrectAudio
  | MeasureOnly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioNormalizationAlgorithmControl where
    parser = takeLowerText >>= \case
        "correct_audio" -> pure CorrectAudio
        "measure_only" -> pure MeasureOnly
        e -> fromTextError $ "Failure parsing AudioNormalizationAlgorithmControl from value: '" <> e
           <> "'. Accepted values: correct_audio, measure_only"

instance ToText AudioNormalizationAlgorithmControl where
    toText = \case
        CorrectAudio -> "CORRECT_AUDIO"
        MeasureOnly -> "MEASURE_ONLY"

instance Hashable     AudioNormalizationAlgorithmControl
instance NFData       AudioNormalizationAlgorithmControl
instance ToByteString AudioNormalizationAlgorithmControl
instance ToQuery      AudioNormalizationAlgorithmControl
instance ToHeader     AudioNormalizationAlgorithmControl

instance ToJSON AudioNormalizationAlgorithmControl where
    toJSON = toJSONText

instance FromJSON AudioNormalizationAlgorithmControl where
    parseJSON = parseJSONText "AudioNormalizationAlgorithmControl"

-- | If set to LOG, log each output's audio track loudness to a CSV file.
data AudioNormalizationLoudnessLogging
  = DontLog
  | Log
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioNormalizationLoudnessLogging where
    parser = takeLowerText >>= \case
        "dont_log" -> pure DontLog
        "log" -> pure Log
        e -> fromTextError $ "Failure parsing AudioNormalizationLoudnessLogging from value: '" <> e
           <> "'. Accepted values: dont_log, log"

instance ToText AudioNormalizationLoudnessLogging where
    toText = \case
        DontLog -> "DONT_LOG"
        Log -> "LOG"

instance Hashable     AudioNormalizationLoudnessLogging
instance NFData       AudioNormalizationLoudnessLogging
instance ToByteString AudioNormalizationLoudnessLogging
instance ToQuery      AudioNormalizationLoudnessLogging
instance ToHeader     AudioNormalizationLoudnessLogging

instance ToJSON AudioNormalizationLoudnessLogging where
    toJSON = toJSONText

instance FromJSON AudioNormalizationLoudnessLogging where
    parseJSON = parseJSONText "AudioNormalizationLoudnessLogging"

-- | If set to TRUE_PEAK, calculate and log the TruePeak for each output's audio track loudness.
data AudioNormalizationPeakCalculation
  = ANPCNone
  | ANPCTruePeak
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioNormalizationPeakCalculation where
    parser = takeLowerText >>= \case
        "none" -> pure ANPCNone
        "true_peak" -> pure ANPCTruePeak
        e -> fromTextError $ "Failure parsing AudioNormalizationPeakCalculation from value: '" <> e
           <> "'. Accepted values: none, true_peak"

instance ToText AudioNormalizationPeakCalculation where
    toText = \case
        ANPCNone -> "NONE"
        ANPCTruePeak -> "TRUE_PEAK"

instance Hashable     AudioNormalizationPeakCalculation
instance NFData       AudioNormalizationPeakCalculation
instance ToByteString AudioNormalizationPeakCalculation
instance ToQuery      AudioNormalizationPeakCalculation
instance ToHeader     AudioNormalizationPeakCalculation

instance ToJSON AudioNormalizationPeakCalculation where
    toJSON = toJSONText

instance FromJSON AudioNormalizationPeakCalculation where
    parseJSON = parseJSONText "AudioNormalizationPeakCalculation"

-- | Specifies the type of the audio selector.
data AudioSelectorType
  = LanguageCode
  | Pid
  | Track
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioSelectorType where
    parser = takeLowerText >>= \case
        "language_code" -> pure LanguageCode
        "pid" -> pure Pid
        "track" -> pure Track
        e -> fromTextError $ "Failure parsing AudioSelectorType from value: '" <> e
           <> "'. Accepted values: language_code, pid, track"

instance ToText AudioSelectorType where
    toText = \case
        LanguageCode -> "LANGUAGE_CODE"
        Pid -> "PID"
        Track -> "TRACK"

instance Hashable     AudioSelectorType
instance NFData       AudioSelectorType
instance ToByteString AudioSelectorType
instance ToQuery      AudioSelectorType
instance ToHeader     AudioSelectorType

instance ToJSON AudioSelectorType where
    toJSON = toJSONText

instance FromJSON AudioSelectorType where
    parseJSON = parseJSONText "AudioSelectorType"

-- | When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type, then that value is passed through to the output. If the input contains no ISO 639 audio_type, the value in Audio Type is included in the output. Otherwise the value in Audio Type is included in the output. Note that this field and audioType are both ignored if audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
data AudioTypeControl
  = ATCFollowInput
  | ATCUseConfigured
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioTypeControl where
    parser = takeLowerText >>= \case
        "follow_input" -> pure ATCFollowInput
        "use_configured" -> pure ATCUseConfigured
        e -> fromTextError $ "Failure parsing AudioTypeControl from value: '" <> e
           <> "'. Accepted values: follow_input, use_configured"

instance ToText AudioTypeControl where
    toText = \case
        ATCFollowInput -> "FOLLOW_INPUT"
        ATCUseConfigured -> "USE_CONFIGURED"

instance Hashable     AudioTypeControl
instance NFData       AudioTypeControl
instance ToByteString AudioTypeControl
instance ToQuery      AudioTypeControl
instance ToHeader     AudioTypeControl

instance ToJSON AudioTypeControl where
    toJSON = toJSONText

instance FromJSON AudioTypeControl where
    parseJSON = parseJSONText "AudioTypeControl"

-- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
data BurninSubtitleAlignment
  = BSACentered
  | BSALeft'
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BurninSubtitleAlignment where
    parser = takeLowerText >>= \case
        "centered" -> pure BSACentered
        "left" -> pure BSALeft'
        e -> fromTextError $ "Failure parsing BurninSubtitleAlignment from value: '" <> e
           <> "'. Accepted values: centered, left"

instance ToText BurninSubtitleAlignment where
    toText = \case
        BSACentered -> "CENTERED"
        BSALeft' -> "LEFT"

instance Hashable     BurninSubtitleAlignment
instance NFData       BurninSubtitleAlignment
instance ToByteString BurninSubtitleAlignment
instance ToQuery      BurninSubtitleAlignment
instance ToHeader     BurninSubtitleAlignment

instance ToJSON BurninSubtitleAlignment where
    toJSON = toJSONText

instance FromJSON BurninSubtitleAlignment where
    parseJSON = parseJSONText "BurninSubtitleAlignment"

-- | Specifies the color of the rectangle behind the captions.
--
-- All burn-in and DVB-Sub font settings must match.
data BurninSubtitleBackgroundColor
  = BSBCBlack
  | BSBCNone
  | BSBCWhite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BurninSubtitleBackgroundColor where
    parser = takeLowerText >>= \case
        "black" -> pure BSBCBlack
        "none" -> pure BSBCNone
        "white" -> pure BSBCWhite
        e -> fromTextError $ "Failure parsing BurninSubtitleBackgroundColor from value: '" <> e
           <> "'. Accepted values: black, none, white"

instance ToText BurninSubtitleBackgroundColor where
    toText = \case
        BSBCBlack -> "BLACK"
        BSBCNone -> "NONE"
        BSBCWhite -> "WHITE"

instance Hashable     BurninSubtitleBackgroundColor
instance NFData       BurninSubtitleBackgroundColor
instance ToByteString BurninSubtitleBackgroundColor
instance ToQuery      BurninSubtitleBackgroundColor
instance ToHeader     BurninSubtitleBackgroundColor

instance ToJSON BurninSubtitleBackgroundColor where
    toJSON = toJSONText

instance FromJSON BurninSubtitleBackgroundColor where
    parseJSON = parseJSONText "BurninSubtitleBackgroundColor"

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
data BurninSubtitleFontColor
  = BSFCBlack
  | BSFCBlue
  | BSFCGreen
  | BSFCRed
  | BSFCWhite
  | BSFCYellow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BurninSubtitleFontColor where
    parser = takeLowerText >>= \case
        "black" -> pure BSFCBlack
        "blue" -> pure BSFCBlue
        "green" -> pure BSFCGreen
        "red" -> pure BSFCRed
        "white" -> pure BSFCWhite
        "yellow" -> pure BSFCYellow
        e -> fromTextError $ "Failure parsing BurninSubtitleFontColor from value: '" <> e
           <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText BurninSubtitleFontColor where
    toText = \case
        BSFCBlack -> "BLACK"
        BSFCBlue -> "BLUE"
        BSFCGreen -> "GREEN"
        BSFCRed -> "RED"
        BSFCWhite -> "WHITE"
        BSFCYellow -> "YELLOW"

instance Hashable     BurninSubtitleFontColor
instance NFData       BurninSubtitleFontColor
instance ToByteString BurninSubtitleFontColor
instance ToQuery      BurninSubtitleFontColor
instance ToHeader     BurninSubtitleFontColor

instance ToJSON BurninSubtitleFontColor where
    toJSON = toJSONText

instance FromJSON BurninSubtitleFontColor where
    parseJSON = parseJSONText "BurninSubtitleFontColor"

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
data BurninSubtitleOutlineColor
  = BSOCBlack
  | BSOCBlue
  | BSOCGreen
  | BSOCRed
  | BSOCWhite
  | BSOCYellow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BurninSubtitleOutlineColor where
    parser = takeLowerText >>= \case
        "black" -> pure BSOCBlack
        "blue" -> pure BSOCBlue
        "green" -> pure BSOCGreen
        "red" -> pure BSOCRed
        "white" -> pure BSOCWhite
        "yellow" -> pure BSOCYellow
        e -> fromTextError $ "Failure parsing BurninSubtitleOutlineColor from value: '" <> e
           <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText BurninSubtitleOutlineColor where
    toText = \case
        BSOCBlack -> "BLACK"
        BSOCBlue -> "BLUE"
        BSOCGreen -> "GREEN"
        BSOCRed -> "RED"
        BSOCWhite -> "WHITE"
        BSOCYellow -> "YELLOW"

instance Hashable     BurninSubtitleOutlineColor
instance NFData       BurninSubtitleOutlineColor
instance ToByteString BurninSubtitleOutlineColor
instance ToQuery      BurninSubtitleOutlineColor
instance ToHeader     BurninSubtitleOutlineColor

instance ToJSON BurninSubtitleOutlineColor where
    toJSON = toJSONText

instance FromJSON BurninSubtitleOutlineColor where
    parseJSON = parseJSONText "BurninSubtitleOutlineColor"

-- | Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
data BurninSubtitleShadowColor
  = BSSCBlack
  | BSSCNone
  | BSSCWhite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BurninSubtitleShadowColor where
    parser = takeLowerText >>= \case
        "black" -> pure BSSCBlack
        "none" -> pure BSSCNone
        "white" -> pure BSSCWhite
        e -> fromTextError $ "Failure parsing BurninSubtitleShadowColor from value: '" <> e
           <> "'. Accepted values: black, none, white"

instance ToText BurninSubtitleShadowColor where
    toText = \case
        BSSCBlack -> "BLACK"
        BSSCNone -> "NONE"
        BSSCWhite -> "WHITE"

instance Hashable     BurninSubtitleShadowColor
instance NFData       BurninSubtitleShadowColor
instance ToByteString BurninSubtitleShadowColor
instance ToQuery      BurninSubtitleShadowColor
instance ToHeader     BurninSubtitleShadowColor

instance ToJSON BurninSubtitleShadowColor where
    toJSON = toJSONText

instance FromJSON BurninSubtitleShadowColor where
    parseJSON = parseJSONText "BurninSubtitleShadowColor"

-- | Controls whether a fixed grid size or proportional font spacing will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
data BurninSubtitleTeletextSpacing
  = BSTSFixedGrid
  | BSTSProportional
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BurninSubtitleTeletextSpacing where
    parser = takeLowerText >>= \case
        "fixed_grid" -> pure BSTSFixedGrid
        "proportional" -> pure BSTSProportional
        e -> fromTextError $ "Failure parsing BurninSubtitleTeletextSpacing from value: '" <> e
           <> "'. Accepted values: fixed_grid, proportional"

instance ToText BurninSubtitleTeletextSpacing where
    toText = \case
        BSTSFixedGrid -> "FIXED_GRID"
        BSTSProportional -> "PROPORTIONAL"

instance Hashable     BurninSubtitleTeletextSpacing
instance NFData       BurninSubtitleTeletextSpacing
instance ToByteString BurninSubtitleTeletextSpacing
instance ToQuery      BurninSubtitleTeletextSpacing
instance ToHeader     BurninSubtitleTeletextSpacing

instance ToJSON BurninSubtitleTeletextSpacing where
    toJSON = toJSONText

instance FromJSON BurninSubtitleTeletextSpacing where
    parseJSON = parseJSONText "BurninSubtitleTeletextSpacing"

-- | Type of Caption output, including Burn-In, Embedded, SCC, SRT, TTML, WebVTT, DVB-Sub, Teletext.
data CaptionDestinationType
  = CDTBurnIn
  | CDTDvbSub
  | CDTEmbedded
  | CDTScc
  | CDTSrt
  | CDTTeletext
  | CDTTtml
  | CDTWebvtt
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CaptionDestinationType where
    parser = takeLowerText >>= \case
        "burn_in" -> pure CDTBurnIn
        "dvb_sub" -> pure CDTDvbSub
        "embedded" -> pure CDTEmbedded
        "scc" -> pure CDTScc
        "srt" -> pure CDTSrt
        "teletext" -> pure CDTTeletext
        "ttml" -> pure CDTTtml
        "webvtt" -> pure CDTWebvtt
        e -> fromTextError $ "Failure parsing CaptionDestinationType from value: '" <> e
           <> "'. Accepted values: burn_in, dvb_sub, embedded, scc, srt, teletext, ttml, webvtt"

instance ToText CaptionDestinationType where
    toText = \case
        CDTBurnIn -> "BURN_IN"
        CDTDvbSub -> "DVB_SUB"
        CDTEmbedded -> "EMBEDDED"
        CDTScc -> "SCC"
        CDTSrt -> "SRT"
        CDTTeletext -> "TELETEXT"
        CDTTtml -> "TTML"
        CDTWebvtt -> "WEBVTT"

instance Hashable     CaptionDestinationType
instance NFData       CaptionDestinationType
instance ToByteString CaptionDestinationType
instance ToQuery      CaptionDestinationType
instance ToHeader     CaptionDestinationType

instance ToJSON CaptionDestinationType where
    toJSON = toJSONText

instance FromJSON CaptionDestinationType where
    parseJSON = parseJSONText "CaptionDestinationType"

-- | Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
data CaptionSourceType
  = CSTAncillary
  | CSTDvbSub
  | CSTEmbedded
  | CSTNullSource
  | CSTScc
  | CSTSrt
  | CSTStl
  | CSTTeletext
  | CSTTtml
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CaptionSourceType where
    parser = takeLowerText >>= \case
        "ancillary" -> pure CSTAncillary
        "dvb_sub" -> pure CSTDvbSub
        "embedded" -> pure CSTEmbedded
        "null_source" -> pure CSTNullSource
        "scc" -> pure CSTScc
        "srt" -> pure CSTSrt
        "stl" -> pure CSTStl
        "teletext" -> pure CSTTeletext
        "ttml" -> pure CSTTtml
        e -> fromTextError $ "Failure parsing CaptionSourceType from value: '" <> e
           <> "'. Accepted values: ancillary, dvb_sub, embedded, null_source, scc, srt, stl, teletext, ttml"

instance ToText CaptionSourceType where
    toText = \case
        CSTAncillary -> "ANCILLARY"
        CSTDvbSub -> "DVB_SUB"
        CSTEmbedded -> "EMBEDDED"
        CSTNullSource -> "NULL_SOURCE"
        CSTScc -> "SCC"
        CSTSrt -> "SRT"
        CSTStl -> "STL"
        CSTTeletext -> "TELETEXT"
        CSTTtml -> "TTML"

instance Hashable     CaptionSourceType
instance NFData       CaptionSourceType
instance ToByteString CaptionSourceType
instance ToQuery      CaptionSourceType
instance ToHeader     CaptionSourceType

instance ToJSON CaptionSourceType where
    toJSON = toJSONText

instance FromJSON CaptionSourceType where
    parseJSON = parseJSONText "CaptionSourceType"

-- | Enable Insert color metadata (ColorMetadata) to include color metadata in this output. This setting is enabled by default.
data ColorMetadata
  = Ignore
  | Insert
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ColorMetadata where
    parser = takeLowerText >>= \case
        "ignore" -> pure Ignore
        "insert" -> pure Insert
        e -> fromTextError $ "Failure parsing ColorMetadata from value: '" <> e
           <> "'. Accepted values: ignore, insert"

instance ToText ColorMetadata where
    toText = \case
        Ignore -> "IGNORE"
        Insert -> "INSERT"

instance Hashable     ColorMetadata
instance NFData       ColorMetadata
instance ToByteString ColorMetadata
instance ToQuery      ColorMetadata
instance ToHeader     ColorMetadata

instance ToJSON ColorMetadata where
    toJSON = toJSONText

instance FromJSON ColorMetadata where
    parseJSON = parseJSONText "ColorMetadata"

-- | Specifies the colorspace of an input. This setting works in tandem with "Color Corrector":#color_corrector > color_space_conversion to determine if any conversion will be performed.
data ColorSpace
  = Follow
  | HDR10
  | Hlg2020
  | Rec601
  | Rec709
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ColorSpace where
    parser = takeLowerText >>= \case
        "follow" -> pure Follow
        "hdr10" -> pure HDR10
        "hlg_2020" -> pure Hlg2020
        "rec_601" -> pure Rec601
        "rec_709" -> pure Rec709
        e -> fromTextError $ "Failure parsing ColorSpace from value: '" <> e
           <> "'. Accepted values: follow, hdr10, hlg_2020, rec_601, rec_709"

instance ToText ColorSpace where
    toText = \case
        Follow -> "FOLLOW"
        HDR10 -> "HDR10"
        Hlg2020 -> "HLG_2020"
        Rec601 -> "REC_601"
        Rec709 -> "REC_709"

instance Hashable     ColorSpace
instance NFData       ColorSpace
instance ToByteString ColorSpace
instance ToQuery      ColorSpace
instance ToHeader     ColorSpace

instance ToJSON ColorSpace where
    toJSON = toJSONText

instance FromJSON ColorSpace where
    parseJSON = parseJSONText "ColorSpace"

-- | Determines if colorspace conversion will be performed. If set to _None_, no conversion will be performed. If _Force 601_ or _Force 709_ are selected, conversion will be performed for inputs with differing colorspaces. An input's colorspace can be specified explicitly in the "Video Selector":#inputs-video_selector if necessary.
data ColorSpaceConversion
  = CSCForce601
  | CSCForce709
  | CSCForceHDR10
  | CSCForceHlg2020
  | CSCNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ColorSpaceConversion where
    parser = takeLowerText >>= \case
        "force_601" -> pure CSCForce601
        "force_709" -> pure CSCForce709
        "force_hdr10" -> pure CSCForceHDR10
        "force_hlg_2020" -> pure CSCForceHlg2020
        "none" -> pure CSCNone
        e -> fromTextError $ "Failure parsing ColorSpaceConversion from value: '" <> e
           <> "'. Accepted values: force_601, force_709, force_hdr10, force_hlg_2020, none"

instance ToText ColorSpaceConversion where
    toText = \case
        CSCForce601 -> "FORCE_601"
        CSCForce709 -> "FORCE_709"
        CSCForceHDR10 -> "FORCE_HDR10"
        CSCForceHlg2020 -> "FORCE_HLG_2020"
        CSCNone -> "NONE"

instance Hashable     ColorSpaceConversion
instance NFData       ColorSpaceConversion
instance ToByteString ColorSpaceConversion
instance ToQuery      ColorSpaceConversion
instance ToHeader     ColorSpaceConversion

instance ToJSON ColorSpaceConversion where
    toJSON = toJSONText

instance FromJSON ColorSpaceConversion where
    parseJSON = parseJSONText "ColorSpaceConversion"

-- | There are two sources for color metadata, the input file and the job configuration. This enum controls which takes precedence. FORCE: System will use color metadata supplied by user, if any. If the user does not supply color metadata the system will use data from the source. FALLBACK: System will use color metadata from the source. If source has no color metadata, the system will use user-supplied color metadata values if available.
data ColorSpaceUsage
  = Fallback
  | Force
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ColorSpaceUsage where
    parser = takeLowerText >>= \case
        "fallback" -> pure Fallback
        "force" -> pure Force
        e -> fromTextError $ "Failure parsing ColorSpaceUsage from value: '" <> e
           <> "'. Accepted values: fallback, force"

instance ToText ColorSpaceUsage where
    toText = \case
        Fallback -> "FALLBACK"
        Force -> "FORCE"

instance Hashable     ColorSpaceUsage
instance NFData       ColorSpaceUsage
instance ToByteString ColorSpaceUsage
instance ToQuery      ColorSpaceUsage
instance ToHeader     ColorSpaceUsage

instance ToJSON ColorSpaceUsage where
    toJSON = toJSONText

instance FromJSON ColorSpaceUsage where
    parseJSON = parseJSONText "ColorSpaceUsage"

-- | Container for this output. Some containers require a container settings object. If not specified, the default object will be created.
data ContainerType
  = F4V
  | Ismv
  | M2TS
  | M3U8
  | MP4
  | Mov
  | Mpd
  | Mxf
  | Raw
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContainerType where
    parser = takeLowerText >>= \case
        "f4v" -> pure F4V
        "ismv" -> pure Ismv
        "m2ts" -> pure M2TS
        "m3u8" -> pure M3U8
        "mp4" -> pure MP4
        "mov" -> pure Mov
        "mpd" -> pure Mpd
        "mxf" -> pure Mxf
        "raw" -> pure Raw
        e -> fromTextError $ "Failure parsing ContainerType from value: '" <> e
           <> "'. Accepted values: f4v, ismv, m2ts, m3u8, mp4, mov, mpd, mxf, raw"

instance ToText ContainerType where
    toText = \case
        F4V -> "F4V"
        Ismv -> "ISMV"
        M2TS -> "M2TS"
        M3U8 -> "M3U8"
        MP4 -> "MP4"
        Mov -> "MOV"
        Mpd -> "MPD"
        Mxf -> "MXF"
        Raw -> "RAW"

instance Hashable     ContainerType
instance NFData       ContainerType
instance ToByteString ContainerType
instance ToQuery      ContainerType
instance ToHeader     ContainerType

instance ToJSON ContainerType where
    toJSON = toJSONText

instance FromJSON ContainerType where
    parseJSON = parseJSONText "ContainerType"

-- | Supports HbbTV specification as indicated
data DashIsoHbbtvCompliance
  = DIHCHbbtv15
  | DIHCNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DashIsoHbbtvCompliance where
    parser = takeLowerText >>= \case
        "hbbtv_1_5" -> pure DIHCHbbtv15
        "none" -> pure DIHCNone
        e -> fromTextError $ "Failure parsing DashIsoHbbtvCompliance from value: '" <> e
           <> "'. Accepted values: hbbtv_1_5, none"

instance ToText DashIsoHbbtvCompliance where
    toText = \case
        DIHCHbbtv15 -> "HBBTV_1_5"
        DIHCNone -> "NONE"

instance Hashable     DashIsoHbbtvCompliance
instance NFData       DashIsoHbbtvCompliance
instance ToByteString DashIsoHbbtvCompliance
instance ToQuery      DashIsoHbbtvCompliance
instance ToHeader     DashIsoHbbtvCompliance

instance ToJSON DashIsoHbbtvCompliance where
    toJSON = toJSONText

instance FromJSON DashIsoHbbtvCompliance where
    parseJSON = parseJSONText "DashIsoHbbtvCompliance"

-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
data DashIsoSegmentControl
  = DISCSegmentedFiles
  | DISCSingleFile
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DashIsoSegmentControl where
    parser = takeLowerText >>= \case
        "segmented_files" -> pure DISCSegmentedFiles
        "single_file" -> pure DISCSingleFile
        e -> fromTextError $ "Failure parsing DashIsoSegmentControl from value: '" <> e
           <> "'. Accepted values: segmented_files, single_file"

instance ToText DashIsoSegmentControl where
    toText = \case
        DISCSegmentedFiles -> "SEGMENTED_FILES"
        DISCSingleFile -> "SINGLE_FILE"

instance Hashable     DashIsoSegmentControl
instance NFData       DashIsoSegmentControl
instance ToByteString DashIsoSegmentControl
instance ToQuery      DashIsoSegmentControl
instance ToHeader     DashIsoSegmentControl

instance ToJSON DashIsoSegmentControl where
    toJSON = toJSONText

instance FromJSON DashIsoSegmentControl where
    parseJSON = parseJSONText "DashIsoSegmentControl"

-- | Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.
data DeinterlaceAlgorithm
  = DABlend
  | DABlendTicker
  | DAInterpolate
  | DAInterpolateTicker
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeinterlaceAlgorithm where
    parser = takeLowerText >>= \case
        "blend" -> pure DABlend
        "blend_ticker" -> pure DABlendTicker
        "interpolate" -> pure DAInterpolate
        "interpolate_ticker" -> pure DAInterpolateTicker
        e -> fromTextError $ "Failure parsing DeinterlaceAlgorithm from value: '" <> e
           <> "'. Accepted values: blend, blend_ticker, interpolate, interpolate_ticker"

instance ToText DeinterlaceAlgorithm where
    toText = \case
        DABlend -> "BLEND"
        DABlendTicker -> "BLEND_TICKER"
        DAInterpolate -> "INTERPOLATE"
        DAInterpolateTicker -> "INTERPOLATE_TICKER"

instance Hashable     DeinterlaceAlgorithm
instance NFData       DeinterlaceAlgorithm
instance ToByteString DeinterlaceAlgorithm
instance ToQuery      DeinterlaceAlgorithm
instance ToHeader     DeinterlaceAlgorithm

instance ToJSON DeinterlaceAlgorithm where
    toJSON = toJSONText

instance FromJSON DeinterlaceAlgorithm where
    parseJSON = parseJSONText "DeinterlaceAlgorithm"

-- | - When set to NORMAL (default), the deinterlacer does not convert frames that are tagged  in metadata as progressive. It will only convert those that are tagged as some other type. - When set to FORCE_ALL_FRAMES, the deinterlacer converts every frame to progressive - even those that are already tagged as progressive. Turn Force mode on only if there is  a good chance that the metadata has tagged frames as progressive when they are not  progressive. Do not turn on otherwise; processing frames that are already progressive  into progressive will probably result in lower quality video.
data DeinterlacerControl
  = ForceAllFrames
  | Normal
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeinterlacerControl where
    parser = takeLowerText >>= \case
        "force_all_frames" -> pure ForceAllFrames
        "normal" -> pure Normal
        e -> fromTextError $ "Failure parsing DeinterlacerControl from value: '" <> e
           <> "'. Accepted values: force_all_frames, normal"

instance ToText DeinterlacerControl where
    toText = \case
        ForceAllFrames -> "FORCE_ALL_FRAMES"
        Normal -> "NORMAL"

instance Hashable     DeinterlacerControl
instance NFData       DeinterlacerControl
instance ToByteString DeinterlacerControl
instance ToQuery      DeinterlacerControl
instance ToHeader     DeinterlacerControl

instance ToJSON DeinterlacerControl where
    toJSON = toJSONText

instance FromJSON DeinterlacerControl where
    parseJSON = parseJSONText "DeinterlacerControl"

-- | Use Deinterlacer (DeinterlaceMode) to choose how the service will do deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced to progressive. - Inverse telecine converts Hard Telecine 29.97i to progressive 23.976p. - Adaptive auto-detects and converts to progressive.
data DeinterlacerMode
  = Adaptive
  | Deinterlace
  | InverseTelecine
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeinterlacerMode where
    parser = takeLowerText >>= \case
        "adaptive" -> pure Adaptive
        "deinterlace" -> pure Deinterlace
        "inverse_telecine" -> pure InverseTelecine
        e -> fromTextError $ "Failure parsing DeinterlacerMode from value: '" <> e
           <> "'. Accepted values: adaptive, deinterlace, inverse_telecine"

instance ToText DeinterlacerMode where
    toText = \case
        Adaptive -> "ADAPTIVE"
        Deinterlace -> "DEINTERLACE"
        InverseTelecine -> "INVERSE_TELECINE"

instance Hashable     DeinterlacerMode
instance NFData       DeinterlacerMode
instance ToByteString DeinterlacerMode
instance ToQuery      DeinterlacerMode
instance ToHeader     DeinterlacerMode

instance ToJSON DeinterlacerMode where
    toJSON = toJSONText

instance FromJSON DeinterlacerMode where
    parseJSON = parseJSONText "DeinterlacerMode"

-- | Applies only to 29.97 fps outputs. When this feature is enabled, the service will use drop-frame timecode on outputs. If it is not possible to use drop-frame timecode, the system will fall back to non-drop-frame. This setting is enabled by default when Timecode insertion (TimecodeInsertion) is enabled.
data DropFrameTimecode
  = DFTDisabled
  | DFTEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DropFrameTimecode where
    parser = takeLowerText >>= \case
        "disabled" -> pure DFTDisabled
        "enabled" -> pure DFTEnabled
        e -> fromTextError $ "Failure parsing DropFrameTimecode from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText DropFrameTimecode where
    toText = \case
        DFTDisabled -> "DISABLED"
        DFTEnabled -> "ENABLED"

instance Hashable     DropFrameTimecode
instance NFData       DropFrameTimecode
instance ToByteString DropFrameTimecode
instance ToQuery      DropFrameTimecode
instance ToHeader     DropFrameTimecode

instance ToJSON DropFrameTimecode where
    toJSON = toJSONText

instance FromJSON DropFrameTimecode where
    parseJSON = parseJSONText "DropFrameTimecode"

-- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
data DvbSubtitleAlignment
  = Centered
  | Left'
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DvbSubtitleAlignment where
    parser = takeLowerText >>= \case
        "centered" -> pure Centered
        "left" -> pure Left'
        e -> fromTextError $ "Failure parsing DvbSubtitleAlignment from value: '" <> e
           <> "'. Accepted values: centered, left"

instance ToText DvbSubtitleAlignment where
    toText = \case
        Centered -> "CENTERED"
        Left' -> "LEFT"

instance Hashable     DvbSubtitleAlignment
instance NFData       DvbSubtitleAlignment
instance ToByteString DvbSubtitleAlignment
instance ToQuery      DvbSubtitleAlignment
instance ToHeader     DvbSubtitleAlignment

instance ToJSON DvbSubtitleAlignment where
    toJSON = toJSONText

instance FromJSON DvbSubtitleAlignment where
    parseJSON = parseJSONText "DvbSubtitleAlignment"

-- | Specifies the color of the rectangle behind the captions.
--
-- All burn-in and DVB-Sub font settings must match.
data DvbSubtitleBackgroundColor
  = DSBCBlack
  | DSBCNone
  | DSBCWhite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DvbSubtitleBackgroundColor where
    parser = takeLowerText >>= \case
        "black" -> pure DSBCBlack
        "none" -> pure DSBCNone
        "white" -> pure DSBCWhite
        e -> fromTextError $ "Failure parsing DvbSubtitleBackgroundColor from value: '" <> e
           <> "'. Accepted values: black, none, white"

instance ToText DvbSubtitleBackgroundColor where
    toText = \case
        DSBCBlack -> "BLACK"
        DSBCNone -> "NONE"
        DSBCWhite -> "WHITE"

instance Hashable     DvbSubtitleBackgroundColor
instance NFData       DvbSubtitleBackgroundColor
instance ToByteString DvbSubtitleBackgroundColor
instance ToQuery      DvbSubtitleBackgroundColor
instance ToHeader     DvbSubtitleBackgroundColor

instance ToJSON DvbSubtitleBackgroundColor where
    toJSON = toJSONText

instance FromJSON DvbSubtitleBackgroundColor where
    parseJSON = parseJSONText "DvbSubtitleBackgroundColor"

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
data DvbSubtitleFontColor
  = DSFCBlack
  | DSFCBlue
  | DSFCGreen
  | DSFCRed
  | DSFCWhite
  | DSFCYellow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DvbSubtitleFontColor where
    parser = takeLowerText >>= \case
        "black" -> pure DSFCBlack
        "blue" -> pure DSFCBlue
        "green" -> pure DSFCGreen
        "red" -> pure DSFCRed
        "white" -> pure DSFCWhite
        "yellow" -> pure DSFCYellow
        e -> fromTextError $ "Failure parsing DvbSubtitleFontColor from value: '" <> e
           <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText DvbSubtitleFontColor where
    toText = \case
        DSFCBlack -> "BLACK"
        DSFCBlue -> "BLUE"
        DSFCGreen -> "GREEN"
        DSFCRed -> "RED"
        DSFCWhite -> "WHITE"
        DSFCYellow -> "YELLOW"

instance Hashable     DvbSubtitleFontColor
instance NFData       DvbSubtitleFontColor
instance ToByteString DvbSubtitleFontColor
instance ToQuery      DvbSubtitleFontColor
instance ToHeader     DvbSubtitleFontColor

instance ToJSON DvbSubtitleFontColor where
    toJSON = toJSONText

instance FromJSON DvbSubtitleFontColor where
    parseJSON = parseJSONText "DvbSubtitleFontColor"

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
data DvbSubtitleOutlineColor
  = Black
  | Blue
  | Green
  | Red
  | White
  | Yellow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DvbSubtitleOutlineColor where
    parser = takeLowerText >>= \case
        "black" -> pure Black
        "blue" -> pure Blue
        "green" -> pure Green
        "red" -> pure Red
        "white" -> pure White
        "yellow" -> pure Yellow
        e -> fromTextError $ "Failure parsing DvbSubtitleOutlineColor from value: '" <> e
           <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText DvbSubtitleOutlineColor where
    toText = \case
        Black -> "BLACK"
        Blue -> "BLUE"
        Green -> "GREEN"
        Red -> "RED"
        White -> "WHITE"
        Yellow -> "YELLOW"

instance Hashable     DvbSubtitleOutlineColor
instance NFData       DvbSubtitleOutlineColor
instance ToByteString DvbSubtitleOutlineColor
instance ToQuery      DvbSubtitleOutlineColor
instance ToHeader     DvbSubtitleOutlineColor

instance ToJSON DvbSubtitleOutlineColor where
    toJSON = toJSONText

instance FromJSON DvbSubtitleOutlineColor where
    parseJSON = parseJSONText "DvbSubtitleOutlineColor"

-- | Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
data DvbSubtitleShadowColor
  = DSSCBlack
  | DSSCNone
  | DSSCWhite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DvbSubtitleShadowColor where
    parser = takeLowerText >>= \case
        "black" -> pure DSSCBlack
        "none" -> pure DSSCNone
        "white" -> pure DSSCWhite
        e -> fromTextError $ "Failure parsing DvbSubtitleShadowColor from value: '" <> e
           <> "'. Accepted values: black, none, white"

instance ToText DvbSubtitleShadowColor where
    toText = \case
        DSSCBlack -> "BLACK"
        DSSCNone -> "NONE"
        DSSCWhite -> "WHITE"

instance Hashable     DvbSubtitleShadowColor
instance NFData       DvbSubtitleShadowColor
instance ToByteString DvbSubtitleShadowColor
instance ToQuery      DvbSubtitleShadowColor
instance ToHeader     DvbSubtitleShadowColor

instance ToJSON DvbSubtitleShadowColor where
    toJSON = toJSONText

instance FromJSON DvbSubtitleShadowColor where
    parseJSON = parseJSONText "DvbSubtitleShadowColor"

-- | Controls whether a fixed grid size or proportional font spacing will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
data DvbSubtitleTeletextSpacing
  = FixedGrid
  | Proportional
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DvbSubtitleTeletextSpacing where
    parser = takeLowerText >>= \case
        "fixed_grid" -> pure FixedGrid
        "proportional" -> pure Proportional
        e -> fromTextError $ "Failure parsing DvbSubtitleTeletextSpacing from value: '" <> e
           <> "'. Accepted values: fixed_grid, proportional"

instance ToText DvbSubtitleTeletextSpacing where
    toText = \case
        FixedGrid -> "FIXED_GRID"
        Proportional -> "PROPORTIONAL"

instance Hashable     DvbSubtitleTeletextSpacing
instance NFData       DvbSubtitleTeletextSpacing
instance ToByteString DvbSubtitleTeletextSpacing
instance ToQuery      DvbSubtitleTeletextSpacing
instance ToHeader     DvbSubtitleTeletextSpacing

instance ToJSON DvbSubtitleTeletextSpacing where
    toJSON = toJSONText

instance FromJSON DvbSubtitleTeletextSpacing where
    parseJSON = parseJSONText "DvbSubtitleTeletextSpacing"

-- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
data Eac3AttenuationControl
  = EACAttenuate3DB
  | EACNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3AttenuationControl where
    parser = takeLowerText >>= \case
        "attenuate_3_db" -> pure EACAttenuate3DB
        "none" -> pure EACNone
        e -> fromTextError $ "Failure parsing Eac3AttenuationControl from value: '" <> e
           <> "'. Accepted values: attenuate_3_db, none"

instance ToText Eac3AttenuationControl where
    toText = \case
        EACAttenuate3DB -> "ATTENUATE_3_DB"
        EACNone -> "NONE"

instance Hashable     Eac3AttenuationControl
instance NFData       Eac3AttenuationControl
instance ToByteString Eac3AttenuationControl
instance ToQuery      Eac3AttenuationControl
instance ToHeader     Eac3AttenuationControl

instance ToJSON Eac3AttenuationControl where
    toJSON = toJSONText

instance FromJSON Eac3AttenuationControl where
    parseJSON = parseJSONText "Eac3AttenuationControl"

-- | Specifies the "Bitstream Mode" (bsmod) for the emitted E-AC-3 stream. See ATSC A/52-2012 (Annex E) for background on these values.
data Eac3BitstreamMode
  = EBMCommentary
  | EBMCompleteMain
  | EBMEmergency
  | EBMHearingImpaired
  | EBMVisuallyImpaired
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3BitstreamMode where
    parser = takeLowerText >>= \case
        "commentary" -> pure EBMCommentary
        "complete_main" -> pure EBMCompleteMain
        "emergency" -> pure EBMEmergency
        "hearing_impaired" -> pure EBMHearingImpaired
        "visually_impaired" -> pure EBMVisuallyImpaired
        e -> fromTextError $ "Failure parsing Eac3BitstreamMode from value: '" <> e
           <> "'. Accepted values: commentary, complete_main, emergency, hearing_impaired, visually_impaired"

instance ToText Eac3BitstreamMode where
    toText = \case
        EBMCommentary -> "COMMENTARY"
        EBMCompleteMain -> "COMPLETE_MAIN"
        EBMEmergency -> "EMERGENCY"
        EBMHearingImpaired -> "HEARING_IMPAIRED"
        EBMVisuallyImpaired -> "VISUALLY_IMPAIRED"

instance Hashable     Eac3BitstreamMode
instance NFData       Eac3BitstreamMode
instance ToByteString Eac3BitstreamMode
instance ToQuery      Eac3BitstreamMode
instance ToHeader     Eac3BitstreamMode

instance ToJSON Eac3BitstreamMode where
    toJSON = toJSONText

instance FromJSON Eac3BitstreamMode where
    parseJSON = parseJSONText "Eac3BitstreamMode"

-- | Dolby Digital Plus coding mode. Determines number of channels.
data Eac3CodingMode
  = ECMCodingMode10
  | ECMCodingMode20
  | ECMCodingMode32
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3CodingMode where
    parser = takeLowerText >>= \case
        "coding_mode_1_0" -> pure ECMCodingMode10
        "coding_mode_2_0" -> pure ECMCodingMode20
        "coding_mode_3_2" -> pure ECMCodingMode32
        e -> fromTextError $ "Failure parsing Eac3CodingMode from value: '" <> e
           <> "'. Accepted values: coding_mode_1_0, coding_mode_2_0, coding_mode_3_2"

instance ToText Eac3CodingMode where
    toText = \case
        ECMCodingMode10 -> "CODING_MODE_1_0"
        ECMCodingMode20 -> "CODING_MODE_2_0"
        ECMCodingMode32 -> "CODING_MODE_3_2"

instance Hashable     Eac3CodingMode
instance NFData       Eac3CodingMode
instance ToByteString Eac3CodingMode
instance ToQuery      Eac3CodingMode
instance ToHeader     Eac3CodingMode

instance ToJSON Eac3CodingMode where
    toJSON = toJSONText

instance FromJSON Eac3CodingMode where
    parseJSON = parseJSONText "Eac3CodingMode"

-- | Activates a DC highpass filter for all input channels.
data Eac3DcFilter
  = EDFDisabled
  | EDFEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3DcFilter where
    parser = takeLowerText >>= \case
        "disabled" -> pure EDFDisabled
        "enabled" -> pure EDFEnabled
        e -> fromTextError $ "Failure parsing Eac3DcFilter from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText Eac3DcFilter where
    toText = \case
        EDFDisabled -> "DISABLED"
        EDFEnabled -> "ENABLED"

instance Hashable     Eac3DcFilter
instance NFData       Eac3DcFilter
instance ToByteString Eac3DcFilter
instance ToQuery      Eac3DcFilter
instance ToHeader     Eac3DcFilter

instance ToJSON Eac3DcFilter where
    toJSON = toJSONText

instance FromJSON Eac3DcFilter where
    parseJSON = parseJSONText "Eac3DcFilter"

-- | Enables Dynamic Range Compression that restricts the absolute peak level for a signal.
data Eac3DynamicRangeCompressionLine
  = EDRCLFilmLight
  | EDRCLFilmStandard
  | EDRCLMusicLight
  | EDRCLMusicStandard
  | EDRCLNone
  | EDRCLSpeech
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3DynamicRangeCompressionLine where
    parser = takeLowerText >>= \case
        "film_light" -> pure EDRCLFilmLight
        "film_standard" -> pure EDRCLFilmStandard
        "music_light" -> pure EDRCLMusicLight
        "music_standard" -> pure EDRCLMusicStandard
        "none" -> pure EDRCLNone
        "speech" -> pure EDRCLSpeech
        e -> fromTextError $ "Failure parsing Eac3DynamicRangeCompressionLine from value: '" <> e
           <> "'. Accepted values: film_light, film_standard, music_light, music_standard, none, speech"

instance ToText Eac3DynamicRangeCompressionLine where
    toText = \case
        EDRCLFilmLight -> "FILM_LIGHT"
        EDRCLFilmStandard -> "FILM_STANDARD"
        EDRCLMusicLight -> "MUSIC_LIGHT"
        EDRCLMusicStandard -> "MUSIC_STANDARD"
        EDRCLNone -> "NONE"
        EDRCLSpeech -> "SPEECH"

instance Hashable     Eac3DynamicRangeCompressionLine
instance NFData       Eac3DynamicRangeCompressionLine
instance ToByteString Eac3DynamicRangeCompressionLine
instance ToQuery      Eac3DynamicRangeCompressionLine
instance ToHeader     Eac3DynamicRangeCompressionLine

instance ToJSON Eac3DynamicRangeCompressionLine where
    toJSON = toJSONText

instance FromJSON Eac3DynamicRangeCompressionLine where
    parseJSON = parseJSONText "Eac3DynamicRangeCompressionLine"

-- | Enables Heavy Dynamic Range Compression, ensures that the instantaneous signal peaks do not exceed specified levels.
data Eac3DynamicRangeCompressionRf
  = EDRCRFilmLight
  | EDRCRFilmStandard
  | EDRCRMusicLight
  | EDRCRMusicStandard
  | EDRCRNone
  | EDRCRSpeech
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3DynamicRangeCompressionRf where
    parser = takeLowerText >>= \case
        "film_light" -> pure EDRCRFilmLight
        "film_standard" -> pure EDRCRFilmStandard
        "music_light" -> pure EDRCRMusicLight
        "music_standard" -> pure EDRCRMusicStandard
        "none" -> pure EDRCRNone
        "speech" -> pure EDRCRSpeech
        e -> fromTextError $ "Failure parsing Eac3DynamicRangeCompressionRf from value: '" <> e
           <> "'. Accepted values: film_light, film_standard, music_light, music_standard, none, speech"

instance ToText Eac3DynamicRangeCompressionRf where
    toText = \case
        EDRCRFilmLight -> "FILM_LIGHT"
        EDRCRFilmStandard -> "FILM_STANDARD"
        EDRCRMusicLight -> "MUSIC_LIGHT"
        EDRCRMusicStandard -> "MUSIC_STANDARD"
        EDRCRNone -> "NONE"
        EDRCRSpeech -> "SPEECH"

instance Hashable     Eac3DynamicRangeCompressionRf
instance NFData       Eac3DynamicRangeCompressionRf
instance ToByteString Eac3DynamicRangeCompressionRf
instance ToQuery      Eac3DynamicRangeCompressionRf
instance ToHeader     Eac3DynamicRangeCompressionRf

instance ToJSON Eac3DynamicRangeCompressionRf where
    toJSON = toJSONText

instance FromJSON Eac3DynamicRangeCompressionRf where
    parseJSON = parseJSONText "Eac3DynamicRangeCompressionRf"

-- | When encoding 3/2 audio, controls whether the LFE channel is enabled
data Eac3LfeControl
  = Lfe
  | NoLfe
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3LfeControl where
    parser = takeLowerText >>= \case
        "lfe" -> pure Lfe
        "no_lfe" -> pure NoLfe
        e -> fromTextError $ "Failure parsing Eac3LfeControl from value: '" <> e
           <> "'. Accepted values: lfe, no_lfe"

instance ToText Eac3LfeControl where
    toText = \case
        Lfe -> "LFE"
        NoLfe -> "NO_LFE"

instance Hashable     Eac3LfeControl
instance NFData       Eac3LfeControl
instance ToByteString Eac3LfeControl
instance ToQuery      Eac3LfeControl
instance ToHeader     Eac3LfeControl

instance ToJSON Eac3LfeControl where
    toJSON = toJSONText

instance FromJSON Eac3LfeControl where
    parseJSON = parseJSONText "Eac3LfeControl"

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
data Eac3LfeFilter
  = ELFDisabled
  | ELFEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3LfeFilter where
    parser = takeLowerText >>= \case
        "disabled" -> pure ELFDisabled
        "enabled" -> pure ELFEnabled
        e -> fromTextError $ "Failure parsing Eac3LfeFilter from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText Eac3LfeFilter where
    toText = \case
        ELFDisabled -> "DISABLED"
        ELFEnabled -> "ENABLED"

instance Hashable     Eac3LfeFilter
instance NFData       Eac3LfeFilter
instance ToByteString Eac3LfeFilter
instance ToQuery      Eac3LfeFilter
instance ToHeader     Eac3LfeFilter

instance ToJSON Eac3LfeFilter where
    toJSON = toJSONText

instance FromJSON Eac3LfeFilter where
    parseJSON = parseJSONText "Eac3LfeFilter"

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
data Eac3MetadataControl
  = EMCFollowInput
  | EMCUseConfigured
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3MetadataControl where
    parser = takeLowerText >>= \case
        "follow_input" -> pure EMCFollowInput
        "use_configured" -> pure EMCUseConfigured
        e -> fromTextError $ "Failure parsing Eac3MetadataControl from value: '" <> e
           <> "'. Accepted values: follow_input, use_configured"

instance ToText Eac3MetadataControl where
    toText = \case
        EMCFollowInput -> "FOLLOW_INPUT"
        EMCUseConfigured -> "USE_CONFIGURED"

instance Hashable     Eac3MetadataControl
instance NFData       Eac3MetadataControl
instance ToByteString Eac3MetadataControl
instance ToQuery      Eac3MetadataControl
instance ToHeader     Eac3MetadataControl

instance ToJSON Eac3MetadataControl where
    toJSON = toJSONText

instance FromJSON Eac3MetadataControl where
    parseJSON = parseJSONText "Eac3MetadataControl"

-- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it is present on the input. this detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
data Eac3PassthroughControl
  = NoPassthrough
  | WhenPossible
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3PassthroughControl where
    parser = takeLowerText >>= \case
        "no_passthrough" -> pure NoPassthrough
        "when_possible" -> pure WhenPossible
        e -> fromTextError $ "Failure parsing Eac3PassthroughControl from value: '" <> e
           <> "'. Accepted values: no_passthrough, when_possible"

instance ToText Eac3PassthroughControl where
    toText = \case
        NoPassthrough -> "NO_PASSTHROUGH"
        WhenPossible -> "WHEN_POSSIBLE"

instance Hashable     Eac3PassthroughControl
instance NFData       Eac3PassthroughControl
instance ToByteString Eac3PassthroughControl
instance ToQuery      Eac3PassthroughControl
instance ToHeader     Eac3PassthroughControl

instance ToJSON Eac3PassthroughControl where
    toJSON = toJSONText

instance FromJSON Eac3PassthroughControl where
    parseJSON = parseJSONText "Eac3PassthroughControl"

-- | Controls the amount of phase-shift applied to the surround channels. Only used for 3/2 coding mode.
data Eac3PhaseControl
  = NoShift
  | Shift90Degrees
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3PhaseControl where
    parser = takeLowerText >>= \case
        "no_shift" -> pure NoShift
        "shift_90_degrees" -> pure Shift90Degrees
        e -> fromTextError $ "Failure parsing Eac3PhaseControl from value: '" <> e
           <> "'. Accepted values: no_shift, shift_90_degrees"

instance ToText Eac3PhaseControl where
    toText = \case
        NoShift -> "NO_SHIFT"
        Shift90Degrees -> "SHIFT_90_DEGREES"

instance Hashable     Eac3PhaseControl
instance NFData       Eac3PhaseControl
instance ToByteString Eac3PhaseControl
instance ToQuery      Eac3PhaseControl
instance ToHeader     Eac3PhaseControl

instance ToJSON Eac3PhaseControl where
    toJSON = toJSONText

instance FromJSON Eac3PhaseControl where
    parseJSON = parseJSONText "Eac3PhaseControl"

-- | Stereo downmix preference. Only used for 3/2 coding mode.
data Eac3StereoDownmix
  = DPL2
  | LoRo
  | LtRt
  | NotIndicated
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3StereoDownmix where
    parser = takeLowerText >>= \case
        "dpl2" -> pure DPL2
        "lo_ro" -> pure LoRo
        "lt_rt" -> pure LtRt
        "not_indicated" -> pure NotIndicated
        e -> fromTextError $ "Failure parsing Eac3StereoDownmix from value: '" <> e
           <> "'. Accepted values: dpl2, lo_ro, lt_rt, not_indicated"

instance ToText Eac3StereoDownmix where
    toText = \case
        DPL2 -> "DPL2"
        LoRo -> "LO_RO"
        LtRt -> "LT_RT"
        NotIndicated -> "NOT_INDICATED"

instance Hashable     Eac3StereoDownmix
instance NFData       Eac3StereoDownmix
instance ToByteString Eac3StereoDownmix
instance ToQuery      Eac3StereoDownmix
instance ToHeader     Eac3StereoDownmix

instance ToJSON Eac3StereoDownmix where
    toJSON = toJSONText

instance FromJSON Eac3StereoDownmix where
    parseJSON = parseJSONText "Eac3StereoDownmix"

-- | When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
data Eac3SurroundExMode
  = ESEMDisabled
  | ESEMEnabled
  | ESEMNotIndicated
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3SurroundExMode where
    parser = takeLowerText >>= \case
        "disabled" -> pure ESEMDisabled
        "enabled" -> pure ESEMEnabled
        "not_indicated" -> pure ESEMNotIndicated
        e -> fromTextError $ "Failure parsing Eac3SurroundExMode from value: '" <> e
           <> "'. Accepted values: disabled, enabled, not_indicated"

instance ToText Eac3SurroundExMode where
    toText = \case
        ESEMDisabled -> "DISABLED"
        ESEMEnabled -> "ENABLED"
        ESEMNotIndicated -> "NOT_INDICATED"

instance Hashable     Eac3SurroundExMode
instance NFData       Eac3SurroundExMode
instance ToByteString Eac3SurroundExMode
instance ToQuery      Eac3SurroundExMode
instance ToHeader     Eac3SurroundExMode

instance ToJSON Eac3SurroundExMode where
    toJSON = toJSONText

instance FromJSON Eac3SurroundExMode where
    parseJSON = parseJSONText "Eac3SurroundExMode"

-- | When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
data Eac3SurroundMode
  = ESMDisabled
  | ESMEnabled
  | ESMNotIndicated
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3SurroundMode where
    parser = takeLowerText >>= \case
        "disabled" -> pure ESMDisabled
        "enabled" -> pure ESMEnabled
        "not_indicated" -> pure ESMNotIndicated
        e -> fromTextError $ "Failure parsing Eac3SurroundMode from value: '" <> e
           <> "'. Accepted values: disabled, enabled, not_indicated"

instance ToText Eac3SurroundMode where
    toText = \case
        ESMDisabled -> "DISABLED"
        ESMEnabled -> "ENABLED"
        ESMNotIndicated -> "NOT_INDICATED"

instance Hashable     Eac3SurroundMode
instance NFData       Eac3SurroundMode
instance ToByteString Eac3SurroundMode
instance ToQuery      Eac3SurroundMode
instance ToHeader     Eac3SurroundMode

instance ToJSON Eac3SurroundMode where
    toJSON = toJSONText

instance FromJSON Eac3SurroundMode where
    parseJSON = parseJSONText "Eac3SurroundMode"

-- | When set to UPCONVERT, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
data EmbeddedConvert608To708
  = ECTDisabled
  | ECTUpconvert
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EmbeddedConvert608To708 where
    parser = takeLowerText >>= \case
        "disabled" -> pure ECTDisabled
        "upconvert" -> pure ECTUpconvert
        e -> fromTextError $ "Failure parsing EmbeddedConvert608To708 from value: '" <> e
           <> "'. Accepted values: disabled, upconvert"

instance ToText EmbeddedConvert608To708 where
    toText = \case
        ECTDisabled -> "DISABLED"
        ECTUpconvert -> "UPCONVERT"

instance Hashable     EmbeddedConvert608To708
instance NFData       EmbeddedConvert608To708
instance ToByteString EmbeddedConvert608To708
instance ToQuery      EmbeddedConvert608To708
instance ToHeader     EmbeddedConvert608To708

instance ToJSON EmbeddedConvert608To708 where
    toJSON = toJSONText

instance FromJSON EmbeddedConvert608To708 where
    parseJSON = parseJSONText "EmbeddedConvert608To708"

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
data F4vMoovPlacement
  = FMPNormal
  | FMPProgressiveDownload
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText F4vMoovPlacement where
    parser = takeLowerText >>= \case
        "normal" -> pure FMPNormal
        "progressive_download" -> pure FMPProgressiveDownload
        e -> fromTextError $ "Failure parsing F4vMoovPlacement from value: '" <> e
           <> "'. Accepted values: normal, progressive_download"

instance ToText F4vMoovPlacement where
    toText = \case
        FMPNormal -> "NORMAL"
        FMPProgressiveDownload -> "PROGRESSIVE_DOWNLOAD"

instance Hashable     F4vMoovPlacement
instance NFData       F4vMoovPlacement
instance ToByteString F4vMoovPlacement
instance ToQuery      F4vMoovPlacement
instance ToHeader     F4vMoovPlacement

instance ToJSON F4vMoovPlacement where
    toJSON = toJSONText

instance FromJSON F4vMoovPlacement where
    parseJSON = parseJSONText "F4vMoovPlacement"

-- | If set to UPCONVERT, 608 caption data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
data FileSourceConvert608To708
  = FSCTDisabled
  | FSCTUpconvert
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FileSourceConvert608To708 where
    parser = takeLowerText >>= \case
        "disabled" -> pure FSCTDisabled
        "upconvert" -> pure FSCTUpconvert
        e -> fromTextError $ "Failure parsing FileSourceConvert608To708 from value: '" <> e
           <> "'. Accepted values: disabled, upconvert"

instance ToText FileSourceConvert608To708 where
    toText = \case
        FSCTDisabled -> "DISABLED"
        FSCTUpconvert -> "UPCONVERT"

instance Hashable     FileSourceConvert608To708
instance NFData       FileSourceConvert608To708
instance ToByteString FileSourceConvert608To708
instance ToQuery      FileSourceConvert608To708
instance ToHeader     FileSourceConvert608To708

instance ToJSON FileSourceConvert608To708 where
    toJSON = toJSONText

instance FromJSON FileSourceConvert608To708 where
    parseJSON = parseJSONText "FileSourceConvert608To708"

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
data H264AdaptiveQuantization
  = HAQHigh
  | HAQHigher
  | HAQLow
  | HAQMax
  | HAQMedium
  | HAQOff
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264AdaptiveQuantization where
    parser = takeLowerText >>= \case
        "high" -> pure HAQHigh
        "higher" -> pure HAQHigher
        "low" -> pure HAQLow
        "max" -> pure HAQMax
        "medium" -> pure HAQMedium
        "off" -> pure HAQOff
        e -> fromTextError $ "Failure parsing H264AdaptiveQuantization from value: '" <> e
           <> "'. Accepted values: high, higher, low, max, medium, off"

instance ToText H264AdaptiveQuantization where
    toText = \case
        HAQHigh -> "HIGH"
        HAQHigher -> "HIGHER"
        HAQLow -> "LOW"
        HAQMax -> "MAX"
        HAQMedium -> "MEDIUM"
        HAQOff -> "OFF"

instance Hashable     H264AdaptiveQuantization
instance NFData       H264AdaptiveQuantization
instance ToByteString H264AdaptiveQuantization
instance ToQuery      H264AdaptiveQuantization
instance ToHeader     H264AdaptiveQuantization

instance ToJSON H264AdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H264AdaptiveQuantization where
    parseJSON = parseJSONText "H264AdaptiveQuantization"

-- | H.264 Level.
data H264CodecLevel
  = HCLAuto
  | HCLLevel1
  | HCLLevel11
  | HCLLevel12
  | HCLLevel13
  | HCLLevel2
  | HCLLevel21
  | HCLLevel22
  | HCLLevel3
  | HCLLevel31
  | HCLLevel32
  | HCLLevel4
  | HCLLevel41
  | HCLLevel42
  | HCLLevel5
  | HCLLevel51
  | HCLLevel52
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264CodecLevel where
    parser = takeLowerText >>= \case
        "auto" -> pure HCLAuto
        "level_1" -> pure HCLLevel1
        "level_1_1" -> pure HCLLevel11
        "level_1_2" -> pure HCLLevel12
        "level_1_3" -> pure HCLLevel13
        "level_2" -> pure HCLLevel2
        "level_2_1" -> pure HCLLevel21
        "level_2_2" -> pure HCLLevel22
        "level_3" -> pure HCLLevel3
        "level_3_1" -> pure HCLLevel31
        "level_3_2" -> pure HCLLevel32
        "level_4" -> pure HCLLevel4
        "level_4_1" -> pure HCLLevel41
        "level_4_2" -> pure HCLLevel42
        "level_5" -> pure HCLLevel5
        "level_5_1" -> pure HCLLevel51
        "level_5_2" -> pure HCLLevel52
        e -> fromTextError $ "Failure parsing H264CodecLevel from value: '" <> e
           <> "'. Accepted values: auto, level_1, level_1_1, level_1_2, level_1_3, level_2, level_2_1, level_2_2, level_3, level_3_1, level_3_2, level_4, level_4_1, level_4_2, level_5, level_5_1, level_5_2"

instance ToText H264CodecLevel where
    toText = \case
        HCLAuto -> "AUTO"
        HCLLevel1 -> "LEVEL_1"
        HCLLevel11 -> "LEVEL_1_1"
        HCLLevel12 -> "LEVEL_1_2"
        HCLLevel13 -> "LEVEL_1_3"
        HCLLevel2 -> "LEVEL_2"
        HCLLevel21 -> "LEVEL_2_1"
        HCLLevel22 -> "LEVEL_2_2"
        HCLLevel3 -> "LEVEL_3"
        HCLLevel31 -> "LEVEL_3_1"
        HCLLevel32 -> "LEVEL_3_2"
        HCLLevel4 -> "LEVEL_4"
        HCLLevel41 -> "LEVEL_4_1"
        HCLLevel42 -> "LEVEL_4_2"
        HCLLevel5 -> "LEVEL_5"
        HCLLevel51 -> "LEVEL_5_1"
        HCLLevel52 -> "LEVEL_5_2"

instance Hashable     H264CodecLevel
instance NFData       H264CodecLevel
instance ToByteString H264CodecLevel
instance ToQuery      H264CodecLevel
instance ToHeader     H264CodecLevel

instance ToJSON H264CodecLevel where
    toJSON = toJSONText

instance FromJSON H264CodecLevel where
    parseJSON = parseJSONText "H264CodecLevel"

-- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
data H264CodecProfile
  = HCPBaseline
  | HCPHigh
  | HCPHigh10BIT
  | HCPHigh422
  | HCPHigh42210BIT
  | HCPMain
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264CodecProfile where
    parser = takeLowerText >>= \case
        "baseline" -> pure HCPBaseline
        "high" -> pure HCPHigh
        "high_10bit" -> pure HCPHigh10BIT
        "high_422" -> pure HCPHigh422
        "high_422_10bit" -> pure HCPHigh42210BIT
        "main" -> pure HCPMain
        e -> fromTextError $ "Failure parsing H264CodecProfile from value: '" <> e
           <> "'. Accepted values: baseline, high, high_10bit, high_422, high_422_10bit, main"

instance ToText H264CodecProfile where
    toText = \case
        HCPBaseline -> "BASELINE"
        HCPHigh -> "HIGH"
        HCPHigh10BIT -> "HIGH_10BIT"
        HCPHigh422 -> "HIGH_422"
        HCPHigh42210BIT -> "HIGH_422_10BIT"
        HCPMain -> "MAIN"

instance Hashable     H264CodecProfile
instance NFData       H264CodecProfile
instance ToByteString H264CodecProfile
instance ToQuery      H264CodecProfile
instance ToHeader     H264CodecProfile

instance ToJSON H264CodecProfile where
    toJSON = toJSONText

instance FromJSON H264CodecProfile where
    parseJSON = parseJSONText "H264CodecProfile"

-- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or CAVLC.
data H264EntropyEncoding
  = Cabac
  | Cavlc
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264EntropyEncoding where
    parser = takeLowerText >>= \case
        "cabac" -> pure Cabac
        "cavlc" -> pure Cavlc
        e -> fromTextError $ "Failure parsing H264EntropyEncoding from value: '" <> e
           <> "'. Accepted values: cabac, cavlc"

instance ToText H264EntropyEncoding where
    toText = \case
        Cabac -> "CABAC"
        Cavlc -> "CAVLC"

instance Hashable     H264EntropyEncoding
instance NFData       H264EntropyEncoding
instance ToByteString H264EntropyEncoding
instance ToQuery      H264EntropyEncoding
instance ToHeader     H264EntropyEncoding

instance ToJSON H264EntropyEncoding where
    toJSON = toJSONText

instance FromJSON H264EntropyEncoding where
    parseJSON = parseJSONText "H264EntropyEncoding"

-- | Choosing FORCE_FIELD disables PAFF encoding for interlaced outputs.
data H264FieldEncoding
  = ForceField
  | Paff
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264FieldEncoding where
    parser = takeLowerText >>= \case
        "force_field" -> pure ForceField
        "paff" -> pure Paff
        e -> fromTextError $ "Failure parsing H264FieldEncoding from value: '" <> e
           <> "'. Accepted values: force_field, paff"

instance ToText H264FieldEncoding where
    toText = \case
        ForceField -> "FORCE_FIELD"
        Paff -> "PAFF"

instance Hashable     H264FieldEncoding
instance NFData       H264FieldEncoding
instance ToByteString H264FieldEncoding
instance ToQuery      H264FieldEncoding
instance ToHeader     H264FieldEncoding

instance ToJSON H264FieldEncoding where
    toJSON = toJSONText

instance FromJSON H264FieldEncoding where
    parseJSON = parseJSONText "H264FieldEncoding"

-- | Adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
data H264FlickerAdaptiveQuantization
  = HFAQFDisabled
  | HFAQFEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264FlickerAdaptiveQuantization where
    parser = takeLowerText >>= \case
        "disabled" -> pure HFAQFDisabled
        "enabled" -> pure HFAQFEnabled
        e -> fromTextError $ "Failure parsing H264FlickerAdaptiveQuantization from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264FlickerAdaptiveQuantization where
    toText = \case
        HFAQFDisabled -> "DISABLED"
        HFAQFEnabled -> "ENABLED"

instance Hashable     H264FlickerAdaptiveQuantization
instance NFData       H264FlickerAdaptiveQuantization
instance ToByteString H264FlickerAdaptiveQuantization
instance ToQuery      H264FlickerAdaptiveQuantization
instance ToHeader     H264FlickerAdaptiveQuantization

instance ToJSON H264FlickerAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H264FlickerAdaptiveQuantization where
    parseJSON = parseJSONText "H264FlickerAdaptiveQuantization"

-- | Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.
data H264FramerateControl
  = HInitializeFromSource
  | HSpecified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264FramerateControl where
    parser = takeLowerText >>= \case
        "initialize_from_source" -> pure HInitializeFromSource
        "specified" -> pure HSpecified
        e -> fromTextError $ "Failure parsing H264FramerateControl from value: '" <> e
           <> "'. Accepted values: initialize_from_source, specified"

instance ToText H264FramerateControl where
    toText = \case
        HInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
        HSpecified -> "SPECIFIED"

instance Hashable     H264FramerateControl
instance NFData       H264FramerateControl
instance ToByteString H264FramerateControl
instance ToQuery      H264FramerateControl
instance ToHeader     H264FramerateControl

instance ToJSON H264FramerateControl where
    toJSON = toJSONText

instance FromJSON H264FramerateControl where
    parseJSON = parseJSONText "H264FramerateControl"

-- | When set to INTERPOLATE, produces smoother motion during framerate conversion.
data H264FramerateConversionAlgorithm
  = HFCADuplicateDrop
  | HFCAInterpolate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264FramerateConversionAlgorithm where
    parser = takeLowerText >>= \case
        "duplicate_drop" -> pure HFCADuplicateDrop
        "interpolate" -> pure HFCAInterpolate
        e -> fromTextError $ "Failure parsing H264FramerateConversionAlgorithm from value: '" <> e
           <> "'. Accepted values: duplicate_drop, interpolate"

instance ToText H264FramerateConversionAlgorithm where
    toText = \case
        HFCADuplicateDrop -> "DUPLICATE_DROP"
        HFCAInterpolate -> "INTERPOLATE"

instance Hashable     H264FramerateConversionAlgorithm
instance NFData       H264FramerateConversionAlgorithm
instance ToByteString H264FramerateConversionAlgorithm
instance ToQuery      H264FramerateConversionAlgorithm
instance ToHeader     H264FramerateConversionAlgorithm

instance ToJSON H264FramerateConversionAlgorithm where
    toJSON = toJSONText

instance FromJSON H264FramerateConversionAlgorithm where
    parseJSON = parseJSONText "H264FramerateConversionAlgorithm"

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
data H264GopBReference
  = HGBRGDisabled
  | HGBRGEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264GopBReference where
    parser = takeLowerText >>= \case
        "disabled" -> pure HGBRGDisabled
        "enabled" -> pure HGBRGEnabled
        e -> fromTextError $ "Failure parsing H264GopBReference from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264GopBReference where
    toText = \case
        HGBRGDisabled -> "DISABLED"
        HGBRGEnabled -> "ENABLED"

instance Hashable     H264GopBReference
instance NFData       H264GopBReference
instance ToByteString H264GopBReference
instance ToQuery      H264GopBReference
instance ToHeader     H264GopBReference

instance ToJSON H264GopBReference where
    toJSON = toJSONText

instance FromJSON H264GopBReference where
    parseJSON = parseJSONText "H264GopBReference"

-- | Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
data H264GopSizeUnits
  = HGSUFrames
  | HGSUSeconds
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264GopSizeUnits where
    parser = takeLowerText >>= \case
        "frames" -> pure HGSUFrames
        "seconds" -> pure HGSUSeconds
        e -> fromTextError $ "Failure parsing H264GopSizeUnits from value: '" <> e
           <> "'. Accepted values: frames, seconds"

instance ToText H264GopSizeUnits where
    toText = \case
        HGSUFrames -> "FRAMES"
        HGSUSeconds -> "SECONDS"

instance Hashable     H264GopSizeUnits
instance NFData       H264GopSizeUnits
instance ToByteString H264GopSizeUnits
instance ToQuery      H264GopSizeUnits
instance ToHeader     H264GopSizeUnits

instance ToJSON H264GopSizeUnits where
    toJSON = toJSONText

instance FromJSON H264GopSizeUnits where
    parseJSON = parseJSONText "H264GopSizeUnits"

-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
data H264InterlaceMode
  = HIMBottomField
  | HIMFollowBottomField
  | HIMFollowTopField
  | HIMProgressive
  | HIMTopField
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264InterlaceMode where
    parser = takeLowerText >>= \case
        "bottom_field" -> pure HIMBottomField
        "follow_bottom_field" -> pure HIMFollowBottomField
        "follow_top_field" -> pure HIMFollowTopField
        "progressive" -> pure HIMProgressive
        "top_field" -> pure HIMTopField
        e -> fromTextError $ "Failure parsing H264InterlaceMode from value: '" <> e
           <> "'. Accepted values: bottom_field, follow_bottom_field, follow_top_field, progressive, top_field"

instance ToText H264InterlaceMode where
    toText = \case
        HIMBottomField -> "BOTTOM_FIELD"
        HIMFollowBottomField -> "FOLLOW_BOTTOM_FIELD"
        HIMFollowTopField -> "FOLLOW_TOP_FIELD"
        HIMProgressive -> "PROGRESSIVE"
        HIMTopField -> "TOP_FIELD"

instance Hashable     H264InterlaceMode
instance NFData       H264InterlaceMode
instance ToByteString H264InterlaceMode
instance ToQuery      H264InterlaceMode
instance ToHeader     H264InterlaceMode

instance ToJSON H264InterlaceMode where
    toJSON = toJSONText

instance FromJSON H264InterlaceMode where
    parseJSON = parseJSONText "H264InterlaceMode"

-- | Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
data H264ParControl
  = HPCInitializeFromSource
  | HPCSpecified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264ParControl where
    parser = takeLowerText >>= \case
        "initialize_from_source" -> pure HPCInitializeFromSource
        "specified" -> pure HPCSpecified
        e -> fromTextError $ "Failure parsing H264ParControl from value: '" <> e
           <> "'. Accepted values: initialize_from_source, specified"

instance ToText H264ParControl where
    toText = \case
        HPCInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
        HPCSpecified -> "SPECIFIED"

instance Hashable     H264ParControl
instance NFData       H264ParControl
instance ToByteString H264ParControl
instance ToQuery      H264ParControl
instance ToHeader     H264ParControl

instance ToJSON H264ParControl where
    toJSON = toJSONText

instance FromJSON H264ParControl where
    parseJSON = parseJSONText "H264ParControl"

-- | Use Quality tuning level (H264QualityTuningLevel) to specifiy whether to use fast single-pass, high-quality singlepass, or high-quality multipass video encoding.
data H264QualityTuningLevel
  = HQTLMultiPassHq
  | HQTLSinglePass
  | HQTLSinglePassHq
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264QualityTuningLevel where
    parser = takeLowerText >>= \case
        "multi_pass_hq" -> pure HQTLMultiPassHq
        "single_pass" -> pure HQTLSinglePass
        "single_pass_hq" -> pure HQTLSinglePassHq
        e -> fromTextError $ "Failure parsing H264QualityTuningLevel from value: '" <> e
           <> "'. Accepted values: multi_pass_hq, single_pass, single_pass_hq"

instance ToText H264QualityTuningLevel where
    toText = \case
        HQTLMultiPassHq -> "MULTI_PASS_HQ"
        HQTLSinglePass -> "SINGLE_PASS"
        HQTLSinglePassHq -> "SINGLE_PASS_HQ"

instance Hashable     H264QualityTuningLevel
instance NFData       H264QualityTuningLevel
instance ToByteString H264QualityTuningLevel
instance ToQuery      H264QualityTuningLevel
instance ToHeader     H264QualityTuningLevel

instance ToJSON H264QualityTuningLevel where
    toJSON = toJSONText

instance FromJSON H264QualityTuningLevel where
    parseJSON = parseJSONText "H264QualityTuningLevel"

-- | Rate control mode. CQ uses constant quantizer (qp), ABR (average bitrate) does not write HRD parameters.
data H264RateControlMode
  = HRCMCbr
  | HRCMVbr
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264RateControlMode where
    parser = takeLowerText >>= \case
        "cbr" -> pure HRCMCbr
        "vbr" -> pure HRCMVbr
        e -> fromTextError $ "Failure parsing H264RateControlMode from value: '" <> e
           <> "'. Accepted values: cbr, vbr"

instance ToText H264RateControlMode where
    toText = \case
        HRCMCbr -> "CBR"
        HRCMVbr -> "VBR"

instance Hashable     H264RateControlMode
instance NFData       H264RateControlMode
instance ToByteString H264RateControlMode
instance ToQuery      H264RateControlMode
instance ToHeader     H264RateControlMode

instance ToJSON H264RateControlMode where
    toJSON = toJSONText

instance FromJSON H264RateControlMode where
    parseJSON = parseJSONText "H264RateControlMode"

-- | Places a PPS header on each encoded picture, even if repeated.
data H264RepeatPps
  = HRPDisabled
  | HRPEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264RepeatPps where
    parser = takeLowerText >>= \case
        "disabled" -> pure HRPDisabled
        "enabled" -> pure HRPEnabled
        e -> fromTextError $ "Failure parsing H264RepeatPps from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264RepeatPps where
    toText = \case
        HRPDisabled -> "DISABLED"
        HRPEnabled -> "ENABLED"

instance Hashable     H264RepeatPps
instance NFData       H264RepeatPps
instance ToByteString H264RepeatPps
instance ToQuery      H264RepeatPps
instance ToHeader     H264RepeatPps

instance ToJSON H264RepeatPps where
    toJSON = toJSONText

instance FromJSON H264RepeatPps where
    parseJSON = parseJSONText "H264RepeatPps"

-- | Scene change detection (inserts I-frames on scene changes).
data H264SceneChangeDetect
  = HSCDSDisabled
  | HSCDSEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264SceneChangeDetect where
    parser = takeLowerText >>= \case
        "disabled" -> pure HSCDSDisabled
        "enabled" -> pure HSCDSEnabled
        e -> fromTextError $ "Failure parsing H264SceneChangeDetect from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264SceneChangeDetect where
    toText = \case
        HSCDSDisabled -> "DISABLED"
        HSCDSEnabled -> "ENABLED"

instance Hashable     H264SceneChangeDetect
instance NFData       H264SceneChangeDetect
instance ToByteString H264SceneChangeDetect
instance ToQuery      H264SceneChangeDetect
instance ToHeader     H264SceneChangeDetect

instance ToJSON H264SceneChangeDetect where
    toJSON = toJSONText

instance FromJSON H264SceneChangeDetect where
    parseJSON = parseJSONText "H264SceneChangeDetect"

-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
data H264SlowPal
  = HSPSDisabled
  | HSPSEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264SlowPal where
    parser = takeLowerText >>= \case
        "disabled" -> pure HSPSDisabled
        "enabled" -> pure HSPSEnabled
        e -> fromTextError $ "Failure parsing H264SlowPal from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264SlowPal where
    toText = \case
        HSPSDisabled -> "DISABLED"
        HSPSEnabled -> "ENABLED"

instance Hashable     H264SlowPal
instance NFData       H264SlowPal
instance ToByteString H264SlowPal
instance ToQuery      H264SlowPal
instance ToHeader     H264SlowPal

instance ToJSON H264SlowPal where
    toJSON = toJSONText

instance FromJSON H264SlowPal where
    parseJSON = parseJSONText "H264SlowPal"

-- | Adjust quantization within each frame based on spatial variation of content complexity.
data H264SpatialAdaptiveQuantization
  = HSAQSDisabled
  | HSAQSEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264SpatialAdaptiveQuantization where
    parser = takeLowerText >>= \case
        "disabled" -> pure HSAQSDisabled
        "enabled" -> pure HSAQSEnabled
        e -> fromTextError $ "Failure parsing H264SpatialAdaptiveQuantization from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264SpatialAdaptiveQuantization where
    toText = \case
        HSAQSDisabled -> "DISABLED"
        HSAQSEnabled -> "ENABLED"

instance Hashable     H264SpatialAdaptiveQuantization
instance NFData       H264SpatialAdaptiveQuantization
instance ToByteString H264SpatialAdaptiveQuantization
instance ToQuery      H264SpatialAdaptiveQuantization
instance ToHeader     H264SpatialAdaptiveQuantization

instance ToJSON H264SpatialAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H264SpatialAdaptiveQuantization where
    parseJSON = parseJSONText "H264SpatialAdaptiveQuantization"

-- | Produces a bitstream compliant with SMPTE RP-2027.
data H264Syntax
  = HSDefault
  | HSRP2027
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264Syntax where
    parser = takeLowerText >>= \case
        "default" -> pure HSDefault
        "rp2027" -> pure HSRP2027
        e -> fromTextError $ "Failure parsing H264Syntax from value: '" <> e
           <> "'. Accepted values: default, rp2027"

instance ToText H264Syntax where
    toText = \case
        HSDefault -> "DEFAULT"
        HSRP2027 -> "RP2027"

instance Hashable     H264Syntax
instance NFData       H264Syntax
instance ToByteString H264Syntax
instance ToQuery      H264Syntax
instance ToHeader     H264Syntax

instance ToJSON H264Syntax where
    toJSON = toJSONText

instance FromJSON H264Syntax where
    parseJSON = parseJSONText "H264Syntax"

-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
data H264Telecine
  = HHard
  | HNone
  | HSoft
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264Telecine where
    parser = takeLowerText >>= \case
        "hard" -> pure HHard
        "none" -> pure HNone
        "soft" -> pure HSoft
        e -> fromTextError $ "Failure parsing H264Telecine from value: '" <> e
           <> "'. Accepted values: hard, none, soft"

instance ToText H264Telecine where
    toText = \case
        HHard -> "HARD"
        HNone -> "NONE"
        HSoft -> "SOFT"

instance Hashable     H264Telecine
instance NFData       H264Telecine
instance ToByteString H264Telecine
instance ToQuery      H264Telecine
instance ToHeader     H264Telecine

instance ToJSON H264Telecine where
    toJSON = toJSONText

instance FromJSON H264Telecine where
    parseJSON = parseJSONText "H264Telecine"

-- | Adjust quantization within each frame based on temporal variation of content complexity.
data H264TemporalAdaptiveQuantization
  = H26Disabled
  | H26Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264TemporalAdaptiveQuantization where
    parser = takeLowerText >>= \case
        "disabled" -> pure H26Disabled
        "enabled" -> pure H26Enabled
        e -> fromTextError $ "Failure parsing H264TemporalAdaptiveQuantization from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264TemporalAdaptiveQuantization where
    toText = \case
        H26Disabled -> "DISABLED"
        H26Enabled -> "ENABLED"

instance Hashable     H264TemporalAdaptiveQuantization
instance NFData       H264TemporalAdaptiveQuantization
instance ToByteString H264TemporalAdaptiveQuantization
instance ToQuery      H264TemporalAdaptiveQuantization
instance ToHeader     H264TemporalAdaptiveQuantization

instance ToJSON H264TemporalAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H264TemporalAdaptiveQuantization where
    parseJSON = parseJSONText "H264TemporalAdaptiveQuantization"

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
data H264UnregisteredSeiTimecode
  = HDisabled
  | HEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264UnregisteredSeiTimecode where
    parser = takeLowerText >>= \case
        "disabled" -> pure HDisabled
        "enabled" -> pure HEnabled
        e -> fromTextError $ "Failure parsing H264UnregisteredSeiTimecode from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264UnregisteredSeiTimecode where
    toText = \case
        HDisabled -> "DISABLED"
        HEnabled -> "ENABLED"

instance Hashable     H264UnregisteredSeiTimecode
instance NFData       H264UnregisteredSeiTimecode
instance ToByteString H264UnregisteredSeiTimecode
instance ToQuery      H264UnregisteredSeiTimecode
instance ToHeader     H264UnregisteredSeiTimecode

instance ToJSON H264UnregisteredSeiTimecode where
    toJSON = toJSONText

instance FromJSON H264UnregisteredSeiTimecode where
    parseJSON = parseJSONText "H264UnregisteredSeiTimecode"

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
data H265AdaptiveQuantization
  = High
  | Higher
  | Low
  | Max
  | Medium
  | Off
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265AdaptiveQuantization where
    parser = takeLowerText >>= \case
        "high" -> pure High
        "higher" -> pure Higher
        "low" -> pure Low
        "max" -> pure Max
        "medium" -> pure Medium
        "off" -> pure Off
        e -> fromTextError $ "Failure parsing H265AdaptiveQuantization from value: '" <> e
           <> "'. Accepted values: high, higher, low, max, medium, off"

instance ToText H265AdaptiveQuantization where
    toText = \case
        High -> "HIGH"
        Higher -> "HIGHER"
        Low -> "LOW"
        Max -> "MAX"
        Medium -> "MEDIUM"
        Off -> "OFF"

instance Hashable     H265AdaptiveQuantization
instance NFData       H265AdaptiveQuantization
instance ToByteString H265AdaptiveQuantization
instance ToQuery      H265AdaptiveQuantization
instance ToHeader     H265AdaptiveQuantization

instance ToJSON H265AdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H265AdaptiveQuantization where
    parseJSON = parseJSONText "H265AdaptiveQuantization"

-- | Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
data H265AlternateTransferFunctionSei
  = HATFSDisabled
  | HATFSEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265AlternateTransferFunctionSei where
    parser = takeLowerText >>= \case
        "disabled" -> pure HATFSDisabled
        "enabled" -> pure HATFSEnabled
        e -> fromTextError $ "Failure parsing H265AlternateTransferFunctionSei from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H265AlternateTransferFunctionSei where
    toText = \case
        HATFSDisabled -> "DISABLED"
        HATFSEnabled -> "ENABLED"

instance Hashable     H265AlternateTransferFunctionSei
instance NFData       H265AlternateTransferFunctionSei
instance ToByteString H265AlternateTransferFunctionSei
instance ToQuery      H265AlternateTransferFunctionSei
instance ToHeader     H265AlternateTransferFunctionSei

instance ToJSON H265AlternateTransferFunctionSei where
    toJSON = toJSONText

instance FromJSON H265AlternateTransferFunctionSei where
    parseJSON = parseJSONText "H265AlternateTransferFunctionSei"

-- | H.265 Level.
data H265CodecLevel
  = Auto
  | Level1
  | Level2
  | Level21
  | Level3
  | Level31
  | Level4
  | Level41
  | Level5
  | Level51
  | Level52
  | Level6
  | Level61
  | Level62
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265CodecLevel where
    parser = takeLowerText >>= \case
        "auto" -> pure Auto
        "level_1" -> pure Level1
        "level_2" -> pure Level2
        "level_2_1" -> pure Level21
        "level_3" -> pure Level3
        "level_3_1" -> pure Level31
        "level_4" -> pure Level4
        "level_4_1" -> pure Level41
        "level_5" -> pure Level5
        "level_5_1" -> pure Level51
        "level_5_2" -> pure Level52
        "level_6" -> pure Level6
        "level_6_1" -> pure Level61
        "level_6_2" -> pure Level62
        e -> fromTextError $ "Failure parsing H265CodecLevel from value: '" <> e
           <> "'. Accepted values: auto, level_1, level_2, level_2_1, level_3, level_3_1, level_4, level_4_1, level_5, level_5_1, level_5_2, level_6, level_6_1, level_6_2"

instance ToText H265CodecLevel where
    toText = \case
        Auto -> "AUTO"
        Level1 -> "LEVEL_1"
        Level2 -> "LEVEL_2"
        Level21 -> "LEVEL_2_1"
        Level3 -> "LEVEL_3"
        Level31 -> "LEVEL_3_1"
        Level4 -> "LEVEL_4"
        Level41 -> "LEVEL_4_1"
        Level5 -> "LEVEL_5"
        Level51 -> "LEVEL_5_1"
        Level52 -> "LEVEL_5_2"
        Level6 -> "LEVEL_6"
        Level61 -> "LEVEL_6_1"
        Level62 -> "LEVEL_6_2"

instance Hashable     H265CodecLevel
instance NFData       H265CodecLevel
instance ToByteString H265CodecLevel
instance ToQuery      H265CodecLevel
instance ToHeader     H265CodecLevel

instance ToJSON H265CodecLevel where
    toJSON = toJSONText

instance FromJSON H265CodecLevel where
    parseJSON = parseJSONText "H265CodecLevel"

-- | Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
data H265CodecProfile
  = MAIN10High
  | MAIN10Main
  | Main42210BITHigh
  | Main42210BITMain
  | Main4228BITHigh
  | Main4228BITMain
  | MainHigh
  | MainMain
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265CodecProfile where
    parser = takeLowerText >>= \case
        "main10_high" -> pure MAIN10High
        "main10_main" -> pure MAIN10Main
        "main_422_10bit_high" -> pure Main42210BITHigh
        "main_422_10bit_main" -> pure Main42210BITMain
        "main_422_8bit_high" -> pure Main4228BITHigh
        "main_422_8bit_main" -> pure Main4228BITMain
        "main_high" -> pure MainHigh
        "main_main" -> pure MainMain
        e -> fromTextError $ "Failure parsing H265CodecProfile from value: '" <> e
           <> "'. Accepted values: main10_high, main10_main, main_422_10bit_high, main_422_10bit_main, main_422_8bit_high, main_422_8bit_main, main_high, main_main"

instance ToText H265CodecProfile where
    toText = \case
        MAIN10High -> "MAIN10_HIGH"
        MAIN10Main -> "MAIN10_MAIN"
        Main42210BITHigh -> "MAIN_422_10BIT_HIGH"
        Main42210BITMain -> "MAIN_422_10BIT_MAIN"
        Main4228BITHigh -> "MAIN_422_8BIT_HIGH"
        Main4228BITMain -> "MAIN_422_8BIT_MAIN"
        MainHigh -> "MAIN_HIGH"
        MainMain -> "MAIN_MAIN"

instance Hashable     H265CodecProfile
instance NFData       H265CodecProfile
instance ToByteString H265CodecProfile
instance ToQuery      H265CodecProfile
instance ToHeader     H265CodecProfile

instance ToJSON H265CodecProfile where
    toJSON = toJSONText

instance FromJSON H265CodecProfile where
    parseJSON = parseJSONText "H265CodecProfile"

-- | Adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
data H265FlickerAdaptiveQuantization
  = HFAQDisabled
  | HFAQEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265FlickerAdaptiveQuantization where
    parser = takeLowerText >>= \case
        "disabled" -> pure HFAQDisabled
        "enabled" -> pure HFAQEnabled
        e -> fromTextError $ "Failure parsing H265FlickerAdaptiveQuantization from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H265FlickerAdaptiveQuantization where
    toText = \case
        HFAQDisabled -> "DISABLED"
        HFAQEnabled -> "ENABLED"

instance Hashable     H265FlickerAdaptiveQuantization
instance NFData       H265FlickerAdaptiveQuantization
instance ToByteString H265FlickerAdaptiveQuantization
instance ToQuery      H265FlickerAdaptiveQuantization
instance ToHeader     H265FlickerAdaptiveQuantization

instance ToJSON H265FlickerAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H265FlickerAdaptiveQuantization where
    parseJSON = parseJSONText "H265FlickerAdaptiveQuantization"

-- | Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.
data H265FramerateControl
  = HFCInitializeFromSource
  | HFCSpecified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265FramerateControl where
    parser = takeLowerText >>= \case
        "initialize_from_source" -> pure HFCInitializeFromSource
        "specified" -> pure HFCSpecified
        e -> fromTextError $ "Failure parsing H265FramerateControl from value: '" <> e
           <> "'. Accepted values: initialize_from_source, specified"

instance ToText H265FramerateControl where
    toText = \case
        HFCInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
        HFCSpecified -> "SPECIFIED"

instance Hashable     H265FramerateControl
instance NFData       H265FramerateControl
instance ToByteString H265FramerateControl
instance ToQuery      H265FramerateControl
instance ToHeader     H265FramerateControl

instance ToJSON H265FramerateControl where
    toJSON = toJSONText

instance FromJSON H265FramerateControl where
    parseJSON = parseJSONText "H265FramerateControl"

-- | When set to INTERPOLATE, produces smoother motion during framerate conversion.
data H265FramerateConversionAlgorithm
  = DuplicateDrop
  | Interpolate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265FramerateConversionAlgorithm where
    parser = takeLowerText >>= \case
        "duplicate_drop" -> pure DuplicateDrop
        "interpolate" -> pure Interpolate
        e -> fromTextError $ "Failure parsing H265FramerateConversionAlgorithm from value: '" <> e
           <> "'. Accepted values: duplicate_drop, interpolate"

instance ToText H265FramerateConversionAlgorithm where
    toText = \case
        DuplicateDrop -> "DUPLICATE_DROP"
        Interpolate -> "INTERPOLATE"

instance Hashable     H265FramerateConversionAlgorithm
instance NFData       H265FramerateConversionAlgorithm
instance ToByteString H265FramerateConversionAlgorithm
instance ToQuery      H265FramerateConversionAlgorithm
instance ToHeader     H265FramerateConversionAlgorithm

instance ToJSON H265FramerateConversionAlgorithm where
    toJSON = toJSONText

instance FromJSON H265FramerateConversionAlgorithm where
    parseJSON = parseJSONText "H265FramerateConversionAlgorithm"

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
data H265GopBReference
  = HGBRDisabled
  | HGBREnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265GopBReference where
    parser = takeLowerText >>= \case
        "disabled" -> pure HGBRDisabled
        "enabled" -> pure HGBREnabled
        e -> fromTextError $ "Failure parsing H265GopBReference from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H265GopBReference where
    toText = \case
        HGBRDisabled -> "DISABLED"
        HGBREnabled -> "ENABLED"

instance Hashable     H265GopBReference
instance NFData       H265GopBReference
instance ToByteString H265GopBReference
instance ToQuery      H265GopBReference
instance ToHeader     H265GopBReference

instance ToJSON H265GopBReference where
    toJSON = toJSONText

instance FromJSON H265GopBReference where
    parseJSON = parseJSONText "H265GopBReference"

-- | Indicates if the GOP Size in H265 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
data H265GopSizeUnits
  = Frames
  | Seconds
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265GopSizeUnits where
    parser = takeLowerText >>= \case
        "frames" -> pure Frames
        "seconds" -> pure Seconds
        e -> fromTextError $ "Failure parsing H265GopSizeUnits from value: '" <> e
           <> "'. Accepted values: frames, seconds"

instance ToText H265GopSizeUnits where
    toText = \case
        Frames -> "FRAMES"
        Seconds -> "SECONDS"

instance Hashable     H265GopSizeUnits
instance NFData       H265GopSizeUnits
instance ToByteString H265GopSizeUnits
instance ToQuery      H265GopSizeUnits
instance ToHeader     H265GopSizeUnits

instance ToJSON H265GopSizeUnits where
    toJSON = toJSONText

instance FromJSON H265GopSizeUnits where
    parseJSON = parseJSONText "H265GopSizeUnits"

-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
data H265InterlaceMode
  = BottomField
  | FollowBottomField
  | FollowTopField
  | Progressive
  | TopField
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265InterlaceMode where
    parser = takeLowerText >>= \case
        "bottom_field" -> pure BottomField
        "follow_bottom_field" -> pure FollowBottomField
        "follow_top_field" -> pure FollowTopField
        "progressive" -> pure Progressive
        "top_field" -> pure TopField
        e -> fromTextError $ "Failure parsing H265InterlaceMode from value: '" <> e
           <> "'. Accepted values: bottom_field, follow_bottom_field, follow_top_field, progressive, top_field"

instance ToText H265InterlaceMode where
    toText = \case
        BottomField -> "BOTTOM_FIELD"
        FollowBottomField -> "FOLLOW_BOTTOM_FIELD"
        FollowTopField -> "FOLLOW_TOP_FIELD"
        Progressive -> "PROGRESSIVE"
        TopField -> "TOP_FIELD"

instance Hashable     H265InterlaceMode
instance NFData       H265InterlaceMode
instance ToByteString H265InterlaceMode
instance ToQuery      H265InterlaceMode
instance ToHeader     H265InterlaceMode

instance ToJSON H265InterlaceMode where
    toJSON = toJSONText

instance FromJSON H265InterlaceMode where
    parseJSON = parseJSONText "H265InterlaceMode"

-- | Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
data H265ParControl
  = InitializeFromSource
  | Specified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265ParControl where
    parser = takeLowerText >>= \case
        "initialize_from_source" -> pure InitializeFromSource
        "specified" -> pure Specified
        e -> fromTextError $ "Failure parsing H265ParControl from value: '" <> e
           <> "'. Accepted values: initialize_from_source, specified"

instance ToText H265ParControl where
    toText = \case
        InitializeFromSource -> "INITIALIZE_FROM_SOURCE"
        Specified -> "SPECIFIED"

instance Hashable     H265ParControl
instance NFData       H265ParControl
instance ToByteString H265ParControl
instance ToQuery      H265ParControl
instance ToHeader     H265ParControl

instance ToJSON H265ParControl where
    toJSON = toJSONText

instance FromJSON H265ParControl where
    parseJSON = parseJSONText "H265ParControl"

-- | Use Quality tuning level (H265QualityTuningLevel) to specifiy whether to use fast single-pass, high-quality singlepass, or high-quality multipass video encoding.
data H265QualityTuningLevel
  = MultiPassHq
  | SinglePass
  | SinglePassHq
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265QualityTuningLevel where
    parser = takeLowerText >>= \case
        "multi_pass_hq" -> pure MultiPassHq
        "single_pass" -> pure SinglePass
        "single_pass_hq" -> pure SinglePassHq
        e -> fromTextError $ "Failure parsing H265QualityTuningLevel from value: '" <> e
           <> "'. Accepted values: multi_pass_hq, single_pass, single_pass_hq"

instance ToText H265QualityTuningLevel where
    toText = \case
        MultiPassHq -> "MULTI_PASS_HQ"
        SinglePass -> "SINGLE_PASS"
        SinglePassHq -> "SINGLE_PASS_HQ"

instance Hashable     H265QualityTuningLevel
instance NFData       H265QualityTuningLevel
instance ToByteString H265QualityTuningLevel
instance ToQuery      H265QualityTuningLevel
instance ToHeader     H265QualityTuningLevel

instance ToJSON H265QualityTuningLevel where
    toJSON = toJSONText

instance FromJSON H265QualityTuningLevel where
    parseJSON = parseJSONText "H265QualityTuningLevel"

-- | Rate control mode. CQ uses constant quantizer (qp), ABR (average bitrate) does not write HRD parameters.
data H265RateControlMode
  = Cbr
  | Vbr
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265RateControlMode where
    parser = takeLowerText >>= \case
        "cbr" -> pure Cbr
        "vbr" -> pure Vbr
        e -> fromTextError $ "Failure parsing H265RateControlMode from value: '" <> e
           <> "'. Accepted values: cbr, vbr"

instance ToText H265RateControlMode where
    toText = \case
        Cbr -> "CBR"
        Vbr -> "VBR"

instance Hashable     H265RateControlMode
instance NFData       H265RateControlMode
instance ToByteString H265RateControlMode
instance ToQuery      H265RateControlMode
instance ToHeader     H265RateControlMode

instance ToJSON H265RateControlMode where
    toJSON = toJSONText

instance FromJSON H265RateControlMode where
    parseJSON = parseJSONText "H265RateControlMode"

-- | Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content
data H265SampleAdaptiveOffsetFilterMode
  = HSAOFMAdaptive
  | HSAOFMDefault
  | HSAOFMOff
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265SampleAdaptiveOffsetFilterMode where
    parser = takeLowerText >>= \case
        "adaptive" -> pure HSAOFMAdaptive
        "default" -> pure HSAOFMDefault
        "off" -> pure HSAOFMOff
        e -> fromTextError $ "Failure parsing H265SampleAdaptiveOffsetFilterMode from value: '" <> e
           <> "'. Accepted values: adaptive, default, off"

instance ToText H265SampleAdaptiveOffsetFilterMode where
    toText = \case
        HSAOFMAdaptive -> "ADAPTIVE"
        HSAOFMDefault -> "DEFAULT"
        HSAOFMOff -> "OFF"

instance Hashable     H265SampleAdaptiveOffsetFilterMode
instance NFData       H265SampleAdaptiveOffsetFilterMode
instance ToByteString H265SampleAdaptiveOffsetFilterMode
instance ToQuery      H265SampleAdaptiveOffsetFilterMode
instance ToHeader     H265SampleAdaptiveOffsetFilterMode

instance ToJSON H265SampleAdaptiveOffsetFilterMode where
    toJSON = toJSONText

instance FromJSON H265SampleAdaptiveOffsetFilterMode where
    parseJSON = parseJSONText "H265SampleAdaptiveOffsetFilterMode"

-- | Scene change detection (inserts I-frames on scene changes).
data H265SceneChangeDetect
  = HSCDDisabled
  | HSCDEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265SceneChangeDetect where
    parser = takeLowerText >>= \case
        "disabled" -> pure HSCDDisabled
        "enabled" -> pure HSCDEnabled
        e -> fromTextError $ "Failure parsing H265SceneChangeDetect from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H265SceneChangeDetect where
    toText = \case
        HSCDDisabled -> "DISABLED"
        HSCDEnabled -> "ENABLED"

instance Hashable     H265SceneChangeDetect
instance NFData       H265SceneChangeDetect
instance ToByteString H265SceneChangeDetect
instance ToQuery      H265SceneChangeDetect
instance ToHeader     H265SceneChangeDetect

instance ToJSON H265SceneChangeDetect where
    toJSON = toJSONText

instance FromJSON H265SceneChangeDetect where
    parseJSON = parseJSONText "H265SceneChangeDetect"

-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
data H265SlowPal
  = HSPDisabled
  | HSPEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265SlowPal where
    parser = takeLowerText >>= \case
        "disabled" -> pure HSPDisabled
        "enabled" -> pure HSPEnabled
        e -> fromTextError $ "Failure parsing H265SlowPal from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H265SlowPal where
    toText = \case
        HSPDisabled -> "DISABLED"
        HSPEnabled -> "ENABLED"

instance Hashable     H265SlowPal
instance NFData       H265SlowPal
instance ToByteString H265SlowPal
instance ToQuery      H265SlowPal
instance ToHeader     H265SlowPal

instance ToJSON H265SlowPal where
    toJSON = toJSONText

instance FromJSON H265SlowPal where
    parseJSON = parseJSONText "H265SlowPal"

-- | Adjust quantization within each frame based on spatial variation of content complexity.
data H265SpatialAdaptiveQuantization
  = HSAQDisabled
  | HSAQEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265SpatialAdaptiveQuantization where
    parser = takeLowerText >>= \case
        "disabled" -> pure HSAQDisabled
        "enabled" -> pure HSAQEnabled
        e -> fromTextError $ "Failure parsing H265SpatialAdaptiveQuantization from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H265SpatialAdaptiveQuantization where
    toText = \case
        HSAQDisabled -> "DISABLED"
        HSAQEnabled -> "ENABLED"

instance Hashable     H265SpatialAdaptiveQuantization
instance NFData       H265SpatialAdaptiveQuantization
instance ToByteString H265SpatialAdaptiveQuantization
instance ToQuery      H265SpatialAdaptiveQuantization
instance ToHeader     H265SpatialAdaptiveQuantization

instance ToJSON H265SpatialAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H265SpatialAdaptiveQuantization where
    parseJSON = parseJSONText "H265SpatialAdaptiveQuantization"

-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
data H265Telecine
  = HTHard
  | HTNone
  | HTSoft
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265Telecine where
    parser = takeLowerText >>= \case
        "hard" -> pure HTHard
        "none" -> pure HTNone
        "soft" -> pure HTSoft
        e -> fromTextError $ "Failure parsing H265Telecine from value: '" <> e
           <> "'. Accepted values: hard, none, soft"

instance ToText H265Telecine where
    toText = \case
        HTHard -> "HARD"
        HTNone -> "NONE"
        HTSoft -> "SOFT"

instance Hashable     H265Telecine
instance NFData       H265Telecine
instance ToByteString H265Telecine
instance ToQuery      H265Telecine
instance ToHeader     H265Telecine

instance ToJSON H265Telecine where
    toJSON = toJSONText

instance FromJSON H265Telecine where
    parseJSON = parseJSONText "H265Telecine"

-- | Adjust quantization within each frame based on temporal variation of content complexity.
data H265TemporalAdaptiveQuantization
  = HTAQDisabled
  | HTAQEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265TemporalAdaptiveQuantization where
    parser = takeLowerText >>= \case
        "disabled" -> pure HTAQDisabled
        "enabled" -> pure HTAQEnabled
        e -> fromTextError $ "Failure parsing H265TemporalAdaptiveQuantization from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H265TemporalAdaptiveQuantization where
    toText = \case
        HTAQDisabled -> "DISABLED"
        HTAQEnabled -> "ENABLED"

instance Hashable     H265TemporalAdaptiveQuantization
instance NFData       H265TemporalAdaptiveQuantization
instance ToByteString H265TemporalAdaptiveQuantization
instance ToQuery      H265TemporalAdaptiveQuantization
instance ToHeader     H265TemporalAdaptiveQuantization

instance ToJSON H265TemporalAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H265TemporalAdaptiveQuantization where
    parseJSON = parseJSONText "H265TemporalAdaptiveQuantization"

-- | Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.
data H265TemporalIds
  = HTIDisabled
  | HTIEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265TemporalIds where
    parser = takeLowerText >>= \case
        "disabled" -> pure HTIDisabled
        "enabled" -> pure HTIEnabled
        e -> fromTextError $ "Failure parsing H265TemporalIds from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H265TemporalIds where
    toText = \case
        HTIDisabled -> "DISABLED"
        HTIEnabled -> "ENABLED"

instance Hashable     H265TemporalIds
instance NFData       H265TemporalIds
instance ToByteString H265TemporalIds
instance ToQuery      H265TemporalIds
instance ToHeader     H265TemporalIds

instance ToJSON H265TemporalIds where
    toJSON = toJSONText

instance FromJSON H265TemporalIds where
    parseJSON = parseJSONText "H265TemporalIds"

-- | Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.
data H265Tiles
  = HTDisabled
  | HTEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265Tiles where
    parser = takeLowerText >>= \case
        "disabled" -> pure HTDisabled
        "enabled" -> pure HTEnabled
        e -> fromTextError $ "Failure parsing H265Tiles from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H265Tiles where
    toText = \case
        HTDisabled -> "DISABLED"
        HTEnabled -> "ENABLED"

instance Hashable     H265Tiles
instance NFData       H265Tiles
instance ToByteString H265Tiles
instance ToQuery      H265Tiles
instance ToHeader     H265Tiles

instance ToJSON H265Tiles where
    toJSON = toJSONText

instance FromJSON H265Tiles where
    parseJSON = parseJSONText "H265Tiles"

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
data H265UnregisteredSeiTimecode
  = HUSTDisabled
  | HUSTEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H265UnregisteredSeiTimecode where
    parser = takeLowerText >>= \case
        "disabled" -> pure HUSTDisabled
        "enabled" -> pure HUSTEnabled
        e -> fromTextError $ "Failure parsing H265UnregisteredSeiTimecode from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H265UnregisteredSeiTimecode where
    toText = \case
        HUSTDisabled -> "DISABLED"
        HUSTEnabled -> "ENABLED"

instance Hashable     H265UnregisteredSeiTimecode
instance NFData       H265UnregisteredSeiTimecode
instance ToByteString H265UnregisteredSeiTimecode
instance ToQuery      H265UnregisteredSeiTimecode
instance ToHeader     H265UnregisteredSeiTimecode

instance ToJSON H265UnregisteredSeiTimecode where
    toJSON = toJSONText

instance FromJSON H265UnregisteredSeiTimecode where
    parseJSON = parseJSONText "H265UnregisteredSeiTimecode"

data HlsAdMarkers
  = Elemental
  | ElementalSCTE35
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsAdMarkers where
    parser = takeLowerText >>= \case
        "elemental" -> pure Elemental
        "elemental_scte35" -> pure ElementalSCTE35
        e -> fromTextError $ "Failure parsing HlsAdMarkers from value: '" <> e
           <> "'. Accepted values: elemental, elemental_scte35"

instance ToText HlsAdMarkers where
    toText = \case
        Elemental -> "ELEMENTAL"
        ElementalSCTE35 -> "ELEMENTAL_SCTE35"

instance Hashable     HlsAdMarkers
instance NFData       HlsAdMarkers
instance ToByteString HlsAdMarkers
instance ToQuery      HlsAdMarkers
instance ToHeader     HlsAdMarkers

instance ToJSON HlsAdMarkers where
    toJSON = toJSONText

instance FromJSON HlsAdMarkers where
    parseJSON = parseJSONText "HlsAdMarkers"

-- | Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
data HlsAudioTrackType
  = AlternateAudioAutoSelect
  | AlternateAudioAutoSelectDefault
  | AlternateAudioNotAutoSelect
  | AudioOnlyVariantStream
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsAudioTrackType where
    parser = takeLowerText >>= \case
        "alternate_audio_auto_select" -> pure AlternateAudioAutoSelect
        "alternate_audio_auto_select_default" -> pure AlternateAudioAutoSelectDefault
        "alternate_audio_not_auto_select" -> pure AlternateAudioNotAutoSelect
        "audio_only_variant_stream" -> pure AudioOnlyVariantStream
        e -> fromTextError $ "Failure parsing HlsAudioTrackType from value: '" <> e
           <> "'. Accepted values: alternate_audio_auto_select, alternate_audio_auto_select_default, alternate_audio_not_auto_select, audio_only_variant_stream"

instance ToText HlsAudioTrackType where
    toText = \case
        AlternateAudioAutoSelect -> "ALTERNATE_AUDIO_AUTO_SELECT"
        AlternateAudioAutoSelectDefault -> "ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT"
        AlternateAudioNotAutoSelect -> "ALTERNATE_AUDIO_NOT_AUTO_SELECT"
        AudioOnlyVariantStream -> "AUDIO_ONLY_VARIANT_STREAM"

instance Hashable     HlsAudioTrackType
instance NFData       HlsAudioTrackType
instance ToByteString HlsAudioTrackType
instance ToQuery      HlsAudioTrackType
instance ToHeader     HlsAudioTrackType

instance ToJSON HlsAudioTrackType where
    toJSON = toJSONText

instance FromJSON HlsAudioTrackType where
    parseJSON = parseJSONText "HlsAudioTrackType"

-- | Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.
data HlsCaptionLanguageSetting
  = HCLSInsert
  | HCLSNone
  | HCLSOmit
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsCaptionLanguageSetting where
    parser = takeLowerText >>= \case
        "insert" -> pure HCLSInsert
        "none" -> pure HCLSNone
        "omit" -> pure HCLSOmit
        e -> fromTextError $ "Failure parsing HlsCaptionLanguageSetting from value: '" <> e
           <> "'. Accepted values: insert, none, omit"

instance ToText HlsCaptionLanguageSetting where
    toText = \case
        HCLSInsert -> "INSERT"
        HCLSNone -> "NONE"
        HCLSOmit -> "OMIT"

instance Hashable     HlsCaptionLanguageSetting
instance NFData       HlsCaptionLanguageSetting
instance ToByteString HlsCaptionLanguageSetting
instance ToQuery      HlsCaptionLanguageSetting
instance ToHeader     HlsCaptionLanguageSetting

instance ToJSON HlsCaptionLanguageSetting where
    toJSON = toJSONText

instance FromJSON HlsCaptionLanguageSetting where
    parseJSON = parseJSONText "HlsCaptionLanguageSetting"

-- | When set to ENABLED, sets #EXT-X-ALLOW-CACHE:no tag, which prevents client from saving media segments for later replay.
data HlsClientCache
  = HCCDisabled
  | HCCEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsClientCache where
    parser = takeLowerText >>= \case
        "disabled" -> pure HCCDisabled
        "enabled" -> pure HCCEnabled
        e -> fromTextError $ "Failure parsing HlsClientCache from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText HlsClientCache where
    toText = \case
        HCCDisabled -> "DISABLED"
        HCCEnabled -> "ENABLED"

instance Hashable     HlsClientCache
instance NFData       HlsClientCache
instance ToByteString HlsClientCache
instance ToQuery      HlsClientCache
instance ToHeader     HlsClientCache

instance ToJSON HlsClientCache where
    toJSON = toJSONText

instance FromJSON HlsClientCache where
    parseJSON = parseJSONText "HlsClientCache"

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
data HlsCodecSpecification
  = Rfc4281
  | Rfc6381
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsCodecSpecification where
    parser = takeLowerText >>= \case
        "rfc_4281" -> pure Rfc4281
        "rfc_6381" -> pure Rfc6381
        e -> fromTextError $ "Failure parsing HlsCodecSpecification from value: '" <> e
           <> "'. Accepted values: rfc_4281, rfc_6381"

instance ToText HlsCodecSpecification where
    toText = \case
        Rfc4281 -> "RFC_4281"
        Rfc6381 -> "RFC_6381"

instance Hashable     HlsCodecSpecification
instance NFData       HlsCodecSpecification
instance ToByteString HlsCodecSpecification
instance ToQuery      HlsCodecSpecification
instance ToHeader     HlsCodecSpecification

instance ToJSON HlsCodecSpecification where
    toJSON = toJSONText

instance FromJSON HlsCodecSpecification where
    parseJSON = parseJSONText "HlsCodecSpecification"

-- | Indicates whether segments should be placed in subdirectories.
data HlsDirectoryStructure
  = SingleDirectory
  | SubdirectoryPerStream
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsDirectoryStructure where
    parser = takeLowerText >>= \case
        "single_directory" -> pure SingleDirectory
        "subdirectory_per_stream" -> pure SubdirectoryPerStream
        e -> fromTextError $ "Failure parsing HlsDirectoryStructure from value: '" <> e
           <> "'. Accepted values: single_directory, subdirectory_per_stream"

instance ToText HlsDirectoryStructure where
    toText = \case
        SingleDirectory -> "SINGLE_DIRECTORY"
        SubdirectoryPerStream -> "SUBDIRECTORY_PER_STREAM"

instance Hashable     HlsDirectoryStructure
instance NFData       HlsDirectoryStructure
instance ToByteString HlsDirectoryStructure
instance ToQuery      HlsDirectoryStructure
instance ToHeader     HlsDirectoryStructure

instance ToJSON HlsDirectoryStructure where
    toJSON = toJSONText

instance FromJSON HlsDirectoryStructure where
    parseJSON = parseJSONText "HlsDirectoryStructure"

-- | Encrypts the segments with the given encryption scheme. Leave blank to disable. Selecting 'Disabled' in the web interface also disables encryption.
data HlsEncryptionType
  = AES128
  | SampleAES
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsEncryptionType where
    parser = takeLowerText >>= \case
        "aes128" -> pure AES128
        "sample_aes" -> pure SampleAES
        e -> fromTextError $ "Failure parsing HlsEncryptionType from value: '" <> e
           <> "'. Accepted values: aes128, sample_aes"

instance ToText HlsEncryptionType where
    toText = \case
        AES128 -> "AES128"
        SampleAES -> "SAMPLE_AES"

instance Hashable     HlsEncryptionType
instance NFData       HlsEncryptionType
instance ToByteString HlsEncryptionType
instance ToQuery      HlsEncryptionType
instance ToHeader     HlsEncryptionType

instance ToJSON HlsEncryptionType where
    toJSON = toJSONText

instance FromJSON HlsEncryptionType where
    parseJSON = parseJSONText "HlsEncryptionType"

-- | When set to INCLUDE, writes I-Frame Only Manifest in addition to the HLS manifest
data HlsIFrameOnlyManifest
  = HIFOMExclude
  | HIFOMInclude
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsIFrameOnlyManifest where
    parser = takeLowerText >>= \case
        "exclude" -> pure HIFOMExclude
        "include" -> pure HIFOMInclude
        e -> fromTextError $ "Failure parsing HlsIFrameOnlyManifest from value: '" <> e
           <> "'. Accepted values: exclude, include"

instance ToText HlsIFrameOnlyManifest where
    toText = \case
        HIFOMExclude -> "EXCLUDE"
        HIFOMInclude -> "INCLUDE"

instance Hashable     HlsIFrameOnlyManifest
instance NFData       HlsIFrameOnlyManifest
instance ToByteString HlsIFrameOnlyManifest
instance ToQuery      HlsIFrameOnlyManifest
instance ToHeader     HlsIFrameOnlyManifest

instance ToJSON HlsIFrameOnlyManifest where
    toJSON = toJSONText

instance FromJSON HlsIFrameOnlyManifest where
    parseJSON = parseJSONText "HlsIFrameOnlyManifest"

-- | The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.
data HlsInitializationVectorInManifest
  = HIVIMExclude
  | HIVIMInclude
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsInitializationVectorInManifest where
    parser = takeLowerText >>= \case
        "exclude" -> pure HIVIMExclude
        "include" -> pure HIVIMInclude
        e -> fromTextError $ "Failure parsing HlsInitializationVectorInManifest from value: '" <> e
           <> "'. Accepted values: exclude, include"

instance ToText HlsInitializationVectorInManifest where
    toText = \case
        HIVIMExclude -> "EXCLUDE"
        HIVIMInclude -> "INCLUDE"

instance Hashable     HlsInitializationVectorInManifest
instance NFData       HlsInitializationVectorInManifest
instance ToByteString HlsInitializationVectorInManifest
instance ToQuery      HlsInitializationVectorInManifest
instance ToHeader     HlsInitializationVectorInManifest

instance ToJSON HlsInitializationVectorInManifest where
    toJSON = toJSONText

instance FromJSON HlsInitializationVectorInManifest where
    parseJSON = parseJSONText "HlsInitializationVectorInManifest"

-- | Indicates which type of key provider is used for encryption.
data HlsKeyProviderType
  = Speke
  | StaticKey
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsKeyProviderType where
    parser = takeLowerText >>= \case
        "speke" -> pure Speke
        "static_key" -> pure StaticKey
        e -> fromTextError $ "Failure parsing HlsKeyProviderType from value: '" <> e
           <> "'. Accepted values: speke, static_key"

instance ToText HlsKeyProviderType where
    toText = \case
        Speke -> "SPEKE"
        StaticKey -> "STATIC_KEY"

instance Hashable     HlsKeyProviderType
instance NFData       HlsKeyProviderType
instance ToByteString HlsKeyProviderType
instance ToQuery      HlsKeyProviderType
instance ToHeader     HlsKeyProviderType

instance ToJSON HlsKeyProviderType where
    toJSON = toJSONText

instance FromJSON HlsKeyProviderType where
    parseJSON = parseJSONText "HlsKeyProviderType"

-- | When set to GZIP, compresses HLS playlist.
data HlsManifestCompression
  = HMCGzip
  | HMCNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsManifestCompression where
    parser = takeLowerText >>= \case
        "gzip" -> pure HMCGzip
        "none" -> pure HMCNone
        e -> fromTextError $ "Failure parsing HlsManifestCompression from value: '" <> e
           <> "'. Accepted values: gzip, none"

instance ToText HlsManifestCompression where
    toText = \case
        HMCGzip -> "GZIP"
        HMCNone -> "NONE"

instance Hashable     HlsManifestCompression
instance NFData       HlsManifestCompression
instance ToByteString HlsManifestCompression
instance ToQuery      HlsManifestCompression
instance ToHeader     HlsManifestCompression

instance ToJSON HlsManifestCompression where
    toJSON = toJSONText

instance FromJSON HlsManifestCompression where
    parseJSON = parseJSONText "HlsManifestCompression"

-- | Indicates whether the output manifest should use floating point values for segment duration.
data HlsManifestDurationFormat
  = FloatingPoint
  | Integer
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsManifestDurationFormat where
    parser = takeLowerText >>= \case
        "floating_point" -> pure FloatingPoint
        "integer" -> pure Integer
        e -> fromTextError $ "Failure parsing HlsManifestDurationFormat from value: '" <> e
           <> "'. Accepted values: floating_point, integer"

instance ToText HlsManifestDurationFormat where
    toText = \case
        FloatingPoint -> "FLOATING_POINT"
        Integer -> "INTEGER"

instance Hashable     HlsManifestDurationFormat
instance NFData       HlsManifestDurationFormat
instance ToByteString HlsManifestDurationFormat
instance ToQuery      HlsManifestDurationFormat
instance ToHeader     HlsManifestDurationFormat

instance ToJSON HlsManifestDurationFormat where
    toJSON = toJSONText

instance FromJSON HlsManifestDurationFormat where
    parseJSON = parseJSONText "HlsManifestDurationFormat"

-- | Indicates whether the .m3u8 manifest file should be generated for this HLS output group.
data HlsOutputSelection
  = ManifestsAndSegments
  | SegmentsOnly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsOutputSelection where
    parser = takeLowerText >>= \case
        "manifests_and_segments" -> pure ManifestsAndSegments
        "segments_only" -> pure SegmentsOnly
        e -> fromTextError $ "Failure parsing HlsOutputSelection from value: '" <> e
           <> "'. Accepted values: manifests_and_segments, segments_only"

instance ToText HlsOutputSelection where
    toText = \case
        ManifestsAndSegments -> "MANIFESTS_AND_SEGMENTS"
        SegmentsOnly -> "SEGMENTS_ONLY"

instance Hashable     HlsOutputSelection
instance NFData       HlsOutputSelection
instance ToByteString HlsOutputSelection
instance ToQuery      HlsOutputSelection
instance ToHeader     HlsOutputSelection

instance ToJSON HlsOutputSelection where
    toJSON = toJSONText

instance FromJSON HlsOutputSelection where
    parseJSON = parseJSONText "HlsOutputSelection"

-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.
data HlsProgramDateTime
  = Exclude
  | Include
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsProgramDateTime where
    parser = takeLowerText >>= \case
        "exclude" -> pure Exclude
        "include" -> pure Include
        e -> fromTextError $ "Failure parsing HlsProgramDateTime from value: '" <> e
           <> "'. Accepted values: exclude, include"

instance ToText HlsProgramDateTime where
    toText = \case
        Exclude -> "EXCLUDE"
        Include -> "INCLUDE"

instance Hashable     HlsProgramDateTime
instance NFData       HlsProgramDateTime
instance ToByteString HlsProgramDateTime
instance ToQuery      HlsProgramDateTime
instance ToHeader     HlsProgramDateTime

instance ToJSON HlsProgramDateTime where
    toJSON = toJSONText

instance FromJSON HlsProgramDateTime where
    parseJSON = parseJSONText "HlsProgramDateTime"

-- | When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.
data HlsSegmentControl
  = SegmentedFiles
  | SingleFile
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsSegmentControl where
    parser = takeLowerText >>= \case
        "segmented_files" -> pure SegmentedFiles
        "single_file" -> pure SingleFile
        e -> fromTextError $ "Failure parsing HlsSegmentControl from value: '" <> e
           <> "'. Accepted values: segmented_files, single_file"

instance ToText HlsSegmentControl where
    toText = \case
        SegmentedFiles -> "SEGMENTED_FILES"
        SingleFile -> "SINGLE_FILE"

instance Hashable     HlsSegmentControl
instance NFData       HlsSegmentControl
instance ToByteString HlsSegmentControl
instance ToQuery      HlsSegmentControl
instance ToHeader     HlsSegmentControl

instance ToJSON HlsSegmentControl where
    toJSON = toJSONText

instance FromJSON HlsSegmentControl where
    parseJSON = parseJSONText "HlsSegmentControl"

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
data HlsStreamInfResolution
  = HSIRExclude
  | HSIRInclude
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsStreamInfResolution where
    parser = takeLowerText >>= \case
        "exclude" -> pure HSIRExclude
        "include" -> pure HSIRInclude
        e -> fromTextError $ "Failure parsing HlsStreamInfResolution from value: '" <> e
           <> "'. Accepted values: exclude, include"

instance ToText HlsStreamInfResolution where
    toText = \case
        HSIRExclude -> "EXCLUDE"
        HSIRInclude -> "INCLUDE"

instance Hashable     HlsStreamInfResolution
instance NFData       HlsStreamInfResolution
instance ToByteString HlsStreamInfResolution
instance ToQuery      HlsStreamInfResolution
instance ToHeader     HlsStreamInfResolution

instance ToJSON HlsStreamInfResolution where
    toJSON = toJSONText

instance FromJSON HlsStreamInfResolution where
    parseJSON = parseJSONText "HlsStreamInfResolution"

-- | Indicates ID3 frame that has the timecode.
data HlsTimedMetadataId3Frame
  = HTMIFNone
  | HTMIFPriv
  | HTMIFTdrl
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsTimedMetadataId3Frame where
    parser = takeLowerText >>= \case
        "none" -> pure HTMIFNone
        "priv" -> pure HTMIFPriv
        "tdrl" -> pure HTMIFTdrl
        e -> fromTextError $ "Failure parsing HlsTimedMetadataId3Frame from value: '" <> e
           <> "'. Accepted values: none, priv, tdrl"

instance ToText HlsTimedMetadataId3Frame where
    toText = \case
        HTMIFNone -> "NONE"
        HTMIFPriv -> "PRIV"
        HTMIFTdrl -> "TDRL"

instance Hashable     HlsTimedMetadataId3Frame
instance NFData       HlsTimedMetadataId3Frame
instance ToByteString HlsTimedMetadataId3Frame
instance ToQuery      HlsTimedMetadataId3Frame
instance ToHeader     HlsTimedMetadataId3Frame

instance ToJSON HlsTimedMetadataId3Frame where
    toJSON = toJSONText

instance FromJSON HlsTimedMetadataId3Frame where
    parseJSON = parseJSONText "HlsTimedMetadataId3Frame"

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manaully controllable for MPEG2 and uncompressed video inputs.
data InputDeblockFilter
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputDeblockFilter where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing InputDeblockFilter from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText InputDeblockFilter where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable     InputDeblockFilter
instance NFData       InputDeblockFilter
instance ToByteString InputDeblockFilter
instance ToQuery      InputDeblockFilter
instance ToHeader     InputDeblockFilter

instance ToJSON InputDeblockFilter where
    toJSON = toJSONText

instance FromJSON InputDeblockFilter where
    parseJSON = parseJSONText "InputDeblockFilter"

-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
data InputDenoiseFilter
  = IDFDisabled
  | IDFEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputDenoiseFilter where
    parser = takeLowerText >>= \case
        "disabled" -> pure IDFDisabled
        "enabled" -> pure IDFEnabled
        e -> fromTextError $ "Failure parsing InputDenoiseFilter from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText InputDenoiseFilter where
    toText = \case
        IDFDisabled -> "DISABLED"
        IDFEnabled -> "ENABLED"

instance Hashable     InputDenoiseFilter
instance NFData       InputDenoiseFilter
instance ToByteString InputDenoiseFilter
instance ToQuery      InputDenoiseFilter
instance ToHeader     InputDenoiseFilter

instance ToJSON InputDenoiseFilter where
    toJSON = toJSONText

instance FromJSON InputDenoiseFilter where
    parseJSON = parseJSONText "InputDenoiseFilter"

-- | Use Filter enable (InputFilterEnable) to specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The in put is filtered regardless of input type.
data InputFilterEnable
  = IFEAuto
  | IFEDisable
  | IFEForce
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputFilterEnable where
    parser = takeLowerText >>= \case
        "auto" -> pure IFEAuto
        "disable" -> pure IFEDisable
        "force" -> pure IFEForce
        e -> fromTextError $ "Failure parsing InputFilterEnable from value: '" <> e
           <> "'. Accepted values: auto, disable, force"

instance ToText InputFilterEnable where
    toText = \case
        IFEAuto -> "AUTO"
        IFEDisable -> "DISABLE"
        IFEForce -> "FORCE"

instance Hashable     InputFilterEnable
instance NFData       InputFilterEnable
instance ToByteString InputFilterEnable
instance ToQuery      InputFilterEnable
instance ToHeader     InputFilterEnable

instance ToJSON InputFilterEnable where
    toJSON = toJSONText

instance FromJSON InputFilterEnable where
    parseJSON = parseJSONText "InputFilterEnable"

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
data InputPsiControl
  = IgnorePsi
  | UsePsi
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputPsiControl where
    parser = takeLowerText >>= \case
        "ignore_psi" -> pure IgnorePsi
        "use_psi" -> pure UsePsi
        e -> fromTextError $ "Failure parsing InputPsiControl from value: '" <> e
           <> "'. Accepted values: ignore_psi, use_psi"

instance ToText InputPsiControl where
    toText = \case
        IgnorePsi -> "IGNORE_PSI"
        UsePsi -> "USE_PSI"

instance Hashable     InputPsiControl
instance NFData       InputPsiControl
instance ToByteString InputPsiControl
instance ToQuery      InputPsiControl
instance ToHeader     InputPsiControl

instance ToJSON InputPsiControl where
    toJSON = toJSONText

instance FromJSON InputPsiControl where
    parseJSON = parseJSONText "InputPsiControl"

-- | Use Timecode source (InputTimecodeSource) to specify how timecode information from your input is adjusted and encoded in all outputs for the job. Default is embedded. Set to Embedded (EMBEDDED) to use the timecode that is in the input video. If no embedded timecode is in the source, will set the timecode for the first frame to 00:00:00:00. Set to Start at 0 (ZEROBASED) to set the timecode of the initial frame to 00:00:00:00. Set to Specified start (SPECIFIEDSTART) to provide the initial timecode yourself the setting (Start).
data InputTimecodeSource
  = Embedded
  | Specifiedstart
  | Zerobased
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputTimecodeSource where
    parser = takeLowerText >>= \case
        "embedded" -> pure Embedded
        "specifiedstart" -> pure Specifiedstart
        "zerobased" -> pure Zerobased
        e -> fromTextError $ "Failure parsing InputTimecodeSource from value: '" <> e
           <> "'. Accepted values: embedded, specifiedstart, zerobased"

instance ToText InputTimecodeSource where
    toText = \case
        Embedded -> "EMBEDDED"
        Specifiedstart -> "SPECIFIEDSTART"
        Zerobased -> "ZEROBASED"

instance Hashable     InputTimecodeSource
instance NFData       InputTimecodeSource
instance ToByteString InputTimecodeSource
instance ToQuery      InputTimecodeSource
instance ToHeader     InputTimecodeSource

instance ToJSON InputTimecodeSource where
    toJSON = toJSONText

instance FromJSON InputTimecodeSource where
    parseJSON = parseJSONText "InputTimecodeSource"

-- | A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
data JobStatus
  = Canceled
  | Complete
  | Error'
  | Progressing
  | Submitted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobStatus where
    parser = takeLowerText >>= \case
        "canceled" -> pure Canceled
        "complete" -> pure Complete
        "error" -> pure Error'
        "progressing" -> pure Progressing
        "submitted" -> pure Submitted
        e -> fromTextError $ "Failure parsing JobStatus from value: '" <> e
           <> "'. Accepted values: canceled, complete, error, progressing, submitted"

instance ToText JobStatus where
    toText = \case
        Canceled -> "CANCELED"
        Complete -> "COMPLETE"
        Error' -> "ERROR"
        Progressing -> "PROGRESSING"
        Submitted -> "SUBMITTED"

instance Hashable     JobStatus
instance NFData       JobStatus
instance ToByteString JobStatus
instance ToQuery      JobStatus
instance ToHeader     JobStatus

instance ToJSON JobStatus where
    toJSON = toJSONText

instance FromJSON JobStatus where
    parseJSON = parseJSONText "JobStatus"

-- | Optional. When you request a list of job templates, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
data JobTemplateListBy
  = JTLBCreationDate
  | JTLBName
  | JTLBSystem
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobTemplateListBy where
    parser = takeLowerText >>= \case
        "creation_date" -> pure JTLBCreationDate
        "name" -> pure JTLBName
        "system" -> pure JTLBSystem
        e -> fromTextError $ "Failure parsing JobTemplateListBy from value: '" <> e
           <> "'. Accepted values: creation_date, name, system"

instance ToText JobTemplateListBy where
    toText = \case
        JTLBCreationDate -> "CREATION_DATE"
        JTLBName -> "NAME"
        JTLBSystem -> "SYSTEM"

instance Hashable     JobTemplateListBy
instance NFData       JobTemplateListBy
instance ToByteString JobTemplateListBy
instance ToQuery      JobTemplateListBy
instance ToHeader     JobTemplateListBy

instance ToJSON JobTemplateListBy where
    toJSON = toJSONText

-- | Code to specify the language, following the specification "ISO 639-2 three-digit code":http://www.loc.gov/standards/iso639-2/
data LanguageCode
  = Aar
  | Abk
  | Afr
  | Aka
  | Amh
  | Ara
  | Arg
  | Asm
  | Ava
  | Ave
  | Aym
  | Aze
  | Bak
  | Bam
  | Bel
  | Ben
  | Bih
  | Bis
  | Bod
  | Bos
  | Bre
  | Bul
  | Cat
  | Ces
  | Cha
  | Che
  | Chu
  | Chv
  | Cor
  | Cos
  | Cre
  | Cym
  | Dan
  | Deu
  | Div
  | Dzo
  | Ell
  | Eng
  | Enm
  | Epo
  | Est
  | Eus
  | Ewe
  | Fao
  | Fas
  | Fij
  | Fin
  | Fra
  | Frm
  | Fry
  | Ful
  | Ger
  | Gla
  | Gle
  | Glg
  | Glv
  | Grn
  | Guj
  | Hat
  | Hau
  | Heb
  | Her
  | Hin
  | Hmo
  | Hrv
  | Hun
  | Hye
  | IPk
  | Ibo
  | Ido
  | Iii
  | Iku
  | Ile
  | Ina
  | Ind
  | Isl
  | Ita
  | Jav
  | Jpn
  | Kal
  | Kan
  | Kas
  | Kat
  | Kau
  | Kaz
  | Khm
  | Kik
  | Kin
  | Kir
  | Kom
  | Kon
  | Kor
  | Kua
  | Kur
  | Lao
  | Lat
  | Lav
  | Lim
  | Lin
  | Lit
  | Ltz
  | Lub
  | Lug
  | Mah
  | Mal
  | Mar
  | Mkd
  | Mlg
  | Mlt
  | Mon
  | Mri
  | Msa
  | Mya
  | Nau
  | Nav
  | Nbl
  | Nde
  | Ndo
  | Nep
  | Nld
  | Nno
  | Nob
  | Nor
  | Nya
  | OSs
  | Oci
  | Oji
  | Ori
  | Orj
  | Orm
  | Pan
  | Pli
  | Pol
  | Por
  | Pus
  | Qaa
  | Qpc
  | Que
  | Roh
  | Ron
  | Run
  | Rus
  | Sag
  | San
  | Sin
  | Slk
  | Slv
  | Sme
  | Smo
  | Sna
  | Snd
  | Som
  | Sot
  | Spa
  | Sqi
  | Srb
  | Srd
  | Ssw
  | Sun
  | Swa
  | Swe
  | Tah
  | Tam
  | Tat
  | Tel
  | Tgk
  | Tgl
  | Tha
  | Tir
  | Tng
  | Ton
  | Tsn
  | Tso
  | Tuk
  | Tur
  | Twi
  | Uig
  | Ukr
  | Urd
  | Uzb
  | Ven
  | Vie
  | Vol
  | Wln
  | Wol
  | Xho
  | Yid
  | Yor
  | Zha
  | Zho
  | Zul
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LanguageCode where
    parser = takeLowerText >>= \case
        "aar" -> pure Aar
        "abk" -> pure Abk
        "afr" -> pure Afr
        "aka" -> pure Aka
        "amh" -> pure Amh
        "ara" -> pure Ara
        "arg" -> pure Arg
        "asm" -> pure Asm
        "ava" -> pure Ava
        "ave" -> pure Ave
        "aym" -> pure Aym
        "aze" -> pure Aze
        "bak" -> pure Bak
        "bam" -> pure Bam
        "bel" -> pure Bel
        "ben" -> pure Ben
        "bih" -> pure Bih
        "bis" -> pure Bis
        "bod" -> pure Bod
        "bos" -> pure Bos
        "bre" -> pure Bre
        "bul" -> pure Bul
        "cat" -> pure Cat
        "ces" -> pure Ces
        "cha" -> pure Cha
        "che" -> pure Che
        "chu" -> pure Chu
        "chv" -> pure Chv
        "cor" -> pure Cor
        "cos" -> pure Cos
        "cre" -> pure Cre
        "cym" -> pure Cym
        "dan" -> pure Dan
        "deu" -> pure Deu
        "div" -> pure Div
        "dzo" -> pure Dzo
        "ell" -> pure Ell
        "eng" -> pure Eng
        "enm" -> pure Enm
        "epo" -> pure Epo
        "est" -> pure Est
        "eus" -> pure Eus
        "ewe" -> pure Ewe
        "fao" -> pure Fao
        "fas" -> pure Fas
        "fij" -> pure Fij
        "fin" -> pure Fin
        "fra" -> pure Fra
        "frm" -> pure Frm
        "fry" -> pure Fry
        "ful" -> pure Ful
        "ger" -> pure Ger
        "gla" -> pure Gla
        "gle" -> pure Gle
        "glg" -> pure Glg
        "glv" -> pure Glv
        "grn" -> pure Grn
        "guj" -> pure Guj
        "hat" -> pure Hat
        "hau" -> pure Hau
        "heb" -> pure Heb
        "her" -> pure Her
        "hin" -> pure Hin
        "hmo" -> pure Hmo
        "hrv" -> pure Hrv
        "hun" -> pure Hun
        "hye" -> pure Hye
        "ipk" -> pure IPk
        "ibo" -> pure Ibo
        "ido" -> pure Ido
        "iii" -> pure Iii
        "iku" -> pure Iku
        "ile" -> pure Ile
        "ina" -> pure Ina
        "ind" -> pure Ind
        "isl" -> pure Isl
        "ita" -> pure Ita
        "jav" -> pure Jav
        "jpn" -> pure Jpn
        "kal" -> pure Kal
        "kan" -> pure Kan
        "kas" -> pure Kas
        "kat" -> pure Kat
        "kau" -> pure Kau
        "kaz" -> pure Kaz
        "khm" -> pure Khm
        "kik" -> pure Kik
        "kin" -> pure Kin
        "kir" -> pure Kir
        "kom" -> pure Kom
        "kon" -> pure Kon
        "kor" -> pure Kor
        "kua" -> pure Kua
        "kur" -> pure Kur
        "lao" -> pure Lao
        "lat" -> pure Lat
        "lav" -> pure Lav
        "lim" -> pure Lim
        "lin" -> pure Lin
        "lit" -> pure Lit
        "ltz" -> pure Ltz
        "lub" -> pure Lub
        "lug" -> pure Lug
        "mah" -> pure Mah
        "mal" -> pure Mal
        "mar" -> pure Mar
        "mkd" -> pure Mkd
        "mlg" -> pure Mlg
        "mlt" -> pure Mlt
        "mon" -> pure Mon
        "mri" -> pure Mri
        "msa" -> pure Msa
        "mya" -> pure Mya
        "nau" -> pure Nau
        "nav" -> pure Nav
        "nbl" -> pure Nbl
        "nde" -> pure Nde
        "ndo" -> pure Ndo
        "nep" -> pure Nep
        "nld" -> pure Nld
        "nno" -> pure Nno
        "nob" -> pure Nob
        "nor" -> pure Nor
        "nya" -> pure Nya
        "oss" -> pure OSs
        "oci" -> pure Oci
        "oji" -> pure Oji
        "ori" -> pure Ori
        "orj" -> pure Orj
        "orm" -> pure Orm
        "pan" -> pure Pan
        "pli" -> pure Pli
        "pol" -> pure Pol
        "por" -> pure Por
        "pus" -> pure Pus
        "qaa" -> pure Qaa
        "qpc" -> pure Qpc
        "que" -> pure Que
        "roh" -> pure Roh
        "ron" -> pure Ron
        "run" -> pure Run
        "rus" -> pure Rus
        "sag" -> pure Sag
        "san" -> pure San
        "sin" -> pure Sin
        "slk" -> pure Slk
        "slv" -> pure Slv
        "sme" -> pure Sme
        "smo" -> pure Smo
        "sna" -> pure Sna
        "snd" -> pure Snd
        "som" -> pure Som
        "sot" -> pure Sot
        "spa" -> pure Spa
        "sqi" -> pure Sqi
        "srb" -> pure Srb
        "srd" -> pure Srd
        "ssw" -> pure Ssw
        "sun" -> pure Sun
        "swa" -> pure Swa
        "swe" -> pure Swe
        "tah" -> pure Tah
        "tam" -> pure Tam
        "tat" -> pure Tat
        "tel" -> pure Tel
        "tgk" -> pure Tgk
        "tgl" -> pure Tgl
        "tha" -> pure Tha
        "tir" -> pure Tir
        "tng" -> pure Tng
        "ton" -> pure Ton
        "tsn" -> pure Tsn
        "tso" -> pure Tso
        "tuk" -> pure Tuk
        "tur" -> pure Tur
        "twi" -> pure Twi
        "uig" -> pure Uig
        "ukr" -> pure Ukr
        "urd" -> pure Urd
        "uzb" -> pure Uzb
        "ven" -> pure Ven
        "vie" -> pure Vie
        "vol" -> pure Vol
        "wln" -> pure Wln
        "wol" -> pure Wol
        "xho" -> pure Xho
        "yid" -> pure Yid
        "yor" -> pure Yor
        "zha" -> pure Zha
        "zho" -> pure Zho
        "zul" -> pure Zul
        e -> fromTextError $ "Failure parsing LanguageCode from value: '" <> e
           <> "'. Accepted values: aar, abk, afr, aka, amh, ara, arg, asm, ava, ave, aym, aze, bak, bam, bel, ben, bih, bis, bod, bos, bre, bul, cat, ces, cha, che, chu, chv, cor, cos, cre, cym, dan, deu, div, dzo, ell, eng, enm, epo, est, eus, ewe, fao, fas, fij, fin, fra, frm, fry, ful, ger, gla, gle, glg, glv, grn, guj, hat, hau, heb, her, hin, hmo, hrv, hun, hye, ipk, ibo, ido, iii, iku, ile, ina, ind, isl, ita, jav, jpn, kal, kan, kas, kat, kau, kaz, khm, kik, kin, kir, kom, kon, kor, kua, kur, lao, lat, lav, lim, lin, lit, ltz, lub, lug, mah, mal, mar, mkd, mlg, mlt, mon, mri, msa, mya, nau, nav, nbl, nde, ndo, nep, nld, nno, nob, nor, nya, oss, oci, oji, ori, orj, orm, pan, pli, pol, por, pus, qaa, qpc, que, roh, ron, run, rus, sag, san, sin, slk, slv, sme, smo, sna, snd, som, sot, spa, sqi, srb, srd, ssw, sun, swa, swe, tah, tam, tat, tel, tgk, tgl, tha, tir, tng, ton, tsn, tso, tuk, tur, twi, uig, ukr, urd, uzb, ven, vie, vol, wln, wol, xho, yid, yor, zha, zho, zul"

instance ToText LanguageCode where
    toText = \case
        Aar -> "AAR"
        Abk -> "ABK"
        Afr -> "AFR"
        Aka -> "AKA"
        Amh -> "AMH"
        Ara -> "ARA"
        Arg -> "ARG"
        Asm -> "ASM"
        Ava -> "AVA"
        Ave -> "AVE"
        Aym -> "AYM"
        Aze -> "AZE"
        Bak -> "BAK"
        Bam -> "BAM"
        Bel -> "BEL"
        Ben -> "BEN"
        Bih -> "BIH"
        Bis -> "BIS"
        Bod -> "BOD"
        Bos -> "BOS"
        Bre -> "BRE"
        Bul -> "BUL"
        Cat -> "CAT"
        Ces -> "CES"
        Cha -> "CHA"
        Che -> "CHE"
        Chu -> "CHU"
        Chv -> "CHV"
        Cor -> "COR"
        Cos -> "COS"
        Cre -> "CRE"
        Cym -> "CYM"
        Dan -> "DAN"
        Deu -> "DEU"
        Div -> "DIV"
        Dzo -> "DZO"
        Ell -> "ELL"
        Eng -> "ENG"
        Enm -> "ENM"
        Epo -> "EPO"
        Est -> "EST"
        Eus -> "EUS"
        Ewe -> "EWE"
        Fao -> "FAO"
        Fas -> "FAS"
        Fij -> "FIJ"
        Fin -> "FIN"
        Fra -> "FRA"
        Frm -> "FRM"
        Fry -> "FRY"
        Ful -> "FUL"
        Ger -> "GER"
        Gla -> "GLA"
        Gle -> "GLE"
        Glg -> "GLG"
        Glv -> "GLV"
        Grn -> "GRN"
        Guj -> "GUJ"
        Hat -> "HAT"
        Hau -> "HAU"
        Heb -> "HEB"
        Her -> "HER"
        Hin -> "HIN"
        Hmo -> "HMO"
        Hrv -> "HRV"
        Hun -> "HUN"
        Hye -> "HYE"
        IPk -> "IPK"
        Ibo -> "IBO"
        Ido -> "IDO"
        Iii -> "III"
        Iku -> "IKU"
        Ile -> "ILE"
        Ina -> "INA"
        Ind -> "IND"
        Isl -> "ISL"
        Ita -> "ITA"
        Jav -> "JAV"
        Jpn -> "JPN"
        Kal -> "KAL"
        Kan -> "KAN"
        Kas -> "KAS"
        Kat -> "KAT"
        Kau -> "KAU"
        Kaz -> "KAZ"
        Khm -> "KHM"
        Kik -> "KIK"
        Kin -> "KIN"
        Kir -> "KIR"
        Kom -> "KOM"
        Kon -> "KON"
        Kor -> "KOR"
        Kua -> "KUA"
        Kur -> "KUR"
        Lao -> "LAO"
        Lat -> "LAT"
        Lav -> "LAV"
        Lim -> "LIM"
        Lin -> "LIN"
        Lit -> "LIT"
        Ltz -> "LTZ"
        Lub -> "LUB"
        Lug -> "LUG"
        Mah -> "MAH"
        Mal -> "MAL"
        Mar -> "MAR"
        Mkd -> "MKD"
        Mlg -> "MLG"
        Mlt -> "MLT"
        Mon -> "MON"
        Mri -> "MRI"
        Msa -> "MSA"
        Mya -> "MYA"
        Nau -> "NAU"
        Nav -> "NAV"
        Nbl -> "NBL"
        Nde -> "NDE"
        Ndo -> "NDO"
        Nep -> "NEP"
        Nld -> "NLD"
        Nno -> "NNO"
        Nob -> "NOB"
        Nor -> "NOR"
        Nya -> "NYA"
        OSs -> "OSS"
        Oci -> "OCI"
        Oji -> "OJI"
        Ori -> "ORI"
        Orj -> "ORJ"
        Orm -> "ORM"
        Pan -> "PAN"
        Pli -> "PLI"
        Pol -> "POL"
        Por -> "POR"
        Pus -> "PUS"
        Qaa -> "QAA"
        Qpc -> "QPC"
        Que -> "QUE"
        Roh -> "ROH"
        Ron -> "RON"
        Run -> "RUN"
        Rus -> "RUS"
        Sag -> "SAG"
        San -> "SAN"
        Sin -> "SIN"
        Slk -> "SLK"
        Slv -> "SLV"
        Sme -> "SME"
        Smo -> "SMO"
        Sna -> "SNA"
        Snd -> "SND"
        Som -> "SOM"
        Sot -> "SOT"
        Spa -> "SPA"
        Sqi -> "SQI"
        Srb -> "SRB"
        Srd -> "SRD"
        Ssw -> "SSW"
        Sun -> "SUN"
        Swa -> "SWA"
        Swe -> "SWE"
        Tah -> "TAH"
        Tam -> "TAM"
        Tat -> "TAT"
        Tel -> "TEL"
        Tgk -> "TGK"
        Tgl -> "TGL"
        Tha -> "THA"
        Tir -> "TIR"
        Tng -> "TNG"
        Ton -> "TON"
        Tsn -> "TSN"
        Tso -> "TSO"
        Tuk -> "TUK"
        Tur -> "TUR"
        Twi -> "TWI"
        Uig -> "UIG"
        Ukr -> "UKR"
        Urd -> "URD"
        Uzb -> "UZB"
        Ven -> "VEN"
        Vie -> "VIE"
        Vol -> "VOL"
        Wln -> "WLN"
        Wol -> "WOL"
        Xho -> "XHO"
        Yid -> "YID"
        Yor -> "YOR"
        Zha -> "ZHA"
        Zho -> "ZHO"
        Zul -> "ZUL"

instance Hashable     LanguageCode
instance NFData       LanguageCode
instance ToByteString LanguageCode
instance ToQuery      LanguageCode
instance ToHeader     LanguageCode

instance ToJSON LanguageCode where
    toJSON = toJSONText

instance FromJSON LanguageCode where
    parseJSON = parseJSONText "LanguageCode"

-- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
data M2tsAudioBufferModel
  = Atsc
  | Dvb
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsAudioBufferModel where
    parser = takeLowerText >>= \case
        "atsc" -> pure Atsc
        "dvb" -> pure Dvb
        e -> fromTextError $ "Failure parsing M2tsAudioBufferModel from value: '" <> e
           <> "'. Accepted values: atsc, dvb"

instance ToText M2tsAudioBufferModel where
    toText = \case
        Atsc -> "ATSC"
        Dvb -> "DVB"

instance Hashable     M2tsAudioBufferModel
instance NFData       M2tsAudioBufferModel
instance ToByteString M2tsAudioBufferModel
instance ToQuery      M2tsAudioBufferModel
instance ToHeader     M2tsAudioBufferModel

instance ToJSON M2tsAudioBufferModel where
    toJSON = toJSONText

instance FromJSON M2tsAudioBufferModel where
    parseJSON = parseJSONText "M2tsAudioBufferModel"

-- | Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
data M2tsBufferModel
  = MBMMultiplex
  | MBMNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsBufferModel where
    parser = takeLowerText >>= \case
        "multiplex" -> pure MBMMultiplex
        "none" -> pure MBMNone
        e -> fromTextError $ "Failure parsing M2tsBufferModel from value: '" <> e
           <> "'. Accepted values: multiplex, none"

instance ToText M2tsBufferModel where
    toText = \case
        MBMMultiplex -> "MULTIPLEX"
        MBMNone -> "NONE"

instance Hashable     M2tsBufferModel
instance NFData       M2tsBufferModel
instance ToByteString M2tsBufferModel
instance ToQuery      M2tsBufferModel
instance ToHeader     M2tsBufferModel

instance ToJSON M2tsBufferModel where
    toJSON = toJSONText

instance FromJSON M2tsBufferModel where
    parseJSON = parseJSONText "M2tsBufferModel"

-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. When set to VIDEO_INTERVAL, these additional markers will not be inserted. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
data M2tsEbpAudioInterval
  = VideoAndFixedIntervals
  | VideoInterval
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsEbpAudioInterval where
    parser = takeLowerText >>= \case
        "video_and_fixed_intervals" -> pure VideoAndFixedIntervals
        "video_interval" -> pure VideoInterval
        e -> fromTextError $ "Failure parsing M2tsEbpAudioInterval from value: '" <> e
           <> "'. Accepted values: video_and_fixed_intervals, video_interval"

instance ToText M2tsEbpAudioInterval where
    toText = \case
        VideoAndFixedIntervals -> "VIDEO_AND_FIXED_INTERVALS"
        VideoInterval -> "VIDEO_INTERVAL"

instance Hashable     M2tsEbpAudioInterval
instance NFData       M2tsEbpAudioInterval
instance ToByteString M2tsEbpAudioInterval
instance ToQuery      M2tsEbpAudioInterval
instance ToHeader     M2tsEbpAudioInterval

instance ToJSON M2tsEbpAudioInterval where
    toJSON = toJSONText

instance FromJSON M2tsEbpAudioInterval where
    parseJSON = parseJSONText "M2tsEbpAudioInterval"

-- | Selects which PIDs to place EBP markers on. They can either be placed only on the video PID, or on both the video PID and all audio PIDs. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
data M2tsEbpPlacement
  = VideoAndAudioPids
  | VideoPid
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsEbpPlacement where
    parser = takeLowerText >>= \case
        "video_and_audio_pids" -> pure VideoAndAudioPids
        "video_pid" -> pure VideoPid
        e -> fromTextError $ "Failure parsing M2tsEbpPlacement from value: '" <> e
           <> "'. Accepted values: video_and_audio_pids, video_pid"

instance ToText M2tsEbpPlacement where
    toText = \case
        VideoAndAudioPids -> "VIDEO_AND_AUDIO_PIDS"
        VideoPid -> "VIDEO_PID"

instance Hashable     M2tsEbpPlacement
instance NFData       M2tsEbpPlacement
instance ToByteString M2tsEbpPlacement
instance ToQuery      M2tsEbpPlacement
instance ToHeader     M2tsEbpPlacement

instance ToJSON M2tsEbpPlacement where
    toJSON = toJSONText

instance FromJSON M2tsEbpPlacement where
    parseJSON = parseJSONText "M2tsEbpPlacement"

-- | Controls whether to include the ES Rate field in the PES header.
data M2tsEsRateInPes
  = MERIPExclude
  | MERIPInclude
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsEsRateInPes where
    parser = takeLowerText >>= \case
        "exclude" -> pure MERIPExclude
        "include" -> pure MERIPInclude
        e -> fromTextError $ "Failure parsing M2tsEsRateInPes from value: '" <> e
           <> "'. Accepted values: exclude, include"

instance ToText M2tsEsRateInPes where
    toText = \case
        MERIPExclude -> "EXCLUDE"
        MERIPInclude -> "INCLUDE"

instance Hashable     M2tsEsRateInPes
instance NFData       M2tsEsRateInPes
instance ToByteString M2tsEsRateInPes
instance ToQuery      M2tsEsRateInPes
instance ToHeader     M2tsEsRateInPes

instance ToJSON M2tsEsRateInPes where
    toJSON = toJSONText

instance FromJSON M2tsEsRateInPes where
    parseJSON = parseJSONText "M2tsEsRateInPes"

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
data M2tsNielsenId3
  = MNIInsert
  | MNINone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsNielsenId3 where
    parser = takeLowerText >>= \case
        "insert" -> pure MNIInsert
        "none" -> pure MNINone
        e -> fromTextError $ "Failure parsing M2tsNielsenId3 from value: '" <> e
           <> "'. Accepted values: insert, none"

instance ToText M2tsNielsenId3 where
    toText = \case
        MNIInsert -> "INSERT"
        MNINone -> "NONE"

instance Hashable     M2tsNielsenId3
instance NFData       M2tsNielsenId3
instance ToByteString M2tsNielsenId3
instance ToQuery      M2tsNielsenId3
instance ToHeader     M2tsNielsenId3

instance ToJSON M2tsNielsenId3 where
    toJSON = toJSONText

instance FromJSON M2tsNielsenId3 where
    parseJSON = parseJSONText "M2tsNielsenId3"

-- | When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This is effective only when the PCR PID is the same as the video or audio elementary stream.
data M2tsPcrControl
  = ConfiguredPcrPeriod
  | PcrEveryPesPacket
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsPcrControl where
    parser = takeLowerText >>= \case
        "configured_pcr_period" -> pure ConfiguredPcrPeriod
        "pcr_every_pes_packet" -> pure PcrEveryPesPacket
        e -> fromTextError $ "Failure parsing M2tsPcrControl from value: '" <> e
           <> "'. Accepted values: configured_pcr_period, pcr_every_pes_packet"

instance ToText M2tsPcrControl where
    toText = \case
        ConfiguredPcrPeriod -> "CONFIGURED_PCR_PERIOD"
        PcrEveryPesPacket -> "PCR_EVERY_PES_PACKET"

instance Hashable     M2tsPcrControl
instance NFData       M2tsPcrControl
instance ToByteString M2tsPcrControl
instance ToQuery      M2tsPcrControl
instance ToHeader     M2tsPcrControl

instance ToJSON M2tsPcrControl where
    toJSON = toJSONText

instance FromJSON M2tsPcrControl where
    parseJSON = parseJSONText "M2tsPcrControl"

-- | When set to CBR, inserts null packets into transport stream to fill specified bitrate. When set to VBR, the bitrate setting acts as the maximum bitrate, but the output will not be padded up to that bitrate.
data M2tsRateMode
  = MRMCbr
  | MRMVbr
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsRateMode where
    parser = takeLowerText >>= \case
        "cbr" -> pure MRMCbr
        "vbr" -> pure MRMVbr
        e -> fromTextError $ "Failure parsing M2tsRateMode from value: '" <> e
           <> "'. Accepted values: cbr, vbr"

instance ToText M2tsRateMode where
    toText = \case
        MRMCbr -> "CBR"
        MRMVbr -> "VBR"

instance Hashable     M2tsRateMode
instance NFData       M2tsRateMode
instance ToByteString M2tsRateMode
instance ToQuery      M2tsRateMode
instance ToHeader     M2tsRateMode

instance ToJSON M2tsRateMode where
    toJSON = toJSONText

instance FromJSON M2tsRateMode where
    parseJSON = parseJSONText "M2tsRateMode"

-- | Enables SCTE-35 passthrough (scte35Source) to pass any SCTE-35 signals from input to output.
data M2tsScte35Source
  = MSSNone
  | MSSPassthrough
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsScte35Source where
    parser = takeLowerText >>= \case
        "none" -> pure MSSNone
        "passthrough" -> pure MSSPassthrough
        e -> fromTextError $ "Failure parsing M2tsScte35Source from value: '" <> e
           <> "'. Accepted values: none, passthrough"

instance ToText M2tsScte35Source where
    toText = \case
        MSSNone -> "NONE"
        MSSPassthrough -> "PASSTHROUGH"

instance Hashable     M2tsScte35Source
instance NFData       M2tsScte35Source
instance ToByteString M2tsScte35Source
instance ToQuery      M2tsScte35Source
instance ToHeader     M2tsScte35Source

instance ToJSON M2tsScte35Source where
    toJSON = toJSONText

instance FromJSON M2tsScte35Source where
    parseJSON = parseJSONText "M2tsScte35Source"

-- | Inserts segmentation markers at each segmentation_time period. rai_segstart sets the Random Access Indicator bit in the adaptation field. rai_adapt sets the RAI bit and adds the current timecode in the private data bytes. psi_segstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebp_legacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
data M2tsSegmentationMarkers
  = MSMEbp
  | MSMEbpLegacy
  | MSMNone
  | MSMPsiSegstart
  | MSMRaiAdapt
  | MSMRaiSegstart
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsSegmentationMarkers where
    parser = takeLowerText >>= \case
        "ebp" -> pure MSMEbp
        "ebp_legacy" -> pure MSMEbpLegacy
        "none" -> pure MSMNone
        "psi_segstart" -> pure MSMPsiSegstart
        "rai_adapt" -> pure MSMRaiAdapt
        "rai_segstart" -> pure MSMRaiSegstart
        e -> fromTextError $ "Failure parsing M2tsSegmentationMarkers from value: '" <> e
           <> "'. Accepted values: ebp, ebp_legacy, none, psi_segstart, rai_adapt, rai_segstart"

instance ToText M2tsSegmentationMarkers where
    toText = \case
        MSMEbp -> "EBP"
        MSMEbpLegacy -> "EBP_LEGACY"
        MSMNone -> "NONE"
        MSMPsiSegstart -> "PSI_SEGSTART"
        MSMRaiAdapt -> "RAI_ADAPT"
        MSMRaiSegstart -> "RAI_SEGSTART"

instance Hashable     M2tsSegmentationMarkers
instance NFData       M2tsSegmentationMarkers
instance ToByteString M2tsSegmentationMarkers
instance ToQuery      M2tsSegmentationMarkers
instance ToHeader     M2tsSegmentationMarkers

instance ToJSON M2tsSegmentationMarkers where
    toJSON = toJSONText

instance FromJSON M2tsSegmentationMarkers where
    parseJSON = parseJSONText "M2tsSegmentationMarkers"

-- | The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "reset_cadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of of $segmentation_time seconds. When a segmentation style of "maintain_cadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentation_time seconds. Note that EBP lookahead is a slight exception to this rule.
data M2tsSegmentationStyle
  = MaintainCadence
  | ResetCadence
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsSegmentationStyle where
    parser = takeLowerText >>= \case
        "maintain_cadence" -> pure MaintainCadence
        "reset_cadence" -> pure ResetCadence
        e -> fromTextError $ "Failure parsing M2tsSegmentationStyle from value: '" <> e
           <> "'. Accepted values: maintain_cadence, reset_cadence"

instance ToText M2tsSegmentationStyle where
    toText = \case
        MaintainCadence -> "MAINTAIN_CADENCE"
        ResetCadence -> "RESET_CADENCE"

instance Hashable     M2tsSegmentationStyle
instance NFData       M2tsSegmentationStyle
instance ToByteString M2tsSegmentationStyle
instance ToQuery      M2tsSegmentationStyle
instance ToHeader     M2tsSegmentationStyle

instance ToJSON M2tsSegmentationStyle where
    toJSON = toJSONText

instance FromJSON M2tsSegmentationStyle where
    parseJSON = parseJSONText "M2tsSegmentationStyle"

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
data M3u8NielsenId3
  = M3uInsert
  | M3uNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M3u8NielsenId3 where
    parser = takeLowerText >>= \case
        "insert" -> pure M3uInsert
        "none" -> pure M3uNone
        e -> fromTextError $ "Failure parsing M3u8NielsenId3 from value: '" <> e
           <> "'. Accepted values: insert, none"

instance ToText M3u8NielsenId3 where
    toText = \case
        M3uInsert -> "INSERT"
        M3uNone -> "NONE"

instance Hashable     M3u8NielsenId3
instance NFData       M3u8NielsenId3
instance ToByteString M3u8NielsenId3
instance ToQuery      M3u8NielsenId3
instance ToHeader     M3u8NielsenId3

instance ToJSON M3u8NielsenId3 where
    toJSON = toJSONText

instance FromJSON M3u8NielsenId3 where
    parseJSON = parseJSONText "M3u8NielsenId3"

-- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
data M3u8PcrControl
  = MPCConfiguredPcrPeriod
  | MPCPcrEveryPesPacket
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M3u8PcrControl where
    parser = takeLowerText >>= \case
        "configured_pcr_period" -> pure MPCConfiguredPcrPeriod
        "pcr_every_pes_packet" -> pure MPCPcrEveryPesPacket
        e -> fromTextError $ "Failure parsing M3u8PcrControl from value: '" <> e
           <> "'. Accepted values: configured_pcr_period, pcr_every_pes_packet"

instance ToText M3u8PcrControl where
    toText = \case
        MPCConfiguredPcrPeriod -> "CONFIGURED_PCR_PERIOD"
        MPCPcrEveryPesPacket -> "PCR_EVERY_PES_PACKET"

instance Hashable     M3u8PcrControl
instance NFData       M3u8PcrControl
instance ToByteString M3u8PcrControl
instance ToQuery      M3u8PcrControl
instance ToHeader     M3u8PcrControl

instance ToJSON M3u8PcrControl where
    toJSON = toJSONText

instance FromJSON M3u8PcrControl where
    parseJSON = parseJSONText "M3u8PcrControl"

-- | Enables SCTE-35 passthrough (scte35Source) to pass any SCTE-35 signals from input to output.
data M3u8Scte35Source
  = MNone
  | MPassthrough
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M3u8Scte35Source where
    parser = takeLowerText >>= \case
        "none" -> pure MNone
        "passthrough" -> pure MPassthrough
        e -> fromTextError $ "Failure parsing M3u8Scte35Source from value: '" <> e
           <> "'. Accepted values: none, passthrough"

instance ToText M3u8Scte35Source where
    toText = \case
        MNone -> "NONE"
        MPassthrough -> "PASSTHROUGH"

instance Hashable     M3u8Scte35Source
instance NFData       M3u8Scte35Source
instance ToByteString M3u8Scte35Source
instance ToQuery      M3u8Scte35Source
instance ToHeader     M3u8Scte35Source

instance ToJSON M3u8Scte35Source where
    toJSON = toJSONText

instance FromJSON M3u8Scte35Source where
    parseJSON = parseJSONText "M3u8Scte35Source"

-- | When enabled, include 'clap' atom if appropriate for the video output settings.
data MovClapAtom
  = MExclude
  | MInclude
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MovClapAtom where
    parser = takeLowerText >>= \case
        "exclude" -> pure MExclude
        "include" -> pure MInclude
        e -> fromTextError $ "Failure parsing MovClapAtom from value: '" <> e
           <> "'. Accepted values: exclude, include"

instance ToText MovClapAtom where
    toText = \case
        MExclude -> "EXCLUDE"
        MInclude -> "INCLUDE"

instance Hashable     MovClapAtom
instance NFData       MovClapAtom
instance ToByteString MovClapAtom
instance ToQuery      MovClapAtom
instance ToHeader     MovClapAtom

instance ToJSON MovClapAtom where
    toJSON = toJSONText

instance FromJSON MovClapAtom where
    parseJSON = parseJSONText "MovClapAtom"

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
data MovCslgAtom
  = MCAExclude
  | MCAInclude
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MovCslgAtom where
    parser = takeLowerText >>= \case
        "exclude" -> pure MCAExclude
        "include" -> pure MCAInclude
        e -> fromTextError $ "Failure parsing MovCslgAtom from value: '" <> e
           <> "'. Accepted values: exclude, include"

instance ToText MovCslgAtom where
    toText = \case
        MCAExclude -> "EXCLUDE"
        MCAInclude -> "INCLUDE"

instance Hashable     MovCslgAtom
instance NFData       MovCslgAtom
instance ToByteString MovCslgAtom
instance ToQuery      MovCslgAtom
instance ToHeader     MovCslgAtom

instance ToJSON MovCslgAtom where
    toJSON = toJSONText

instance FromJSON MovCslgAtom where
    parseJSON = parseJSONText "MovCslgAtom"

-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
data MovMpeg2FourCCControl
  = Mpeg
  | Xdcam
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MovMpeg2FourCCControl where
    parser = takeLowerText >>= \case
        "mpeg" -> pure Mpeg
        "xdcam" -> pure Xdcam
        e -> fromTextError $ "Failure parsing MovMpeg2FourCCControl from value: '" <> e
           <> "'. Accepted values: mpeg, xdcam"

instance ToText MovMpeg2FourCCControl where
    toText = \case
        Mpeg -> "MPEG"
        Xdcam -> "XDCAM"

instance Hashable     MovMpeg2FourCCControl
instance NFData       MovMpeg2FourCCControl
instance ToByteString MovMpeg2FourCCControl
instance ToQuery      MovMpeg2FourCCControl
instance ToHeader     MovMpeg2FourCCControl

instance ToJSON MovMpeg2FourCCControl where
    toJSON = toJSONText

instance FromJSON MovMpeg2FourCCControl where
    parseJSON = parseJSONText "MovMpeg2FourCCControl"

-- | If set to OMNEON, inserts Omneon-compatible padding
data MovPaddingControl
  = MPCNone
  | MPCOmneon
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MovPaddingControl where
    parser = takeLowerText >>= \case
        "none" -> pure MPCNone
        "omneon" -> pure MPCOmneon
        e -> fromTextError $ "Failure parsing MovPaddingControl from value: '" <> e
           <> "'. Accepted values: none, omneon"

instance ToText MovPaddingControl where
    toText = \case
        MPCNone -> "NONE"
        MPCOmneon -> "OMNEON"

instance Hashable     MovPaddingControl
instance NFData       MovPaddingControl
instance ToByteString MovPaddingControl
instance ToQuery      MovPaddingControl
instance ToHeader     MovPaddingControl

instance ToJSON MovPaddingControl where
    toJSON = toJSONText

instance FromJSON MovPaddingControl where
    parseJSON = parseJSONText "MovPaddingControl"

-- | A value of 'external' creates separate media files and the wrapper file (.mov) contains references to these media files. A value of 'self_contained' creates only a wrapper (.mov) file and this file contains all of the media.
data MovReference
  = External
  | SelfContained
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MovReference where
    parser = takeLowerText >>= \case
        "external" -> pure External
        "self_contained" -> pure SelfContained
        e -> fromTextError $ "Failure parsing MovReference from value: '" <> e
           <> "'. Accepted values: external, self_contained"

instance ToText MovReference where
    toText = \case
        External -> "EXTERNAL"
        SelfContained -> "SELF_CONTAINED"

instance Hashable     MovReference
instance NFData       MovReference
instance ToByteString MovReference
instance ToQuery      MovReference
instance ToHeader     MovReference

instance ToJSON MovReference where
    toJSON = toJSONText

instance FromJSON MovReference where
    parseJSON = parseJSONText "MovReference"

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
data Mp4CslgAtom
  = Mp4Exclude
  | Mp4Include
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mp4CslgAtom where
    parser = takeLowerText >>= \case
        "exclude" -> pure Mp4Exclude
        "include" -> pure Mp4Include
        e -> fromTextError $ "Failure parsing Mp4CslgAtom from value: '" <> e
           <> "'. Accepted values: exclude, include"

instance ToText Mp4CslgAtom where
    toText = \case
        Mp4Exclude -> "EXCLUDE"
        Mp4Include -> "INCLUDE"

instance Hashable     Mp4CslgAtom
instance NFData       Mp4CslgAtom
instance ToByteString Mp4CslgAtom
instance ToQuery      Mp4CslgAtom
instance ToHeader     Mp4CslgAtom

instance ToJSON Mp4CslgAtom where
    toJSON = toJSONText

instance FromJSON Mp4CslgAtom where
    parseJSON = parseJSONText "Mp4CslgAtom"

-- | Inserts a free-space box immediately after the moov box.
data Mp4FreeSpaceBox
  = MFSBExclude
  | MFSBInclude
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mp4FreeSpaceBox where
    parser = takeLowerText >>= \case
        "exclude" -> pure MFSBExclude
        "include" -> pure MFSBInclude
        e -> fromTextError $ "Failure parsing Mp4FreeSpaceBox from value: '" <> e
           <> "'. Accepted values: exclude, include"

instance ToText Mp4FreeSpaceBox where
    toText = \case
        MFSBExclude -> "EXCLUDE"
        MFSBInclude -> "INCLUDE"

instance Hashable     Mp4FreeSpaceBox
instance NFData       Mp4FreeSpaceBox
instance ToByteString Mp4FreeSpaceBox
instance ToQuery      Mp4FreeSpaceBox
instance ToHeader     Mp4FreeSpaceBox

instance ToJSON Mp4FreeSpaceBox where
    toJSON = toJSONText

instance FromJSON Mp4FreeSpaceBox where
    parseJSON = parseJSONText "Mp4FreeSpaceBox"

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
data Mp4MoovPlacement
  = MMPNormal
  | MMPProgressiveDownload
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mp4MoovPlacement where
    parser = takeLowerText >>= \case
        "normal" -> pure MMPNormal
        "progressive_download" -> pure MMPProgressiveDownload
        e -> fromTextError $ "Failure parsing Mp4MoovPlacement from value: '" <> e
           <> "'. Accepted values: normal, progressive_download"

instance ToText Mp4MoovPlacement where
    toText = \case
        MMPNormal -> "NORMAL"
        MMPProgressiveDownload -> "PROGRESSIVE_DOWNLOAD"

instance Hashable     Mp4MoovPlacement
instance NFData       Mp4MoovPlacement
instance ToByteString Mp4MoovPlacement
instance ToQuery      Mp4MoovPlacement
instance ToHeader     Mp4MoovPlacement

instance ToJSON Mp4MoovPlacement where
    toJSON = toJSONText

instance FromJSON Mp4MoovPlacement where
    parseJSON = parseJSONText "Mp4MoovPlacement"

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
data Mpeg2AdaptiveQuantization
  = MAQHigh
  | MAQLow
  | MAQMedium
  | MAQOff
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2AdaptiveQuantization where
    parser = takeLowerText >>= \case
        "high" -> pure MAQHigh
        "low" -> pure MAQLow
        "medium" -> pure MAQMedium
        "off" -> pure MAQOff
        e -> fromTextError $ "Failure parsing Mpeg2AdaptiveQuantization from value: '" <> e
           <> "'. Accepted values: high, low, medium, off"

instance ToText Mpeg2AdaptiveQuantization where
    toText = \case
        MAQHigh -> "HIGH"
        MAQLow -> "LOW"
        MAQMedium -> "MEDIUM"
        MAQOff -> "OFF"

instance Hashable     Mpeg2AdaptiveQuantization
instance NFData       Mpeg2AdaptiveQuantization
instance ToByteString Mpeg2AdaptiveQuantization
instance ToQuery      Mpeg2AdaptiveQuantization
instance ToHeader     Mpeg2AdaptiveQuantization

instance ToJSON Mpeg2AdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON Mpeg2AdaptiveQuantization where
    parseJSON = parseJSONText "Mpeg2AdaptiveQuantization"

-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
data Mpeg2CodecLevel
  = MCLAuto
  | MCLHIGH1440
  | MCLHigh
  | MCLLow
  | MCLMain
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2CodecLevel where
    parser = takeLowerText >>= \case
        "auto" -> pure MCLAuto
        "high1440" -> pure MCLHIGH1440
        "high" -> pure MCLHigh
        "low" -> pure MCLLow
        "main" -> pure MCLMain
        e -> fromTextError $ "Failure parsing Mpeg2CodecLevel from value: '" <> e
           <> "'. Accepted values: auto, high1440, high, low, main"

instance ToText Mpeg2CodecLevel where
    toText = \case
        MCLAuto -> "AUTO"
        MCLHIGH1440 -> "HIGH1440"
        MCLHigh -> "HIGH"
        MCLLow -> "LOW"
        MCLMain -> "MAIN"

instance Hashable     Mpeg2CodecLevel
instance NFData       Mpeg2CodecLevel
instance ToByteString Mpeg2CodecLevel
instance ToQuery      Mpeg2CodecLevel
instance ToHeader     Mpeg2CodecLevel

instance ToJSON Mpeg2CodecLevel where
    toJSON = toJSONText

instance FromJSON Mpeg2CodecLevel where
    parseJSON = parseJSONText "Mpeg2CodecLevel"

-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
data Mpeg2CodecProfile
  = Main
  | Profile422
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2CodecProfile where
    parser = takeLowerText >>= \case
        "main" -> pure Main
        "profile_422" -> pure Profile422
        e -> fromTextError $ "Failure parsing Mpeg2CodecProfile from value: '" <> e
           <> "'. Accepted values: main, profile_422"

instance ToText Mpeg2CodecProfile where
    toText = \case
        Main -> "MAIN"
        Profile422 -> "PROFILE_422"

instance Hashable     Mpeg2CodecProfile
instance NFData       Mpeg2CodecProfile
instance ToByteString Mpeg2CodecProfile
instance ToQuery      Mpeg2CodecProfile
instance ToHeader     Mpeg2CodecProfile

instance ToJSON Mpeg2CodecProfile where
    toJSON = toJSONText

instance FromJSON Mpeg2CodecProfile where
    parseJSON = parseJSONText "Mpeg2CodecProfile"

-- | Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.
data Mpeg2FramerateControl
  = MFCInitializeFromSource
  | MFCSpecified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2FramerateControl where
    parser = takeLowerText >>= \case
        "initialize_from_source" -> pure MFCInitializeFromSource
        "specified" -> pure MFCSpecified
        e -> fromTextError $ "Failure parsing Mpeg2FramerateControl from value: '" <> e
           <> "'. Accepted values: initialize_from_source, specified"

instance ToText Mpeg2FramerateControl where
    toText = \case
        MFCInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
        MFCSpecified -> "SPECIFIED"

instance Hashable     Mpeg2FramerateControl
instance NFData       Mpeg2FramerateControl
instance ToByteString Mpeg2FramerateControl
instance ToQuery      Mpeg2FramerateControl
instance ToHeader     Mpeg2FramerateControl

instance ToJSON Mpeg2FramerateControl where
    toJSON = toJSONText

instance FromJSON Mpeg2FramerateControl where
    parseJSON = parseJSONText "Mpeg2FramerateControl"

-- | When set to INTERPOLATE, produces smoother motion during framerate conversion.
data Mpeg2FramerateConversionAlgorithm
  = MFCADuplicateDrop
  | MFCAInterpolate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2FramerateConversionAlgorithm where
    parser = takeLowerText >>= \case
        "duplicate_drop" -> pure MFCADuplicateDrop
        "interpolate" -> pure MFCAInterpolate
        e -> fromTextError $ "Failure parsing Mpeg2FramerateConversionAlgorithm from value: '" <> e
           <> "'. Accepted values: duplicate_drop, interpolate"

instance ToText Mpeg2FramerateConversionAlgorithm where
    toText = \case
        MFCADuplicateDrop -> "DUPLICATE_DROP"
        MFCAInterpolate -> "INTERPOLATE"

instance Hashable     Mpeg2FramerateConversionAlgorithm
instance NFData       Mpeg2FramerateConversionAlgorithm
instance ToByteString Mpeg2FramerateConversionAlgorithm
instance ToQuery      Mpeg2FramerateConversionAlgorithm
instance ToHeader     Mpeg2FramerateConversionAlgorithm

instance ToJSON Mpeg2FramerateConversionAlgorithm where
    toJSON = toJSONText

instance FromJSON Mpeg2FramerateConversionAlgorithm where
    parseJSON = parseJSONText "Mpeg2FramerateConversionAlgorithm"

-- | Indicates if the GOP Size in MPEG2 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
data Mpeg2GopSizeUnits
  = MGSUFrames
  | MGSUSeconds
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2GopSizeUnits where
    parser = takeLowerText >>= \case
        "frames" -> pure MGSUFrames
        "seconds" -> pure MGSUSeconds
        e -> fromTextError $ "Failure parsing Mpeg2GopSizeUnits from value: '" <> e
           <> "'. Accepted values: frames, seconds"

instance ToText Mpeg2GopSizeUnits where
    toText = \case
        MGSUFrames -> "FRAMES"
        MGSUSeconds -> "SECONDS"

instance Hashable     Mpeg2GopSizeUnits
instance NFData       Mpeg2GopSizeUnits
instance ToByteString Mpeg2GopSizeUnits
instance ToQuery      Mpeg2GopSizeUnits
instance ToHeader     Mpeg2GopSizeUnits

instance ToJSON Mpeg2GopSizeUnits where
    toJSON = toJSONText

instance FromJSON Mpeg2GopSizeUnits where
    parseJSON = parseJSONText "Mpeg2GopSizeUnits"

-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
data Mpeg2InterlaceMode
  = MIMBottomField
  | MIMFollowBottomField
  | MIMFollowTopField
  | MIMProgressive
  | MIMTopField
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2InterlaceMode where
    parser = takeLowerText >>= \case
        "bottom_field" -> pure MIMBottomField
        "follow_bottom_field" -> pure MIMFollowBottomField
        "follow_top_field" -> pure MIMFollowTopField
        "progressive" -> pure MIMProgressive
        "top_field" -> pure MIMTopField
        e -> fromTextError $ "Failure parsing Mpeg2InterlaceMode from value: '" <> e
           <> "'. Accepted values: bottom_field, follow_bottom_field, follow_top_field, progressive, top_field"

instance ToText Mpeg2InterlaceMode where
    toText = \case
        MIMBottomField -> "BOTTOM_FIELD"
        MIMFollowBottomField -> "FOLLOW_BOTTOM_FIELD"
        MIMFollowTopField -> "FOLLOW_TOP_FIELD"
        MIMProgressive -> "PROGRESSIVE"
        MIMTopField -> "TOP_FIELD"

instance Hashable     Mpeg2InterlaceMode
instance NFData       Mpeg2InterlaceMode
instance ToByteString Mpeg2InterlaceMode
instance ToQuery      Mpeg2InterlaceMode
instance ToHeader     Mpeg2InterlaceMode

instance ToJSON Mpeg2InterlaceMode where
    toJSON = toJSONText

instance FromJSON Mpeg2InterlaceMode where
    parseJSON = parseJSONText "Mpeg2InterlaceMode"

-- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
data Mpeg2IntraDcPrecision
  = MIDPAuto
  | MIDPIntraDcPrecision10
  | MIDPIntraDcPrecision11
  | MIDPIntraDcPrecision8
  | MIDPIntraDcPrecision9
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2IntraDcPrecision where
    parser = takeLowerText >>= \case
        "auto" -> pure MIDPAuto
        "intra_dc_precision_10" -> pure MIDPIntraDcPrecision10
        "intra_dc_precision_11" -> pure MIDPIntraDcPrecision11
        "intra_dc_precision_8" -> pure MIDPIntraDcPrecision8
        "intra_dc_precision_9" -> pure MIDPIntraDcPrecision9
        e -> fromTextError $ "Failure parsing Mpeg2IntraDcPrecision from value: '" <> e
           <> "'. Accepted values: auto, intra_dc_precision_10, intra_dc_precision_11, intra_dc_precision_8, intra_dc_precision_9"

instance ToText Mpeg2IntraDcPrecision where
    toText = \case
        MIDPAuto -> "AUTO"
        MIDPIntraDcPrecision10 -> "INTRA_DC_PRECISION_10"
        MIDPIntraDcPrecision11 -> "INTRA_DC_PRECISION_11"
        MIDPIntraDcPrecision8 -> "INTRA_DC_PRECISION_8"
        MIDPIntraDcPrecision9 -> "INTRA_DC_PRECISION_9"

instance Hashable     Mpeg2IntraDcPrecision
instance NFData       Mpeg2IntraDcPrecision
instance ToByteString Mpeg2IntraDcPrecision
instance ToQuery      Mpeg2IntraDcPrecision
instance ToHeader     Mpeg2IntraDcPrecision

instance ToJSON Mpeg2IntraDcPrecision where
    toJSON = toJSONText

instance FromJSON Mpeg2IntraDcPrecision where
    parseJSON = parseJSONText "Mpeg2IntraDcPrecision"

-- | Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
data Mpeg2ParControl
  = MPCInitializeFromSource
  | MPCSpecified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2ParControl where
    parser = takeLowerText >>= \case
        "initialize_from_source" -> pure MPCInitializeFromSource
        "specified" -> pure MPCSpecified
        e -> fromTextError $ "Failure parsing Mpeg2ParControl from value: '" <> e
           <> "'. Accepted values: initialize_from_source, specified"

instance ToText Mpeg2ParControl where
    toText = \case
        MPCInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
        MPCSpecified -> "SPECIFIED"

instance Hashable     Mpeg2ParControl
instance NFData       Mpeg2ParControl
instance ToByteString Mpeg2ParControl
instance ToQuery      Mpeg2ParControl
instance ToHeader     Mpeg2ParControl

instance ToJSON Mpeg2ParControl where
    toJSON = toJSONText

instance FromJSON Mpeg2ParControl where
    parseJSON = parseJSONText "Mpeg2ParControl"

-- | Use Quality tuning level (Mpeg2QualityTuningLevel) to specifiy whether to use single-pass or multipass video encoding.
data Mpeg2QualityTuningLevel
  = MQTLMultiPass
  | MQTLSinglePass
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2QualityTuningLevel where
    parser = takeLowerText >>= \case
        "multi_pass" -> pure MQTLMultiPass
        "single_pass" -> pure MQTLSinglePass
        e -> fromTextError $ "Failure parsing Mpeg2QualityTuningLevel from value: '" <> e
           <> "'. Accepted values: multi_pass, single_pass"

instance ToText Mpeg2QualityTuningLevel where
    toText = \case
        MQTLMultiPass -> "MULTI_PASS"
        MQTLSinglePass -> "SINGLE_PASS"

instance Hashable     Mpeg2QualityTuningLevel
instance NFData       Mpeg2QualityTuningLevel
instance ToByteString Mpeg2QualityTuningLevel
instance ToQuery      Mpeg2QualityTuningLevel
instance ToHeader     Mpeg2QualityTuningLevel

instance ToJSON Mpeg2QualityTuningLevel where
    toJSON = toJSONText

instance FromJSON Mpeg2QualityTuningLevel where
    parseJSON = parseJSONText "Mpeg2QualityTuningLevel"

-- | Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
data Mpeg2RateControlMode
  = MRCMCbr
  | MRCMVbr
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2RateControlMode where
    parser = takeLowerText >>= \case
        "cbr" -> pure MRCMCbr
        "vbr" -> pure MRCMVbr
        e -> fromTextError $ "Failure parsing Mpeg2RateControlMode from value: '" <> e
           <> "'. Accepted values: cbr, vbr"

instance ToText Mpeg2RateControlMode where
    toText = \case
        MRCMCbr -> "CBR"
        MRCMVbr -> "VBR"

instance Hashable     Mpeg2RateControlMode
instance NFData       Mpeg2RateControlMode
instance ToByteString Mpeg2RateControlMode
instance ToQuery      Mpeg2RateControlMode
instance ToHeader     Mpeg2RateControlMode

instance ToJSON Mpeg2RateControlMode where
    toJSON = toJSONText

instance FromJSON Mpeg2RateControlMode where
    parseJSON = parseJSONText "Mpeg2RateControlMode"

-- | Scene change detection (inserts I-frames on scene changes).
data Mpeg2SceneChangeDetect
  = MSCDDisabled
  | MSCDEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2SceneChangeDetect where
    parser = takeLowerText >>= \case
        "disabled" -> pure MSCDDisabled
        "enabled" -> pure MSCDEnabled
        e -> fromTextError $ "Failure parsing Mpeg2SceneChangeDetect from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText Mpeg2SceneChangeDetect where
    toText = \case
        MSCDDisabled -> "DISABLED"
        MSCDEnabled -> "ENABLED"

instance Hashable     Mpeg2SceneChangeDetect
instance NFData       Mpeg2SceneChangeDetect
instance ToByteString Mpeg2SceneChangeDetect
instance ToQuery      Mpeg2SceneChangeDetect
instance ToHeader     Mpeg2SceneChangeDetect

instance ToJSON Mpeg2SceneChangeDetect where
    toJSON = toJSONText

instance FromJSON Mpeg2SceneChangeDetect where
    parseJSON = parseJSONText "Mpeg2SceneChangeDetect"

-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
data Mpeg2SlowPal
  = MSPDisabled
  | MSPEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2SlowPal where
    parser = takeLowerText >>= \case
        "disabled" -> pure MSPDisabled
        "enabled" -> pure MSPEnabled
        e -> fromTextError $ "Failure parsing Mpeg2SlowPal from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText Mpeg2SlowPal where
    toText = \case
        MSPDisabled -> "DISABLED"
        MSPEnabled -> "ENABLED"

instance Hashable     Mpeg2SlowPal
instance NFData       Mpeg2SlowPal
instance ToByteString Mpeg2SlowPal
instance ToQuery      Mpeg2SlowPal
instance ToHeader     Mpeg2SlowPal

instance ToJSON Mpeg2SlowPal where
    toJSON = toJSONText

instance FromJSON Mpeg2SlowPal where
    parseJSON = parseJSONText "Mpeg2SlowPal"

-- | Adjust quantization within each frame based on spatial variation of content complexity.
data Mpeg2SpatialAdaptiveQuantization
  = MSAQDisabled
  | MSAQEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2SpatialAdaptiveQuantization where
    parser = takeLowerText >>= \case
        "disabled" -> pure MSAQDisabled
        "enabled" -> pure MSAQEnabled
        e -> fromTextError $ "Failure parsing Mpeg2SpatialAdaptiveQuantization from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText Mpeg2SpatialAdaptiveQuantization where
    toText = \case
        MSAQDisabled -> "DISABLED"
        MSAQEnabled -> "ENABLED"

instance Hashable     Mpeg2SpatialAdaptiveQuantization
instance NFData       Mpeg2SpatialAdaptiveQuantization
instance ToByteString Mpeg2SpatialAdaptiveQuantization
instance ToQuery      Mpeg2SpatialAdaptiveQuantization
instance ToHeader     Mpeg2SpatialAdaptiveQuantization

instance ToJSON Mpeg2SpatialAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON Mpeg2SpatialAdaptiveQuantization where
    parseJSON = parseJSONText "Mpeg2SpatialAdaptiveQuantization"

-- | Produces a Type D-10 compatible bitstream (SMPTE 356M-2001).
data Mpeg2Syntax
  = MSD10
  | MSDefault
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2Syntax where
    parser = takeLowerText >>= \case
        "d_10" -> pure MSD10
        "default" -> pure MSDefault
        e -> fromTextError $ "Failure parsing Mpeg2Syntax from value: '" <> e
           <> "'. Accepted values: d_10, default"

instance ToText Mpeg2Syntax where
    toText = \case
        MSD10 -> "D_10"
        MSDefault -> "DEFAULT"

instance Hashable     Mpeg2Syntax
instance NFData       Mpeg2Syntax
instance ToByteString Mpeg2Syntax
instance ToQuery      Mpeg2Syntax
instance ToHeader     Mpeg2Syntax

instance ToJSON Mpeg2Syntax where
    toJSON = toJSONText

instance FromJSON Mpeg2Syntax where
    parseJSON = parseJSONText "Mpeg2Syntax"

-- | Only use Telecine (Mpeg2Telecine) when you set Framerate (Framerate) to 29.970. Set Telecine (Mpeg2Telecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.
data Mpeg2Telecine
  = MTHard
  | MTNone
  | MTSoft
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2Telecine where
    parser = takeLowerText >>= \case
        "hard" -> pure MTHard
        "none" -> pure MTNone
        "soft" -> pure MTSoft
        e -> fromTextError $ "Failure parsing Mpeg2Telecine from value: '" <> e
           <> "'. Accepted values: hard, none, soft"

instance ToText Mpeg2Telecine where
    toText = \case
        MTHard -> "HARD"
        MTNone -> "NONE"
        MTSoft -> "SOFT"

instance Hashable     Mpeg2Telecine
instance NFData       Mpeg2Telecine
instance ToByteString Mpeg2Telecine
instance ToQuery      Mpeg2Telecine
instance ToHeader     Mpeg2Telecine

instance ToJSON Mpeg2Telecine where
    toJSON = toJSONText

instance FromJSON Mpeg2Telecine where
    parseJSON = parseJSONText "Mpeg2Telecine"

-- | Adjust quantization within each frame based on temporal variation of content complexity.
data Mpeg2TemporalAdaptiveQuantization
  = MTAQDisabled
  | MTAQEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mpeg2TemporalAdaptiveQuantization where
    parser = takeLowerText >>= \case
        "disabled" -> pure MTAQDisabled
        "enabled" -> pure MTAQEnabled
        e -> fromTextError $ "Failure parsing Mpeg2TemporalAdaptiveQuantization from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText Mpeg2TemporalAdaptiveQuantization where
    toText = \case
        MTAQDisabled -> "DISABLED"
        MTAQEnabled -> "ENABLED"

instance Hashable     Mpeg2TemporalAdaptiveQuantization
instance NFData       Mpeg2TemporalAdaptiveQuantization
instance ToByteString Mpeg2TemporalAdaptiveQuantization
instance ToQuery      Mpeg2TemporalAdaptiveQuantization
instance ToHeader     Mpeg2TemporalAdaptiveQuantization

instance ToJSON Mpeg2TemporalAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON Mpeg2TemporalAdaptiveQuantization where
    parseJSON = parseJSONText "Mpeg2TemporalAdaptiveQuantization"

-- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings across a Microsoft Smooth output group into a single audio stream.
data MsSmoothAudioDeduplication
  = CombineDuplicateStreams
  | None
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MsSmoothAudioDeduplication where
    parser = takeLowerText >>= \case
        "combine_duplicate_streams" -> pure CombineDuplicateStreams
        "none" -> pure None
        e -> fromTextError $ "Failure parsing MsSmoothAudioDeduplication from value: '" <> e
           <> "'. Accepted values: combine_duplicate_streams, none"

instance ToText MsSmoothAudioDeduplication where
    toText = \case
        CombineDuplicateStreams -> "COMBINE_DUPLICATE_STREAMS"
        None -> "NONE"

instance Hashable     MsSmoothAudioDeduplication
instance NFData       MsSmoothAudioDeduplication
instance ToByteString MsSmoothAudioDeduplication
instance ToQuery      MsSmoothAudioDeduplication
instance ToHeader     MsSmoothAudioDeduplication

instance ToJSON MsSmoothAudioDeduplication where
    toJSON = toJSONText

instance FromJSON MsSmoothAudioDeduplication where
    parseJSON = parseJSONText "MsSmoothAudioDeduplication"

-- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding format for the server and client manifest. Valid options are utf8 and utf16.
data MsSmoothManifestEncoding
  = UTF16
  | UTF8
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MsSmoothManifestEncoding where
    parser = takeLowerText >>= \case
        "utf16" -> pure UTF16
        "utf8" -> pure UTF8
        e -> fromTextError $ "Failure parsing MsSmoothManifestEncoding from value: '" <> e
           <> "'. Accepted values: utf16, utf8"

instance ToText MsSmoothManifestEncoding where
    toText = \case
        UTF16 -> "UTF16"
        UTF8 -> "UTF8"

instance Hashable     MsSmoothManifestEncoding
instance NFData       MsSmoothManifestEncoding
instance ToByteString MsSmoothManifestEncoding
instance ToQuery      MsSmoothManifestEncoding
instance ToHeader     MsSmoothManifestEncoding

instance ToJSON MsSmoothManifestEncoding where
    toJSON = toJSONText

instance FromJSON MsSmoothManifestEncoding where
    parseJSON = parseJSONText "MsSmoothManifestEncoding"

-- | Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral is an edge preserving noise reduction filter * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) are convolution filters * Conserve is a min/max noise reduction filter * Spatial is frequency-domain filter based on JND principles.
data NoiseReducerFilter
  = Bilateral
  | Conserve
  | Gaussian
  | Lanczos
  | Mean
  | Sharpen
  | Spatial
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NoiseReducerFilter where
    parser = takeLowerText >>= \case
        "bilateral" -> pure Bilateral
        "conserve" -> pure Conserve
        "gaussian" -> pure Gaussian
        "lanczos" -> pure Lanczos
        "mean" -> pure Mean
        "sharpen" -> pure Sharpen
        "spatial" -> pure Spatial
        e -> fromTextError $ "Failure parsing NoiseReducerFilter from value: '" <> e
           <> "'. Accepted values: bilateral, conserve, gaussian, lanczos, mean, sharpen, spatial"

instance ToText NoiseReducerFilter where
    toText = \case
        Bilateral -> "BILATERAL"
        Conserve -> "CONSERVE"
        Gaussian -> "GAUSSIAN"
        Lanczos -> "LANCZOS"
        Mean -> "MEAN"
        Sharpen -> "SHARPEN"
        Spatial -> "SPATIAL"

instance Hashable     NoiseReducerFilter
instance NFData       NoiseReducerFilter
instance ToByteString NoiseReducerFilter
instance ToQuery      NoiseReducerFilter
instance ToHeader     NoiseReducerFilter

instance ToJSON NoiseReducerFilter where
    toJSON = toJSONText

instance FromJSON NoiseReducerFilter where
    parseJSON = parseJSONText "NoiseReducerFilter"

-- | When you request lists of resources, you can optionally specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
data Order
  = Ascending
  | Descending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Order where
    parser = takeLowerText >>= \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        e -> fromTextError $ "Failure parsing Order from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText Order where
    toText = \case
        Ascending -> "ASCENDING"
        Descending -> "DESCENDING"

instance Hashable     Order
instance NFData       Order
instance ToByteString Order
instance ToQuery      Order
instance ToHeader     Order

instance ToJSON Order where
    toJSON = toJSONText

-- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth Streaming)
data OutputGroupType
  = DashIsoGroupSettings
  | FileGroupSettings
  | HlsGroupSettings
  | MsSmoothGroupSettings
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OutputGroupType where
    parser = takeLowerText >>= \case
        "dash_iso_group_settings" -> pure DashIsoGroupSettings
        "file_group_settings" -> pure FileGroupSettings
        "hls_group_settings" -> pure HlsGroupSettings
        "ms_smooth_group_settings" -> pure MsSmoothGroupSettings
        e -> fromTextError $ "Failure parsing OutputGroupType from value: '" <> e
           <> "'. Accepted values: dash_iso_group_settings, file_group_settings, hls_group_settings, ms_smooth_group_settings"

instance ToText OutputGroupType where
    toText = \case
        DashIsoGroupSettings -> "DASH_ISO_GROUP_SETTINGS"
        FileGroupSettings -> "FILE_GROUP_SETTINGS"
        HlsGroupSettings -> "HLS_GROUP_SETTINGS"
        MsSmoothGroupSettings -> "MS_SMOOTH_GROUP_SETTINGS"

instance Hashable     OutputGroupType
instance NFData       OutputGroupType
instance ToByteString OutputGroupType
instance ToQuery      OutputGroupType
instance ToHeader     OutputGroupType

instance ToJSON OutputGroupType where
    toJSON = toJSONText

instance FromJSON OutputGroupType where
    parseJSON = parseJSONText "OutputGroupType"

-- | Selects method of inserting SDT information into output stream.  "Follow input SDT" copies SDT information from input stream to  output stream. "Follow input SDT if present" copies SDT information from  input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. Enter "SDT  Manually" means user will enter the SDT information. "No SDT" means output  stream will not contain SDT information.
data OutputSdt
  = SdtFollow
  | SdtFollowIfPresent
  | SdtManual
  | SdtNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OutputSdt where
    parser = takeLowerText >>= \case
        "sdt_follow" -> pure SdtFollow
        "sdt_follow_if_present" -> pure SdtFollowIfPresent
        "sdt_manual" -> pure SdtManual
        "sdt_none" -> pure SdtNone
        e -> fromTextError $ "Failure parsing OutputSdt from value: '" <> e
           <> "'. Accepted values: sdt_follow, sdt_follow_if_present, sdt_manual, sdt_none"

instance ToText OutputSdt where
    toText = \case
        SdtFollow -> "SDT_FOLLOW"
        SdtFollowIfPresent -> "SDT_FOLLOW_IF_PRESENT"
        SdtManual -> "SDT_MANUAL"
        SdtNone -> "SDT_NONE"

instance Hashable     OutputSdt
instance NFData       OutputSdt
instance ToByteString OutputSdt
instance ToQuery      OutputSdt
instance ToHeader     OutputSdt

instance ToJSON OutputSdt where
    toJSON = toJSONText

instance FromJSON OutputSdt where
    parseJSON = parseJSONText "OutputSdt"

-- | Optional. When you request a list of presets, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
data PresetListBy
  = PLBCreationDate
  | PLBName
  | PLBSystem
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PresetListBy where
    parser = takeLowerText >>= \case
        "creation_date" -> pure PLBCreationDate
        "name" -> pure PLBName
        "system" -> pure PLBSystem
        e -> fromTextError $ "Failure parsing PresetListBy from value: '" <> e
           <> "'. Accepted values: creation_date, name, system"

instance ToText PresetListBy where
    toText = \case
        PLBCreationDate -> "CREATION_DATE"
        PLBName -> "NAME"
        PLBSystem -> "SYSTEM"

instance Hashable     PresetListBy
instance NFData       PresetListBy
instance ToByteString PresetListBy
instance ToQuery      PresetListBy
instance ToHeader     PresetListBy

instance ToJSON PresetListBy where
    toJSON = toJSONText

-- | Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
data ProresCodecProfile
  = AppleProres422
  | AppleProres422Hq
  | AppleProres422LT
  | AppleProres422Proxy
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProresCodecProfile where
    parser = takeLowerText >>= \case
        "apple_prores_422" -> pure AppleProres422
        "apple_prores_422_hq" -> pure AppleProres422Hq
        "apple_prores_422_lt" -> pure AppleProres422LT
        "apple_prores_422_proxy" -> pure AppleProres422Proxy
        e -> fromTextError $ "Failure parsing ProresCodecProfile from value: '" <> e
           <> "'. Accepted values: apple_prores_422, apple_prores_422_hq, apple_prores_422_lt, apple_prores_422_proxy"

instance ToText ProresCodecProfile where
    toText = \case
        AppleProres422 -> "APPLE_PRORES_422"
        AppleProres422Hq -> "APPLE_PRORES_422_HQ"
        AppleProres422LT -> "APPLE_PRORES_422_LT"
        AppleProres422Proxy -> "APPLE_PRORES_422_PROXY"

instance Hashable     ProresCodecProfile
instance NFData       ProresCodecProfile
instance ToByteString ProresCodecProfile
instance ToQuery      ProresCodecProfile
instance ToHeader     ProresCodecProfile

instance ToJSON ProresCodecProfile where
    toJSON = toJSONText

instance FromJSON ProresCodecProfile where
    parseJSON = parseJSONText "ProresCodecProfile"

-- | Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.
data ProresFramerateControl
  = PFCInitializeFromSource
  | PFCSpecified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProresFramerateControl where
    parser = takeLowerText >>= \case
        "initialize_from_source" -> pure PFCInitializeFromSource
        "specified" -> pure PFCSpecified
        e -> fromTextError $ "Failure parsing ProresFramerateControl from value: '" <> e
           <> "'. Accepted values: initialize_from_source, specified"

instance ToText ProresFramerateControl where
    toText = \case
        PFCInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
        PFCSpecified -> "SPECIFIED"

instance Hashable     ProresFramerateControl
instance NFData       ProresFramerateControl
instance ToByteString ProresFramerateControl
instance ToQuery      ProresFramerateControl
instance ToHeader     ProresFramerateControl

instance ToJSON ProresFramerateControl where
    toJSON = toJSONText

instance FromJSON ProresFramerateControl where
    parseJSON = parseJSONText "ProresFramerateControl"

-- | When set to INTERPOLATE, produces smoother motion during framerate conversion.
data ProresFramerateConversionAlgorithm
  = PFCADuplicateDrop
  | PFCAInterpolate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProresFramerateConversionAlgorithm where
    parser = takeLowerText >>= \case
        "duplicate_drop" -> pure PFCADuplicateDrop
        "interpolate" -> pure PFCAInterpolate
        e -> fromTextError $ "Failure parsing ProresFramerateConversionAlgorithm from value: '" <> e
           <> "'. Accepted values: duplicate_drop, interpolate"

instance ToText ProresFramerateConversionAlgorithm where
    toText = \case
        PFCADuplicateDrop -> "DUPLICATE_DROP"
        PFCAInterpolate -> "INTERPOLATE"

instance Hashable     ProresFramerateConversionAlgorithm
instance NFData       ProresFramerateConversionAlgorithm
instance ToByteString ProresFramerateConversionAlgorithm
instance ToQuery      ProresFramerateConversionAlgorithm
instance ToHeader     ProresFramerateConversionAlgorithm

instance ToJSON ProresFramerateConversionAlgorithm where
    toJSON = toJSONText

instance FromJSON ProresFramerateConversionAlgorithm where
    parseJSON = parseJSONText "ProresFramerateConversionAlgorithm"

-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
data ProresInterlaceMode
  = PIMBottomField
  | PIMFollowBottomField
  | PIMFollowTopField
  | PIMProgressive
  | PIMTopField
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProresInterlaceMode where
    parser = takeLowerText >>= \case
        "bottom_field" -> pure PIMBottomField
        "follow_bottom_field" -> pure PIMFollowBottomField
        "follow_top_field" -> pure PIMFollowTopField
        "progressive" -> pure PIMProgressive
        "top_field" -> pure PIMTopField
        e -> fromTextError $ "Failure parsing ProresInterlaceMode from value: '" <> e
           <> "'. Accepted values: bottom_field, follow_bottom_field, follow_top_field, progressive, top_field"

instance ToText ProresInterlaceMode where
    toText = \case
        PIMBottomField -> "BOTTOM_FIELD"
        PIMFollowBottomField -> "FOLLOW_BOTTOM_FIELD"
        PIMFollowTopField -> "FOLLOW_TOP_FIELD"
        PIMProgressive -> "PROGRESSIVE"
        PIMTopField -> "TOP_FIELD"

instance Hashable     ProresInterlaceMode
instance NFData       ProresInterlaceMode
instance ToByteString ProresInterlaceMode
instance ToQuery      ProresInterlaceMode
instance ToHeader     ProresInterlaceMode

instance ToJSON ProresInterlaceMode where
    toJSON = toJSONText

instance FromJSON ProresInterlaceMode where
    parseJSON = parseJSONText "ProresInterlaceMode"

-- | Use (ProresParControl) to specify how the service determines the pixel aspect ratio. Set to Follow source (INITIALIZE_FROM_SOURCE) to use the pixel aspect ratio from the input.  To specify a different pixel aspect ratio: Using the console, choose it from the dropdown menu. Using the API, set ProresParControl to (SPECIFIED) and provide  for (ParNumerator) and (ParDenominator).
data ProresParControl
  = PPCInitializeFromSource
  | PPCSpecified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProresParControl where
    parser = takeLowerText >>= \case
        "initialize_from_source" -> pure PPCInitializeFromSource
        "specified" -> pure PPCSpecified
        e -> fromTextError $ "Failure parsing ProresParControl from value: '" <> e
           <> "'. Accepted values: initialize_from_source, specified"

instance ToText ProresParControl where
    toText = \case
        PPCInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
        PPCSpecified -> "SPECIFIED"

instance Hashable     ProresParControl
instance NFData       ProresParControl
instance ToByteString ProresParControl
instance ToQuery      ProresParControl
instance ToHeader     ProresParControl

instance ToJSON ProresParControl where
    toJSON = toJSONText

instance FromJSON ProresParControl where
    parseJSON = parseJSONText "ProresParControl"

-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
data ProresSlowPal
  = PSPDisabled
  | PSPEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProresSlowPal where
    parser = takeLowerText >>= \case
        "disabled" -> pure PSPDisabled
        "enabled" -> pure PSPEnabled
        e -> fromTextError $ "Failure parsing ProresSlowPal from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText ProresSlowPal where
    toText = \case
        PSPDisabled -> "DISABLED"
        PSPEnabled -> "ENABLED"

instance Hashable     ProresSlowPal
instance NFData       ProresSlowPal
instance ToByteString ProresSlowPal
instance ToQuery      ProresSlowPal
instance ToHeader     ProresSlowPal

instance ToJSON ProresSlowPal where
    toJSON = toJSONText

instance FromJSON ProresSlowPal where
    parseJSON = parseJSONText "ProresSlowPal"

-- | Only use Telecine (ProresTelecine) when you set Framerate (Framerate) to 29.970. Set Telecine (ProresTelecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.
data ProresTelecine
  = PTHard
  | PTNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProresTelecine where
    parser = takeLowerText >>= \case
        "hard" -> pure PTHard
        "none" -> pure PTNone
        e -> fromTextError $ "Failure parsing ProresTelecine from value: '" <> e
           <> "'. Accepted values: hard, none"

instance ToText ProresTelecine where
    toText = \case
        PTHard -> "HARD"
        PTNone -> "NONE"

instance Hashable     ProresTelecine
instance NFData       ProresTelecine
instance ToByteString ProresTelecine
instance ToQuery      ProresTelecine
instance ToHeader     ProresTelecine

instance ToJSON ProresTelecine where
    toJSON = toJSONText

instance FromJSON ProresTelecine where
    parseJSON = parseJSONText "ProresTelecine"

-- | Optional. When you request a list of queues, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by creation date.
data QueueListBy
  = CreationDate
  | Name
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText QueueListBy where
    parser = takeLowerText >>= \case
        "creation_date" -> pure CreationDate
        "name" -> pure Name
        e -> fromTextError $ "Failure parsing QueueListBy from value: '" <> e
           <> "'. Accepted values: creation_date, name"

instance ToText QueueListBy where
    toText = \case
        CreationDate -> "CREATION_DATE"
        Name -> "NAME"

instance Hashable     QueueListBy
instance NFData       QueueListBy
instance ToByteString QueueListBy
instance ToQuery      QueueListBy
instance ToHeader     QueueListBy

instance ToJSON QueueListBy where
    toJSON = toJSONText

-- | Queues can be ACTIVE or PAUSED. If you pause a queue, jobs in that queue will not begin. Jobs running when a queue is paused continue to run until they finish or error out.
data QueueStatus
  = Active
  | Paused
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText QueueStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "paused" -> pure Paused
        e -> fromTextError $ "Failure parsing QueueStatus from value: '" <> e
           <> "'. Accepted values: active, paused"

instance ToText QueueStatus where
    toText = \case
        Active -> "ACTIVE"
        Paused -> "PAUSED"

instance Hashable     QueueStatus
instance NFData       QueueStatus
instance ToByteString QueueStatus
instance ToQuery      QueueStatus
instance ToHeader     QueueStatus

instance ToJSON QueueStatus where
    toJSON = toJSONText

instance FromJSON QueueStatus where
    parseJSON = parseJSONText "QueueStatus"

-- | Use Respond to AFD (RespondToAfd) to specify how the service changes the video itself in response to AFD values in the input. * Choose Respond to clip the input video frame according to the AFD value, input display aspect ratio, and output display aspect ratio. * Choose Passthrough to include the input AFD values. Do not choose this when AfdSignaling is set to (NONE). A preferred implementation of this workflow is to set RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to remove all input AFD values from this output.
data RespondToAfd
  = RTANone
  | RTAPassthrough
  | RTARespond
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RespondToAfd where
    parser = takeLowerText >>= \case
        "none" -> pure RTANone
        "passthrough" -> pure RTAPassthrough
        "respond" -> pure RTARespond
        e -> fromTextError $ "Failure parsing RespondToAfd from value: '" <> e
           <> "'. Accepted values: none, passthrough, respond"

instance ToText RespondToAfd where
    toText = \case
        RTANone -> "NONE"
        RTAPassthrough -> "PASSTHROUGH"
        RTARespond -> "RESPOND"

instance Hashable     RespondToAfd
instance NFData       RespondToAfd
instance ToByteString RespondToAfd
instance ToQuery      RespondToAfd
instance ToHeader     RespondToAfd

instance ToJSON RespondToAfd where
    toJSON = toJSONText

instance FromJSON RespondToAfd where
    parseJSON = parseJSONText "RespondToAfd"

-- | Applies only if your input aspect ratio is different from your output aspect ratio. Enable Stretch to output (StretchToOutput) to have the service stretch your video image to fit. Leave this setting disabled to allow the service to letterbox your video instead. This setting overrides any positioning value you specify elsewhere in the job.
data ScalingBehavior
  = SBDefault
  | SBStretchToOutput
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ScalingBehavior where
    parser = takeLowerText >>= \case
        "default" -> pure SBDefault
        "stretch_to_output" -> pure SBStretchToOutput
        e -> fromTextError $ "Failure parsing ScalingBehavior from value: '" <> e
           <> "'. Accepted values: default, stretch_to_output"

instance ToText ScalingBehavior where
    toText = \case
        SBDefault -> "DEFAULT"
        SBStretchToOutput -> "STRETCH_TO_OUTPUT"

instance Hashable     ScalingBehavior
instance NFData       ScalingBehavior
instance ToByteString ScalingBehavior
instance ToQuery      ScalingBehavior
instance ToHeader     ScalingBehavior

instance ToJSON ScalingBehavior where
    toJSON = toJSONText

instance FromJSON ScalingBehavior where
    parseJSON = parseJSONText "ScalingBehavior"

-- | Set Framerate (SccDestinationFramerate) to make sure that the captions and the video are synchronized in the output. Specify a framerate that matches the framerate of the associated video. If the video framerate is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the video has video_insertion=true and drop_frame_timecode=true; otherwise, choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
data SccDestinationFramerate
  = Framerate2397
  | Framerate24
  | Framerate2997Dropframe
  | Framerate2997NonDropframe
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SccDestinationFramerate where
    parser = takeLowerText >>= \case
        "framerate_23_97" -> pure Framerate2397
        "framerate_24" -> pure Framerate24
        "framerate_29_97_dropframe" -> pure Framerate2997Dropframe
        "framerate_29_97_non_dropframe" -> pure Framerate2997NonDropframe
        e -> fromTextError $ "Failure parsing SccDestinationFramerate from value: '" <> e
           <> "'. Accepted values: framerate_23_97, framerate_24, framerate_29_97_dropframe, framerate_29_97_non_dropframe"

instance ToText SccDestinationFramerate where
    toText = \case
        Framerate2397 -> "FRAMERATE_23_97"
        Framerate24 -> "FRAMERATE_24"
        Framerate2997Dropframe -> "FRAMERATE_29_97_DROPFRAME"
        Framerate2997NonDropframe -> "FRAMERATE_29_97_NON_DROPFRAME"

instance Hashable     SccDestinationFramerate
instance NFData       SccDestinationFramerate
instance ToByteString SccDestinationFramerate
instance ToQuery      SccDestinationFramerate
instance ToHeader     SccDestinationFramerate

instance ToJSON SccDestinationFramerate where
    toJSON = toJSONText

instance FromJSON SccDestinationFramerate where
    parseJSON = parseJSONText "SccDestinationFramerate"

-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.
data TimecodeBurninPosition
  = BottomCenter
  | BottomLeft
  | BottomRight
  | MiddleCenter
  | MiddleLeft
  | MiddleRight
  | TopCenter
  | TopLeft
  | TopRight
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TimecodeBurninPosition where
    parser = takeLowerText >>= \case
        "bottom_center" -> pure BottomCenter
        "bottom_left" -> pure BottomLeft
        "bottom_right" -> pure BottomRight
        "middle_center" -> pure MiddleCenter
        "middle_left" -> pure MiddleLeft
        "middle_right" -> pure MiddleRight
        "top_center" -> pure TopCenter
        "top_left" -> pure TopLeft
        "top_right" -> pure TopRight
        e -> fromTextError $ "Failure parsing TimecodeBurninPosition from value: '" <> e
           <> "'. Accepted values: bottom_center, bottom_left, bottom_right, middle_center, middle_left, middle_right, top_center, top_left, top_right"

instance ToText TimecodeBurninPosition where
    toText = \case
        BottomCenter -> "BOTTOM_CENTER"
        BottomLeft -> "BOTTOM_LEFT"
        BottomRight -> "BOTTOM_RIGHT"
        MiddleCenter -> "MIDDLE_CENTER"
        MiddleLeft -> "MIDDLE_LEFT"
        MiddleRight -> "MIDDLE_RIGHT"
        TopCenter -> "TOP_CENTER"
        TopLeft -> "TOP_LEFT"
        TopRight -> "TOP_RIGHT"

instance Hashable     TimecodeBurninPosition
instance NFData       TimecodeBurninPosition
instance ToByteString TimecodeBurninPosition
instance ToQuery      TimecodeBurninPosition
instance ToHeader     TimecodeBurninPosition

instance ToJSON TimecodeBurninPosition where
    toJSON = toJSONText

instance FromJSON TimecodeBurninPosition where
    parseJSON = parseJSONText "TimecodeBurninPosition"

-- | Use Timecode source (TimecodeSource) to set how timecodes are handled within this input. To make sure that your video, audio, captions, and markers are synchronized and that time-based features, such as image inserter, work correctly, choose the Timecode source option that matches your assets. All timecodes are in a 24-hour format with frame number (HH:MM:SS:FF). * Embedded (EMBEDDED) - Use the timecode that is in the input video. If no embedded timecode is in the source, the service will use Start at 0 (ZEROBASED) instead. * Start at 0 (ZEROBASED) - Set the timecode of the initial frame to 00:00:00:00. * Specified Start (SPECIFIEDSTART) - Set the timecode of the initial frame to a value other than zero. You use Start timecode (Start) to provide this value.
data TimecodeSource
  = TSEmbedded
  | TSSpecifiedstart
  | TSZerobased
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TimecodeSource where
    parser = takeLowerText >>= \case
        "embedded" -> pure TSEmbedded
        "specifiedstart" -> pure TSSpecifiedstart
        "zerobased" -> pure TSZerobased
        e -> fromTextError $ "Failure parsing TimecodeSource from value: '" <> e
           <> "'. Accepted values: embedded, specifiedstart, zerobased"

instance ToText TimecodeSource where
    toText = \case
        TSEmbedded -> "EMBEDDED"
        TSSpecifiedstart -> "SPECIFIEDSTART"
        TSZerobased -> "ZEROBASED"

instance Hashable     TimecodeSource
instance NFData       TimecodeSource
instance ToByteString TimecodeSource
instance ToQuery      TimecodeSource
instance ToHeader     TimecodeSource

instance ToJSON TimecodeSource where
    toJSON = toJSONText

instance FromJSON TimecodeSource where
    parseJSON = parseJSONText "TimecodeSource"

-- | If PASSTHROUGH, inserts ID3 timed metadata from the timed_metadata REST command into this output.
data TimedMetadata
  = TMNone
  | TMPassthrough
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TimedMetadata where
    parser = takeLowerText >>= \case
        "none" -> pure TMNone
        "passthrough" -> pure TMPassthrough
        e -> fromTextError $ "Failure parsing TimedMetadata from value: '" <> e
           <> "'. Accepted values: none, passthrough"

instance ToText TimedMetadata where
    toText = \case
        TMNone -> "NONE"
        TMPassthrough -> "PASSTHROUGH"

instance Hashable     TimedMetadata
instance NFData       TimedMetadata
instance ToByteString TimedMetadata
instance ToQuery      TimedMetadata
instance ToHeader     TimedMetadata

instance ToJSON TimedMetadata where
    toJSON = toJSONText

instance FromJSON TimedMetadata where
    parseJSON = parseJSONText "TimedMetadata"

-- | Pass through style and position information from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT output or TTML output.
data TtmlStylePassthrough
  = TSPDisabled
  | TSPEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TtmlStylePassthrough where
    parser = takeLowerText >>= \case
        "disabled" -> pure TSPDisabled
        "enabled" -> pure TSPEnabled
        e -> fromTextError $ "Failure parsing TtmlStylePassthrough from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText TtmlStylePassthrough where
    toText = \case
        TSPDisabled -> "DISABLED"
        TSPEnabled -> "ENABLED"

instance Hashable     TtmlStylePassthrough
instance NFData       TtmlStylePassthrough
instance ToByteString TtmlStylePassthrough
instance ToQuery      TtmlStylePassthrough
instance ToHeader     TtmlStylePassthrough

instance ToJSON TtmlStylePassthrough where
    toJSON = toJSONText

instance FromJSON TtmlStylePassthrough where
    parseJSON = parseJSONText "TtmlStylePassthrough"

data Type
  = Custom
  | System
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Type where
    parser = takeLowerText >>= \case
        "custom" -> pure Custom
        "system" -> pure System
        e -> fromTextError $ "Failure parsing Type from value: '" <> e
           <> "'. Accepted values: custom, system"

instance ToText Type where
    toText = \case
        Custom -> "CUSTOM"
        System -> "SYSTEM"

instance Hashable     Type
instance NFData       Type
instance ToByteString Type
instance ToQuery      Type
instance ToHeader     Type

instance FromJSON Type where
    parseJSON = parseJSONText "Type"

-- | Type of video codec
data VideoCodec
  = FrameCapture
  | H264
  | H265
  | MPEG2
  | Prores
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VideoCodec where
    parser = takeLowerText >>= \case
        "frame_capture" -> pure FrameCapture
        "h_264" -> pure H264
        "h_265" -> pure H265
        "mpeg2" -> pure MPEG2
        "prores" -> pure Prores
        e -> fromTextError $ "Failure parsing VideoCodec from value: '" <> e
           <> "'. Accepted values: frame_capture, h_264, h_265, mpeg2, prores"

instance ToText VideoCodec where
    toText = \case
        FrameCapture -> "FRAME_CAPTURE"
        H264 -> "H_264"
        H265 -> "H_265"
        MPEG2 -> "MPEG2"
        Prores -> "PRORES"

instance Hashable     VideoCodec
instance NFData       VideoCodec
instance ToByteString VideoCodec
instance ToQuery      VideoCodec
instance ToHeader     VideoCodec

instance ToJSON VideoCodec where
    toJSON = toJSONText

instance FromJSON VideoCodec where
    parseJSON = parseJSONText "VideoCodec"

-- | Enable Timecode insertion to include timecode information in this output. Do this in the API by setting (VideoTimecodeInsertion) to (PIC_TIMING_SEI). To get timecodes to appear correctly in your output, also set up the timecode configuration for your job in the input settings. Only enable Timecode insertion when the input framerate is identical to output framerate. Disable this setting to remove the timecode from the output. Default is disabled.
data VideoTimecodeInsertion
  = VTIDisabled
  | VTIPicTimingSei
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VideoTimecodeInsertion where
    parser = takeLowerText >>= \case
        "disabled" -> pure VTIDisabled
        "pic_timing_sei" -> pure VTIPicTimingSei
        e -> fromTextError $ "Failure parsing VideoTimecodeInsertion from value: '" <> e
           <> "'. Accepted values: disabled, pic_timing_sei"

instance ToText VideoTimecodeInsertion where
    toText = \case
        VTIDisabled -> "DISABLED"
        VTIPicTimingSei -> "PIC_TIMING_SEI"

instance Hashable     VideoTimecodeInsertion
instance NFData       VideoTimecodeInsertion
instance ToByteString VideoTimecodeInsertion
instance ToQuery      VideoTimecodeInsertion
instance ToHeader     VideoTimecodeInsertion

instance ToJSON VideoTimecodeInsertion where
    toJSON = toJSONText

instance FromJSON VideoTimecodeInsertion where
    parseJSON = parseJSONText "VideoTimecodeInsertion"
