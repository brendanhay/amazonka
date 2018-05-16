{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Sum where

import Network.AWS.Prelude

-- | Placeholder documentation for AacCodingMode
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

-- | Placeholder documentation for AacInputType
data AacInputType
  = BroadcasterMixedAd
  | Normal
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AacInputType where
    parser = takeLowerText >>= \case
        "broadcaster_mixed_ad" -> pure BroadcasterMixedAd
        "normal" -> pure Normal
        e -> fromTextError $ "Failure parsing AacInputType from value: '" <> e
           <> "'. Accepted values: broadcaster_mixed_ad, normal"

instance ToText AacInputType where
    toText = \case
        BroadcasterMixedAd -> "BROADCASTER_MIXED_AD"
        Normal -> "NORMAL"

instance Hashable     AacInputType
instance NFData       AacInputType
instance ToByteString AacInputType
instance ToQuery      AacInputType
instance ToHeader     AacInputType

instance ToJSON AacInputType where
    toJSON = toJSONText

instance FromJSON AacInputType where
    parseJSON = parseJSONText "AacInputType"

-- | Placeholder documentation for AacProfile
data AacProfile
  = HEV1
  | HEV2
  | LC
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AacProfile where
    parser = takeLowerText >>= \case
        "hev1" -> pure HEV1
        "hev2" -> pure HEV2
        "lc" -> pure LC
        e -> fromTextError $ "Failure parsing AacProfile from value: '" <> e
           <> "'. Accepted values: hev1, hev2, lc"

instance ToText AacProfile where
    toText = \case
        HEV1 -> "HEV1"
        HEV2 -> "HEV2"
        LC -> "LC"

instance Hashable     AacProfile
instance NFData       AacProfile
instance ToByteString AacProfile
instance ToQuery      AacProfile
instance ToHeader     AacProfile

instance ToJSON AacProfile where
    toJSON = toJSONText

instance FromJSON AacProfile where
    parseJSON = parseJSONText "AacProfile"

-- | Placeholder documentation for AacRateControlMode
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

-- | Placeholder documentation for AacRawFormat
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

-- | Placeholder documentation for AacSpec
data AacSpec
  = ASMPEG2
  | ASMPEG4
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AacSpec where
    parser = takeLowerText >>= \case
        "mpeg2" -> pure ASMPEG2
        "mpeg4" -> pure ASMPEG4
        e -> fromTextError $ "Failure parsing AacSpec from value: '" <> e
           <> "'. Accepted values: mpeg2, mpeg4"

instance ToText AacSpec where
    toText = \case
        ASMPEG2 -> "MPEG2"
        ASMPEG4 -> "MPEG4"

instance Hashable     AacSpec
instance NFData       AacSpec
instance ToByteString AacSpec
instance ToQuery      AacSpec
instance ToHeader     AacSpec

instance ToJSON AacSpec where
    toJSON = toJSONText

instance FromJSON AacSpec where
    parseJSON = parseJSONText "AacSpec"

-- | Placeholder documentation for AacVbrQuality
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

-- | Placeholder documentation for Ac3BitstreamMode
data Ac3BitstreamMode
  = ABMCommentary
  | ABMCompleteMain
  | ABMDialogue
  | ABMEmergency
  | ABMHearingImpaired
  | ABMMusicAndEffects
  | ABMVisuallyImpaired
  | ABMVoiceOver
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Ac3BitstreamMode where
    parser = takeLowerText >>= \case
        "commentary" -> pure ABMCommentary
        "complete_main" -> pure ABMCompleteMain
        "dialogue" -> pure ABMDialogue
        "emergency" -> pure ABMEmergency
        "hearing_impaired" -> pure ABMHearingImpaired
        "music_and_effects" -> pure ABMMusicAndEffects
        "visually_impaired" -> pure ABMVisuallyImpaired
        "voice_over" -> pure ABMVoiceOver
        e -> fromTextError $ "Failure parsing Ac3BitstreamMode from value: '" <> e
           <> "'. Accepted values: commentary, complete_main, dialogue, emergency, hearing_impaired, music_and_effects, visually_impaired, voice_over"

instance ToText Ac3BitstreamMode where
    toText = \case
        ABMCommentary -> "COMMENTARY"
        ABMCompleteMain -> "COMPLETE_MAIN"
        ABMDialogue -> "DIALOGUE"
        ABMEmergency -> "EMERGENCY"
        ABMHearingImpaired -> "HEARING_IMPAIRED"
        ABMMusicAndEffects -> "MUSIC_AND_EFFECTS"
        ABMVisuallyImpaired -> "VISUALLY_IMPAIRED"
        ABMVoiceOver -> "VOICE_OVER"

instance Hashable     Ac3BitstreamMode
instance NFData       Ac3BitstreamMode
instance ToByteString Ac3BitstreamMode
instance ToQuery      Ac3BitstreamMode
instance ToHeader     Ac3BitstreamMode

instance ToJSON Ac3BitstreamMode where
    toJSON = toJSONText

instance FromJSON Ac3BitstreamMode where
    parseJSON = parseJSONText "Ac3BitstreamMode"

-- | Placeholder documentation for Ac3CodingMode
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

-- | Placeholder documentation for Ac3DrcProfile
data Ac3DrcProfile
  = ADPFilmStandard
  | ADPNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Ac3DrcProfile where
    parser = takeLowerText >>= \case
        "film_standard" -> pure ADPFilmStandard
        "none" -> pure ADPNone
        e -> fromTextError $ "Failure parsing Ac3DrcProfile from value: '" <> e
           <> "'. Accepted values: film_standard, none"

instance ToText Ac3DrcProfile where
    toText = \case
        ADPFilmStandard -> "FILM_STANDARD"
        ADPNone -> "NONE"

instance Hashable     Ac3DrcProfile
instance NFData       Ac3DrcProfile
instance ToByteString Ac3DrcProfile
instance ToQuery      Ac3DrcProfile
instance ToHeader     Ac3DrcProfile

instance ToJSON Ac3DrcProfile where
    toJSON = toJSONText

instance FromJSON Ac3DrcProfile where
    parseJSON = parseJSONText "Ac3DrcProfile"

-- | Placeholder documentation for Ac3LfeFilter
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

-- | Placeholder documentation for Ac3MetadataControl
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

-- | Placeholder documentation for AfdSignaling
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

-- | Placeholder documentation for AudioDescriptionAudioTypeControl
data AudioDescriptionAudioTypeControl
  = ADATCFollowInput
  | ADATCUseConfigured
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioDescriptionAudioTypeControl where
    parser = takeLowerText >>= \case
        "follow_input" -> pure ADATCFollowInput
        "use_configured" -> pure ADATCUseConfigured
        e -> fromTextError $ "Failure parsing AudioDescriptionAudioTypeControl from value: '" <> e
           <> "'. Accepted values: follow_input, use_configured"

instance ToText AudioDescriptionAudioTypeControl where
    toText = \case
        ADATCFollowInput -> "FOLLOW_INPUT"
        ADATCUseConfigured -> "USE_CONFIGURED"

instance Hashable     AudioDescriptionAudioTypeControl
instance NFData       AudioDescriptionAudioTypeControl
instance ToByteString AudioDescriptionAudioTypeControl
instance ToQuery      AudioDescriptionAudioTypeControl
instance ToHeader     AudioDescriptionAudioTypeControl

instance ToJSON AudioDescriptionAudioTypeControl where
    toJSON = toJSONText

instance FromJSON AudioDescriptionAudioTypeControl where
    parseJSON = parseJSONText "AudioDescriptionAudioTypeControl"

-- | Placeholder documentation for AudioDescriptionLanguageCodeControl
data AudioDescriptionLanguageCodeControl
  = ADLCCFollowInput
  | ADLCCUseConfigured
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioDescriptionLanguageCodeControl where
    parser = takeLowerText >>= \case
        "follow_input" -> pure ADLCCFollowInput
        "use_configured" -> pure ADLCCUseConfigured
        e -> fromTextError $ "Failure parsing AudioDescriptionLanguageCodeControl from value: '" <> e
           <> "'. Accepted values: follow_input, use_configured"

instance ToText AudioDescriptionLanguageCodeControl where
    toText = \case
        ADLCCFollowInput -> "FOLLOW_INPUT"
        ADLCCUseConfigured -> "USE_CONFIGURED"

instance Hashable     AudioDescriptionLanguageCodeControl
instance NFData       AudioDescriptionLanguageCodeControl
instance ToByteString AudioDescriptionLanguageCodeControl
instance ToQuery      AudioDescriptionLanguageCodeControl
instance ToHeader     AudioDescriptionLanguageCodeControl

instance ToJSON AudioDescriptionLanguageCodeControl where
    toJSON = toJSONText

instance FromJSON AudioDescriptionLanguageCodeControl where
    parseJSON = parseJSONText "AudioDescriptionLanguageCodeControl"

-- | Placeholder documentation for AudioLanguageSelectionPolicy
data AudioLanguageSelectionPolicy
  = Loose
  | Strict
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioLanguageSelectionPolicy where
    parser = takeLowerText >>= \case
        "loose" -> pure Loose
        "strict" -> pure Strict
        e -> fromTextError $ "Failure parsing AudioLanguageSelectionPolicy from value: '" <> e
           <> "'. Accepted values: loose, strict"

instance ToText AudioLanguageSelectionPolicy where
    toText = \case
        Loose -> "LOOSE"
        Strict -> "STRICT"

instance Hashable     AudioLanguageSelectionPolicy
instance NFData       AudioLanguageSelectionPolicy
instance ToByteString AudioLanguageSelectionPolicy
instance ToQuery      AudioLanguageSelectionPolicy
instance ToHeader     AudioLanguageSelectionPolicy

instance ToJSON AudioLanguageSelectionPolicy where
    toJSON = toJSONText

instance FromJSON AudioLanguageSelectionPolicy where
    parseJSON = parseJSONText "AudioLanguageSelectionPolicy"

-- | Placeholder documentation for AudioNormalizationAlgorithm
data AudioNormalizationAlgorithm
  = Itu17701
  | Itu17702
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioNormalizationAlgorithm where
    parser = takeLowerText >>= \case
        "itu_1770_1" -> pure Itu17701
        "itu_1770_2" -> pure Itu17702
        e -> fromTextError $ "Failure parsing AudioNormalizationAlgorithm from value: '" <> e
           <> "'. Accepted values: itu_1770_1, itu_1770_2"

instance ToText AudioNormalizationAlgorithm where
    toText = \case
        Itu17701 -> "ITU_1770_1"
        Itu17702 -> "ITU_1770_2"

instance Hashable     AudioNormalizationAlgorithm
instance NFData       AudioNormalizationAlgorithm
instance ToByteString AudioNormalizationAlgorithm
instance ToQuery      AudioNormalizationAlgorithm
instance ToHeader     AudioNormalizationAlgorithm

instance ToJSON AudioNormalizationAlgorithm where
    toJSON = toJSONText

instance FromJSON AudioNormalizationAlgorithm where
    parseJSON = parseJSONText "AudioNormalizationAlgorithm"

-- | Placeholder documentation for AudioNormalizationAlgorithmControl
data AudioNormalizationAlgorithmControl =
  CorrectAudio
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioNormalizationAlgorithmControl where
    parser = takeLowerText >>= \case
        "correct_audio" -> pure CorrectAudio
        e -> fromTextError $ "Failure parsing AudioNormalizationAlgorithmControl from value: '" <> e
           <> "'. Accepted values: correct_audio"

instance ToText AudioNormalizationAlgorithmControl where
    toText = \case
        CorrectAudio -> "CORRECT_AUDIO"

instance Hashable     AudioNormalizationAlgorithmControl
instance NFData       AudioNormalizationAlgorithmControl
instance ToByteString AudioNormalizationAlgorithmControl
instance ToQuery      AudioNormalizationAlgorithmControl
instance ToHeader     AudioNormalizationAlgorithmControl

instance ToJSON AudioNormalizationAlgorithmControl where
    toJSON = toJSONText

instance FromJSON AudioNormalizationAlgorithmControl where
    parseJSON = parseJSONText "AudioNormalizationAlgorithmControl"

-- | Placeholder documentation for AudioOnlyHlsTrackType
data AudioOnlyHlsTrackType
  = AlternateAudioAutoSelect
  | AlternateAudioAutoSelectDefault
  | AlternateAudioNotAutoSelect
  | AudioOnlyVariantStream
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioOnlyHlsTrackType where
    parser = takeLowerText >>= \case
        "alternate_audio_auto_select" -> pure AlternateAudioAutoSelect
        "alternate_audio_auto_select_default" -> pure AlternateAudioAutoSelectDefault
        "alternate_audio_not_auto_select" -> pure AlternateAudioNotAutoSelect
        "audio_only_variant_stream" -> pure AudioOnlyVariantStream
        e -> fromTextError $ "Failure parsing AudioOnlyHlsTrackType from value: '" <> e
           <> "'. Accepted values: alternate_audio_auto_select, alternate_audio_auto_select_default, alternate_audio_not_auto_select, audio_only_variant_stream"

instance ToText AudioOnlyHlsTrackType where
    toText = \case
        AlternateAudioAutoSelect -> "ALTERNATE_AUDIO_AUTO_SELECT"
        AlternateAudioAutoSelectDefault -> "ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT"
        AlternateAudioNotAutoSelect -> "ALTERNATE_AUDIO_NOT_AUTO_SELECT"
        AudioOnlyVariantStream -> "AUDIO_ONLY_VARIANT_STREAM"

instance Hashable     AudioOnlyHlsTrackType
instance NFData       AudioOnlyHlsTrackType
instance ToByteString AudioOnlyHlsTrackType
instance ToQuery      AudioOnlyHlsTrackType
instance ToHeader     AudioOnlyHlsTrackType

instance ToJSON AudioOnlyHlsTrackType where
    toJSON = toJSONText

instance FromJSON AudioOnlyHlsTrackType where
    parseJSON = parseJSONText "AudioOnlyHlsTrackType"

-- | Placeholder documentation for AudioType
data AudioType
  = CleanEffects
  | HearingImpaired
  | Undefined
  | VisualImpairedCommentary
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AudioType where
    parser = takeLowerText >>= \case
        "clean_effects" -> pure CleanEffects
        "hearing_impaired" -> pure HearingImpaired
        "undefined" -> pure Undefined
        "visual_impaired_commentary" -> pure VisualImpairedCommentary
        e -> fromTextError $ "Failure parsing AudioType from value: '" <> e
           <> "'. Accepted values: clean_effects, hearing_impaired, undefined, visual_impaired_commentary"

instance ToText AudioType where
    toText = \case
        CleanEffects -> "CLEAN_EFFECTS"
        HearingImpaired -> "HEARING_IMPAIRED"
        Undefined -> "UNDEFINED"
        VisualImpairedCommentary -> "VISUAL_IMPAIRED_COMMENTARY"

instance Hashable     AudioType
instance NFData       AudioType
instance ToByteString AudioType
instance ToQuery      AudioType
instance ToHeader     AudioType

instance ToJSON AudioType where
    toJSON = toJSONText

instance FromJSON AudioType where
    parseJSON = parseJSONText "AudioType"

-- | Placeholder documentation for AuthenticationScheme
data AuthenticationScheme
  = Akamai
  | Common
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AuthenticationScheme where
    parser = takeLowerText >>= \case
        "akamai" -> pure Akamai
        "common" -> pure Common
        e -> fromTextError $ "Failure parsing AuthenticationScheme from value: '" <> e
           <> "'. Accepted values: akamai, common"

instance ToText AuthenticationScheme where
    toText = \case
        Akamai -> "AKAMAI"
        Common -> "COMMON"

instance Hashable     AuthenticationScheme
instance NFData       AuthenticationScheme
instance ToByteString AuthenticationScheme
instance ToQuery      AuthenticationScheme
instance ToHeader     AuthenticationScheme

instance ToJSON AuthenticationScheme where
    toJSON = toJSONText

instance FromJSON AuthenticationScheme where
    parseJSON = parseJSONText "AuthenticationScheme"

-- | Placeholder documentation for AvailBlankingState
data AvailBlankingState
  = ABSDisabled
  | ABSEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AvailBlankingState where
    parser = takeLowerText >>= \case
        "disabled" -> pure ABSDisabled
        "enabled" -> pure ABSEnabled
        e -> fromTextError $ "Failure parsing AvailBlankingState from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText AvailBlankingState where
    toText = \case
        ABSDisabled -> "DISABLED"
        ABSEnabled -> "ENABLED"

instance Hashable     AvailBlankingState
instance NFData       AvailBlankingState
instance ToByteString AvailBlankingState
instance ToQuery      AvailBlankingState
instance ToHeader     AvailBlankingState

instance ToJSON AvailBlankingState where
    toJSON = toJSONText

instance FromJSON AvailBlankingState where
    parseJSON = parseJSONText "AvailBlankingState"

-- | Placeholder documentation for BlackoutSlateNetworkEndBlackout
data BlackoutSlateNetworkEndBlackout
  = BSNEBDisabled
  | BSNEBEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BlackoutSlateNetworkEndBlackout where
    parser = takeLowerText >>= \case
        "disabled" -> pure BSNEBDisabled
        "enabled" -> pure BSNEBEnabled
        e -> fromTextError $ "Failure parsing BlackoutSlateNetworkEndBlackout from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText BlackoutSlateNetworkEndBlackout where
    toText = \case
        BSNEBDisabled -> "DISABLED"
        BSNEBEnabled -> "ENABLED"

instance Hashable     BlackoutSlateNetworkEndBlackout
instance NFData       BlackoutSlateNetworkEndBlackout
instance ToByteString BlackoutSlateNetworkEndBlackout
instance ToQuery      BlackoutSlateNetworkEndBlackout
instance ToHeader     BlackoutSlateNetworkEndBlackout

instance ToJSON BlackoutSlateNetworkEndBlackout where
    toJSON = toJSONText

instance FromJSON BlackoutSlateNetworkEndBlackout where
    parseJSON = parseJSONText "BlackoutSlateNetworkEndBlackout"

-- | Placeholder documentation for BlackoutSlateState
data BlackoutSlateState
  = BSSDisabled
  | BSSEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BlackoutSlateState where
    parser = takeLowerText >>= \case
        "disabled" -> pure BSSDisabled
        "enabled" -> pure BSSEnabled
        e -> fromTextError $ "Failure parsing BlackoutSlateState from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText BlackoutSlateState where
    toText = \case
        BSSDisabled -> "DISABLED"
        BSSEnabled -> "ENABLED"

instance Hashable     BlackoutSlateState
instance NFData       BlackoutSlateState
instance ToByteString BlackoutSlateState
instance ToQuery      BlackoutSlateState
instance ToHeader     BlackoutSlateState

instance ToJSON BlackoutSlateState where
    toJSON = toJSONText

instance FromJSON BlackoutSlateState where
    parseJSON = parseJSONText "BlackoutSlateState"

-- | Placeholder documentation for BurnInAlignment
data BurnInAlignment
  = BIACentered
  | BIALeft'
  | BIASmart
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BurnInAlignment where
    parser = takeLowerText >>= \case
        "centered" -> pure BIACentered
        "left" -> pure BIALeft'
        "smart" -> pure BIASmart
        e -> fromTextError $ "Failure parsing BurnInAlignment from value: '" <> e
           <> "'. Accepted values: centered, left, smart"

instance ToText BurnInAlignment where
    toText = \case
        BIACentered -> "CENTERED"
        BIALeft' -> "LEFT"
        BIASmart -> "SMART"

instance Hashable     BurnInAlignment
instance NFData       BurnInAlignment
instance ToByteString BurnInAlignment
instance ToQuery      BurnInAlignment
instance ToHeader     BurnInAlignment

instance ToJSON BurnInAlignment where
    toJSON = toJSONText

instance FromJSON BurnInAlignment where
    parseJSON = parseJSONText "BurnInAlignment"

-- | Placeholder documentation for BurnInBackgroundColor
data BurnInBackgroundColor
  = BIBCBlack
  | BIBCNone
  | BIBCWhite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BurnInBackgroundColor where
    parser = takeLowerText >>= \case
        "black" -> pure BIBCBlack
        "none" -> pure BIBCNone
        "white" -> pure BIBCWhite
        e -> fromTextError $ "Failure parsing BurnInBackgroundColor from value: '" <> e
           <> "'. Accepted values: black, none, white"

instance ToText BurnInBackgroundColor where
    toText = \case
        BIBCBlack -> "BLACK"
        BIBCNone -> "NONE"
        BIBCWhite -> "WHITE"

instance Hashable     BurnInBackgroundColor
instance NFData       BurnInBackgroundColor
instance ToByteString BurnInBackgroundColor
instance ToQuery      BurnInBackgroundColor
instance ToHeader     BurnInBackgroundColor

instance ToJSON BurnInBackgroundColor where
    toJSON = toJSONText

instance FromJSON BurnInBackgroundColor where
    parseJSON = parseJSONText "BurnInBackgroundColor"

-- | Placeholder documentation for BurnInFontColor
data BurnInFontColor
  = BIFCBlack
  | BIFCBlue
  | BIFCGreen
  | BIFCRed
  | BIFCWhite
  | BIFCYellow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BurnInFontColor where
    parser = takeLowerText >>= \case
        "black" -> pure BIFCBlack
        "blue" -> pure BIFCBlue
        "green" -> pure BIFCGreen
        "red" -> pure BIFCRed
        "white" -> pure BIFCWhite
        "yellow" -> pure BIFCYellow
        e -> fromTextError $ "Failure parsing BurnInFontColor from value: '" <> e
           <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText BurnInFontColor where
    toText = \case
        BIFCBlack -> "BLACK"
        BIFCBlue -> "BLUE"
        BIFCGreen -> "GREEN"
        BIFCRed -> "RED"
        BIFCWhite -> "WHITE"
        BIFCYellow -> "YELLOW"

instance Hashable     BurnInFontColor
instance NFData       BurnInFontColor
instance ToByteString BurnInFontColor
instance ToQuery      BurnInFontColor
instance ToHeader     BurnInFontColor

instance ToJSON BurnInFontColor where
    toJSON = toJSONText

instance FromJSON BurnInFontColor where
    parseJSON = parseJSONText "BurnInFontColor"

-- | Placeholder documentation for BurnInOutlineColor
data BurnInOutlineColor
  = BIOCBlack
  | BIOCBlue
  | BIOCGreen
  | BIOCRed
  | BIOCWhite
  | BIOCYellow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BurnInOutlineColor where
    parser = takeLowerText >>= \case
        "black" -> pure BIOCBlack
        "blue" -> pure BIOCBlue
        "green" -> pure BIOCGreen
        "red" -> pure BIOCRed
        "white" -> pure BIOCWhite
        "yellow" -> pure BIOCYellow
        e -> fromTextError $ "Failure parsing BurnInOutlineColor from value: '" <> e
           <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText BurnInOutlineColor where
    toText = \case
        BIOCBlack -> "BLACK"
        BIOCBlue -> "BLUE"
        BIOCGreen -> "GREEN"
        BIOCRed -> "RED"
        BIOCWhite -> "WHITE"
        BIOCYellow -> "YELLOW"

instance Hashable     BurnInOutlineColor
instance NFData       BurnInOutlineColor
instance ToByteString BurnInOutlineColor
instance ToQuery      BurnInOutlineColor
instance ToHeader     BurnInOutlineColor

instance ToJSON BurnInOutlineColor where
    toJSON = toJSONText

instance FromJSON BurnInOutlineColor where
    parseJSON = parseJSONText "BurnInOutlineColor"

-- | Placeholder documentation for BurnInShadowColor
data BurnInShadowColor
  = BISCBlack
  | BISCNone
  | BISCWhite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BurnInShadowColor where
    parser = takeLowerText >>= \case
        "black" -> pure BISCBlack
        "none" -> pure BISCNone
        "white" -> pure BISCWhite
        e -> fromTextError $ "Failure parsing BurnInShadowColor from value: '" <> e
           <> "'. Accepted values: black, none, white"

instance ToText BurnInShadowColor where
    toText = \case
        BISCBlack -> "BLACK"
        BISCNone -> "NONE"
        BISCWhite -> "WHITE"

instance Hashable     BurnInShadowColor
instance NFData       BurnInShadowColor
instance ToByteString BurnInShadowColor
instance ToQuery      BurnInShadowColor
instance ToHeader     BurnInShadowColor

instance ToJSON BurnInShadowColor where
    toJSON = toJSONText

instance FromJSON BurnInShadowColor where
    parseJSON = parseJSONText "BurnInShadowColor"

-- | Placeholder documentation for BurnInTeletextGridControl
data BurnInTeletextGridControl
  = BITGCFixed
  | BITGCScaled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BurnInTeletextGridControl where
    parser = takeLowerText >>= \case
        "fixed" -> pure BITGCFixed
        "scaled" -> pure BITGCScaled
        e -> fromTextError $ "Failure parsing BurnInTeletextGridControl from value: '" <> e
           <> "'. Accepted values: fixed, scaled"

instance ToText BurnInTeletextGridControl where
    toText = \case
        BITGCFixed -> "FIXED"
        BITGCScaled -> "SCALED"

instance Hashable     BurnInTeletextGridControl
instance NFData       BurnInTeletextGridControl
instance ToByteString BurnInTeletextGridControl
instance ToQuery      BurnInTeletextGridControl
instance ToHeader     BurnInTeletextGridControl

instance ToJSON BurnInTeletextGridControl where
    toJSON = toJSONText

instance FromJSON BurnInTeletextGridControl where
    parseJSON = parseJSONText "BurnInTeletextGridControl"

-- | Placeholder documentation for ChannelState
data ChannelState
  = CreateFailed
  | Creating
  | Deleted
  | Deleting
  | Idle
  | Recovering
  | Running
  | Starting
  | Stopping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChannelState where
    parser = takeLowerText >>= \case
        "create_failed" -> pure CreateFailed
        "creating" -> pure Creating
        "deleted" -> pure Deleted
        "deleting" -> pure Deleting
        "idle" -> pure Idle
        "recovering" -> pure Recovering
        "running" -> pure Running
        "starting" -> pure Starting
        "stopping" -> pure Stopping
        e -> fromTextError $ "Failure parsing ChannelState from value: '" <> e
           <> "'. Accepted values: create_failed, creating, deleted, deleting, idle, recovering, running, starting, stopping"

instance ToText ChannelState where
    toText = \case
        CreateFailed -> "CREATE_FAILED"
        Creating -> "CREATING"
        Deleted -> "DELETED"
        Deleting -> "DELETING"
        Idle -> "IDLE"
        Recovering -> "RECOVERING"
        Running -> "RUNNING"
        Starting -> "STARTING"
        Stopping -> "STOPPING"

instance Hashable     ChannelState
instance NFData       ChannelState
instance ToByteString ChannelState
instance ToQuery      ChannelState
instance ToHeader     ChannelState

instance FromJSON ChannelState where
    parseJSON = parseJSONText "ChannelState"

-- | Placeholder documentation for DvbSdtOutputSdt
data DvbSdtOutputSdt
  = SdtFollow
  | SdtFollowIfPresent
  | SdtManual
  | SdtNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DvbSdtOutputSdt where
    parser = takeLowerText >>= \case
        "sdt_follow" -> pure SdtFollow
        "sdt_follow_if_present" -> pure SdtFollowIfPresent
        "sdt_manual" -> pure SdtManual
        "sdt_none" -> pure SdtNone
        e -> fromTextError $ "Failure parsing DvbSdtOutputSdt from value: '" <> e
           <> "'. Accepted values: sdt_follow, sdt_follow_if_present, sdt_manual, sdt_none"

instance ToText DvbSdtOutputSdt where
    toText = \case
        SdtFollow -> "SDT_FOLLOW"
        SdtFollowIfPresent -> "SDT_FOLLOW_IF_PRESENT"
        SdtManual -> "SDT_MANUAL"
        SdtNone -> "SDT_NONE"

instance Hashable     DvbSdtOutputSdt
instance NFData       DvbSdtOutputSdt
instance ToByteString DvbSdtOutputSdt
instance ToQuery      DvbSdtOutputSdt
instance ToHeader     DvbSdtOutputSdt

instance ToJSON DvbSdtOutputSdt where
    toJSON = toJSONText

instance FromJSON DvbSdtOutputSdt where
    parseJSON = parseJSONText "DvbSdtOutputSdt"

-- | Placeholder documentation for DvbSubDestinationAlignment
data DvbSubDestinationAlignment
  = Centered
  | Left'
  | Smart
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DvbSubDestinationAlignment where
    parser = takeLowerText >>= \case
        "centered" -> pure Centered
        "left" -> pure Left'
        "smart" -> pure Smart
        e -> fromTextError $ "Failure parsing DvbSubDestinationAlignment from value: '" <> e
           <> "'. Accepted values: centered, left, smart"

instance ToText DvbSubDestinationAlignment where
    toText = \case
        Centered -> "CENTERED"
        Left' -> "LEFT"
        Smart -> "SMART"

instance Hashable     DvbSubDestinationAlignment
instance NFData       DvbSubDestinationAlignment
instance ToByteString DvbSubDestinationAlignment
instance ToQuery      DvbSubDestinationAlignment
instance ToHeader     DvbSubDestinationAlignment

instance ToJSON DvbSubDestinationAlignment where
    toJSON = toJSONText

instance FromJSON DvbSubDestinationAlignment where
    parseJSON = parseJSONText "DvbSubDestinationAlignment"

-- | Placeholder documentation for DvbSubDestinationBackgroundColor
data DvbSubDestinationBackgroundColor
  = DSDBCBlack
  | DSDBCNone
  | DSDBCWhite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DvbSubDestinationBackgroundColor where
    parser = takeLowerText >>= \case
        "black" -> pure DSDBCBlack
        "none" -> pure DSDBCNone
        "white" -> pure DSDBCWhite
        e -> fromTextError $ "Failure parsing DvbSubDestinationBackgroundColor from value: '" <> e
           <> "'. Accepted values: black, none, white"

instance ToText DvbSubDestinationBackgroundColor where
    toText = \case
        DSDBCBlack -> "BLACK"
        DSDBCNone -> "NONE"
        DSDBCWhite -> "WHITE"

instance Hashable     DvbSubDestinationBackgroundColor
instance NFData       DvbSubDestinationBackgroundColor
instance ToByteString DvbSubDestinationBackgroundColor
instance ToQuery      DvbSubDestinationBackgroundColor
instance ToHeader     DvbSubDestinationBackgroundColor

instance ToJSON DvbSubDestinationBackgroundColor where
    toJSON = toJSONText

instance FromJSON DvbSubDestinationBackgroundColor where
    parseJSON = parseJSONText "DvbSubDestinationBackgroundColor"

-- | Placeholder documentation for DvbSubDestinationFontColor
data DvbSubDestinationFontColor
  = DSDFCBlack
  | DSDFCBlue
  | DSDFCGreen
  | DSDFCRed
  | DSDFCWhite
  | DSDFCYellow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DvbSubDestinationFontColor where
    parser = takeLowerText >>= \case
        "black" -> pure DSDFCBlack
        "blue" -> pure DSDFCBlue
        "green" -> pure DSDFCGreen
        "red" -> pure DSDFCRed
        "white" -> pure DSDFCWhite
        "yellow" -> pure DSDFCYellow
        e -> fromTextError $ "Failure parsing DvbSubDestinationFontColor from value: '" <> e
           <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText DvbSubDestinationFontColor where
    toText = \case
        DSDFCBlack -> "BLACK"
        DSDFCBlue -> "BLUE"
        DSDFCGreen -> "GREEN"
        DSDFCRed -> "RED"
        DSDFCWhite -> "WHITE"
        DSDFCYellow -> "YELLOW"

instance Hashable     DvbSubDestinationFontColor
instance NFData       DvbSubDestinationFontColor
instance ToByteString DvbSubDestinationFontColor
instance ToQuery      DvbSubDestinationFontColor
instance ToHeader     DvbSubDestinationFontColor

instance ToJSON DvbSubDestinationFontColor where
    toJSON = toJSONText

instance FromJSON DvbSubDestinationFontColor where
    parseJSON = parseJSONText "DvbSubDestinationFontColor"

-- | Placeholder documentation for DvbSubDestinationOutlineColor
data DvbSubDestinationOutlineColor
  = Black
  | Blue
  | Green
  | Red
  | White
  | Yellow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DvbSubDestinationOutlineColor where
    parser = takeLowerText >>= \case
        "black" -> pure Black
        "blue" -> pure Blue
        "green" -> pure Green
        "red" -> pure Red
        "white" -> pure White
        "yellow" -> pure Yellow
        e -> fromTextError $ "Failure parsing DvbSubDestinationOutlineColor from value: '" <> e
           <> "'. Accepted values: black, blue, green, red, white, yellow"

instance ToText DvbSubDestinationOutlineColor where
    toText = \case
        Black -> "BLACK"
        Blue -> "BLUE"
        Green -> "GREEN"
        Red -> "RED"
        White -> "WHITE"
        Yellow -> "YELLOW"

instance Hashable     DvbSubDestinationOutlineColor
instance NFData       DvbSubDestinationOutlineColor
instance ToByteString DvbSubDestinationOutlineColor
instance ToQuery      DvbSubDestinationOutlineColor
instance ToHeader     DvbSubDestinationOutlineColor

instance ToJSON DvbSubDestinationOutlineColor where
    toJSON = toJSONText

instance FromJSON DvbSubDestinationOutlineColor where
    parseJSON = parseJSONText "DvbSubDestinationOutlineColor"

-- | Placeholder documentation for DvbSubDestinationShadowColor
data DvbSubDestinationShadowColor
  = DSDSCBlack
  | DSDSCNone
  | DSDSCWhite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DvbSubDestinationShadowColor where
    parser = takeLowerText >>= \case
        "black" -> pure DSDSCBlack
        "none" -> pure DSDSCNone
        "white" -> pure DSDSCWhite
        e -> fromTextError $ "Failure parsing DvbSubDestinationShadowColor from value: '" <> e
           <> "'. Accepted values: black, none, white"

instance ToText DvbSubDestinationShadowColor where
    toText = \case
        DSDSCBlack -> "BLACK"
        DSDSCNone -> "NONE"
        DSDSCWhite -> "WHITE"

instance Hashable     DvbSubDestinationShadowColor
instance NFData       DvbSubDestinationShadowColor
instance ToByteString DvbSubDestinationShadowColor
instance ToQuery      DvbSubDestinationShadowColor
instance ToHeader     DvbSubDestinationShadowColor

instance ToJSON DvbSubDestinationShadowColor where
    toJSON = toJSONText

instance FromJSON DvbSubDestinationShadowColor where
    parseJSON = parseJSONText "DvbSubDestinationShadowColor"

-- | Placeholder documentation for DvbSubDestinationTeletextGridControl
data DvbSubDestinationTeletextGridControl
  = Fixed
  | Scaled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DvbSubDestinationTeletextGridControl where
    parser = takeLowerText >>= \case
        "fixed" -> pure Fixed
        "scaled" -> pure Scaled
        e -> fromTextError $ "Failure parsing DvbSubDestinationTeletextGridControl from value: '" <> e
           <> "'. Accepted values: fixed, scaled"

instance ToText DvbSubDestinationTeletextGridControl where
    toText = \case
        Fixed -> "FIXED"
        Scaled -> "SCALED"

instance Hashable     DvbSubDestinationTeletextGridControl
instance NFData       DvbSubDestinationTeletextGridControl
instance ToByteString DvbSubDestinationTeletextGridControl
instance ToQuery      DvbSubDestinationTeletextGridControl
instance ToHeader     DvbSubDestinationTeletextGridControl

instance ToJSON DvbSubDestinationTeletextGridControl where
    toJSON = toJSONText

instance FromJSON DvbSubDestinationTeletextGridControl where
    parseJSON = parseJSONText "DvbSubDestinationTeletextGridControl"

-- | Placeholder documentation for Eac3AttenuationControl
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

-- | Placeholder documentation for Eac3BitstreamMode
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

-- | Placeholder documentation for Eac3CodingMode
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

-- | Placeholder documentation for Eac3DcFilter
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

-- | Placeholder documentation for Eac3DrcLine
data Eac3DrcLine
  = EDLFilmLight
  | EDLFilmStandard
  | EDLMusicLight
  | EDLMusicStandard
  | EDLNone
  | EDLSpeech
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3DrcLine where
    parser = takeLowerText >>= \case
        "film_light" -> pure EDLFilmLight
        "film_standard" -> pure EDLFilmStandard
        "music_light" -> pure EDLMusicLight
        "music_standard" -> pure EDLMusicStandard
        "none" -> pure EDLNone
        "speech" -> pure EDLSpeech
        e -> fromTextError $ "Failure parsing Eac3DrcLine from value: '" <> e
           <> "'. Accepted values: film_light, film_standard, music_light, music_standard, none, speech"

instance ToText Eac3DrcLine where
    toText = \case
        EDLFilmLight -> "FILM_LIGHT"
        EDLFilmStandard -> "FILM_STANDARD"
        EDLMusicLight -> "MUSIC_LIGHT"
        EDLMusicStandard -> "MUSIC_STANDARD"
        EDLNone -> "NONE"
        EDLSpeech -> "SPEECH"

instance Hashable     Eac3DrcLine
instance NFData       Eac3DrcLine
instance ToByteString Eac3DrcLine
instance ToQuery      Eac3DrcLine
instance ToHeader     Eac3DrcLine

instance ToJSON Eac3DrcLine where
    toJSON = toJSONText

instance FromJSON Eac3DrcLine where
    parseJSON = parseJSONText "Eac3DrcLine"

-- | Placeholder documentation for Eac3DrcRf
data Eac3DrcRf
  = EDRFilmLight
  | EDRFilmStandard
  | EDRMusicLight
  | EDRMusicStandard
  | EDRNone
  | EDRSpeech
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Eac3DrcRf where
    parser = takeLowerText >>= \case
        "film_light" -> pure EDRFilmLight
        "film_standard" -> pure EDRFilmStandard
        "music_light" -> pure EDRMusicLight
        "music_standard" -> pure EDRMusicStandard
        "none" -> pure EDRNone
        "speech" -> pure EDRSpeech
        e -> fromTextError $ "Failure parsing Eac3DrcRf from value: '" <> e
           <> "'. Accepted values: film_light, film_standard, music_light, music_standard, none, speech"

instance ToText Eac3DrcRf where
    toText = \case
        EDRFilmLight -> "FILM_LIGHT"
        EDRFilmStandard -> "FILM_STANDARD"
        EDRMusicLight -> "MUSIC_LIGHT"
        EDRMusicStandard -> "MUSIC_STANDARD"
        EDRNone -> "NONE"
        EDRSpeech -> "SPEECH"

instance Hashable     Eac3DrcRf
instance NFData       Eac3DrcRf
instance ToByteString Eac3DrcRf
instance ToQuery      Eac3DrcRf
instance ToHeader     Eac3DrcRf

instance ToJSON Eac3DrcRf where
    toJSON = toJSONText

instance FromJSON Eac3DrcRf where
    parseJSON = parseJSONText "Eac3DrcRf"

-- | Placeholder documentation for Eac3LfeControl
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

-- | Placeholder documentation for Eac3LfeFilter
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

-- | Placeholder documentation for Eac3MetadataControl
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

-- | Placeholder documentation for Eac3PassthroughControl
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

-- | Placeholder documentation for Eac3PhaseControl
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

-- | Placeholder documentation for Eac3StereoDownmix
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

-- | Placeholder documentation for Eac3SurroundExMode
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

-- | Placeholder documentation for Eac3SurroundMode
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

-- | Placeholder documentation for EmbeddedConvert608To708
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

-- | Placeholder documentation for EmbeddedScte20Detection
data EmbeddedScte20Detection
  = Auto
  | Off
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EmbeddedScte20Detection where
    parser = takeLowerText >>= \case
        "auto" -> pure Auto
        "off" -> pure Off
        e -> fromTextError $ "Failure parsing EmbeddedScte20Detection from value: '" <> e
           <> "'. Accepted values: auto, off"

instance ToText EmbeddedScte20Detection where
    toText = \case
        Auto -> "AUTO"
        Off -> "OFF"

instance Hashable     EmbeddedScte20Detection
instance NFData       EmbeddedScte20Detection
instance ToByteString EmbeddedScte20Detection
instance ToQuery      EmbeddedScte20Detection
instance ToHeader     EmbeddedScte20Detection

instance ToJSON EmbeddedScte20Detection where
    toJSON = toJSONText

instance FromJSON EmbeddedScte20Detection where
    parseJSON = parseJSONText "EmbeddedScte20Detection"

-- | Placeholder documentation for FecOutputIncludeFec
data FecOutputIncludeFec
  = Column
  | ColumnAndRow
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FecOutputIncludeFec where
    parser = takeLowerText >>= \case
        "column" -> pure Column
        "column_and_row" -> pure ColumnAndRow
        e -> fromTextError $ "Failure parsing FecOutputIncludeFec from value: '" <> e
           <> "'. Accepted values: column, column_and_row"

instance ToText FecOutputIncludeFec where
    toText = \case
        Column -> "COLUMN"
        ColumnAndRow -> "COLUMN_AND_ROW"

instance Hashable     FecOutputIncludeFec
instance NFData       FecOutputIncludeFec
instance ToByteString FecOutputIncludeFec
instance ToQuery      FecOutputIncludeFec
instance ToHeader     FecOutputIncludeFec

instance ToJSON FecOutputIncludeFec where
    toJSON = toJSONText

instance FromJSON FecOutputIncludeFec where
    parseJSON = parseJSONText "FecOutputIncludeFec"

-- | Placeholder documentation for FixedAfd
data FixedAfd
  = Afd0000
  | Afd0010
  | Afd0011
  | Afd0100
  | Afd1000
  | Afd1001
  | Afd1010
  | Afd1011
  | Afd1101
  | Afd1110
  | Afd1111
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FixedAfd where
    parser = takeLowerText >>= \case
        "afd_0000" -> pure Afd0000
        "afd_0010" -> pure Afd0010
        "afd_0011" -> pure Afd0011
        "afd_0100" -> pure Afd0100
        "afd_1000" -> pure Afd1000
        "afd_1001" -> pure Afd1001
        "afd_1010" -> pure Afd1010
        "afd_1011" -> pure Afd1011
        "afd_1101" -> pure Afd1101
        "afd_1110" -> pure Afd1110
        "afd_1111" -> pure Afd1111
        e -> fromTextError $ "Failure parsing FixedAfd from value: '" <> e
           <> "'. Accepted values: afd_0000, afd_0010, afd_0011, afd_0100, afd_1000, afd_1001, afd_1010, afd_1011, afd_1101, afd_1110, afd_1111"

instance ToText FixedAfd where
    toText = \case
        Afd0000 -> "AFD_0000"
        Afd0010 -> "AFD_0010"
        Afd0011 -> "AFD_0011"
        Afd0100 -> "AFD_0100"
        Afd1000 -> "AFD_1000"
        Afd1001 -> "AFD_1001"
        Afd1010 -> "AFD_1010"
        Afd1011 -> "AFD_1011"
        Afd1101 -> "AFD_1101"
        Afd1110 -> "AFD_1110"
        Afd1111 -> "AFD_1111"

instance Hashable     FixedAfd
instance NFData       FixedAfd
instance ToByteString FixedAfd
instance ToQuery      FixedAfd
instance ToHeader     FixedAfd

instance ToJSON FixedAfd where
    toJSON = toJSONText

instance FromJSON FixedAfd where
    parseJSON = parseJSONText "FixedAfd"

-- | Placeholder documentation for GlobalConfigurationInputEndAction
data GlobalConfigurationInputEndAction
  = GCIEANone
  | GCIEASwitchAndLoopInputs
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GlobalConfigurationInputEndAction where
    parser = takeLowerText >>= \case
        "none" -> pure GCIEANone
        "switch_and_loop_inputs" -> pure GCIEASwitchAndLoopInputs
        e -> fromTextError $ "Failure parsing GlobalConfigurationInputEndAction from value: '" <> e
           <> "'. Accepted values: none, switch_and_loop_inputs"

instance ToText GlobalConfigurationInputEndAction where
    toText = \case
        GCIEANone -> "NONE"
        GCIEASwitchAndLoopInputs -> "SWITCH_AND_LOOP_INPUTS"

instance Hashable     GlobalConfigurationInputEndAction
instance NFData       GlobalConfigurationInputEndAction
instance ToByteString GlobalConfigurationInputEndAction
instance ToQuery      GlobalConfigurationInputEndAction
instance ToHeader     GlobalConfigurationInputEndAction

instance ToJSON GlobalConfigurationInputEndAction where
    toJSON = toJSONText

instance FromJSON GlobalConfigurationInputEndAction where
    parseJSON = parseJSONText "GlobalConfigurationInputEndAction"

-- | Placeholder documentation for GlobalConfigurationLowFramerateInputs
data GlobalConfigurationLowFramerateInputs
  = GCLFIDisabled
  | GCLFIEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GlobalConfigurationLowFramerateInputs where
    parser = takeLowerText >>= \case
        "disabled" -> pure GCLFIDisabled
        "enabled" -> pure GCLFIEnabled
        e -> fromTextError $ "Failure parsing GlobalConfigurationLowFramerateInputs from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText GlobalConfigurationLowFramerateInputs where
    toText = \case
        GCLFIDisabled -> "DISABLED"
        GCLFIEnabled -> "ENABLED"

instance Hashable     GlobalConfigurationLowFramerateInputs
instance NFData       GlobalConfigurationLowFramerateInputs
instance ToByteString GlobalConfigurationLowFramerateInputs
instance ToQuery      GlobalConfigurationLowFramerateInputs
instance ToHeader     GlobalConfigurationLowFramerateInputs

instance ToJSON GlobalConfigurationLowFramerateInputs where
    toJSON = toJSONText

instance FromJSON GlobalConfigurationLowFramerateInputs where
    parseJSON = parseJSONText "GlobalConfigurationLowFramerateInputs"

-- | Placeholder documentation for GlobalConfigurationOutputTimingSource
data GlobalConfigurationOutputTimingSource
  = GCOTSInputClock
  | GCOTSSystemClock
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GlobalConfigurationOutputTimingSource where
    parser = takeLowerText >>= \case
        "input_clock" -> pure GCOTSInputClock
        "system_clock" -> pure GCOTSSystemClock
        e -> fromTextError $ "Failure parsing GlobalConfigurationOutputTimingSource from value: '" <> e
           <> "'. Accepted values: input_clock, system_clock"

instance ToText GlobalConfigurationOutputTimingSource where
    toText = \case
        GCOTSInputClock -> "INPUT_CLOCK"
        GCOTSSystemClock -> "SYSTEM_CLOCK"

instance Hashable     GlobalConfigurationOutputTimingSource
instance NFData       GlobalConfigurationOutputTimingSource
instance ToByteString GlobalConfigurationOutputTimingSource
instance ToQuery      GlobalConfigurationOutputTimingSource
instance ToHeader     GlobalConfigurationOutputTimingSource

instance ToJSON GlobalConfigurationOutputTimingSource where
    toJSON = toJSONText

instance FromJSON GlobalConfigurationOutputTimingSource where
    parseJSON = parseJSONText "GlobalConfigurationOutputTimingSource"

-- | Placeholder documentation for H264AdaptiveQuantization
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

-- | Placeholder documentation for H264ColorMetadata
data H264ColorMetadata
  = Ignore
  | Insert
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264ColorMetadata where
    parser = takeLowerText >>= \case
        "ignore" -> pure Ignore
        "insert" -> pure Insert
        e -> fromTextError $ "Failure parsing H264ColorMetadata from value: '" <> e
           <> "'. Accepted values: ignore, insert"

instance ToText H264ColorMetadata where
    toText = \case
        Ignore -> "IGNORE"
        Insert -> "INSERT"

instance Hashable     H264ColorMetadata
instance NFData       H264ColorMetadata
instance ToByteString H264ColorMetadata
instance ToQuery      H264ColorMetadata
instance ToHeader     H264ColorMetadata

instance ToJSON H264ColorMetadata where
    toJSON = toJSONText

instance FromJSON H264ColorMetadata where
    parseJSON = parseJSONText "H264ColorMetadata"

-- | Placeholder documentation for H264EntropyEncoding
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

-- | Placeholder documentation for H264FlickerAq
data H264FlickerAq
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264FlickerAq where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing H264FlickerAq from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264FlickerAq where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable     H264FlickerAq
instance NFData       H264FlickerAq
instance ToByteString H264FlickerAq
instance ToQuery      H264FlickerAq
instance ToHeader     H264FlickerAq

instance ToJSON H264FlickerAq where
    toJSON = toJSONText

instance FromJSON H264FlickerAq where
    parseJSON = parseJSONText "H264FlickerAq"

-- | Placeholder documentation for H264FramerateControl
data H264FramerateControl
  = HFCInitializeFromSource
  | HFCSpecified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264FramerateControl where
    parser = takeLowerText >>= \case
        "initialize_from_source" -> pure HFCInitializeFromSource
        "specified" -> pure HFCSpecified
        e -> fromTextError $ "Failure parsing H264FramerateControl from value: '" <> e
           <> "'. Accepted values: initialize_from_source, specified"

instance ToText H264FramerateControl where
    toText = \case
        HFCInitializeFromSource -> "INITIALIZE_FROM_SOURCE"
        HFCSpecified -> "SPECIFIED"

instance Hashable     H264FramerateControl
instance NFData       H264FramerateControl
instance ToByteString H264FramerateControl
instance ToQuery      H264FramerateControl
instance ToHeader     H264FramerateControl

instance ToJSON H264FramerateControl where
    toJSON = toJSONText

instance FromJSON H264FramerateControl where
    parseJSON = parseJSONText "H264FramerateControl"

-- | Placeholder documentation for H264GopBReference
data H264GopBReference
  = HGBRDisabled
  | HGBREnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264GopBReference where
    parser = takeLowerText >>= \case
        "disabled" -> pure HGBRDisabled
        "enabled" -> pure HGBREnabled
        e -> fromTextError $ "Failure parsing H264GopBReference from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264GopBReference where
    toText = \case
        HGBRDisabled -> "DISABLED"
        HGBREnabled -> "ENABLED"

instance Hashable     H264GopBReference
instance NFData       H264GopBReference
instance ToByteString H264GopBReference
instance ToQuery      H264GopBReference
instance ToHeader     H264GopBReference

instance ToJSON H264GopBReference where
    toJSON = toJSONText

instance FromJSON H264GopBReference where
    parseJSON = parseJSONText "H264GopBReference"

-- | Placeholder documentation for H264GopSizeUnits
data H264GopSizeUnits
  = Frames
  | Seconds
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264GopSizeUnits where
    parser = takeLowerText >>= \case
        "frames" -> pure Frames
        "seconds" -> pure Seconds
        e -> fromTextError $ "Failure parsing H264GopSizeUnits from value: '" <> e
           <> "'. Accepted values: frames, seconds"

instance ToText H264GopSizeUnits where
    toText = \case
        Frames -> "FRAMES"
        Seconds -> "SECONDS"

instance Hashable     H264GopSizeUnits
instance NFData       H264GopSizeUnits
instance ToByteString H264GopSizeUnits
instance ToQuery      H264GopSizeUnits
instance ToHeader     H264GopSizeUnits

instance ToJSON H264GopSizeUnits where
    toJSON = toJSONText

instance FromJSON H264GopSizeUnits where
    parseJSON = parseJSONText "H264GopSizeUnits"

-- | Placeholder documentation for H264Level
data H264Level
  = H264Level1
  | H264Level11
  | H264Level12
  | H264Level13
  | H264Level2
  | H264Level21
  | H264Level22
  | H264Level3
  | H264Level31
  | H264Level32
  | H264Level4
  | H264Level41
  | H264Level42
  | H264Level5
  | H264Level51
  | H264Level52
  | H264LevelAuto
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264Level where
    parser = takeLowerText >>= \case
        "h264_level_1" -> pure H264Level1
        "h264_level_1_1" -> pure H264Level11
        "h264_level_1_2" -> pure H264Level12
        "h264_level_1_3" -> pure H264Level13
        "h264_level_2" -> pure H264Level2
        "h264_level_2_1" -> pure H264Level21
        "h264_level_2_2" -> pure H264Level22
        "h264_level_3" -> pure H264Level3
        "h264_level_3_1" -> pure H264Level31
        "h264_level_3_2" -> pure H264Level32
        "h264_level_4" -> pure H264Level4
        "h264_level_4_1" -> pure H264Level41
        "h264_level_4_2" -> pure H264Level42
        "h264_level_5" -> pure H264Level5
        "h264_level_5_1" -> pure H264Level51
        "h264_level_5_2" -> pure H264Level52
        "h264_level_auto" -> pure H264LevelAuto
        e -> fromTextError $ "Failure parsing H264Level from value: '" <> e
           <> "'. Accepted values: h264_level_1, h264_level_1_1, h264_level_1_2, h264_level_1_3, h264_level_2, h264_level_2_1, h264_level_2_2, h264_level_3, h264_level_3_1, h264_level_3_2, h264_level_4, h264_level_4_1, h264_level_4_2, h264_level_5, h264_level_5_1, h264_level_5_2, h264_level_auto"

instance ToText H264Level where
    toText = \case
        H264Level1 -> "H264_LEVEL_1"
        H264Level11 -> "H264_LEVEL_1_1"
        H264Level12 -> "H264_LEVEL_1_2"
        H264Level13 -> "H264_LEVEL_1_3"
        H264Level2 -> "H264_LEVEL_2"
        H264Level21 -> "H264_LEVEL_2_1"
        H264Level22 -> "H264_LEVEL_2_2"
        H264Level3 -> "H264_LEVEL_3"
        H264Level31 -> "H264_LEVEL_3_1"
        H264Level32 -> "H264_LEVEL_3_2"
        H264Level4 -> "H264_LEVEL_4"
        H264Level41 -> "H264_LEVEL_4_1"
        H264Level42 -> "H264_LEVEL_4_2"
        H264Level5 -> "H264_LEVEL_5"
        H264Level51 -> "H264_LEVEL_5_1"
        H264Level52 -> "H264_LEVEL_5_2"
        H264LevelAuto -> "H264_LEVEL_AUTO"

instance Hashable     H264Level
instance NFData       H264Level
instance ToByteString H264Level
instance ToQuery      H264Level
instance ToHeader     H264Level

instance ToJSON H264Level where
    toJSON = toJSONText

instance FromJSON H264Level where
    parseJSON = parseJSONText "H264Level"

-- | Placeholder documentation for H264LookAheadRateControl
data H264LookAheadRateControl
  = HLARCHigh
  | HLARCLow
  | HLARCMedium
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264LookAheadRateControl where
    parser = takeLowerText >>= \case
        "high" -> pure HLARCHigh
        "low" -> pure HLARCLow
        "medium" -> pure HLARCMedium
        e -> fromTextError $ "Failure parsing H264LookAheadRateControl from value: '" <> e
           <> "'. Accepted values: high, low, medium"

instance ToText H264LookAheadRateControl where
    toText = \case
        HLARCHigh -> "HIGH"
        HLARCLow -> "LOW"
        HLARCMedium -> "MEDIUM"

instance Hashable     H264LookAheadRateControl
instance NFData       H264LookAheadRateControl
instance ToByteString H264LookAheadRateControl
instance ToQuery      H264LookAheadRateControl
instance ToHeader     H264LookAheadRateControl

instance ToJSON H264LookAheadRateControl where
    toJSON = toJSONText

instance FromJSON H264LookAheadRateControl where
    parseJSON = parseJSONText "H264LookAheadRateControl"

-- | Placeholder documentation for H264ParControl
data H264ParControl
  = InitializeFromSource
  | Specified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264ParControl where
    parser = takeLowerText >>= \case
        "initialize_from_source" -> pure InitializeFromSource
        "specified" -> pure Specified
        e -> fromTextError $ "Failure parsing H264ParControl from value: '" <> e
           <> "'. Accepted values: initialize_from_source, specified"

instance ToText H264ParControl where
    toText = \case
        InitializeFromSource -> "INITIALIZE_FROM_SOURCE"
        Specified -> "SPECIFIED"

instance Hashable     H264ParControl
instance NFData       H264ParControl
instance ToByteString H264ParControl
instance ToQuery      H264ParControl
instance ToHeader     H264ParControl

instance ToJSON H264ParControl where
    toJSON = toJSONText

instance FromJSON H264ParControl where
    parseJSON = parseJSONText "H264ParControl"

-- | Placeholder documentation for H264Profile
data H264Profile
  = Baseline
  | High
  | High10BIT
  | High422
  | High42210BIT
  | Main
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264Profile where
    parser = takeLowerText >>= \case
        "baseline" -> pure Baseline
        "high" -> pure High
        "high_10bit" -> pure High10BIT
        "high_422" -> pure High422
        "high_422_10bit" -> pure High42210BIT
        "main" -> pure Main
        e -> fromTextError $ "Failure parsing H264Profile from value: '" <> e
           <> "'. Accepted values: baseline, high, high_10bit, high_422, high_422_10bit, main"

instance ToText H264Profile where
    toText = \case
        Baseline -> "BASELINE"
        High -> "HIGH"
        High10BIT -> "HIGH_10BIT"
        High422 -> "HIGH_422"
        High42210BIT -> "HIGH_422_10BIT"
        Main -> "MAIN"

instance Hashable     H264Profile
instance NFData       H264Profile
instance ToByteString H264Profile
instance ToQuery      H264Profile
instance ToHeader     H264Profile

instance ToJSON H264Profile where
    toJSON = toJSONText

instance FromJSON H264Profile where
    parseJSON = parseJSONText "H264Profile"

-- | Placeholder documentation for H264RateControlMode
data H264RateControlMode
  = Cbr
  | Vbr
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264RateControlMode where
    parser = takeLowerText >>= \case
        "cbr" -> pure Cbr
        "vbr" -> pure Vbr
        e -> fromTextError $ "Failure parsing H264RateControlMode from value: '" <> e
           <> "'. Accepted values: cbr, vbr"

instance ToText H264RateControlMode where
    toText = \case
        Cbr -> "CBR"
        Vbr -> "VBR"

instance Hashable     H264RateControlMode
instance NFData       H264RateControlMode
instance ToByteString H264RateControlMode
instance ToQuery      H264RateControlMode
instance ToHeader     H264RateControlMode

instance ToJSON H264RateControlMode where
    toJSON = toJSONText

instance FromJSON H264RateControlMode where
    parseJSON = parseJSONText "H264RateControlMode"

-- | Placeholder documentation for H264ScanType
data H264ScanType
  = Interlaced
  | Progressive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264ScanType where
    parser = takeLowerText >>= \case
        "interlaced" -> pure Interlaced
        "progressive" -> pure Progressive
        e -> fromTextError $ "Failure parsing H264ScanType from value: '" <> e
           <> "'. Accepted values: interlaced, progressive"

instance ToText H264ScanType where
    toText = \case
        Interlaced -> "INTERLACED"
        Progressive -> "PROGRESSIVE"

instance Hashable     H264ScanType
instance NFData       H264ScanType
instance ToByteString H264ScanType
instance ToQuery      H264ScanType
instance ToHeader     H264ScanType

instance ToJSON H264ScanType where
    toJSON = toJSONText

instance FromJSON H264ScanType where
    parseJSON = parseJSONText "H264ScanType"

-- | Placeholder documentation for H264SceneChangeDetect
data H264SceneChangeDetect
  = HSCDDisabled
  | HSCDEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264SceneChangeDetect where
    parser = takeLowerText >>= \case
        "disabled" -> pure HSCDDisabled
        "enabled" -> pure HSCDEnabled
        e -> fromTextError $ "Failure parsing H264SceneChangeDetect from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264SceneChangeDetect where
    toText = \case
        HSCDDisabled -> "DISABLED"
        HSCDEnabled -> "ENABLED"

instance Hashable     H264SceneChangeDetect
instance NFData       H264SceneChangeDetect
instance ToByteString H264SceneChangeDetect
instance ToQuery      H264SceneChangeDetect
instance ToHeader     H264SceneChangeDetect

instance ToJSON H264SceneChangeDetect where
    toJSON = toJSONText

instance FromJSON H264SceneChangeDetect where
    parseJSON = parseJSONText "H264SceneChangeDetect"

-- | Placeholder documentation for H264SpatialAq
data H264SpatialAq
  = HSADisabled
  | HSAEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264SpatialAq where
    parser = takeLowerText >>= \case
        "disabled" -> pure HSADisabled
        "enabled" -> pure HSAEnabled
        e -> fromTextError $ "Failure parsing H264SpatialAq from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264SpatialAq where
    toText = \case
        HSADisabled -> "DISABLED"
        HSAEnabled -> "ENABLED"

instance Hashable     H264SpatialAq
instance NFData       H264SpatialAq
instance ToByteString H264SpatialAq
instance ToQuery      H264SpatialAq
instance ToHeader     H264SpatialAq

instance ToJSON H264SpatialAq where
    toJSON = toJSONText

instance FromJSON H264SpatialAq where
    parseJSON = parseJSONText "H264SpatialAq"

-- | Placeholder documentation for H264Syntax
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

-- | Placeholder documentation for H264TemporalAq
data H264TemporalAq
  = HTADisabled
  | HTAEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264TemporalAq where
    parser = takeLowerText >>= \case
        "disabled" -> pure HTADisabled
        "enabled" -> pure HTAEnabled
        e -> fromTextError $ "Failure parsing H264TemporalAq from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264TemporalAq where
    toText = \case
        HTADisabled -> "DISABLED"
        HTAEnabled -> "ENABLED"

instance Hashable     H264TemporalAq
instance NFData       H264TemporalAq
instance ToByteString H264TemporalAq
instance ToQuery      H264TemporalAq
instance ToHeader     H264TemporalAq

instance ToJSON H264TemporalAq where
    toJSON = toJSONText

instance FromJSON H264TemporalAq where
    parseJSON = parseJSONText "H264TemporalAq"

-- | Placeholder documentation for H264TimecodeInsertionBehavior
data H264TimecodeInsertionBehavior
  = HTIBDisabled
  | HTIBPicTimingSei
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText H264TimecodeInsertionBehavior where
    parser = takeLowerText >>= \case
        "disabled" -> pure HTIBDisabled
        "pic_timing_sei" -> pure HTIBPicTimingSei
        e -> fromTextError $ "Failure parsing H264TimecodeInsertionBehavior from value: '" <> e
           <> "'. Accepted values: disabled, pic_timing_sei"

instance ToText H264TimecodeInsertionBehavior where
    toText = \case
        HTIBDisabled -> "DISABLED"
        HTIBPicTimingSei -> "PIC_TIMING_SEI"

instance Hashable     H264TimecodeInsertionBehavior
instance NFData       H264TimecodeInsertionBehavior
instance ToByteString H264TimecodeInsertionBehavior
instance ToQuery      H264TimecodeInsertionBehavior
instance ToHeader     H264TimecodeInsertionBehavior

instance ToJSON H264TimecodeInsertionBehavior where
    toJSON = toJSONText

instance FromJSON H264TimecodeInsertionBehavior where
    parseJSON = parseJSONText "H264TimecodeInsertionBehavior"

-- | Placeholder documentation for HlsAdMarkers
data HlsAdMarkers
  = Adobe
  | Elemental
  | ElementalSCTE35
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsAdMarkers where
    parser = takeLowerText >>= \case
        "adobe" -> pure Adobe
        "elemental" -> pure Elemental
        "elemental_scte35" -> pure ElementalSCTE35
        e -> fromTextError $ "Failure parsing HlsAdMarkers from value: '" <> e
           <> "'. Accepted values: adobe, elemental, elemental_scte35"

instance ToText HlsAdMarkers where
    toText = \case
        Adobe -> "ADOBE"
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

-- | Placeholder documentation for HlsAkamaiHttpTransferMode
data HlsAkamaiHTTPTransferMode
  = AkamaiChunked
  | AkamaiNonChunked
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsAkamaiHTTPTransferMode where
    parser = takeLowerText >>= \case
        "chunked" -> pure AkamaiChunked
        "non_chunked" -> pure AkamaiNonChunked
        e -> fromTextError $ "Failure parsing HlsAkamaiHTTPTransferMode from value: '" <> e
           <> "'. Accepted values: chunked, non_chunked"

instance ToText HlsAkamaiHTTPTransferMode where
    toText = \case
        AkamaiChunked -> "CHUNKED"
        AkamaiNonChunked -> "NON_CHUNKED"

instance Hashable     HlsAkamaiHTTPTransferMode
instance NFData       HlsAkamaiHTTPTransferMode
instance ToByteString HlsAkamaiHTTPTransferMode
instance ToQuery      HlsAkamaiHTTPTransferMode
instance ToHeader     HlsAkamaiHTTPTransferMode

instance ToJSON HlsAkamaiHTTPTransferMode where
    toJSON = toJSONText

instance FromJSON HlsAkamaiHTTPTransferMode where
    parseJSON = parseJSONText "HlsAkamaiHTTPTransferMode"

-- | Placeholder documentation for HlsCaptionLanguageSetting
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

-- | Placeholder documentation for HlsClientCache
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

-- | Placeholder documentation for HlsCodecSpecification
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

-- | Placeholder documentation for HlsDirectoryStructure
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

-- | Placeholder documentation for HlsEncryptionType
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

-- | Placeholder documentation for HlsIvInManifest
data HlsIvInManifest
  = Exclude
  | Include
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsIvInManifest where
    parser = takeLowerText >>= \case
        "exclude" -> pure Exclude
        "include" -> pure Include
        e -> fromTextError $ "Failure parsing HlsIvInManifest from value: '" <> e
           <> "'. Accepted values: exclude, include"

instance ToText HlsIvInManifest where
    toText = \case
        Exclude -> "EXCLUDE"
        Include -> "INCLUDE"

instance Hashable     HlsIvInManifest
instance NFData       HlsIvInManifest
instance ToByteString HlsIvInManifest
instance ToQuery      HlsIvInManifest
instance ToHeader     HlsIvInManifest

instance ToJSON HlsIvInManifest where
    toJSON = toJSONText

instance FromJSON HlsIvInManifest where
    parseJSON = parseJSONText "HlsIvInManifest"

-- | Placeholder documentation for HlsIvSource
data HlsIvSource
  = Explicit
  | FollowsSegmentNumber
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsIvSource where
    parser = takeLowerText >>= \case
        "explicit" -> pure Explicit
        "follows_segment_number" -> pure FollowsSegmentNumber
        e -> fromTextError $ "Failure parsing HlsIvSource from value: '" <> e
           <> "'. Accepted values: explicit, follows_segment_number"

instance ToText HlsIvSource where
    toText = \case
        Explicit -> "EXPLICIT"
        FollowsSegmentNumber -> "FOLLOWS_SEGMENT_NUMBER"

instance Hashable     HlsIvSource
instance NFData       HlsIvSource
instance ToByteString HlsIvSource
instance ToQuery      HlsIvSource
instance ToHeader     HlsIvSource

instance ToJSON HlsIvSource where
    toJSON = toJSONText

instance FromJSON HlsIvSource where
    parseJSON = parseJSONText "HlsIvSource"

-- | Placeholder documentation for HlsManifestCompression
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

-- | Placeholder documentation for HlsManifestDurationFormat
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

-- | Placeholder documentation for HlsMediaStoreStorageClass
data HlsMediaStoreStorageClass =
  Temporal
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsMediaStoreStorageClass where
    parser = takeLowerText >>= \case
        "temporal" -> pure Temporal
        e -> fromTextError $ "Failure parsing HlsMediaStoreStorageClass from value: '" <> e
           <> "'. Accepted values: temporal"

instance ToText HlsMediaStoreStorageClass where
    toText = \case
        Temporal -> "TEMPORAL"

instance Hashable     HlsMediaStoreStorageClass
instance NFData       HlsMediaStoreStorageClass
instance ToByteString HlsMediaStoreStorageClass
instance ToQuery      HlsMediaStoreStorageClass
instance ToHeader     HlsMediaStoreStorageClass

instance ToJSON HlsMediaStoreStorageClass where
    toJSON = toJSONText

instance FromJSON HlsMediaStoreStorageClass where
    parseJSON = parseJSONText "HlsMediaStoreStorageClass"

-- | Placeholder documentation for HlsMode
data HlsMode
  = Live
  | Vod
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsMode where
    parser = takeLowerText >>= \case
        "live" -> pure Live
        "vod" -> pure Vod
        e -> fromTextError $ "Failure parsing HlsMode from value: '" <> e
           <> "'. Accepted values: live, vod"

instance ToText HlsMode where
    toText = \case
        Live -> "LIVE"
        Vod -> "VOD"

instance Hashable     HlsMode
instance NFData       HlsMode
instance ToByteString HlsMode
instance ToQuery      HlsMode
instance ToHeader     HlsMode

instance ToJSON HlsMode where
    toJSON = toJSONText

instance FromJSON HlsMode where
    parseJSON = parseJSONText "HlsMode"

-- | Placeholder documentation for HlsOutputSelection
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

-- | Placeholder documentation for HlsProgramDateTime
data HlsProgramDateTime
  = HPDTExclude
  | HPDTInclude
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsProgramDateTime where
    parser = takeLowerText >>= \case
        "exclude" -> pure HPDTExclude
        "include" -> pure HPDTInclude
        e -> fromTextError $ "Failure parsing HlsProgramDateTime from value: '" <> e
           <> "'. Accepted values: exclude, include"

instance ToText HlsProgramDateTime where
    toText = \case
        HPDTExclude -> "EXCLUDE"
        HPDTInclude -> "INCLUDE"

instance Hashable     HlsProgramDateTime
instance NFData       HlsProgramDateTime
instance ToByteString HlsProgramDateTime
instance ToQuery      HlsProgramDateTime
instance ToHeader     HlsProgramDateTime

instance ToJSON HlsProgramDateTime where
    toJSON = toJSONText

instance FromJSON HlsProgramDateTime where
    parseJSON = parseJSONText "HlsProgramDateTime"

-- | Placeholder documentation for HlsSegmentationMode
data HlsSegmentationMode
  = HSMUseInputSegmentation
  | HSMUseSegmentDuration
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsSegmentationMode where
    parser = takeLowerText >>= \case
        "use_input_segmentation" -> pure HSMUseInputSegmentation
        "use_segment_duration" -> pure HSMUseSegmentDuration
        e -> fromTextError $ "Failure parsing HlsSegmentationMode from value: '" <> e
           <> "'. Accepted values: use_input_segmentation, use_segment_duration"

instance ToText HlsSegmentationMode where
    toText = \case
        HSMUseInputSegmentation -> "USE_INPUT_SEGMENTATION"
        HSMUseSegmentDuration -> "USE_SEGMENT_DURATION"

instance Hashable     HlsSegmentationMode
instance NFData       HlsSegmentationMode
instance ToByteString HlsSegmentationMode
instance ToQuery      HlsSegmentationMode
instance ToHeader     HlsSegmentationMode

instance ToJSON HlsSegmentationMode where
    toJSON = toJSONText

instance FromJSON HlsSegmentationMode where
    parseJSON = parseJSONText "HlsSegmentationMode"

-- | Placeholder documentation for HlsStreamInfResolution
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

-- | Placeholder documentation for HlsTimedMetadataId3Frame
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

-- | Placeholder documentation for HlsTsFileMode
data HlsTsFileMode
  = SegmentedFiles
  | SingleFile
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsTsFileMode where
    parser = takeLowerText >>= \case
        "segmented_files" -> pure SegmentedFiles
        "single_file" -> pure SingleFile
        e -> fromTextError $ "Failure parsing HlsTsFileMode from value: '" <> e
           <> "'. Accepted values: segmented_files, single_file"

instance ToText HlsTsFileMode where
    toText = \case
        SegmentedFiles -> "SEGMENTED_FILES"
        SingleFile -> "SINGLE_FILE"

instance Hashable     HlsTsFileMode
instance NFData       HlsTsFileMode
instance ToByteString HlsTsFileMode
instance ToQuery      HlsTsFileMode
instance ToHeader     HlsTsFileMode

instance ToJSON HlsTsFileMode where
    toJSON = toJSONText

instance FromJSON HlsTsFileMode where
    parseJSON = parseJSONText "HlsTsFileMode"

-- | Placeholder documentation for HlsWebdavHttpTransferMode
data HlsWebdavHTTPTransferMode
  = WebdavChunked
  | WebdavNonChunked
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HlsWebdavHTTPTransferMode where
    parser = takeLowerText >>= \case
        "chunked" -> pure WebdavChunked
        "non_chunked" -> pure WebdavNonChunked
        e -> fromTextError $ "Failure parsing HlsWebdavHTTPTransferMode from value: '" <> e
           <> "'. Accepted values: chunked, non_chunked"

instance ToText HlsWebdavHTTPTransferMode where
    toText = \case
        WebdavChunked -> "CHUNKED"
        WebdavNonChunked -> "NON_CHUNKED"

instance Hashable     HlsWebdavHTTPTransferMode
instance NFData       HlsWebdavHTTPTransferMode
instance ToByteString HlsWebdavHTTPTransferMode
instance ToQuery      HlsWebdavHTTPTransferMode
instance ToHeader     HlsWebdavHTTPTransferMode

instance ToJSON HlsWebdavHTTPTransferMode where
    toJSON = toJSONText

instance FromJSON HlsWebdavHTTPTransferMode where
    parseJSON = parseJSONText "HlsWebdavHTTPTransferMode"

-- | codec in increasing order of complexity
data InputCodec
  = Avc
  | Hevc
  | MPEG2
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputCodec where
    parser = takeLowerText >>= \case
        "avc" -> pure Avc
        "hevc" -> pure Hevc
        "mpeg2" -> pure MPEG2
        e -> fromTextError $ "Failure parsing InputCodec from value: '" <> e
           <> "'. Accepted values: avc, hevc, mpeg2"

instance ToText InputCodec where
    toText = \case
        Avc -> "AVC"
        Hevc -> "HEVC"
        MPEG2 -> "MPEG2"

instance Hashable     InputCodec
instance NFData       InputCodec
instance ToByteString InputCodec
instance ToQuery      InputCodec
instance ToHeader     InputCodec

instance ToJSON InputCodec where
    toJSON = toJSONText

instance FromJSON InputCodec where
    parseJSON = parseJSONText "InputCodec"

-- | Placeholder documentation for InputDeblockFilter
data InputDeblockFilter
  = IDFDisabled
  | IDFEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputDeblockFilter where
    parser = takeLowerText >>= \case
        "disabled" -> pure IDFDisabled
        "enabled" -> pure IDFEnabled
        e -> fromTextError $ "Failure parsing InputDeblockFilter from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText InputDeblockFilter where
    toText = \case
        IDFDisabled -> "DISABLED"
        IDFEnabled -> "ENABLED"

instance Hashable     InputDeblockFilter
instance NFData       InputDeblockFilter
instance ToByteString InputDeblockFilter
instance ToQuery      InputDeblockFilter
instance ToHeader     InputDeblockFilter

instance ToJSON InputDeblockFilter where
    toJSON = toJSONText

instance FromJSON InputDeblockFilter where
    parseJSON = parseJSONText "InputDeblockFilter"

-- | Placeholder documentation for InputDenoiseFilter
data InputDenoiseFilter
  = IDisabled
  | IEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputDenoiseFilter where
    parser = takeLowerText >>= \case
        "disabled" -> pure IDisabled
        "enabled" -> pure IEnabled
        e -> fromTextError $ "Failure parsing InputDenoiseFilter from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText InputDenoiseFilter where
    toText = \case
        IDisabled -> "DISABLED"
        IEnabled -> "ENABLED"

instance Hashable     InputDenoiseFilter
instance NFData       InputDenoiseFilter
instance ToByteString InputDenoiseFilter
instance ToQuery      InputDenoiseFilter
instance ToHeader     InputDenoiseFilter

instance ToJSON InputDenoiseFilter where
    toJSON = toJSONText

instance FromJSON InputDenoiseFilter where
    parseJSON = parseJSONText "InputDenoiseFilter"

-- | Placeholder documentation for InputFilter
data InputFilter
  = IFAuto
  | IFDisabled
  | IFForced
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputFilter where
    parser = takeLowerText >>= \case
        "auto" -> pure IFAuto
        "disabled" -> pure IFDisabled
        "forced" -> pure IFForced
        e -> fromTextError $ "Failure parsing InputFilter from value: '" <> e
           <> "'. Accepted values: auto, disabled, forced"

instance ToText InputFilter where
    toText = \case
        IFAuto -> "AUTO"
        IFDisabled -> "DISABLED"
        IFForced -> "FORCED"

instance Hashable     InputFilter
instance NFData       InputFilter
instance ToByteString InputFilter
instance ToQuery      InputFilter
instance ToHeader     InputFilter

instance ToJSON InputFilter where
    toJSON = toJSONText

instance FromJSON InputFilter where
    parseJSON = parseJSONText "InputFilter"

-- | Placeholder documentation for InputLossActionForHlsOut
data InputLossActionForHlsOut
  = ILAFHOEmitOutput
  | ILAFHOPauseOutput
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputLossActionForHlsOut where
    parser = takeLowerText >>= \case
        "emit_output" -> pure ILAFHOEmitOutput
        "pause_output" -> pure ILAFHOPauseOutput
        e -> fromTextError $ "Failure parsing InputLossActionForHlsOut from value: '" <> e
           <> "'. Accepted values: emit_output, pause_output"

instance ToText InputLossActionForHlsOut where
    toText = \case
        ILAFHOEmitOutput -> "EMIT_OUTPUT"
        ILAFHOPauseOutput -> "PAUSE_OUTPUT"

instance Hashable     InputLossActionForHlsOut
instance NFData       InputLossActionForHlsOut
instance ToByteString InputLossActionForHlsOut
instance ToQuery      InputLossActionForHlsOut
instance ToHeader     InputLossActionForHlsOut

instance ToJSON InputLossActionForHlsOut where
    toJSON = toJSONText

instance FromJSON InputLossActionForHlsOut where
    parseJSON = parseJSONText "InputLossActionForHlsOut"

-- | Placeholder documentation for InputLossActionForMsSmoothOut
data InputLossActionForMsSmoothOut
  = EmitOutput
  | PauseOutput
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputLossActionForMsSmoothOut where
    parser = takeLowerText >>= \case
        "emit_output" -> pure EmitOutput
        "pause_output" -> pure PauseOutput
        e -> fromTextError $ "Failure parsing InputLossActionForMsSmoothOut from value: '" <> e
           <> "'. Accepted values: emit_output, pause_output"

instance ToText InputLossActionForMsSmoothOut where
    toText = \case
        EmitOutput -> "EMIT_OUTPUT"
        PauseOutput -> "PAUSE_OUTPUT"

instance Hashable     InputLossActionForMsSmoothOut
instance NFData       InputLossActionForMsSmoothOut
instance ToByteString InputLossActionForMsSmoothOut
instance ToQuery      InputLossActionForMsSmoothOut
instance ToHeader     InputLossActionForMsSmoothOut

instance ToJSON InputLossActionForMsSmoothOut where
    toJSON = toJSONText

instance FromJSON InputLossActionForMsSmoothOut where
    parseJSON = parseJSONText "InputLossActionForMsSmoothOut"

-- | Placeholder documentation for InputLossActionForUdpOut
data InputLossActionForUdpOut
  = DropProgram
  | DropTs
  | EmitProgram
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputLossActionForUdpOut where
    parser = takeLowerText >>= \case
        "drop_program" -> pure DropProgram
        "drop_ts" -> pure DropTs
        "emit_program" -> pure EmitProgram
        e -> fromTextError $ "Failure parsing InputLossActionForUdpOut from value: '" <> e
           <> "'. Accepted values: drop_program, drop_ts, emit_program"

instance ToText InputLossActionForUdpOut where
    toText = \case
        DropProgram -> "DROP_PROGRAM"
        DropTs -> "DROP_TS"
        EmitProgram -> "EMIT_PROGRAM"

instance Hashable     InputLossActionForUdpOut
instance NFData       InputLossActionForUdpOut
instance ToByteString InputLossActionForUdpOut
instance ToQuery      InputLossActionForUdpOut
instance ToHeader     InputLossActionForUdpOut

instance ToJSON InputLossActionForUdpOut where
    toJSON = toJSONText

instance FromJSON InputLossActionForUdpOut where
    parseJSON = parseJSONText "InputLossActionForUdpOut"

-- | Placeholder documentation for InputLossImageType
data InputLossImageType
  = Color
  | Slate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputLossImageType where
    parser = takeLowerText >>= \case
        "color" -> pure Color
        "slate" -> pure Slate
        e -> fromTextError $ "Failure parsing InputLossImageType from value: '" <> e
           <> "'. Accepted values: color, slate"

instance ToText InputLossImageType where
    toText = \case
        Color -> "COLOR"
        Slate -> "SLATE"

instance Hashable     InputLossImageType
instance NFData       InputLossImageType
instance ToByteString InputLossImageType
instance ToQuery      InputLossImageType
instance ToHeader     InputLossImageType

instance ToJSON InputLossImageType where
    toJSON = toJSONText

instance FromJSON InputLossImageType where
    parseJSON = parseJSONText "InputLossImageType"

-- | Maximum input bitrate in megabits per second. Bitrates up to 50 Mbps are supported currently.
data InputMaximumBitrate
  = Max10Mbps
  | Max20Mbps
  | Max50Mbps
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputMaximumBitrate where
    parser = takeLowerText >>= \case
        "max_10_mbps" -> pure Max10Mbps
        "max_20_mbps" -> pure Max20Mbps
        "max_50_mbps" -> pure Max50Mbps
        e -> fromTextError $ "Failure parsing InputMaximumBitrate from value: '" <> e
           <> "'. Accepted values: max_10_mbps, max_20_mbps, max_50_mbps"

instance ToText InputMaximumBitrate where
    toText = \case
        Max10Mbps -> "MAX_10_MBPS"
        Max20Mbps -> "MAX_20_MBPS"
        Max50Mbps -> "MAX_50_MBPS"

instance Hashable     InputMaximumBitrate
instance NFData       InputMaximumBitrate
instance ToByteString InputMaximumBitrate
instance ToQuery      InputMaximumBitrate
instance ToHeader     InputMaximumBitrate

instance ToJSON InputMaximumBitrate where
    toJSON = toJSONText

instance FromJSON InputMaximumBitrate where
    parseJSON = parseJSONText "InputMaximumBitrate"

-- | Input resolution based on lines of vertical resolution in the input; SD is less than 720 lines, HD is 720 to 1080 lines, UHD is greater than 1080 lines
data InputResolution
  = HD
  | SD
  | Uhd
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputResolution where
    parser = takeLowerText >>= \case
        "hd" -> pure HD
        "sd" -> pure SD
        "uhd" -> pure Uhd
        e -> fromTextError $ "Failure parsing InputResolution from value: '" <> e
           <> "'. Accepted values: hd, sd, uhd"

instance ToText InputResolution where
    toText = \case
        HD -> "HD"
        SD -> "SD"
        Uhd -> "UHD"

instance Hashable     InputResolution
instance NFData       InputResolution
instance ToByteString InputResolution
instance ToQuery      InputResolution
instance ToHeader     InputResolution

instance ToJSON InputResolution where
    toJSON = toJSONText

instance FromJSON InputResolution where
    parseJSON = parseJSONText "InputResolution"

-- | Placeholder documentation for InputSecurityGroupState
data InputSecurityGroupState
  = ISGSDeleted
  | ISGSIdle
  | ISGSInUse
  | ISGSUpdating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputSecurityGroupState where
    parser = takeLowerText >>= \case
        "deleted" -> pure ISGSDeleted
        "idle" -> pure ISGSIdle
        "in_use" -> pure ISGSInUse
        "updating" -> pure ISGSUpdating
        e -> fromTextError $ "Failure parsing InputSecurityGroupState from value: '" <> e
           <> "'. Accepted values: deleted, idle, in_use, updating"

instance ToText InputSecurityGroupState where
    toText = \case
        ISGSDeleted -> "DELETED"
        ISGSIdle -> "IDLE"
        ISGSInUse -> "IN_USE"
        ISGSUpdating -> "UPDATING"

instance Hashable     InputSecurityGroupState
instance NFData       InputSecurityGroupState
instance ToByteString InputSecurityGroupState
instance ToQuery      InputSecurityGroupState
instance ToHeader     InputSecurityGroupState

instance FromJSON InputSecurityGroupState where
    parseJSON = parseJSONText "InputSecurityGroupState"

-- | Placeholder documentation for InputSourceEndBehavior
data InputSourceEndBehavior
  = Continue
  | Loop
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputSourceEndBehavior where
    parser = takeLowerText >>= \case
        "continue" -> pure Continue
        "loop" -> pure Loop
        e -> fromTextError $ "Failure parsing InputSourceEndBehavior from value: '" <> e
           <> "'. Accepted values: continue, loop"

instance ToText InputSourceEndBehavior where
    toText = \case
        Continue -> "CONTINUE"
        Loop -> "LOOP"

instance Hashable     InputSourceEndBehavior
instance NFData       InputSourceEndBehavior
instance ToByteString InputSourceEndBehavior
instance ToQuery      InputSourceEndBehavior
instance ToHeader     InputSourceEndBehavior

instance ToJSON InputSourceEndBehavior where
    toJSON = toJSONText

instance FromJSON InputSourceEndBehavior where
    parseJSON = parseJSONText "InputSourceEndBehavior"

-- | Placeholder documentation for InputState
data InputState
  = ISAttached
  | ISCreating
  | ISDeleted
  | ISDeleting
  | ISDetached
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputState where
    parser = takeLowerText >>= \case
        "attached" -> pure ISAttached
        "creating" -> pure ISCreating
        "deleted" -> pure ISDeleted
        "deleting" -> pure ISDeleting
        "detached" -> pure ISDetached
        e -> fromTextError $ "Failure parsing InputState from value: '" <> e
           <> "'. Accepted values: attached, creating, deleted, deleting, detached"

instance ToText InputState where
    toText = \case
        ISAttached -> "ATTACHED"
        ISCreating -> "CREATING"
        ISDeleted -> "DELETED"
        ISDeleting -> "DELETING"
        ISDetached -> "DETACHED"

instance Hashable     InputState
instance NFData       InputState
instance ToByteString InputState
instance ToQuery      InputState
instance ToHeader     InputState

instance FromJSON InputState where
    parseJSON = parseJSONText "InputState"

-- | Placeholder documentation for InputType
data InputType
  = RtmpPull
  | RtmpPush
  | RtpPush
  | URLPull
  | UdpPush
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputType where
    parser = takeLowerText >>= \case
        "rtmp_pull" -> pure RtmpPull
        "rtmp_push" -> pure RtmpPush
        "rtp_push" -> pure RtpPush
        "url_pull" -> pure URLPull
        "udp_push" -> pure UdpPush
        e -> fromTextError $ "Failure parsing InputType from value: '" <> e
           <> "'. Accepted values: rtmp_pull, rtmp_push, rtp_push, url_pull, udp_push"

instance ToText InputType where
    toText = \case
        RtmpPull -> "RTMP_PULL"
        RtmpPush -> "RTMP_PUSH"
        RtpPush -> "RTP_PUSH"
        URLPull -> "URL_PULL"
        UdpPush -> "UDP_PUSH"

instance Hashable     InputType
instance NFData       InputType
instance ToByteString InputType
instance ToQuery      InputType
instance ToHeader     InputType

instance ToJSON InputType where
    toJSON = toJSONText

instance FromJSON InputType where
    parseJSON = parseJSONText "InputType"

-- | Placeholder documentation for M2tsAbsentInputAudioBehavior
data M2tsAbsentInputAudioBehavior
  = Drop
  | EncodeSilence
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsAbsentInputAudioBehavior where
    parser = takeLowerText >>= \case
        "drop" -> pure Drop
        "encode_silence" -> pure EncodeSilence
        e -> fromTextError $ "Failure parsing M2tsAbsentInputAudioBehavior from value: '" <> e
           <> "'. Accepted values: drop, encode_silence"

instance ToText M2tsAbsentInputAudioBehavior where
    toText = \case
        Drop -> "DROP"
        EncodeSilence -> "ENCODE_SILENCE"

instance Hashable     M2tsAbsentInputAudioBehavior
instance NFData       M2tsAbsentInputAudioBehavior
instance ToByteString M2tsAbsentInputAudioBehavior
instance ToQuery      M2tsAbsentInputAudioBehavior
instance ToHeader     M2tsAbsentInputAudioBehavior

instance ToJSON M2tsAbsentInputAudioBehavior where
    toJSON = toJSONText

instance FromJSON M2tsAbsentInputAudioBehavior where
    parseJSON = parseJSONText "M2tsAbsentInputAudioBehavior"

-- | Placeholder documentation for M2tsArib
data M2tsArib
  = MADisabled
  | MAEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsArib where
    parser = takeLowerText >>= \case
        "disabled" -> pure MADisabled
        "enabled" -> pure MAEnabled
        e -> fromTextError $ "Failure parsing M2tsArib from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText M2tsArib where
    toText = \case
        MADisabled -> "DISABLED"
        MAEnabled -> "ENABLED"

instance Hashable     M2tsArib
instance NFData       M2tsArib
instance ToByteString M2tsArib
instance ToQuery      M2tsArib
instance ToHeader     M2tsArib

instance ToJSON M2tsArib where
    toJSON = toJSONText

instance FromJSON M2tsArib where
    parseJSON = parseJSONText "M2tsArib"

-- | Placeholder documentation for M2tsAribCaptionsPidControl
data M2tsAribCaptionsPidControl
  = MACPCAuto
  | MACPCUseConfigured
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsAribCaptionsPidControl where
    parser = takeLowerText >>= \case
        "auto" -> pure MACPCAuto
        "use_configured" -> pure MACPCUseConfigured
        e -> fromTextError $ "Failure parsing M2tsAribCaptionsPidControl from value: '" <> e
           <> "'. Accepted values: auto, use_configured"

instance ToText M2tsAribCaptionsPidControl where
    toText = \case
        MACPCAuto -> "AUTO"
        MACPCUseConfigured -> "USE_CONFIGURED"

instance Hashable     M2tsAribCaptionsPidControl
instance NFData       M2tsAribCaptionsPidControl
instance ToByteString M2tsAribCaptionsPidControl
instance ToQuery      M2tsAribCaptionsPidControl
instance ToHeader     M2tsAribCaptionsPidControl

instance ToJSON M2tsAribCaptionsPidControl where
    toJSON = toJSONText

instance FromJSON M2tsAribCaptionsPidControl where
    parseJSON = parseJSONText "M2tsAribCaptionsPidControl"

-- | Placeholder documentation for M2tsAudioBufferModel
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

-- | Placeholder documentation for M2tsAudioInterval
data M2tsAudioInterval
  = VideoAndFixedIntervals
  | VideoInterval
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsAudioInterval where
    parser = takeLowerText >>= \case
        "video_and_fixed_intervals" -> pure VideoAndFixedIntervals
        "video_interval" -> pure VideoInterval
        e -> fromTextError $ "Failure parsing M2tsAudioInterval from value: '" <> e
           <> "'. Accepted values: video_and_fixed_intervals, video_interval"

instance ToText M2tsAudioInterval where
    toText = \case
        VideoAndFixedIntervals -> "VIDEO_AND_FIXED_INTERVALS"
        VideoInterval -> "VIDEO_INTERVAL"

instance Hashable     M2tsAudioInterval
instance NFData       M2tsAudioInterval
instance ToByteString M2tsAudioInterval
instance ToQuery      M2tsAudioInterval
instance ToHeader     M2tsAudioInterval

instance ToJSON M2tsAudioInterval where
    toJSON = toJSONText

instance FromJSON M2tsAudioInterval where
    parseJSON = parseJSONText "M2tsAudioInterval"

-- | Placeholder documentation for M2tsAudioStreamType
data M2tsAudioStreamType
  = MASTAtsc
  | MASTDvb
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsAudioStreamType where
    parser = takeLowerText >>= \case
        "atsc" -> pure MASTAtsc
        "dvb" -> pure MASTDvb
        e -> fromTextError $ "Failure parsing M2tsAudioStreamType from value: '" <> e
           <> "'. Accepted values: atsc, dvb"

instance ToText M2tsAudioStreamType where
    toText = \case
        MASTAtsc -> "ATSC"
        MASTDvb -> "DVB"

instance Hashable     M2tsAudioStreamType
instance NFData       M2tsAudioStreamType
instance ToByteString M2tsAudioStreamType
instance ToQuery      M2tsAudioStreamType
instance ToHeader     M2tsAudioStreamType

instance ToJSON M2tsAudioStreamType where
    toJSON = toJSONText

instance FromJSON M2tsAudioStreamType where
    parseJSON = parseJSONText "M2tsAudioStreamType"

-- | Placeholder documentation for M2tsBufferModel
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

-- | Placeholder documentation for M2tsCcDescriptor
data M2tsCCDescriptor
  = MCCDDisabled
  | MCCDEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsCCDescriptor where
    parser = takeLowerText >>= \case
        "disabled" -> pure MCCDDisabled
        "enabled" -> pure MCCDEnabled
        e -> fromTextError $ "Failure parsing M2tsCCDescriptor from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText M2tsCCDescriptor where
    toText = \case
        MCCDDisabled -> "DISABLED"
        MCCDEnabled -> "ENABLED"

instance Hashable     M2tsCCDescriptor
instance NFData       M2tsCCDescriptor
instance ToByteString M2tsCCDescriptor
instance ToQuery      M2tsCCDescriptor
instance ToHeader     M2tsCCDescriptor

instance ToJSON M2tsCCDescriptor where
    toJSON = toJSONText

instance FromJSON M2tsCCDescriptor where
    parseJSON = parseJSONText "M2tsCCDescriptor"

-- | Placeholder documentation for M2tsEbifControl
data M2tsEbifControl
  = MECNone
  | MECPassthrough
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsEbifControl where
    parser = takeLowerText >>= \case
        "none" -> pure MECNone
        "passthrough" -> pure MECPassthrough
        e -> fromTextError $ "Failure parsing M2tsEbifControl from value: '" <> e
           <> "'. Accepted values: none, passthrough"

instance ToText M2tsEbifControl where
    toText = \case
        MECNone -> "NONE"
        MECPassthrough -> "PASSTHROUGH"

instance Hashable     M2tsEbifControl
instance NFData       M2tsEbifControl
instance ToByteString M2tsEbifControl
instance ToQuery      M2tsEbifControl
instance ToHeader     M2tsEbifControl

instance ToJSON M2tsEbifControl where
    toJSON = toJSONText

instance FromJSON M2tsEbifControl where
    parseJSON = parseJSONText "M2tsEbifControl"

-- | Placeholder documentation for M2tsEbpPlacement
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

-- | Placeholder documentation for M2tsEsRateInPes
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

-- | Placeholder documentation for M2tsKlv
data M2tsKlv
  = MKNone
  | MKPassthrough
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsKlv where
    parser = takeLowerText >>= \case
        "none" -> pure MKNone
        "passthrough" -> pure MKPassthrough
        e -> fromTextError $ "Failure parsing M2tsKlv from value: '" <> e
           <> "'. Accepted values: none, passthrough"

instance ToText M2tsKlv where
    toText = \case
        MKNone -> "NONE"
        MKPassthrough -> "PASSTHROUGH"

instance Hashable     M2tsKlv
instance NFData       M2tsKlv
instance ToByteString M2tsKlv
instance ToQuery      M2tsKlv
instance ToHeader     M2tsKlv

instance ToJSON M2tsKlv where
    toJSON = toJSONText

instance FromJSON M2tsKlv where
    parseJSON = parseJSONText "M2tsKlv"

-- | Placeholder documentation for M2tsPcrControl
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

-- | Placeholder documentation for M2tsRateMode
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

-- | Placeholder documentation for M2tsScte35Control
data M2tsScte35Control
  = MSCNone
  | MSCPassthrough
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsScte35Control where
    parser = takeLowerText >>= \case
        "none" -> pure MSCNone
        "passthrough" -> pure MSCPassthrough
        e -> fromTextError $ "Failure parsing M2tsScte35Control from value: '" <> e
           <> "'. Accepted values: none, passthrough"

instance ToText M2tsScte35Control where
    toText = \case
        MSCNone -> "NONE"
        MSCPassthrough -> "PASSTHROUGH"

instance Hashable     M2tsScte35Control
instance NFData       M2tsScte35Control
instance ToByteString M2tsScte35Control
instance ToQuery      M2tsScte35Control
instance ToHeader     M2tsScte35Control

instance ToJSON M2tsScte35Control where
    toJSON = toJSONText

instance FromJSON M2tsScte35Control where
    parseJSON = parseJSONText "M2tsScte35Control"

-- | Placeholder documentation for M2tsSegmentationMarkers
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

-- | Placeholder documentation for M2tsSegmentationStyle
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

-- | Placeholder documentation for M2tsTimedMetadataBehavior
data M2tsTimedMetadataBehavior
  = MTMBNoPassthrough
  | MTMBPassthrough
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M2tsTimedMetadataBehavior where
    parser = takeLowerText >>= \case
        "no_passthrough" -> pure MTMBNoPassthrough
        "passthrough" -> pure MTMBPassthrough
        e -> fromTextError $ "Failure parsing M2tsTimedMetadataBehavior from value: '" <> e
           <> "'. Accepted values: no_passthrough, passthrough"

instance ToText M2tsTimedMetadataBehavior where
    toText = \case
        MTMBNoPassthrough -> "NO_PASSTHROUGH"
        MTMBPassthrough -> "PASSTHROUGH"

instance Hashable     M2tsTimedMetadataBehavior
instance NFData       M2tsTimedMetadataBehavior
instance ToByteString M2tsTimedMetadataBehavior
instance ToQuery      M2tsTimedMetadataBehavior
instance ToHeader     M2tsTimedMetadataBehavior

instance ToJSON M2tsTimedMetadataBehavior where
    toJSON = toJSONText

instance FromJSON M2tsTimedMetadataBehavior where
    parseJSON = parseJSONText "M2tsTimedMetadataBehavior"

-- | Placeholder documentation for M3u8PcrControl
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

-- | Placeholder documentation for M3u8Scte35Behavior
data M3u8Scte35Behavior
  = MSBNoPassthrough
  | MSBPassthrough
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M3u8Scte35Behavior where
    parser = takeLowerText >>= \case
        "no_passthrough" -> pure MSBNoPassthrough
        "passthrough" -> pure MSBPassthrough
        e -> fromTextError $ "Failure parsing M3u8Scte35Behavior from value: '" <> e
           <> "'. Accepted values: no_passthrough, passthrough"

instance ToText M3u8Scte35Behavior where
    toText = \case
        MSBNoPassthrough -> "NO_PASSTHROUGH"
        MSBPassthrough -> "PASSTHROUGH"

instance Hashable     M3u8Scte35Behavior
instance NFData       M3u8Scte35Behavior
instance ToByteString M3u8Scte35Behavior
instance ToQuery      M3u8Scte35Behavior
instance ToHeader     M3u8Scte35Behavior

instance ToJSON M3u8Scte35Behavior where
    toJSON = toJSONText

instance FromJSON M3u8Scte35Behavior where
    parseJSON = parseJSONText "M3u8Scte35Behavior"

-- | Placeholder documentation for M3u8TimedMetadataBehavior
data M3u8TimedMetadataBehavior
  = MNoPassthrough
  | MPassthrough
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText M3u8TimedMetadataBehavior where
    parser = takeLowerText >>= \case
        "no_passthrough" -> pure MNoPassthrough
        "passthrough" -> pure MPassthrough
        e -> fromTextError $ "Failure parsing M3u8TimedMetadataBehavior from value: '" <> e
           <> "'. Accepted values: no_passthrough, passthrough"

instance ToText M3u8TimedMetadataBehavior where
    toText = \case
        MNoPassthrough -> "NO_PASSTHROUGH"
        MPassthrough -> "PASSTHROUGH"

instance Hashable     M3u8TimedMetadataBehavior
instance NFData       M3u8TimedMetadataBehavior
instance ToByteString M3u8TimedMetadataBehavior
instance ToQuery      M3u8TimedMetadataBehavior
instance ToHeader     M3u8TimedMetadataBehavior

instance ToJSON M3u8TimedMetadataBehavior where
    toJSON = toJSONText

instance FromJSON M3u8TimedMetadataBehavior where
    parseJSON = parseJSONText "M3u8TimedMetadataBehavior"

-- | Placeholder documentation for Mp2CodingMode
data Mp2CodingMode
  = MCMCodingMode10
  | MCMCodingMode20
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Mp2CodingMode where
    parser = takeLowerText >>= \case
        "coding_mode_1_0" -> pure MCMCodingMode10
        "coding_mode_2_0" -> pure MCMCodingMode20
        e -> fromTextError $ "Failure parsing Mp2CodingMode from value: '" <> e
           <> "'. Accepted values: coding_mode_1_0, coding_mode_2_0"

instance ToText Mp2CodingMode where
    toText = \case
        MCMCodingMode10 -> "CODING_MODE_1_0"
        MCMCodingMode20 -> "CODING_MODE_2_0"

instance Hashable     Mp2CodingMode
instance NFData       Mp2CodingMode
instance ToByteString Mp2CodingMode
instance ToQuery      Mp2CodingMode
instance ToHeader     Mp2CodingMode

instance ToJSON Mp2CodingMode where
    toJSON = toJSONText

instance FromJSON Mp2CodingMode where
    parseJSON = parseJSONText "Mp2CodingMode"

-- | Placeholder documentation for NetworkInputServerValidation
data NetworkInputServerValidation
  = CheckCryptographyAndValidateName
  | CheckCryptographyOnly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkInputServerValidation where
    parser = takeLowerText >>= \case
        "check_cryptography_and_validate_name" -> pure CheckCryptographyAndValidateName
        "check_cryptography_only" -> pure CheckCryptographyOnly
        e -> fromTextError $ "Failure parsing NetworkInputServerValidation from value: '" <> e
           <> "'. Accepted values: check_cryptography_and_validate_name, check_cryptography_only"

instance ToText NetworkInputServerValidation where
    toText = \case
        CheckCryptographyAndValidateName -> "CHECK_CRYPTOGRAPHY_AND_VALIDATE_NAME"
        CheckCryptographyOnly -> "CHECK_CRYPTOGRAPHY_ONLY"

instance Hashable     NetworkInputServerValidation
instance NFData       NetworkInputServerValidation
instance ToByteString NetworkInputServerValidation
instance ToQuery      NetworkInputServerValidation
instance ToHeader     NetworkInputServerValidation

instance ToJSON NetworkInputServerValidation where
    toJSON = toJSONText

instance FromJSON NetworkInputServerValidation where
    parseJSON = parseJSONText "NetworkInputServerValidation"

-- | Placeholder documentation for RtmpCacheFullBehavior
data RtmpCacheFullBehavior
  = DisconnectImmediately
  | WaitForServer
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RtmpCacheFullBehavior where
    parser = takeLowerText >>= \case
        "disconnect_immediately" -> pure DisconnectImmediately
        "wait_for_server" -> pure WaitForServer
        e -> fromTextError $ "Failure parsing RtmpCacheFullBehavior from value: '" <> e
           <> "'. Accepted values: disconnect_immediately, wait_for_server"

instance ToText RtmpCacheFullBehavior where
    toText = \case
        DisconnectImmediately -> "DISCONNECT_IMMEDIATELY"
        WaitForServer -> "WAIT_FOR_SERVER"

instance Hashable     RtmpCacheFullBehavior
instance NFData       RtmpCacheFullBehavior
instance ToByteString RtmpCacheFullBehavior
instance ToQuery      RtmpCacheFullBehavior
instance ToHeader     RtmpCacheFullBehavior

instance ToJSON RtmpCacheFullBehavior where
    toJSON = toJSONText

instance FromJSON RtmpCacheFullBehavior where
    parseJSON = parseJSONText "RtmpCacheFullBehavior"

-- | Placeholder documentation for RtmpCaptionData
data RtmpCaptionData
  = All
  | FIELD1608
  | FIELD1AndFIELD2608
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RtmpCaptionData where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "field1_608" -> pure FIELD1608
        "field1_and_field2_608" -> pure FIELD1AndFIELD2608
        e -> fromTextError $ "Failure parsing RtmpCaptionData from value: '" <> e
           <> "'. Accepted values: all, field1_608, field1_and_field2_608"

instance ToText RtmpCaptionData where
    toText = \case
        All -> "ALL"
        FIELD1608 -> "FIELD1_608"
        FIELD1AndFIELD2608 -> "FIELD1_AND_FIELD2_608"

instance Hashable     RtmpCaptionData
instance NFData       RtmpCaptionData
instance ToByteString RtmpCaptionData
instance ToQuery      RtmpCaptionData
instance ToHeader     RtmpCaptionData

instance ToJSON RtmpCaptionData where
    toJSON = toJSONText

instance FromJSON RtmpCaptionData where
    parseJSON = parseJSONText "RtmpCaptionData"

-- | Placeholder documentation for RtmpOutputCertificateMode
data RtmpOutputCertificateMode
  = ROCMSelfSigned
  | ROCMVerifyAuthenticity
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RtmpOutputCertificateMode where
    parser = takeLowerText >>= \case
        "self_signed" -> pure ROCMSelfSigned
        "verify_authenticity" -> pure ROCMVerifyAuthenticity
        e -> fromTextError $ "Failure parsing RtmpOutputCertificateMode from value: '" <> e
           <> "'. Accepted values: self_signed, verify_authenticity"

instance ToText RtmpOutputCertificateMode where
    toText = \case
        ROCMSelfSigned -> "SELF_SIGNED"
        ROCMVerifyAuthenticity -> "VERIFY_AUTHENTICITY"

instance Hashable     RtmpOutputCertificateMode
instance NFData       RtmpOutputCertificateMode
instance ToByteString RtmpOutputCertificateMode
instance ToQuery      RtmpOutputCertificateMode
instance ToHeader     RtmpOutputCertificateMode

instance ToJSON RtmpOutputCertificateMode where
    toJSON = toJSONText

instance FromJSON RtmpOutputCertificateMode where
    parseJSON = parseJSONText "RtmpOutputCertificateMode"

-- | Placeholder documentation for Scte20Convert608To708
data Scte20Convert608To708
  = SCTDisabled
  | SCTUpconvert
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Scte20Convert608To708 where
    parser = takeLowerText >>= \case
        "disabled" -> pure SCTDisabled
        "upconvert" -> pure SCTUpconvert
        e -> fromTextError $ "Failure parsing Scte20Convert608To708 from value: '" <> e
           <> "'. Accepted values: disabled, upconvert"

instance ToText Scte20Convert608To708 where
    toText = \case
        SCTDisabled -> "DISABLED"
        SCTUpconvert -> "UPCONVERT"

instance Hashable     Scte20Convert608To708
instance NFData       Scte20Convert608To708
instance ToByteString Scte20Convert608To708
instance ToQuery      Scte20Convert608To708
instance ToHeader     Scte20Convert608To708

instance ToJSON Scte20Convert608To708 where
    toJSON = toJSONText

instance FromJSON Scte20Convert608To708 where
    parseJSON = parseJSONText "Scte20Convert608To708"

-- | Placeholder documentation for Scte35AposNoRegionalBlackoutBehavior
data Scte35AposNoRegionalBlackoutBehavior
  = SANRBBFollow
  | SANRBBIgnore
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Scte35AposNoRegionalBlackoutBehavior where
    parser = takeLowerText >>= \case
        "follow" -> pure SANRBBFollow
        "ignore" -> pure SANRBBIgnore
        e -> fromTextError $ "Failure parsing Scte35AposNoRegionalBlackoutBehavior from value: '" <> e
           <> "'. Accepted values: follow, ignore"

instance ToText Scte35AposNoRegionalBlackoutBehavior where
    toText = \case
        SANRBBFollow -> "FOLLOW"
        SANRBBIgnore -> "IGNORE"

instance Hashable     Scte35AposNoRegionalBlackoutBehavior
instance NFData       Scte35AposNoRegionalBlackoutBehavior
instance ToByteString Scte35AposNoRegionalBlackoutBehavior
instance ToQuery      Scte35AposNoRegionalBlackoutBehavior
instance ToHeader     Scte35AposNoRegionalBlackoutBehavior

instance ToJSON Scte35AposNoRegionalBlackoutBehavior where
    toJSON = toJSONText

instance FromJSON Scte35AposNoRegionalBlackoutBehavior where
    parseJSON = parseJSONText "Scte35AposNoRegionalBlackoutBehavior"

-- | Placeholder documentation for Scte35AposWebDeliveryAllowedBehavior
data Scte35AposWebDeliveryAllowedBehavior
  = SAWDABFollow
  | SAWDABIgnore
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Scte35AposWebDeliveryAllowedBehavior where
    parser = takeLowerText >>= \case
        "follow" -> pure SAWDABFollow
        "ignore" -> pure SAWDABIgnore
        e -> fromTextError $ "Failure parsing Scte35AposWebDeliveryAllowedBehavior from value: '" <> e
           <> "'. Accepted values: follow, ignore"

instance ToText Scte35AposWebDeliveryAllowedBehavior where
    toText = \case
        SAWDABFollow -> "FOLLOW"
        SAWDABIgnore -> "IGNORE"

instance Hashable     Scte35AposWebDeliveryAllowedBehavior
instance NFData       Scte35AposWebDeliveryAllowedBehavior
instance ToByteString Scte35AposWebDeliveryAllowedBehavior
instance ToQuery      Scte35AposWebDeliveryAllowedBehavior
instance ToHeader     Scte35AposWebDeliveryAllowedBehavior

instance ToJSON Scte35AposWebDeliveryAllowedBehavior where
    toJSON = toJSONText

instance FromJSON Scte35AposWebDeliveryAllowedBehavior where
    parseJSON = parseJSONText "Scte35AposWebDeliveryAllowedBehavior"

-- | Placeholder documentation for Scte35SpliceInsertNoRegionalBlackoutBehavior
data Scte35SpliceInsertNoRegionalBlackoutBehavior
  = SSINRBBFollow
  | SSINRBBIgnore
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Scte35SpliceInsertNoRegionalBlackoutBehavior where
    parser = takeLowerText >>= \case
        "follow" -> pure SSINRBBFollow
        "ignore" -> pure SSINRBBIgnore
        e -> fromTextError $ "Failure parsing Scte35SpliceInsertNoRegionalBlackoutBehavior from value: '" <> e
           <> "'. Accepted values: follow, ignore"

instance ToText Scte35SpliceInsertNoRegionalBlackoutBehavior where
    toText = \case
        SSINRBBFollow -> "FOLLOW"
        SSINRBBIgnore -> "IGNORE"

instance Hashable     Scte35SpliceInsertNoRegionalBlackoutBehavior
instance NFData       Scte35SpliceInsertNoRegionalBlackoutBehavior
instance ToByteString Scte35SpliceInsertNoRegionalBlackoutBehavior
instance ToQuery      Scte35SpliceInsertNoRegionalBlackoutBehavior
instance ToHeader     Scte35SpliceInsertNoRegionalBlackoutBehavior

instance ToJSON Scte35SpliceInsertNoRegionalBlackoutBehavior where
    toJSON = toJSONText

instance FromJSON Scte35SpliceInsertNoRegionalBlackoutBehavior where
    parseJSON = parseJSONText "Scte35SpliceInsertNoRegionalBlackoutBehavior"

-- | Placeholder documentation for Scte35SpliceInsertWebDeliveryAllowedBehavior
data Scte35SpliceInsertWebDeliveryAllowedBehavior
  = SSIWDABFollow
  | SSIWDABIgnore
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Scte35SpliceInsertWebDeliveryAllowedBehavior where
    parser = takeLowerText >>= \case
        "follow" -> pure SSIWDABFollow
        "ignore" -> pure SSIWDABIgnore
        e -> fromTextError $ "Failure parsing Scte35SpliceInsertWebDeliveryAllowedBehavior from value: '" <> e
           <> "'. Accepted values: follow, ignore"

instance ToText Scte35SpliceInsertWebDeliveryAllowedBehavior where
    toText = \case
        SSIWDABFollow -> "FOLLOW"
        SSIWDABIgnore -> "IGNORE"

instance Hashable     Scte35SpliceInsertWebDeliveryAllowedBehavior
instance NFData       Scte35SpliceInsertWebDeliveryAllowedBehavior
instance ToByteString Scte35SpliceInsertWebDeliveryAllowedBehavior
instance ToQuery      Scte35SpliceInsertWebDeliveryAllowedBehavior
instance ToHeader     Scte35SpliceInsertWebDeliveryAllowedBehavior

instance ToJSON Scte35SpliceInsertWebDeliveryAllowedBehavior where
    toJSON = toJSONText

instance FromJSON Scte35SpliceInsertWebDeliveryAllowedBehavior where
    parseJSON = parseJSONText "Scte35SpliceInsertWebDeliveryAllowedBehavior"

-- | Placeholder documentation for SmoothGroupAudioOnlyTimecodeControl
data SmoothGroupAudioOnlyTimecodeControl
  = SGAOTCPassthrough
  | SGAOTCUseConfiguredClock
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SmoothGroupAudioOnlyTimecodeControl where
    parser = takeLowerText >>= \case
        "passthrough" -> pure SGAOTCPassthrough
        "use_configured_clock" -> pure SGAOTCUseConfiguredClock
        e -> fromTextError $ "Failure parsing SmoothGroupAudioOnlyTimecodeControl from value: '" <> e
           <> "'. Accepted values: passthrough, use_configured_clock"

instance ToText SmoothGroupAudioOnlyTimecodeControl where
    toText = \case
        SGAOTCPassthrough -> "PASSTHROUGH"
        SGAOTCUseConfiguredClock -> "USE_CONFIGURED_CLOCK"

instance Hashable     SmoothGroupAudioOnlyTimecodeControl
instance NFData       SmoothGroupAudioOnlyTimecodeControl
instance ToByteString SmoothGroupAudioOnlyTimecodeControl
instance ToQuery      SmoothGroupAudioOnlyTimecodeControl
instance ToHeader     SmoothGroupAudioOnlyTimecodeControl

instance ToJSON SmoothGroupAudioOnlyTimecodeControl where
    toJSON = toJSONText

instance FromJSON SmoothGroupAudioOnlyTimecodeControl where
    parseJSON = parseJSONText "SmoothGroupAudioOnlyTimecodeControl"

-- | Placeholder documentation for SmoothGroupCertificateMode
data SmoothGroupCertificateMode
  = SelfSigned
  | VerifyAuthenticity
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SmoothGroupCertificateMode where
    parser = takeLowerText >>= \case
        "self_signed" -> pure SelfSigned
        "verify_authenticity" -> pure VerifyAuthenticity
        e -> fromTextError $ "Failure parsing SmoothGroupCertificateMode from value: '" <> e
           <> "'. Accepted values: self_signed, verify_authenticity"

instance ToText SmoothGroupCertificateMode where
    toText = \case
        SelfSigned -> "SELF_SIGNED"
        VerifyAuthenticity -> "VERIFY_AUTHENTICITY"

instance Hashable     SmoothGroupCertificateMode
instance NFData       SmoothGroupCertificateMode
instance ToByteString SmoothGroupCertificateMode
instance ToQuery      SmoothGroupCertificateMode
instance ToHeader     SmoothGroupCertificateMode

instance ToJSON SmoothGroupCertificateMode where
    toJSON = toJSONText

instance FromJSON SmoothGroupCertificateMode where
    parseJSON = parseJSONText "SmoothGroupCertificateMode"

-- | Placeholder documentation for SmoothGroupEventIdMode
data SmoothGroupEventIdMode
  = NoEventId
  | UseConfigured
  | UseTimestamp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SmoothGroupEventIdMode where
    parser = takeLowerText >>= \case
        "no_event_id" -> pure NoEventId
        "use_configured" -> pure UseConfigured
        "use_timestamp" -> pure UseTimestamp
        e -> fromTextError $ "Failure parsing SmoothGroupEventIdMode from value: '" <> e
           <> "'. Accepted values: no_event_id, use_configured, use_timestamp"

instance ToText SmoothGroupEventIdMode where
    toText = \case
        NoEventId -> "NO_EVENT_ID"
        UseConfigured -> "USE_CONFIGURED"
        UseTimestamp -> "USE_TIMESTAMP"

instance Hashable     SmoothGroupEventIdMode
instance NFData       SmoothGroupEventIdMode
instance ToByteString SmoothGroupEventIdMode
instance ToQuery      SmoothGroupEventIdMode
instance ToHeader     SmoothGroupEventIdMode

instance ToJSON SmoothGroupEventIdMode where
    toJSON = toJSONText

instance FromJSON SmoothGroupEventIdMode where
    parseJSON = parseJSONText "SmoothGroupEventIdMode"

-- | Placeholder documentation for SmoothGroupEventStopBehavior
data SmoothGroupEventStopBehavior
  = SGESBNone
  | SGESBSendEos
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SmoothGroupEventStopBehavior where
    parser = takeLowerText >>= \case
        "none" -> pure SGESBNone
        "send_eos" -> pure SGESBSendEos
        e -> fromTextError $ "Failure parsing SmoothGroupEventStopBehavior from value: '" <> e
           <> "'. Accepted values: none, send_eos"

instance ToText SmoothGroupEventStopBehavior where
    toText = \case
        SGESBNone -> "NONE"
        SGESBSendEos -> "SEND_EOS"

instance Hashable     SmoothGroupEventStopBehavior
instance NFData       SmoothGroupEventStopBehavior
instance ToByteString SmoothGroupEventStopBehavior
instance ToQuery      SmoothGroupEventStopBehavior
instance ToHeader     SmoothGroupEventStopBehavior

instance ToJSON SmoothGroupEventStopBehavior where
    toJSON = toJSONText

instance FromJSON SmoothGroupEventStopBehavior where
    parseJSON = parseJSONText "SmoothGroupEventStopBehavior"

-- | Placeholder documentation for SmoothGroupSegmentationMode
data SmoothGroupSegmentationMode
  = UseInputSegmentation
  | UseSegmentDuration
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SmoothGroupSegmentationMode where
    parser = takeLowerText >>= \case
        "use_input_segmentation" -> pure UseInputSegmentation
        "use_segment_duration" -> pure UseSegmentDuration
        e -> fromTextError $ "Failure parsing SmoothGroupSegmentationMode from value: '" <> e
           <> "'. Accepted values: use_input_segmentation, use_segment_duration"

instance ToText SmoothGroupSegmentationMode where
    toText = \case
        UseInputSegmentation -> "USE_INPUT_SEGMENTATION"
        UseSegmentDuration -> "USE_SEGMENT_DURATION"

instance Hashable     SmoothGroupSegmentationMode
instance NFData       SmoothGroupSegmentationMode
instance ToByteString SmoothGroupSegmentationMode
instance ToQuery      SmoothGroupSegmentationMode
instance ToHeader     SmoothGroupSegmentationMode

instance ToJSON SmoothGroupSegmentationMode where
    toJSON = toJSONText

instance FromJSON SmoothGroupSegmentationMode where
    parseJSON = parseJSONText "SmoothGroupSegmentationMode"

-- | Placeholder documentation for SmoothGroupSparseTrackType
data SmoothGroupSparseTrackType
  = SGSTTNone
  | SGSTTScte35
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SmoothGroupSparseTrackType where
    parser = takeLowerText >>= \case
        "none" -> pure SGSTTNone
        "scte_35" -> pure SGSTTScte35
        e -> fromTextError $ "Failure parsing SmoothGroupSparseTrackType from value: '" <> e
           <> "'. Accepted values: none, scte_35"

instance ToText SmoothGroupSparseTrackType where
    toText = \case
        SGSTTNone -> "NONE"
        SGSTTScte35 -> "SCTE_35"

instance Hashable     SmoothGroupSparseTrackType
instance NFData       SmoothGroupSparseTrackType
instance ToByteString SmoothGroupSparseTrackType
instance ToQuery      SmoothGroupSparseTrackType
instance ToHeader     SmoothGroupSparseTrackType

instance ToJSON SmoothGroupSparseTrackType where
    toJSON = toJSONText

instance FromJSON SmoothGroupSparseTrackType where
    parseJSON = parseJSONText "SmoothGroupSparseTrackType"

-- | Placeholder documentation for SmoothGroupStreamManifestBehavior
data SmoothGroupStreamManifestBehavior
  = DoNotSend
  | Send
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SmoothGroupStreamManifestBehavior where
    parser = takeLowerText >>= \case
        "do_not_send" -> pure DoNotSend
        "send" -> pure Send
        e -> fromTextError $ "Failure parsing SmoothGroupStreamManifestBehavior from value: '" <> e
           <> "'. Accepted values: do_not_send, send"

instance ToText SmoothGroupStreamManifestBehavior where
    toText = \case
        DoNotSend -> "DO_NOT_SEND"
        Send -> "SEND"

instance Hashable     SmoothGroupStreamManifestBehavior
instance NFData       SmoothGroupStreamManifestBehavior
instance ToByteString SmoothGroupStreamManifestBehavior
instance ToQuery      SmoothGroupStreamManifestBehavior
instance ToHeader     SmoothGroupStreamManifestBehavior

instance ToJSON SmoothGroupStreamManifestBehavior where
    toJSON = toJSONText

instance FromJSON SmoothGroupStreamManifestBehavior where
    parseJSON = parseJSONText "SmoothGroupStreamManifestBehavior"

-- | Placeholder documentation for SmoothGroupTimestampOffsetMode
data SmoothGroupTimestampOffsetMode
  = UseConfiguredOffset
  | UseEventStartDate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SmoothGroupTimestampOffsetMode where
    parser = takeLowerText >>= \case
        "use_configured_offset" -> pure UseConfiguredOffset
        "use_event_start_date" -> pure UseEventStartDate
        e -> fromTextError $ "Failure parsing SmoothGroupTimestampOffsetMode from value: '" <> e
           <> "'. Accepted values: use_configured_offset, use_event_start_date"

instance ToText SmoothGroupTimestampOffsetMode where
    toText = \case
        UseConfiguredOffset -> "USE_CONFIGURED_OFFSET"
        UseEventStartDate -> "USE_EVENT_START_DATE"

instance Hashable     SmoothGroupTimestampOffsetMode
instance NFData       SmoothGroupTimestampOffsetMode
instance ToByteString SmoothGroupTimestampOffsetMode
instance ToQuery      SmoothGroupTimestampOffsetMode
instance ToHeader     SmoothGroupTimestampOffsetMode

instance ToJSON SmoothGroupTimestampOffsetMode where
    toJSON = toJSONText

instance FromJSON SmoothGroupTimestampOffsetMode where
    parseJSON = parseJSONText "SmoothGroupTimestampOffsetMode"

-- | Placeholder documentation for TimecodeConfigSource
data TimecodeConfigSource
  = Embedded
  | Systemclock
  | Zerobased
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TimecodeConfigSource where
    parser = takeLowerText >>= \case
        "embedded" -> pure Embedded
        "systemclock" -> pure Systemclock
        "zerobased" -> pure Zerobased
        e -> fromTextError $ "Failure parsing TimecodeConfigSource from value: '" <> e
           <> "'. Accepted values: embedded, systemclock, zerobased"

instance ToText TimecodeConfigSource where
    toText = \case
        Embedded -> "EMBEDDED"
        Systemclock -> "SYSTEMCLOCK"
        Zerobased -> "ZEROBASED"

instance Hashable     TimecodeConfigSource
instance NFData       TimecodeConfigSource
instance ToByteString TimecodeConfigSource
instance ToQuery      TimecodeConfigSource
instance ToHeader     TimecodeConfigSource

instance ToJSON TimecodeConfigSource where
    toJSON = toJSONText

instance FromJSON TimecodeConfigSource where
    parseJSON = parseJSONText "TimecodeConfigSource"

-- | Placeholder documentation for TtmlDestinationStyleControl
data TtmlDestinationStyleControl
  = TDSCPassthrough
  | TDSCUseConfigured
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TtmlDestinationStyleControl where
    parser = takeLowerText >>= \case
        "passthrough" -> pure TDSCPassthrough
        "use_configured" -> pure TDSCUseConfigured
        e -> fromTextError $ "Failure parsing TtmlDestinationStyleControl from value: '" <> e
           <> "'. Accepted values: passthrough, use_configured"

instance ToText TtmlDestinationStyleControl where
    toText = \case
        TDSCPassthrough -> "PASSTHROUGH"
        TDSCUseConfigured -> "USE_CONFIGURED"

instance Hashable     TtmlDestinationStyleControl
instance NFData       TtmlDestinationStyleControl
instance ToByteString TtmlDestinationStyleControl
instance ToQuery      TtmlDestinationStyleControl
instance ToHeader     TtmlDestinationStyleControl

instance ToJSON TtmlDestinationStyleControl where
    toJSON = toJSONText

instance FromJSON TtmlDestinationStyleControl where
    parseJSON = parseJSONText "TtmlDestinationStyleControl"

-- | Placeholder documentation for UdpTimedMetadataId3Frame
data UdpTimedMetadataId3Frame
  = UTMIFNone
  | UTMIFPriv
  | UTMIFTdrl
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UdpTimedMetadataId3Frame where
    parser = takeLowerText >>= \case
        "none" -> pure UTMIFNone
        "priv" -> pure UTMIFPriv
        "tdrl" -> pure UTMIFTdrl
        e -> fromTextError $ "Failure parsing UdpTimedMetadataId3Frame from value: '" <> e
           <> "'. Accepted values: none, priv, tdrl"

instance ToText UdpTimedMetadataId3Frame where
    toText = \case
        UTMIFNone -> "NONE"
        UTMIFPriv -> "PRIV"
        UTMIFTdrl -> "TDRL"

instance Hashable     UdpTimedMetadataId3Frame
instance NFData       UdpTimedMetadataId3Frame
instance ToByteString UdpTimedMetadataId3Frame
instance ToQuery      UdpTimedMetadataId3Frame
instance ToHeader     UdpTimedMetadataId3Frame

instance ToJSON UdpTimedMetadataId3Frame where
    toJSON = toJSONText

instance FromJSON UdpTimedMetadataId3Frame where
    parseJSON = parseJSONText "UdpTimedMetadataId3Frame"

-- | Placeholder documentation for VideoDescriptionRespondToAfd
data VideoDescriptionRespondToAfd
  = None
  | Passthrough
  | Respond
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VideoDescriptionRespondToAfd where
    parser = takeLowerText >>= \case
        "none" -> pure None
        "passthrough" -> pure Passthrough
        "respond" -> pure Respond
        e -> fromTextError $ "Failure parsing VideoDescriptionRespondToAfd from value: '" <> e
           <> "'. Accepted values: none, passthrough, respond"

instance ToText VideoDescriptionRespondToAfd where
    toText = \case
        None -> "NONE"
        Passthrough -> "PASSTHROUGH"
        Respond -> "RESPOND"

instance Hashable     VideoDescriptionRespondToAfd
instance NFData       VideoDescriptionRespondToAfd
instance ToByteString VideoDescriptionRespondToAfd
instance ToQuery      VideoDescriptionRespondToAfd
instance ToHeader     VideoDescriptionRespondToAfd

instance ToJSON VideoDescriptionRespondToAfd where
    toJSON = toJSONText

instance FromJSON VideoDescriptionRespondToAfd where
    parseJSON = parseJSONText "VideoDescriptionRespondToAfd"

-- | Placeholder documentation for VideoDescriptionScalingBehavior
data VideoDescriptionScalingBehavior
  = Default
  | StretchToOutput
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VideoDescriptionScalingBehavior where
    parser = takeLowerText >>= \case
        "default" -> pure Default
        "stretch_to_output" -> pure StretchToOutput
        e -> fromTextError $ "Failure parsing VideoDescriptionScalingBehavior from value: '" <> e
           <> "'. Accepted values: default, stretch_to_output"

instance ToText VideoDescriptionScalingBehavior where
    toText = \case
        Default -> "DEFAULT"
        StretchToOutput -> "STRETCH_TO_OUTPUT"

instance Hashable     VideoDescriptionScalingBehavior
instance NFData       VideoDescriptionScalingBehavior
instance ToByteString VideoDescriptionScalingBehavior
instance ToQuery      VideoDescriptionScalingBehavior
instance ToHeader     VideoDescriptionScalingBehavior

instance ToJSON VideoDescriptionScalingBehavior where
    toJSON = toJSONText

instance FromJSON VideoDescriptionScalingBehavior where
    parseJSON = parseJSONText "VideoDescriptionScalingBehavior"

-- | Placeholder documentation for VideoSelectorColorSpace
data VideoSelectorColorSpace
  = Follow
  | Rec601
  | Rec709
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VideoSelectorColorSpace where
    parser = takeLowerText >>= \case
        "follow" -> pure Follow
        "rec_601" -> pure Rec601
        "rec_709" -> pure Rec709
        e -> fromTextError $ "Failure parsing VideoSelectorColorSpace from value: '" <> e
           <> "'. Accepted values: follow, rec_601, rec_709"

instance ToText VideoSelectorColorSpace where
    toText = \case
        Follow -> "FOLLOW"
        Rec601 -> "REC_601"
        Rec709 -> "REC_709"

instance Hashable     VideoSelectorColorSpace
instance NFData       VideoSelectorColorSpace
instance ToByteString VideoSelectorColorSpace
instance ToQuery      VideoSelectorColorSpace
instance ToHeader     VideoSelectorColorSpace

instance ToJSON VideoSelectorColorSpace where
    toJSON = toJSONText

instance FromJSON VideoSelectorColorSpace where
    parseJSON = parseJSONText "VideoSelectorColorSpace"

-- | Placeholder documentation for VideoSelectorColorSpaceUsage
data VideoSelectorColorSpaceUsage
  = Fallback
  | Force
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VideoSelectorColorSpaceUsage where
    parser = takeLowerText >>= \case
        "fallback" -> pure Fallback
        "force" -> pure Force
        e -> fromTextError $ "Failure parsing VideoSelectorColorSpaceUsage from value: '" <> e
           <> "'. Accepted values: fallback, force"

instance ToText VideoSelectorColorSpaceUsage where
    toText = \case
        Fallback -> "FALLBACK"
        Force -> "FORCE"

instance Hashable     VideoSelectorColorSpaceUsage
instance NFData       VideoSelectorColorSpaceUsage
instance ToByteString VideoSelectorColorSpaceUsage
instance ToQuery      VideoSelectorColorSpaceUsage
instance ToHeader     VideoSelectorColorSpaceUsage

instance ToJSON VideoSelectorColorSpaceUsage where
    toJSON = toJSONText

instance FromJSON VideoSelectorColorSpaceUsage where
    parseJSON = parseJSONText "VideoSelectorColorSpaceUsage"
