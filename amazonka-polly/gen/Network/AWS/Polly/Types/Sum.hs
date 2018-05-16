{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.Sum where

import Network.AWS.Prelude

data Gender
  = Female
  | Male
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Gender where
    parser = takeLowerText >>= \case
        "female" -> pure Female
        "male" -> pure Male
        e -> fromTextError $ "Failure parsing Gender from value: '" <> e
           <> "'. Accepted values: female, male"

instance ToText Gender where
    toText = \case
        Female -> "Female"
        Male -> "Male"

instance Hashable     Gender
instance NFData       Gender
instance ToByteString Gender
instance ToQuery      Gender
instance ToHeader     Gender

instance FromJSON Gender where
    parseJSON = parseJSONText "Gender"

data LanguageCode
  = CyGb
  | DaDk
  | DeDe
  | EnAu
  | EnGb
  | EnGbWls
  | EnIn
  | EnUs
  | EsEs
  | EsUs
  | FrCa
  | FrFr
  | IsIs
  | ItIt
  | JaJp
  | KoKr
  | NbNo
  | NlNl
  | PlPl
  | PtBr
  | PtPt
  | RoRo
  | RuRu
  | SvSe
  | TrTr
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LanguageCode where
    parser = takeLowerText >>= \case
        "cy-gb" -> pure CyGb
        "da-dk" -> pure DaDk
        "de-de" -> pure DeDe
        "en-au" -> pure EnAu
        "en-gb" -> pure EnGb
        "en-gb-wls" -> pure EnGbWls
        "en-in" -> pure EnIn
        "en-us" -> pure EnUs
        "es-es" -> pure EsEs
        "es-us" -> pure EsUs
        "fr-ca" -> pure FrCa
        "fr-fr" -> pure FrFr
        "is-is" -> pure IsIs
        "it-it" -> pure ItIt
        "ja-jp" -> pure JaJp
        "ko-kr" -> pure KoKr
        "nb-no" -> pure NbNo
        "nl-nl" -> pure NlNl
        "pl-pl" -> pure PlPl
        "pt-br" -> pure PtBr
        "pt-pt" -> pure PtPt
        "ro-ro" -> pure RoRo
        "ru-ru" -> pure RuRu
        "sv-se" -> pure SvSe
        "tr-tr" -> pure TrTr
        e -> fromTextError $ "Failure parsing LanguageCode from value: '" <> e
           <> "'. Accepted values: cy-gb, da-dk, de-de, en-au, en-gb, en-gb-wls, en-in, en-us, es-es, es-us, fr-ca, fr-fr, is-is, it-it, ja-jp, ko-kr, nb-no, nl-nl, pl-pl, pt-br, pt-pt, ro-ro, ru-ru, sv-se, tr-tr"

instance ToText LanguageCode where
    toText = \case
        CyGb -> "cy-GB"
        DaDk -> "da-DK"
        DeDe -> "de-DE"
        EnAu -> "en-AU"
        EnGb -> "en-GB"
        EnGbWls -> "en-GB-WLS"
        EnIn -> "en-IN"
        EnUs -> "en-US"
        EsEs -> "es-ES"
        EsUs -> "es-US"
        FrCa -> "fr-CA"
        FrFr -> "fr-FR"
        IsIs -> "is-IS"
        ItIt -> "it-IT"
        JaJp -> "ja-JP"
        KoKr -> "ko-KR"
        NbNo -> "nb-NO"
        NlNl -> "nl-NL"
        PlPl -> "pl-PL"
        PtBr -> "pt-BR"
        PtPt -> "pt-PT"
        RoRo -> "ro-RO"
        RuRu -> "ru-RU"
        SvSe -> "sv-SE"
        TrTr -> "tr-TR"

instance Hashable     LanguageCode
instance NFData       LanguageCode
instance ToByteString LanguageCode
instance ToQuery      LanguageCode
instance ToHeader     LanguageCode

instance ToJSON LanguageCode where
    toJSON = toJSONText

instance FromJSON LanguageCode where
    parseJSON = parseJSONText "LanguageCode"

data OutputFormat
  = JSON
  | MP3
  | OggVorbis
  | Pcm
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OutputFormat where
    parser = takeLowerText >>= \case
        "json" -> pure JSON
        "mp3" -> pure MP3
        "ogg_vorbis" -> pure OggVorbis
        "pcm" -> pure Pcm
        e -> fromTextError $ "Failure parsing OutputFormat from value: '" <> e
           <> "'. Accepted values: json, mp3, ogg_vorbis, pcm"

instance ToText OutputFormat where
    toText = \case
        JSON -> "json"
        MP3 -> "mp3"
        OggVorbis -> "ogg_vorbis"
        Pcm -> "pcm"

instance Hashable     OutputFormat
instance NFData       OutputFormat
instance ToByteString OutputFormat
instance ToQuery      OutputFormat
instance ToHeader     OutputFormat

instance ToJSON OutputFormat where
    toJSON = toJSONText

data SpeechMarkType
  = Sentence
  | Ssml
  | Viseme
  | Word
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SpeechMarkType where
    parser = takeLowerText >>= \case
        "sentence" -> pure Sentence
        "ssml" -> pure Ssml
        "viseme" -> pure Viseme
        "word" -> pure Word
        e -> fromTextError $ "Failure parsing SpeechMarkType from value: '" <> e
           <> "'. Accepted values: sentence, ssml, viseme, word"

instance ToText SpeechMarkType where
    toText = \case
        Sentence -> "sentence"
        Ssml -> "ssml"
        Viseme -> "viseme"
        Word -> "word"

instance Hashable     SpeechMarkType
instance NFData       SpeechMarkType
instance ToByteString SpeechMarkType
instance ToQuery      SpeechMarkType
instance ToHeader     SpeechMarkType

instance ToJSON SpeechMarkType where
    toJSON = toJSONText

data TextType
  = TTSsml
  | TTText
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TextType where
    parser = takeLowerText >>= \case
        "ssml" -> pure TTSsml
        "text" -> pure TTText
        e -> fromTextError $ "Failure parsing TextType from value: '" <> e
           <> "'. Accepted values: ssml, text"

instance ToText TextType where
    toText = \case
        TTSsml -> "ssml"
        TTText -> "text"

instance Hashable     TextType
instance NFData       TextType
instance ToByteString TextType
instance ToQuery      TextType
instance ToHeader     TextType

instance ToJSON TextType where
    toJSON = toJSONText

data VoiceId
  = Aditi
  | Amy
  | Astrid
  | Brian
  | Carla
  | Carmen
  | Celine
  | Chantal
  | Conchita
  | Cristiano
  | Dora
  | Emma
  | Enrique
  | Ewa
  | Filiz
  | Geraint
  | Giorgio
  | Gwyneth
  | Hans
  | Ines
  | Ivy
  | Jacek
  | Jan
  | Joanna
  | Joey
  | Justin
  | Karl
  | Kendra
  | Kimberly
  | Liv
  | Lotte
  | Mads
  | Maja
  | Marlene
  | Mathieu
  | Matthew
  | Maxim
  | Miguel
  | Mizuki
  | Naja
  | Nicole
  | Penelope
  | Raveena
  | Ricardo
  | Ruben
  | Russell
  | Salli
  | Seoyeon
  | Takumi
  | Tatyana
  | Vicki
  | Vitoria
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VoiceId where
    parser = takeLowerText >>= \case
        "aditi" -> pure Aditi
        "amy" -> pure Amy
        "astrid" -> pure Astrid
        "brian" -> pure Brian
        "carla" -> pure Carla
        "carmen" -> pure Carmen
        "celine" -> pure Celine
        "chantal" -> pure Chantal
        "conchita" -> pure Conchita
        "cristiano" -> pure Cristiano
        "dora" -> pure Dora
        "emma" -> pure Emma
        "enrique" -> pure Enrique
        "ewa" -> pure Ewa
        "filiz" -> pure Filiz
        "geraint" -> pure Geraint
        "giorgio" -> pure Giorgio
        "gwyneth" -> pure Gwyneth
        "hans" -> pure Hans
        "ines" -> pure Ines
        "ivy" -> pure Ivy
        "jacek" -> pure Jacek
        "jan" -> pure Jan
        "joanna" -> pure Joanna
        "joey" -> pure Joey
        "justin" -> pure Justin
        "karl" -> pure Karl
        "kendra" -> pure Kendra
        "kimberly" -> pure Kimberly
        "liv" -> pure Liv
        "lotte" -> pure Lotte
        "mads" -> pure Mads
        "maja" -> pure Maja
        "marlene" -> pure Marlene
        "mathieu" -> pure Mathieu
        "matthew" -> pure Matthew
        "maxim" -> pure Maxim
        "miguel" -> pure Miguel
        "mizuki" -> pure Mizuki
        "naja" -> pure Naja
        "nicole" -> pure Nicole
        "penelope" -> pure Penelope
        "raveena" -> pure Raveena
        "ricardo" -> pure Ricardo
        "ruben" -> pure Ruben
        "russell" -> pure Russell
        "salli" -> pure Salli
        "seoyeon" -> pure Seoyeon
        "takumi" -> pure Takumi
        "tatyana" -> pure Tatyana
        "vicki" -> pure Vicki
        "vitoria" -> pure Vitoria
        e -> fromTextError $ "Failure parsing VoiceId from value: '" <> e
           <> "'. Accepted values: aditi, amy, astrid, brian, carla, carmen, celine, chantal, conchita, cristiano, dora, emma, enrique, ewa, filiz, geraint, giorgio, gwyneth, hans, ines, ivy, jacek, jan, joanna, joey, justin, karl, kendra, kimberly, liv, lotte, mads, maja, marlene, mathieu, matthew, maxim, miguel, mizuki, naja, nicole, penelope, raveena, ricardo, ruben, russell, salli, seoyeon, takumi, tatyana, vicki, vitoria"

instance ToText VoiceId where
    toText = \case
        Aditi -> "Aditi"
        Amy -> "Amy"
        Astrid -> "Astrid"
        Brian -> "Brian"
        Carla -> "Carla"
        Carmen -> "Carmen"
        Celine -> "Celine"
        Chantal -> "Chantal"
        Conchita -> "Conchita"
        Cristiano -> "Cristiano"
        Dora -> "Dora"
        Emma -> "Emma"
        Enrique -> "Enrique"
        Ewa -> "Ewa"
        Filiz -> "Filiz"
        Geraint -> "Geraint"
        Giorgio -> "Giorgio"
        Gwyneth -> "Gwyneth"
        Hans -> "Hans"
        Ines -> "Ines"
        Ivy -> "Ivy"
        Jacek -> "Jacek"
        Jan -> "Jan"
        Joanna -> "Joanna"
        Joey -> "Joey"
        Justin -> "Justin"
        Karl -> "Karl"
        Kendra -> "Kendra"
        Kimberly -> "Kimberly"
        Liv -> "Liv"
        Lotte -> "Lotte"
        Mads -> "Mads"
        Maja -> "Maja"
        Marlene -> "Marlene"
        Mathieu -> "Mathieu"
        Matthew -> "Matthew"
        Maxim -> "Maxim"
        Miguel -> "Miguel"
        Mizuki -> "Mizuki"
        Naja -> "Naja"
        Nicole -> "Nicole"
        Penelope -> "Penelope"
        Raveena -> "Raveena"
        Ricardo -> "Ricardo"
        Ruben -> "Ruben"
        Russell -> "Russell"
        Salli -> "Salli"
        Seoyeon -> "Seoyeon"
        Takumi -> "Takumi"
        Tatyana -> "Tatyana"
        Vicki -> "Vicki"
        Vitoria -> "Vitoria"

instance Hashable     VoiceId
instance NFData       VoiceId
instance ToByteString VoiceId
instance ToQuery      VoiceId
instance ToHeader     VoiceId

instance ToJSON VoiceId where
    toJSON = toJSONText

instance FromJSON VoiceId where
    parseJSON = parseJSONText "VoiceId"
