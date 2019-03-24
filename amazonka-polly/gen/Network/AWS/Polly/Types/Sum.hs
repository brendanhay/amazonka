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
  = CmnCn
  | CyGb
  | DaDk
  | DeDe
  | EnAu
  | EnGb
  | EnGbWls
  | EnIn
  | EnUs
  | EsEs
  | EsMx
  | EsUs
  | FrCa
  | FrFr
  | HiIn
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
        "cmn-cn" -> pure CmnCn
        "cy-gb" -> pure CyGb
        "da-dk" -> pure DaDk
        "de-de" -> pure DeDe
        "en-au" -> pure EnAu
        "en-gb" -> pure EnGb
        "en-gb-wls" -> pure EnGbWls
        "en-in" -> pure EnIn
        "en-us" -> pure EnUs
        "es-es" -> pure EsEs
        "es-mx" -> pure EsMx
        "es-us" -> pure EsUs
        "fr-ca" -> pure FrCa
        "fr-fr" -> pure FrFr
        "hi-in" -> pure HiIn
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
           <> "'. Accepted values: cmn-cn, cy-gb, da-dk, de-de, en-au, en-gb, en-gb-wls, en-in, en-us, es-es, es-mx, es-us, fr-ca, fr-fr, hi-in, is-is, it-it, ja-jp, ko-kr, nb-no, nl-nl, pl-pl, pt-br, pt-pt, ro-ro, ru-ru, sv-se, tr-tr"

instance ToText LanguageCode where
    toText = \case
        CmnCn -> "cmn-CN"
        CyGb -> "cy-GB"
        DaDk -> "da-DK"
        DeDe -> "de-DE"
        EnAu -> "en-AU"
        EnGb -> "en-GB"
        EnGbWls -> "en-GB-WLS"
        EnIn -> "en-IN"
        EnUs -> "en-US"
        EsEs -> "es-ES"
        EsMx -> "es-MX"
        EsUs -> "es-US"
        FrCa -> "fr-CA"
        FrFr -> "fr-FR"
        HiIn -> "hi-IN"
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

instance FromJSON OutputFormat where
    parseJSON = parseJSONText "OutputFormat"

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

instance FromJSON SpeechMarkType where
    parseJSON = parseJSONText "SpeechMarkType"

data TaskStatus
  = Completed
  | Failed
  | InProgress
  | Scheduled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TaskStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "scheduled" -> pure Scheduled
        e -> fromTextError $ "Failure parsing TaskStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, inprogress, scheduled"

instance ToText TaskStatus where
    toText = \case
        Completed -> "completed"
        Failed -> "failed"
        InProgress -> "inProgress"
        Scheduled -> "scheduled"

instance Hashable     TaskStatus
instance NFData       TaskStatus
instance ToByteString TaskStatus
instance ToQuery      TaskStatus
instance ToHeader     TaskStatus

instance ToJSON TaskStatus where
    toJSON = toJSONText

instance FromJSON TaskStatus where
    parseJSON = parseJSONText "TaskStatus"

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

instance FromJSON TextType where
    parseJSON = parseJSONText "TextType"

data VoiceId
  = Aditi
  | Amy
  | Astrid
  | Bianca
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
  | Lea
  | Liv
  | Lotte
  | Lucia
  | Mads
  | Maja
  | Marlene
  | Mathieu
  | Matthew
  | Maxim
  | Mia
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
  | Zhiyu
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VoiceId where
    parser = takeLowerText >>= \case
        "aditi" -> pure Aditi
        "amy" -> pure Amy
        "astrid" -> pure Astrid
        "bianca" -> pure Bianca
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
        "lea" -> pure Lea
        "liv" -> pure Liv
        "lotte" -> pure Lotte
        "lucia" -> pure Lucia
        "mads" -> pure Mads
        "maja" -> pure Maja
        "marlene" -> pure Marlene
        "mathieu" -> pure Mathieu
        "matthew" -> pure Matthew
        "maxim" -> pure Maxim
        "mia" -> pure Mia
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
        "zhiyu" -> pure Zhiyu
        e -> fromTextError $ "Failure parsing VoiceId from value: '" <> e
           <> "'. Accepted values: aditi, amy, astrid, bianca, brian, carla, carmen, celine, chantal, conchita, cristiano, dora, emma, enrique, ewa, filiz, geraint, giorgio, gwyneth, hans, ines, ivy, jacek, jan, joanna, joey, justin, karl, kendra, kimberly, lea, liv, lotte, lucia, mads, maja, marlene, mathieu, matthew, maxim, mia, miguel, mizuki, naja, nicole, penelope, raveena, ricardo, ruben, russell, salli, seoyeon, takumi, tatyana, vicki, vitoria, zhiyu"

instance ToText VoiceId where
    toText = \case
        Aditi -> "Aditi"
        Amy -> "Amy"
        Astrid -> "Astrid"
        Bianca -> "Bianca"
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
        Lea -> "Lea"
        Liv -> "Liv"
        Lotte -> "Lotte"
        Lucia -> "Lucia"
        Mads -> "Mads"
        Maja -> "Maja"
        Marlene -> "Marlene"
        Mathieu -> "Mathieu"
        Matthew -> "Matthew"
        Maxim -> "Maxim"
        Mia -> "Mia"
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
        Zhiyu -> "Zhiyu"

instance Hashable     VoiceId
instance NFData       VoiceId
instance ToByteString VoiceId
instance ToQuery      VoiceId
instance ToHeader     VoiceId

instance ToJSON VoiceId where
    toJSON = toJSONText

instance FromJSON VoiceId where
    parseJSON = parseJSONText "VoiceId"
