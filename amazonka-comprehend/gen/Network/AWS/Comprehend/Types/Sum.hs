{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.Sum where

import Network.AWS.Prelude

data EntityType
  = CommercialItem
  | Date
  | Event
  | Location
  | Organization
  | Other
  | Person
  | Quantity
  | Title
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EntityType where
    parser = takeLowerText >>= \case
        "commercial_item" -> pure CommercialItem
        "date" -> pure Date
        "event" -> pure Event
        "location" -> pure Location
        "organization" -> pure Organization
        "other" -> pure Other
        "person" -> pure Person
        "quantity" -> pure Quantity
        "title" -> pure Title
        e -> fromTextError $ "Failure parsing EntityType from value: '" <> e
           <> "'. Accepted values: commercial_item, date, event, location, organization, other, person, quantity, title"

instance ToText EntityType where
    toText = \case
        CommercialItem -> "COMMERCIAL_ITEM"
        Date -> "DATE"
        Event -> "EVENT"
        Location -> "LOCATION"
        Organization -> "ORGANIZATION"
        Other -> "OTHER"
        Person -> "PERSON"
        Quantity -> "QUANTITY"
        Title -> "TITLE"

instance Hashable     EntityType
instance NFData       EntityType
instance ToByteString EntityType
instance ToQuery      EntityType
instance ToHeader     EntityType

instance FromJSON EntityType where
    parseJSON = parseJSONText "EntityType"

data InputFormat
  = OneDocPerFile
  | OneDocPerLine
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InputFormat where
    parser = takeLowerText >>= \case
        "one_doc_per_file" -> pure OneDocPerFile
        "one_doc_per_line" -> pure OneDocPerLine
        e -> fromTextError $ "Failure parsing InputFormat from value: '" <> e
           <> "'. Accepted values: one_doc_per_file, one_doc_per_line"

instance ToText InputFormat where
    toText = \case
        OneDocPerFile -> "ONE_DOC_PER_FILE"
        OneDocPerLine -> "ONE_DOC_PER_LINE"

instance Hashable     InputFormat
instance NFData       InputFormat
instance ToByteString InputFormat
instance ToQuery      InputFormat
instance ToHeader     InputFormat

instance ToJSON InputFormat where
    toJSON = toJSONText

instance FromJSON InputFormat where
    parseJSON = parseJSONText "InputFormat"

data JobStatus
  = Completed
  | Failed
  | InProgress
  | StopRequested
  | Stopped
  | Submitted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "stop_requested" -> pure StopRequested
        "stopped" -> pure Stopped
        "submitted" -> pure Submitted
        e -> fromTextError $ "Failure parsing JobStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, in_progress, stop_requested, stopped, submitted"

instance ToText JobStatus where
    toText = \case
        Completed -> "COMPLETED"
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        StopRequested -> "STOP_REQUESTED"
        Stopped -> "STOPPED"
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

data LanguageCode
  = DE
  | EN
  | ES
  | FR
  | IT
  | PT
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LanguageCode where
    parser = takeLowerText >>= \case
        "de" -> pure DE
        "en" -> pure EN
        "es" -> pure ES
        "fr" -> pure FR
        "it" -> pure IT
        "pt" -> pure PT
        e -> fromTextError $ "Failure parsing LanguageCode from value: '" <> e
           <> "'. Accepted values: de, en, es, fr, it, pt"

instance ToText LanguageCode where
    toText = \case
        DE -> "de"
        EN -> "en"
        ES -> "es"
        FR -> "fr"
        IT -> "it"
        PT -> "pt"

instance Hashable     LanguageCode
instance NFData       LanguageCode
instance ToByteString LanguageCode
instance ToQuery      LanguageCode
instance ToHeader     LanguageCode

instance ToJSON LanguageCode where
    toJSON = toJSONText

instance FromJSON LanguageCode where
    parseJSON = parseJSONText "LanguageCode"

data ModelStatus
  = MSDeleting
  | MSInError
  | MSStopRequested
  | MSStopped
  | MSSubmitted
  | MSTrained
  | MSTraining
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ModelStatus where
    parser = takeLowerText >>= \case
        "deleting" -> pure MSDeleting
        "in_error" -> pure MSInError
        "stop_requested" -> pure MSStopRequested
        "stopped" -> pure MSStopped
        "submitted" -> pure MSSubmitted
        "trained" -> pure MSTrained
        "training" -> pure MSTraining
        e -> fromTextError $ "Failure parsing ModelStatus from value: '" <> e
           <> "'. Accepted values: deleting, in_error, stop_requested, stopped, submitted, trained, training"

instance ToText ModelStatus where
    toText = \case
        MSDeleting -> "DELETING"
        MSInError -> "IN_ERROR"
        MSStopRequested -> "STOP_REQUESTED"
        MSStopped -> "STOPPED"
        MSSubmitted -> "SUBMITTED"
        MSTrained -> "TRAINED"
        MSTraining -> "TRAINING"

instance Hashable     ModelStatus
instance NFData       ModelStatus
instance ToByteString ModelStatus
instance ToQuery      ModelStatus
instance ToHeader     ModelStatus

instance ToJSON ModelStatus where
    toJSON = toJSONText

instance FromJSON ModelStatus where
    parseJSON = parseJSONText "ModelStatus"

data PartOfSpeechTagType
  = Adj
  | Adp
  | Adv
  | Aux
  | Cconj
  | Conj
  | Det
  | Intj
  | Noun
  | Num
  | O
  | Part
  | Pron
  | Propn
  | Punct
  | Sconj
  | Sym
  | Verb
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PartOfSpeechTagType where
    parser = takeLowerText >>= \case
        "adj" -> pure Adj
        "adp" -> pure Adp
        "adv" -> pure Adv
        "aux" -> pure Aux
        "cconj" -> pure Cconj
        "conj" -> pure Conj
        "det" -> pure Det
        "intj" -> pure Intj
        "noun" -> pure Noun
        "num" -> pure Num
        "o" -> pure O
        "part" -> pure Part
        "pron" -> pure Pron
        "propn" -> pure Propn
        "punct" -> pure Punct
        "sconj" -> pure Sconj
        "sym" -> pure Sym
        "verb" -> pure Verb
        e -> fromTextError $ "Failure parsing PartOfSpeechTagType from value: '" <> e
           <> "'. Accepted values: adj, adp, adv, aux, cconj, conj, det, intj, noun, num, o, part, pron, propn, punct, sconj, sym, verb"

instance ToText PartOfSpeechTagType where
    toText = \case
        Adj -> "ADJ"
        Adp -> "ADP"
        Adv -> "ADV"
        Aux -> "AUX"
        Cconj -> "CCONJ"
        Conj -> "CONJ"
        Det -> "DET"
        Intj -> "INTJ"
        Noun -> "NOUN"
        Num -> "NUM"
        O -> "O"
        Part -> "PART"
        Pron -> "PRON"
        Propn -> "PROPN"
        Punct -> "PUNCT"
        Sconj -> "SCONJ"
        Sym -> "SYM"
        Verb -> "VERB"

instance Hashable     PartOfSpeechTagType
instance NFData       PartOfSpeechTagType
instance ToByteString PartOfSpeechTagType
instance ToQuery      PartOfSpeechTagType
instance ToHeader     PartOfSpeechTagType

instance FromJSON PartOfSpeechTagType where
    parseJSON = parseJSONText "PartOfSpeechTagType"

data SentimentType
  = Mixed
  | Negative
  | Neutral
  | Positive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SentimentType where
    parser = takeLowerText >>= \case
        "mixed" -> pure Mixed
        "negative" -> pure Negative
        "neutral" -> pure Neutral
        "positive" -> pure Positive
        e -> fromTextError $ "Failure parsing SentimentType from value: '" <> e
           <> "'. Accepted values: mixed, negative, neutral, positive"

instance ToText SentimentType where
    toText = \case
        Mixed -> "MIXED"
        Negative -> "NEGATIVE"
        Neutral -> "NEUTRAL"
        Positive -> "POSITIVE"

instance Hashable     SentimentType
instance NFData       SentimentType
instance ToByteString SentimentType
instance ToQuery      SentimentType
instance ToHeader     SentimentType

instance FromJSON SentimentType where
    parseJSON = parseJSONText "SentimentType"

data SyntaxLanguageCode
  = SLCDE
  | SLCEN
  | SLCES
  | SLCFR
  | SLCIT
  | SLCPT
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SyntaxLanguageCode where
    parser = takeLowerText >>= \case
        "de" -> pure SLCDE
        "en" -> pure SLCEN
        "es" -> pure SLCES
        "fr" -> pure SLCFR
        "it" -> pure SLCIT
        "pt" -> pure SLCPT
        e -> fromTextError $ "Failure parsing SyntaxLanguageCode from value: '" <> e
           <> "'. Accepted values: de, en, es, fr, it, pt"

instance ToText SyntaxLanguageCode where
    toText = \case
        SLCDE -> "de"
        SLCEN -> "en"
        SLCES -> "es"
        SLCFR -> "fr"
        SLCIT -> "it"
        SLCPT -> "pt"

instance Hashable     SyntaxLanguageCode
instance NFData       SyntaxLanguageCode
instance ToByteString SyntaxLanguageCode
instance ToQuery      SyntaxLanguageCode
instance ToHeader     SyntaxLanguageCode

instance ToJSON SyntaxLanguageCode where
    toJSON = toJSONText
