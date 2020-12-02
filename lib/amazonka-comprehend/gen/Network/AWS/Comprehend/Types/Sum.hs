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
  | Submitted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "submitted" -> pure Submitted
        e -> fromTextError $ "Failure parsing JobStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, in_progress, submitted"

instance ToText JobStatus where
    toText = \case
        Completed -> "COMPLETED"
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
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
  = EN
  | ES
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LanguageCode where
    parser = takeLowerText >>= \case
        "en" -> pure EN
        "es" -> pure ES
        e -> fromTextError $ "Failure parsing LanguageCode from value: '" <> e
           <> "'. Accepted values: en, es"

instance ToText LanguageCode where
    toText = \case
        EN -> "en"
        ES -> "es"

instance Hashable     LanguageCode
instance NFData       LanguageCode
instance ToByteString LanguageCode
instance ToQuery      LanguageCode
instance ToHeader     LanguageCode

instance ToJSON LanguageCode where
    toJSON = toJSONText

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
