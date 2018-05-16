{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.Sum where

import Network.AWS.Prelude

data EntityStatusCode
  = Impaired
  | Unimpaired
  | Unknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EntityStatusCode where
    parser = takeLowerText >>= \case
        "impaired" -> pure Impaired
        "unimpaired" -> pure Unimpaired
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing EntityStatusCode from value: '" <> e
           <> "'. Accepted values: impaired, unimpaired, unknown"

instance ToText EntityStatusCode where
    toText = \case
        Impaired -> "IMPAIRED"
        Unimpaired -> "UNIMPAIRED"
        Unknown -> "UNKNOWN"

instance Hashable     EntityStatusCode
instance NFData       EntityStatusCode
instance ToByteString EntityStatusCode
instance ToQuery      EntityStatusCode
instance ToHeader     EntityStatusCode

instance ToJSON EntityStatusCode where
    toJSON = toJSONText

instance FromJSON EntityStatusCode where
    parseJSON = parseJSONText "EntityStatusCode"

data EventAggregateField =
  EventTypeCategory
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventAggregateField where
    parser = takeLowerText >>= \case
        "eventtypecategory" -> pure EventTypeCategory
        e -> fromTextError $ "Failure parsing EventAggregateField from value: '" <> e
           <> "'. Accepted values: eventtypecategory"

instance ToText EventAggregateField where
    toText = \case
        EventTypeCategory -> "eventTypeCategory"

instance Hashable     EventAggregateField
instance NFData       EventAggregateField
instance ToByteString EventAggregateField
instance ToQuery      EventAggregateField
instance ToHeader     EventAggregateField

instance ToJSON EventAggregateField where
    toJSON = toJSONText

data EventStatusCode
  = Closed
  | Open
  | Upcoming
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventStatusCode where
    parser = takeLowerText >>= \case
        "closed" -> pure Closed
        "open" -> pure Open
        "upcoming" -> pure Upcoming
        e -> fromTextError $ "Failure parsing EventStatusCode from value: '" <> e
           <> "'. Accepted values: closed, open, upcoming"

instance ToText EventStatusCode where
    toText = \case
        Closed -> "closed"
        Open -> "open"
        Upcoming -> "upcoming"

instance Hashable     EventStatusCode
instance NFData       EventStatusCode
instance ToByteString EventStatusCode
instance ToQuery      EventStatusCode
instance ToHeader     EventStatusCode

instance ToJSON EventStatusCode where
    toJSON = toJSONText

instance FromJSON EventStatusCode where
    parseJSON = parseJSONText "EventStatusCode"

data EventTypeCategory
  = AccountNotification
  | Issue
  | ScheduledChange
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventTypeCategory where
    parser = takeLowerText >>= \case
        "accountnotification" -> pure AccountNotification
        "issue" -> pure Issue
        "scheduledchange" -> pure ScheduledChange
        e -> fromTextError $ "Failure parsing EventTypeCategory from value: '" <> e
           <> "'. Accepted values: accountnotification, issue, scheduledchange"

instance ToText EventTypeCategory where
    toText = \case
        AccountNotification -> "accountNotification"
        Issue -> "issue"
        ScheduledChange -> "scheduledChange"

instance Hashable     EventTypeCategory
instance NFData       EventTypeCategory
instance ToByteString EventTypeCategory
instance ToQuery      EventTypeCategory
instance ToHeader     EventTypeCategory

instance ToJSON EventTypeCategory where
    toJSON = toJSONText

instance FromJSON EventTypeCategory where
    parseJSON = parseJSONText "EventTypeCategory"
