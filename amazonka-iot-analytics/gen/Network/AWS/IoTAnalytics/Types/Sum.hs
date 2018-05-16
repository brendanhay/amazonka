{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.Sum where

import Network.AWS.Prelude

data ChannelStatus
  = CSActive
  | CSCreating
  | CSDeleting
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChannelStatus where
    parser = takeLowerText >>= \case
        "active" -> pure CSActive
        "creating" -> pure CSCreating
        "deleting" -> pure CSDeleting
        e -> fromTextError $ "Failure parsing ChannelStatus from value: '" <> e
           <> "'. Accepted values: active, creating, deleting"

instance ToText ChannelStatus where
    toText = \case
        CSActive -> "ACTIVE"
        CSCreating -> "CREATING"
        CSDeleting -> "DELETING"

instance Hashable     ChannelStatus
instance NFData       ChannelStatus
instance ToByteString ChannelStatus
instance ToQuery      ChannelStatus
instance ToHeader     ChannelStatus

instance FromJSON ChannelStatus where
    parseJSON = parseJSONText "ChannelStatus"

data DatasetContentState
  = DCSCreating
  | DCSFailed
  | DCSSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DatasetContentState where
    parser = takeLowerText >>= \case
        "creating" -> pure DCSCreating
        "failed" -> pure DCSFailed
        "succeeded" -> pure DCSSucceeded
        e -> fromTextError $ "Failure parsing DatasetContentState from value: '" <> e
           <> "'. Accepted values: creating, failed, succeeded"

instance ToText DatasetContentState where
    toText = \case
        DCSCreating -> "CREATING"
        DCSFailed -> "FAILED"
        DCSSucceeded -> "SUCCEEDED"

instance Hashable     DatasetContentState
instance NFData       DatasetContentState
instance ToByteString DatasetContentState
instance ToQuery      DatasetContentState
instance ToHeader     DatasetContentState

instance FromJSON DatasetContentState where
    parseJSON = parseJSONText "DatasetContentState"

data DatasetStatus
  = Active
  | Creating
  | Deleting
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DatasetStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "creating" -> pure Creating
        "deleting" -> pure Deleting
        e -> fromTextError $ "Failure parsing DatasetStatus from value: '" <> e
           <> "'. Accepted values: active, creating, deleting"

instance ToText DatasetStatus where
    toText = \case
        Active -> "ACTIVE"
        Creating -> "CREATING"
        Deleting -> "DELETING"

instance Hashable     DatasetStatus
instance NFData       DatasetStatus
instance ToByteString DatasetStatus
instance ToQuery      DatasetStatus
instance ToHeader     DatasetStatus

instance FromJSON DatasetStatus where
    parseJSON = parseJSONText "DatasetStatus"

data DatastoreStatus
  = DSActive
  | DSCreating
  | DSDeleting
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DatastoreStatus where
    parser = takeLowerText >>= \case
        "active" -> pure DSActive
        "creating" -> pure DSCreating
        "deleting" -> pure DSDeleting
        e -> fromTextError $ "Failure parsing DatastoreStatus from value: '" <> e
           <> "'. Accepted values: active, creating, deleting"

instance ToText DatastoreStatus where
    toText = \case
        DSActive -> "ACTIVE"
        DSCreating -> "CREATING"
        DSDeleting -> "DELETING"

instance Hashable     DatastoreStatus
instance NFData       DatastoreStatus
instance ToByteString DatastoreStatus
instance ToQuery      DatastoreStatus
instance ToHeader     DatastoreStatus

instance FromJSON DatastoreStatus where
    parseJSON = parseJSONText "DatastoreStatus"

data LoggingLevel =
  Error'
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoggingLevel where
    parser = takeLowerText >>= \case
        "error" -> pure Error'
        e -> fromTextError $ "Failure parsing LoggingLevel from value: '" <> e
           <> "'. Accepted values: error"

instance ToText LoggingLevel where
    toText = \case
        Error' -> "ERROR"

instance Hashable     LoggingLevel
instance NFData       LoggingLevel
instance ToByteString LoggingLevel
instance ToQuery      LoggingLevel
instance ToHeader     LoggingLevel

instance ToJSON LoggingLevel where
    toJSON = toJSONText

instance FromJSON LoggingLevel where
    parseJSON = parseJSONText "LoggingLevel"

data ReprocessingStatus
  = Cancelled
  | Failed
  | Running
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReprocessingStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure Cancelled
        "failed" -> pure Failed
        "running" -> pure Running
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing ReprocessingStatus from value: '" <> e
           <> "'. Accepted values: cancelled, failed, running, succeeded"

instance ToText ReprocessingStatus where
    toText = \case
        Cancelled -> "CANCELLED"
        Failed -> "FAILED"
        Running -> "RUNNING"
        Succeeded -> "SUCCEEDED"

instance Hashable     ReprocessingStatus
instance NFData       ReprocessingStatus
instance ToByteString ReprocessingStatus
instance ToQuery      ReprocessingStatus
instance ToHeader     ReprocessingStatus

instance FromJSON ReprocessingStatus where
    parseJSON = parseJSONText "ReprocessingStatus"
