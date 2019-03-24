{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.Sum where

import Network.AWS.Prelude

-- | The method used to distribute log data to the destination, which can be either random or grouped by log stream.
--
--
data Distribution
  = ByLogStream
  | Random
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Distribution where
    parser = takeLowerText >>= \case
        "bylogstream" -> pure ByLogStream
        "random" -> pure Random
        e -> fromTextError $ "Failure parsing Distribution from value: '" <> e
           <> "'. Accepted values: bylogstream, random"

instance ToText Distribution where
    toText = \case
        ByLogStream -> "ByLogStream"
        Random -> "Random"

instance Hashable     Distribution
instance NFData       Distribution
instance ToByteString Distribution
instance ToQuery      Distribution
instance ToHeader     Distribution

instance ToJSON Distribution where
    toJSON = toJSONText

instance FromJSON Distribution where
    parseJSON = parseJSONText "Distribution"

data ExportTaskStatusCode
  = ETSCCancelled
  | ETSCCompleted
  | ETSCFailed
  | ETSCPending
  | ETSCPendingCancel
  | ETSCRunning
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExportTaskStatusCode where
    parser = takeLowerText >>= \case
        "cancelled" -> pure ETSCCancelled
        "completed" -> pure ETSCCompleted
        "failed" -> pure ETSCFailed
        "pending" -> pure ETSCPending
        "pending_cancel" -> pure ETSCPendingCancel
        "running" -> pure ETSCRunning
        e -> fromTextError $ "Failure parsing ExportTaskStatusCode from value: '" <> e
           <> "'. Accepted values: cancelled, completed, failed, pending, pending_cancel, running"

instance ToText ExportTaskStatusCode where
    toText = \case
        ETSCCancelled -> "CANCELLED"
        ETSCCompleted -> "COMPLETED"
        ETSCFailed -> "FAILED"
        ETSCPending -> "PENDING"
        ETSCPendingCancel -> "PENDING_CANCEL"
        ETSCRunning -> "RUNNING"

instance Hashable     ExportTaskStatusCode
instance NFData       ExportTaskStatusCode
instance ToByteString ExportTaskStatusCode
instance ToQuery      ExportTaskStatusCode
instance ToHeader     ExportTaskStatusCode

instance ToJSON ExportTaskStatusCode where
    toJSON = toJSONText

instance FromJSON ExportTaskStatusCode where
    parseJSON = parseJSONText "ExportTaskStatusCode"

data OrderBy
  = LastEventTime
  | LogStreamName
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OrderBy where
    parser = takeLowerText >>= \case
        "lasteventtime" -> pure LastEventTime
        "logstreamname" -> pure LogStreamName
        e -> fromTextError $ "Failure parsing OrderBy from value: '" <> e
           <> "'. Accepted values: lasteventtime, logstreamname"

instance ToText OrderBy where
    toText = \case
        LastEventTime -> "LastEventTime"
        LogStreamName -> "LogStreamName"

instance Hashable     OrderBy
instance NFData       OrderBy
instance ToByteString OrderBy
instance ToQuery      OrderBy
instance ToHeader     OrderBy

instance ToJSON OrderBy where
    toJSON = toJSONText

data QueryStatus
  = Cancelled
  | Complete
  | Failed
  | Running
  | Scheduled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText QueryStatus where
    parser = takeLowerText >>= \case
        "cancelled" -> pure Cancelled
        "complete" -> pure Complete
        "failed" -> pure Failed
        "running" -> pure Running
        "scheduled" -> pure Scheduled
        e -> fromTextError $ "Failure parsing QueryStatus from value: '" <> e
           <> "'. Accepted values: cancelled, complete, failed, running, scheduled"

instance ToText QueryStatus where
    toText = \case
        Cancelled -> "Cancelled"
        Complete -> "Complete"
        Failed -> "Failed"
        Running -> "Running"
        Scheduled -> "Scheduled"

instance Hashable     QueryStatus
instance NFData       QueryStatus
instance ToByteString QueryStatus
instance ToQuery      QueryStatus
instance ToHeader     QueryStatus

instance ToJSON QueryStatus where
    toJSON = toJSONText

instance FromJSON QueryStatus where
    parseJSON = parseJSONText "QueryStatus"
