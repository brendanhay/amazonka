{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.Sum where

import           Network.AWS.Prelude

data ExportTaskStatusCode
    = Cancelled
    | Completed
    | Failed
    | Pending
    | PendingCancel
    | Running
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ExportTaskStatusCode where
    parser = takeLowerText >>= \case
        "cancelled" -> pure Cancelled
        "completed" -> pure Completed
        "failed" -> pure Failed
        "pending" -> pure Pending
        "pending_cancel" -> pure PendingCancel
        "running" -> pure Running
        e -> fromTextError $ "Failure parsing ExportTaskStatusCode from value: '" <> e
           <> "'. Accepted values: CANCELLED, COMPLETED, FAILED, PENDING, PENDING_CANCEL, RUNNING"

instance ToText ExportTaskStatusCode where
    toText = \case
        Cancelled -> "CANCELLED"
        Completed -> "COMPLETED"
        Failed -> "FAILED"
        Pending -> "PENDING"
        PendingCancel -> "PENDING_CANCEL"
        Running -> "RUNNING"

instance Hashable     ExportTaskStatusCode
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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText OrderBy where
    parser = takeLowerText >>= \case
        "lasteventtime" -> pure LastEventTime
        "logstreamname" -> pure LogStreamName
        e -> fromTextError $ "Failure parsing OrderBy from value: '" <> e
           <> "'. Accepted values: LastEventTime, LogStreamName"

instance ToText OrderBy where
    toText = \case
        LastEventTime -> "LastEventTime"
        LogStreamName -> "LogStreamName"

instance Hashable     OrderBy
instance ToByteString OrderBy
instance ToQuery      OrderBy
instance ToHeader     OrderBy

instance ToJSON OrderBy where
    toJSON = toJSONText
