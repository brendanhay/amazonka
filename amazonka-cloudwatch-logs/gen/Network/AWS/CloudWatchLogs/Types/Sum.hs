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
