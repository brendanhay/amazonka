{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.GetLogEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves log events from the specified log stream. You can provide an
-- optional time range to filter the results on the event timestamp. By
-- default, this operation returns as much log events as can fit in a response
-- size of 1MB, up to 10,000 log events. The response will always include a
-- nextForwardToken and a nextBackwardToken in the response body. You can use
-- any of these tokens in subsequent GetLogEvents requests to paginate through
-- events in either forward or backward direction. You can also limit the
-- number of log events returned in the response by specifying the limit
-- parameter in the request. Retrieves all the events from a log stream The
-- following is an example of a GetLogEvents request and response. POST /
-- HTTP/1.1 Host: logs.. X-Amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.GetLogEvents { "logGroupName":
-- "exampleLogGroupName", "logStreamName": "exampleLogStreamName" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]> { "events": [ { "ingestionTime": 1396035394997,
-- "timestamp": 1396035378988, "message": "Example Event 1" }, {
-- "ingestionTime": 1396035394997, "timestamp": 1396035378988, "message":
-- "Example Event 2" }, { "ingestionTime": 1396035394997, "timestamp":
-- 1396035378989, "message": "Example Event 3" } ], "nextBackwardToken":
-- "b/31132629274945519779805322857203735586714454643391594505",
-- "nextForwardToken":
-- "f/31132629323784151764587387538205132201699397759403884544" }.
module Network.AWS.CloudWatchLogs.V2014_03_28.GetLogEvents where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'GetLogEvents' request.
getLogEvents :: Text -- ^ '_glerLogGroupName'
             -> Text -- ^ '_glerLogStreamName'
             -> GetLogEvents
getLogEvents p1 p2 = GetLogEvents
    { _glerLogGroupName = p1
    , _glerLogStreamName = p2
    , _glerLimit = Nothing
    , _glerNextToken = Nothing
    , _glerStartFromHead = Nothing
    , _glerStartTime = Nothing
    , _glerEndTime = Nothing
    }

data GetLogEvents = GetLogEvents
    { _glerLogGroupName :: Text
    , _glerLogStreamName :: Text
    , _glerLimit :: Maybe Integer
      -- ^ The maximum number of log events returned in the response. If you
      -- don't specify a value, the request would return as much log
      -- events as can fit in a response size of 1MB, up to 10,000 log
      -- events.
    , _glerNextToken :: Maybe Text
      -- ^ A string token used for pagination that points to the next page
      -- of results. It must be a value obtained from the nextForwardToken
      -- or nextBackwardToken fields in the response of the previous
      -- GetLogEvents request.
    , _glerStartFromHead :: Maybe Bool
    , _glerStartTime :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    , _glerEndTime :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    } deriving (Show, Generic)

makeLenses ''GetLogEvents

instance ToPath GetLogEvents

instance ToQuery GetLogEvents

instance ToHeaders GetLogEvents

instance ToJSON GetLogEvents

data GetLogEventsResponse = GetLogEventsResponse
    { _glesNextBackwardToken :: Maybe Text
      -- ^ A string token used for pagination that points to the next page
      -- of results. It must be a value obtained from the response of the
      -- previous request. The token expires after 24 hours.
    , _glesNextForwardToken :: Maybe Text
      -- ^ A string token used for pagination that points to the next page
      -- of results. It must be a value obtained from the response of the
      -- previous request. The token expires after 24 hours.
    , _glesEvents :: [OutputLogEvent]
    } deriving (Show, Generic)

makeLenses ''GetLogEventsResponse

instance FromJSON GetLogEventsResponse

instance AWSRequest GetLogEvents where
    type Sv GetLogEvents = CloudWatchLogs
    type Rs GetLogEvents = GetLogEventsResponse

    request = get
    response _ = jsonResponse
