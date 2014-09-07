{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.CloudWatchLogs.V2014_03_28.GetLogEvents
    (
    -- * Request
      GetLogEvents
    -- ** Request constructor
    , mkGetLogEvents
    -- ** Request lenses
    , gleLogGroupName
    , gleLogStreamName
    , gleStartTime
    , gleEndTime
    , gleNextToken
    , gleLimit
    , gleStartFromHead

    -- * Response
    , GetLogEventsResponse
    -- ** Response lenses
    , glersEvents
    , glersNextForwardToken
    , glersNextBackwardToken
    ) where

import Network.AWS.CloudWatchLogs.V2014_03_28.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data GetLogEvents = GetLogEvents
    { _gleLogGroupName :: Text
    , _gleLogStreamName :: Text
    , _gleStartTime :: Maybe Integer
    , _gleEndTime :: Maybe Integer
    , _gleNextToken :: Maybe Text
    , _gleLimit :: Maybe Integer
    , _gleStartFromHead :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetLogEvents' request.
mkGetLogEvents :: Text -- ^ 'gleLogGroupName'
               -> Text -- ^ 'gleLogStreamName'
               -> GetLogEvents
mkGetLogEvents p1 p2 = GetLogEvents
    { _gleLogGroupName = p1
    , _gleLogStreamName = p2
    , _gleStartTime = Nothing
    , _gleEndTime = Nothing
    , _gleNextToken = Nothing
    , _gleLimit = Nothing
    , _gleStartFromHead = Nothing
    }

gleLogGroupName :: Lens' GetLogEvents Text
gleLogGroupName = lens _gleLogGroupName (\s a -> s { _gleLogGroupName = a })

gleLogStreamName :: Lens' GetLogEvents Text
gleLogStreamName =
    lens _gleLogStreamName (\s a -> s { _gleLogStreamName = a })

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
gleStartTime :: Lens' GetLogEvents (Maybe Integer)
gleStartTime = lens _gleStartTime (\s a -> s { _gleStartTime = a })

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
gleEndTime :: Lens' GetLogEvents (Maybe Integer)
gleEndTime = lens _gleEndTime (\s a -> s { _gleEndTime = a })

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the nextForwardToken or nextBackwardToken
-- fields in the response of the previous GetLogEvents request.
gleNextToken :: Lens' GetLogEvents (Maybe Text)
gleNextToken = lens _gleNextToken (\s a -> s { _gleNextToken = a })

-- | The maximum number of log events returned in the response. If you don't
-- specify a value, the request would return as much log events as can fit in
-- a response size of 1MB, up to 10,000 log events.
gleLimit :: Lens' GetLogEvents (Maybe Integer)
gleLimit = lens _gleLimit (\s a -> s { _gleLimit = a })

gleStartFromHead :: Lens' GetLogEvents (Maybe Bool)
gleStartFromHead =
    lens _gleStartFromHead (\s a -> s { _gleStartFromHead = a })

instance ToPath GetLogEvents

instance ToQuery GetLogEvents

instance ToHeaders GetLogEvents

instance ToJSON GetLogEvents

data GetLogEventsResponse = GetLogEventsResponse
    { _glersEvents :: [OutputLogEvent]
    , _glersNextForwardToken :: Maybe Text
    , _glersNextBackwardToken :: Maybe Text
    } deriving (Show, Generic)

glersEvents :: Lens' GetLogEventsResponse [OutputLogEvent]
glersEvents = lens _glersEvents (\s a -> s { _glersEvents = a })

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous request. The
-- token expires after 24 hours.
glersNextForwardToken :: Lens' GetLogEventsResponse (Maybe Text)
glersNextForwardToken =
    lens _glersNextForwardToken (\s a -> s { _glersNextForwardToken = a })

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous request. The
-- token expires after 24 hours.
glersNextBackwardToken :: Lens' GetLogEventsResponse (Maybe Text)
glersNextBackwardToken =
    lens _glersNextBackwardToken (\s a -> s { _glersNextBackwardToken = a })

instance FromJSON GetLogEventsResponse

instance AWSRequest GetLogEvents where
    type Sv GetLogEvents = CloudWatchLogs
    type Rs GetLogEvents = GetLogEventsResponse

    request = get
    response _ = jsonResponse
