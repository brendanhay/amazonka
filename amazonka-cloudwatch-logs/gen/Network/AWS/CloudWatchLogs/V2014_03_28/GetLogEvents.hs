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
    , getLogEvents
    -- ** Request lenses
    , glerLogGroupName
    , glerLogStreamName
    , glerLimit
    , glerNextToken
    , glerStartFromHead
    , glerStartTime
    , glerEndTime

    -- * Response
    , GetLogEventsResponse
    -- ** Response lenses
    , glesNextForwardToken
    , glesNextBackwardToken
    , glesEvents
    ) where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'GetLogEvents' request.
getLogEvents :: Text -- ^ 'glerLogGroupName'
             -> Text -- ^ 'glerLogStreamName'
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

glerLogGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> GetLogEvents
    -> f GetLogEvents
glerLogGroupName f x =
    (\y -> x { _glerLogGroupName = y })
       <$> f (_glerLogGroupName x)
{-# INLINE glerLogGroupName #-}

glerLogStreamName
    :: Functor f
    => (Text
    -> f (Text))
    -> GetLogEvents
    -> f GetLogEvents
glerLogStreamName f x =
    (\y -> x { _glerLogStreamName = y })
       <$> f (_glerLogStreamName x)
{-# INLINE glerLogStreamName #-}

-- | The maximum number of log events returned in the response. If you don't
-- specify a value, the request would return as much log events as can fit in
-- a response size of 1MB, up to 10,000 log events.
glerLimit
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> GetLogEvents
    -> f GetLogEvents
glerLimit f x =
    (\y -> x { _glerLimit = y })
       <$> f (_glerLimit x)
{-# INLINE glerLimit #-}

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the nextForwardToken or nextBackwardToken
-- fields in the response of the previous GetLogEvents request.
glerNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetLogEvents
    -> f GetLogEvents
glerNextToken f x =
    (\y -> x { _glerNextToken = y })
       <$> f (_glerNextToken x)
{-# INLINE glerNextToken #-}

glerStartFromHead
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> GetLogEvents
    -> f GetLogEvents
glerStartFromHead f x =
    (\y -> x { _glerStartFromHead = y })
       <$> f (_glerStartFromHead x)
{-# INLINE glerStartFromHead #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
glerStartTime
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> GetLogEvents
    -> f GetLogEvents
glerStartTime f x =
    (\y -> x { _glerStartTime = y })
       <$> f (_glerStartTime x)
{-# INLINE glerStartTime #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
glerEndTime
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> GetLogEvents
    -> f GetLogEvents
glerEndTime f x =
    (\y -> x { _glerEndTime = y })
       <$> f (_glerEndTime x)
{-# INLINE glerEndTime #-}

instance ToPath GetLogEvents

instance ToQuery GetLogEvents

instance ToHeaders GetLogEvents

instance ToJSON GetLogEvents

data GetLogEventsResponse = GetLogEventsResponse
    { _glesNextForwardToken :: Maybe Text
      -- ^ A string token used for pagination that points to the next page
      -- of results. It must be a value obtained from the response of the
      -- previous request. The token expires after 24 hours.
    , _glesNextBackwardToken :: Maybe Text
      -- ^ A string token used for pagination that points to the next page
      -- of results. It must be a value obtained from the response of the
      -- previous request. The token expires after 24 hours.
    , _glesEvents :: [OutputLogEvent]
    } deriving (Show, Generic)

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous request. The
-- token expires after 24 hours.
glesNextForwardToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetLogEventsResponse
    -> f GetLogEventsResponse
glesNextForwardToken f x =
    (\y -> x { _glesNextForwardToken = y })
       <$> f (_glesNextForwardToken x)
{-# INLINE glesNextForwardToken #-}

-- | A string token used for pagination that points to the next page of results.
-- It must be a value obtained from the response of the previous request. The
-- token expires after 24 hours.
glesNextBackwardToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetLogEventsResponse
    -> f GetLogEventsResponse
glesNextBackwardToken f x =
    (\y -> x { _glesNextBackwardToken = y })
       <$> f (_glesNextBackwardToken x)
{-# INLINE glesNextBackwardToken #-}

glesEvents
    :: Functor f
    => ([OutputLogEvent]
    -> f ([OutputLogEvent]))
    -> GetLogEventsResponse
    -> f GetLogEventsResponse
glesEvents f x =
    (\y -> x { _glesEvents = y })
       <$> f (_glesEvents x)
{-# INLINE glesEvents #-}

instance FromJSON GetLogEventsResponse

instance AWSRequest GetLogEvents where
    type Sv GetLogEvents = CloudWatchLogs
    type Rs GetLogEvents = GetLogEventsResponse

    request = get
    response _ = jsonResponse
