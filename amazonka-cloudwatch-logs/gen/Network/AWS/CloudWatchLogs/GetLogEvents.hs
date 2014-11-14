{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.CloudWatchLogs.GetLogEvents
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
-- parameter in the request.
module Network.AWS.CloudWatchLogs.GetLogEvents
    (
    -- * Request
      GetLogEvents
    -- ** Request constructor
    , getLogEvents
    -- ** Request lenses
    , gleEndTime
    , gleLimit
    , gleLogGroupName
    , gleLogStreamName
    , gleNextToken
    , gleStartFromHead
    , gleStartTime

    -- * Response
    , GetLogEventsResponse
    -- ** Response constructor
    , getLogEventsResponse
    -- ** Response lenses
    , glerEvents
    , glerNextBackwardToken
    , glerNextForwardToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudWatchLogs.Types

data GetLogEvents = GetLogEvents
    { _gleEndTime       :: Maybe Natural
    , _gleLimit         :: Maybe Natural
    , _gleLogGroupName  :: Text
    , _gleLogStreamName :: Text
    , _gleNextToken     :: Maybe Text
    , _gleStartFromHead :: Maybe Bool
    , _gleStartTime     :: Maybe Natural
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetLogEvents' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gleEndTime' @::@ 'Maybe' 'Natural'
--
-- * 'gleLimit' @::@ 'Maybe' 'Natural'
--
-- * 'gleLogGroupName' @::@ 'Text'
--
-- * 'gleLogStreamName' @::@ 'Text'
--
-- * 'gleNextToken' @::@ 'Maybe' 'Text'
--
-- * 'gleStartFromHead' @::@ 'Maybe' 'Bool'
--
-- * 'gleStartTime' @::@ 'Maybe' 'Natural'
--
getLogEvents :: Text -- ^ 'gleLogGroupName'
             -> Text -- ^ 'gleLogStreamName'
             -> GetLogEvents
getLogEvents p1 p2 = GetLogEvents
    { _gleLogGroupName  = p1
    , _gleLogStreamName = p2
    , _gleStartTime     = Nothing
    , _gleEndTime       = Nothing
    , _gleNextToken     = Nothing
    , _gleLimit         = Nothing
    , _gleStartFromHead = Nothing
    }

gleEndTime :: Lens' GetLogEvents (Maybe Natural)
gleEndTime = lens _gleEndTime (\s a -> s { _gleEndTime = a })

-- | The maximum number of log events returned in the response. If you don't
-- specify a value, the request would return as much log events as can fit
-- in a response size of 1MB, up to 10,000 log events.
gleLimit :: Lens' GetLogEvents (Maybe Natural)
gleLimit = lens _gleLimit (\s a -> s { _gleLimit = a })

gleLogGroupName :: Lens' GetLogEvents Text
gleLogGroupName = lens _gleLogGroupName (\s a -> s { _gleLogGroupName = a })

gleLogStreamName :: Lens' GetLogEvents Text
gleLogStreamName = lens _gleLogStreamName (\s a -> s { _gleLogStreamName = a })

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the nextForwardToken or
-- nextBackwardToken fields in the response of the previous GetLogEvents
-- request.
gleNextToken :: Lens' GetLogEvents (Maybe Text)
gleNextToken = lens _gleNextToken (\s a -> s { _gleNextToken = a })

-- | If set to true, the earliest log events would be returned first. The
-- default is false (the latest log events are returned first).
gleStartFromHead :: Lens' GetLogEvents (Maybe Bool)
gleStartFromHead = lens _gleStartFromHead (\s a -> s { _gleStartFromHead = a })

gleStartTime :: Lens' GetLogEvents (Maybe Natural)
gleStartTime = lens _gleStartTime (\s a -> s { _gleStartTime = a })

instance ToPath GetLogEvents where
    toPath = const "/"

instance ToQuery GetLogEvents where
    toQuery = const mempty

instance ToHeaders GetLogEvents

instance ToBody GetLogEvents where
    toBody = toBody . encode . _gleLogGroupName

data GetLogEventsResponse = GetLogEventsResponse
    { _glerEvents            :: [OutputLogEvent]
    , _glerNextBackwardToken :: Maybe Text
    , _glerNextForwardToken  :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'GetLogEventsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'glerEvents' @::@ ['OutputLogEvent']
--
-- * 'glerNextBackwardToken' @::@ 'Maybe' 'Text'
--
-- * 'glerNextForwardToken' @::@ 'Maybe' 'Text'
--
getLogEventsResponse :: GetLogEventsResponse
getLogEventsResponse = GetLogEventsResponse
    { _glerEvents            = mempty
    , _glerNextForwardToken  = Nothing
    , _glerNextBackwardToken = Nothing
    }

glerEvents :: Lens' GetLogEventsResponse [OutputLogEvent]
glerEvents = lens _glerEvents (\s a -> s { _glerEvents = a })

glerNextBackwardToken :: Lens' GetLogEventsResponse (Maybe Text)
glerNextBackwardToken =
    lens _glerNextBackwardToken (\s a -> s { _glerNextBackwardToken = a })

glerNextForwardToken :: Lens' GetLogEventsResponse (Maybe Text)
glerNextForwardToken =
    lens _glerNextForwardToken (\s a -> s { _glerNextForwardToken = a })

instance AWSRequest GetLogEvents where
    type Sv GetLogEvents = CloudWatchLogs
    type Rs GetLogEvents = GetLogEventsResponse

    request  = post
    response = jsonResponse $ \h o -> GetLogEventsResponse
        <$> o .: "events"
        <*> o .: "nextBackwardToken"
        <*> o .: "nextForwardToken"
