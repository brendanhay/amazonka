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

-- Module      : Network.AWS.ElastiCache.DescribeEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeEvents operation returns events related to cache clusters,
-- cache security groups, and cache parameter groups. You can obtain events
-- specific to a particular cache cluster, cache security group, or cache
-- parameter group by providing the name as a parameter. By default, only the
-- events occurring within the last hour are returned; however, you can
-- retrieve up to 14 days' worth of events if necessary.
module Network.AWS.ElastiCache.DescribeEvents
    (
    -- * Request
      DescribeEventsMessage
    -- ** Request constructor
    , describeEventsMessage
    -- ** Request lenses
    , demDuration
    , demEndTime
    , demMarker
    , demMaxRecords
    , demSourceIdentifier
    , demSourceType
    , demStartTime

    -- * Response
    , EventsMessage
    -- ** Response constructor
    , eventsMessage
    -- ** Response lenses
    , emEvents
    , emMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DescribeEventsMessage = DescribeEventsMessage
    { _demDuration         :: Maybe Int
    , _demEndTime          :: Maybe RFC822
    , _demMarker           :: Maybe Text
    , _demMaxRecords       :: Maybe Int
    , _demSourceIdentifier :: Maybe Text
    , _demSourceType       :: Maybe Text
    , _demStartTime        :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeEventsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'demDuration' @::@ 'Maybe' 'Int'
--
-- * 'demEndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'demMarker' @::@ 'Maybe' 'Text'
--
-- * 'demMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'demSourceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'demSourceType' @::@ 'Maybe' 'Text'
--
-- * 'demStartTime' @::@ 'Maybe' 'UTCTime'
--
describeEventsMessage :: DescribeEventsMessage
describeEventsMessage = DescribeEventsMessage
    { _demSourceIdentifier = Nothing
    , _demSourceType       = Nothing
    , _demStartTime        = Nothing
    , _demEndTime          = Nothing
    , _demDuration         = Nothing
    , _demMaxRecords       = Nothing
    , _demMarker           = Nothing
    }

-- | The number of minutes' worth of events to retrieve.
demDuration :: Lens' DescribeEventsMessage (Maybe Int)
demDuration = lens _demDuration (\s a -> s { _demDuration = a })

-- | The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format.
demEndTime :: Lens' DescribeEventsMessage (Maybe UTCTime)
demEndTime = lens _demEndTime (\s a -> s { _demEndTime = a })
    . mapping _Time

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
demMarker :: Lens' DescribeEventsMessage (Maybe Text)
demMarker = lens _demMarker (\s a -> s { _demMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
demMaxRecords :: Lens' DescribeEventsMessage (Maybe Int)
demMaxRecords = lens _demMaxRecords (\s a -> s { _demMaxRecords = a })

-- | The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response.
demSourceIdentifier :: Lens' DescribeEventsMessage (Maybe Text)
demSourceIdentifier =
    lens _demSourceIdentifier (\s a -> s { _demSourceIdentifier = a })

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned. Valid values are: cache-cluster |
-- cache-parameter-group | cache-security-group | cache-subnet-group.
demSourceType :: Lens' DescribeEventsMessage (Maybe Text)
demSourceType = lens _demSourceType (\s a -> s { _demSourceType = a })

-- | The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format.
demStartTime :: Lens' DescribeEventsMessage (Maybe UTCTime)
demStartTime = lens _demStartTime (\s a -> s { _demStartTime = a })
    . mapping _Time

instance ToPath DescribeEventsMessage where
    toPath = const "/"

instance ToQuery DescribeEventsMessage

data EventsMessage = EventsMessage
    { _emEvents :: [Event]
    , _emMarker :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'EventsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'emEvents' @::@ ['Event']
--
-- * 'emMarker' @::@ 'Maybe' 'Text'
--
eventsMessage :: EventsMessage
eventsMessage = EventsMessage
    { _emMarker = Nothing
    , _emEvents = mempty
    }

-- | A list of events. Each element in the list contains detailed information
-- about one event.
emEvents :: Lens' EventsMessage [Event]
emEvents = lens _emEvents (\s a -> s { _emEvents = a })

-- | Provides an identifier to allow retrieval of paginated results.
emMarker :: Lens' EventsMessage (Maybe Text)
emMarker = lens _emMarker (\s a -> s { _emMarker = a })

instance AWSRequest DescribeEventsMessage where
    type Sv DescribeEventsMessage = ElastiCache
    type Rs DescribeEventsMessage = EventsMessage

    request  = post "DescribeEvents"
    response = const . xmlResponse $ \h x -> EventsMessage
record
