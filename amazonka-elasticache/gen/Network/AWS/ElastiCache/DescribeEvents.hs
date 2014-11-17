{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeEvents.html>
module Network.AWS.ElastiCache.DescribeEvents
    (
    -- * Request
      DescribeEvents
    -- ** Request constructor
    , describeEvents
    -- ** Request lenses
    , deDuration
    , deEndTime
    , deMarker
    , deMaxRecords
    , deSourceIdentifier
    , deSourceType
    , deStartTime

    -- * Response
    , DescribeEventsResponse
    -- ** Response constructor
    , describeEventsResponse
    -- ** Response lenses
    , derEvents
    , derMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DescribeEvents = DescribeEvents
    { _deDuration         :: Maybe Int
    , _deEndTime          :: Maybe RFC822
    , _deMarker           :: Maybe Text
    , _deMaxRecords       :: Maybe Int
    , _deSourceIdentifier :: Maybe Text
    , _deSourceType       :: Maybe Text
    , _deStartTime        :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeEvents' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deDuration' @::@ 'Maybe' 'Int'
--
-- * 'deEndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'deMarker' @::@ 'Maybe' 'Text'
--
-- * 'deMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'deSourceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'deSourceType' @::@ 'Maybe' 'Text'
--
-- * 'deStartTime' @::@ 'Maybe' 'UTCTime'
--
describeEvents :: DescribeEvents
describeEvents = DescribeEvents
    { _deSourceIdentifier = Nothing
    , _deSourceType       = Nothing
    , _deStartTime        = Nothing
    , _deEndTime          = Nothing
    , _deDuration         = Nothing
    , _deMaxRecords       = Nothing
    , _deMarker           = Nothing
    }

-- | The number of minutes' worth of events to retrieve.
deDuration :: Lens' DescribeEvents (Maybe Int)
deDuration = lens _deDuration (\s a -> s { _deDuration = a })

-- | The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format.
deEndTime :: Lens' DescribeEvents (Maybe UTCTime)
deEndTime = lens _deEndTime (\s a -> s { _deEndTime = a })
    . mapping _Time

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
deMarker :: Lens' DescribeEvents (Maybe Text)
deMarker = lens _deMarker (\s a -> s { _deMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
deMaxRecords :: Lens' DescribeEvents (Maybe Int)
deMaxRecords = lens _deMaxRecords (\s a -> s { _deMaxRecords = a })

-- | The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response.
deSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
deSourceIdentifier =
    lens _deSourceIdentifier (\s a -> s { _deSourceIdentifier = a })

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned. Valid values are: cache-cluster |
-- cache-parameter-group | cache-security-group | cache-subnet-group.
deSourceType :: Lens' DescribeEvents (Maybe Text)
deSourceType = lens _deSourceType (\s a -> s { _deSourceType = a })

-- | The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format.
deStartTime :: Lens' DescribeEvents (Maybe UTCTime)
deStartTime = lens _deStartTime (\s a -> s { _deStartTime = a })
    . mapping _Time

data DescribeEventsResponse = DescribeEventsResponse
    { _derEvents :: [Event]
    , _derMarker :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeEventsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derEvents' @::@ ['Event']
--
-- * 'derMarker' @::@ 'Maybe' 'Text'
--
describeEventsResponse :: DescribeEventsResponse
describeEventsResponse = DescribeEventsResponse
    { _derMarker = Nothing
    , _derEvents = mempty
    }

-- | A list of events. Each element in the list contains detailed information
-- about one event.
derEvents :: Lens' DescribeEventsResponse [Event]
derEvents = lens _derEvents (\s a -> s { _derEvents = a })

-- | Provides an identifier to allow retrieval of paginated results.
derMarker :: Lens' DescribeEventsResponse (Maybe Text)
derMarker = lens _derMarker (\s a -> s { _derMarker = a })

instance ToPath DescribeEvents where
    toPath = const "/"

instance ToQuery DescribeEvents

instance ToHeaders DescribeEvents

instance AWSRequest DescribeEvents where
    type Sv DescribeEvents = ElastiCache
    type Rs DescribeEvents = DescribeEventsResponse

    request  = post "DescribeEvents"
    response = xmlResponse

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeEventsResponse"
