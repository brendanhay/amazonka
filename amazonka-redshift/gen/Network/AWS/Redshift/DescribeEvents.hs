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

-- Module      : Network.AWS.Redshift.DescribeEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns events related to clusters, security groups, snapshots, and
-- parameter groups for the past 14 days. Events specific to a particular
-- cluster, security group, snapshot or parameter group can be obtained by
-- providing the name as a parameter. By default, the past hour of events are
-- returned.
module Network.AWS.Redshift.DescribeEvents
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
import Network.AWS.Redshift.Types

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

-- | The number of minutes prior to the time of the request for which to
-- retrieve events. For example, if the request is sent at 18:00 and you
-- specify a duration of 60, then only events which have occurred after
-- 17:00 will be returned. Default: 60.
demDuration :: Lens' DescribeEventsMessage (Maybe Int)
demDuration = lens _demDuration (\s a -> s { _demDuration = a })

-- | The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format. For more information about ISO 8601, go to the ISO8601
-- Wikipedia page. Example: 2009-07-08T18:00Z.
demEndTime :: Lens' DescribeEventsMessage (Maybe UTCTime)
demEndTime = lens _demEndTime (\s a -> s { _demEndTime = a })
    . mapping _Time

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeEvents request exceed
-- the value specified in MaxRecords, AWS returns a value in the Marker
-- field of the response. You can retrieve the next set of response records
-- by providing the returned marker value in the Marker parameter and
-- retrying the request.
demMarker :: Lens' DescribeEventsMessage (Maybe Text)
demMarker = lens _demMarker (\s a -> s { _demMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
demMaxRecords :: Lens' DescribeEventsMessage (Maybe Int)
demMaxRecords = lens _demMaxRecords (\s a -> s { _demMaxRecords = a })

-- | The identifier of the event source for which events will be returned. If
-- this parameter is not specified, then all sources are included in the
-- response. Constraints: If SourceIdentifier is supplied, SourceType must
-- also be provided. Specify a cluster identifier when SourceType is
-- cluster. Specify a cluster security group name when SourceType is
-- cluster-security-group. Specify a cluster parameter group name when
-- SourceType is cluster-parameter-group. Specify a cluster snapshot
-- identifier when SourceType is cluster-snapshot.
demSourceIdentifier :: Lens' DescribeEventsMessage (Maybe Text)
demSourceIdentifier =
    lens _demSourceIdentifier (\s a -> s { _demSourceIdentifier = a })

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned. Constraints: If SourceType is supplied,
-- SourceIdentifier must also be provided. Specify cluster when
-- SourceIdentifier is a cluster identifier. Specify cluster-security-group
-- when SourceIdentifier is a cluster security group name. Specify
-- cluster-parameter-group when SourceIdentifier is a cluster parameter
-- group name. Specify cluster-snapshot when SourceIdentifier is a cluster
-- snapshot identifier.
demSourceType :: Lens' DescribeEventsMessage (Maybe Text)
demSourceType = lens _demSourceType (\s a -> s { _demSourceType = a })

-- | The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format. For more information about ISO 8601, go to the ISO8601
-- Wikipedia page. Example: 2009-07-08T18:00Z.
demStartTime :: Lens' DescribeEventsMessage (Maybe UTCTime)
demStartTime = lens _demStartTime (\s a -> s { _demStartTime = a })
    . mapping _Time
instance ToQuery DescribeEventsMessage

instance ToPath DescribeEventsMessage where
    toPath = const "/"

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

-- | A list of Event instances.
emEvents :: Lens' EventsMessage [Event]
emEvents = lens _emEvents (\s a -> s { _emEvents = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
emMarker :: Lens' EventsMessage (Maybe Text)
emMarker = lens _emMarker (\s a -> s { _emMarker = a })
instance FromXML EventsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventsMessage"

instance AWSRequest DescribeEventsMessage where
    type Sv DescribeEventsMessage = Redshift
    type Rs DescribeEventsMessage = EventsMessage

    request  = post "DescribeEvents"
    response = xmlResponse $ \h x -> EventsMessage
        <$> x %| "Events"
        <*> x %| "Marker"
