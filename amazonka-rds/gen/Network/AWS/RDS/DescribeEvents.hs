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

-- Module      : Network.AWS.RDS.DescribeEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns events related to DB instances, DB security groups, DB snapshots,
-- and DB parameter groups for the past 14 days. Events specific to a
-- particular DB instance, DB security group, database snapshot, or DB
-- parameter group can be obtained by providing the name as a parameter. By
-- default, the past hour of events are returned.
module Network.AWS.RDS.DescribeEvents
    (
    -- * Request
      DescribeEventsMessage
    -- ** Request constructor
    , describeEvents
    -- ** Request lenses
    , demDuration
    , demEndTime
    , demEventCategories
    , demFilters
    , demMarker
    , demMaxRecords
    , demSourceIdentifier
    , demSourceType
    , demStartTime

    -- * Response
    , EventsMessage
    -- ** Response constructor
    , describeEventsResponse
    -- ** Response lenses
    , emEvents
    , emMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeEventsMessage = DescribeEventsMessage
    { _demDuration         :: Maybe Int
    , _demEndTime          :: Maybe RFC822
    , _demEventCategories  :: [Text]
    , _demFilters          :: [Filter]
    , _demMarker           :: Maybe Text
    , _demMaxRecords       :: Maybe Int
    , _demSourceIdentifier :: Maybe Text
    , _demSourceType       :: Maybe Text
    , _demStartTime        :: Maybe RFC822
    } deriving (Eq, Show, Generic)

-- | 'DescribeEventsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'demDuration' @::@ 'Maybe' 'Int'
--
-- * 'demEndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'demEventCategories' @::@ ['Text']
--
-- * 'demFilters' @::@ ['Filter']
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
describeEvents :: DescribeEventsMessage
describeEvents = DescribeEventsMessage
    { _demSourceIdentifier = Nothing
    , _demSourceType       = Nothing
    , _demStartTime        = Nothing
    , _demEndTime          = Nothing
    , _demDuration         = Nothing
    , _demEventCategories  = mempty
    , _demFilters          = mempty
    , _demMaxRecords       = Nothing
    , _demMarker           = Nothing
    }

-- | The number of minutes to retrieve events for. Default: 60.
demDuration :: Lens' DescribeEventsMessage (Maybe Int)
demDuration = lens _demDuration (\s a -> s { _demDuration = a })

-- | The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format. For more information about ISO 8601, go to the ISO8601
-- Wikipedia page. Example: 2009-07-08T18:00Z.
demEndTime :: Lens' DescribeEventsMessage (Maybe UTCTime)
demEndTime = lens _demEndTime (\s a -> s { _demEndTime = a })
    . mapping _Time

-- | A list of event categories that trigger notifications for a event
-- notification subscription.
demEventCategories :: Lens' DescribeEventsMessage [Text]
demEventCategories =
    lens _demEventCategories (\s a -> s { _demEventCategories = a })

-- | This parameter is not currently supported.
demFilters :: Lens' DescribeEventsMessage [Filter]
demFilters = lens _demFilters (\s a -> s { _demFilters = a })

-- | An optional pagination token provided by a previous DescribeEvents
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by MaxRecords.
demMarker :: Lens' DescribeEventsMessage (Maybe Text)
demMarker = lens _demMarker (\s a -> s { _demMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
demMaxRecords :: Lens' DescribeEventsMessage (Maybe Int)
demMaxRecords = lens _demMaxRecords (\s a -> s { _demMaxRecords = a })

-- | The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response.
-- Constraints: If SourceIdentifier is supplied, SourceType must also be
-- provided. If the source type is DBInstance, then a DBInstanceIdentifier
-- must be supplied. If the source type is DBSecurityGroup, a
-- DBSecurityGroupName must be supplied. If the source type is
-- DBParameterGroup, a DBParameterGroupName must be supplied. If the source
-- type is DBSnapshot, a DBSnapshotIdentifier must be supplied. Cannot end
-- with a hyphen or contain two consecutive hyphens.
demSourceIdentifier :: Lens' DescribeEventsMessage (Maybe Text)
demSourceIdentifier =
    lens _demSourceIdentifier (\s a -> s { _demSourceIdentifier = a })

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned.
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
describeEventsResponse :: EventsMessage
describeEventsResponse = EventsMessage
    { _emMarker = Nothing
    , _emEvents = mempty
    }

-- | A list of Event instances.
emEvents :: Lens' EventsMessage [Event]
emEvents = lens _emEvents (\s a -> s { _emEvents = a })

-- | An optional pagination token provided by a previous Events request. If
-- this parameter is specified, the response includes only records beyond
-- the marker, up to the value specified by MaxRecords .
emMarker :: Lens' EventsMessage (Maybe Text)
emMarker = lens _emMarker (\s a -> s { _emMarker = a })

instance FromXML EventsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventsMessage"

instance AWSRequest DescribeEventsMessage where
    type Sv DescribeEventsMessage = RDS
    type Rs DescribeEventsMessage = EventsMessage

    request  = post "DescribeEvents"
    response = xmlResponse $ \h x -> EventsMessage
        <$> x %| "Events"
        <*> x %| "Marker"
