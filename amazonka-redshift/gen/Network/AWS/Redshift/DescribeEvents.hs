{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeEvents.html>
module Network.AWS.Redshift.DescribeEvents
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
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DescribeEvents = DescribeEvents
    { _deDuration         :: Maybe Int
    , _deEndTime          :: Maybe RFC822
    , _deMarker           :: Maybe Text
    , _deMaxRecords       :: Maybe Int
    , _deSourceIdentifier :: Maybe Text
    , _deSourceType       :: Maybe SourceType
    , _deStartTime        :: Maybe RFC822
    } deriving (Eq, Show)

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
-- * 'deSourceType' @::@ 'Maybe' 'SourceType'
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

-- | The number of minutes prior to the time of the request for which to
-- retrieve events. For example, if the request is sent at 18:00 and you
-- specify a duration of 60, then only events which have occurred after
-- 17:00 will be returned. Default: @60@.
deDuration :: Lens' DescribeEvents (Maybe Int)
deDuration = lens _deDuration (\s a -> s { _deDuration = a })

-- | The end of the time interval for which to retrieve events, specified in
-- ISO 8601 format. For more information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.> Example:
-- @2009-07-08T18:00Z@.
deEndTime :: Lens' DescribeEvents (Maybe UTCTime)
deEndTime = lens _deEndTime (\s a -> s { _deEndTime = a }) . mapping _Time

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeEvents> request exceed
-- the value specified in @MaxRecords@, AWS returns a value in the @Marker@
-- field of the response. You can retrieve the next set of response records
-- by providing the returned marker value in the @Marker@ parameter and
-- retrying the request.
deMarker :: Lens' DescribeEvents (Maybe Text)
deMarker = lens _deMarker (\s a -> s { _deMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: @100@ Constraints: minimum 20, maximum
-- 100.
deMaxRecords :: Lens' DescribeEvents (Maybe Int)
deMaxRecords = lens _deMaxRecords (\s a -> s { _deMaxRecords = a })

-- | The identifier of the event source for which events will be returned. If
-- this parameter is not specified, then all sources are included in the
-- response. Constraints: If /SourceIdentifier/ is supplied, /SourceType/
-- must also be provided. Specify a cluster identifier when /SourceType/ is
-- @cluster@. Specify a cluster security group name when /SourceType/ is
-- @cluster-security-group@. Specify a cluster parameter group name when
-- /SourceType/ is @cluster-parameter-group@. Specify a cluster snapshot
-- identifier when /SourceType/ is @cluster-snapshot@.
deSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
deSourceIdentifier =
    lens _deSourceIdentifier (\s a -> s { _deSourceIdentifier = a })

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned. Constraints: If /SourceType/ is supplied,
-- /SourceIdentifier/ must also be provided. Specify @cluster@ when
-- /SourceIdentifier/ is a cluster identifier. Specify
-- @cluster-security-group@ when /SourceIdentifier/ is a cluster security
-- group name. Specify @cluster-parameter-group@ when /SourceIdentifier/ is
-- a cluster parameter group name. Specify @cluster-snapshot@ when
-- /SourceIdentifier/ is a cluster snapshot identifier.
deSourceType :: Lens' DescribeEvents (Maybe SourceType)
deSourceType = lens _deSourceType (\s a -> s { _deSourceType = a })

-- | The beginning of the time interval to retrieve events for, specified in
-- ISO 8601 format. For more information about ISO 8601, go to the
-- <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.> Example:
-- @2009-07-08T18:00Z@.
deStartTime :: Lens' DescribeEvents (Maybe UTCTime)
deStartTime = lens _deStartTime (\s a -> s { _deStartTime = a }) . mapping _Time

data DescribeEventsResponse = DescribeEventsResponse
    { _derEvents :: List "Event" Event
    , _derMarker :: Maybe Text
    } deriving (Eq, Show)

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

-- | A list of Event> instances.
derEvents :: Lens' DescribeEventsResponse [Event]
derEvents = lens _derEvents (\s a -> s { _derEvents = a }) . _List

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for the
-- request.
derMarker :: Lens' DescribeEventsResponse (Maybe Text)
derMarker = lens _derMarker (\s a -> s { _derMarker = a })

instance ToPath DescribeEvents where
    toPath = const "/"

instance ToQuery DescribeEvents where
    toQuery DescribeEvents{..} = mconcat
        [ "Duration"         =? _deDuration
        , "EndTime"          =? _deEndTime
        , "Marker"           =? _deMarker
        , "MaxRecords"       =? _deMaxRecords
        , "SourceIdentifier" =? _deSourceIdentifier
        , "SourceType"       =? _deSourceType
        , "StartTime"        =? _deStartTime
        ]

instance ToHeaders DescribeEvents

instance AWSRequest DescribeEvents where
    type Sv DescribeEvents = Redshift
    type Rs DescribeEvents = DescribeEventsResponse

    request  = post "DescribeEvents"
    response = xmlResponse

instance FromXML DescribeEventsResponse where
    parseXML = withElement "DescribeEventsResult" $ \x -> DescribeEventsResponse
        <$> x .@  "Events"
        <*> x .@? "Marker"

instance AWSPager DescribeEvents where
    page rq rs
        | stop (rq ^. deMarker) = Nothing
        | otherwise = (\x -> rq & deMarker ?~ x)
            <$> (rs ^. derMarker)
