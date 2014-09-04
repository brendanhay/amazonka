{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeEvents
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
-- returned. https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeEvents
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T232427Z
-- &x-amz-signedheaders=content-type;host;x-amz-date Cluster security group
-- securitygroup1 has been updated. Changes need to be applied to all clusters
-- using this cluster security group. cluster-security-group
-- 2012-12-07T23:05:02.660Z securitygroup1
-- 3eeb9efe-40c5-11e2-816a-1bba29fad1f5.
module Network.AWS.Redshift.V2012_12_01.DescribeEvents
    (
    -- * Request
      DescribeEvents
    -- ** Request constructor
    , mkDescribeEventsMessage
    -- ** Request lenses
    , demSourceIdentifier
    , demSourceType
    , demStartTime
    , demEndTime
    , demDuration
    , demMaxRecords
    , demMarker

    -- * Response
    , DescribeEventsResponse
    -- ** Response lenses
    , emMarker
    , emEvents
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEvents' request.
mkDescribeEventsMessage :: DescribeEvents
mkDescribeEventsMessage = DescribeEvents
    { _demSourceIdentifier = Nothing
    , _demSourceType = Nothing
    , _demStartTime = Nothing
    , _demEndTime = Nothing
    , _demDuration = Nothing
    , _demMaxRecords = Nothing
    , _demMarker = Nothing
    }
{-# INLINE mkDescribeEventsMessage #-}

data DescribeEvents = DescribeEvents
    { _demSourceIdentifier :: Maybe Text
      -- ^ The identifier of the event source for which events will be
      -- returned. If this parameter is not specified, then all sources
      -- are included in the response. Constraints: If SourceIdentifier is
      -- supplied, SourceType must also be provided. Specify a cluster
      -- identifier when SourceType is cluster. Specify a cluster security
      -- group name when SourceType is cluster-security-group. Specify a
      -- cluster parameter group name when SourceType is
      -- cluster-parameter-group. Specify a cluster snapshot identifier
      -- when SourceType is cluster-snapshot.
    , _demSourceType :: Maybe SourceType
      -- ^ The event source to retrieve events for. If no value is
      -- specified, all events are returned. Constraints: If SourceType is
      -- supplied, SourceIdentifier must also be provided. Specify cluster
      -- when SourceIdentifier is a cluster identifier. Specify
      -- cluster-security-group when SourceIdentifier is a cluster
      -- security group name. Specify cluster-parameter-group when
      -- SourceIdentifier is a cluster parameter group name. Specify
      -- cluster-snapshot when SourceIdentifier is a cluster snapshot
      -- identifier.
    , _demStartTime :: Maybe ISO8601
      -- ^ The beginning of the time interval to retrieve events for,
      -- specified in ISO 8601 format. For more information about ISO
      -- 8601, go to the ISO8601 Wikipedia page. Example:
      -- 2009-07-08T18:00Z.
    , _demEndTime :: Maybe ISO8601
      -- ^ The end of the time interval for which to retrieve events,
      -- specified in ISO 8601 format. For more information about ISO
      -- 8601, go to the ISO8601 Wikipedia page. Example:
      -- 2009-07-08T18:00Z.
    , _demDuration :: Maybe Integer
      -- ^ The number of minutes prior to the time of the request for which
      -- to retrieve events. For example, if the request is sent at 18:00
      -- and you specify a duration of 60, then only events which have
      -- occurred after 17:00 will be returned. Default: 60.
    , _demMaxRecords :: Maybe Integer
      -- ^ The maximum number of response records to return in each call. If
      -- the number of remaining response records exceeds the specified
      -- MaxRecords value, a value is returned in a marker field of the
      -- response. You can retrieve the next set of records by retrying
      -- the command with the returned marker value. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , _demMarker :: Maybe Text
      -- ^ An optional parameter that specifies the starting point to return
      -- a set of response records. When the results of a DescribeEvents
      -- request exceed the value specified in MaxRecords, AWS returns a
      -- value in the Marker field of the response. You can retrieve the
      -- next set of response records by providing the returned marker
      -- value in the Marker parameter and retrying the request.
    } deriving (Show, Generic)

-- | The identifier of the event source for which events will be returned. If
-- this parameter is not specified, then all sources are included in the
-- response. Constraints: If SourceIdentifier is supplied, SourceType must
-- also be provided. Specify a cluster identifier when SourceType is cluster.
-- Specify a cluster security group name when SourceType is
-- cluster-security-group. Specify a cluster parameter group name when
-- SourceType is cluster-parameter-group. Specify a cluster snapshot
-- identifier when SourceType is cluster-snapshot.
demSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
demSourceIdentifier = lens _demSourceIdentifier (\s a -> s { _demSourceIdentifier = a })
{-# INLINE demSourceIdentifier #-}

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned. Constraints: If SourceType is supplied,
-- SourceIdentifier must also be provided. Specify cluster when
-- SourceIdentifier is a cluster identifier. Specify cluster-security-group
-- when SourceIdentifier is a cluster security group name. Specify
-- cluster-parameter-group when SourceIdentifier is a cluster parameter group
-- name. Specify cluster-snapshot when SourceIdentifier is a cluster snapshot
-- identifier.
demSourceType :: Lens' DescribeEvents (Maybe SourceType)
demSourceType = lens _demSourceType (\s a -> s { _demSourceType = a })
{-# INLINE demSourceType #-}

-- | The beginning of the time interval to retrieve events for, specified in ISO
-- 8601 format. For more information about ISO 8601, go to the ISO8601
-- Wikipedia page. Example: 2009-07-08T18:00Z.
demStartTime :: Lens' DescribeEvents (Maybe ISO8601)
demStartTime = lens _demStartTime (\s a -> s { _demStartTime = a })
{-# INLINE demStartTime #-}

-- | The end of the time interval for which to retrieve events, specified in ISO
-- 8601 format. For more information about ISO 8601, go to the ISO8601
-- Wikipedia page. Example: 2009-07-08T18:00Z.
demEndTime :: Lens' DescribeEvents (Maybe ISO8601)
demEndTime = lens _demEndTime (\s a -> s { _demEndTime = a })
{-# INLINE demEndTime #-}

-- | The number of minutes prior to the time of the request for which to
-- retrieve events. For example, if the request is sent at 18:00 and you
-- specify a duration of 60, then only events which have occurred after 17:00
-- will be returned. Default: 60.
demDuration :: Lens' DescribeEvents (Maybe Integer)
demDuration = lens _demDuration (\s a -> s { _demDuration = a })
{-# INLINE demDuration #-}

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
demMaxRecords :: Lens' DescribeEvents (Maybe Integer)
demMaxRecords = lens _demMaxRecords (\s a -> s { _demMaxRecords = a })
{-# INLINE demMaxRecords #-}

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeEvents request exceed the
-- value specified in MaxRecords, AWS returns a value in the Marker field of
-- the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
demMarker :: Lens' DescribeEvents (Maybe Text)
demMarker = lens _demMarker (\s a -> s { _demMarker = a })
{-# INLINE demMarker #-}

instance ToQuery DescribeEvents where
    toQuery = genericQuery def

data DescribeEventsResponse = DescribeEventsResponse
    { _emMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    , _emEvents :: [Event]
      -- ^ A list of Event instances.
    } deriving (Show, Generic)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
emMarker :: Lens' DescribeEventsResponse (Maybe Text)
emMarker = lens _emMarker (\s a -> s { _emMarker = a })
{-# INLINE emMarker #-}

-- | A list of Event instances.
emEvents :: Lens' DescribeEventsResponse ([Event])
emEvents = lens _emEvents (\s a -> s { _emEvents = a })
{-# INLINE emEvents #-}

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEvents where
    type Sv DescribeEvents = Redshift
    type Rs DescribeEvents = DescribeEventsResponse

    request = post "DescribeEvents"
    response _ = xmlResponse

instance AWSPager DescribeEvents where
    next rq rs = (\x -> rq { _demMarker = Just x })
        <$> (_emMarker rs)
