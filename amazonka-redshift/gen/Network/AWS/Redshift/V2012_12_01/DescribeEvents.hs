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
    , mkDescribeEvents
    -- ** Request lenses
    , deSourceIdentifier
    , deSourceType
    , deStartTime
    , deEndTime
    , deDuration
    , deMaxRecords
    , deMarker

    -- * Response
    , DescribeEventsResponse
    -- ** Response lenses
    , dersMarker
    , dersEvents
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
data DescribeEvents = DescribeEvents
    { _deSourceIdentifier :: Maybe Text
    , _deSourceType :: Maybe SourceType
    , _deStartTime :: Maybe ISO8601
    , _deEndTime :: Maybe ISO8601
    , _deDuration :: Maybe Integer
    , _deMaxRecords :: Maybe Integer
    , _deMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEvents' request.
mkDescribeEvents :: DescribeEvents
mkDescribeEvents = DescribeEvents
    { _deSourceIdentifier = Nothing
    , _deSourceType = Nothing
    , _deStartTime = Nothing
    , _deEndTime = Nothing
    , _deDuration = Nothing
    , _deMaxRecords = Nothing
    , _deMarker = Nothing
    }

-- | The identifier of the event source for which events will be returned. If
-- this parameter is not specified, then all sources are included in the
-- response. Constraints: If SourceIdentifier is supplied, SourceType must
-- also be provided. Specify a cluster identifier when SourceType is cluster.
-- Specify a cluster security group name when SourceType is
-- cluster-security-group. Specify a cluster parameter group name when
-- SourceType is cluster-parameter-group. Specify a cluster snapshot
-- identifier when SourceType is cluster-snapshot.
deSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
deSourceIdentifier =
    lens _deSourceIdentifier (\s a -> s { _deSourceIdentifier = a })

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned. Constraints: If SourceType is supplied,
-- SourceIdentifier must also be provided. Specify cluster when
-- SourceIdentifier is a cluster identifier. Specify cluster-security-group
-- when SourceIdentifier is a cluster security group name. Specify
-- cluster-parameter-group when SourceIdentifier is a cluster parameter group
-- name. Specify cluster-snapshot when SourceIdentifier is a cluster snapshot
-- identifier.
deSourceType :: Lens' DescribeEvents (Maybe SourceType)
deSourceType = lens _deSourceType (\s a -> s { _deSourceType = a })

-- | The beginning of the time interval to retrieve events for, specified in ISO
-- 8601 format. For more information about ISO 8601, go to the ISO8601
-- Wikipedia page. Example: 2009-07-08T18:00Z.
deStartTime :: Lens' DescribeEvents (Maybe ISO8601)
deStartTime = lens _deStartTime (\s a -> s { _deStartTime = a })

-- | The end of the time interval for which to retrieve events, specified in ISO
-- 8601 format. For more information about ISO 8601, go to the ISO8601
-- Wikipedia page. Example: 2009-07-08T18:00Z.
deEndTime :: Lens' DescribeEvents (Maybe ISO8601)
deEndTime = lens _deEndTime (\s a -> s { _deEndTime = a })

-- | The number of minutes prior to the time of the request for which to
-- retrieve events. For example, if the request is sent at 18:00 and you
-- specify a duration of 60, then only events which have occurred after 17:00
-- will be returned. Default: 60.
deDuration :: Lens' DescribeEvents (Maybe Integer)
deDuration = lens _deDuration (\s a -> s { _deDuration = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
deMaxRecords :: Lens' DescribeEvents (Maybe Integer)
deMaxRecords = lens _deMaxRecords (\s a -> s { _deMaxRecords = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeEvents request exceed the
-- value specified in MaxRecords, AWS returns a value in the Marker field of
-- the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
deMarker :: Lens' DescribeEvents (Maybe Text)
deMarker = lens _deMarker (\s a -> s { _deMarker = a })

instance ToQuery DescribeEvents where
    toQuery = genericQuery def

-- | Contains the output from the DescribeEvents action.
data DescribeEventsResponse = DescribeEventsResponse
    { _dersMarker :: Maybe Text
    , _dersEvents :: [Event]
    } deriving (Show, Generic)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
dersMarker :: Lens' DescribeEventsResponse (Maybe Text)
dersMarker = lens _dersMarker (\s a -> s { _dersMarker = a })

-- | A list of Event instances.
dersEvents :: Lens' DescribeEventsResponse [Event]
dersEvents = lens _dersEvents (\s a -> s { _dersEvents = a })

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEvents where
    type Sv DescribeEvents = Redshift
    type Rs DescribeEvents = DescribeEventsResponse

    request = post "DescribeEvents"
    response _ = xmlResponse

instance AWSPager DescribeEvents where
    next rq rs = (\x -> rq & deMarker ?~ x)
        <$> (rs ^. dersMarker)
