{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DescribeEvents
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
-- retrieve up to 14 days' worth of events if necessary. Some of the output
-- has been omitted for brevity. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeEvents &MaxRecords=100 &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= Cache cluster created cache-cluster
-- 2014-04-01T18:22:18.202Z my-redis-primary (...output omitted...)
-- e21c81b4-b9cd-11e3-8a16-7978bb24ffdf.
module Network.AWS.ElastiCache.V2014_07_15.DescribeEvents
    (
    -- * Request
      DescribeEvents
    -- ** Request constructor
    , describeEvents
    -- ** Request lenses
    , demDuration
    , demMaxRecords
    , demSourceType
    , demSourceIdentifier
    , demMarker
    , demStartTime
    , demEndTime

    -- * Response
    , DescribeEventsResponse
    -- ** Response lenses
    , emEvents
    , emMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeEvents' request.
describeEvents :: DescribeEvents
describeEvents = DescribeEvents
    { _demDuration = Nothing
    , _demMaxRecords = Nothing
    , _demSourceType = Nothing
    , _demSourceIdentifier = Nothing
    , _demMarker = Nothing
    , _demStartTime = Nothing
    , _demEndTime = Nothing
    }
{-# INLINE describeEvents #-}

data DescribeEvents = DescribeEvents
    { _demDuration :: Maybe Integer
      -- ^ The number of minutes' worth of events to retrieve.
    , _demMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _demSourceType :: Maybe SourceType
      -- ^ The event source to retrieve events for. If no value is
      -- specified, all events are returned. Valid values are:
      -- cache-cluster | cache-parameter-group | cache-security-group |
      -- cache-subnet-group.
    , _demSourceIdentifier :: Maybe Text
      -- ^ The identifier of the event source for which events will be
      -- returned. If not specified, then all sources are included in the
      -- response.
    , _demMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    , _demStartTime :: Maybe ISO8601
      -- ^ The beginning of the time interval to retrieve events for,
      -- specified in ISO 8601 format.
    , _demEndTime :: Maybe ISO8601
      -- ^ The end of the time interval for which to retrieve events,
      -- specified in ISO 8601 format.
    } deriving (Show, Generic)

-- | The number of minutes' worth of events to retrieve.
demDuration :: Lens' DescribeEvents (Maybe Integer)
demDuration f x =
    f (_demDuration x)
        <&> \y -> x { _demDuration = y }
{-# INLINE demDuration #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
demMaxRecords :: Lens' DescribeEvents (Maybe Integer)
demMaxRecords f x =
    f (_demMaxRecords x)
        <&> \y -> x { _demMaxRecords = y }
{-# INLINE demMaxRecords #-}

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned. Valid values are: cache-cluster |
-- cache-parameter-group | cache-security-group | cache-subnet-group.
demSourceType :: Lens' DescribeEvents (Maybe SourceType)
demSourceType f x =
    f (_demSourceType x)
        <&> \y -> x { _demSourceType = y }
{-# INLINE demSourceType #-}

-- | The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response.
demSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
demSourceIdentifier f x =
    f (_demSourceIdentifier x)
        <&> \y -> x { _demSourceIdentifier = y }
{-# INLINE demSourceIdentifier #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
demMarker :: Lens' DescribeEvents (Maybe Text)
demMarker f x =
    f (_demMarker x)
        <&> \y -> x { _demMarker = y }
{-# INLINE demMarker #-}

-- | The beginning of the time interval to retrieve events for, specified in ISO
-- 8601 format.
demStartTime :: Lens' DescribeEvents (Maybe ISO8601)
demStartTime f x =
    f (_demStartTime x)
        <&> \y -> x { _demStartTime = y }
{-# INLINE demStartTime #-}

-- | The end of the time interval for which to retrieve events, specified in ISO
-- 8601 format.
demEndTime :: Lens' DescribeEvents (Maybe ISO8601)
demEndTime f x =
    f (_demEndTime x)
        <&> \y -> x { _demEndTime = y }
{-# INLINE demEndTime #-}

instance ToQuery DescribeEvents where
    toQuery = genericQuery def

data DescribeEventsResponse = DescribeEventsResponse
    { _emEvents :: [Event]
      -- ^ A list of events. Each element in the list contains detailed
      -- information about one event.
    , _emMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Show, Generic)

-- | A list of events. Each element in the list contains detailed information
-- about one event.
emEvents :: Lens' DescribeEventsResponse ([Event])
emEvents f x =
    f (_emEvents x)
        <&> \y -> x { _emEvents = y }
{-# INLINE emEvents #-}

-- | Provides an identifier to allow retrieval of paginated results.
emMarker :: Lens' DescribeEventsResponse (Maybe Text)
emMarker f x =
    f (_emMarker x)
        <&> \y -> x { _emMarker = y }
{-# INLINE emMarker #-}

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEvents where
    type Sv DescribeEvents = ElastiCache
    type Rs DescribeEvents = DescribeEventsResponse

    request = post "DescribeEvents"
    response _ = xmlResponse

instance AWSPager DescribeEvents where
    next rq rs = (\x -> rq { _demMarker = Just x })
        <$> (_emMarker rs)
