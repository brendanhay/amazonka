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
import Network.AWS.ElastiCache.V2014_07_15.Types
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
      -- returned. If not specified, then all sources are included in the
      -- response.
    , _demSourceType :: Maybe SourceType
      -- ^ The event source to retrieve events for. If no value is
      -- specified, all events are returned. Valid values are:
      -- cache-cluster | cache-parameter-group | cache-security-group |
      -- cache-subnet-group.
    , _demStartTime :: Maybe ISO8601
      -- ^ The beginning of the time interval to retrieve events for,
      -- specified in ISO 8601 format.
    , _demEndTime :: Maybe ISO8601
      -- ^ The end of the time interval for which to retrieve events,
      -- specified in ISO 8601 format.
    , _demDuration :: Maybe Integer
      -- ^ The number of minutes' worth of events to retrieve.
    , _demMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _demMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The identifier of the event source for which events will be returned. If
-- not specified, then all sources are included in the response.
demSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
demSourceIdentifier = lens _demSourceIdentifier (\s a -> s { _demSourceIdentifier = a })
{-# INLINE demSourceIdentifier #-}

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned. Valid values are: cache-cluster |
-- cache-parameter-group | cache-security-group | cache-subnet-group.
demSourceType :: Lens' DescribeEvents (Maybe SourceType)
demSourceType = lens _demSourceType (\s a -> s { _demSourceType = a })
{-# INLINE demSourceType #-}

-- | The beginning of the time interval to retrieve events for, specified in ISO
-- 8601 format.
demStartTime :: Lens' DescribeEvents (Maybe ISO8601)
demStartTime = lens _demStartTime (\s a -> s { _demStartTime = a })
{-# INLINE demStartTime #-}

-- | The end of the time interval for which to retrieve events, specified in ISO
-- 8601 format.
demEndTime :: Lens' DescribeEvents (Maybe ISO8601)
demEndTime = lens _demEndTime (\s a -> s { _demEndTime = a })
{-# INLINE demEndTime #-}

-- | The number of minutes' worth of events to retrieve.
demDuration :: Lens' DescribeEvents (Maybe Integer)
demDuration = lens _demDuration (\s a -> s { _demDuration = a })
{-# INLINE demDuration #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
demMaxRecords :: Lens' DescribeEvents (Maybe Integer)
demMaxRecords = lens _demMaxRecords (\s a -> s { _demMaxRecords = a })
{-# INLINE demMaxRecords #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
demMarker :: Lens' DescribeEvents (Maybe Text)
demMarker = lens _demMarker (\s a -> s { _demMarker = a })
{-# INLINE demMarker #-}

instance ToQuery DescribeEvents where
    toQuery = genericQuery def

data DescribeEventsResponse = DescribeEventsResponse
    { _emMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    , _emEvents :: [Event]
      -- ^ A list of events. Each element in the list contains detailed
      -- information about one event.
    } deriving (Show, Generic)

-- | Provides an identifier to allow retrieval of paginated results.
emMarker :: Lens' DescribeEventsResponse (Maybe Text)
emMarker = lens _emMarker (\s a -> s { _emMarker = a })
{-# INLINE emMarker #-}

-- | A list of events. Each element in the list contains detailed information
-- about one event.
emEvents :: Lens' DescribeEventsResponse ([Event])
emEvents = lens _emEvents (\s a -> s { _emEvents = a })
{-# INLINE emEvents #-}

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
