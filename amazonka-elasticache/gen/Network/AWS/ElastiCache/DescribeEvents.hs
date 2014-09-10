{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache
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
module Network.AWS.ElastiCache
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
    -- ** Response constructor
    , mkDescribeEventsResponse
    -- ** Response lenses
    , derMarker
    , derEvents
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a DescribeEvents operation.
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
--
-- The fields accessible through corresponding lenses are:
--
-- * @SourceIdentifier ::@ @Maybe Text@
--
-- * @SourceType ::@ @Maybe SourceType@
--
-- * @StartTime ::@ @Maybe ISO8601@
--
-- * @EndTime ::@ @Maybe ISO8601@
--
-- * @Duration ::@ @Maybe Integer@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
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
-- not specified, then all sources are included in the response.
deSourceIdentifier :: Lens' DescribeEvents (Maybe Text)
deSourceIdentifier =
    lens _deSourceIdentifier (\s a -> s { _deSourceIdentifier = a })

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned. Valid values are: cache-cluster |
-- cache-parameter-group | cache-security-group | cache-subnet-group.
deSourceType :: Lens' DescribeEvents (Maybe SourceType)
deSourceType = lens _deSourceType (\s a -> s { _deSourceType = a })

-- | The beginning of the time interval to retrieve events for, specified in ISO
-- 8601 format.
deStartTime :: Lens' DescribeEvents (Maybe ISO8601)
deStartTime = lens _deStartTime (\s a -> s { _deStartTime = a })

-- | The end of the time interval for which to retrieve events, specified in ISO
-- 8601 format.
deEndTime :: Lens' DescribeEvents (Maybe ISO8601)
deEndTime = lens _deEndTime (\s a -> s { _deEndTime = a })

-- | The number of minutes' worth of events to retrieve.
deDuration :: Lens' DescribeEvents (Maybe Integer)
deDuration = lens _deDuration (\s a -> s { _deDuration = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
deMaxRecords :: Lens' DescribeEvents (Maybe Integer)
deMaxRecords = lens _deMaxRecords (\s a -> s { _deMaxRecords = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
deMarker :: Lens' DescribeEvents (Maybe Text)
deMarker = lens _deMarker (\s a -> s { _deMarker = a })

instance ToQuery DescribeEvents where
    toQuery = genericQuery def

-- | Represents the output of a DescribeEvents operation.
data DescribeEventsResponse = DescribeEventsResponse
    { _derMarker :: Maybe Text
    , _derEvents :: [Event]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEventsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @Events ::@ @[Event]@
--
mkDescribeEventsResponse :: DescribeEventsResponse
mkDescribeEventsResponse = DescribeEventsResponse
    { _derMarker = Nothing
    , _derEvents = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
derMarker :: Lens' DescribeEventsResponse (Maybe Text)
derMarker = lens _derMarker (\s a -> s { _derMarker = a })

-- | A list of events. Each element in the list contains detailed information
-- about one event.
derEvents :: Lens' DescribeEventsResponse [Event]
derEvents = lens _derEvents (\s a -> s { _derEvents = a })

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEvents where
    type Sv DescribeEvents = ElastiCache
    type Rs DescribeEvents = DescribeEventsResponse

    request = post "DescribeEvents"
    response _ = xmlResponse

instance AWSPager DescribeEvents where
    next rq rs = (\x -> rq & deMarker ?~ x)
        <$> (rs ^. derMarker)
