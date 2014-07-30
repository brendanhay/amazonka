{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.DescribeEvents
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
module Network.AWS.ElastiCache.V2014_03_24.DescribeEvents where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ElastiCache.V2014_03_24.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DescribeEvents' request.
describeEvents :: DescribeEvents
describeEvents = DescribeEvents
    { _demMaxRecords = Nothing
    , _demDuration = Nothing
    , _demSourceType = Nothing
    , _demSourceIdentifier = Nothing
    , _demMarker = Nothing
    , _demStartTime = Nothing
    , _demEndTime = Nothing
    }

data DescribeEvents = DescribeEvents
    { _demMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _demDuration :: Maybe Integer
      -- ^ The number of minutes' worth of events to retrieve.
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
    } deriving (Generic)

instance ToQuery DescribeEvents where
    toQuery = genericToQuery def

instance AWSRequest DescribeEvents where
    type Sv DescribeEvents = ElastiCache
    type Rs DescribeEvents = DescribeEventsResponse

    request = post "DescribeEvents"
    response _ = xmlResponse

instance AWSPager DescribeEvents where
    next rq rs = (\x -> rq { _demMarker = Just x })
        <$> _emMarker rs

data DescribeEventsResponse = DescribeEventsResponse
    { _emEvents :: [Event]
      -- ^ A list of events. Each element in the list contains detailed
      -- information about one event.
    , _emMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Generic)

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions
