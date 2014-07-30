{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.DescribeCacheParameterGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheParameterGroups operation returns a list of cache
-- parameter group descriptions. If a cache parameter group name is specified,
-- the list will contain only the descriptions for that group.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeCacheParameterGroups &MaxRecords=100 &Version=2014-03-24
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2014-03-27T01%3A34%3A31.045Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE default.memcached1.4 memcached1.4 Default
-- parameter group for memcached1.4 mycacheparametergroup memcached1.4 My
-- cache parameter group mycacheparametergroup1 memcached1.4 My first cache
-- parameter group mycacheparametergroup3 memcached1.4 My first cache
-- parameter group 7193fbb8-b7fc-11e0-9b0b-a9261be2b354.
module Network.AWS.ElastiCache.V2014_03_24.DescribeCacheParameterGroups where

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

-- | Minimum specification for a 'DescribeCacheParameterGroups' request.
describeCacheParameterGroups :: DescribeCacheParameterGroups
describeCacheParameterGroups = DescribeCacheParameterGroups
    { _dcpgmMaxRecords = Nothing
    , _dcpgmCacheParameterGroupName = Nothing
    , _dcpgmMarker = Nothing
    }

data DescribeCacheParameterGroups = DescribeCacheParameterGroups
    { _dcpgmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _dcpgmCacheParameterGroupName :: Maybe Text
      -- ^ The name of a specific cache parameter group to return details
      -- for.
    , _dcpgmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Generic)

instance ToQuery DescribeCacheParameterGroups where
    toQuery = genericToQuery def

instance AWSRequest DescribeCacheParameterGroups where
    type Sv DescribeCacheParameterGroups = ElastiCache
    type Rs DescribeCacheParameterGroups = DescribeCacheParameterGroupsResponse

    request = post "DescribeCacheParameterGroups"
    response _ = xmlResponse

instance AWSPager DescribeCacheParameterGroups where
    next rq rs = (\x -> rq { _dcpgmMarker = Just x })
        <$> _cpgmMarker rs

data DescribeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse
    { _cpgmCacheParameterGroups :: [CacheParameterGroup]
      -- ^ A list of cache parameter groups. Each element in the list
      -- contains detailed information about one cache parameter group.
    , _cpgmMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Generic)

instance FromXML DescribeCacheParameterGroupsResponse where
    fromXMLOptions = xmlOptions
