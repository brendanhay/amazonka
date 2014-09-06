{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DescribeCache
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns information about the cache of a gateway. This
-- operation is supported only for the gateway-cached volume architecture. The
-- response includes disk IDs that are configured as cache, and it includes
-- the amount of cache allocated and used. Example Request The following
-- example shows a request to obtain a description of a gateway's working
-- storage. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- Content-Type: application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120912T120000Z x-amz-target:
-- StorageGateway_20120630.DescribeCache {
-- "GatewayARN":"arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 12 Sep 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 271 {
-- "CacheAllocationInBytes": 2199023255552, "CacheDirtyPercentage": 0.07,
-- "CacheHitPercentage": 99.68, "CacheMissPercentage": 0.32,
-- "CacheUsedPercentage": 0.07, "DiskIds": [ "pci-0000:03:00.0-scsi-0:0:0:0",
-- "pci-0000:04:00.0-scsi-0:1:0:0" ], "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
module Network.AWS.StorageGateway.V2013_06_30.DescribeCache
    (
    -- * Request
      DescribeCache
    -- ** Request constructor
    , mkDescribeCache
    -- ** Request lenses
    , dcGatewayARN

    -- * Response
    , DescribeCacheResponse
    -- ** Response lenses
    , dcrsGatewayARN
    , dcrsDiskIds
    , dcrsCacheAllocatedInBytes
    , dcrsCacheUsedPercentage
    , dcrsCacheDirtyPercentage
    , dcrsCacheHitPercentage
    , dcrsCacheMissPercentage
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

newtype DescribeCache = DescribeCache
    { _dcGatewayARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCache' request.
mkDescribeCache :: Text -- ^ 'dcGatewayARN'
                -> DescribeCache
mkDescribeCache p1 = DescribeCache
    { _dcGatewayARN = p1
    }
{-# INLINE mkDescribeCache #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dcGatewayARN :: Lens' DescribeCache Text
dcGatewayARN = lens _dcGatewayARN (\s a -> s { _dcGatewayARN = a })
{-# INLINE dcGatewayARN #-}

instance ToPath DescribeCache

instance ToQuery DescribeCache

instance ToHeaders DescribeCache

instance ToJSON DescribeCache

data DescribeCacheResponse = DescribeCacheResponse
    { _dcrsGatewayARN :: Maybe Text
    , _dcrsDiskIds :: [Text]
    , _dcrsCacheAllocatedInBytes :: Maybe Integer
    , _dcrsCacheUsedPercentage :: Maybe Double
    , _dcrsCacheDirtyPercentage :: Maybe Double
    , _dcrsCacheHitPercentage :: Maybe Double
    , _dcrsCacheMissPercentage :: Maybe Double
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dcrsGatewayARN :: Lens' DescribeCacheResponse (Maybe Text)
dcrsGatewayARN = lens _dcrsGatewayARN (\s a -> s { _dcrsGatewayARN = a })
{-# INLINE dcrsGatewayARN #-}

dcrsDiskIds :: Lens' DescribeCacheResponse [Text]
dcrsDiskIds = lens _dcrsDiskIds (\s a -> s { _dcrsDiskIds = a })
{-# INLINE dcrsDiskIds #-}

dcrsCacheAllocatedInBytes :: Lens' DescribeCacheResponse (Maybe Integer)
dcrsCacheAllocatedInBytes =
    lens _dcrsCacheAllocatedInBytes
         (\s a -> s { _dcrsCacheAllocatedInBytes = a })
{-# INLINE dcrsCacheAllocatedInBytes #-}

dcrsCacheUsedPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrsCacheUsedPercentage =
    lens _dcrsCacheUsedPercentage
         (\s a -> s { _dcrsCacheUsedPercentage = a })
{-# INLINE dcrsCacheUsedPercentage #-}

dcrsCacheDirtyPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrsCacheDirtyPercentage =
    lens _dcrsCacheDirtyPercentage
         (\s a -> s { _dcrsCacheDirtyPercentage = a })
{-# INLINE dcrsCacheDirtyPercentage #-}

dcrsCacheHitPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrsCacheHitPercentage =
    lens _dcrsCacheHitPercentage (\s a -> s { _dcrsCacheHitPercentage = a })
{-# INLINE dcrsCacheHitPercentage #-}

dcrsCacheMissPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcrsCacheMissPercentage =
    lens _dcrsCacheMissPercentage
         (\s a -> s { _dcrsCacheMissPercentage = a })
{-# INLINE dcrsCacheMissPercentage #-}

instance FromJSON DescribeCacheResponse

instance AWSRequest DescribeCache where
    type Sv DescribeCache = StorageGateway
    type Rs DescribeCache = DescribeCacheResponse

    request = get
    response _ = jsonResponse
