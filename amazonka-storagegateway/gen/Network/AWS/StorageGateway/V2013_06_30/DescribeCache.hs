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
    , describeCache
    -- ** Request lenses
    , dciGatewayARN

    -- * Response
    , DescribeCacheResponse
    -- ** Response lenses
    , dcoDiskIds
    , dcoCacheUsedPercentage
    , dcoCacheDirtyPercentage
    , dcoCacheHitPercentage
    , dcoCacheMissPercentage
    , dcoGatewayARN
    , dcoCacheAllocatedInBytes
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeCache' request.
describeCache :: Text -- ^ 'dciGatewayARN'
              -> DescribeCache
describeCache p1 = DescribeCache
    { _dciGatewayARN = p1
    }
{-# INLINE describeCache #-}

data DescribeCache = DescribeCache
    { _dciGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dciGatewayARN :: Lens' DescribeCache (Text)
dciGatewayARN f x =
    f (_dciGatewayARN x)
        <&> \y -> x { _dciGatewayARN = y }
{-# INLINE dciGatewayARN #-}

instance ToPath DescribeCache

instance ToQuery DescribeCache

instance ToHeaders DescribeCache

instance ToJSON DescribeCache

data DescribeCacheResponse = DescribeCacheResponse
    { _dcoDiskIds :: [Text]
    , _dcoCacheUsedPercentage :: Maybe Double
    , _dcoCacheDirtyPercentage :: Maybe Double
    , _dcoCacheHitPercentage :: Maybe Double
    , _dcoCacheMissPercentage :: Maybe Double
    , _dcoGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _dcoCacheAllocatedInBytes :: Maybe Integer
    } deriving (Show, Generic)

dcoDiskIds :: Lens' DescribeCacheResponse ([Text])
dcoDiskIds f x =
    f (_dcoDiskIds x)
        <&> \y -> x { _dcoDiskIds = y }
{-# INLINE dcoDiskIds #-}

dcoCacheUsedPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcoCacheUsedPercentage f x =
    f (_dcoCacheUsedPercentage x)
        <&> \y -> x { _dcoCacheUsedPercentage = y }
{-# INLINE dcoCacheUsedPercentage #-}

dcoCacheDirtyPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcoCacheDirtyPercentage f x =
    f (_dcoCacheDirtyPercentage x)
        <&> \y -> x { _dcoCacheDirtyPercentage = y }
{-# INLINE dcoCacheDirtyPercentage #-}

dcoCacheHitPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcoCacheHitPercentage f x =
    f (_dcoCacheHitPercentage x)
        <&> \y -> x { _dcoCacheHitPercentage = y }
{-# INLINE dcoCacheHitPercentage #-}

dcoCacheMissPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcoCacheMissPercentage f x =
    f (_dcoCacheMissPercentage x)
        <&> \y -> x { _dcoCacheMissPercentage = y }
{-# INLINE dcoCacheMissPercentage #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dcoGatewayARN :: Lens' DescribeCacheResponse (Maybe Text)
dcoGatewayARN f x =
    f (_dcoGatewayARN x)
        <&> \y -> x { _dcoGatewayARN = y }
{-# INLINE dcoGatewayARN #-}

dcoCacheAllocatedInBytes :: Lens' DescribeCacheResponse (Maybe Integer)
dcoCacheAllocatedInBytes f x =
    f (_dcoCacheAllocatedInBytes x)
        <&> \y -> x { _dcoCacheAllocatedInBytes = y }
{-# INLINE dcoCacheAllocatedInBytes #-}

instance FromJSON DescribeCacheResponse

instance AWSRequest DescribeCache where
    type Sv DescribeCache = StorageGateway
    type Rs DescribeCache = DescribeCacheResponse

    request = get
    response _ = jsonResponse
