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
    , mkDescribeCacheInput
    -- ** Request lenses
    , dciGatewayARN

    -- * Response
    , DescribeCacheResponse
    -- ** Response lenses
    , dcoGatewayARN
    , dcoDiskIds
    , dcoCacheAllocatedInBytes
    , dcoCacheUsedPercentage
    , dcoCacheDirtyPercentage
    , dcoCacheHitPercentage
    , dcoCacheMissPercentage
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeCache' request.
mkDescribeCacheInput :: Text -- ^ 'dciGatewayARN'
                     -> DescribeCache
mkDescribeCacheInput p1 = DescribeCache
    { _dciGatewayARN = p1
    }
{-# INLINE mkDescribeCacheInput #-}

newtype DescribeCache = DescribeCache
    { _dciGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dciGatewayARN :: Lens' DescribeCache (Text)
dciGatewayARN = lens _dciGatewayARN (\s a -> s { _dciGatewayARN = a })
{-# INLINE dciGatewayARN #-}

instance ToPath DescribeCache

instance ToQuery DescribeCache

instance ToHeaders DescribeCache

instance ToJSON DescribeCache

data DescribeCacheResponse = DescribeCacheResponse
    { _dcoGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _dcoDiskIds :: [Text]
    , _dcoCacheAllocatedInBytes :: Maybe Integer
    , _dcoCacheUsedPercentage :: Maybe Double
    , _dcoCacheDirtyPercentage :: Maybe Double
    , _dcoCacheHitPercentage :: Maybe Double
    , _dcoCacheMissPercentage :: Maybe Double
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dcoGatewayARN :: Lens' DescribeCacheResponse (Maybe Text)
dcoGatewayARN = lens _dcoGatewayARN (\s a -> s { _dcoGatewayARN = a })
{-# INLINE dcoGatewayARN #-}

dcoDiskIds :: Lens' DescribeCacheResponse ([Text])
dcoDiskIds = lens _dcoDiskIds (\s a -> s { _dcoDiskIds = a })
{-# INLINE dcoDiskIds #-}

dcoCacheAllocatedInBytes :: Lens' DescribeCacheResponse (Maybe Integer)
dcoCacheAllocatedInBytes = lens _dcoCacheAllocatedInBytes (\s a -> s { _dcoCacheAllocatedInBytes = a })
{-# INLINE dcoCacheAllocatedInBytes #-}

dcoCacheUsedPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcoCacheUsedPercentage = lens _dcoCacheUsedPercentage (\s a -> s { _dcoCacheUsedPercentage = a })
{-# INLINE dcoCacheUsedPercentage #-}

dcoCacheDirtyPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcoCacheDirtyPercentage = lens _dcoCacheDirtyPercentage (\s a -> s { _dcoCacheDirtyPercentage = a })
{-# INLINE dcoCacheDirtyPercentage #-}

dcoCacheHitPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcoCacheHitPercentage = lens _dcoCacheHitPercentage (\s a -> s { _dcoCacheHitPercentage = a })
{-# INLINE dcoCacheHitPercentage #-}

dcoCacheMissPercentage :: Lens' DescribeCacheResponse (Maybe Double)
dcoCacheMissPercentage = lens _dcoCacheMissPercentage (\s a -> s { _dcoCacheMissPercentage = a })
{-# INLINE dcoCacheMissPercentage #-}

instance FromJSON DescribeCacheResponse

instance AWSRequest DescribeCache where
    type Sv DescribeCache = StorageGateway
    type Rs DescribeCache = DescribeCacheResponse

    request = get
    response _ = jsonResponse
