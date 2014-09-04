{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DescribeBandwidthRateLimit
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns the bandwidth rate limits of a gateway. By default,
-- these limits are not set, which means no bandwidth rate limiting is in
-- effect. This operation only returns a value for a bandwidth rate limit only
-- if the limit is set. If no limits are set for the gateway, then this
-- operation returns only the gateway ARN in the response body. To specify
-- which gateway to describe, use the Amazon Resource Name (ARN) of the
-- gateway in your request. Example Request The following example shows a
-- request that returns the bandwidth throttle properties of a gateway. POST /
-- HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeBandwidthRateLimit { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygate way" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 169 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "AverageUploadRateLimitInBitsPerSec": 102400,
-- "AverageDownloadRateLimitInBitsPerSec": 51200 }.
module Network.AWS.StorageGateway.V2013_06_30.DescribeBandwidthRateLimit
    (
    -- * Request
      DescribeBandwidthRateLimit
    -- ** Request constructor
    , describeBandwidthRateLimit
    -- ** Request lenses
    , dbrljGatewayARN

    -- * Response
    , DescribeBandwidthRateLimitResponse
    -- ** Response lenses
    , dbrlpAverageDownloadRateLimitInBitsPerSec
    , dbrlpAverageUploadRateLimitInBitsPerSec
    , dbrlpGatewayARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeBandwidthRateLimit' request.
describeBandwidthRateLimit :: Text -- ^ 'dbrljGatewayARN'
                           -> DescribeBandwidthRateLimit
describeBandwidthRateLimit p1 = DescribeBandwidthRateLimit
    { _dbrljGatewayARN = p1
    }
{-# INLINE describeBandwidthRateLimit #-}

data DescribeBandwidthRateLimit = DescribeBandwidthRateLimit
    { _dbrljGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dbrljGatewayARN :: Lens' DescribeBandwidthRateLimit (Text)
dbrljGatewayARN f x =
    f (_dbrljGatewayARN x)
        <&> \y -> x { _dbrljGatewayARN = y }
{-# INLINE dbrljGatewayARN #-}

instance ToPath DescribeBandwidthRateLimit

instance ToQuery DescribeBandwidthRateLimit

instance ToHeaders DescribeBandwidthRateLimit

instance ToJSON DescribeBandwidthRateLimit

data DescribeBandwidthRateLimitResponse = DescribeBandwidthRateLimitResponse
    { _dbrlpAverageDownloadRateLimitInBitsPerSec :: Maybe Integer
      -- ^ The average download bandwidth rate limit in bits per second.
      -- This field does not appear in the response if the download rate
      -- limit is not set.
    , _dbrlpAverageUploadRateLimitInBitsPerSec :: Maybe Integer
      -- ^ The average upload bandwidth rate limit in bits per second. This
      -- field does not appear in the response if the upload rate limit is
      -- not set.
    , _dbrlpGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The average download bandwidth rate limit in bits per second. This field
-- does not appear in the response if the download rate limit is not set.
dbrlpAverageDownloadRateLimitInBitsPerSec :: Lens' DescribeBandwidthRateLimitResponse (Maybe Integer)
dbrlpAverageDownloadRateLimitInBitsPerSec f x =
    f (_dbrlpAverageDownloadRateLimitInBitsPerSec x)
        <&> \y -> x { _dbrlpAverageDownloadRateLimitInBitsPerSec = y }
{-# INLINE dbrlpAverageDownloadRateLimitInBitsPerSec #-}

-- | The average upload bandwidth rate limit in bits per second. This field does
-- not appear in the response if the upload rate limit is not set.
dbrlpAverageUploadRateLimitInBitsPerSec :: Lens' DescribeBandwidthRateLimitResponse (Maybe Integer)
dbrlpAverageUploadRateLimitInBitsPerSec f x =
    f (_dbrlpAverageUploadRateLimitInBitsPerSec x)
        <&> \y -> x { _dbrlpAverageUploadRateLimitInBitsPerSec = y }
{-# INLINE dbrlpAverageUploadRateLimitInBitsPerSec #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dbrlpGatewayARN :: Lens' DescribeBandwidthRateLimitResponse (Maybe Text)
dbrlpGatewayARN f x =
    f (_dbrlpGatewayARN x)
        <&> \y -> x { _dbrlpGatewayARN = y }
{-# INLINE dbrlpGatewayARN #-}

instance FromJSON DescribeBandwidthRateLimitResponse

instance AWSRequest DescribeBandwidthRateLimit where
    type Sv DescribeBandwidthRateLimit = StorageGateway
    type Rs DescribeBandwidthRateLimit = DescribeBandwidthRateLimitResponse

    request = get
    response _ = jsonResponse
