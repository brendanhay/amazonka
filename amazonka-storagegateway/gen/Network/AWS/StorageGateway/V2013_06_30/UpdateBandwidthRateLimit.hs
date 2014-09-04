{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.UpdateBandwidthRateLimit
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates the bandwidth rate limits of a gateway. You can
-- update both the upload and download bandwidth rate limit or specify only
-- one of the two. If you don't set a bandwidth rate limit, the existing rate
-- limit remains. By default, a gateway's bandwidth rate limits are not set.
-- If you don't set any limit, the gateway does not have any limitations on
-- its bandwidth usage and could potentially use the maximum available
-- bandwidth. To specify which gateway to update, use the Amazon Resource Name
-- (ARN) of the gateway in your request. Example Request The following example
-- shows a request that returns the bandwidth throttle properties of a
-- gateway. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- x-amz-Date: 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateBandwidthRateLimit { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "AverageUploadRateLimitInBitsPerSec": 51200,
-- "AverageDownloadRateLimitInBitsPerSec": 102400 } HTTP/1.1 200 OK
-- x-amzn-RequestId: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Date: Wed, 25 Apr 2012 12:00:02 GMT Content-type:
-- application/x-amz-json-1.1 Content-length: 80 { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
module Network.AWS.StorageGateway.V2013_06_30.UpdateBandwidthRateLimit
    (
    -- * Request
      UpdateBandwidthRateLimit
    -- ** Request constructor
    , updateBandwidthRateLimit
    -- ** Request lenses
    , ubrliGatewayARN
    , ubrliAverageDownloadRateLimitInBitsPerSec
    , ubrliAverageUploadRateLimitInBitsPerSec

    -- * Response
    , UpdateBandwidthRateLimitResponse
    -- ** Response lenses
    , ubrloGatewayARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'UpdateBandwidthRateLimit' request.
updateBandwidthRateLimit :: Text -- ^ 'ubrliGatewayARN'
                         -> UpdateBandwidthRateLimit
updateBandwidthRateLimit p1 = UpdateBandwidthRateLimit
    { _ubrliGatewayARN = p1
    , _ubrliAverageDownloadRateLimitInBitsPerSec = Nothing
    , _ubrliAverageUploadRateLimitInBitsPerSec = Nothing
    }
{-# INLINE updateBandwidthRateLimit #-}

data UpdateBandwidthRateLimit = UpdateBandwidthRateLimit
    { _ubrliGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _ubrliAverageDownloadRateLimitInBitsPerSec :: Maybe Integer
      -- ^ The average download bandwidth rate limit in bits per second.
    , _ubrliAverageUploadRateLimitInBitsPerSec :: Maybe Integer
      -- ^ The average upload bandwidth rate limit in bits per second.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
ubrliGatewayARN :: Lens' UpdateBandwidthRateLimit (Text)
ubrliGatewayARN f x =
    f (_ubrliGatewayARN x)
        <&> \y -> x { _ubrliGatewayARN = y }
{-# INLINE ubrliGatewayARN #-}

-- | The average download bandwidth rate limit in bits per second.
ubrliAverageDownloadRateLimitInBitsPerSec :: Lens' UpdateBandwidthRateLimit (Maybe Integer)
ubrliAverageDownloadRateLimitInBitsPerSec f x =
    f (_ubrliAverageDownloadRateLimitInBitsPerSec x)
        <&> \y -> x { _ubrliAverageDownloadRateLimitInBitsPerSec = y }
{-# INLINE ubrliAverageDownloadRateLimitInBitsPerSec #-}

-- | The average upload bandwidth rate limit in bits per second.
ubrliAverageUploadRateLimitInBitsPerSec :: Lens' UpdateBandwidthRateLimit (Maybe Integer)
ubrliAverageUploadRateLimitInBitsPerSec f x =
    f (_ubrliAverageUploadRateLimitInBitsPerSec x)
        <&> \y -> x { _ubrliAverageUploadRateLimitInBitsPerSec = y }
{-# INLINE ubrliAverageUploadRateLimitInBitsPerSec #-}

instance ToPath UpdateBandwidthRateLimit

instance ToQuery UpdateBandwidthRateLimit

instance ToHeaders UpdateBandwidthRateLimit

instance ToJSON UpdateBandwidthRateLimit

data UpdateBandwidthRateLimitResponse = UpdateBandwidthRateLimitResponse
    { _ubrloGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
ubrloGatewayARN :: Lens' UpdateBandwidthRateLimitResponse (Maybe Text)
ubrloGatewayARN f x =
    f (_ubrloGatewayARN x)
        <&> \y -> x { _ubrloGatewayARN = y }
{-# INLINE ubrloGatewayARN #-}

instance FromJSON UpdateBandwidthRateLimitResponse

instance AWSRequest UpdateBandwidthRateLimit where
    type Sv UpdateBandwidthRateLimit = StorageGateway
    type Rs UpdateBandwidthRateLimit = UpdateBandwidthRateLimitResponse

    request = get
    response _ = jsonResponse
