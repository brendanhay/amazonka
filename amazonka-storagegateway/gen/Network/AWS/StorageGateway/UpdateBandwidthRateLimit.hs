{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.UpdateBandwidthRateLimit
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
-- (ARN) of the gateway in your request.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateBandwidthRateLimit.html>
module Network.AWS.StorageGateway.UpdateBandwidthRateLimit
    (
    -- * Request
      UpdateBandwidthRateLimit
    -- ** Request constructor
    , updateBandwidthRateLimit
    -- ** Request lenses
    , ubrlAverageDownloadRateLimitInBitsPerSec
    , ubrlAverageUploadRateLimitInBitsPerSec
    , ubrlGatewayARN

    -- * Response
    , UpdateBandwidthRateLimitResponse
    -- ** Response constructor
    , updateBandwidthRateLimitResponse
    -- ** Response lenses
    , ubrlrGatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data UpdateBandwidthRateLimit = UpdateBandwidthRateLimit
    { _ubrlAverageDownloadRateLimitInBitsPerSec :: Maybe Nat
    , _ubrlAverageUploadRateLimitInBitsPerSec   :: Maybe Nat
    , _ubrlGatewayARN                           :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateBandwidthRateLimit' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ubrlAverageDownloadRateLimitInBitsPerSec' @::@ 'Maybe' 'Natural'
--
-- * 'ubrlAverageUploadRateLimitInBitsPerSec' @::@ 'Maybe' 'Natural'
--
-- * 'ubrlGatewayARN' @::@ 'Text'
--
updateBandwidthRateLimit :: Text -- ^ 'ubrlGatewayARN'
                         -> UpdateBandwidthRateLimit
updateBandwidthRateLimit p1 = UpdateBandwidthRateLimit
    { _ubrlGatewayARN                           = p1
    , _ubrlAverageUploadRateLimitInBitsPerSec   = Nothing
    , _ubrlAverageDownloadRateLimitInBitsPerSec = Nothing
    }

-- | The average download bandwidth rate limit in bits per second.
ubrlAverageDownloadRateLimitInBitsPerSec :: Lens' UpdateBandwidthRateLimit (Maybe Natural)
ubrlAverageDownloadRateLimitInBitsPerSec =
    lens _ubrlAverageDownloadRateLimitInBitsPerSec
        (\s a -> s { _ubrlAverageDownloadRateLimitInBitsPerSec = a })
            . mapping _Nat

-- | The average upload bandwidth rate limit in bits per second.
ubrlAverageUploadRateLimitInBitsPerSec :: Lens' UpdateBandwidthRateLimit (Maybe Natural)
ubrlAverageUploadRateLimitInBitsPerSec =
    lens _ubrlAverageUploadRateLimitInBitsPerSec
        (\s a -> s { _ubrlAverageUploadRateLimitInBitsPerSec = a })
            . mapping _Nat

ubrlGatewayARN :: Lens' UpdateBandwidthRateLimit Text
ubrlGatewayARN = lens _ubrlGatewayARN (\s a -> s { _ubrlGatewayARN = a })

newtype UpdateBandwidthRateLimitResponse = UpdateBandwidthRateLimitResponse
    { _ubrlrGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'UpdateBandwidthRateLimitResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ubrlrGatewayARN' @::@ 'Maybe' 'Text'
--
updateBandwidthRateLimitResponse :: UpdateBandwidthRateLimitResponse
updateBandwidthRateLimitResponse = UpdateBandwidthRateLimitResponse
    { _ubrlrGatewayARN = Nothing
    }

ubrlrGatewayARN :: Lens' UpdateBandwidthRateLimitResponse (Maybe Text)
ubrlrGatewayARN = lens _ubrlrGatewayARN (\s a -> s { _ubrlrGatewayARN = a })

instance ToPath UpdateBandwidthRateLimit where
    toPath = const "/"

instance ToQuery UpdateBandwidthRateLimit where
    toQuery = const mempty

instance ToHeaders UpdateBandwidthRateLimit
instance ToJSON UpdateBandwidthRateLimit where
    toJSON = genericToJSON jsonOptions

instance AWSRequest UpdateBandwidthRateLimit where
    type Sv UpdateBandwidthRateLimit = StorageGateway
    type Rs UpdateBandwidthRateLimit = UpdateBandwidthRateLimitResponse

    request  = post
    response = jsonResponse

instance FromJSON UpdateBandwidthRateLimitResponse where
    parseJSON = genericParseJSON jsonOptions
