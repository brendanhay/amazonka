{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeBandwidthRateLimit
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns the bandwidth rate limits of a gateway. By default,
-- these limits are not set, which means no bandwidth rate limiting is in effect.
--
-- This operation only returns a value for a bandwidth rate limit only if the
-- limit is set. If no limits are set for the gateway, then this operation
-- returns only the gateway ARN in the response body. To specify which gateway
-- to describe, use the Amazon Resource Name (ARN) of the gateway in your
-- request.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeBandwidthRateLimit.html>
module Network.AWS.StorageGateway.DescribeBandwidthRateLimit
    (
    -- * Request
      DescribeBandwidthRateLimit
    -- ** Request constructor
    , describeBandwidthRateLimit
    -- ** Request lenses
    , dbrlGatewayARN

    -- * Response
    , DescribeBandwidthRateLimitResponse
    -- ** Response constructor
    , describeBandwidthRateLimitResponse
    -- ** Response lenses
    , dbrlrAverageDownloadRateLimitInBitsPerSec
    , dbrlrAverageUploadRateLimitInBitsPerSec
    , dbrlrGatewayARN
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype DescribeBandwidthRateLimit = DescribeBandwidthRateLimit
    { _dbrlGatewayARN :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DescribeBandwidthRateLimit' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbrlGatewayARN' @::@ 'Text'
--
describeBandwidthRateLimit :: Text -- ^ 'dbrlGatewayARN'
                           -> DescribeBandwidthRateLimit
describeBandwidthRateLimit p1 = DescribeBandwidthRateLimit
    { _dbrlGatewayARN = p1
    }

dbrlGatewayARN :: Lens' DescribeBandwidthRateLimit Text
dbrlGatewayARN = lens _dbrlGatewayARN (\s a -> s { _dbrlGatewayARN = a })

data DescribeBandwidthRateLimitResponse = DescribeBandwidthRateLimitResponse
    { _dbrlrAverageDownloadRateLimitInBitsPerSec :: Maybe Nat
    , _dbrlrAverageUploadRateLimitInBitsPerSec   :: Maybe Nat
    , _dbrlrGatewayARN                           :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeBandwidthRateLimitResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbrlrAverageDownloadRateLimitInBitsPerSec' @::@ 'Maybe' 'Natural'
--
-- * 'dbrlrAverageUploadRateLimitInBitsPerSec' @::@ 'Maybe' 'Natural'
--
-- * 'dbrlrGatewayARN' @::@ 'Maybe' 'Text'
--
describeBandwidthRateLimitResponse :: DescribeBandwidthRateLimitResponse
describeBandwidthRateLimitResponse = DescribeBandwidthRateLimitResponse
    { _dbrlrGatewayARN                           = Nothing
    , _dbrlrAverageUploadRateLimitInBitsPerSec   = Nothing
    , _dbrlrAverageDownloadRateLimitInBitsPerSec = Nothing
    }

-- | The average download bandwidth rate limit in bits per second. This field does
-- not appear in the response if the download rate limit is not set.
dbrlrAverageDownloadRateLimitInBitsPerSec :: Lens' DescribeBandwidthRateLimitResponse (Maybe Natural)
dbrlrAverageDownloadRateLimitInBitsPerSec =
    lens _dbrlrAverageDownloadRateLimitInBitsPerSec
        (\s a -> s { _dbrlrAverageDownloadRateLimitInBitsPerSec = a })
            . mapping _Nat

-- | The average upload bandwidth rate limit in bits per second. This field does
-- not appear in the response if the upload rate limit is not set.
dbrlrAverageUploadRateLimitInBitsPerSec :: Lens' DescribeBandwidthRateLimitResponse (Maybe Natural)
dbrlrAverageUploadRateLimitInBitsPerSec =
    lens _dbrlrAverageUploadRateLimitInBitsPerSec
        (\s a -> s { _dbrlrAverageUploadRateLimitInBitsPerSec = a })
            . mapping _Nat

dbrlrGatewayARN :: Lens' DescribeBandwidthRateLimitResponse (Maybe Text)
dbrlrGatewayARN = lens _dbrlrGatewayARN (\s a -> s { _dbrlrGatewayARN = a })

instance ToPath DescribeBandwidthRateLimit where
    toPath = const "/"

instance ToQuery DescribeBandwidthRateLimit where
    toQuery = const mempty

instance ToHeaders DescribeBandwidthRateLimit

instance ToJSON DescribeBandwidthRateLimit where
    toJSON DescribeBandwidthRateLimit{..} = object
        [ "GatewayARN" .= _dbrlGatewayARN
        ]

instance AWSRequest DescribeBandwidthRateLimit where
    type Sv DescribeBandwidthRateLimit = StorageGateway
    type Rs DescribeBandwidthRateLimit = DescribeBandwidthRateLimitResponse

    request  = post "DescribeBandwidthRateLimit"
    response = jsonResponse

instance FromJSON DescribeBandwidthRateLimitResponse where
    parseJSON = withObject "DescribeBandwidthRateLimitResponse" $ \o -> DescribeBandwidthRateLimitResponse
        <$> o .:? "AverageDownloadRateLimitInBitsPerSec"
        <*> o .:? "AverageUploadRateLimitInBitsPerSec"
        <*> o .:? "GatewayARN"
