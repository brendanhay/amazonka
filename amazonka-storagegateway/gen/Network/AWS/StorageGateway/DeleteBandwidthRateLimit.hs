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

-- Module      : Network.AWS.StorageGateway.DeleteBandwidthRateLimit
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation deletes the bandwidth rate limits of a gateway. You can
-- delete either the upload and download bandwidth rate limit, or you can
-- delete both. If you delete only one of the limits, the other limit remains
-- unchanged. To specify which gateway to work with, use the Amazon Resource
-- Name (ARN) of the gateway in your request.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteBandwidthRateLimit.html>
module Network.AWS.StorageGateway.DeleteBandwidthRateLimit
    (
    -- * Request
      DeleteBandwidthRateLimit
    -- ** Request constructor
    , deleteBandwidthRateLimit
    -- ** Request lenses
    , dbrl1BandwidthType
    , dbrl1GatewayARN

    -- * Response
    , DeleteBandwidthRateLimitResponse
    -- ** Response constructor
    , deleteBandwidthRateLimitResponse
    -- ** Response lenses
    , dbrlr1GatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data DeleteBandwidthRateLimit = DeleteBandwidthRateLimit
    { _dbrl1BandwidthType :: Text
    , _dbrl1GatewayARN    :: Text
    } deriving (Eq, Ord, Show)

-- | 'DeleteBandwidthRateLimit' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbrl1BandwidthType' @::@ 'Text'
--
-- * 'dbrl1GatewayARN' @::@ 'Text'
--
deleteBandwidthRateLimit :: Text -- ^ 'dbrl1GatewayARN'
                         -> Text -- ^ 'dbrl1BandwidthType'
                         -> DeleteBandwidthRateLimit
deleteBandwidthRateLimit p1 p2 = DeleteBandwidthRateLimit
    { _dbrl1GatewayARN    = p1
    , _dbrl1BandwidthType = p2
    }

dbrl1BandwidthType :: Lens' DeleteBandwidthRateLimit Text
dbrl1BandwidthType =
    lens _dbrl1BandwidthType (\s a -> s { _dbrl1BandwidthType = a })

dbrl1GatewayARN :: Lens' DeleteBandwidthRateLimit Text
dbrl1GatewayARN = lens _dbrl1GatewayARN (\s a -> s { _dbrl1GatewayARN = a })

newtype DeleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse
    { _dbrlr1GatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'DeleteBandwidthRateLimitResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbrlr1GatewayARN' @::@ 'Maybe' 'Text'
--
deleteBandwidthRateLimitResponse :: DeleteBandwidthRateLimitResponse
deleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse
    { _dbrlr1GatewayARN = Nothing
    }

dbrlr1GatewayARN :: Lens' DeleteBandwidthRateLimitResponse (Maybe Text)
dbrlr1GatewayARN = lens _dbrlr1GatewayARN (\s a -> s { _dbrlr1GatewayARN = a })

instance ToPath DeleteBandwidthRateLimit where
    toPath = const "/"

instance ToQuery DeleteBandwidthRateLimit where
    toQuery = const mempty

instance ToHeaders DeleteBandwidthRateLimit

instance ToJSON DeleteBandwidthRateLimit where
    toJSON DeleteBandwidthRateLimit{..} = object
        [ "GatewayARN"    .= _dbrl1GatewayARN
        , "BandwidthType" .= _dbrl1BandwidthType
        ]

instance AWSRequest DeleteBandwidthRateLimit where
    type Sv DeleteBandwidthRateLimit = StorageGateway
    type Rs DeleteBandwidthRateLimit = DeleteBandwidthRateLimitResponse

    request  = post "DeleteBandwidthRateLimit"
    response = jsonResponse

instance FromJSON DeleteBandwidthRateLimitResponse where
    parseJSON = withObject "DeleteBandwidthRateLimitResponse" $ \o -> DeleteBandwidthRateLimitResponse
        <$> o .:? "GatewayARN"


Some kind of operator / class to check the types whether to continue?
