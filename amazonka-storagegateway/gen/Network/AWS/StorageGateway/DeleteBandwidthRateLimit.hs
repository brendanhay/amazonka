{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- Name (ARN) of the gateway in your request. Example Request The following
-- example shows a request that deletes both of the bandwidth rate limits of a
-- gateway. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- x-amz-Date: 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DeleteBandwidthRateLimit { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "BandwidthType: "All" } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 80 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
module Network.AWS.StorageGateway.DeleteBandwidthRateLimit
    (
    -- * Request
      DeleteBandwidthRateLimit
    -- ** Request constructor
    , mkDeleteBandwidthRateLimit
    -- ** Request lenses
    , dbrlGatewayARN
    , dbrlBandwidthType

    -- * Response
    , DeleteBandwidthRateLimitResponse
    -- ** Response constructor
    , mkDeleteBandwidthRateLimitResponse
    -- ** Response lenses
    , dbrlrGatewayARN
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DeleteBandwidthRateLimit = DeleteBandwidthRateLimit
    { _dbrlGatewayARN :: Text
    , _dbrlBandwidthType :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBandwidthRateLimit' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
-- * @BandwidthType ::@ @Text@
--
mkDeleteBandwidthRateLimit :: Text -- ^ 'dbrlGatewayARN'
                           -> Text -- ^ 'dbrlBandwidthType'
                           -> DeleteBandwidthRateLimit
mkDeleteBandwidthRateLimit p1 p2 = DeleteBandwidthRateLimit
    { _dbrlGatewayARN = p1
    , _dbrlBandwidthType = p2
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dbrlGatewayARN :: Lens' DeleteBandwidthRateLimit Text
dbrlGatewayARN = lens _dbrlGatewayARN (\s a -> s { _dbrlGatewayARN = a })

dbrlBandwidthType :: Lens' DeleteBandwidthRateLimit Text
dbrlBandwidthType =
    lens _dbrlBandwidthType (\s a -> s { _dbrlBandwidthType = a })

instance ToPath DeleteBandwidthRateLimit

instance ToQuery DeleteBandwidthRateLimit

instance ToHeaders DeleteBandwidthRateLimit

instance ToJSON DeleteBandwidthRateLimit

-- | A JSON object containing the of the gateway whose bandwidth rate
-- information was deleted.
newtype DeleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse
    { _dbrlrGatewayARN :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBandwidthRateLimitResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Maybe Text@
--
mkDeleteBandwidthRateLimitResponse :: DeleteBandwidthRateLimitResponse
mkDeleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse
    { _dbrlrGatewayARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dbrlrGatewayARN :: Lens' DeleteBandwidthRateLimitResponse (Maybe Text)
dbrlrGatewayARN = lens _dbrlrGatewayARN (\s a -> s { _dbrlrGatewayARN = a })

instance FromJSON DeleteBandwidthRateLimitResponse

instance AWSRequest DeleteBandwidthRateLimit where
    type Sv DeleteBandwidthRateLimit = StorageGateway
    type Rs DeleteBandwidthRateLimit = DeleteBandwidthRateLimitResponse

    request = get
    response _ = jsonResponse
