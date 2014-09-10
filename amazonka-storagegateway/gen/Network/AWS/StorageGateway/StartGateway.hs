{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.StartGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation starts a gateway that you previously shut down (see
-- ShutdownGateway). After the gateway starts, you can then make other API
-- calls, your applications can read from or write to the gateway's storage
-- volumes and you will be able to take snapshot backups. When you make a
-- request, you will get a 200 OK success response immediately. However, it
-- might take some time for the gateway to be ready. You should call
-- DescribeGatewayInformation and check the status before making any
-- additional API calls. For more information, see ActivateGateway. To specify
-- which gateway to start, use the Amazon Resource Name (ARN) of the gateway
-- in your request. Example Request The following example shows a request that
-- starts a gateway. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.StartGateway { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 80 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
module Network.AWS.StorageGateway.StartGateway
    (
    -- * Request
      StartGateway
    -- ** Request constructor
    , mkStartGateway
    -- ** Request lenses
    , sg1GatewayARN

    -- * Response
    , StartGatewayResponse
    -- ** Response constructor
    , mkStartGatewayResponse
    -- ** Response lenses
    , sgrrGatewayARN
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing the of the gateway to start.
newtype StartGateway = StartGateway
    { _sg1GatewayARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StartGateway' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
mkStartGateway :: Text -- ^ 'sg1GatewayARN'
               -> StartGateway
mkStartGateway p1 = StartGateway
    { _sg1GatewayARN = p1
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
sg1GatewayARN :: Lens' StartGateway Text
sg1GatewayARN = lens _sg1GatewayARN (\s a -> s { _sg1GatewayARN = a })

instance ToPath StartGateway

instance ToQuery StartGateway

instance ToHeaders StartGateway

instance ToJSON StartGateway

-- | A JSON object containing the of the gateway that was restarted.
newtype StartGatewayResponse = StartGatewayResponse
    { _sgrrGatewayARN :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StartGatewayResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Maybe Text@
--
mkStartGatewayResponse :: StartGatewayResponse
mkStartGatewayResponse = StartGatewayResponse
    { _sgrrGatewayARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
sgrrGatewayARN :: Lens' StartGatewayResponse (Maybe Text)
sgrrGatewayARN = lens _sgrrGatewayARN (\s a -> s { _sgrrGatewayARN = a })

instance FromJSON StartGatewayResponse

instance AWSRequest StartGateway where
    type Sv StartGateway = StorageGateway
    type Rs StartGateway = StartGatewayResponse

    request = get
    response _ = jsonResponse
