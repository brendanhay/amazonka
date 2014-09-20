{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.UpdateGatewayInformation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates a gateway's metadata, which includes the gateway's
-- name and time zone. To specify which gateway to update, use the Amazon
-- Resource Name (ARN) of the gateway in your request. Example Request The
-- following example shows a request that updates the name of a gateway. POST
-- / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateGatewayInformation { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "GatewayName" "mygateway2" } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 81 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway2" }.
module Network.AWS.StorageGateway.UpdateGatewayInformation
    (
    -- * Request
      UpdateGatewayInformation
    -- ** Request constructor
    , updateGatewayInformation
    -- ** Request lenses
    , ugiGatewayARN
    , ugiGatewayName
    , ugiGatewayTimezone

    -- * Response
    , UpdateGatewayInformationResponse
    -- ** Response constructor
    , updateGatewayInformationResponse
    -- ** Response lenses
    , ugirGatewayARN
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data UpdateGatewayInformation = UpdateGatewayInformation
    { _ugiGatewayARN :: Text
    , _ugiGatewayName :: Maybe Text
    , _ugiGatewayTimezone :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateGatewayInformation' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
-- * @GatewayName ::@ @Maybe Text@
--
-- * @GatewayTimezone ::@ @Maybe Text@
--
updateGatewayInformation :: Text -- ^ 'ugiGatewayARN'
                         -> UpdateGatewayInformation
updateGatewayInformation p1 = UpdateGatewayInformation
    { _ugiGatewayARN = p1
    , _ugiGatewayName = Nothing
    , _ugiGatewayTimezone = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
ugiGatewayARN :: Lens' UpdateGatewayInformation Text
ugiGatewayARN = lens _ugiGatewayARN (\s a -> s { _ugiGatewayARN = a })

-- | A unique identifier for your gateway. This name becomes part of the gateway
-- Amazon Resources Name (ARN) which is what you use as an input to other
-- operations.
ugiGatewayName :: Lens' UpdateGatewayInformation (Maybe Text)
ugiGatewayName = lens _ugiGatewayName (\s a -> s { _ugiGatewayName = a })

ugiGatewayTimezone :: Lens' UpdateGatewayInformation (Maybe Text)
ugiGatewayTimezone =
    lens _ugiGatewayTimezone (\s a -> s { _ugiGatewayTimezone = a })

instance ToPath UpdateGatewayInformation

instance ToQuery UpdateGatewayInformation

instance ToHeaders UpdateGatewayInformation

instance ToJSON UpdateGatewayInformation

-- | A JSON object containing the of the gateway that was updated.
newtype UpdateGatewayInformationResponse = UpdateGatewayInformationResponse
    { _ugirGatewayARN :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateGatewayInformationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Maybe Text@
--
updateGatewayInformationResponse :: UpdateGatewayInformationResponse
updateGatewayInformationResponse = UpdateGatewayInformationResponse
    { _ugirGatewayARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
ugirGatewayARN :: Lens' UpdateGatewayInformationResponse (Maybe Text)
ugirGatewayARN = lens _ugirGatewayARN (\s a -> s { _ugirGatewayARN = a })

instance FromJSON UpdateGatewayInformationResponse

instance AWSRequest UpdateGatewayInformation where
    type Sv UpdateGatewayInformation = StorageGateway
    type Rs UpdateGatewayInformation = UpdateGatewayInformationResponse

    request = get
    response _ = jsonResponse
