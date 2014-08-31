{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.UpdateGatewayInformation
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
module Network.AWS.StorageGateway.V2013_06_30.UpdateGatewayInformation where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'UpdateGatewayInformation' request.
updateGatewayInformation :: Text -- ^ '_ugiiGatewayARN'
                         -> UpdateGatewayInformation
updateGatewayInformation p1 = UpdateGatewayInformation
    { _ugiiGatewayARN = p1
    , _ugiiGatewayName = Nothing
    , _ugiiGatewayTimezone = Nothing
    }

data UpdateGatewayInformation = UpdateGatewayInformation
    { _ugiiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _ugiiGatewayName :: Maybe Text
      -- ^ A unique identifier for your gateway. This name becomes part of
      -- the gateway Amazon Resources Name (ARN) which is what you use as
      -- an input to other operations.
    , _ugiiGatewayTimezone :: Maybe Text
    } deriving (Show, Generic)

makeLenses ''UpdateGatewayInformation

instance ToPath UpdateGatewayInformation

instance ToQuery UpdateGatewayInformation

instance ToHeaders UpdateGatewayInformation

instance ToJSON UpdateGatewayInformation

data UpdateGatewayInformationResponse = UpdateGatewayInformationResponse
    { _ugioGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

makeLenses ''UpdateGatewayInformationResponse

instance FromJSON UpdateGatewayInformationResponse

instance AWSRequest UpdateGatewayInformation where
    type Sv UpdateGatewayInformation = StorageGateway
    type Rs UpdateGatewayInformation = UpdateGatewayInformationResponse

    request = get
    response _ = jsonResponse
