{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.ListGateways
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation lists gateways owned by an AWS account in a region specified
-- in the request. The returned list is ordered by gateway Amazon Resource
-- Name (ARN). By default, the operation returns a maximum of 100 gateways.
-- This operation supports pagination that allows you to optionally reduce the
-- number of gateways returned in a response. If you have more gateways than
-- are returned in a response-that is, the response returns only a truncated
-- list of your gateways-the response contains a marker that you can specify
-- in your next request to fetch the next page of gateways. List Gateways The
-- following example does not specify any criteria for the returned list. Note
-- that the request body is "{}". The response returns gateways (or up to the
-- first 100) in the specified region owned by the AWS account. POST /
-- HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.ListGateways HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 178 {
-- "GatewayList": [ { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway2" } ] }.
module Network.AWS.StorageGateway.V2013_06_30.ListGateways where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'ListGateways' request.
listGateways :: ListGateways
listGateways = ListGateways
    { _lgiMarker = Nothing
    , _lgiLimit = Nothing
    }

data ListGateways = ListGateways
    { _lgiMarker :: Maybe Text
      -- ^ An opaque string that indicates the position at which to begin
      -- the returned list of gateways.
    , _lgiLimit :: Maybe Integer
      -- ^ Specifies that the list of gateways returned be limited to the
      -- specified number of items.
    } deriving (Show, Generic)

makeLenses ''ListGateways

instance ToPath ListGateways

instance ToQuery ListGateways

instance ToHeaders ListGateways

instance ToJSON ListGateways

data ListGatewaysResponse = ListGatewaysResponse
    { _lgoGateways :: [GatewayInformation]
    , _lgoMarker :: Maybe Text
    } deriving (Show, Generic)

makeLenses ''ListGatewaysResponse

instance FromJSON ListGatewaysResponse

instance AWSRequest ListGateways where
    type Sv ListGateways = StorageGateway
    type Rs ListGateways = ListGatewaysResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListGateways where
    next rq rs = (\x -> rq { _lgiMarker = Just x })
        <$> (_lgoMarker rs)
