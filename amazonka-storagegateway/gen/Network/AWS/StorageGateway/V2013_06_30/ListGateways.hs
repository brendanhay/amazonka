{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.StorageGateway.V2013_06_30.ListGateways
    (
    -- * Request
      ListGateways
    -- ** Request constructor
    , mkListGateways
    -- ** Request lenses
    , lgMarker
    , lgLimit

    -- * Response
    , ListGatewaysResponse
    -- ** Response lenses
    , lgrsGateways
    , lgrsMarker
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | A JSON object containing zero or more of the following fields:
-- ListGatewaysInput$Limit ListGatewaysInput$Marker.
data ListGateways = ListGateways
    { _lgMarker :: Maybe Text
    , _lgLimit :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListGateways' request.
mkListGateways :: ListGateways
mkListGateways = ListGateways
    { _lgMarker = Nothing
    , _lgLimit = Nothing
    }

-- | An opaque string that indicates the position at which to begin the returned
-- list of gateways.
lgMarker :: Lens' ListGateways (Maybe Text)
lgMarker = lens _lgMarker (\s a -> s { _lgMarker = a })

-- | Specifies that the list of gateways returned be limited to the specified
-- number of items.
lgLimit :: Lens' ListGateways (Maybe Integer)
lgLimit = lens _lgLimit (\s a -> s { _lgLimit = a })

instance ToPath ListGateways

instance ToQuery ListGateways

instance ToHeaders ListGateways

instance ToJSON ListGateways

data ListGatewaysResponse = ListGatewaysResponse
    { _lgrsGateways :: [GatewayInformation]
    , _lgrsMarker :: Maybe Text
    } deriving (Show, Generic)

lgrsGateways :: Lens' ListGatewaysResponse [GatewayInformation]
lgrsGateways = lens _lgrsGateways (\s a -> s { _lgrsGateways = a })

lgrsMarker :: Lens' ListGatewaysResponse (Maybe Text)
lgrsMarker = lens _lgrsMarker (\s a -> s { _lgrsMarker = a })

instance FromJSON ListGatewaysResponse

instance AWSRequest ListGateways where
    type Sv ListGateways = StorageGateway
    type Rs ListGateways = ListGatewaysResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListGateways where
    next rq rs = (\x -> rq & lgMarker ?~ x) <$> (rs ^. lgrsMarker)

