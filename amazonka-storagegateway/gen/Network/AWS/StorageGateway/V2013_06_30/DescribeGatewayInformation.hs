{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DescribeGatewayInformation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns metadata about a gateway such as its name, network
-- interfaces, configured time zone, and the state (whether the gateway is
-- running or not). To specify which gateway to describe, use the Amazon
-- Resource Name (ARN) of the gateway in your request. Example Request The
-- following example shows a request for describing a gateway. POST / HTTP/1.1
-- Host: storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeGatewayInformation { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 227 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "GatewayId": "sgw-AABB1122", "GatewayNetworkInterfaces": [ {"Ipv4Address":
-- "10.35.69.216"} ], "GatewayState": "STATE_RUNNING", "GatewayTimezone":
-- "GMT-8:00" }.
module Network.AWS.StorageGateway.V2013_06_30.DescribeGatewayInformation
    (
    -- * Request
      DescribeGatewayInformation
    -- ** Request constructor
    , mkDescribeGatewayInformation
    -- ** Request lenses
    , dgiGatewayARN

    -- * Response
    , DescribeGatewayInformationResponse
    -- ** Response lenses
    , dgirsGatewayARN
    , dgirsGatewayId
    , dgirsGatewayTimezone
    , dgirsGatewayState
    , dgirsGatewayNetworkInterfaces
    , dgirsGatewayType
    , dgirsNextUpdateAvailabilityDate
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | A JSON object containing the of the gateway.
newtype DescribeGatewayInformation = DescribeGatewayInformation
    { _dgiGatewayARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeGatewayInformation' request.
mkDescribeGatewayInformation :: Text -- ^ 'dgiGatewayARN'
                             -> DescribeGatewayInformation
mkDescribeGatewayInformation p1 = DescribeGatewayInformation
    { _dgiGatewayARN = p1
    }
{-# INLINE mkDescribeGatewayInformation #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dgiGatewayARN :: Lens' DescribeGatewayInformation Text
dgiGatewayARN = lens _dgiGatewayARN (\s a -> s { _dgiGatewayARN = a })
{-# INLINE dgiGatewayARN #-}

instance ToPath DescribeGatewayInformation

instance ToQuery DescribeGatewayInformation

instance ToHeaders DescribeGatewayInformation

instance ToJSON DescribeGatewayInformation

-- | A JSON object containing the following fields:.
data DescribeGatewayInformationResponse = DescribeGatewayInformationResponse
    { _dgirsGatewayARN :: Maybe Text
    , _dgirsGatewayId :: Maybe Text
    , _dgirsGatewayTimezone :: Maybe Text
    , _dgirsGatewayState :: Maybe Text
    , _dgirsGatewayNetworkInterfaces :: [NetworkInterface]
    , _dgirsGatewayType :: Maybe Text
    , _dgirsNextUpdateAvailabilityDate :: Maybe Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dgirsGatewayARN :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayARN = lens _dgirsGatewayARN (\s a -> s { _dgirsGatewayARN = a })
{-# INLINE dgirsGatewayARN #-}

-- | The gateway ID.
dgirsGatewayId :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayId = lens _dgirsGatewayId (\s a -> s { _dgirsGatewayId = a })
{-# INLINE dgirsGatewayId #-}

-- | One of the values that indicates the time zone configured for the gateway.
dgirsGatewayTimezone :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayTimezone =
    lens _dgirsGatewayTimezone (\s a -> s { _dgirsGatewayTimezone = a })
{-# INLINE dgirsGatewayTimezone #-}

-- | One of the values that indicates the operating state of the gateway.
dgirsGatewayState :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayState =
    lens _dgirsGatewayState (\s a -> s { _dgirsGatewayState = a })
{-# INLINE dgirsGatewayState #-}

-- | A NetworkInterface array that contains descriptions of the gateway network
-- interfaces.
dgirsGatewayNetworkInterfaces :: Lens' DescribeGatewayInformationResponse [NetworkInterface]
dgirsGatewayNetworkInterfaces =
    lens _dgirsGatewayNetworkInterfaces
         (\s a -> s { _dgirsGatewayNetworkInterfaces = a })
{-# INLINE dgirsGatewayNetworkInterfaces #-}

-- | TBD.
dgirsGatewayType :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsGatewayType =
    lens _dgirsGatewayType (\s a -> s { _dgirsGatewayType = a })
{-# INLINE dgirsGatewayType #-}

-- | The date at which an update to the gateway is available. This date is in
-- the time zone of the gateway. If the gateway is not available for an update
-- this field is not returned in the response. response example. -->.
dgirsNextUpdateAvailabilityDate :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirsNextUpdateAvailabilityDate =
    lens _dgirsNextUpdateAvailabilityDate
         (\s a -> s { _dgirsNextUpdateAvailabilityDate = a })
{-# INLINE dgirsNextUpdateAvailabilityDate #-}

instance FromJSON DescribeGatewayInformationResponse

instance AWSRequest DescribeGatewayInformation where
    type Sv DescribeGatewayInformation = StorageGateway
    type Rs DescribeGatewayInformation = DescribeGatewayInformationResponse

    request = get
    response _ = jsonResponse
