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
    , mkDescribeGatewayInformationInput
    -- ** Request lenses
    , dgiiGatewayARN

    -- * Response
    , DescribeGatewayInformationResponse
    -- ** Response lenses
    , dgioGatewayARN
    , dgioGatewayId
    , dgioGatewayTimezone
    , dgioGatewayState
    , dgioGatewayNetworkInterfaces
    , dgioGatewayType
    , dgioNextUpdateAvailabilityDate
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeGatewayInformation' request.
mkDescribeGatewayInformationInput :: Text -- ^ 'dgiiGatewayARN'
                                  -> DescribeGatewayInformation
mkDescribeGatewayInformationInput p1 = DescribeGatewayInformation
    { _dgiiGatewayARN = p1
    }
{-# INLINE mkDescribeGatewayInformationInput #-}

newtype DescribeGatewayInformation = DescribeGatewayInformation
    { _dgiiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dgiiGatewayARN :: Lens' DescribeGatewayInformation (Text)
dgiiGatewayARN = lens _dgiiGatewayARN (\s a -> s { _dgiiGatewayARN = a })
{-# INLINE dgiiGatewayARN #-}

instance ToPath DescribeGatewayInformation

instance ToQuery DescribeGatewayInformation

instance ToHeaders DescribeGatewayInformation

instance ToJSON DescribeGatewayInformation

data DescribeGatewayInformationResponse = DescribeGatewayInformationResponse
    { _dgioGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _dgioGatewayId :: Maybe Text
      -- ^ The gateway ID.
    , _dgioGatewayTimezone :: Maybe Text
      -- ^ One of the values that indicates the time zone configured for the
      -- gateway.
    , _dgioGatewayState :: Maybe Text
      -- ^ One of the values that indicates the operating state of the
      -- gateway.
    , _dgioGatewayNetworkInterfaces :: [NetworkInterface]
      -- ^ A NetworkInterface array that contains descriptions of the
      -- gateway network interfaces.
    , _dgioGatewayType :: Maybe Text
      -- ^ TBD.
    , _dgioNextUpdateAvailabilityDate :: Maybe Text
      -- ^ The date at which an update to the gateway is available. This
      -- date is in the time zone of the gateway. If the gateway is not
      -- available for an update this field is not returned in the
      -- response. response example. -->.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dgioGatewayARN :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgioGatewayARN = lens _dgioGatewayARN (\s a -> s { _dgioGatewayARN = a })
{-# INLINE dgioGatewayARN #-}

-- | The gateway ID.
dgioGatewayId :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgioGatewayId = lens _dgioGatewayId (\s a -> s { _dgioGatewayId = a })
{-# INLINE dgioGatewayId #-}

-- | One of the values that indicates the time zone configured for the gateway.
dgioGatewayTimezone :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgioGatewayTimezone = lens _dgioGatewayTimezone (\s a -> s { _dgioGatewayTimezone = a })
{-# INLINE dgioGatewayTimezone #-}

-- | One of the values that indicates the operating state of the gateway.
dgioGatewayState :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgioGatewayState = lens _dgioGatewayState (\s a -> s { _dgioGatewayState = a })
{-# INLINE dgioGatewayState #-}

-- | A NetworkInterface array that contains descriptions of the gateway network
-- interfaces.
dgioGatewayNetworkInterfaces :: Lens' DescribeGatewayInformationResponse ([NetworkInterface])
dgioGatewayNetworkInterfaces = lens _dgioGatewayNetworkInterfaces (\s a -> s { _dgioGatewayNetworkInterfaces = a })
{-# INLINE dgioGatewayNetworkInterfaces #-}

-- | TBD.
dgioGatewayType :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgioGatewayType = lens _dgioGatewayType (\s a -> s { _dgioGatewayType = a })
{-# INLINE dgioGatewayType #-}

-- | The date at which an update to the gateway is available. This date is in
-- the time zone of the gateway. If the gateway is not available for an update
-- this field is not returned in the response. response example. -->.
dgioNextUpdateAvailabilityDate :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgioNextUpdateAvailabilityDate = lens _dgioNextUpdateAvailabilityDate (\s a -> s { _dgioNextUpdateAvailabilityDate = a })
{-# INLINE dgioNextUpdateAvailabilityDate #-}

instance FromJSON DescribeGatewayInformationResponse

instance AWSRequest DescribeGatewayInformation where
    type Sv DescribeGatewayInformation = StorageGateway
    type Rs DescribeGatewayInformation = DescribeGatewayInformationResponse

    request = get
    response _ = jsonResponse
