{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeGatewayInformation
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
module Network.AWS.StorageGateway
    (
    -- * Request
      DescribeGatewayInformation
    -- ** Request constructor
    , mkDescribeGatewayInformation
    -- ** Request lenses
    , dgiGatewayARN

    -- * Response
    , DescribeGatewayInformationResponse
    -- ** Response constructor
    , mkDescribeGatewayInformationResponse
    -- ** Response lenses
    , dgirGatewayARN
    , dgirGatewayId
    , dgirGatewayTimezone
    , dgirGatewayState
    , dgirGatewayNetworkInterfaces
    , dgirGatewayType
    , dgirNextUpdateAvailabilityDate
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing the of the gateway.
newtype DescribeGatewayInformation = DescribeGatewayInformation
    { _dgiGatewayARN :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeGatewayInformation' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
mkDescribeGatewayInformation :: Text -- ^ 'dgiGatewayARN'
                             -> DescribeGatewayInformation
mkDescribeGatewayInformation p1 = DescribeGatewayInformation
    { _dgiGatewayARN = p1
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dgiGatewayARN :: Lens' DescribeGatewayInformation Text
dgiGatewayARN = lens _dgiGatewayARN (\s a -> s { _dgiGatewayARN = a })

instance ToPath DescribeGatewayInformation

instance ToQuery DescribeGatewayInformation

instance ToHeaders DescribeGatewayInformation

instance ToJSON DescribeGatewayInformation

-- | A JSON object containing the following fields:.
data DescribeGatewayInformationResponse = DescribeGatewayInformationResponse
    { _dgirGatewayARN :: !(Maybe Text)
    , _dgirGatewayId :: !(Maybe Text)
    , _dgirGatewayTimezone :: !(Maybe Text)
    , _dgirGatewayState :: !(Maybe Text)
    , _dgirGatewayNetworkInterfaces :: [NetworkInterface]
    , _dgirGatewayType :: !(Maybe Text)
    , _dgirNextUpdateAvailabilityDate :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeGatewayInformationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Maybe Text@
--
-- * @GatewayId ::@ @Maybe Text@
--
-- * @GatewayTimezone ::@ @Maybe Text@
--
-- * @GatewayState ::@ @Maybe Text@
--
-- * @GatewayNetworkInterfaces ::@ @[NetworkInterface]@
--
-- * @GatewayType ::@ @Maybe Text@
--
-- * @NextUpdateAvailabilityDate ::@ @Maybe Text@
--
mkDescribeGatewayInformationResponse :: DescribeGatewayInformationResponse
mkDescribeGatewayInformationResponse = DescribeGatewayInformationResponse
    { _dgirGatewayARN = Nothing
    , _dgirGatewayId = Nothing
    , _dgirGatewayTimezone = Nothing
    , _dgirGatewayState = Nothing
    , _dgirGatewayNetworkInterfaces = mempty
    , _dgirGatewayType = Nothing
    , _dgirNextUpdateAvailabilityDate = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dgirGatewayARN :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirGatewayARN = lens _dgirGatewayARN (\s a -> s { _dgirGatewayARN = a })

-- | The gateway ID.
dgirGatewayId :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirGatewayId = lens _dgirGatewayId (\s a -> s { _dgirGatewayId = a })

-- | One of the values that indicates the time zone configured for the gateway.
dgirGatewayTimezone :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirGatewayTimezone =
    lens _dgirGatewayTimezone (\s a -> s { _dgirGatewayTimezone = a })

-- | One of the values that indicates the operating state of the gateway.
dgirGatewayState :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirGatewayState =
    lens _dgirGatewayState (\s a -> s { _dgirGatewayState = a })

-- | A NetworkInterface array that contains descriptions of the gateway network
-- interfaces.
dgirGatewayNetworkInterfaces :: Lens' DescribeGatewayInformationResponse [NetworkInterface]
dgirGatewayNetworkInterfaces =
    lens _dgirGatewayNetworkInterfaces
         (\s a -> s { _dgirGatewayNetworkInterfaces = a })

-- | TBD.
dgirGatewayType :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirGatewayType = lens _dgirGatewayType (\s a -> s { _dgirGatewayType = a })

-- | The date at which an update to the gateway is available. This date is in
-- the time zone of the gateway. If the gateway is not available for an update
-- this field is not returned in the response. response example. -->.
dgirNextUpdateAvailabilityDate :: Lens' DescribeGatewayInformationResponse (Maybe Text)
dgirNextUpdateAvailabilityDate =
    lens _dgirNextUpdateAvailabilityDate
         (\s a -> s { _dgirNextUpdateAvailabilityDate = a })

instance FromJSON DescribeGatewayInformationResponse

instance AWSRequest DescribeGatewayInformation where
    type Sv DescribeGatewayInformation = StorageGateway
    type Rs DescribeGatewayInformation = DescribeGatewayInformationResponse

    request = get
    response _ = jsonResponse
