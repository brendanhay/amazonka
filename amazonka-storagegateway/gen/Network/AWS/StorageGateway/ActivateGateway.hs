{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.ActivateGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation activates the gateway you previously deployed on your host.
-- For more information, see Downloading and Deploying AWS Storage Gateway VM.
-- In the activation process you specify information such as the region you
-- want to use for storing snapshots, the time zone for scheduled snapshots
-- and the gateway schedule window, an activation key, and a name for your
-- gateway. The activation process also associates your gateway with your
-- account (see UpdateGatewayInformation). You must power on the gateway VM
-- before you can activate your gateway. Example Request The following example
-- shows a request that activates a gateway. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.ActivateGateway { "ActivationKey":
-- "29AV1-3OFV9-VVIUB-NKT0I-LRO6V", "GatewayName": "mygateway",
-- "GatewayTimezone": "GMT-12:00", "GatewayRegion": "us-east-1",
-- "GatewayType": "STORED" } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 80 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
module Network.AWS.StorageGateway.ActivateGateway
    (
    -- * Request
      ActivateGateway
    -- ** Request constructor
    , mkActivateGateway
    -- ** Request lenses
    , agActivationKey
    , agGatewayName
    , agGatewayTimezone
    , agGatewayRegion
    , agGatewayType

    -- * Response
    , ActivateGatewayResponse
    -- ** Response constructor
    , mkActivateGatewayResponse
    -- ** Response lenses
    , agrGatewayARN
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing one or more of the following fields:
-- ActivateGatewayInput$ActivationKey GatewayName
-- ActivateGatewayInput$GatewayRegion ActivateGatewayInput$GatewayTimezone
-- ActivateGatewayInput$GatewayType.
data ActivateGateway = ActivateGateway
    { _agActivationKey :: !Text
    , _agGatewayName :: !Text
    , _agGatewayTimezone :: !Text
    , _agGatewayRegion :: !Text
    , _agGatewayType :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ActivateGateway' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ActivationKey ::@ @Text@
--
-- * @GatewayName ::@ @Text@
--
-- * @GatewayTimezone ::@ @Text@
--
-- * @GatewayRegion ::@ @Text@
--
-- * @GatewayType ::@ @Maybe Text@
--
mkActivateGateway :: Text -- ^ 'agActivationKey'
                  -> Text -- ^ 'agGatewayName'
                  -> Text -- ^ 'agGatewayTimezone'
                  -> Text -- ^ 'agGatewayRegion'
                  -> ActivateGateway
mkActivateGateway p1 p2 p3 p4 = ActivateGateway
    { _agActivationKey = p1
    , _agGatewayName = p2
    , _agGatewayTimezone = p3
    , _agGatewayRegion = p4
    , _agGatewayType = Nothing
    }

-- | Your gateway activation key. You can obtain the activation key by sending
-- an HTTP GET request with redirects enabled to the gateway IP address (port
-- 80). The redirect URL returned in the response provides you the activation
-- key for your gateway in the query string parameter activationKey. It may
-- also include other activation-related parameters, however, these are merely
-- defaults -- the arguments you pass to the ActivateGateway API call
-- determine the actual configuration of your gateway.
agActivationKey :: Lens' ActivateGateway Text
agActivationKey = lens _agActivationKey (\s a -> s { _agActivationKey = a })

-- | A unique identifier for your gateway. This name becomes part of the gateway
-- Amazon Resources Name (ARN) which is what you use as an input to other
-- operations.
agGatewayName :: Lens' ActivateGateway Text
agGatewayName = lens _agGatewayName (\s a -> s { _agGatewayName = a })

-- | One of the values that indicates the time zone you want to set for the
-- gateway. The time zone is used, for example, for scheduling snapshots and
-- your gateway's maintenance schedule.
agGatewayTimezone :: Lens' ActivateGateway Text
agGatewayTimezone =
    lens _agGatewayTimezone (\s a -> s { _agGatewayTimezone = a })

-- | One of the values that indicates the region where you want to store the
-- snapshot backups. The gateway region specified must be the same region as
-- the region in your Host header in the request. For more information about
-- available regions and endpoints for AWS Storage Gateway, see Regions and
-- Endpoints in the Amazon Web Services Glossary. Valid Values: "us-east-1",
-- "us-west-1", "us-west-2", "eu-west-1", "ap-northeast-1", "ap-southest-1",
-- "sa-east-1".
agGatewayRegion :: Lens' ActivateGateway Text
agGatewayRegion = lens _agGatewayRegion (\s a -> s { _agGatewayRegion = a })

-- | One of the values that defines the type of gateway to activate. The type
-- specified is critical to all later functions of the gateway and cannot be
-- changed after activation. The default value is STORED.
agGatewayType :: Lens' ActivateGateway (Maybe Text)
agGatewayType = lens _agGatewayType (\s a -> s { _agGatewayType = a })

instance ToPath ActivateGateway

instance ToQuery ActivateGateway

instance ToHeaders ActivateGateway

instance ToJSON ActivateGateway

-- | AWS Storage Gateway returns the Amazon Resource Name (ARN) of the activated
-- gateway. It is a string made of information such as your account, gateway
-- name, and region. This ARN is used to reference the gateway in other API
-- operations as well as resource-based authorization.
newtype ActivateGatewayResponse = ActivateGatewayResponse
    { _agrGatewayARN :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ActivateGatewayResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Maybe Text@
--
mkActivateGatewayResponse :: ActivateGatewayResponse
mkActivateGatewayResponse = ActivateGatewayResponse
    { _agrGatewayARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
agrGatewayARN :: Lens' ActivateGatewayResponse (Maybe Text)
agrGatewayARN = lens _agrGatewayARN (\s a -> s { _agrGatewayARN = a })

instance FromJSON ActivateGatewayResponse

instance AWSRequest ActivateGateway where
    type Sv ActivateGateway = StorageGateway
    type Rs ActivateGateway = ActivateGatewayResponse

    request = get
    response _ = jsonResponse
