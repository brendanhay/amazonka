{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates the gateway virtual machine (VM) software. The
-- request immediately triggers the software update. Before sending this
-- request, you should make sure all your applications have finished writing
-- to your gateway's storage volumes in order to avoid data loss. During the
-- update, applications cannot use the gateway's storage volumes. --> When you
-- make this request, you get a 200 OK success response immediately. However,
-- it might take some time for the update to complete. You can call
-- DescribeGatewayInformation to verify the gateway is in the STATE_RUNNING
-- state. A software update forces a system restart of your gateway. You can
-- minimize the chance of any disruption to your applications by increasing
-- your iSCSI Initiators' timeouts. For more information about increasing
-- iSCSI Initiator timeouts for Windows and Linux, see Customizing Your
-- Windows iSCSI Settings and Customizing Your Linux iSCSI Settings,
-- respectively. Example Request The following example shows a request that
-- initiates a gateway VM update. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateGatewaySoftwareNow { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 80 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
module Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
    (
    -- * Request
      UpdateGatewaySoftwareNow
    -- ** Request constructor
    , updateGatewaySoftwareNow
    -- ** Request lenses
    , ugsnGatewayARN

    -- * Response
    , UpdateGatewaySoftwareNowResponse
    -- ** Response constructor
    , updateGatewaySoftwareNowResponse
    -- ** Response lenses
    , ugsnrGatewayARN
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing the of the gateway to update.
newtype UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNow
    { _ugsnGatewayARN :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateGatewaySoftwareNow' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
updateGatewaySoftwareNow :: Text -- ^ 'ugsnGatewayARN'
                         -> UpdateGatewaySoftwareNow
updateGatewaySoftwareNow p1 = UpdateGatewaySoftwareNow
    { _ugsnGatewayARN = p1
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
ugsnGatewayARN :: Lens' UpdateGatewaySoftwareNow Text
ugsnGatewayARN = lens _ugsnGatewayARN (\s a -> s { _ugsnGatewayARN = a })

instance ToPath UpdateGatewaySoftwareNow

instance ToQuery UpdateGatewaySoftwareNow

instance ToHeaders UpdateGatewaySoftwareNow

instance ToJSON UpdateGatewaySoftwareNow

-- | A JSON object containing the of the gateway that was updated.
newtype UpdateGatewaySoftwareNowResponse = UpdateGatewaySoftwareNowResponse
    { _ugsnrGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateGatewaySoftwareNowResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Maybe Text@
--
updateGatewaySoftwareNowResponse :: UpdateGatewaySoftwareNowResponse
updateGatewaySoftwareNowResponse = UpdateGatewaySoftwareNowResponse
    { _ugsnrGatewayARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
ugsnrGatewayARN :: Lens' UpdateGatewaySoftwareNowResponse (Maybe Text)
ugsnrGatewayARN = lens _ugsnrGatewayARN (\s a -> s { _ugsnrGatewayARN = a })

instance FromJSON UpdateGatewaySoftwareNowResponse

instance AWSRequest UpdateGatewaySoftwareNow where
    type Sv UpdateGatewaySoftwareNow = StorageGateway
    type Rs UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNowResponse

    request = get
    response _ = jsonResponse
