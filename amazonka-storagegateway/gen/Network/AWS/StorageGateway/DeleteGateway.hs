{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation deletes a gateway. To specify which gateway to delete, use
-- the Amazon Resource Name (ARN) of the gateway in your request. The
-- operation deletes the gateway; however, it does not delete the gateway
-- virtual machine (VM) from your host computer. After you delete a gateway,
-- you cannot reactivate it. Completed snapshots of the gateway volumes are
-- not deleted upon deleting the gateway, however, pending snapshots will not
-- complete. After you delete a gateway, your next step is to remove it from
-- your environment. You no longer pay software charges after the gateway is
-- deleted; however, your existing Amazon EBS snapshots persist and you will
-- continue to be billed for these snapshots. You can choose to remove all
-- remaining Amazon EBS snapshots by canceling your Amazon EC2 subscription.
-- If you prefer not to cancel your Amazon EC2 subscription, you can delete
-- your snapshots using the Amazon EC2 console. For more information, see the
-- AWS Storage Gateway Detail Page. Example Request The following example
-- shows a request that deactivates a gateway. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DeleteGateway { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 80 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
module Network.AWS.StorageGateway
    (
    -- * Request
      DeleteGateway
    -- ** Request constructor
    , mkDeleteGateway
    -- ** Request lenses
    , dgGatewayARN

    -- * Response
    , DeleteGatewayResponse
    -- ** Response constructor
    , mkDeleteGatewayResponse
    -- ** Response lenses
    , dgrGatewayARN
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing the of the gateway to delete.
newtype DeleteGateway = DeleteGateway
    { _dgGatewayARN :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteGateway' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
mkDeleteGateway :: Text -- ^ 'dgGatewayARN'
                -> DeleteGateway
mkDeleteGateway p1 = DeleteGateway
    { _dgGatewayARN = p1
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dgGatewayARN :: Lens' DeleteGateway Text
dgGatewayARN = lens _dgGatewayARN (\s a -> s { _dgGatewayARN = a })

instance ToPath DeleteGateway

instance ToQuery DeleteGateway

instance ToHeaders DeleteGateway

instance ToJSON DeleteGateway

-- | A JSON object containing the of the deleted gateway.
newtype DeleteGatewayResponse = DeleteGatewayResponse
    { _dgrGatewayARN :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteGatewayResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Maybe Text@
--
mkDeleteGatewayResponse :: DeleteGatewayResponse
mkDeleteGatewayResponse = DeleteGatewayResponse
    { _dgrGatewayARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dgrGatewayARN :: Lens' DeleteGatewayResponse (Maybe Text)
dgrGatewayARN = lens _dgrGatewayARN (\s a -> s { _dgrGatewayARN = a })

instance FromJSON DeleteGatewayResponse

instance AWSRequest DeleteGateway where
    type Sv DeleteGateway = StorageGateway
    type Rs DeleteGateway = DeleteGatewayResponse

    request = get
    response _ = jsonResponse
