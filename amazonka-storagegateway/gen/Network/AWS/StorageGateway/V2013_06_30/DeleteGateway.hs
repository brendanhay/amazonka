{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DeleteGateway
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
module Network.AWS.StorageGateway.V2013_06_30.DeleteGateway
    (
    -- * Request
      DeleteGateway
    -- ** Request constructor
    , deleteGateway
    -- ** Request lenses
    , dgiGatewayARN

    -- * Response
    , DeleteGatewayResponse
    -- ** Response lenses
    , dgoGatewayARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DeleteGateway' request.
deleteGateway :: Text -- ^ 'dgiGatewayARN'
              -> DeleteGateway
deleteGateway p1 = DeleteGateway
    { _dgiGatewayARN = p1
    }
{-# INLINE deleteGateway #-}

data DeleteGateway = DeleteGateway
    { _dgiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dgiGatewayARN :: Lens' DeleteGateway (Text)
dgiGatewayARN f x =
    f (_dgiGatewayARN x)
        <&> \y -> x { _dgiGatewayARN = y }
{-# INLINE dgiGatewayARN #-}

instance ToPath DeleteGateway

instance ToQuery DeleteGateway

instance ToHeaders DeleteGateway

instance ToJSON DeleteGateway

data DeleteGatewayResponse = DeleteGatewayResponse
    { _dgoGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dgoGatewayARN :: Lens' DeleteGatewayResponse (Maybe Text)
dgoGatewayARN f x =
    f (_dgoGatewayARN x)
        <&> \y -> x { _dgoGatewayARN = y }
{-# INLINE dgoGatewayARN #-}

instance FromJSON DeleteGatewayResponse

instance AWSRequest DeleteGateway where
    type Sv DeleteGateway = StorageGateway
    type Rs DeleteGateway = DeleteGatewayResponse

    request = get
    response _ = jsonResponse
