{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.AddWorkingStorage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation configures one or more gateway local disks as working
-- storage for a gateway. This operation is supported only for the
-- gateway-stored volume architecture. Working storage is also referred to as
-- upload buffer. You can also use the AddUploadBuffer operation to add upload
-- buffer to a stored-volume gateway. In the request, you specify the gateway
-- Amazon Resource Name (ARN) to which you want to add working storage, and
-- one or more disk IDs that you want to configure as working storage. Example
-- Request The following example shows a request that specifies that two local
-- disks of a gateway are to be configured as working storage. POST / HTTP/1.1
-- Host: storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.AddWorkingStorage { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway"
-- "DiskIds": ["pci-0000:03:00.0-scsi-0:0:0:0",
-- "pci-0000:04:00.0-scsi-1:0:0:0"] } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 80 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
module Network.AWS.StorageGateway.V2013_06_30.AddWorkingStorage
    (
    -- * Request
      AddWorkingStorage
    -- ** Request constructor
    , mkAddWorkingStorage
    -- ** Request lenses
    , awsGatewayARN
    , awsDiskIds

    -- * Response
    , AddWorkingStorageResponse
    -- ** Response lenses
    , awsrsGatewayARN
    ) where

import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing one or more of the following fields:
-- AddWorkingStorageInput$DiskIds.
data AddWorkingStorage = AddWorkingStorage
    { _awsGatewayARN :: Text
    , _awsDiskIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddWorkingStorage' request.
mkAddWorkingStorage :: Text -- ^ 'awsGatewayARN'
                    -> [Text] -- ^ 'awsDiskIds'
                    -> AddWorkingStorage
mkAddWorkingStorage p1 p2 = AddWorkingStorage
    { _awsGatewayARN = p1
    , _awsDiskIds = p2
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
awsGatewayARN :: Lens' AddWorkingStorage Text
awsGatewayARN = lens _awsGatewayARN (\s a -> s { _awsGatewayARN = a })

-- | An array of strings that identify disks that are to be configured as
-- working storage. Each string have a minimum length of 1 and maximum length
-- of 300. You can get the disk IDs from the ListLocalDisks API.
awsDiskIds :: Lens' AddWorkingStorage [Text]
awsDiskIds = lens _awsDiskIds (\s a -> s { _awsDiskIds = a })

instance ToPath AddWorkingStorage

instance ToQuery AddWorkingStorage

instance ToHeaders AddWorkingStorage

instance ToJSON AddWorkingStorage

-- | A JSON object containing the of the gateway for which working storage was
-- configured.
newtype AddWorkingStorageResponse = AddWorkingStorageResponse
    { _awsrsGatewayARN :: Maybe Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
awsrsGatewayARN :: Lens' AddWorkingStorageResponse (Maybe Text)
awsrsGatewayARN = lens _awsrsGatewayARN (\s a -> s { _awsrsGatewayARN = a })

instance FromJSON AddWorkingStorageResponse

instance AWSRequest AddWorkingStorage where
    type Sv AddWorkingStorage = StorageGateway
    type Rs AddWorkingStorage = AddWorkingStorageResponse

    request = get
    response _ = jsonResponse
