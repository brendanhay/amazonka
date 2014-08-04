{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.StorageGateway.V2013_06_30.AddWorkingStorage where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude

data AddWorkingStorage = AddWorkingStorage
    { _awsiDiskIds :: [Text]
      -- ^ An array of strings that identify disks that are to be configured
      -- as working storage. Each string have a minimum length of 1 and
      -- maximum length of 300. You can get the disk IDs from the
      -- ListLocalDisks API.
    , _awsiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Generic)

makeLenses ''AddWorkingStorage

instance ToPath AddWorkingStorage

instance ToQuery AddWorkingStorage

instance ToHeaders AddWorkingStorage

instance ToJSON AddWorkingStorage

data AddWorkingStorageResponse = AddWorkingStorageResponse
    { _awsoGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Generic)

makeLenses ''AddWorkingStorageResponse

instance FromJSON AddWorkingStorageResponse

instance AWSRequest AddWorkingStorage where
    type Sv AddWorkingStorage = StorageGateway
    type Rs AddWorkingStorage = AddWorkingStorageResponse

    request = get
    response _ = jsonResponse
