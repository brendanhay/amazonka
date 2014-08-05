{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DescribeWorkingStorage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns information about the working storage of a gateway.
-- This operation is supported only for the gateway-stored volume
-- architecture. Working storage is also referred to as upload buffer. You can
-- also use the DescribeUploadBuffer operation to add upload buffer to a
-- stored-volume gateway. The response includes disk IDs that are configured
-- as working storage, and it includes the amount of working storage allocated
-- and used. Example Request The following example shows a request to obtain a
-- description of a gateway's working storage. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeWorkingStorage {
-- "GatewayARN":"arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 241 {
-- "DiskIds": ["pci-0000:03:00.0-scsi-0:0:0:0",
-- "pci-0000:03:00.0-scsi-0:0:1:0"], "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "WorkingStorageAllocatedInBytes": 2199023255552,
-- "WorkingStorageUsedInBytes": 789207040 }.
module Network.AWS.StorageGateway.V2013_06_30.DescribeWorkingStorage where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude

data DescribeWorkingStorage = DescribeWorkingStorage
    { _dwsiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

makeLenses ''DescribeWorkingStorage

instance ToPath DescribeWorkingStorage

instance ToQuery DescribeWorkingStorage

instance ToHeaders DescribeWorkingStorage

instance ToJSON DescribeWorkingStorage

data DescribeWorkingStorageResponse = DescribeWorkingStorageResponse
    { _dwsoDiskIds :: [Text]
      -- ^ An array of the gateway's local disk IDs that are configured as
      -- working storage. Each local disk ID is specified as a string
      -- (minimum length of 1 and maximum length of 300). If no local
      -- disks are configured as working storage, then the DiskIds array
      -- is empty.
    , _dwsoGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _dwsoWorkingStorageAllocatedInBytes :: Maybe Integer
      -- ^ The total working storage in bytes allocated for the gateway. If
      -- no working storage is configured for the gateway, this field
      -- returns 0.
    , _dwsoWorkingStorageUsedInBytes :: Maybe Integer
      -- ^ The total working storage in bytes in use by the gateway. If no
      -- working storage is configured for the gateway, this field returns
      -- 0.
    } deriving (Show, Generic)

makeLenses ''DescribeWorkingStorageResponse

instance FromJSON DescribeWorkingStorageResponse

instance AWSRequest DescribeWorkingStorage where
    type Sv DescribeWorkingStorage = StorageGateway
    type Rs DescribeWorkingStorage = DescribeWorkingStorageResponse

    request = get
    response _ = undefined
