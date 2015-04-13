{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeWorkingStorage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns information about the working storage of a gateway.
-- This operation is supported only for the gateway-stored volume architecture.
-- This operation is deprecated in cached-volumes API version (20120630). Use
-- DescribeUploadBuffer instead.
--
-- Working storage is also referred to as upload buffer. You can also use the
-- DescribeUploadBuffer operation to add upload buffer to a stored-volume
-- gateway.
--
-- The response includes disk IDs that are configured as working storage, and
-- it includes the amount of working storage allocated and used.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeWorkingStorage.html>
module Network.AWS.StorageGateway.DescribeWorkingStorage
    (
    -- * Request
      DescribeWorkingStorage
    -- ** Request constructor
    , describeWorkingStorage
    -- ** Request lenses
    , dwsGatewayARN

    -- * Response
    , DescribeWorkingStorageResponse
    -- ** Response constructor
    , describeWorkingStorageResponse
    -- ** Response lenses
    , dwsrDiskIds
    , dwsrGatewayARN
    , dwsrWorkingStorageAllocatedInBytes
    , dwsrWorkingStorageUsedInBytes
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype DescribeWorkingStorage = DescribeWorkingStorage
    { _dwsGatewayARN :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DescribeWorkingStorage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwsGatewayARN' @::@ 'Text'
--
describeWorkingStorage :: Text -- ^ 'dwsGatewayARN'
                       -> DescribeWorkingStorage
describeWorkingStorage p1 = DescribeWorkingStorage
    { _dwsGatewayARN = p1
    }

dwsGatewayARN :: Lens' DescribeWorkingStorage Text
dwsGatewayARN = lens _dwsGatewayARN (\s a -> s { _dwsGatewayARN = a })

data DescribeWorkingStorageResponse = DescribeWorkingStorageResponse
    { _dwsrDiskIds                        :: List "DiskIds" Text
    , _dwsrGatewayARN                     :: Maybe Text
    , _dwsrWorkingStorageAllocatedInBytes :: Maybe Integer
    , _dwsrWorkingStorageUsedInBytes      :: Maybe Integer
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeWorkingStorageResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwsrDiskIds' @::@ ['Text']
--
-- * 'dwsrGatewayARN' @::@ 'Maybe' 'Text'
--
-- * 'dwsrWorkingStorageAllocatedInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'dwsrWorkingStorageUsedInBytes' @::@ 'Maybe' 'Integer'
--
describeWorkingStorageResponse :: DescribeWorkingStorageResponse
describeWorkingStorageResponse = DescribeWorkingStorageResponse
    { _dwsrGatewayARN                     = Nothing
    , _dwsrDiskIds                        = mempty
    , _dwsrWorkingStorageUsedInBytes      = Nothing
    , _dwsrWorkingStorageAllocatedInBytes = Nothing
    }

-- | An array of the gateway's local disk IDs that are configured as working
-- storage. Each local disk ID is specified as a string (minimum length of 1 and
-- maximum length of 300). If no local disks are configured as working storage,
-- then the DiskIds array is empty.
dwsrDiskIds :: Lens' DescribeWorkingStorageResponse [Text]
dwsrDiskIds = lens _dwsrDiskIds (\s a -> s { _dwsrDiskIds = a }) . _List

dwsrGatewayARN :: Lens' DescribeWorkingStorageResponse (Maybe Text)
dwsrGatewayARN = lens _dwsrGatewayARN (\s a -> s { _dwsrGatewayARN = a })

-- | The total working storage in bytes allocated for the gateway. If no working
-- storage is configured for the gateway, this field returns 0.
dwsrWorkingStorageAllocatedInBytes :: Lens' DescribeWorkingStorageResponse (Maybe Integer)
dwsrWorkingStorageAllocatedInBytes =
    lens _dwsrWorkingStorageAllocatedInBytes
        (\s a -> s { _dwsrWorkingStorageAllocatedInBytes = a })

-- | The total working storage in bytes in use by the gateway. If no working
-- storage is configured for the gateway, this field returns 0.
dwsrWorkingStorageUsedInBytes :: Lens' DescribeWorkingStorageResponse (Maybe Integer)
dwsrWorkingStorageUsedInBytes =
    lens _dwsrWorkingStorageUsedInBytes
        (\s a -> s { _dwsrWorkingStorageUsedInBytes = a })

instance ToPath DescribeWorkingStorage where
    toPath = const "/"

instance ToQuery DescribeWorkingStorage where
    toQuery = const mempty

instance ToHeaders DescribeWorkingStorage

instance ToJSON DescribeWorkingStorage where
    toJSON DescribeWorkingStorage{..} = object
        [ "GatewayARN" .= _dwsGatewayARN
        ]

instance AWSRequest DescribeWorkingStorage where
    type Sv DescribeWorkingStorage = StorageGateway
    type Rs DescribeWorkingStorage = DescribeWorkingStorageResponse

    request  = post "DescribeWorkingStorage"
    response = jsonResponse

instance FromJSON DescribeWorkingStorageResponse where
    parseJSON = withObject "DescribeWorkingStorageResponse" $ \o -> DescribeWorkingStorageResponse
        <$> o .:? "DiskIds" .!= mempty
        <*> o .:? "GatewayARN"
        <*> o .:? "WorkingStorageAllocatedInBytes"
        <*> o .:? "WorkingStorageUsedInBytes"
