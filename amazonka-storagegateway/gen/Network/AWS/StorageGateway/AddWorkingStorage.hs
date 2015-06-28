{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.AddWorkingStorage
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation configures one or more gateway local disks as working
-- storage for a gateway. This operation is supported only for the
-- gateway-stored volume architecture. This operation is deprecated method
-- in cached-volumes API version (20120630). Use AddUploadBuffer instead.
--
-- Working storage is also referred to as upload buffer. You can also use
-- the AddUploadBuffer operation to add upload buffer to a stored-volume
-- gateway.
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to
-- which you want to add working storage, and one or more disk IDs that you
-- want to configure as working storage.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_AddWorkingStorage.html>
module Network.AWS.StorageGateway.AddWorkingStorage
    (
    -- * Request
      AddWorkingStorage
    -- ** Request constructor
    , addWorkingStorage
    -- ** Request lenses
    , awsGatewayARN
    , awsDiskIds

    -- * Response
    , AddWorkingStorageResponse
    -- ** Response constructor
    , addWorkingStorageResponse
    -- ** Response lenses
    , awsrGatewayARN
    , awsrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   AddWorkingStorageInput$DiskIds
--
-- /See:/ 'addWorkingStorage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'awsGatewayARN'
--
-- * 'awsDiskIds'
data AddWorkingStorage = AddWorkingStorage'
    { _awsGatewayARN :: !Text
    , _awsDiskIds    :: ![Text]
    } deriving (Eq,Read,Show)

-- | 'AddWorkingStorage' smart constructor.
addWorkingStorage :: Text -> AddWorkingStorage
addWorkingStorage pGatewayARN =
    AddWorkingStorage'
    { _awsGatewayARN = pGatewayARN
    , _awsDiskIds = mempty
    }

-- | FIXME: Undocumented member.
awsGatewayARN :: Lens' AddWorkingStorage Text
awsGatewayARN = lens _awsGatewayARN (\ s a -> s{_awsGatewayARN = a});

-- | An array of strings that identify disks that are to be configured as
-- working storage. Each string have a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
awsDiskIds :: Lens' AddWorkingStorage [Text]
awsDiskIds = lens _awsDiskIds (\ s a -> s{_awsDiskIds = a});

instance AWSRequest AddWorkingStorage where
        type Sv AddWorkingStorage = StorageGateway
        type Rs AddWorkingStorage = AddWorkingStorageResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 AddWorkingStorageResponse' <$>
                   (x .?> "GatewayARN") <*> (pure s))

instance ToHeaders AddWorkingStorage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.AddWorkingStorage" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddWorkingStorage where
        toJSON AddWorkingStorage'{..}
          = object
              ["GatewayARN" .= _awsGatewayARN,
               "DiskIds" .= _awsDiskIds]

instance ToPath AddWorkingStorage where
        toPath = const "/"

instance ToQuery AddWorkingStorage where
        toQuery = const mempty

-- | A JSON object containing the of the gateway for which working storage
-- was configured.
--
-- /See:/ 'addWorkingStorageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'awsrGatewayARN'
--
-- * 'awsrStatus'
data AddWorkingStorageResponse = AddWorkingStorageResponse'
    { _awsrGatewayARN :: !(Maybe Text)
    , _awsrStatus     :: !Status
    } deriving (Eq,Read,Show)

-- | 'AddWorkingStorageResponse' smart constructor.
addWorkingStorageResponse :: Status -> AddWorkingStorageResponse
addWorkingStorageResponse pStatus =
    AddWorkingStorageResponse'
    { _awsrGatewayARN = Nothing
    , _awsrStatus = pStatus
    }

-- | FIXME: Undocumented member.
awsrGatewayARN :: Lens' AddWorkingStorageResponse (Maybe Text)
awsrGatewayARN = lens _awsrGatewayARN (\ s a -> s{_awsrGatewayARN = a});

-- | FIXME: Undocumented member.
awsrStatus :: Lens' AddWorkingStorageResponse Status
awsrStatus = lens _awsrStatus (\ s a -> s{_awsrStatus = a});
