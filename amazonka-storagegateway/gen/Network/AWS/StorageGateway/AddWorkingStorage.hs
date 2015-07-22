{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AddWorkingStorage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation configures one or more gateway local disks as working
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
    , awsrqGatewayARN
    , awsrqDiskIds

    -- * Response
    , AddWorkingStorageResponse
    -- ** Response constructor
    , addWorkingStorageResponse
    -- ** Response lenses
    , awsrsGatewayARN
    , awsrsStatus
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
-- * 'awsrqGatewayARN'
--
-- * 'awsrqDiskIds'
data AddWorkingStorage = AddWorkingStorage'
    { _awsrqGatewayARN :: !Text
    , _awsrqDiskIds    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddWorkingStorage' smart constructor.
addWorkingStorage :: Text -> AddWorkingStorage
addWorkingStorage pGatewayARN =
    AddWorkingStorage'
    { _awsrqGatewayARN = pGatewayARN
    , _awsrqDiskIds = mempty
    }

-- | FIXME: Undocumented member.
awsrqGatewayARN :: Lens' AddWorkingStorage Text
awsrqGatewayARN = lens _awsrqGatewayARN (\ s a -> s{_awsrqGatewayARN = a});

-- | An array of strings that identify disks that are to be configured as
-- working storage. Each string have a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
awsrqDiskIds :: Lens' AddWorkingStorage [Text]
awsrqDiskIds = lens _awsrqDiskIds (\ s a -> s{_awsrqDiskIds = a});

instance AWSRequest AddWorkingStorage where
        type Sv AddWorkingStorage = StorageGateway
        type Rs AddWorkingStorage = AddWorkingStorageResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 AddWorkingStorageResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

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
              ["GatewayARN" .= _awsrqGatewayARN,
               "DiskIds" .= _awsrqDiskIds]

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
-- * 'awsrsGatewayARN'
--
-- * 'awsrsStatus'
data AddWorkingStorageResponse = AddWorkingStorageResponse'
    { _awsrsGatewayARN :: !(Maybe Text)
    , _awsrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddWorkingStorageResponse' smart constructor.
addWorkingStorageResponse :: Int -> AddWorkingStorageResponse
addWorkingStorageResponse pStatus =
    AddWorkingStorageResponse'
    { _awsrsGatewayARN = Nothing
    , _awsrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
awsrsGatewayARN :: Lens' AddWorkingStorageResponse (Maybe Text)
awsrsGatewayARN = lens _awsrsGatewayARN (\ s a -> s{_awsrsGatewayARN = a});

-- | FIXME: Undocumented member.
awsrsStatus :: Lens' AddWorkingStorageResponse Int
awsrsStatus = lens _awsrsStatus (\ s a -> s{_awsrsStatus = a});
