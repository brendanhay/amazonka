{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AddWorkingStorage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures one or more gateway local disks as working storage for a gateway. This operation is only supported in the stored volume gateway type. This operation is deprecated in cached volume API version 20120630. Use 'AddUploadBuffer' instead.
--
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to which you want to add working storage, and one or more disk IDs that you want to configure as working storage.
--
module Network.AWS.StorageGateway.AddWorkingStorage
    (
    -- * Creating a Request
      addWorkingStorage
    , AddWorkingStorage
    -- * Request Lenses
    , awsGatewayARN
    , awsDiskIds

    -- * Destructuring the Response
    , addWorkingStorageResponse
    , AddWorkingStorageResponse
    -- * Response Lenses
    , awsrsGatewayARN
    , awsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'AddWorkingStorageInput$DiskIds'
--
--
--
--
-- /See:/ 'addWorkingStorage' smart constructor.
data AddWorkingStorage = AddWorkingStorage'
  { _awsGatewayARN :: !Text
  , _awsDiskIds    :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddWorkingStorage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'awsGatewayARN' - Undocumented member.
--
-- * 'awsDiskIds' - An array of strings that identify disks that are to be configured as working storage. Each string have a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
addWorkingStorage
    :: Text -- ^ 'awsGatewayARN'
    -> AddWorkingStorage
addWorkingStorage pGatewayARN_ =
  AddWorkingStorage' {_awsGatewayARN = pGatewayARN_, _awsDiskIds = mempty}


-- | Undocumented member.
awsGatewayARN :: Lens' AddWorkingStorage Text
awsGatewayARN = lens _awsGatewayARN (\ s a -> s{_awsGatewayARN = a})

-- | An array of strings that identify disks that are to be configured as working storage. Each string have a minimum length of 1 and maximum length of 300. You can get the disk IDs from the 'ListLocalDisks' API.
awsDiskIds :: Lens' AddWorkingStorage [Text]
awsDiskIds = lens _awsDiskIds (\ s a -> s{_awsDiskIds = a}) . _Coerce

instance AWSRequest AddWorkingStorage where
        type Rs AddWorkingStorage = AddWorkingStorageResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 AddWorkingStorageResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance Hashable AddWorkingStorage where

instance NFData AddWorkingStorage where

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
              (catMaybes
                 [Just ("GatewayARN" .= _awsGatewayARN),
                  Just ("DiskIds" .= _awsDiskIds)])

instance ToPath AddWorkingStorage where
        toPath = const "/"

instance ToQuery AddWorkingStorage where
        toQuery = const mempty

-- | A JSON object containing the of the gateway for which working storage was configured.
--
--
--
-- /See:/ 'addWorkingStorageResponse' smart constructor.
data AddWorkingStorageResponse = AddWorkingStorageResponse'
  { _awsrsGatewayARN     :: !(Maybe Text)
  , _awsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddWorkingStorageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'awsrsGatewayARN' - Undocumented member.
--
-- * 'awsrsResponseStatus' - -- | The response status code.
addWorkingStorageResponse
    :: Int -- ^ 'awsrsResponseStatus'
    -> AddWorkingStorageResponse
addWorkingStorageResponse pResponseStatus_ =
  AddWorkingStorageResponse'
    {_awsrsGatewayARN = Nothing, _awsrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
awsrsGatewayARN :: Lens' AddWorkingStorageResponse (Maybe Text)
awsrsGatewayARN = lens _awsrsGatewayARN (\ s a -> s{_awsrsGatewayARN = a})

-- | -- | The response status code.
awsrsResponseStatus :: Lens' AddWorkingStorageResponse Int
awsrsResponseStatus = lens _awsrsResponseStatus (\ s a -> s{_awsrsResponseStatus = a})

instance NFData AddWorkingStorageResponse where
