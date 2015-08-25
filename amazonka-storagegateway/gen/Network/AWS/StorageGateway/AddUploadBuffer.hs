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
-- Module      : Network.AWS.StorageGateway.AddUploadBuffer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation configures one or more gateway local disks as upload
-- buffer for a specified gateway. This operation is supported for both the
-- gateway-stored and gateway-cached volume architectures.
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to
-- which you want to add upload buffer, and one or more disk IDs that you
-- want to configure as upload buffer.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_AddUploadBuffer.html AWS API Reference> for AddUploadBuffer.
module Network.AWS.StorageGateway.AddUploadBuffer
    (
    -- * Creating a Request
      addUploadBuffer
    , AddUploadBuffer
    -- * Request Lenses
    , aubGatewayARN
    , aubDiskIds

    -- * Destructuring the Response
    , addUploadBufferResponse
    , AddUploadBufferResponse
    -- * Response Lenses
    , aubrsGatewayARN
    , aubrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'addUploadBuffer' smart constructor.
data AddUploadBuffer = AddUploadBuffer'
    { _aubGatewayARN :: !Text
    , _aubDiskIds    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddUploadBuffer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aubGatewayARN'
--
-- * 'aubDiskIds'
addUploadBuffer
    :: Text -- ^ 'aubGatewayARN'
    -> AddUploadBuffer
addUploadBuffer pGatewayARN_ =
    AddUploadBuffer'
    { _aubGatewayARN = pGatewayARN_
    , _aubDiskIds = mempty
    }

-- | Undocumented member.
aubGatewayARN :: Lens' AddUploadBuffer Text
aubGatewayARN = lens _aubGatewayARN (\ s a -> s{_aubGatewayARN = a});

-- | Undocumented member.
aubDiskIds :: Lens' AddUploadBuffer [Text]
aubDiskIds = lens _aubDiskIds (\ s a -> s{_aubDiskIds = a}) . _Coerce;

instance AWSRequest AddUploadBuffer where
        type Rs AddUploadBuffer = AddUploadBufferResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 AddUploadBufferResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance ToHeaders AddUploadBuffer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.AddUploadBuffer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddUploadBuffer where
        toJSON AddUploadBuffer'{..}
          = object
              (catMaybes
                 [Just ("GatewayARN" .= _aubGatewayARN),
                  Just ("DiskIds" .= _aubDiskIds)])

instance ToPath AddUploadBuffer where
        toPath = const "/"

instance ToQuery AddUploadBuffer where
        toQuery = const mempty

-- | /See:/ 'addUploadBufferResponse' smart constructor.
data AddUploadBufferResponse = AddUploadBufferResponse'
    { _aubrsGatewayARN :: !(Maybe Text)
    , _aubrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddUploadBufferResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aubrsGatewayARN'
--
-- * 'aubrsStatus'
addUploadBufferResponse
    :: Int -- ^ 'aubrsStatus'
    -> AddUploadBufferResponse
addUploadBufferResponse pStatus_ =
    AddUploadBufferResponse'
    { _aubrsGatewayARN = Nothing
    , _aubrsStatus = pStatus_
    }

-- | Undocumented member.
aubrsGatewayARN :: Lens' AddUploadBufferResponse (Maybe Text)
aubrsGatewayARN = lens _aubrsGatewayARN (\ s a -> s{_aubrsGatewayARN = a});

-- | The response status code.
aubrsStatus :: Lens' AddUploadBufferResponse Int
aubrsStatus = lens _aubrsStatus (\ s a -> s{_aubrsStatus = a});
