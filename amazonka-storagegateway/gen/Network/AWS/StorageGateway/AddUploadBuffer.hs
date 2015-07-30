{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AddUploadBuffer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_AddUploadBuffer.html>
module Network.AWS.StorageGateway.AddUploadBuffer
    (
    -- * Request
      AddUploadBuffer
    -- ** Request constructor
    , addUploadBuffer
    -- ** Request lenses
    , aubGatewayARN
    , aubDiskIds

    -- * Response
    , AddUploadBufferResponse
    -- ** Response constructor
    , addUploadBufferResponse
    -- ** Response lenses
    , aubrsGatewayARN
    , aubrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | /See:/ 'addUploadBuffer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aubGatewayARN'
--
-- * 'aubDiskIds'
data AddUploadBuffer = AddUploadBuffer'
    { _aubGatewayARN :: !Text
    , _aubDiskIds    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddUploadBuffer' smart constructor.
addUploadBuffer :: Text -> AddUploadBuffer
addUploadBuffer pGatewayARN_ =
    AddUploadBuffer'
    { _aubGatewayARN = pGatewayARN_
    , _aubDiskIds = mempty
    }

-- | FIXME: Undocumented member.
aubGatewayARN :: Lens' AddUploadBuffer Text
aubGatewayARN = lens _aubGatewayARN (\ s a -> s{_aubGatewayARN = a});

-- | FIXME: Undocumented member.
aubDiskIds :: Lens' AddUploadBuffer [Text]
aubDiskIds = lens _aubDiskIds (\ s a -> s{_aubDiskIds = a}) . _Coerce;

instance AWSRequest AddUploadBuffer where
        type Sv AddUploadBuffer = StorageGateway
        type Rs AddUploadBuffer = AddUploadBufferResponse
        request = postJSON
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
              ["GatewayARN" .= _aubGatewayARN,
               "DiskIds" .= _aubDiskIds]

instance ToPath AddUploadBuffer where
        toPath = const mempty

instance ToQuery AddUploadBuffer where
        toQuery = const mempty

-- | /See:/ 'addUploadBufferResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aubrsGatewayARN'
--
-- * 'aubrsStatus'
data AddUploadBufferResponse = AddUploadBufferResponse'
    { _aubrsGatewayARN :: !(Maybe Text)
    , _aubrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddUploadBufferResponse' smart constructor.
addUploadBufferResponse :: Int -> AddUploadBufferResponse
addUploadBufferResponse pStatus_ =
    AddUploadBufferResponse'
    { _aubrsGatewayARN = Nothing
    , _aubrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
aubrsGatewayARN :: Lens' AddUploadBufferResponse (Maybe Text)
aubrsGatewayARN = lens _aubrsGatewayARN (\ s a -> s{_aubrsGatewayARN = a});

-- | FIXME: Undocumented member.
aubrsStatus :: Lens' AddUploadBufferResponse Int
aubrsStatus = lens _aubrsStatus (\ s a -> s{_aubrsStatus = a});
