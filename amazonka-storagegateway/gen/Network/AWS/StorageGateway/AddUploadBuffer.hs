{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.AddUploadBuffer
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

-- | This operation configures one or more gateway local disks as upload
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
    , aubrGatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'addUploadBuffer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aubGatewayARN'
--
-- * 'aubDiskIds'
data AddUploadBuffer = AddUploadBuffer'{_aubGatewayARN :: Text, _aubDiskIds :: [Text]} deriving (Eq, Read, Show)

-- | 'AddUploadBuffer' smart constructor.
addUploadBuffer :: Text -> AddUploadBuffer
addUploadBuffer pGatewayARN = AddUploadBuffer'{_aubGatewayARN = pGatewayARN, _aubDiskIds = mempty};

-- | FIXME: Undocumented member.
aubGatewayARN :: Lens' AddUploadBuffer Text
aubGatewayARN = lens _aubGatewayARN (\ s a -> s{_aubGatewayARN = a});

-- | FIXME: Undocumented member.
aubDiskIds :: Lens' AddUploadBuffer [Text]
aubDiskIds = lens _aubDiskIds (\ s a -> s{_aubDiskIds = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest AddUploadBuffer where
        type Sv AddUploadBuffer = StorageGateway
        type Rs AddUploadBuffer = AddUploadBufferResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 AddUploadBufferResponse' <$> (x .?> "GatewayARN"))

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
        toPath = const "/"

instance ToQuery AddUploadBuffer where
        toQuery = const mempty

-- | /See:/ 'addUploadBufferResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aubrGatewayARN'
newtype AddUploadBufferResponse = AddUploadBufferResponse'{_aubrGatewayARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AddUploadBufferResponse' smart constructor.
addUploadBufferResponse :: AddUploadBufferResponse
addUploadBufferResponse = AddUploadBufferResponse'{_aubrGatewayARN = Nothing};

-- | FIXME: Undocumented member.
aubrGatewayARN :: Lens' AddUploadBufferResponse (Maybe Text)
aubrGatewayARN = lens _aubrGatewayARN (\ s a -> s{_aubrGatewayARN = a});
