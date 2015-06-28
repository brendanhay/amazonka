{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.DescribeUploadBuffer
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

-- | This operation returns information about the upload buffer of a gateway.
-- This operation is supported for both the gateway-stored and
-- gateway-cached volume architectures.
--
-- The response includes disk IDs that are configured as upload buffer
-- space, and it includes the amount of upload buffer space allocated and
-- used.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeUploadBuffer.html>
module Network.AWS.StorageGateway.DescribeUploadBuffer
    (
    -- * Request
      DescribeUploadBuffer
    -- ** Request constructor
    , describeUploadBuffer
    -- ** Request lenses
    , dubGatewayARN

    -- * Response
    , DescribeUploadBufferResponse
    -- ** Response constructor
    , describeUploadBufferResponse
    -- ** Response lenses
    , dubrUploadBufferAllocatedInBytes
    , dubrGatewayARN
    , dubrDiskIds
    , dubrUploadBufferUsedInBytes
    , dubrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | /See:/ 'describeUploadBuffer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dubGatewayARN'
newtype DescribeUploadBuffer = DescribeUploadBuffer'
    { _dubGatewayARN :: Text
    } deriving (Eq,Read,Show)

-- | 'DescribeUploadBuffer' smart constructor.
describeUploadBuffer :: Text -> DescribeUploadBuffer
describeUploadBuffer pGatewayARN =
    DescribeUploadBuffer'
    { _dubGatewayARN = pGatewayARN
    }

-- | FIXME: Undocumented member.
dubGatewayARN :: Lens' DescribeUploadBuffer Text
dubGatewayARN = lens _dubGatewayARN (\ s a -> s{_dubGatewayARN = a});

instance AWSRequest DescribeUploadBuffer where
        type Sv DescribeUploadBuffer = StorageGateway
        type Rs DescribeUploadBuffer =
             DescribeUploadBufferResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUploadBufferResponse' <$>
                   (x .?> "UploadBufferAllocatedInBytes") <*>
                     (x .?> "GatewayARN")
                     <*> (x .?> "DiskIds" .!@ mempty)
                     <*> (x .?> "UploadBufferUsedInBytes")
                     <*> (pure s))

instance ToHeaders DescribeUploadBuffer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeUploadBuffer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeUploadBuffer where
        toJSON DescribeUploadBuffer'{..}
          = object ["GatewayARN" .= _dubGatewayARN]

instance ToPath DescribeUploadBuffer where
        toPath = const "/"

instance ToQuery DescribeUploadBuffer where
        toQuery = const mempty

-- | /See:/ 'describeUploadBufferResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dubrUploadBufferAllocatedInBytes'
--
-- * 'dubrGatewayARN'
--
-- * 'dubrDiskIds'
--
-- * 'dubrUploadBufferUsedInBytes'
--
-- * 'dubrStatus'
data DescribeUploadBufferResponse = DescribeUploadBufferResponse'
    { _dubrUploadBufferAllocatedInBytes :: !(Maybe Integer)
    , _dubrGatewayARN                   :: !(Maybe Text)
    , _dubrDiskIds                      :: !(Maybe [Text])
    , _dubrUploadBufferUsedInBytes      :: !(Maybe Integer)
    , _dubrStatus                       :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeUploadBufferResponse' smart constructor.
describeUploadBufferResponse :: Status -> DescribeUploadBufferResponse
describeUploadBufferResponse pStatus =
    DescribeUploadBufferResponse'
    { _dubrUploadBufferAllocatedInBytes = Nothing
    , _dubrGatewayARN = Nothing
    , _dubrDiskIds = Nothing
    , _dubrUploadBufferUsedInBytes = Nothing
    , _dubrStatus = pStatus
    }

-- | FIXME: Undocumented member.
dubrUploadBufferAllocatedInBytes :: Lens' DescribeUploadBufferResponse (Maybe Integer)
dubrUploadBufferAllocatedInBytes = lens _dubrUploadBufferAllocatedInBytes (\ s a -> s{_dubrUploadBufferAllocatedInBytes = a});

-- | FIXME: Undocumented member.
dubrGatewayARN :: Lens' DescribeUploadBufferResponse (Maybe Text)
dubrGatewayARN = lens _dubrGatewayARN (\ s a -> s{_dubrGatewayARN = a});

-- | FIXME: Undocumented member.
dubrDiskIds :: Lens' DescribeUploadBufferResponse [Text]
dubrDiskIds = lens _dubrDiskIds (\ s a -> s{_dubrDiskIds = a}) . _Default;

-- | FIXME: Undocumented member.
dubrUploadBufferUsedInBytes :: Lens' DescribeUploadBufferResponse (Maybe Integer)
dubrUploadBufferUsedInBytes = lens _dubrUploadBufferUsedInBytes (\ s a -> s{_dubrUploadBufferUsedInBytes = a});

-- | FIXME: Undocumented member.
dubrStatus :: Lens' DescribeUploadBufferResponse Status
dubrStatus = lens _dubrStatus (\ s a -> s{_dubrStatus = a});
