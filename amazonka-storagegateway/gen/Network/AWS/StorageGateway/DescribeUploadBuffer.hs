{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeUploadBuffer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about the upload buffer of a gateway.
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
    , dubrsUploadBufferAllocatedInBytes
    , dubrsGatewayARN
    , dubrsDiskIds
    , dubrsUploadBufferUsedInBytes
    , dubrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeUploadBuffer' smart constructor.
describeUploadBuffer :: Text -> DescribeUploadBuffer
describeUploadBuffer pGatewayARN_ =
    DescribeUploadBuffer'
    { _dubGatewayARN = pGatewayARN_
    }

-- | FIXME: Undocumented member.
dubGatewayARN :: Lens' DescribeUploadBuffer Text
dubGatewayARN = lens _dubGatewayARN (\ s a -> s{_dubGatewayARN = a});

instance AWSRequest DescribeUploadBuffer where
        type Sv DescribeUploadBuffer = StorageGateway
        type Rs DescribeUploadBuffer =
             DescribeUploadBufferResponse
        request = postJSON "DescribeUploadBuffer"
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUploadBufferResponse' <$>
                   (x .?> "UploadBufferAllocatedInBytes") <*>
                     (x .?> "GatewayARN")
                     <*> (x .?> "DiskIds" .!@ mempty)
                     <*> (x .?> "UploadBufferUsedInBytes")
                     <*> (pure (fromEnum s)))

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
-- * 'dubrsUploadBufferAllocatedInBytes'
--
-- * 'dubrsGatewayARN'
--
-- * 'dubrsDiskIds'
--
-- * 'dubrsUploadBufferUsedInBytes'
--
-- * 'dubrsStatus'
data DescribeUploadBufferResponse = DescribeUploadBufferResponse'
    { _dubrsUploadBufferAllocatedInBytes :: !(Maybe Integer)
    , _dubrsGatewayARN                   :: !(Maybe Text)
    , _dubrsDiskIds                      :: !(Maybe [Text])
    , _dubrsUploadBufferUsedInBytes      :: !(Maybe Integer)
    , _dubrsStatus                       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeUploadBufferResponse' smart constructor.
describeUploadBufferResponse :: Int -> DescribeUploadBufferResponse
describeUploadBufferResponse pStatus_ =
    DescribeUploadBufferResponse'
    { _dubrsUploadBufferAllocatedInBytes = Nothing
    , _dubrsGatewayARN = Nothing
    , _dubrsDiskIds = Nothing
    , _dubrsUploadBufferUsedInBytes = Nothing
    , _dubrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
dubrsUploadBufferAllocatedInBytes :: Lens' DescribeUploadBufferResponse (Maybe Integer)
dubrsUploadBufferAllocatedInBytes = lens _dubrsUploadBufferAllocatedInBytes (\ s a -> s{_dubrsUploadBufferAllocatedInBytes = a});

-- | FIXME: Undocumented member.
dubrsGatewayARN :: Lens' DescribeUploadBufferResponse (Maybe Text)
dubrsGatewayARN = lens _dubrsGatewayARN (\ s a -> s{_dubrsGatewayARN = a});

-- | FIXME: Undocumented member.
dubrsDiskIds :: Lens' DescribeUploadBufferResponse [Text]
dubrsDiskIds = lens _dubrsDiskIds (\ s a -> s{_dubrsDiskIds = a}) . _Default;

-- | FIXME: Undocumented member.
dubrsUploadBufferUsedInBytes :: Lens' DescribeUploadBufferResponse (Maybe Integer)
dubrsUploadBufferUsedInBytes = lens _dubrsUploadBufferUsedInBytes (\ s a -> s{_dubrsUploadBufferUsedInBytes = a});

-- | FIXME: Undocumented member.
dubrsStatus :: Lens' DescribeUploadBufferResponse Int
dubrsStatus = lens _dubrsStatus (\ s a -> s{_dubrsStatus = a});
