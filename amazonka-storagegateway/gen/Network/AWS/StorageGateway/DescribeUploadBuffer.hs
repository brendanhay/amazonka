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
-- Module      : Network.AWS.StorageGateway.DescribeUploadBuffer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the upload buffer of a gateway. This operation is supported for the stored volume, cached volume and tape gateway types.
--
--
-- The response includes disk IDs that are configured as upload buffer space, and it includes the amount of upload buffer space allocated and used.
--
module Network.AWS.StorageGateway.DescribeUploadBuffer
    (
    -- * Creating a Request
      describeUploadBuffer
    , DescribeUploadBuffer
    -- * Request Lenses
    , dubGatewayARN

    -- * Destructuring the Response
    , describeUploadBufferResponse
    , DescribeUploadBufferResponse
    -- * Response Lenses
    , dubrsUploadBufferAllocatedInBytes
    , dubrsGatewayARN
    , dubrsDiskIds
    , dubrsUploadBufferUsedInBytes
    , dubrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'describeUploadBuffer' smart constructor.
newtype DescribeUploadBuffer = DescribeUploadBuffer'
  { _dubGatewayARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUploadBuffer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dubGatewayARN' - Undocumented member.
describeUploadBuffer
    :: Text -- ^ 'dubGatewayARN'
    -> DescribeUploadBuffer
describeUploadBuffer pGatewayARN_ =
  DescribeUploadBuffer' {_dubGatewayARN = pGatewayARN_}


-- | Undocumented member.
dubGatewayARN :: Lens' DescribeUploadBuffer Text
dubGatewayARN = lens _dubGatewayARN (\ s a -> s{_dubGatewayARN = a})

instance AWSRequest DescribeUploadBuffer where
        type Rs DescribeUploadBuffer =
             DescribeUploadBufferResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUploadBufferResponse' <$>
                   (x .?> "UploadBufferAllocatedInBytes") <*>
                     (x .?> "GatewayARN")
                     <*> (x .?> "DiskIds" .!@ mempty)
                     <*> (x .?> "UploadBufferUsedInBytes")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeUploadBuffer where

instance NFData DescribeUploadBuffer where

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
          = object
              (catMaybes [Just ("GatewayARN" .= _dubGatewayARN)])

instance ToPath DescribeUploadBuffer where
        toPath = const "/"

instance ToQuery DescribeUploadBuffer where
        toQuery = const mempty

-- | /See:/ 'describeUploadBufferResponse' smart constructor.
data DescribeUploadBufferResponse = DescribeUploadBufferResponse'
  { _dubrsUploadBufferAllocatedInBytes :: !(Maybe Integer)
  , _dubrsGatewayARN                   :: !(Maybe Text)
  , _dubrsDiskIds                      :: !(Maybe [Text])
  , _dubrsUploadBufferUsedInBytes      :: !(Maybe Integer)
  , _dubrsResponseStatus               :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUploadBufferResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dubrsUploadBufferAllocatedInBytes' - Undocumented member.
--
-- * 'dubrsGatewayARN' - Undocumented member.
--
-- * 'dubrsDiskIds' - Undocumented member.
--
-- * 'dubrsUploadBufferUsedInBytes' - Undocumented member.
--
-- * 'dubrsResponseStatus' - -- | The response status code.
describeUploadBufferResponse
    :: Int -- ^ 'dubrsResponseStatus'
    -> DescribeUploadBufferResponse
describeUploadBufferResponse pResponseStatus_ =
  DescribeUploadBufferResponse'
    { _dubrsUploadBufferAllocatedInBytes = Nothing
    , _dubrsGatewayARN = Nothing
    , _dubrsDiskIds = Nothing
    , _dubrsUploadBufferUsedInBytes = Nothing
    , _dubrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
dubrsUploadBufferAllocatedInBytes :: Lens' DescribeUploadBufferResponse (Maybe Integer)
dubrsUploadBufferAllocatedInBytes = lens _dubrsUploadBufferAllocatedInBytes (\ s a -> s{_dubrsUploadBufferAllocatedInBytes = a})

-- | Undocumented member.
dubrsGatewayARN :: Lens' DescribeUploadBufferResponse (Maybe Text)
dubrsGatewayARN = lens _dubrsGatewayARN (\ s a -> s{_dubrsGatewayARN = a})

-- | Undocumented member.
dubrsDiskIds :: Lens' DescribeUploadBufferResponse [Text]
dubrsDiskIds = lens _dubrsDiskIds (\ s a -> s{_dubrsDiskIds = a}) . _Default . _Coerce

-- | Undocumented member.
dubrsUploadBufferUsedInBytes :: Lens' DescribeUploadBufferResponse (Maybe Integer)
dubrsUploadBufferUsedInBytes = lens _dubrsUploadBufferUsedInBytes (\ s a -> s{_dubrsUploadBufferUsedInBytes = a})

-- | -- | The response status code.
dubrsResponseStatus :: Lens' DescribeUploadBufferResponse Int
dubrsResponseStatus = lens _dubrsResponseStatus (\ s a -> s{_dubrsResponseStatus = a})

instance NFData DescribeUploadBufferResponse where
