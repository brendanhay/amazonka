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
-- Module      : Network.AWS.StorageGateway.DescribeNFSFileShares
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description for one or more file shares from a file gateway. This operation is only supported in the file gateway type.
--
--
module Network.AWS.StorageGateway.DescribeNFSFileShares
    (
    -- * Creating a Request
      describeNFSFileShares
    , DescribeNFSFileShares
    -- * Request Lenses
    , dnfsfsFileShareARNList

    -- * Destructuring the Response
    , describeNFSFileSharesResponse
    , DescribeNFSFileSharesResponse
    -- * Response Lenses
    , dnfsfsrsNFSFileShareInfoList
    , dnfsfsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | DescribeNFSFileSharesInput
--
--
--
-- /See:/ 'describeNFSFileShares' smart constructor.
newtype DescribeNFSFileShares = DescribeNFSFileShares'
  { _dnfsfsFileShareARNList :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNFSFileShares' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnfsfsFileShareARNList' - An array containing the Amazon Resource Name (ARN) of each file share to be described.
describeNFSFileShares
    :: NonEmpty Text -- ^ 'dnfsfsFileShareARNList'
    -> DescribeNFSFileShares
describeNFSFileShares pFileShareARNList_ =
  DescribeNFSFileShares' {_dnfsfsFileShareARNList = _List1 # pFileShareARNList_}


-- | An array containing the Amazon Resource Name (ARN) of each file share to be described.
dnfsfsFileShareARNList :: Lens' DescribeNFSFileShares (NonEmpty Text)
dnfsfsFileShareARNList = lens _dnfsfsFileShareARNList (\ s a -> s{_dnfsfsFileShareARNList = a}) . _List1

instance AWSRequest DescribeNFSFileShares where
        type Rs DescribeNFSFileShares =
             DescribeNFSFileSharesResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DescribeNFSFileSharesResponse' <$>
                   (x .?> "NFSFileShareInfoList" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeNFSFileShares where

instance NFData DescribeNFSFileShares where

instance ToHeaders DescribeNFSFileShares where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeNFSFileShares" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeNFSFileShares where
        toJSON DescribeNFSFileShares'{..}
          = object
              (catMaybes
                 [Just
                    ("FileShareARNList" .= _dnfsfsFileShareARNList)])

instance ToPath DescribeNFSFileShares where
        toPath = const "/"

instance ToQuery DescribeNFSFileShares where
        toQuery = const mempty

-- | DescribeNFSFileSharesOutput
--
--
--
-- /See:/ 'describeNFSFileSharesResponse' smart constructor.
data DescribeNFSFileSharesResponse = DescribeNFSFileSharesResponse'
  { _dnfsfsrsNFSFileShareInfoList :: !(Maybe [NFSFileShareInfo])
  , _dnfsfsrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNFSFileSharesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnfsfsrsNFSFileShareInfoList' - An array containing a description for each requested file share.
--
-- * 'dnfsfsrsResponseStatus' - -- | The response status code.
describeNFSFileSharesResponse
    :: Int -- ^ 'dnfsfsrsResponseStatus'
    -> DescribeNFSFileSharesResponse
describeNFSFileSharesResponse pResponseStatus_ =
  DescribeNFSFileSharesResponse'
    { _dnfsfsrsNFSFileShareInfoList = Nothing
    , _dnfsfsrsResponseStatus = pResponseStatus_
    }


-- | An array containing a description for each requested file share.
dnfsfsrsNFSFileShareInfoList :: Lens' DescribeNFSFileSharesResponse [NFSFileShareInfo]
dnfsfsrsNFSFileShareInfoList = lens _dnfsfsrsNFSFileShareInfoList (\ s a -> s{_dnfsfsrsNFSFileShareInfoList = a}) . _Default . _Coerce

-- | -- | The response status code.
dnfsfsrsResponseStatus :: Lens' DescribeNFSFileSharesResponse Int
dnfsfsrsResponseStatus = lens _dnfsfsrsResponseStatus (\ s a -> s{_dnfsfsrsResponseStatus = a})

instance NFData DescribeNFSFileSharesResponse where
