{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeSharedDirectories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the shared directories in your account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.DescribeSharedDirectories
  ( -- * Creating a Request
    describeSharedDirectories,
    DescribeSharedDirectories,

    -- * Request Lenses
    dsdSharedDirectoryIds,
    dsdNextToken,
    dsdLimit,
    dsdOwnerDirectoryId,

    -- * Destructuring the Response
    describeSharedDirectoriesResponse,
    DescribeSharedDirectoriesResponse,

    -- * Response Lenses
    dsdrsSharedDirectories,
    dsdrsNextToken,
    dsdrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSharedDirectories' smart constructor.
data DescribeSharedDirectories = DescribeSharedDirectories'
  { _dsdSharedDirectoryIds ::
      !(Maybe [Text]),
    _dsdNextToken :: !(Maybe Text),
    _dsdLimit :: !(Maybe Nat),
    _dsdOwnerDirectoryId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSharedDirectories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdSharedDirectoryIds' - A list of identifiers of all shared directories in your account.
--
-- * 'dsdNextToken' - The @DescribeSharedDirectoriesResult.NextToken@ value from a previous call to 'DescribeSharedDirectories' . Pass null if this is the first call.
--
-- * 'dsdLimit' - The number of shared directories to return in the response object.
--
-- * 'dsdOwnerDirectoryId' - Returns the identifier of the directory in the directory owner account.
describeSharedDirectories ::
  -- | 'dsdOwnerDirectoryId'
  Text ->
  DescribeSharedDirectories
describeSharedDirectories pOwnerDirectoryId_ =
  DescribeSharedDirectories'
    { _dsdSharedDirectoryIds = Nothing,
      _dsdNextToken = Nothing,
      _dsdLimit = Nothing,
      _dsdOwnerDirectoryId = pOwnerDirectoryId_
    }

-- | A list of identifiers of all shared directories in your account.
dsdSharedDirectoryIds :: Lens' DescribeSharedDirectories [Text]
dsdSharedDirectoryIds = lens _dsdSharedDirectoryIds (\s a -> s {_dsdSharedDirectoryIds = a}) . _Default . _Coerce

-- | The @DescribeSharedDirectoriesResult.NextToken@ value from a previous call to 'DescribeSharedDirectories' . Pass null if this is the first call.
dsdNextToken :: Lens' DescribeSharedDirectories (Maybe Text)
dsdNextToken = lens _dsdNextToken (\s a -> s {_dsdNextToken = a})

-- | The number of shared directories to return in the response object.
dsdLimit :: Lens' DescribeSharedDirectories (Maybe Natural)
dsdLimit = lens _dsdLimit (\s a -> s {_dsdLimit = a}) . mapping _Nat

-- | Returns the identifier of the directory in the directory owner account.
dsdOwnerDirectoryId :: Lens' DescribeSharedDirectories Text
dsdOwnerDirectoryId = lens _dsdOwnerDirectoryId (\s a -> s {_dsdOwnerDirectoryId = a})

instance AWSPager DescribeSharedDirectories where
  page rq rs
    | stop (rs ^. dsdrsNextToken) = Nothing
    | stop (rs ^. dsdrsSharedDirectories) = Nothing
    | otherwise = Just $ rq & dsdNextToken .~ rs ^. dsdrsNextToken

instance AWSRequest DescribeSharedDirectories where
  type
    Rs DescribeSharedDirectories =
      DescribeSharedDirectoriesResponse
  request = postJSON directoryService
  response =
    receiveJSON
      ( \s h x ->
          DescribeSharedDirectoriesResponse'
            <$> (x .?> "SharedDirectories" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeSharedDirectories

instance NFData DescribeSharedDirectories

instance ToHeaders DescribeSharedDirectories where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "DirectoryService_20150416.DescribeSharedDirectories" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeSharedDirectories where
  toJSON DescribeSharedDirectories' {..} =
    object
      ( catMaybes
          [ ("SharedDirectoryIds" .=) <$> _dsdSharedDirectoryIds,
            ("NextToken" .=) <$> _dsdNextToken,
            ("Limit" .=) <$> _dsdLimit,
            Just ("OwnerDirectoryId" .= _dsdOwnerDirectoryId)
          ]
      )

instance ToPath DescribeSharedDirectories where
  toPath = const "/"

instance ToQuery DescribeSharedDirectories where
  toQuery = const mempty

-- | /See:/ 'describeSharedDirectoriesResponse' smart constructor.
data DescribeSharedDirectoriesResponse = DescribeSharedDirectoriesResponse'
  { _dsdrsSharedDirectories ::
      !( Maybe
           [SharedDirectory]
       ),
    _dsdrsNextToken ::
      !(Maybe Text),
    _dsdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSharedDirectoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdrsSharedDirectories' - A list of all shared directories in your account.
--
-- * 'dsdrsNextToken' - If not null, token that indicates that more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeSharedDirectories' to retrieve the next set of items.
--
-- * 'dsdrsResponseStatus' - -- | The response status code.
describeSharedDirectoriesResponse ::
  -- | 'dsdrsResponseStatus'
  Int ->
  DescribeSharedDirectoriesResponse
describeSharedDirectoriesResponse pResponseStatus_ =
  DescribeSharedDirectoriesResponse'
    { _dsdrsSharedDirectories =
        Nothing,
      _dsdrsNextToken = Nothing,
      _dsdrsResponseStatus = pResponseStatus_
    }

-- | A list of all shared directories in your account.
dsdrsSharedDirectories :: Lens' DescribeSharedDirectoriesResponse [SharedDirectory]
dsdrsSharedDirectories = lens _dsdrsSharedDirectories (\s a -> s {_dsdrsSharedDirectories = a}) . _Default . _Coerce

-- | If not null, token that indicates that more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeSharedDirectories' to retrieve the next set of items.
dsdrsNextToken :: Lens' DescribeSharedDirectoriesResponse (Maybe Text)
dsdrsNextToken = lens _dsdrsNextToken (\s a -> s {_dsdrsNextToken = a})

-- | -- | The response status code.
dsdrsResponseStatus :: Lens' DescribeSharedDirectoriesResponse Int
dsdrsResponseStatus = lens _dsdrsResponseStatus (\s a -> s {_dsdrsResponseStatus = a})

instance NFData DescribeSharedDirectoriesResponse
