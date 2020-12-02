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
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available directories that are registered with Amazon WorkSpaces.
--
--
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
  ( -- * Creating a Request
    describeWorkspaceDirectories,
    DescribeWorkspaceDirectories,

    -- * Request Lenses
    dwdNextToken,
    dwdDirectoryIds,
    dwdLimit,

    -- * Destructuring the Response
    describeWorkspaceDirectoriesResponse,
    DescribeWorkspaceDirectoriesResponse,

    -- * Response Lenses
    dwdrsDirectories,
    dwdrsNextToken,
    dwdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'describeWorkspaceDirectories' smart constructor.
data DescribeWorkspaceDirectories = DescribeWorkspaceDirectories'
  { _dwdNextToken ::
      !(Maybe Text),
    _dwdDirectoryIds ::
      !(Maybe (List1 Text)),
    _dwdLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeWorkspaceDirectories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwdNextToken' - If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- * 'dwdDirectoryIds' - The identifiers of the directories. If the value is null, all directories are retrieved.
--
-- * 'dwdLimit' - The maximum number of directories to return.
describeWorkspaceDirectories ::
  DescribeWorkspaceDirectories
describeWorkspaceDirectories =
  DescribeWorkspaceDirectories'
    { _dwdNextToken = Nothing,
      _dwdDirectoryIds = Nothing,
      _dwdLimit = Nothing
    }

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
dwdNextToken :: Lens' DescribeWorkspaceDirectories (Maybe Text)
dwdNextToken = lens _dwdNextToken (\s a -> s {_dwdNextToken = a})

-- | The identifiers of the directories. If the value is null, all directories are retrieved.
dwdDirectoryIds :: Lens' DescribeWorkspaceDirectories (Maybe (NonEmpty Text))
dwdDirectoryIds = lens _dwdDirectoryIds (\s a -> s {_dwdDirectoryIds = a}) . mapping _List1

-- | The maximum number of directories to return.
dwdLimit :: Lens' DescribeWorkspaceDirectories (Maybe Natural)
dwdLimit = lens _dwdLimit (\s a -> s {_dwdLimit = a}) . mapping _Nat

instance AWSPager DescribeWorkspaceDirectories where
  page rq rs
    | stop (rs ^. dwdrsNextToken) = Nothing
    | stop (rs ^. dwdrsDirectories) = Nothing
    | otherwise = Just $ rq & dwdNextToken .~ rs ^. dwdrsNextToken

instance AWSRequest DescribeWorkspaceDirectories where
  type
    Rs DescribeWorkspaceDirectories =
      DescribeWorkspaceDirectoriesResponse
  request = postJSON workSpaces
  response =
    receiveJSON
      ( \s h x ->
          DescribeWorkspaceDirectoriesResponse'
            <$> (x .?> "Directories" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeWorkspaceDirectories

instance NFData DescribeWorkspaceDirectories

instance ToHeaders DescribeWorkspaceDirectories where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.DescribeWorkspaceDirectories" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeWorkspaceDirectories where
  toJSON DescribeWorkspaceDirectories' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _dwdNextToken,
            ("DirectoryIds" .=) <$> _dwdDirectoryIds,
            ("Limit" .=) <$> _dwdLimit
          ]
      )

instance ToPath DescribeWorkspaceDirectories where
  toPath = const "/"

instance ToQuery DescribeWorkspaceDirectories where
  toQuery = const mempty

-- | /See:/ 'describeWorkspaceDirectoriesResponse' smart constructor.
data DescribeWorkspaceDirectoriesResponse = DescribeWorkspaceDirectoriesResponse'
  { _dwdrsDirectories ::
      !( Maybe
           [WorkspaceDirectory]
       ),
    _dwdrsNextToken ::
      !(Maybe Text),
    _dwdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeWorkspaceDirectoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwdrsDirectories' - Information about the directories.
--
-- * 'dwdrsNextToken' - The token to use to retrieve the next set of results, or null if no more results are available.
--
-- * 'dwdrsResponseStatus' - -- | The response status code.
describeWorkspaceDirectoriesResponse ::
  -- | 'dwdrsResponseStatus'
  Int ->
  DescribeWorkspaceDirectoriesResponse
describeWorkspaceDirectoriesResponse pResponseStatus_ =
  DescribeWorkspaceDirectoriesResponse'
    { _dwdrsDirectories =
        Nothing,
      _dwdrsNextToken = Nothing,
      _dwdrsResponseStatus = pResponseStatus_
    }

-- | Information about the directories.
dwdrsDirectories :: Lens' DescribeWorkspaceDirectoriesResponse [WorkspaceDirectory]
dwdrsDirectories = lens _dwdrsDirectories (\s a -> s {_dwdrsDirectories = a}) . _Default . _Coerce

-- | The token to use to retrieve the next set of results, or null if no more results are available.
dwdrsNextToken :: Lens' DescribeWorkspaceDirectoriesResponse (Maybe Text)
dwdrsNextToken = lens _dwdrsNextToken (\s a -> s {_dwdrsNextToken = a})

-- | -- | The response status code.
dwdrsResponseStatus :: Lens' DescribeWorkspaceDirectoriesResponse Int
dwdrsResponseStatus = lens _dwdrsResponseStatus (\s a -> s {_dwdrsResponseStatus = a})

instance NFData DescribeWorkspaceDirectoriesResponse
