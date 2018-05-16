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
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available AWS Directory Service directories that are registered with Amazon WorkSpaces.
--
--
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
    (
    -- * Creating a Request
      describeWorkspaceDirectories
    , DescribeWorkspaceDirectories
    -- * Request Lenses
    , dwdNextToken
    , dwdDirectoryIds

    -- * Destructuring the Response
    , describeWorkspaceDirectoriesResponse
    , DescribeWorkspaceDirectoriesResponse
    -- * Response Lenses
    , dwdrsDirectories
    , dwdrsNextToken
    , dwdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'describeWorkspaceDirectories' smart constructor.
data DescribeWorkspaceDirectories = DescribeWorkspaceDirectories'
  { _dwdNextToken    :: !(Maybe Text)
  , _dwdDirectoryIds :: !(Maybe (List1 Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeWorkspaceDirectories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwdNextToken' - The token for the next set of results. (You received this token from a previous call.)
--
-- * 'dwdDirectoryIds' - The identifiers of the directories. If the value is null, all directories are retrieved.
describeWorkspaceDirectories
    :: DescribeWorkspaceDirectories
describeWorkspaceDirectories =
  DescribeWorkspaceDirectories'
    {_dwdNextToken = Nothing, _dwdDirectoryIds = Nothing}


-- | The token for the next set of results. (You received this token from a previous call.)
dwdNextToken :: Lens' DescribeWorkspaceDirectories (Maybe Text)
dwdNextToken = lens _dwdNextToken (\ s a -> s{_dwdNextToken = a})

-- | The identifiers of the directories. If the value is null, all directories are retrieved.
dwdDirectoryIds :: Lens' DescribeWorkspaceDirectories (Maybe (NonEmpty Text))
dwdDirectoryIds = lens _dwdDirectoryIds (\ s a -> s{_dwdDirectoryIds = a}) . mapping _List1

instance AWSPager DescribeWorkspaceDirectories where
        page rq rs
          | stop (rs ^. dwdrsNextToken) = Nothing
          | stop (rs ^. dwdrsDirectories) = Nothing
          | otherwise =
            Just $ rq & dwdNextToken .~ rs ^. dwdrsNextToken

instance AWSRequest DescribeWorkspaceDirectories
         where
        type Rs DescribeWorkspaceDirectories =
             DescribeWorkspaceDirectoriesResponse
        request = postJSON workSpaces
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkspaceDirectoriesResponse' <$>
                   (x .?> "Directories" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeWorkspaceDirectories where

instance NFData DescribeWorkspaceDirectories where

instance ToHeaders DescribeWorkspaceDirectories where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.DescribeWorkspaceDirectories" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeWorkspaceDirectories where
        toJSON DescribeWorkspaceDirectories'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dwdNextToken,
                  ("DirectoryIds" .=) <$> _dwdDirectoryIds])

instance ToPath DescribeWorkspaceDirectories where
        toPath = const "/"

instance ToQuery DescribeWorkspaceDirectories where
        toQuery = const mempty

-- | /See:/ 'describeWorkspaceDirectoriesResponse' smart constructor.
data DescribeWorkspaceDirectoriesResponse = DescribeWorkspaceDirectoriesResponse'
  { _dwdrsDirectories    :: !(Maybe [WorkspaceDirectory])
  , _dwdrsNextToken      :: !(Maybe Text)
  , _dwdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeWorkspaceDirectoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwdrsDirectories' - Information about the directories.
--
-- * 'dwdrsNextToken' - The token to use to retrieve the next set of results, or null if there are no more results available. This token is valid for one day and must be used within that time frame.
--
-- * 'dwdrsResponseStatus' - -- | The response status code.
describeWorkspaceDirectoriesResponse
    :: Int -- ^ 'dwdrsResponseStatus'
    -> DescribeWorkspaceDirectoriesResponse
describeWorkspaceDirectoriesResponse pResponseStatus_ =
  DescribeWorkspaceDirectoriesResponse'
    { _dwdrsDirectories = Nothing
    , _dwdrsNextToken = Nothing
    , _dwdrsResponseStatus = pResponseStatus_
    }


-- | Information about the directories.
dwdrsDirectories :: Lens' DescribeWorkspaceDirectoriesResponse [WorkspaceDirectory]
dwdrsDirectories = lens _dwdrsDirectories (\ s a -> s{_dwdrsDirectories = a}) . _Default . _Coerce

-- | The token to use to retrieve the next set of results, or null if there are no more results available. This token is valid for one day and must be used within that time frame.
dwdrsNextToken :: Lens' DescribeWorkspaceDirectoriesResponse (Maybe Text)
dwdrsNextToken = lens _dwdrsNextToken (\ s a -> s{_dwdrsNextToken = a})

-- | -- | The response status code.
dwdrsResponseStatus :: Lens' DescribeWorkspaceDirectoriesResponse Int
dwdrsResponseStatus = lens _dwdrsResponseStatus (\ s a -> s{_dwdrsResponseStatus = a})

instance NFData DescribeWorkspaceDirectoriesResponse
         where
