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
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaces
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified WorkSpaces.
--
--
-- You can filter the results using bundle ID, directory ID, or owner, but you can specify only one filter at a time.
--
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaces
    (
    -- * Creating a Request
      describeWorkspaces
    , DescribeWorkspaces
    -- * Request Lenses
    , dwDirectoryId
    , dwWorkspaceIds
    , dwUserName
    , dwBundleId
    , dwNextToken
    , dwLimit

    -- * Destructuring the Response
    , describeWorkspacesResponse
    , DescribeWorkspacesResponse
    -- * Response Lenses
    , dwrsNextToken
    , dwrsWorkspaces
    , dwrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'describeWorkspaces' smart constructor.
data DescribeWorkspaces = DescribeWorkspaces'
  { _dwDirectoryId  :: !(Maybe Text)
  , _dwWorkspaceIds :: !(Maybe (List1 Text))
  , _dwUserName     :: !(Maybe Text)
  , _dwBundleId     :: !(Maybe Text)
  , _dwNextToken    :: !(Maybe Text)
  , _dwLimit        :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeWorkspaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwDirectoryId' - The ID of the directory. In addition, you can optionally specify a specific directory user (see @UserName@ ). This parameter cannot be combined with any other filter.
--
-- * 'dwWorkspaceIds' - The IDs of the WorkSpaces. This parameter cannot be combined with any other filter. Because the 'CreateWorkspaces' operation is asynchronous, the identifier it returns is not immediately available. If you immediately call 'DescribeWorkspaces' with this identifier, no information is returned.
--
-- * 'dwUserName' - The name of the directory user. You must specify this parameter with @DirectoryId@ .
--
-- * 'dwBundleId' - The ID of the bundle. All WorkSpaces that are created from this bundle are retrieved. This parameter cannot be combined with any other filter.
--
-- * 'dwNextToken' - The token for the next set of results. (You received this token from a previous call.)
--
-- * 'dwLimit' - The maximum number of items to return.
describeWorkspaces
    :: DescribeWorkspaces
describeWorkspaces =
  DescribeWorkspaces'
    { _dwDirectoryId = Nothing
    , _dwWorkspaceIds = Nothing
    , _dwUserName = Nothing
    , _dwBundleId = Nothing
    , _dwNextToken = Nothing
    , _dwLimit = Nothing
    }


-- | The ID of the directory. In addition, you can optionally specify a specific directory user (see @UserName@ ). This parameter cannot be combined with any other filter.
dwDirectoryId :: Lens' DescribeWorkspaces (Maybe Text)
dwDirectoryId = lens _dwDirectoryId (\ s a -> s{_dwDirectoryId = a})

-- | The IDs of the WorkSpaces. This parameter cannot be combined with any other filter. Because the 'CreateWorkspaces' operation is asynchronous, the identifier it returns is not immediately available. If you immediately call 'DescribeWorkspaces' with this identifier, no information is returned.
dwWorkspaceIds :: Lens' DescribeWorkspaces (Maybe (NonEmpty Text))
dwWorkspaceIds = lens _dwWorkspaceIds (\ s a -> s{_dwWorkspaceIds = a}) . mapping _List1

-- | The name of the directory user. You must specify this parameter with @DirectoryId@ .
dwUserName :: Lens' DescribeWorkspaces (Maybe Text)
dwUserName = lens _dwUserName (\ s a -> s{_dwUserName = a})

-- | The ID of the bundle. All WorkSpaces that are created from this bundle are retrieved. This parameter cannot be combined with any other filter.
dwBundleId :: Lens' DescribeWorkspaces (Maybe Text)
dwBundleId = lens _dwBundleId (\ s a -> s{_dwBundleId = a})

-- | The token for the next set of results. (You received this token from a previous call.)
dwNextToken :: Lens' DescribeWorkspaces (Maybe Text)
dwNextToken = lens _dwNextToken (\ s a -> s{_dwNextToken = a})

-- | The maximum number of items to return.
dwLimit :: Lens' DescribeWorkspaces (Maybe Natural)
dwLimit = lens _dwLimit (\ s a -> s{_dwLimit = a}) . mapping _Nat

instance AWSPager DescribeWorkspaces where
        page rq rs
          | stop (rs ^. dwrsNextToken) = Nothing
          | stop (rs ^. dwrsWorkspaces) = Nothing
          | otherwise =
            Just $ rq & dwNextToken .~ rs ^. dwrsNextToken

instance AWSRequest DescribeWorkspaces where
        type Rs DescribeWorkspaces =
             DescribeWorkspacesResponse
        request = postJSON workSpaces
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkspacesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Workspaces" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeWorkspaces where

instance NFData DescribeWorkspaces where

instance ToHeaders DescribeWorkspaces where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.DescribeWorkspaces" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeWorkspaces where
        toJSON DescribeWorkspaces'{..}
          = object
              (catMaybes
                 [("DirectoryId" .=) <$> _dwDirectoryId,
                  ("WorkspaceIds" .=) <$> _dwWorkspaceIds,
                  ("UserName" .=) <$> _dwUserName,
                  ("BundleId" .=) <$> _dwBundleId,
                  ("NextToken" .=) <$> _dwNextToken,
                  ("Limit" .=) <$> _dwLimit])

instance ToPath DescribeWorkspaces where
        toPath = const "/"

instance ToQuery DescribeWorkspaces where
        toQuery = const mempty

-- | /See:/ 'describeWorkspacesResponse' smart constructor.
data DescribeWorkspacesResponse = DescribeWorkspacesResponse'
  { _dwrsNextToken      :: !(Maybe Text)
  , _dwrsWorkspaces     :: !(Maybe [Workspace])
  , _dwrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeWorkspacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwrsNextToken' - The token to use to retrieve the next set of results, or null if there are no more results available. This token is valid for one day and must be used within that time frame.
--
-- * 'dwrsWorkspaces' - Information about the WorkSpaces. Because 'CreateWorkspaces' is an asynchronous operation, some of the returned information could be incomplete.
--
-- * 'dwrsResponseStatus' - -- | The response status code.
describeWorkspacesResponse
    :: Int -- ^ 'dwrsResponseStatus'
    -> DescribeWorkspacesResponse
describeWorkspacesResponse pResponseStatus_ =
  DescribeWorkspacesResponse'
    { _dwrsNextToken = Nothing
    , _dwrsWorkspaces = Nothing
    , _dwrsResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next set of results, or null if there are no more results available. This token is valid for one day and must be used within that time frame.
dwrsNextToken :: Lens' DescribeWorkspacesResponse (Maybe Text)
dwrsNextToken = lens _dwrsNextToken (\ s a -> s{_dwrsNextToken = a})

-- | Information about the WorkSpaces. Because 'CreateWorkspaces' is an asynchronous operation, some of the returned information could be incomplete.
dwrsWorkspaces :: Lens' DescribeWorkspacesResponse [Workspace]
dwrsWorkspaces = lens _dwrsWorkspaces (\ s a -> s{_dwrsWorkspaces = a}) . _Default . _Coerce

-- | -- | The response status code.
dwrsResponseStatus :: Lens' DescribeWorkspacesResponse Int
dwrsResponseStatus = lens _dwrsResponseStatus (\ s a -> s{_dwrsResponseStatus = a})

instance NFData DescribeWorkspacesResponse where
