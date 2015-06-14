{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaces
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

-- | Obtains information about the specified WorkSpaces.
--
-- Only one of the filter parameters, such as @BundleId@, @DirectoryId@, or
-- @WorkspaceIds@, can be specified at a time.
--
-- This operation supports pagination with the use of the @NextToken@
-- request and response parameters. If more results are available, the
-- @NextToken@ response member contains a token that you pass in the next
-- call to this operation to retrieve the next set of items.
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_DescribeWorkspaces.html>
module Network.AWS.WorkSpaces.DescribeWorkspaces
    (
    -- * Request
      DescribeWorkspaces
    -- ** Request constructor
    , describeWorkspaces
    -- ** Request lenses
    , dwDirectoryId
    , dwBundleId
    , dwWorkspaceIds
    , dwUserName
    , dwNextToken
    , dwLimit

    -- * Response
    , DescribeWorkspacesResponse
    -- ** Response constructor
    , describeWorkspacesResponse
    -- ** Response lenses
    , dwrWorkspaces
    , dwrNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'describeWorkspaces' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwDirectoryId'
--
-- * 'dwBundleId'
--
-- * 'dwWorkspaceIds'
--
-- * 'dwUserName'
--
-- * 'dwNextToken'
--
-- * 'dwLimit'
data DescribeWorkspaces = DescribeWorkspaces'{_dwDirectoryId :: Maybe Text, _dwBundleId :: Maybe Text, _dwWorkspaceIds :: List1 Text, _dwUserName :: Text, _dwNextToken :: Text, _dwLimit :: Nat} deriving (Eq, Read, Show)

-- | 'DescribeWorkspaces' smart constructor.
describeWorkspaces :: NonEmpty Text -> Text -> Text -> Natural -> DescribeWorkspaces
describeWorkspaces pWorkspaceIds pUserName pNextToken pLimit = DescribeWorkspaces'{_dwDirectoryId = Nothing, _dwBundleId = Nothing, _dwWorkspaceIds = _List1 # pWorkspaceIds, _dwUserName = pUserName, _dwNextToken = pNextToken, _dwLimit = _Nat # pLimit};

-- | Specifies the directory identifier to which to limit the WorkSpaces.
-- Optionally, you can specify a specific directory user with the
-- @UserName@ parameter. This parameter cannot be combined with any other
-- filter parameter.
dwDirectoryId :: Lens' DescribeWorkspaces (Maybe Text)
dwDirectoryId = lens _dwDirectoryId (\ s a -> s{_dwDirectoryId = a});

-- | The identifier of a bundle to obtain the WorkSpaces for. All WorkSpaces
-- that are created from this bundle will be retrieved. This parameter
-- cannot be combined with any other filter parameter.
dwBundleId :: Lens' DescribeWorkspaces (Maybe Text)
dwBundleId = lens _dwBundleId (\ s a -> s{_dwBundleId = a});

-- | An array of strings that contain the identifiers of the WorkSpaces for
-- which to retrieve information. This parameter cannot be combined with
-- any other filter parameter.
--
-- Because the CreateWorkspaces operation is asynchronous, the identifier
-- returned by CreateWorkspaces is not immediately available. If you
-- immediately call DescribeWorkspaces with this identifier, no information
-- will be returned.
dwWorkspaceIds :: Lens' DescribeWorkspaces (NonEmpty Text)
dwWorkspaceIds = lens _dwWorkspaceIds (\ s a -> s{_dwWorkspaceIds = a}) . _List1;

-- | Used with the @DirectoryId@ parameter to specify the directory user for
-- which to obtain the WorkSpace.
dwUserName :: Lens' DescribeWorkspaces Text
dwUserName = lens _dwUserName (\ s a -> s{_dwUserName = a});

-- | The @NextToken@ value from a previous call to this operation. Pass null
-- if this is the first call.
dwNextToken :: Lens' DescribeWorkspaces Text
dwNextToken = lens _dwNextToken (\ s a -> s{_dwNextToken = a});

-- | The maximum number of items to return.
dwLimit :: Lens' DescribeWorkspaces Natural
dwLimit = lens _dwLimit (\ s a -> s{_dwLimit = a}) . _Nat;

instance AWSRequest DescribeWorkspaces where
        type Sv DescribeWorkspaces = WorkSpaces
        type Rs DescribeWorkspaces =
             DescribeWorkspacesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkspacesResponse' <$>
                   x .?> "Workspaces" .!@ mempty <*> x .:> "NextToken")

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
              ["DirectoryId" .= _dwDirectoryId,
               "BundleId" .= _dwBundleId,
               "WorkspaceIds" .= _dwWorkspaceIds,
               "UserName" .= _dwUserName,
               "NextToken" .= _dwNextToken, "Limit" .= _dwLimit]

instance ToPath DescribeWorkspaces where
        toPath = const "/"

instance ToQuery DescribeWorkspaces where
        toQuery = const mempty

-- | /See:/ 'describeWorkspacesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwrWorkspaces'
--
-- * 'dwrNextToken'
data DescribeWorkspacesResponse = DescribeWorkspacesResponse'{_dwrWorkspaces :: [Workspace], _dwrNextToken :: Text} deriving (Eq, Read, Show)

-- | 'DescribeWorkspacesResponse' smart constructor.
describeWorkspacesResponse :: Text -> DescribeWorkspacesResponse
describeWorkspacesResponse pNextToken = DescribeWorkspacesResponse'{_dwrWorkspaces = mempty, _dwrNextToken = pNextToken};

-- | An array of structures that contain the information about the
-- WorkSpaces.
--
-- Because the CreateWorkspaces operation is asynchronous, some of this
-- information may be incomplete for a newly-created WorkSpace.
dwrWorkspaces :: Lens' DescribeWorkspacesResponse [Workspace]
dwrWorkspaces = lens _dwrWorkspaces (\ s a -> s{_dwrWorkspaces = a});

-- | If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to this operation to retrieve
-- the next set of items. This token is valid for one day and must be used
-- within that timeframe.
dwrNextToken :: Lens' DescribeWorkspacesResponse Text
dwrNextToken = lens _dwrNextToken (\ s a -> s{_dwrNextToken = a});
