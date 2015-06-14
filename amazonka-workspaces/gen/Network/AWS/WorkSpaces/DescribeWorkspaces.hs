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
    , dwWorkspaceIds
    , dwUserName
    , dwBundleId
    , dwNextToken
    , dwLimit

    -- * Response
    , DescribeWorkspacesResponse
    -- ** Response constructor
    , describeWorkspacesResponse
    -- ** Response lenses
    , dwrNextToken
    , dwrWorkspaces
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
-- * 'dwWorkspaceIds'
--
-- * 'dwUserName'
--
-- * 'dwBundleId'
--
-- * 'dwNextToken'
--
-- * 'dwLimit'
data DescribeWorkspaces = DescribeWorkspaces'{_dwDirectoryId :: Maybe Text, _dwWorkspaceIds :: Maybe (List1 Text), _dwUserName :: Maybe Text, _dwBundleId :: Maybe Text, _dwNextToken :: Maybe Text, _dwLimit :: Maybe Nat} deriving (Eq, Read, Show)

-- | 'DescribeWorkspaces' smart constructor.
describeWorkspaces :: DescribeWorkspaces
describeWorkspaces = DescribeWorkspaces'{_dwDirectoryId = Nothing, _dwWorkspaceIds = Nothing, _dwUserName = Nothing, _dwBundleId = Nothing, _dwNextToken = Nothing, _dwLimit = Nothing};

-- | Specifies the directory identifier to which to limit the WorkSpaces.
-- Optionally, you can specify a specific directory user with the
-- @UserName@ parameter. This parameter cannot be combined with any other
-- filter parameter.
dwDirectoryId :: Lens' DescribeWorkspaces (Maybe Text)
dwDirectoryId = lens _dwDirectoryId (\ s a -> s{_dwDirectoryId = a});

-- | An array of strings that contain the identifiers of the WorkSpaces for
-- which to retrieve information. This parameter cannot be combined with
-- any other filter parameter.
--
-- Because the CreateWorkspaces operation is asynchronous, the identifier
-- returned by CreateWorkspaces is not immediately available. If you
-- immediately call DescribeWorkspaces with this identifier, no information
-- will be returned.
dwWorkspaceIds :: Lens' DescribeWorkspaces (Maybe (NonEmpty Text))
dwWorkspaceIds = lens _dwWorkspaceIds (\ s a -> s{_dwWorkspaceIds = a}) . mapping _List1;

-- | Used with the @DirectoryId@ parameter to specify the directory user for
-- which to obtain the WorkSpace.
dwUserName :: Lens' DescribeWorkspaces (Maybe Text)
dwUserName = lens _dwUserName (\ s a -> s{_dwUserName = a});

-- | The identifier of a bundle to obtain the WorkSpaces for. All WorkSpaces
-- that are created from this bundle will be retrieved. This parameter
-- cannot be combined with any other filter parameter.
dwBundleId :: Lens' DescribeWorkspaces (Maybe Text)
dwBundleId = lens _dwBundleId (\ s a -> s{_dwBundleId = a});

-- | The @NextToken@ value from a previous call to this operation. Pass null
-- if this is the first call.
dwNextToken :: Lens' DescribeWorkspaces (Maybe Text)
dwNextToken = lens _dwNextToken (\ s a -> s{_dwNextToken = a});

-- | The maximum number of items to return.
dwLimit :: Lens' DescribeWorkspaces (Maybe Natural)
dwLimit = lens _dwLimit (\ s a -> s{_dwLimit = a}) . mapping _Nat;

instance AWSRequest DescribeWorkspaces where
        type Sv DescribeWorkspaces = WorkSpaces
        type Rs DescribeWorkspaces =
             DescribeWorkspacesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkspacesResponse' <$>
                   x .?> "NextToken" <*> x .?> "Workspaces" .!@ mempty)

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
               "WorkspaceIds" .= _dwWorkspaceIds,
               "UserName" .= _dwUserName, "BundleId" .= _dwBundleId,
               "NextToken" .= _dwNextToken, "Limit" .= _dwLimit]

instance ToPath DescribeWorkspaces where
        toPath = const "/"

instance ToQuery DescribeWorkspaces where
        toQuery = const mempty

-- | /See:/ 'describeWorkspacesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwrNextToken'
--
-- * 'dwrWorkspaces'
data DescribeWorkspacesResponse = DescribeWorkspacesResponse'{_dwrNextToken :: Maybe Text, _dwrWorkspaces :: Maybe [Workspace]} deriving (Eq, Read, Show)

-- | 'DescribeWorkspacesResponse' smart constructor.
describeWorkspacesResponse :: DescribeWorkspacesResponse
describeWorkspacesResponse = DescribeWorkspacesResponse'{_dwrNextToken = Nothing, _dwrWorkspaces = Nothing};

-- | If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to this operation to retrieve
-- the next set of items. This token is valid for one day and must be used
-- within that timeframe.
dwrNextToken :: Lens' DescribeWorkspacesResponse (Maybe Text)
dwrNextToken = lens _dwrNextToken (\ s a -> s{_dwrNextToken = a});

-- | An array of structures that contain the information about the
-- WorkSpaces.
--
-- Because the CreateWorkspaces operation is asynchronous, some of this
-- information may be incomplete for a newly-created WorkSpace.
dwrWorkspaces :: Lens' DescribeWorkspacesResponse (Maybe [Workspace])
dwrWorkspaces = lens _dwrWorkspaces (\ s a -> s{_dwrWorkspaces = a});
