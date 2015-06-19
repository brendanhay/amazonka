{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.WorkSpaces.CreateWorkspaces
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

-- | Creates one or more WorkSpaces.
--
-- This operation is asynchronous and returns before the WorkSpaces are
-- created.
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_CreateWorkspaces.html>
module Network.AWS.WorkSpaces.CreateWorkspaces
    (
    -- * Request
      CreateWorkspaces
    -- ** Request constructor
    , createWorkspaces
    -- ** Request lenses
    , cwWorkspaces

    -- * Response
    , CreateWorkspacesResponse
    -- ** Response constructor
    , createWorkspacesResponse
    -- ** Response lenses
    , cwrFailedRequests
    , cwrPendingRequests
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'createWorkspaces' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwWorkspaces'
newtype CreateWorkspaces = CreateWorkspaces'{_cwWorkspaces :: List1 WorkspaceRequest} deriving (Eq, Read, Show)

-- | 'CreateWorkspaces' smart constructor.
createWorkspaces :: NonEmpty WorkspaceRequest -> CreateWorkspaces
createWorkspaces pWorkspaces = CreateWorkspaces'{_cwWorkspaces = _List1 # pWorkspaces};

-- | An array of structures that specify the WorkSpaces to create.
cwWorkspaces :: Lens' CreateWorkspaces (NonEmpty WorkspaceRequest)
cwWorkspaces = lens _cwWorkspaces (\ s a -> s{_cwWorkspaces = a}) . _List1;

instance AWSRequest CreateWorkspaces where
        type Sv CreateWorkspaces = WorkSpaces
        type Rs CreateWorkspaces = CreateWorkspacesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateWorkspacesResponse' <$>
                   (x .?> "FailedRequests" .!@ mempty) <*>
                     (x .?> "PendingRequests" .!@ mempty))

instance ToHeaders CreateWorkspaces where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.CreateWorkspaces" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateWorkspaces where
        toJSON CreateWorkspaces'{..}
          = object ["Workspaces" .= _cwWorkspaces]

instance ToPath CreateWorkspaces where
        toPath = const "/"

instance ToQuery CreateWorkspaces where
        toQuery = const mempty

-- | /See:/ 'createWorkspacesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwrFailedRequests'
--
-- * 'cwrPendingRequests'
data CreateWorkspacesResponse = CreateWorkspacesResponse'{_cwrFailedRequests :: Maybe [FailedCreateWorkspaceRequest], _cwrPendingRequests :: Maybe [Workspace]} deriving (Eq, Read, Show)

-- | 'CreateWorkspacesResponse' smart constructor.
createWorkspacesResponse :: CreateWorkspacesResponse
createWorkspacesResponse = CreateWorkspacesResponse'{_cwrFailedRequests = Nothing, _cwrPendingRequests = Nothing};

-- | An array of structures that represent the WorkSpaces that could not be
-- created.
cwrFailedRequests :: Lens' CreateWorkspacesResponse [FailedCreateWorkspaceRequest]
cwrFailedRequests = lens _cwrFailedRequests (\ s a -> s{_cwrFailedRequests = a}) . _Default;

-- | An array of structures that represent the WorkSpaces that were created.
--
-- Because this operation is asynchronous, the identifier in @WorkspaceId@
-- is not immediately available. If you immediately call DescribeWorkspaces
-- with this identifier, no information will be returned.
cwrPendingRequests :: Lens' CreateWorkspacesResponse [Workspace]
cwrPendingRequests = lens _cwrPendingRequests (\ s a -> s{_cwrPendingRequests = a}) . _Default;
