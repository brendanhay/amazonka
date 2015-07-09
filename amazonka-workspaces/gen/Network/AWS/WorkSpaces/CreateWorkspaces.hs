{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.CreateWorkspaces
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more WorkSpaces.
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
    , cwrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkSpaces.Types

-- | Contains the inputs for the CreateWorkspaces operation.
--
-- /See:/ 'createWorkspaces' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwWorkspaces'
newtype CreateWorkspaces = CreateWorkspaces'
    { _cwWorkspaces :: List1 WorkspaceRequest
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateWorkspaces' smart constructor.
createWorkspaces :: NonEmpty WorkspaceRequest -> CreateWorkspaces
createWorkspaces pWorkspaces =
    CreateWorkspaces'
    { _cwWorkspaces = _List1 # pWorkspaces
    }

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
                     (x .?> "PendingRequests" .!@ mempty)
                     <*> (pure (fromEnum s)))

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

-- | Contains the result of the CreateWorkspaces operation.
--
-- /See:/ 'createWorkspacesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwrFailedRequests'
--
-- * 'cwrPendingRequests'
--
-- * 'cwrStatus'
data CreateWorkspacesResponse = CreateWorkspacesResponse'
    { _cwrFailedRequests  :: !(Maybe [FailedCreateWorkspaceRequest])
    , _cwrPendingRequests :: !(Maybe [Workspace])
    , _cwrStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateWorkspacesResponse' smart constructor.
createWorkspacesResponse :: Int -> CreateWorkspacesResponse
createWorkspacesResponse pStatus =
    CreateWorkspacesResponse'
    { _cwrFailedRequests = Nothing
    , _cwrPendingRequests = Nothing
    , _cwrStatus = pStatus
    }

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

-- | FIXME: Undocumented member.
cwrStatus :: Lens' CreateWorkspacesResponse Int
cwrStatus = lens _cwrStatus (\ s a -> s{_cwrStatus = a});
