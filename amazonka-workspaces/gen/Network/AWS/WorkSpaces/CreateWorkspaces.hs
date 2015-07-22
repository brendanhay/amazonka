{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    , cwrqWorkspaces

    -- * Response
    , CreateWorkspacesResponse
    -- ** Response constructor
    , createWorkspacesResponse
    -- ** Response lenses
    , cwrsFailedRequests
    , cwrsPendingRequests
    , cwrsStatus
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
-- * 'cwrqWorkspaces'
newtype CreateWorkspaces = CreateWorkspaces'
    { _cwrqWorkspaces :: List1 WorkspaceRequest
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateWorkspaces' smart constructor.
createWorkspaces :: NonEmpty WorkspaceRequest -> CreateWorkspaces
createWorkspaces pWorkspaces =
    CreateWorkspaces'
    { _cwrqWorkspaces = _List1 # pWorkspaces
    }

-- | An array of structures that specify the WorkSpaces to create.
cwrqWorkspaces :: Lens' CreateWorkspaces (NonEmpty WorkspaceRequest)
cwrqWorkspaces = lens _cwrqWorkspaces (\ s a -> s{_cwrqWorkspaces = a}) . _List1;

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
          = object ["Workspaces" .= _cwrqWorkspaces]

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
-- * 'cwrsFailedRequests'
--
-- * 'cwrsPendingRequests'
--
-- * 'cwrsStatus'
data CreateWorkspacesResponse = CreateWorkspacesResponse'
    { _cwrsFailedRequests  :: !(Maybe [FailedCreateWorkspaceRequest])
    , _cwrsPendingRequests :: !(Maybe [Workspace])
    , _cwrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateWorkspacesResponse' smart constructor.
createWorkspacesResponse :: Int -> CreateWorkspacesResponse
createWorkspacesResponse pStatus =
    CreateWorkspacesResponse'
    { _cwrsFailedRequests = Nothing
    , _cwrsPendingRequests = Nothing
    , _cwrsStatus = pStatus
    }

-- | An array of structures that represent the WorkSpaces that could not be
-- created.
cwrsFailedRequests :: Lens' CreateWorkspacesResponse [FailedCreateWorkspaceRequest]
cwrsFailedRequests = lens _cwrsFailedRequests (\ s a -> s{_cwrsFailedRequests = a}) . _Default;

-- | An array of structures that represent the WorkSpaces that were created.
--
-- Because this operation is asynchronous, the identifier in @WorkspaceId@
-- is not immediately available. If you immediately call DescribeWorkspaces
-- with this identifier, no information will be returned.
cwrsPendingRequests :: Lens' CreateWorkspacesResponse [Workspace]
cwrsPendingRequests = lens _cwrsPendingRequests (\ s a -> s{_cwrsPendingRequests = a}) . _Default;

-- | FIXME: Undocumented member.
cwrsStatus :: Lens' CreateWorkspacesResponse Int
cwrsStatus = lens _cwrsStatus (\ s a -> s{_cwrsStatus = a});
