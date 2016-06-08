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
-- Module      : Network.AWS.WorkSpaces.CreateWorkspaces
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more WorkSpaces.
--
-- This operation is asynchronous and returns before the WorkSpaces are created.
module Network.AWS.WorkSpaces.CreateWorkspaces
    (
    -- * Creating a Request
      createWorkspaces
    , CreateWorkspaces
    -- * Request Lenses
    , cwWorkspaces

    -- * Destructuring the Response
    , createWorkspacesResponse
    , CreateWorkspacesResponse
    -- * Response Lenses
    , cwrsFailedRequests
    , cwrsPendingRequests
    , cwrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkSpaces.Types
import           Network.AWS.WorkSpaces.Types.Product

-- | Contains the inputs for the < CreateWorkspaces> operation.
--
-- /See:/ 'createWorkspaces' smart constructor.
newtype CreateWorkspaces = CreateWorkspaces'
    { _cwWorkspaces :: List1 WorkspaceRequest
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateWorkspaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwWorkspaces'
createWorkspaces
    :: NonEmpty WorkspaceRequest -- ^ 'cwWorkspaces'
    -> CreateWorkspaces
createWorkspaces pWorkspaces_ =
    CreateWorkspaces'
    { _cwWorkspaces = _List1 # pWorkspaces_
    }

-- | An array of structures that specify the WorkSpaces to create.
cwWorkspaces :: Lens' CreateWorkspaces (NonEmpty WorkspaceRequest)
cwWorkspaces = lens _cwWorkspaces (\ s a -> s{_cwWorkspaces = a}) . _List1;

instance AWSRequest CreateWorkspaces where
        type Rs CreateWorkspaces = CreateWorkspacesResponse
        request = postJSON workSpaces
        response
          = receiveJSON
              (\ s h x ->
                 CreateWorkspacesResponse' <$>
                   (x .?> "FailedRequests" .!@ mempty) <*>
                     (x .?> "PendingRequests" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable CreateWorkspaces

instance NFData CreateWorkspaces

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
          = object
              (catMaybes [Just ("Workspaces" .= _cwWorkspaces)])

instance ToPath CreateWorkspaces where
        toPath = const "/"

instance ToQuery CreateWorkspaces where
        toQuery = const mempty

-- | Contains the result of the < CreateWorkspaces> operation.
--
-- /See:/ 'createWorkspacesResponse' smart constructor.
data CreateWorkspacesResponse = CreateWorkspacesResponse'
    { _cwrsFailedRequests  :: !(Maybe [FailedCreateWorkspaceRequest])
    , _cwrsPendingRequests :: !(Maybe [Workspace])
    , _cwrsResponseStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateWorkspacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwrsFailedRequests'
--
-- * 'cwrsPendingRequests'
--
-- * 'cwrsResponseStatus'
createWorkspacesResponse
    :: Int -- ^ 'cwrsResponseStatus'
    -> CreateWorkspacesResponse
createWorkspacesResponse pResponseStatus_ =
    CreateWorkspacesResponse'
    { _cwrsFailedRequests = Nothing
    , _cwrsPendingRequests = Nothing
    , _cwrsResponseStatus = pResponseStatus_
    }

-- | An array of structures that represent the WorkSpaces that could not be created.
cwrsFailedRequests :: Lens' CreateWorkspacesResponse [FailedCreateWorkspaceRequest]
cwrsFailedRequests = lens _cwrsFailedRequests (\ s a -> s{_cwrsFailedRequests = a}) . _Default . _Coerce;

-- | An array of structures that represent the WorkSpaces that were created.
--
-- Because this operation is asynchronous, the identifier in 'WorkspaceId' is not immediately available. If you immediately call < DescribeWorkspaces> with this identifier, no information will be returned.
cwrsPendingRequests :: Lens' CreateWorkspacesResponse [Workspace]
cwrsPendingRequests = lens _cwrsPendingRequests (\ s a -> s{_cwrsPendingRequests = a}) . _Default . _Coerce;

-- | The response status code.
cwrsResponseStatus :: Lens' CreateWorkspacesResponse Int
cwrsResponseStatus = lens _cwrsResponseStatus (\ s a -> s{_cwrsResponseStatus = a});

instance NFData CreateWorkspacesResponse
