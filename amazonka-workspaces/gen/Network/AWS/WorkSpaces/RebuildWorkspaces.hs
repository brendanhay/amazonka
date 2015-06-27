{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.WorkSpaces.RebuildWorkspaces
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

-- | Rebuilds the specified WorkSpaces.
--
-- Rebuilding a WorkSpace is a potentially destructive action that can
-- result in the loss of data. Rebuilding a WorkSpace causes the following
-- to occur:
--
-- -   The system is restored to the image of the bundle that the WorkSpace
--     is created from. Any applications that have been installed, or
--     system settings that have been made since the WorkSpace was created
--     will be lost.
-- -   The data drive (D drive) is re-created from the last automatic
--     snapshot taken of the data drive. The current contents of the data
--     drive are overwritten. Automatic snapshots of the data drive are
--     taken every 12 hours, so the snapshot can be as much as 12 hours
--     old.
--
-- To be able to rebuild a WorkSpace, the WorkSpace must have a __State__
-- of @AVAILABLE@ or @ERROR@.
--
-- This operation is asynchronous and will return before the WorkSpaces
-- have been completely rebuilt.
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_RebuildWorkspaces.html>
module Network.AWS.WorkSpaces.RebuildWorkspaces
    (
    -- * Request
      RebuildWorkspaces
    -- ** Request constructor
    , rebuildWorkspaces
    -- ** Request lenses
    , rwRebuildWorkspaceRequests

    -- * Response
    , RebuildWorkspacesResponse
    -- ** Response constructor
    , rebuildWorkspacesResponse
    -- ** Response lenses
    , rwrFailedRequests
    , rwrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkSpaces.Types

-- | Contains the inputs for the RebuildWorkspaces operation.
--
-- /See:/ 'rebuildWorkspaces' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rwRebuildWorkspaceRequests'
newtype RebuildWorkspaces = RebuildWorkspaces'
    { _rwRebuildWorkspaceRequests :: List1 RebuildRequest
    } deriving (Eq,Read,Show)

-- | 'RebuildWorkspaces' smart constructor.
rebuildWorkspaces :: NonEmpty RebuildRequest -> RebuildWorkspaces
rebuildWorkspaces pRebuildWorkspaceRequests =
    RebuildWorkspaces'
    { _rwRebuildWorkspaceRequests = _List1 # pRebuildWorkspaceRequests
    }

-- | An array of structures that specify the WorkSpaces to rebuild.
rwRebuildWorkspaceRequests :: Lens' RebuildWorkspaces (NonEmpty RebuildRequest)
rwRebuildWorkspaceRequests = lens _rwRebuildWorkspaceRequests (\ s a -> s{_rwRebuildWorkspaceRequests = a}) . _List1;

instance AWSRequest RebuildWorkspaces where
        type Sv RebuildWorkspaces = WorkSpaces
        type Rs RebuildWorkspaces = RebuildWorkspacesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RebuildWorkspacesResponse' <$>
                   (x .?> "FailedRequests" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders RebuildWorkspaces where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.RebuildWorkspaces" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RebuildWorkspaces where
        toJSON RebuildWorkspaces'{..}
          = object
              ["RebuildWorkspaceRequests" .=
                 _rwRebuildWorkspaceRequests]

instance ToPath RebuildWorkspaces where
        toPath = const "/"

instance ToQuery RebuildWorkspaces where
        toQuery = const mempty

-- | Contains the results of the RebuildWorkspaces operation.
--
-- /See:/ 'rebuildWorkspacesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rwrFailedRequests'
--
-- * 'rwrStatus'
data RebuildWorkspacesResponse = RebuildWorkspacesResponse'
    { _rwrFailedRequests :: !(Maybe [FailedWorkspaceChangeRequest])
    , _rwrStatus         :: !Int
    } deriving (Eq,Read,Show)

-- | 'RebuildWorkspacesResponse' smart constructor.
rebuildWorkspacesResponse :: Int -> RebuildWorkspacesResponse
rebuildWorkspacesResponse pStatus =
    RebuildWorkspacesResponse'
    { _rwrFailedRequests = Nothing
    , _rwrStatus = pStatus
    }

-- | An array of structures that represent any WorkSpaces that could not be
-- rebuilt.
rwrFailedRequests :: Lens' RebuildWorkspacesResponse [FailedWorkspaceChangeRequest]
rwrFailedRequests = lens _rwrFailedRequests (\ s a -> s{_rwrFailedRequests = a}) . _Default;

-- | FIXME: Undocumented member.
rwrStatus :: Lens' RebuildWorkspacesResponse Int
rwrStatus = lens _rwrStatus (\ s a -> s{_rwrStatus = a});
