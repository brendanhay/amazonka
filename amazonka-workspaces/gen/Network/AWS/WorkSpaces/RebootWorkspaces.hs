{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.RebootWorkspaces
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Reboots the specified WorkSpaces.
--
-- To be able to reboot a WorkSpace, the WorkSpace must have a __State__ of
-- @AVAILABLE@, @IMPAIRED@, or @INOPERABLE@.
--
-- This operation is asynchronous and will return before the WorkSpaces
-- have rebooted.
--
-- <http://docs.aws.amazon.com/workspaces/latest/devguide/API_RebootWorkspaces.html>
module Network.AWS.WorkSpaces.RebootWorkspaces
    (
    -- * Request
      RebootWorkspaces
    -- ** Request constructor
    , rebootWorkspaces
    -- ** Request lenses
    , rwRebootWorkspaceRequests

    -- * Response
    , RebootWorkspacesResponse
    -- ** Response constructor
    , rebootWorkspacesResponse
    -- ** Response lenses
    , rebFailedRequests
    , rebStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkSpaces.Types

-- | Contains the inputs for the RebootWorkspaces operation.
--
-- /See:/ 'rebootWorkspaces' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rwRebootWorkspaceRequests'
newtype RebootWorkspaces = RebootWorkspaces'
    { _rwRebootWorkspaceRequests :: List1 RebootRequest
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RebootWorkspaces' smart constructor.
rebootWorkspaces :: NonEmpty RebootRequest -> RebootWorkspaces
rebootWorkspaces pRebootWorkspaceRequests =
    RebootWorkspaces'
    { _rwRebootWorkspaceRequests = _List1 # pRebootWorkspaceRequests
    }

-- | An array of structures that specify the WorkSpaces to reboot.
rwRebootWorkspaceRequests :: Lens' RebootWorkspaces (NonEmpty RebootRequest)
rwRebootWorkspaceRequests = lens _rwRebootWorkspaceRequests (\ s a -> s{_rwRebootWorkspaceRequests = a}) . _List1;

instance AWSRequest RebootWorkspaces where
        type Sv RebootWorkspaces = WorkSpaces
        type Rs RebootWorkspaces = RebootWorkspacesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RebootWorkspacesResponse' <$>
                   (x .?> "FailedRequests" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders RebootWorkspaces where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.RebootWorkspaces" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RebootWorkspaces where
        toJSON RebootWorkspaces'{..}
          = object
              ["RebootWorkspaceRequests" .=
                 _rwRebootWorkspaceRequests]

instance ToPath RebootWorkspaces where
        toPath = const "/"

instance ToQuery RebootWorkspaces where
        toQuery = const mempty

-- | Contains the results of the RebootWorkspaces operation.
--
-- /See:/ 'rebootWorkspacesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rebFailedRequests'
--
-- * 'rebStatus'
data RebootWorkspacesResponse = RebootWorkspacesResponse'
    { _rebFailedRequests :: !(Maybe [FailedWorkspaceChangeRequest])
    , _rebStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RebootWorkspacesResponse' smart constructor.
rebootWorkspacesResponse :: Int -> RebootWorkspacesResponse
rebootWorkspacesResponse pStatus =
    RebootWorkspacesResponse'
    { _rebFailedRequests = Nothing
    , _rebStatus = pStatus
    }

-- | An array of structures that represent any WorkSpaces that could not be
-- rebooted.
rebFailedRequests :: Lens' RebootWorkspacesResponse [FailedWorkspaceChangeRequest]
rebFailedRequests = lens _rebFailedRequests (\ s a -> s{_rebFailedRequests = a}) . _Default;

-- | FIXME: Undocumented member.
rebStatus :: Lens' RebootWorkspacesResponse Int
rebStatus = lens _rebStatus (\ s a -> s{_rebStatus = a});
