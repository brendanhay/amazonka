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
    , rwrqRebootWorkspaceRequests

    -- * Response
    , RebootWorkspacesResponse
    -- ** Response constructor
    , rebootWorkspacesResponse
    -- ** Response lenses
    , rwrsFailedRequests
    , rwrsStatus
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
-- * 'rwrqRebootWorkspaceRequests'
newtype RebootWorkspaces = RebootWorkspaces'
    { _rwrqRebootWorkspaceRequests :: List1 RebootRequest
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RebootWorkspaces' smart constructor.
rebootWorkspaces :: NonEmpty RebootRequest -> RebootWorkspaces
rebootWorkspaces pRebootWorkspaceRequests =
    RebootWorkspaces'
    { _rwrqRebootWorkspaceRequests = _List1 # pRebootWorkspaceRequests
    }

-- | An array of structures that specify the WorkSpaces to reboot.
rwrqRebootWorkspaceRequests :: Lens' RebootWorkspaces (NonEmpty RebootRequest)
rwrqRebootWorkspaceRequests = lens _rwrqRebootWorkspaceRequests (\ s a -> s{_rwrqRebootWorkspaceRequests = a}) . _List1;

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
                 _rwrqRebootWorkspaceRequests]

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
-- * 'rwrsFailedRequests'
--
-- * 'rwrsStatus'
data RebootWorkspacesResponse = RebootWorkspacesResponse'
    { _rwrsFailedRequests :: !(Maybe [FailedWorkspaceChangeRequest])
    , _rwrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RebootWorkspacesResponse' smart constructor.
rebootWorkspacesResponse :: Int -> RebootWorkspacesResponse
rebootWorkspacesResponse pStatus =
    RebootWorkspacesResponse'
    { _rwrsFailedRequests = Nothing
    , _rwrsStatus = pStatus
    }

-- | An array of structures that represent any WorkSpaces that could not be
-- rebooted.
rwrsFailedRequests :: Lens' RebootWorkspacesResponse [FailedWorkspaceChangeRequest]
rwrsFailedRequests = lens _rwrsFailedRequests (\ s a -> s{_rwrsFailedRequests = a}) . _Default;

-- | FIXME: Undocumented member.
rwrsStatus :: Lens' RebootWorkspacesResponse Int
rwrsStatus = lens _rwrsStatus (\ s a -> s{_rwrsStatus = a});
