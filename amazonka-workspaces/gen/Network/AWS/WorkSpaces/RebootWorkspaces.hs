{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.WorkSpaces.RebootWorkspaces
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

-- | Reboots the specified WorkSpaces.
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
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'rebootWorkspaces' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rwRebootWorkspaceRequests'
newtype RebootWorkspaces = RebootWorkspaces'{_rwRebootWorkspaceRequests :: List1 RebootRequest} deriving (Eq, Read, Show)

-- | 'RebootWorkspaces' smart constructor.
rebootWorkspaces :: NonEmpty RebootRequest -> RebootWorkspaces
rebootWorkspaces pRebootWorkspaceRequests = RebootWorkspaces'{_rwRebootWorkspaceRequests = _List1 # pRebootWorkspaceRequests};

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
                   x .?> "FailedRequests" .!@ mempty)

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

-- | /See:/ 'rebootWorkspacesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rebFailedRequests'
newtype RebootWorkspacesResponse = RebootWorkspacesResponse'{_rebFailedRequests :: Maybe [FailedWorkspaceChangeRequest]} deriving (Eq, Read, Show)

-- | 'RebootWorkspacesResponse' smart constructor.
rebootWorkspacesResponse :: RebootWorkspacesResponse
rebootWorkspacesResponse = RebootWorkspacesResponse'{_rebFailedRequests = Nothing};

-- | An array of structures that represent any WorkSpaces that could not be
-- rebooted.
rebFailedRequests :: Lens' RebootWorkspacesResponse (Maybe [FailedWorkspaceChangeRequest])
rebFailedRequests = lens _rebFailedRequests (\ s a -> s{_rebFailedRequests = a});
