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
-- Module      : Network.AWS.WorkSpaces.RebootWorkspaces
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots the specified WorkSpaces.
--
-- To be able to reboot a WorkSpace, the WorkSpace must have a __State__ of 'AVAILABLE', 'IMPAIRED', or 'INOPERABLE'.
--
-- This operation is asynchronous and will return before the WorkSpaces have rebooted.
module Network.AWS.WorkSpaces.RebootWorkspaces
    (
    -- * Creating a Request
      rebootWorkspaces
    , RebootWorkspaces
    -- * Request Lenses
    , rwRebootWorkspaceRequests

    -- * Destructuring the Response
    , rebootWorkspacesResponse
    , RebootWorkspacesResponse
    -- * Response Lenses
    , rrsFailedRequests
    , rrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WorkSpaces.Types
import           Network.AWS.WorkSpaces.Types.Product

-- | Contains the inputs for the < RebootWorkspaces> operation.
--
-- /See:/ 'rebootWorkspaces' smart constructor.
newtype RebootWorkspaces = RebootWorkspaces'
    { _rwRebootWorkspaceRequests :: List1 RebootRequest
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RebootWorkspaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rwRebootWorkspaceRequests'
rebootWorkspaces
    :: NonEmpty RebootRequest -- ^ 'rwRebootWorkspaceRequests'
    -> RebootWorkspaces
rebootWorkspaces pRebootWorkspaceRequests_ =
    RebootWorkspaces'
    { _rwRebootWorkspaceRequests = _List1 # pRebootWorkspaceRequests_
    }

-- | An array of structures that specify the WorkSpaces to reboot.
rwRebootWorkspaceRequests :: Lens' RebootWorkspaces (NonEmpty RebootRequest)
rwRebootWorkspaceRequests = lens _rwRebootWorkspaceRequests (\ s a -> s{_rwRebootWorkspaceRequests = a}) . _List1;

instance AWSRequest RebootWorkspaces where
        type Rs RebootWorkspaces = RebootWorkspacesResponse
        request = postJSON workSpaces
        response
          = receiveJSON
              (\ s h x ->
                 RebootWorkspacesResponse' <$>
                   (x .?> "FailedRequests" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable RebootWorkspaces

instance NFData RebootWorkspaces

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
              (catMaybes
                 [Just
                    ("RebootWorkspaceRequests" .=
                       _rwRebootWorkspaceRequests)])

instance ToPath RebootWorkspaces where
        toPath = const "/"

instance ToQuery RebootWorkspaces where
        toQuery = const mempty

-- | Contains the results of the < RebootWorkspaces> operation.
--
-- /See:/ 'rebootWorkspacesResponse' smart constructor.
data RebootWorkspacesResponse = RebootWorkspacesResponse'
    { _rrsFailedRequests :: !(Maybe [FailedWorkspaceChangeRequest])
    , _rrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RebootWorkspacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsFailedRequests'
--
-- * 'rrsResponseStatus'
rebootWorkspacesResponse
    :: Int -- ^ 'rrsResponseStatus'
    -> RebootWorkspacesResponse
rebootWorkspacesResponse pResponseStatus_ =
    RebootWorkspacesResponse'
    { _rrsFailedRequests = Nothing
    , _rrsResponseStatus = pResponseStatus_
    }

-- | An array of structures that represent any WorkSpaces that could not be rebooted.
rrsFailedRequests :: Lens' RebootWorkspacesResponse [FailedWorkspaceChangeRequest]
rrsFailedRequests = lens _rrsFailedRequests (\ s a -> s{_rrsFailedRequests = a}) . _Default . _Coerce;

-- | The response status code.
rrsResponseStatus :: Lens' RebootWorkspacesResponse Int
rrsResponseStatus = lens _rrsResponseStatus (\ s a -> s{_rrsResponseStatus = a});

instance NFData RebootWorkspacesResponse
