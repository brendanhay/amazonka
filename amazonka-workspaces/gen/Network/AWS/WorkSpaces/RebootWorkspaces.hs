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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots the specified WorkSpaces.
--
--
-- You cannot reboot a WorkSpace unless its state is @AVAILABLE@ or @UNHEALTHY@ .
--
-- This operation is asynchronous and returns before the WorkSpaces have rebooted.
--
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'rebootWorkspaces' smart constructor.
newtype RebootWorkspaces = RebootWorkspaces'
  { _rwRebootWorkspaceRequests :: List1 RebootRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RebootWorkspaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rwRebootWorkspaceRequests' - The WorkSpaces to reboot. You can specify up to 25 WorkSpaces.
rebootWorkspaces
    :: NonEmpty RebootRequest -- ^ 'rwRebootWorkspaceRequests'
    -> RebootWorkspaces
rebootWorkspaces pRebootWorkspaceRequests_ =
  RebootWorkspaces'
    {_rwRebootWorkspaceRequests = _List1 # pRebootWorkspaceRequests_}


-- | The WorkSpaces to reboot. You can specify up to 25 WorkSpaces.
rwRebootWorkspaceRequests :: Lens' RebootWorkspaces (NonEmpty RebootRequest)
rwRebootWorkspaceRequests = lens _rwRebootWorkspaceRequests (\ s a -> s{_rwRebootWorkspaceRequests = a}) . _List1

instance AWSRequest RebootWorkspaces where
        type Rs RebootWorkspaces = RebootWorkspacesResponse
        request = postJSON workSpaces
        response
          = receiveJSON
              (\ s h x ->
                 RebootWorkspacesResponse' <$>
                   (x .?> "FailedRequests" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable RebootWorkspaces where

instance NFData RebootWorkspaces where

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

-- | /See:/ 'rebootWorkspacesResponse' smart constructor.
data RebootWorkspacesResponse = RebootWorkspacesResponse'
  { _rrsFailedRequests :: !(Maybe [FailedWorkspaceChangeRequest])
  , _rrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RebootWorkspacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsFailedRequests' - Information about the WorkSpaces that could not be rebooted.
--
-- * 'rrsResponseStatus' - -- | The response status code.
rebootWorkspacesResponse
    :: Int -- ^ 'rrsResponseStatus'
    -> RebootWorkspacesResponse
rebootWorkspacesResponse pResponseStatus_ =
  RebootWorkspacesResponse'
    {_rrsFailedRequests = Nothing, _rrsResponseStatus = pResponseStatus_}


-- | Information about the WorkSpaces that could not be rebooted.
rrsFailedRequests :: Lens' RebootWorkspacesResponse [FailedWorkspaceChangeRequest]
rrsFailedRequests = lens _rrsFailedRequests (\ s a -> s{_rrsFailedRequests = a}) . _Default . _Coerce

-- | -- | The response status code.
rrsResponseStatus :: Lens' RebootWorkspacesResponse Int
rrsResponseStatus = lens _rrsResponseStatus (\ s a -> s{_rrsResponseStatus = a})

instance NFData RebootWorkspacesResponse where
