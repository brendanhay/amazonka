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
-- Module      : Network.AWS.WorkSpaces.StopWorkspaces
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified WorkSpaces.
--
--
-- You cannot stop a WorkSpace unless it has a running mode of @AutoStop@ and a state of @AVAILABLE@ , @IMPAIRED@ , @UNHEALTHY@ , or @ERROR@ .
--
module Network.AWS.WorkSpaces.StopWorkspaces
    (
    -- * Creating a Request
      stopWorkspaces
    , StopWorkspaces
    -- * Request Lenses
    , swStopWorkspaceRequests

    -- * Destructuring the Response
    , stopWorkspacesResponse
    , StopWorkspacesResponse
    -- * Response Lenses
    , srsFailedRequests
    , srsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'stopWorkspaces' smart constructor.
newtype StopWorkspaces = StopWorkspaces'
  { _swStopWorkspaceRequests :: List1 StopRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopWorkspaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'swStopWorkspaceRequests' - The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
stopWorkspaces
    :: NonEmpty StopRequest -- ^ 'swStopWorkspaceRequests'
    -> StopWorkspaces
stopWorkspaces pStopWorkspaceRequests_ =
  StopWorkspaces' {_swStopWorkspaceRequests = _List1 # pStopWorkspaceRequests_}


-- | The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
swStopWorkspaceRequests :: Lens' StopWorkspaces (NonEmpty StopRequest)
swStopWorkspaceRequests = lens _swStopWorkspaceRequests (\ s a -> s{_swStopWorkspaceRequests = a}) . _List1

instance AWSRequest StopWorkspaces where
        type Rs StopWorkspaces = StopWorkspacesResponse
        request = postJSON workSpaces
        response
          = receiveJSON
              (\ s h x ->
                 StopWorkspacesResponse' <$>
                   (x .?> "FailedRequests" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable StopWorkspaces where

instance NFData StopWorkspaces where

instance ToHeaders StopWorkspaces where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.StopWorkspaces" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopWorkspaces where
        toJSON StopWorkspaces'{..}
          = object
              (catMaybes
                 [Just
                    ("StopWorkspaceRequests" .=
                       _swStopWorkspaceRequests)])

instance ToPath StopWorkspaces where
        toPath = const "/"

instance ToQuery StopWorkspaces where
        toQuery = const mempty

-- | /See:/ 'stopWorkspacesResponse' smart constructor.
data StopWorkspacesResponse = StopWorkspacesResponse'
  { _srsFailedRequests :: !(Maybe [FailedWorkspaceChangeRequest])
  , _srsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopWorkspacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsFailedRequests' - Information about the WorkSpaces that could not be stopped.
--
-- * 'srsResponseStatus' - -- | The response status code.
stopWorkspacesResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StopWorkspacesResponse
stopWorkspacesResponse pResponseStatus_ =
  StopWorkspacesResponse'
    {_srsFailedRequests = Nothing, _srsResponseStatus = pResponseStatus_}


-- | Information about the WorkSpaces that could not be stopped.
srsFailedRequests :: Lens' StopWorkspacesResponse [FailedWorkspaceChangeRequest]
srsFailedRequests = lens _srsFailedRequests (\ s a -> s{_srsFailedRequests = a}) . _Default . _Coerce

-- | -- | The response status code.
srsResponseStatus :: Lens' StopWorkspacesResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StopWorkspacesResponse where
