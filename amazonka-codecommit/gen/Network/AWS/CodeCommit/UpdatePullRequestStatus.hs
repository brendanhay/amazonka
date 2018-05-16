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
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of a pull request.
--
--
module Network.AWS.CodeCommit.UpdatePullRequestStatus
    (
    -- * Creating a Request
      updatePullRequestStatus
    , UpdatePullRequestStatus
    -- * Request Lenses
    , uprsPullRequestId
    , uprsPullRequestStatus

    -- * Destructuring the Response
    , updatePullRequestStatusResponse
    , UpdatePullRequestStatusResponse
    -- * Response Lenses
    , uprsrsResponseStatus
    , uprsrsPullRequest
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updatePullRequestStatus' smart constructor.
data UpdatePullRequestStatus = UpdatePullRequestStatus'
  { _uprsPullRequestId     :: !Text
  , _uprsPullRequestStatus :: !PullRequestStatusEnum
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePullRequestStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprsPullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- * 'uprsPullRequestStatus' - The status of the pull request. The only valid operations are to update the status from @OPEN@ to @OPEN@ , @OPEN@ to @CLOSED@ or from from @CLOSED@ to @CLOSED@ .
updatePullRequestStatus
    :: Text -- ^ 'uprsPullRequestId'
    -> PullRequestStatusEnum -- ^ 'uprsPullRequestStatus'
    -> UpdatePullRequestStatus
updatePullRequestStatus pPullRequestId_ pPullRequestStatus_ =
  UpdatePullRequestStatus'
    { _uprsPullRequestId = pPullRequestId_
    , _uprsPullRequestStatus = pPullRequestStatus_
    }


-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
uprsPullRequestId :: Lens' UpdatePullRequestStatus Text
uprsPullRequestId = lens _uprsPullRequestId (\ s a -> s{_uprsPullRequestId = a})

-- | The status of the pull request. The only valid operations are to update the status from @OPEN@ to @OPEN@ , @OPEN@ to @CLOSED@ or from from @CLOSED@ to @CLOSED@ .
uprsPullRequestStatus :: Lens' UpdatePullRequestStatus PullRequestStatusEnum
uprsPullRequestStatus = lens _uprsPullRequestStatus (\ s a -> s{_uprsPullRequestStatus = a})

instance AWSRequest UpdatePullRequestStatus where
        type Rs UpdatePullRequestStatus =
             UpdatePullRequestStatusResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 UpdatePullRequestStatusResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "pullRequest"))

instance Hashable UpdatePullRequestStatus where

instance NFData UpdatePullRequestStatus where

instance ToHeaders UpdatePullRequestStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.UpdatePullRequestStatus" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdatePullRequestStatus where
        toJSON UpdatePullRequestStatus'{..}
          = object
              (catMaybes
                 [Just ("pullRequestId" .= _uprsPullRequestId),
                  Just
                    ("pullRequestStatus" .= _uprsPullRequestStatus)])

instance ToPath UpdatePullRequestStatus where
        toPath = const "/"

instance ToQuery UpdatePullRequestStatus where
        toQuery = const mempty

-- | /See:/ 'updatePullRequestStatusResponse' smart constructor.
data UpdatePullRequestStatusResponse = UpdatePullRequestStatusResponse'
  { _uprsrsResponseStatus :: !Int
  , _uprsrsPullRequest    :: !PullRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePullRequestStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprsrsResponseStatus' - -- | The response status code.
--
-- * 'uprsrsPullRequest' - Information about the pull request.
updatePullRequestStatusResponse
    :: Int -- ^ 'uprsrsResponseStatus'
    -> PullRequest -- ^ 'uprsrsPullRequest'
    -> UpdatePullRequestStatusResponse
updatePullRequestStatusResponse pResponseStatus_ pPullRequest_ =
  UpdatePullRequestStatusResponse'
    { _uprsrsResponseStatus = pResponseStatus_
    , _uprsrsPullRequest = pPullRequest_
    }


-- | -- | The response status code.
uprsrsResponseStatus :: Lens' UpdatePullRequestStatusResponse Int
uprsrsResponseStatus = lens _uprsrsResponseStatus (\ s a -> s{_uprsrsResponseStatus = a})

-- | Information about the pull request.
uprsrsPullRequest :: Lens' UpdatePullRequestStatusResponse PullRequest
uprsrsPullRequest = lens _uprsrsPullRequest (\ s a -> s{_uprsrsPullRequest = a})

instance NFData UpdatePullRequestStatusResponse where
