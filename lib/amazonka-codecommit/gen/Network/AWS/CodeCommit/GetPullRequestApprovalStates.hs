{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetPullRequestApprovalStates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the approval states for a specified pull request. Approval states only apply to pull requests that have one or more approval rules applied to them.
module Network.AWS.CodeCommit.GetPullRequestApprovalStates
  ( -- * Creating a Request
    getPullRequestApprovalStates,
    GetPullRequestApprovalStates,

    -- * Request Lenses
    gprasPullRequestId,
    gprasRevisionId,

    -- * Destructuring the Response
    getPullRequestApprovalStatesResponse,
    GetPullRequestApprovalStatesResponse,

    -- * Response Lenses
    gprasrsApprovals,
    gprasrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPullRequestApprovalStates' smart constructor.
data GetPullRequestApprovalStates = GetPullRequestApprovalStates'
  { _gprasPullRequestId ::
      !Text,
    _gprasRevisionId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPullRequestApprovalStates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprasPullRequestId' - The system-generated ID for the pull request.
--
-- * 'gprasRevisionId' - The system-generated ID for the pull request revision.
getPullRequestApprovalStates ::
  -- | 'gprasPullRequestId'
  Text ->
  -- | 'gprasRevisionId'
  Text ->
  GetPullRequestApprovalStates
getPullRequestApprovalStates pPullRequestId_ pRevisionId_ =
  GetPullRequestApprovalStates'
    { _gprasPullRequestId =
        pPullRequestId_,
      _gprasRevisionId = pRevisionId_
    }

-- | The system-generated ID for the pull request.
gprasPullRequestId :: Lens' GetPullRequestApprovalStates Text
gprasPullRequestId = lens _gprasPullRequestId (\s a -> s {_gprasPullRequestId = a})

-- | The system-generated ID for the pull request revision.
gprasRevisionId :: Lens' GetPullRequestApprovalStates Text
gprasRevisionId = lens _gprasRevisionId (\s a -> s {_gprasRevisionId = a})

instance AWSRequest GetPullRequestApprovalStates where
  type
    Rs GetPullRequestApprovalStates =
      GetPullRequestApprovalStatesResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          GetPullRequestApprovalStatesResponse'
            <$> (x .?> "approvals" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable GetPullRequestApprovalStates

instance NFData GetPullRequestApprovalStates

instance ToHeaders GetPullRequestApprovalStates where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.GetPullRequestApprovalStates" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetPullRequestApprovalStates where
  toJSON GetPullRequestApprovalStates' {..} =
    object
      ( catMaybes
          [ Just ("pullRequestId" .= _gprasPullRequestId),
            Just ("revisionId" .= _gprasRevisionId)
          ]
      )

instance ToPath GetPullRequestApprovalStates where
  toPath = const "/"

instance ToQuery GetPullRequestApprovalStates where
  toQuery = const mempty

-- | /See:/ 'getPullRequestApprovalStatesResponse' smart constructor.
data GetPullRequestApprovalStatesResponse = GetPullRequestApprovalStatesResponse'
  { _gprasrsApprovals ::
      !( Maybe
           [Approval]
       ),
    _gprasrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPullRequestApprovalStatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprasrsApprovals' - Information about users who have approved the pull request.
--
-- * 'gprasrsResponseStatus' - -- | The response status code.
getPullRequestApprovalStatesResponse ::
  -- | 'gprasrsResponseStatus'
  Int ->
  GetPullRequestApprovalStatesResponse
getPullRequestApprovalStatesResponse pResponseStatus_ =
  GetPullRequestApprovalStatesResponse'
    { _gprasrsApprovals =
        Nothing,
      _gprasrsResponseStatus = pResponseStatus_
    }

-- | Information about users who have approved the pull request.
gprasrsApprovals :: Lens' GetPullRequestApprovalStatesResponse [Approval]
gprasrsApprovals = lens _gprasrsApprovals (\s a -> s {_gprasrsApprovals = a}) . _Default . _Coerce

-- | -- | The response status code.
gprasrsResponseStatus :: Lens' GetPullRequestApprovalStatesResponse Int
gprasrsResponseStatus = lens _gprasrsResponseStatus (\s a -> s {_gprasrsResponseStatus = a})

instance NFData GetPullRequestApprovalStatesResponse
