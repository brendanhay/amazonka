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
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestApprovalState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the state of a user's approval on a pull request. The user is derived from the signed-in account when the request is made.
module Network.AWS.CodeCommit.UpdatePullRequestApprovalState
  ( -- * Creating a Request
    updatePullRequestApprovalState,
    UpdatePullRequestApprovalState,

    -- * Request Lenses
    uprasPullRequestId,
    uprasRevisionId,
    uprasApprovalState,

    -- * Destructuring the Response
    updatePullRequestApprovalStateResponse,
    UpdatePullRequestApprovalStateResponse,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updatePullRequestApprovalState' smart constructor.
data UpdatePullRequestApprovalState = UpdatePullRequestApprovalState'
  { _uprasPullRequestId ::
      !Text,
    _uprasRevisionId :: !Text,
    _uprasApprovalState ::
      !ApprovalState
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdatePullRequestApprovalState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprasPullRequestId' - The system-generated ID of the pull request.
--
-- * 'uprasRevisionId' - The system-generated ID of the revision.
--
-- * 'uprasApprovalState' - The approval state to associate with the user on the pull request.
updatePullRequestApprovalState ::
  -- | 'uprasPullRequestId'
  Text ->
  -- | 'uprasRevisionId'
  Text ->
  -- | 'uprasApprovalState'
  ApprovalState ->
  UpdatePullRequestApprovalState
updatePullRequestApprovalState
  pPullRequestId_
  pRevisionId_
  pApprovalState_ =
    UpdatePullRequestApprovalState'
      { _uprasPullRequestId =
          pPullRequestId_,
        _uprasRevisionId = pRevisionId_,
        _uprasApprovalState = pApprovalState_
      }

-- | The system-generated ID of the pull request.
uprasPullRequestId :: Lens' UpdatePullRequestApprovalState Text
uprasPullRequestId = lens _uprasPullRequestId (\s a -> s {_uprasPullRequestId = a})

-- | The system-generated ID of the revision.
uprasRevisionId :: Lens' UpdatePullRequestApprovalState Text
uprasRevisionId = lens _uprasRevisionId (\s a -> s {_uprasRevisionId = a})

-- | The approval state to associate with the user on the pull request.
uprasApprovalState :: Lens' UpdatePullRequestApprovalState ApprovalState
uprasApprovalState = lens _uprasApprovalState (\s a -> s {_uprasApprovalState = a})

instance AWSRequest UpdatePullRequestApprovalState where
  type
    Rs UpdatePullRequestApprovalState =
      UpdatePullRequestApprovalStateResponse
  request = postJSON codeCommit
  response = receiveNull UpdatePullRequestApprovalStateResponse'

instance Hashable UpdatePullRequestApprovalState

instance NFData UpdatePullRequestApprovalState

instance ToHeaders UpdatePullRequestApprovalState where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "CodeCommit_20150413.UpdatePullRequestApprovalState" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdatePullRequestApprovalState where
  toJSON UpdatePullRequestApprovalState' {..} =
    object
      ( catMaybes
          [ Just ("pullRequestId" .= _uprasPullRequestId),
            Just ("revisionId" .= _uprasRevisionId),
            Just ("approvalState" .= _uprasApprovalState)
          ]
      )

instance ToPath UpdatePullRequestApprovalState where
  toPath = const "/"

instance ToQuery UpdatePullRequestApprovalState where
  toQuery = const mempty

-- | /See:/ 'updatePullRequestApprovalStateResponse' smart constructor.
data UpdatePullRequestApprovalStateResponse = UpdatePullRequestApprovalStateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdatePullRequestApprovalStateResponse' with the minimum fields required to make a request.
updatePullRequestApprovalStateResponse ::
  UpdatePullRequestApprovalStateResponse
updatePullRequestApprovalStateResponse =
  UpdatePullRequestApprovalStateResponse'

instance NFData UpdatePullRequestApprovalStateResponse
