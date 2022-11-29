{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeCommit.UpdatePullRequestApprovalState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the state of a user\'s approval on a pull request. The user is
-- derived from the signed-in account when the request is made.
module Amazonka.CodeCommit.UpdatePullRequestApprovalState
  ( -- * Creating a Request
    UpdatePullRequestApprovalState (..),
    newUpdatePullRequestApprovalState,

    -- * Request Lenses
    updatePullRequestApprovalState_pullRequestId,
    updatePullRequestApprovalState_revisionId,
    updatePullRequestApprovalState_approvalState,

    -- * Destructuring the Response
    UpdatePullRequestApprovalStateResponse (..),
    newUpdatePullRequestApprovalStateResponse,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePullRequestApprovalState' smart constructor.
data UpdatePullRequestApprovalState = UpdatePullRequestApprovalState'
  { -- | The system-generated ID of the pull request.
    pullRequestId :: Prelude.Text,
    -- | The system-generated ID of the revision.
    revisionId :: Prelude.Text,
    -- | The approval state to associate with the user on the pull request.
    approvalState :: ApprovalState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePullRequestApprovalState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestId', 'updatePullRequestApprovalState_pullRequestId' - The system-generated ID of the pull request.
--
-- 'revisionId', 'updatePullRequestApprovalState_revisionId' - The system-generated ID of the revision.
--
-- 'approvalState', 'updatePullRequestApprovalState_approvalState' - The approval state to associate with the user on the pull request.
newUpdatePullRequestApprovalState ::
  -- | 'pullRequestId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  -- | 'approvalState'
  ApprovalState ->
  UpdatePullRequestApprovalState
newUpdatePullRequestApprovalState
  pPullRequestId_
  pRevisionId_
  pApprovalState_ =
    UpdatePullRequestApprovalState'
      { pullRequestId =
          pPullRequestId_,
        revisionId = pRevisionId_,
        approvalState = pApprovalState_
      }

-- | The system-generated ID of the pull request.
updatePullRequestApprovalState_pullRequestId :: Lens.Lens' UpdatePullRequestApprovalState Prelude.Text
updatePullRequestApprovalState_pullRequestId = Lens.lens (\UpdatePullRequestApprovalState' {pullRequestId} -> pullRequestId) (\s@UpdatePullRequestApprovalState' {} a -> s {pullRequestId = a} :: UpdatePullRequestApprovalState)

-- | The system-generated ID of the revision.
updatePullRequestApprovalState_revisionId :: Lens.Lens' UpdatePullRequestApprovalState Prelude.Text
updatePullRequestApprovalState_revisionId = Lens.lens (\UpdatePullRequestApprovalState' {revisionId} -> revisionId) (\s@UpdatePullRequestApprovalState' {} a -> s {revisionId = a} :: UpdatePullRequestApprovalState)

-- | The approval state to associate with the user on the pull request.
updatePullRequestApprovalState_approvalState :: Lens.Lens' UpdatePullRequestApprovalState ApprovalState
updatePullRequestApprovalState_approvalState = Lens.lens (\UpdatePullRequestApprovalState' {approvalState} -> approvalState) (\s@UpdatePullRequestApprovalState' {} a -> s {approvalState = a} :: UpdatePullRequestApprovalState)

instance
  Core.AWSRequest
    UpdatePullRequestApprovalState
  where
  type
    AWSResponse UpdatePullRequestApprovalState =
      UpdatePullRequestApprovalStateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdatePullRequestApprovalStateResponse'

instance
  Prelude.Hashable
    UpdatePullRequestApprovalState
  where
  hashWithSalt
    _salt
    UpdatePullRequestApprovalState' {..} =
      _salt `Prelude.hashWithSalt` pullRequestId
        `Prelude.hashWithSalt` revisionId
        `Prelude.hashWithSalt` approvalState

instance
  Prelude.NFData
    UpdatePullRequestApprovalState
  where
  rnf UpdatePullRequestApprovalState' {..} =
    Prelude.rnf pullRequestId
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf approvalState

instance
  Core.ToHeaders
    UpdatePullRequestApprovalState
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.UpdatePullRequestApprovalState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdatePullRequestApprovalState where
  toJSON UpdatePullRequestApprovalState' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pullRequestId" Core..= pullRequestId),
            Prelude.Just ("revisionId" Core..= revisionId),
            Prelude.Just
              ("approvalState" Core..= approvalState)
          ]
      )

instance Core.ToPath UpdatePullRequestApprovalState where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdatePullRequestApprovalState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePullRequestApprovalStateResponse' smart constructor.
data UpdatePullRequestApprovalStateResponse = UpdatePullRequestApprovalStateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePullRequestApprovalStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdatePullRequestApprovalStateResponse ::
  UpdatePullRequestApprovalStateResponse
newUpdatePullRequestApprovalStateResponse =
  UpdatePullRequestApprovalStateResponse'

instance
  Prelude.NFData
    UpdatePullRequestApprovalStateResponse
  where
  rnf _ = ()
