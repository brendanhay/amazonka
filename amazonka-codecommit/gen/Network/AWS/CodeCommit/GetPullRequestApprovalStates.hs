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
-- Module      : Network.AWS.CodeCommit.GetPullRequestApprovalStates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the approval states for a specified pull request.
-- Approval states only apply to pull requests that have one or more
-- approval rules applied to them.
module Network.AWS.CodeCommit.GetPullRequestApprovalStates
  ( -- * Creating a Request
    GetPullRequestApprovalStates (..),
    newGetPullRequestApprovalStates,

    -- * Request Lenses
    getPullRequestApprovalStates_pullRequestId,
    getPullRequestApprovalStates_revisionId,

    -- * Destructuring the Response
    GetPullRequestApprovalStatesResponse (..),
    newGetPullRequestApprovalStatesResponse,

    -- * Response Lenses
    getPullRequestApprovalStatesResponse_approvals,
    getPullRequestApprovalStatesResponse_httpStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPullRequestApprovalStates' smart constructor.
data GetPullRequestApprovalStates = GetPullRequestApprovalStates'
  { -- | The system-generated ID for the pull request.
    pullRequestId :: Prelude.Text,
    -- | The system-generated ID for the pull request revision.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPullRequestApprovalStates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestId', 'getPullRequestApprovalStates_pullRequestId' - The system-generated ID for the pull request.
--
-- 'revisionId', 'getPullRequestApprovalStates_revisionId' - The system-generated ID for the pull request revision.
newGetPullRequestApprovalStates ::
  -- | 'pullRequestId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  GetPullRequestApprovalStates
newGetPullRequestApprovalStates
  pPullRequestId_
  pRevisionId_ =
    GetPullRequestApprovalStates'
      { pullRequestId =
          pPullRequestId_,
        revisionId = pRevisionId_
      }

-- | The system-generated ID for the pull request.
getPullRequestApprovalStates_pullRequestId :: Lens.Lens' GetPullRequestApprovalStates Prelude.Text
getPullRequestApprovalStates_pullRequestId = Lens.lens (\GetPullRequestApprovalStates' {pullRequestId} -> pullRequestId) (\s@GetPullRequestApprovalStates' {} a -> s {pullRequestId = a} :: GetPullRequestApprovalStates)

-- | The system-generated ID for the pull request revision.
getPullRequestApprovalStates_revisionId :: Lens.Lens' GetPullRequestApprovalStates Prelude.Text
getPullRequestApprovalStates_revisionId = Lens.lens (\GetPullRequestApprovalStates' {revisionId} -> revisionId) (\s@GetPullRequestApprovalStates' {} a -> s {revisionId = a} :: GetPullRequestApprovalStates)

instance Core.AWSRequest GetPullRequestApprovalStates where
  type
    AWSResponse GetPullRequestApprovalStates =
      GetPullRequestApprovalStatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPullRequestApprovalStatesResponse'
            Prelude.<$> (x Core..?> "approvals" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetPullRequestApprovalStates

instance Prelude.NFData GetPullRequestApprovalStates

instance Core.ToHeaders GetPullRequestApprovalStates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetPullRequestApprovalStates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetPullRequestApprovalStates where
  toJSON GetPullRequestApprovalStates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pullRequestId" Core..= pullRequestId),
            Prelude.Just ("revisionId" Core..= revisionId)
          ]
      )

instance Core.ToPath GetPullRequestApprovalStates where
  toPath = Prelude.const "/"

instance Core.ToQuery GetPullRequestApprovalStates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPullRequestApprovalStatesResponse' smart constructor.
data GetPullRequestApprovalStatesResponse = GetPullRequestApprovalStatesResponse'
  { -- | Information about users who have approved the pull request.
    approvals :: Prelude.Maybe [Approval],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPullRequestApprovalStatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvals', 'getPullRequestApprovalStatesResponse_approvals' - Information about users who have approved the pull request.
--
-- 'httpStatus', 'getPullRequestApprovalStatesResponse_httpStatus' - The response's http status code.
newGetPullRequestApprovalStatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPullRequestApprovalStatesResponse
newGetPullRequestApprovalStatesResponse pHttpStatus_ =
  GetPullRequestApprovalStatesResponse'
    { approvals =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about users who have approved the pull request.
getPullRequestApprovalStatesResponse_approvals :: Lens.Lens' GetPullRequestApprovalStatesResponse (Prelude.Maybe [Approval])
getPullRequestApprovalStatesResponse_approvals = Lens.lens (\GetPullRequestApprovalStatesResponse' {approvals} -> approvals) (\s@GetPullRequestApprovalStatesResponse' {} a -> s {approvals = a} :: GetPullRequestApprovalStatesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getPullRequestApprovalStatesResponse_httpStatus :: Lens.Lens' GetPullRequestApprovalStatesResponse Prelude.Int
getPullRequestApprovalStatesResponse_httpStatus = Lens.lens (\GetPullRequestApprovalStatesResponse' {httpStatus} -> httpStatus) (\s@GetPullRequestApprovalStatesResponse' {} a -> s {httpStatus = a} :: GetPullRequestApprovalStatesResponse)

instance
  Prelude.NFData
    GetPullRequestApprovalStatesResponse
