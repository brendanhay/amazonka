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
-- Module      : Network.AWS.CodeCommit.DeletePullRequestApprovalRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an approval rule from a specified pull request. Approval rules
-- can be deleted from a pull request only if the pull request is open, and
-- if the approval rule was created specifically for a pull request and not
-- generated from an approval rule template associated with the repository
-- where the pull request was created. You cannot delete an approval rule
-- from a merged or closed pull request.
module Network.AWS.CodeCommit.DeletePullRequestApprovalRule
  ( -- * Creating a Request
    DeletePullRequestApprovalRule (..),
    newDeletePullRequestApprovalRule,

    -- * Request Lenses
    deletePullRequestApprovalRule_pullRequestId,
    deletePullRequestApprovalRule_approvalRuleName,

    -- * Destructuring the Response
    DeletePullRequestApprovalRuleResponse (..),
    newDeletePullRequestApprovalRuleResponse,

    -- * Response Lenses
    deletePullRequestApprovalRuleResponse_httpStatus,
    deletePullRequestApprovalRuleResponse_approvalRuleId,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePullRequestApprovalRule' smart constructor.
data DeletePullRequestApprovalRule = DeletePullRequestApprovalRule'
  { -- | The system-generated ID of the pull request that contains the approval
    -- rule you want to delete.
    pullRequestId :: Core.Text,
    -- | The name of the approval rule you want to delete.
    approvalRuleName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePullRequestApprovalRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestId', 'deletePullRequestApprovalRule_pullRequestId' - The system-generated ID of the pull request that contains the approval
-- rule you want to delete.
--
-- 'approvalRuleName', 'deletePullRequestApprovalRule_approvalRuleName' - The name of the approval rule you want to delete.
newDeletePullRequestApprovalRule ::
  -- | 'pullRequestId'
  Core.Text ->
  -- | 'approvalRuleName'
  Core.Text ->
  DeletePullRequestApprovalRule
newDeletePullRequestApprovalRule
  pPullRequestId_
  pApprovalRuleName_ =
    DeletePullRequestApprovalRule'
      { pullRequestId =
          pPullRequestId_,
        approvalRuleName = pApprovalRuleName_
      }

-- | The system-generated ID of the pull request that contains the approval
-- rule you want to delete.
deletePullRequestApprovalRule_pullRequestId :: Lens.Lens' DeletePullRequestApprovalRule Core.Text
deletePullRequestApprovalRule_pullRequestId = Lens.lens (\DeletePullRequestApprovalRule' {pullRequestId} -> pullRequestId) (\s@DeletePullRequestApprovalRule' {} a -> s {pullRequestId = a} :: DeletePullRequestApprovalRule)

-- | The name of the approval rule you want to delete.
deletePullRequestApprovalRule_approvalRuleName :: Lens.Lens' DeletePullRequestApprovalRule Core.Text
deletePullRequestApprovalRule_approvalRuleName = Lens.lens (\DeletePullRequestApprovalRule' {approvalRuleName} -> approvalRuleName) (\s@DeletePullRequestApprovalRule' {} a -> s {approvalRuleName = a} :: DeletePullRequestApprovalRule)

instance
  Core.AWSRequest
    DeletePullRequestApprovalRule
  where
  type
    AWSResponse DeletePullRequestApprovalRule =
      DeletePullRequestApprovalRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePullRequestApprovalRuleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "approvalRuleId")
      )

instance Core.Hashable DeletePullRequestApprovalRule

instance Core.NFData DeletePullRequestApprovalRule

instance Core.ToHeaders DeletePullRequestApprovalRule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.DeletePullRequestApprovalRule" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeletePullRequestApprovalRule where
  toJSON DeletePullRequestApprovalRule' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just
              ("approvalRuleName" Core..= approvalRuleName)
          ]
      )

instance Core.ToPath DeletePullRequestApprovalRule where
  toPath = Core.const "/"

instance Core.ToQuery DeletePullRequestApprovalRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeletePullRequestApprovalRuleResponse' smart constructor.
data DeletePullRequestApprovalRuleResponse = DeletePullRequestApprovalRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The ID of the deleted approval rule.
    --
    -- If the approval rule was deleted in an earlier API call, the response is
    -- 200 OK without content.
    approvalRuleId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePullRequestApprovalRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePullRequestApprovalRuleResponse_httpStatus' - The response's http status code.
--
-- 'approvalRuleId', 'deletePullRequestApprovalRuleResponse_approvalRuleId' - The ID of the deleted approval rule.
--
-- If the approval rule was deleted in an earlier API call, the response is
-- 200 OK without content.
newDeletePullRequestApprovalRuleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'approvalRuleId'
  Core.Text ->
  DeletePullRequestApprovalRuleResponse
newDeletePullRequestApprovalRuleResponse
  pHttpStatus_
  pApprovalRuleId_ =
    DeletePullRequestApprovalRuleResponse'
      { httpStatus =
          pHttpStatus_,
        approvalRuleId = pApprovalRuleId_
      }

-- | The response's http status code.
deletePullRequestApprovalRuleResponse_httpStatus :: Lens.Lens' DeletePullRequestApprovalRuleResponse Core.Int
deletePullRequestApprovalRuleResponse_httpStatus = Lens.lens (\DeletePullRequestApprovalRuleResponse' {httpStatus} -> httpStatus) (\s@DeletePullRequestApprovalRuleResponse' {} a -> s {httpStatus = a} :: DeletePullRequestApprovalRuleResponse)

-- | The ID of the deleted approval rule.
--
-- If the approval rule was deleted in an earlier API call, the response is
-- 200 OK without content.
deletePullRequestApprovalRuleResponse_approvalRuleId :: Lens.Lens' DeletePullRequestApprovalRuleResponse Core.Text
deletePullRequestApprovalRuleResponse_approvalRuleId = Lens.lens (\DeletePullRequestApprovalRuleResponse' {approvalRuleId} -> approvalRuleId) (\s@DeletePullRequestApprovalRuleResponse' {} a -> s {approvalRuleId = a} :: DeletePullRequestApprovalRuleResponse)

instance
  Core.NFData
    DeletePullRequestApprovalRuleResponse
