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
-- Module      : Amazonka.CodeCommit.DeletePullRequestApprovalRule
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CodeCommit.DeletePullRequestApprovalRule
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePullRequestApprovalRule' smart constructor.
data DeletePullRequestApprovalRule = DeletePullRequestApprovalRule'
  { -- | The system-generated ID of the pull request that contains the approval
    -- rule you want to delete.
    pullRequestId :: Prelude.Text,
    -- | The name of the approval rule you want to delete.
    approvalRuleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'approvalRuleName'
  Prelude.Text ->
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
deletePullRequestApprovalRule_pullRequestId :: Lens.Lens' DeletePullRequestApprovalRule Prelude.Text
deletePullRequestApprovalRule_pullRequestId = Lens.lens (\DeletePullRequestApprovalRule' {pullRequestId} -> pullRequestId) (\s@DeletePullRequestApprovalRule' {} a -> s {pullRequestId = a} :: DeletePullRequestApprovalRule)

-- | The name of the approval rule you want to delete.
deletePullRequestApprovalRule_approvalRuleName :: Lens.Lens' DeletePullRequestApprovalRule Prelude.Text
deletePullRequestApprovalRule_approvalRuleName = Lens.lens (\DeletePullRequestApprovalRule' {approvalRuleName} -> approvalRuleName) (\s@DeletePullRequestApprovalRule' {} a -> s {approvalRuleName = a} :: DeletePullRequestApprovalRule)

instance
  Core.AWSRequest
    DeletePullRequestApprovalRule
  where
  type
    AWSResponse DeletePullRequestApprovalRule =
      DeletePullRequestApprovalRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePullRequestApprovalRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "approvalRuleId")
      )

instance
  Prelude.Hashable
    DeletePullRequestApprovalRule
  where
  hashWithSalt _salt DeletePullRequestApprovalRule' {..} =
    _salt `Prelude.hashWithSalt` pullRequestId
      `Prelude.hashWithSalt` approvalRuleName

instance Prelude.NFData DeletePullRequestApprovalRule where
  rnf DeletePullRequestApprovalRule' {..} =
    Prelude.rnf pullRequestId
      `Prelude.seq` Prelude.rnf approvalRuleName

instance Data.ToHeaders DeletePullRequestApprovalRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.DeletePullRequestApprovalRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePullRequestApprovalRule where
  toJSON DeletePullRequestApprovalRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pullRequestId" Data..= pullRequestId),
            Prelude.Just
              ("approvalRuleName" Data..= approvalRuleName)
          ]
      )

instance Data.ToPath DeletePullRequestApprovalRule where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePullRequestApprovalRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePullRequestApprovalRuleResponse' smart constructor.
data DeletePullRequestApprovalRuleResponse = DeletePullRequestApprovalRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the deleted approval rule.
    --
    -- If the approval rule was deleted in an earlier API call, the response is
    -- 200 OK without content.
    approvalRuleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'approvalRuleId'
  Prelude.Text ->
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
deletePullRequestApprovalRuleResponse_httpStatus :: Lens.Lens' DeletePullRequestApprovalRuleResponse Prelude.Int
deletePullRequestApprovalRuleResponse_httpStatus = Lens.lens (\DeletePullRequestApprovalRuleResponse' {httpStatus} -> httpStatus) (\s@DeletePullRequestApprovalRuleResponse' {} a -> s {httpStatus = a} :: DeletePullRequestApprovalRuleResponse)

-- | The ID of the deleted approval rule.
--
-- If the approval rule was deleted in an earlier API call, the response is
-- 200 OK without content.
deletePullRequestApprovalRuleResponse_approvalRuleId :: Lens.Lens' DeletePullRequestApprovalRuleResponse Prelude.Text
deletePullRequestApprovalRuleResponse_approvalRuleId = Lens.lens (\DeletePullRequestApprovalRuleResponse' {approvalRuleId} -> approvalRuleId) (\s@DeletePullRequestApprovalRuleResponse' {} a -> s {approvalRuleId = a} :: DeletePullRequestApprovalRuleResponse)

instance
  Prelude.NFData
    DeletePullRequestApprovalRuleResponse
  where
  rnf DeletePullRequestApprovalRuleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf approvalRuleId
