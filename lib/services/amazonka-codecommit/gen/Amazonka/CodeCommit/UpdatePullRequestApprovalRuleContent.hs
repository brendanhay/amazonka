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
-- Module      : Amazonka.CodeCommit.UpdatePullRequestApprovalRuleContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the structure of an approval rule created specifically for a
-- pull request. For example, you can change the number of required
-- approvers and the approval pool for approvers.
module Amazonka.CodeCommit.UpdatePullRequestApprovalRuleContent
  ( -- * Creating a Request
    UpdatePullRequestApprovalRuleContent (..),
    newUpdatePullRequestApprovalRuleContent,

    -- * Request Lenses
    updatePullRequestApprovalRuleContent_existingRuleContentSha256,
    updatePullRequestApprovalRuleContent_pullRequestId,
    updatePullRequestApprovalRuleContent_approvalRuleName,
    updatePullRequestApprovalRuleContent_newRuleContent,

    -- * Destructuring the Response
    UpdatePullRequestApprovalRuleContentResponse (..),
    newUpdatePullRequestApprovalRuleContentResponse,

    -- * Response Lenses
    updatePullRequestApprovalRuleContentResponse_httpStatus,
    updatePullRequestApprovalRuleContentResponse_approvalRule,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePullRequestApprovalRuleContent' smart constructor.
data UpdatePullRequestApprovalRuleContent = UpdatePullRequestApprovalRuleContent'
  { -- | The SHA-256 hash signature for the content of the approval rule. You can
    -- retrieve this information by using GetPullRequest.
    existingRuleContentSha256 :: Prelude.Maybe Prelude.Text,
    -- | The system-generated ID of the pull request.
    pullRequestId :: Prelude.Text,
    -- | The name of the approval rule you want to update.
    approvalRuleName :: Prelude.Text,
    -- | The updated content for the approval rule.
    --
    -- When you update the content of the approval rule, you can specify
    -- approvers in an approval pool in one of two ways:
    --
    -- -   __CodeCommitApprovers__: This option only requires an AWS account
    --     and a resource. It can be used for both IAM users and federated
    --     access users whose name matches the provided resource name. This is
    --     a very powerful option that offers a great deal of flexibility. For
    --     example, if you specify the AWS account /123456789012/ and
    --     /Mary_Major/, all of the following are counted as approvals coming
    --     from that user:
    --
    --     -   An IAM user in the account
    --         (arn:aws:iam::/123456789012/:user\//Mary_Major/)
    --
    --     -   A federated user identified in IAM as Mary_Major
    --         (arn:aws:sts::/123456789012/:federated-user\//Mary_Major/)
    --
    --     This option does not recognize an active session of someone assuming
    --     the role of CodeCommitReview with a role session name of
    --     /Mary_Major/
    --     (arn:aws:sts::/123456789012/:assumed-role\/CodeCommitReview\//Mary_Major/)
    --     unless you include a wildcard (*Mary_Major).
    --
    -- -   __Fully qualified ARN__: This option allows you to specify the fully
    --     qualified Amazon Resource Name (ARN) of the IAM user or role.
    --
    -- For more information about IAM ARNs, wildcards, and formats, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers>
    -- in the /IAM User Guide/.
    newRuleContent' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePullRequestApprovalRuleContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'existingRuleContentSha256', 'updatePullRequestApprovalRuleContent_existingRuleContentSha256' - The SHA-256 hash signature for the content of the approval rule. You can
-- retrieve this information by using GetPullRequest.
--
-- 'pullRequestId', 'updatePullRequestApprovalRuleContent_pullRequestId' - The system-generated ID of the pull request.
--
-- 'approvalRuleName', 'updatePullRequestApprovalRuleContent_approvalRuleName' - The name of the approval rule you want to update.
--
-- 'newRuleContent'', 'updatePullRequestApprovalRuleContent_newRuleContent' - The updated content for the approval rule.
--
-- When you update the content of the approval rule, you can specify
-- approvers in an approval pool in one of two ways:
--
-- -   __CodeCommitApprovers__: This option only requires an AWS account
--     and a resource. It can be used for both IAM users and federated
--     access users whose name matches the provided resource name. This is
--     a very powerful option that offers a great deal of flexibility. For
--     example, if you specify the AWS account /123456789012/ and
--     /Mary_Major/, all of the following are counted as approvals coming
--     from that user:
--
--     -   An IAM user in the account
--         (arn:aws:iam::/123456789012/:user\//Mary_Major/)
--
--     -   A federated user identified in IAM as Mary_Major
--         (arn:aws:sts::/123456789012/:federated-user\//Mary_Major/)
--
--     This option does not recognize an active session of someone assuming
--     the role of CodeCommitReview with a role session name of
--     /Mary_Major/
--     (arn:aws:sts::/123456789012/:assumed-role\/CodeCommitReview\//Mary_Major/)
--     unless you include a wildcard (*Mary_Major).
--
-- -   __Fully qualified ARN__: This option allows you to specify the fully
--     qualified Amazon Resource Name (ARN) of the IAM user or role.
--
-- For more information about IAM ARNs, wildcards, and formats, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers>
-- in the /IAM User Guide/.
newUpdatePullRequestApprovalRuleContent ::
  -- | 'pullRequestId'
  Prelude.Text ->
  -- | 'approvalRuleName'
  Prelude.Text ->
  -- | 'newRuleContent''
  Prelude.Text ->
  UpdatePullRequestApprovalRuleContent
newUpdatePullRequestApprovalRuleContent
  pPullRequestId_
  pApprovalRuleName_
  pNewRuleContent_ =
    UpdatePullRequestApprovalRuleContent'
      { existingRuleContentSha256 =
          Prelude.Nothing,
        pullRequestId = pPullRequestId_,
        approvalRuleName = pApprovalRuleName_,
        newRuleContent' = pNewRuleContent_
      }

-- | The SHA-256 hash signature for the content of the approval rule. You can
-- retrieve this information by using GetPullRequest.
updatePullRequestApprovalRuleContent_existingRuleContentSha256 :: Lens.Lens' UpdatePullRequestApprovalRuleContent (Prelude.Maybe Prelude.Text)
updatePullRequestApprovalRuleContent_existingRuleContentSha256 = Lens.lens (\UpdatePullRequestApprovalRuleContent' {existingRuleContentSha256} -> existingRuleContentSha256) (\s@UpdatePullRequestApprovalRuleContent' {} a -> s {existingRuleContentSha256 = a} :: UpdatePullRequestApprovalRuleContent)

-- | The system-generated ID of the pull request.
updatePullRequestApprovalRuleContent_pullRequestId :: Lens.Lens' UpdatePullRequestApprovalRuleContent Prelude.Text
updatePullRequestApprovalRuleContent_pullRequestId = Lens.lens (\UpdatePullRequestApprovalRuleContent' {pullRequestId} -> pullRequestId) (\s@UpdatePullRequestApprovalRuleContent' {} a -> s {pullRequestId = a} :: UpdatePullRequestApprovalRuleContent)

-- | The name of the approval rule you want to update.
updatePullRequestApprovalRuleContent_approvalRuleName :: Lens.Lens' UpdatePullRequestApprovalRuleContent Prelude.Text
updatePullRequestApprovalRuleContent_approvalRuleName = Lens.lens (\UpdatePullRequestApprovalRuleContent' {approvalRuleName} -> approvalRuleName) (\s@UpdatePullRequestApprovalRuleContent' {} a -> s {approvalRuleName = a} :: UpdatePullRequestApprovalRuleContent)

-- | The updated content for the approval rule.
--
-- When you update the content of the approval rule, you can specify
-- approvers in an approval pool in one of two ways:
--
-- -   __CodeCommitApprovers__: This option only requires an AWS account
--     and a resource. It can be used for both IAM users and federated
--     access users whose name matches the provided resource name. This is
--     a very powerful option that offers a great deal of flexibility. For
--     example, if you specify the AWS account /123456789012/ and
--     /Mary_Major/, all of the following are counted as approvals coming
--     from that user:
--
--     -   An IAM user in the account
--         (arn:aws:iam::/123456789012/:user\//Mary_Major/)
--
--     -   A federated user identified in IAM as Mary_Major
--         (arn:aws:sts::/123456789012/:federated-user\//Mary_Major/)
--
--     This option does not recognize an active session of someone assuming
--     the role of CodeCommitReview with a role session name of
--     /Mary_Major/
--     (arn:aws:sts::/123456789012/:assumed-role\/CodeCommitReview\//Mary_Major/)
--     unless you include a wildcard (*Mary_Major).
--
-- -   __Fully qualified ARN__: This option allows you to specify the fully
--     qualified Amazon Resource Name (ARN) of the IAM user or role.
--
-- For more information about IAM ARNs, wildcards, and formats, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers>
-- in the /IAM User Guide/.
updatePullRequestApprovalRuleContent_newRuleContent :: Lens.Lens' UpdatePullRequestApprovalRuleContent Prelude.Text
updatePullRequestApprovalRuleContent_newRuleContent = Lens.lens (\UpdatePullRequestApprovalRuleContent' {newRuleContent'} -> newRuleContent') (\s@UpdatePullRequestApprovalRuleContent' {} a -> s {newRuleContent' = a} :: UpdatePullRequestApprovalRuleContent)

instance
  Core.AWSRequest
    UpdatePullRequestApprovalRuleContent
  where
  type
    AWSResponse UpdatePullRequestApprovalRuleContent =
      UpdatePullRequestApprovalRuleContentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePullRequestApprovalRuleContentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..:> "approvalRule")
      )

instance
  Prelude.Hashable
    UpdatePullRequestApprovalRuleContent
  where
  hashWithSalt
    _salt
    UpdatePullRequestApprovalRuleContent' {..} =
      _salt
        `Prelude.hashWithSalt` existingRuleContentSha256
        `Prelude.hashWithSalt` pullRequestId
        `Prelude.hashWithSalt` approvalRuleName
        `Prelude.hashWithSalt` newRuleContent'

instance
  Prelude.NFData
    UpdatePullRequestApprovalRuleContent
  where
  rnf UpdatePullRequestApprovalRuleContent' {..} =
    Prelude.rnf existingRuleContentSha256
      `Prelude.seq` Prelude.rnf pullRequestId
      `Prelude.seq` Prelude.rnf approvalRuleName
      `Prelude.seq` Prelude.rnf newRuleContent'

instance
  Data.ToHeaders
    UpdatePullRequestApprovalRuleContent
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.UpdatePullRequestApprovalRuleContent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdatePullRequestApprovalRuleContent
  where
  toJSON UpdatePullRequestApprovalRuleContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("existingRuleContentSha256" Data..=)
              Prelude.<$> existingRuleContentSha256,
            Prelude.Just ("pullRequestId" Data..= pullRequestId),
            Prelude.Just
              ("approvalRuleName" Data..= approvalRuleName),
            Prelude.Just
              ("newRuleContent" Data..= newRuleContent')
          ]
      )

instance
  Data.ToPath
    UpdatePullRequestApprovalRuleContent
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdatePullRequestApprovalRuleContent
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePullRequestApprovalRuleContentResponse' smart constructor.
data UpdatePullRequestApprovalRuleContentResponse = UpdatePullRequestApprovalRuleContentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the updated approval rule.
    approvalRule :: ApprovalRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePullRequestApprovalRuleContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePullRequestApprovalRuleContentResponse_httpStatus' - The response's http status code.
--
-- 'approvalRule', 'updatePullRequestApprovalRuleContentResponse_approvalRule' - Information about the updated approval rule.
newUpdatePullRequestApprovalRuleContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'approvalRule'
  ApprovalRule ->
  UpdatePullRequestApprovalRuleContentResponse
newUpdatePullRequestApprovalRuleContentResponse
  pHttpStatus_
  pApprovalRule_ =
    UpdatePullRequestApprovalRuleContentResponse'
      { httpStatus =
          pHttpStatus_,
        approvalRule = pApprovalRule_
      }

-- | The response's http status code.
updatePullRequestApprovalRuleContentResponse_httpStatus :: Lens.Lens' UpdatePullRequestApprovalRuleContentResponse Prelude.Int
updatePullRequestApprovalRuleContentResponse_httpStatus = Lens.lens (\UpdatePullRequestApprovalRuleContentResponse' {httpStatus} -> httpStatus) (\s@UpdatePullRequestApprovalRuleContentResponse' {} a -> s {httpStatus = a} :: UpdatePullRequestApprovalRuleContentResponse)

-- | Information about the updated approval rule.
updatePullRequestApprovalRuleContentResponse_approvalRule :: Lens.Lens' UpdatePullRequestApprovalRuleContentResponse ApprovalRule
updatePullRequestApprovalRuleContentResponse_approvalRule = Lens.lens (\UpdatePullRequestApprovalRuleContentResponse' {approvalRule} -> approvalRule) (\s@UpdatePullRequestApprovalRuleContentResponse' {} a -> s {approvalRule = a} :: UpdatePullRequestApprovalRuleContentResponse)

instance
  Prelude.NFData
    UpdatePullRequestApprovalRuleContentResponse
  where
  rnf UpdatePullRequestApprovalRuleContentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf approvalRule
