{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeCommit.CreatePullRequestApprovalRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an approval rule for a pull request.
module Network.AWS.CodeCommit.CreatePullRequestApprovalRule
  ( -- * Creating a Request
    CreatePullRequestApprovalRule (..),
    newCreatePullRequestApprovalRule,

    -- * Request Lenses
    createPullRequestApprovalRule_pullRequestId,
    createPullRequestApprovalRule_approvalRuleName,
    createPullRequestApprovalRule_approvalRuleContent,

    -- * Destructuring the Response
    CreatePullRequestApprovalRuleResponse (..),
    newCreatePullRequestApprovalRuleResponse,

    -- * Response Lenses
    createPullRequestApprovalRuleResponse_httpStatus,
    createPullRequestApprovalRuleResponse_approvalRule,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreatePullRequestApprovalRule' smart constructor.
data CreatePullRequestApprovalRule = CreatePullRequestApprovalRule'
  { -- | The system-generated ID of the pull request for which you want to create
    -- the approval rule.
    pullRequestId :: Prelude.Text,
    -- | The name for the approval rule.
    approvalRuleName :: Prelude.Text,
    -- | The content of the approval rule, including the number of approvals
    -- needed and the structure of an approval pool defined for approvals, if
    -- any. For more information about approval pools, see the AWS CodeCommit
    -- User Guide.
    --
    -- When you create the content of the approval rule, you can specify
    -- approvers in an approval pool in one of two ways:
    --
    -- -   __CodeCommitApprovers__: This option only requires an AWS account
    --     and a resource. It can be used for both IAM users and federated
    --     access users whose name matches the provided resource name. This is
    --     a very powerful option that offers a great deal of flexibility. For
    --     example, if you specify the AWS account /123456789012/ and
    --     /Mary_Major/, all of the following would be counted as approvals
    --     coming from that user:
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
    approvalRuleContent :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreatePullRequestApprovalRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pullRequestId', 'createPullRequestApprovalRule_pullRequestId' - The system-generated ID of the pull request for which you want to create
-- the approval rule.
--
-- 'approvalRuleName', 'createPullRequestApprovalRule_approvalRuleName' - The name for the approval rule.
--
-- 'approvalRuleContent', 'createPullRequestApprovalRule_approvalRuleContent' - The content of the approval rule, including the number of approvals
-- needed and the structure of an approval pool defined for approvals, if
-- any. For more information about approval pools, see the AWS CodeCommit
-- User Guide.
--
-- When you create the content of the approval rule, you can specify
-- approvers in an approval pool in one of two ways:
--
-- -   __CodeCommitApprovers__: This option only requires an AWS account
--     and a resource. It can be used for both IAM users and federated
--     access users whose name matches the provided resource name. This is
--     a very powerful option that offers a great deal of flexibility. For
--     example, if you specify the AWS account /123456789012/ and
--     /Mary_Major/, all of the following would be counted as approvals
--     coming from that user:
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
newCreatePullRequestApprovalRule ::
  -- | 'pullRequestId'
  Prelude.Text ->
  -- | 'approvalRuleName'
  Prelude.Text ->
  -- | 'approvalRuleContent'
  Prelude.Text ->
  CreatePullRequestApprovalRule
newCreatePullRequestApprovalRule
  pPullRequestId_
  pApprovalRuleName_
  pApprovalRuleContent_ =
    CreatePullRequestApprovalRule'
      { pullRequestId =
          pPullRequestId_,
        approvalRuleName = pApprovalRuleName_,
        approvalRuleContent = pApprovalRuleContent_
      }

-- | The system-generated ID of the pull request for which you want to create
-- the approval rule.
createPullRequestApprovalRule_pullRequestId :: Lens.Lens' CreatePullRequestApprovalRule Prelude.Text
createPullRequestApprovalRule_pullRequestId = Lens.lens (\CreatePullRequestApprovalRule' {pullRequestId} -> pullRequestId) (\s@CreatePullRequestApprovalRule' {} a -> s {pullRequestId = a} :: CreatePullRequestApprovalRule)

-- | The name for the approval rule.
createPullRequestApprovalRule_approvalRuleName :: Lens.Lens' CreatePullRequestApprovalRule Prelude.Text
createPullRequestApprovalRule_approvalRuleName = Lens.lens (\CreatePullRequestApprovalRule' {approvalRuleName} -> approvalRuleName) (\s@CreatePullRequestApprovalRule' {} a -> s {approvalRuleName = a} :: CreatePullRequestApprovalRule)

-- | The content of the approval rule, including the number of approvals
-- needed and the structure of an approval pool defined for approvals, if
-- any. For more information about approval pools, see the AWS CodeCommit
-- User Guide.
--
-- When you create the content of the approval rule, you can specify
-- approvers in an approval pool in one of two ways:
--
-- -   __CodeCommitApprovers__: This option only requires an AWS account
--     and a resource. It can be used for both IAM users and federated
--     access users whose name matches the provided resource name. This is
--     a very powerful option that offers a great deal of flexibility. For
--     example, if you specify the AWS account /123456789012/ and
--     /Mary_Major/, all of the following would be counted as approvals
--     coming from that user:
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
createPullRequestApprovalRule_approvalRuleContent :: Lens.Lens' CreatePullRequestApprovalRule Prelude.Text
createPullRequestApprovalRule_approvalRuleContent = Lens.lens (\CreatePullRequestApprovalRule' {approvalRuleContent} -> approvalRuleContent) (\s@CreatePullRequestApprovalRule' {} a -> s {approvalRuleContent = a} :: CreatePullRequestApprovalRule)

instance
  Prelude.AWSRequest
    CreatePullRequestApprovalRule
  where
  type
    Rs CreatePullRequestApprovalRule =
      CreatePullRequestApprovalRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePullRequestApprovalRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "approvalRule")
      )

instance
  Prelude.Hashable
    CreatePullRequestApprovalRule

instance Prelude.NFData CreatePullRequestApprovalRule

instance
  Prelude.ToHeaders
    CreatePullRequestApprovalRule
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.CreatePullRequestApprovalRule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreatePullRequestApprovalRule where
  toJSON CreatePullRequestApprovalRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pullRequestId" Prelude..= pullRequestId),
            Prelude.Just
              ("approvalRuleName" Prelude..= approvalRuleName),
            Prelude.Just
              ( "approvalRuleContent"
                  Prelude..= approvalRuleContent
              )
          ]
      )

instance Prelude.ToPath CreatePullRequestApprovalRule where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    CreatePullRequestApprovalRule
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePullRequestApprovalRuleResponse' smart constructor.
data CreatePullRequestApprovalRuleResponse = CreatePullRequestApprovalRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the created approval rule.
    approvalRule :: ApprovalRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreatePullRequestApprovalRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createPullRequestApprovalRuleResponse_httpStatus' - The response's http status code.
--
-- 'approvalRule', 'createPullRequestApprovalRuleResponse_approvalRule' - Information about the created approval rule.
newCreatePullRequestApprovalRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'approvalRule'
  ApprovalRule ->
  CreatePullRequestApprovalRuleResponse
newCreatePullRequestApprovalRuleResponse
  pHttpStatus_
  pApprovalRule_ =
    CreatePullRequestApprovalRuleResponse'
      { httpStatus =
          pHttpStatus_,
        approvalRule = pApprovalRule_
      }

-- | The response's http status code.
createPullRequestApprovalRuleResponse_httpStatus :: Lens.Lens' CreatePullRequestApprovalRuleResponse Prelude.Int
createPullRequestApprovalRuleResponse_httpStatus = Lens.lens (\CreatePullRequestApprovalRuleResponse' {httpStatus} -> httpStatus) (\s@CreatePullRequestApprovalRuleResponse' {} a -> s {httpStatus = a} :: CreatePullRequestApprovalRuleResponse)

-- | Information about the created approval rule.
createPullRequestApprovalRuleResponse_approvalRule :: Lens.Lens' CreatePullRequestApprovalRuleResponse ApprovalRule
createPullRequestApprovalRuleResponse_approvalRule = Lens.lens (\CreatePullRequestApprovalRuleResponse' {approvalRule} -> approvalRule) (\s@CreatePullRequestApprovalRuleResponse' {} a -> s {approvalRule = a} :: CreatePullRequestApprovalRuleResponse)

instance
  Prelude.NFData
    CreatePullRequestApprovalRuleResponse
