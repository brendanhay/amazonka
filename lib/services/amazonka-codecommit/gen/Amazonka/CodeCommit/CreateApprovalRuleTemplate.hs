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
-- Module      : Amazonka.CodeCommit.CreateApprovalRuleTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a template for approval rules that can then be associated with
-- one or more repositories in your AWS account. When you associate a
-- template with a repository, AWS CodeCommit creates an approval rule that
-- matches the conditions of the template for all pull requests that meet
-- the conditions of the template. For more information, see
-- AssociateApprovalRuleTemplateWithRepository.
module Amazonka.CodeCommit.CreateApprovalRuleTemplate
  ( -- * Creating a Request
    CreateApprovalRuleTemplate (..),
    newCreateApprovalRuleTemplate,

    -- * Request Lenses
    createApprovalRuleTemplate_approvalRuleTemplateDescription,
    createApprovalRuleTemplate_approvalRuleTemplateName,
    createApprovalRuleTemplate_approvalRuleTemplateContent,

    -- * Destructuring the Response
    CreateApprovalRuleTemplateResponse (..),
    newCreateApprovalRuleTemplateResponse,

    -- * Response Lenses
    createApprovalRuleTemplateResponse_httpStatus,
    createApprovalRuleTemplateResponse_approvalRuleTemplate,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApprovalRuleTemplate' smart constructor.
data CreateApprovalRuleTemplate = CreateApprovalRuleTemplate'
  { -- | The description of the approval rule template. Consider providing a
    -- description that explains what this template does and when it might be
    -- appropriate to associate it with repositories.
    approvalRuleTemplateDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the approval rule template. Provide descriptive names,
    -- because this name is applied to the approval rules created automatically
    -- in associated repositories.
    approvalRuleTemplateName :: Prelude.Text,
    -- | The content of the approval rule that is created on pull requests in
    -- associated repositories. If you specify one or more destination
    -- references (branches), approval rules are created in an associated
    -- repository only if their destination references (branches) match those
    -- specified in the template.
    --
    -- When you create the content of the approval rule template, you can
    -- specify approvers in an approval pool in one of two ways:
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
    approvalRuleTemplateContent :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApprovalRuleTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleTemplateDescription', 'createApprovalRuleTemplate_approvalRuleTemplateDescription' - The description of the approval rule template. Consider providing a
-- description that explains what this template does and when it might be
-- appropriate to associate it with repositories.
--
-- 'approvalRuleTemplateName', 'createApprovalRuleTemplate_approvalRuleTemplateName' - The name of the approval rule template. Provide descriptive names,
-- because this name is applied to the approval rules created automatically
-- in associated repositories.
--
-- 'approvalRuleTemplateContent', 'createApprovalRuleTemplate_approvalRuleTemplateContent' - The content of the approval rule that is created on pull requests in
-- associated repositories. If you specify one or more destination
-- references (branches), approval rules are created in an associated
-- repository only if their destination references (branches) match those
-- specified in the template.
--
-- When you create the content of the approval rule template, you can
-- specify approvers in an approval pool in one of two ways:
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
newCreateApprovalRuleTemplate ::
  -- | 'approvalRuleTemplateName'
  Prelude.Text ->
  -- | 'approvalRuleTemplateContent'
  Prelude.Text ->
  CreateApprovalRuleTemplate
newCreateApprovalRuleTemplate
  pApprovalRuleTemplateName_
  pApprovalRuleTemplateContent_ =
    CreateApprovalRuleTemplate'
      { approvalRuleTemplateDescription =
          Prelude.Nothing,
        approvalRuleTemplateName =
          pApprovalRuleTemplateName_,
        approvalRuleTemplateContent =
          pApprovalRuleTemplateContent_
      }

-- | The description of the approval rule template. Consider providing a
-- description that explains what this template does and when it might be
-- appropriate to associate it with repositories.
createApprovalRuleTemplate_approvalRuleTemplateDescription :: Lens.Lens' CreateApprovalRuleTemplate (Prelude.Maybe Prelude.Text)
createApprovalRuleTemplate_approvalRuleTemplateDescription = Lens.lens (\CreateApprovalRuleTemplate' {approvalRuleTemplateDescription} -> approvalRuleTemplateDescription) (\s@CreateApprovalRuleTemplate' {} a -> s {approvalRuleTemplateDescription = a} :: CreateApprovalRuleTemplate)

-- | The name of the approval rule template. Provide descriptive names,
-- because this name is applied to the approval rules created automatically
-- in associated repositories.
createApprovalRuleTemplate_approvalRuleTemplateName :: Lens.Lens' CreateApprovalRuleTemplate Prelude.Text
createApprovalRuleTemplate_approvalRuleTemplateName = Lens.lens (\CreateApprovalRuleTemplate' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@CreateApprovalRuleTemplate' {} a -> s {approvalRuleTemplateName = a} :: CreateApprovalRuleTemplate)

-- | The content of the approval rule that is created on pull requests in
-- associated repositories. If you specify one or more destination
-- references (branches), approval rules are created in an associated
-- repository only if their destination references (branches) match those
-- specified in the template.
--
-- When you create the content of the approval rule template, you can
-- specify approvers in an approval pool in one of two ways:
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
createApprovalRuleTemplate_approvalRuleTemplateContent :: Lens.Lens' CreateApprovalRuleTemplate Prelude.Text
createApprovalRuleTemplate_approvalRuleTemplateContent = Lens.lens (\CreateApprovalRuleTemplate' {approvalRuleTemplateContent} -> approvalRuleTemplateContent) (\s@CreateApprovalRuleTemplate' {} a -> s {approvalRuleTemplateContent = a} :: CreateApprovalRuleTemplate)

instance Core.AWSRequest CreateApprovalRuleTemplate where
  type
    AWSResponse CreateApprovalRuleTemplate =
      CreateApprovalRuleTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApprovalRuleTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "approvalRuleTemplate")
      )

instance Prelude.Hashable CreateApprovalRuleTemplate where
  hashWithSalt _salt CreateApprovalRuleTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` approvalRuleTemplateDescription
      `Prelude.hashWithSalt` approvalRuleTemplateName
      `Prelude.hashWithSalt` approvalRuleTemplateContent

instance Prelude.NFData CreateApprovalRuleTemplate where
  rnf CreateApprovalRuleTemplate' {..} =
    Prelude.rnf approvalRuleTemplateDescription
      `Prelude.seq` Prelude.rnf approvalRuleTemplateName
      `Prelude.seq` Prelude.rnf approvalRuleTemplateContent

instance Core.ToHeaders CreateApprovalRuleTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.CreateApprovalRuleTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateApprovalRuleTemplate where
  toJSON CreateApprovalRuleTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("approvalRuleTemplateDescription" Core..=)
              Prelude.<$> approvalRuleTemplateDescription,
            Prelude.Just
              ( "approvalRuleTemplateName"
                  Core..= approvalRuleTemplateName
              ),
            Prelude.Just
              ( "approvalRuleTemplateContent"
                  Core..= approvalRuleTemplateContent
              )
          ]
      )

instance Core.ToPath CreateApprovalRuleTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateApprovalRuleTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApprovalRuleTemplateResponse' smart constructor.
data CreateApprovalRuleTemplateResponse = CreateApprovalRuleTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The content and structure of the created approval rule template.
    approvalRuleTemplate :: ApprovalRuleTemplate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApprovalRuleTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createApprovalRuleTemplateResponse_httpStatus' - The response's http status code.
--
-- 'approvalRuleTemplate', 'createApprovalRuleTemplateResponse_approvalRuleTemplate' - The content and structure of the created approval rule template.
newCreateApprovalRuleTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'approvalRuleTemplate'
  ApprovalRuleTemplate ->
  CreateApprovalRuleTemplateResponse
newCreateApprovalRuleTemplateResponse
  pHttpStatus_
  pApprovalRuleTemplate_ =
    CreateApprovalRuleTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        approvalRuleTemplate =
          pApprovalRuleTemplate_
      }

-- | The response's http status code.
createApprovalRuleTemplateResponse_httpStatus :: Lens.Lens' CreateApprovalRuleTemplateResponse Prelude.Int
createApprovalRuleTemplateResponse_httpStatus = Lens.lens (\CreateApprovalRuleTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateApprovalRuleTemplateResponse' {} a -> s {httpStatus = a} :: CreateApprovalRuleTemplateResponse)

-- | The content and structure of the created approval rule template.
createApprovalRuleTemplateResponse_approvalRuleTemplate :: Lens.Lens' CreateApprovalRuleTemplateResponse ApprovalRuleTemplate
createApprovalRuleTemplateResponse_approvalRuleTemplate = Lens.lens (\CreateApprovalRuleTemplateResponse' {approvalRuleTemplate} -> approvalRuleTemplate) (\s@CreateApprovalRuleTemplateResponse' {} a -> s {approvalRuleTemplate = a} :: CreateApprovalRuleTemplateResponse)

instance
  Prelude.NFData
    CreateApprovalRuleTemplateResponse
  where
  rnf CreateApprovalRuleTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf approvalRuleTemplate
