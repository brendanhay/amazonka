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
-- Module      : Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of a specified approval rule template.
module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
  ( -- * Creating a Request
    UpdateApprovalRuleTemplateName (..),
    newUpdateApprovalRuleTemplateName,

    -- * Request Lenses
    updateApprovalRuleTemplateName_oldApprovalRuleTemplateName,
    updateApprovalRuleTemplateName_newApprovalRuleTemplateName,

    -- * Destructuring the Response
    UpdateApprovalRuleTemplateNameResponse (..),
    newUpdateApprovalRuleTemplateNameResponse,

    -- * Response Lenses
    updateApprovalRuleTemplateNameResponse_httpStatus,
    updateApprovalRuleTemplateNameResponse_approvalRuleTemplate,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateApprovalRuleTemplateName' smart constructor.
data UpdateApprovalRuleTemplateName = UpdateApprovalRuleTemplateName'
  { -- | The current name of the approval rule template.
    oldApprovalRuleTemplateName :: Core.Text,
    -- | The new name you want to apply to the approval rule template.
    newApprovalRuleTemplateName' :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApprovalRuleTemplateName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oldApprovalRuleTemplateName', 'updateApprovalRuleTemplateName_oldApprovalRuleTemplateName' - The current name of the approval rule template.
--
-- 'newApprovalRuleTemplateName'', 'updateApprovalRuleTemplateName_newApprovalRuleTemplateName' - The new name you want to apply to the approval rule template.
newUpdateApprovalRuleTemplateName ::
  -- | 'oldApprovalRuleTemplateName'
  Core.Text ->
  -- | 'newApprovalRuleTemplateName''
  Core.Text ->
  UpdateApprovalRuleTemplateName
newUpdateApprovalRuleTemplateName
  pOldApprovalRuleTemplateName_
  pNewApprovalRuleTemplateName_ =
    UpdateApprovalRuleTemplateName'
      { oldApprovalRuleTemplateName =
          pOldApprovalRuleTemplateName_,
        newApprovalRuleTemplateName' =
          pNewApprovalRuleTemplateName_
      }

-- | The current name of the approval rule template.
updateApprovalRuleTemplateName_oldApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateName Core.Text
updateApprovalRuleTemplateName_oldApprovalRuleTemplateName = Lens.lens (\UpdateApprovalRuleTemplateName' {oldApprovalRuleTemplateName} -> oldApprovalRuleTemplateName) (\s@UpdateApprovalRuleTemplateName' {} a -> s {oldApprovalRuleTemplateName = a} :: UpdateApprovalRuleTemplateName)

-- | The new name you want to apply to the approval rule template.
updateApprovalRuleTemplateName_newApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateName Core.Text
updateApprovalRuleTemplateName_newApprovalRuleTemplateName = Lens.lens (\UpdateApprovalRuleTemplateName' {newApprovalRuleTemplateName'} -> newApprovalRuleTemplateName') (\s@UpdateApprovalRuleTemplateName' {} a -> s {newApprovalRuleTemplateName' = a} :: UpdateApprovalRuleTemplateName)

instance
  Core.AWSRequest
    UpdateApprovalRuleTemplateName
  where
  type
    AWSResponse UpdateApprovalRuleTemplateName =
      UpdateApprovalRuleTemplateNameResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApprovalRuleTemplateNameResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "approvalRuleTemplate")
      )

instance Core.Hashable UpdateApprovalRuleTemplateName

instance Core.NFData UpdateApprovalRuleTemplateName

instance
  Core.ToHeaders
    UpdateApprovalRuleTemplateName
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.UpdateApprovalRuleTemplateName" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateApprovalRuleTemplateName where
  toJSON UpdateApprovalRuleTemplateName' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "oldApprovalRuleTemplateName"
                  Core..= oldApprovalRuleTemplateName
              ),
            Core.Just
              ( "newApprovalRuleTemplateName"
                  Core..= newApprovalRuleTemplateName'
              )
          ]
      )

instance Core.ToPath UpdateApprovalRuleTemplateName where
  toPath = Core.const "/"

instance Core.ToQuery UpdateApprovalRuleTemplateName where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateApprovalRuleTemplateNameResponse' smart constructor.
data UpdateApprovalRuleTemplateNameResponse = UpdateApprovalRuleTemplateNameResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The structure and content of the updated approval rule template.
    approvalRuleTemplate :: ApprovalRuleTemplate
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApprovalRuleTemplateNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApprovalRuleTemplateNameResponse_httpStatus' - The response's http status code.
--
-- 'approvalRuleTemplate', 'updateApprovalRuleTemplateNameResponse_approvalRuleTemplate' - The structure and content of the updated approval rule template.
newUpdateApprovalRuleTemplateNameResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'approvalRuleTemplate'
  ApprovalRuleTemplate ->
  UpdateApprovalRuleTemplateNameResponse
newUpdateApprovalRuleTemplateNameResponse
  pHttpStatus_
  pApprovalRuleTemplate_ =
    UpdateApprovalRuleTemplateNameResponse'
      { httpStatus =
          pHttpStatus_,
        approvalRuleTemplate =
          pApprovalRuleTemplate_
      }

-- | The response's http status code.
updateApprovalRuleTemplateNameResponse_httpStatus :: Lens.Lens' UpdateApprovalRuleTemplateNameResponse Core.Int
updateApprovalRuleTemplateNameResponse_httpStatus = Lens.lens (\UpdateApprovalRuleTemplateNameResponse' {httpStatus} -> httpStatus) (\s@UpdateApprovalRuleTemplateNameResponse' {} a -> s {httpStatus = a} :: UpdateApprovalRuleTemplateNameResponse)

-- | The structure and content of the updated approval rule template.
updateApprovalRuleTemplateNameResponse_approvalRuleTemplate :: Lens.Lens' UpdateApprovalRuleTemplateNameResponse ApprovalRuleTemplate
updateApprovalRuleTemplateNameResponse_approvalRuleTemplate = Lens.lens (\UpdateApprovalRuleTemplateNameResponse' {approvalRuleTemplate} -> approvalRuleTemplate) (\s@UpdateApprovalRuleTemplateNameResponse' {} a -> s {approvalRuleTemplate = a} :: UpdateApprovalRuleTemplateNameResponse)

instance
  Core.NFData
    UpdateApprovalRuleTemplateNameResponse
