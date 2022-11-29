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
-- Module      : Amazonka.CodeCommit.UpdateApprovalRuleTemplateName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of a specified approval rule template.
module Amazonka.CodeCommit.UpdateApprovalRuleTemplateName
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApprovalRuleTemplateName' smart constructor.
data UpdateApprovalRuleTemplateName = UpdateApprovalRuleTemplateName'
  { -- | The current name of the approval rule template.
    oldApprovalRuleTemplateName :: Prelude.Text,
    -- | The new name you want to apply to the approval rule template.
    newApprovalRuleTemplateName' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'newApprovalRuleTemplateName''
  Prelude.Text ->
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
updateApprovalRuleTemplateName_oldApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateName Prelude.Text
updateApprovalRuleTemplateName_oldApprovalRuleTemplateName = Lens.lens (\UpdateApprovalRuleTemplateName' {oldApprovalRuleTemplateName} -> oldApprovalRuleTemplateName) (\s@UpdateApprovalRuleTemplateName' {} a -> s {oldApprovalRuleTemplateName = a} :: UpdateApprovalRuleTemplateName)

-- | The new name you want to apply to the approval rule template.
updateApprovalRuleTemplateName_newApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateName Prelude.Text
updateApprovalRuleTemplateName_newApprovalRuleTemplateName = Lens.lens (\UpdateApprovalRuleTemplateName' {newApprovalRuleTemplateName'} -> newApprovalRuleTemplateName') (\s@UpdateApprovalRuleTemplateName' {} a -> s {newApprovalRuleTemplateName' = a} :: UpdateApprovalRuleTemplateName)

instance
  Core.AWSRequest
    UpdateApprovalRuleTemplateName
  where
  type
    AWSResponse UpdateApprovalRuleTemplateName =
      UpdateApprovalRuleTemplateNameResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApprovalRuleTemplateNameResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "approvalRuleTemplate")
      )

instance
  Prelude.Hashable
    UpdateApprovalRuleTemplateName
  where
  hashWithSalt
    _salt
    UpdateApprovalRuleTemplateName' {..} =
      _salt
        `Prelude.hashWithSalt` oldApprovalRuleTemplateName
        `Prelude.hashWithSalt` newApprovalRuleTemplateName'

instance
  Prelude.NFData
    UpdateApprovalRuleTemplateName
  where
  rnf UpdateApprovalRuleTemplateName' {..} =
    Prelude.rnf oldApprovalRuleTemplateName
      `Prelude.seq` Prelude.rnf newApprovalRuleTemplateName'

instance
  Core.ToHeaders
    UpdateApprovalRuleTemplateName
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.UpdateApprovalRuleTemplateName" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateApprovalRuleTemplateName where
  toJSON UpdateApprovalRuleTemplateName' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "oldApprovalRuleTemplateName"
                  Core..= oldApprovalRuleTemplateName
              ),
            Prelude.Just
              ( "newApprovalRuleTemplateName"
                  Core..= newApprovalRuleTemplateName'
              )
          ]
      )

instance Core.ToPath UpdateApprovalRuleTemplateName where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateApprovalRuleTemplateName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApprovalRuleTemplateNameResponse' smart constructor.
data UpdateApprovalRuleTemplateNameResponse = UpdateApprovalRuleTemplateNameResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The structure and content of the updated approval rule template.
    approvalRuleTemplate :: ApprovalRuleTemplate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
updateApprovalRuleTemplateNameResponse_httpStatus :: Lens.Lens' UpdateApprovalRuleTemplateNameResponse Prelude.Int
updateApprovalRuleTemplateNameResponse_httpStatus = Lens.lens (\UpdateApprovalRuleTemplateNameResponse' {httpStatus} -> httpStatus) (\s@UpdateApprovalRuleTemplateNameResponse' {} a -> s {httpStatus = a} :: UpdateApprovalRuleTemplateNameResponse)

-- | The structure and content of the updated approval rule template.
updateApprovalRuleTemplateNameResponse_approvalRuleTemplate :: Lens.Lens' UpdateApprovalRuleTemplateNameResponse ApprovalRuleTemplate
updateApprovalRuleTemplateNameResponse_approvalRuleTemplate = Lens.lens (\UpdateApprovalRuleTemplateNameResponse' {approvalRuleTemplate} -> approvalRuleTemplate) (\s@UpdateApprovalRuleTemplateNameResponse' {} a -> s {approvalRuleTemplate = a} :: UpdateApprovalRuleTemplateNameResponse)

instance
  Prelude.NFData
    UpdateApprovalRuleTemplateNameResponse
  where
  rnf UpdateApprovalRuleTemplateNameResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf approvalRuleTemplate
