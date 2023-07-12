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
-- Module      : Amazonka.CodeCommit.UpdateApprovalRuleTemplateDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description for a specified approval rule template.
module Amazonka.CodeCommit.UpdateApprovalRuleTemplateDescription
  ( -- * Creating a Request
    UpdateApprovalRuleTemplateDescription (..),
    newUpdateApprovalRuleTemplateDescription,

    -- * Request Lenses
    updateApprovalRuleTemplateDescription_approvalRuleTemplateName,
    updateApprovalRuleTemplateDescription_approvalRuleTemplateDescription,

    -- * Destructuring the Response
    UpdateApprovalRuleTemplateDescriptionResponse (..),
    newUpdateApprovalRuleTemplateDescriptionResponse,

    -- * Response Lenses
    updateApprovalRuleTemplateDescriptionResponse_httpStatus,
    updateApprovalRuleTemplateDescriptionResponse_approvalRuleTemplate,
  )
where

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApprovalRuleTemplateDescription' smart constructor.
data UpdateApprovalRuleTemplateDescription = UpdateApprovalRuleTemplateDescription'
  { -- | The name of the template for which you want to update the description.
    approvalRuleTemplateName :: Prelude.Text,
    -- | The updated description of the approval rule template.
    approvalRuleTemplateDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApprovalRuleTemplateDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleTemplateName', 'updateApprovalRuleTemplateDescription_approvalRuleTemplateName' - The name of the template for which you want to update the description.
--
-- 'approvalRuleTemplateDescription', 'updateApprovalRuleTemplateDescription_approvalRuleTemplateDescription' - The updated description of the approval rule template.
newUpdateApprovalRuleTemplateDescription ::
  -- | 'approvalRuleTemplateName'
  Prelude.Text ->
  -- | 'approvalRuleTemplateDescription'
  Prelude.Text ->
  UpdateApprovalRuleTemplateDescription
newUpdateApprovalRuleTemplateDescription
  pApprovalRuleTemplateName_
  pApprovalRuleTemplateDescription_ =
    UpdateApprovalRuleTemplateDescription'
      { approvalRuleTemplateName =
          pApprovalRuleTemplateName_,
        approvalRuleTemplateDescription =
          pApprovalRuleTemplateDescription_
      }

-- | The name of the template for which you want to update the description.
updateApprovalRuleTemplateDescription_approvalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateDescription Prelude.Text
updateApprovalRuleTemplateDescription_approvalRuleTemplateName = Lens.lens (\UpdateApprovalRuleTemplateDescription' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@UpdateApprovalRuleTemplateDescription' {} a -> s {approvalRuleTemplateName = a} :: UpdateApprovalRuleTemplateDescription)

-- | The updated description of the approval rule template.
updateApprovalRuleTemplateDescription_approvalRuleTemplateDescription :: Lens.Lens' UpdateApprovalRuleTemplateDescription Prelude.Text
updateApprovalRuleTemplateDescription_approvalRuleTemplateDescription = Lens.lens (\UpdateApprovalRuleTemplateDescription' {approvalRuleTemplateDescription} -> approvalRuleTemplateDescription) (\s@UpdateApprovalRuleTemplateDescription' {} a -> s {approvalRuleTemplateDescription = a} :: UpdateApprovalRuleTemplateDescription)

instance
  Core.AWSRequest
    UpdateApprovalRuleTemplateDescription
  where
  type
    AWSResponse
      UpdateApprovalRuleTemplateDescription =
      UpdateApprovalRuleTemplateDescriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApprovalRuleTemplateDescriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "approvalRuleTemplate")
      )

instance
  Prelude.Hashable
    UpdateApprovalRuleTemplateDescription
  where
  hashWithSalt
    _salt
    UpdateApprovalRuleTemplateDescription' {..} =
      _salt
        `Prelude.hashWithSalt` approvalRuleTemplateName
        `Prelude.hashWithSalt` approvalRuleTemplateDescription

instance
  Prelude.NFData
    UpdateApprovalRuleTemplateDescription
  where
  rnf UpdateApprovalRuleTemplateDescription' {..} =
    Prelude.rnf approvalRuleTemplateName
      `Prelude.seq` Prelude.rnf approvalRuleTemplateDescription

instance
  Data.ToHeaders
    UpdateApprovalRuleTemplateDescription
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.UpdateApprovalRuleTemplateDescription" ::
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
    UpdateApprovalRuleTemplateDescription
  where
  toJSON UpdateApprovalRuleTemplateDescription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "approvalRuleTemplateName"
                  Data..= approvalRuleTemplateName
              ),
            Prelude.Just
              ( "approvalRuleTemplateDescription"
                  Data..= approvalRuleTemplateDescription
              )
          ]
      )

instance
  Data.ToPath
    UpdateApprovalRuleTemplateDescription
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateApprovalRuleTemplateDescription
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApprovalRuleTemplateDescriptionResponse' smart constructor.
data UpdateApprovalRuleTemplateDescriptionResponse = UpdateApprovalRuleTemplateDescriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The structure and content of the updated approval rule template.
    approvalRuleTemplate :: ApprovalRuleTemplate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApprovalRuleTemplateDescriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApprovalRuleTemplateDescriptionResponse_httpStatus' - The response's http status code.
--
-- 'approvalRuleTemplate', 'updateApprovalRuleTemplateDescriptionResponse_approvalRuleTemplate' - The structure and content of the updated approval rule template.
newUpdateApprovalRuleTemplateDescriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'approvalRuleTemplate'
  ApprovalRuleTemplate ->
  UpdateApprovalRuleTemplateDescriptionResponse
newUpdateApprovalRuleTemplateDescriptionResponse
  pHttpStatus_
  pApprovalRuleTemplate_ =
    UpdateApprovalRuleTemplateDescriptionResponse'
      { httpStatus =
          pHttpStatus_,
        approvalRuleTemplate =
          pApprovalRuleTemplate_
      }

-- | The response's http status code.
updateApprovalRuleTemplateDescriptionResponse_httpStatus :: Lens.Lens' UpdateApprovalRuleTemplateDescriptionResponse Prelude.Int
updateApprovalRuleTemplateDescriptionResponse_httpStatus = Lens.lens (\UpdateApprovalRuleTemplateDescriptionResponse' {httpStatus} -> httpStatus) (\s@UpdateApprovalRuleTemplateDescriptionResponse' {} a -> s {httpStatus = a} :: UpdateApprovalRuleTemplateDescriptionResponse)

-- | The structure and content of the updated approval rule template.
updateApprovalRuleTemplateDescriptionResponse_approvalRuleTemplate :: Lens.Lens' UpdateApprovalRuleTemplateDescriptionResponse ApprovalRuleTemplate
updateApprovalRuleTemplateDescriptionResponse_approvalRuleTemplate = Lens.lens (\UpdateApprovalRuleTemplateDescriptionResponse' {approvalRuleTemplate} -> approvalRuleTemplate) (\s@UpdateApprovalRuleTemplateDescriptionResponse' {} a -> s {approvalRuleTemplate = a} :: UpdateApprovalRuleTemplateDescriptionResponse)

instance
  Prelude.NFData
    UpdateApprovalRuleTemplateDescriptionResponse
  where
  rnf
    UpdateApprovalRuleTemplateDescriptionResponse' {..} =
      Prelude.rnf httpStatus
        `Prelude.seq` Prelude.rnf approvalRuleTemplate
