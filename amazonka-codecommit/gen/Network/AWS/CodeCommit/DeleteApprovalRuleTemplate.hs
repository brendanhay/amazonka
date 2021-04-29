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
-- Module      : Network.AWS.CodeCommit.DeleteApprovalRuleTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified approval rule template. Deleting a template does not
-- remove approval rules on pull requests already created with the
-- template.
module Network.AWS.CodeCommit.DeleteApprovalRuleTemplate
  ( -- * Creating a Request
    DeleteApprovalRuleTemplate (..),
    newDeleteApprovalRuleTemplate,

    -- * Request Lenses
    deleteApprovalRuleTemplate_approvalRuleTemplateName,

    -- * Destructuring the Response
    DeleteApprovalRuleTemplateResponse (..),
    newDeleteApprovalRuleTemplateResponse,

    -- * Response Lenses
    deleteApprovalRuleTemplateResponse_httpStatus,
    deleteApprovalRuleTemplateResponse_approvalRuleTemplateId,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteApprovalRuleTemplate' smart constructor.
data DeleteApprovalRuleTemplate = DeleteApprovalRuleTemplate'
  { -- | The name of the approval rule template to delete.
    approvalRuleTemplateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteApprovalRuleTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleTemplateName', 'deleteApprovalRuleTemplate_approvalRuleTemplateName' - The name of the approval rule template to delete.
newDeleteApprovalRuleTemplate ::
  -- | 'approvalRuleTemplateName'
  Prelude.Text ->
  DeleteApprovalRuleTemplate
newDeleteApprovalRuleTemplate
  pApprovalRuleTemplateName_ =
    DeleteApprovalRuleTemplate'
      { approvalRuleTemplateName =
          pApprovalRuleTemplateName_
      }

-- | The name of the approval rule template to delete.
deleteApprovalRuleTemplate_approvalRuleTemplateName :: Lens.Lens' DeleteApprovalRuleTemplate Prelude.Text
deleteApprovalRuleTemplate_approvalRuleTemplateName = Lens.lens (\DeleteApprovalRuleTemplate' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@DeleteApprovalRuleTemplate' {} a -> s {approvalRuleTemplateName = a} :: DeleteApprovalRuleTemplate)

instance
  Prelude.AWSRequest
    DeleteApprovalRuleTemplate
  where
  type
    Rs DeleteApprovalRuleTemplate =
      DeleteApprovalRuleTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteApprovalRuleTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "approvalRuleTemplateId")
      )

instance Prelude.Hashable DeleteApprovalRuleTemplate

instance Prelude.NFData DeleteApprovalRuleTemplate

instance Prelude.ToHeaders DeleteApprovalRuleTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.DeleteApprovalRuleTemplate" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteApprovalRuleTemplate where
  toJSON DeleteApprovalRuleTemplate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "approvalRuleTemplateName"
                  Prelude..= approvalRuleTemplateName
              )
          ]
      )

instance Prelude.ToPath DeleteApprovalRuleTemplate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteApprovalRuleTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApprovalRuleTemplateResponse' smart constructor.
data DeleteApprovalRuleTemplateResponse = DeleteApprovalRuleTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The system-generated ID of the deleted approval rule template. If the
    -- template has been previously deleted, the only response is a 200 OK.
    approvalRuleTemplateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteApprovalRuleTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApprovalRuleTemplateResponse_httpStatus' - The response's http status code.
--
-- 'approvalRuleTemplateId', 'deleteApprovalRuleTemplateResponse_approvalRuleTemplateId' - The system-generated ID of the deleted approval rule template. If the
-- template has been previously deleted, the only response is a 200 OK.
newDeleteApprovalRuleTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'approvalRuleTemplateId'
  Prelude.Text ->
  DeleteApprovalRuleTemplateResponse
newDeleteApprovalRuleTemplateResponse
  pHttpStatus_
  pApprovalRuleTemplateId_ =
    DeleteApprovalRuleTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        approvalRuleTemplateId =
          pApprovalRuleTemplateId_
      }

-- | The response's http status code.
deleteApprovalRuleTemplateResponse_httpStatus :: Lens.Lens' DeleteApprovalRuleTemplateResponse Prelude.Int
deleteApprovalRuleTemplateResponse_httpStatus = Lens.lens (\DeleteApprovalRuleTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteApprovalRuleTemplateResponse' {} a -> s {httpStatus = a} :: DeleteApprovalRuleTemplateResponse)

-- | The system-generated ID of the deleted approval rule template. If the
-- template has been previously deleted, the only response is a 200 OK.
deleteApprovalRuleTemplateResponse_approvalRuleTemplateId :: Lens.Lens' DeleteApprovalRuleTemplateResponse Prelude.Text
deleteApprovalRuleTemplateResponse_approvalRuleTemplateId = Lens.lens (\DeleteApprovalRuleTemplateResponse' {approvalRuleTemplateId} -> approvalRuleTemplateId) (\s@DeleteApprovalRuleTemplateResponse' {} a -> s {approvalRuleTemplateId = a} :: DeleteApprovalRuleTemplateResponse)

instance
  Prelude.NFData
    DeleteApprovalRuleTemplateResponse
