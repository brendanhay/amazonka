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
-- Module      : Network.AWS.CodeCommit.GetApprovalRuleTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified approval rule template.
module Network.AWS.CodeCommit.GetApprovalRuleTemplate
  ( -- * Creating a Request
    GetApprovalRuleTemplate (..),
    newGetApprovalRuleTemplate,

    -- * Request Lenses
    getApprovalRuleTemplate_approvalRuleTemplateName,

    -- * Destructuring the Response
    GetApprovalRuleTemplateResponse (..),
    newGetApprovalRuleTemplateResponse,

    -- * Response Lenses
    getApprovalRuleTemplateResponse_httpStatus,
    getApprovalRuleTemplateResponse_approvalRuleTemplate,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetApprovalRuleTemplate' smart constructor.
data GetApprovalRuleTemplate = GetApprovalRuleTemplate'
  { -- | The name of the approval rule template for which you want to get
    -- information.
    approvalRuleTemplateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetApprovalRuleTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleTemplateName', 'getApprovalRuleTemplate_approvalRuleTemplateName' - The name of the approval rule template for which you want to get
-- information.
newGetApprovalRuleTemplate ::
  -- | 'approvalRuleTemplateName'
  Prelude.Text ->
  GetApprovalRuleTemplate
newGetApprovalRuleTemplate pApprovalRuleTemplateName_ =
  GetApprovalRuleTemplate'
    { approvalRuleTemplateName =
        pApprovalRuleTemplateName_
    }

-- | The name of the approval rule template for which you want to get
-- information.
getApprovalRuleTemplate_approvalRuleTemplateName :: Lens.Lens' GetApprovalRuleTemplate Prelude.Text
getApprovalRuleTemplate_approvalRuleTemplateName = Lens.lens (\GetApprovalRuleTemplate' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@GetApprovalRuleTemplate' {} a -> s {approvalRuleTemplateName = a} :: GetApprovalRuleTemplate)

instance Prelude.AWSRequest GetApprovalRuleTemplate where
  type
    Rs GetApprovalRuleTemplate =
      GetApprovalRuleTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApprovalRuleTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "approvalRuleTemplate")
      )

instance Prelude.Hashable GetApprovalRuleTemplate

instance Prelude.NFData GetApprovalRuleTemplate

instance Prelude.ToHeaders GetApprovalRuleTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.GetApprovalRuleTemplate" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetApprovalRuleTemplate where
  toJSON GetApprovalRuleTemplate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "approvalRuleTemplateName"
                  Prelude..= approvalRuleTemplateName
              )
          ]
      )

instance Prelude.ToPath GetApprovalRuleTemplate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetApprovalRuleTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApprovalRuleTemplateResponse' smart constructor.
data GetApprovalRuleTemplateResponse = GetApprovalRuleTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The content and structure of the approval rule template.
    approvalRuleTemplate :: ApprovalRuleTemplate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetApprovalRuleTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getApprovalRuleTemplateResponse_httpStatus' - The response's http status code.
--
-- 'approvalRuleTemplate', 'getApprovalRuleTemplateResponse_approvalRuleTemplate' - The content and structure of the approval rule template.
newGetApprovalRuleTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'approvalRuleTemplate'
  ApprovalRuleTemplate ->
  GetApprovalRuleTemplateResponse
newGetApprovalRuleTemplateResponse
  pHttpStatus_
  pApprovalRuleTemplate_ =
    GetApprovalRuleTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        approvalRuleTemplate =
          pApprovalRuleTemplate_
      }

-- | The response's http status code.
getApprovalRuleTemplateResponse_httpStatus :: Lens.Lens' GetApprovalRuleTemplateResponse Prelude.Int
getApprovalRuleTemplateResponse_httpStatus = Lens.lens (\GetApprovalRuleTemplateResponse' {httpStatus} -> httpStatus) (\s@GetApprovalRuleTemplateResponse' {} a -> s {httpStatus = a} :: GetApprovalRuleTemplateResponse)

-- | The content and structure of the approval rule template.
getApprovalRuleTemplateResponse_approvalRuleTemplate :: Lens.Lens' GetApprovalRuleTemplateResponse ApprovalRuleTemplate
getApprovalRuleTemplateResponse_approvalRuleTemplate = Lens.lens (\GetApprovalRuleTemplateResponse' {approvalRuleTemplate} -> approvalRuleTemplate) (\s@GetApprovalRuleTemplateResponse' {} a -> s {approvalRuleTemplate = a} :: GetApprovalRuleTemplateResponse)

instance
  Prelude.NFData
    GetApprovalRuleTemplateResponse
