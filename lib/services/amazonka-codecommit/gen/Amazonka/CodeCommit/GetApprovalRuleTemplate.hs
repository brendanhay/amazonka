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
-- Module      : Amazonka.CodeCommit.GetApprovalRuleTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified approval rule template.
module Amazonka.CodeCommit.GetApprovalRuleTemplate
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetApprovalRuleTemplate' smart constructor.
data GetApprovalRuleTemplate = GetApprovalRuleTemplate'
  { -- | The name of the approval rule template for which you want to get
    -- information.
    approvalRuleTemplateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetApprovalRuleTemplate where
  type
    AWSResponse GetApprovalRuleTemplate =
      GetApprovalRuleTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApprovalRuleTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "approvalRuleTemplate")
      )

instance Prelude.Hashable GetApprovalRuleTemplate where
  hashWithSalt _salt GetApprovalRuleTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` approvalRuleTemplateName

instance Prelude.NFData GetApprovalRuleTemplate where
  rnf GetApprovalRuleTemplate' {..} =
    Prelude.rnf approvalRuleTemplateName

instance Data.ToHeaders GetApprovalRuleTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeCommit_20150413.GetApprovalRuleTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetApprovalRuleTemplate where
  toJSON GetApprovalRuleTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "approvalRuleTemplateName"
                  Data..= approvalRuleTemplateName
              )
          ]
      )

instance Data.ToPath GetApprovalRuleTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery GetApprovalRuleTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApprovalRuleTemplateResponse' smart constructor.
data GetApprovalRuleTemplateResponse = GetApprovalRuleTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The content and structure of the approval rule template.
    approvalRuleTemplate :: ApprovalRuleTemplate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf GetApprovalRuleTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf approvalRuleTemplate
