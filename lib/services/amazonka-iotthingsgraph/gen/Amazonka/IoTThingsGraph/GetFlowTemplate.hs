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
-- Module      : Amazonka.IoTThingsGraph.GetFlowTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the latest version of the @DefinitionDocument@ and
-- @FlowTemplateSummary@ for the specified workflow.
module Amazonka.IoTThingsGraph.GetFlowTemplate
  ( -- * Creating a Request
    GetFlowTemplate (..),
    newGetFlowTemplate,

    -- * Request Lenses
    getFlowTemplate_revisionNumber,
    getFlowTemplate_id,

    -- * Destructuring the Response
    GetFlowTemplateResponse (..),
    newGetFlowTemplateResponse,

    -- * Response Lenses
    getFlowTemplateResponse_description,
    getFlowTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFlowTemplate' smart constructor.
data GetFlowTemplate = GetFlowTemplate'
  { -- | The number of the workflow revision to retrieve.
    revisionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the workflow.
    --
    -- The ID should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFlowTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionNumber', 'getFlowTemplate_revisionNumber' - The number of the workflow revision to retrieve.
--
-- 'id', 'getFlowTemplate_id' - The ID of the workflow.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
newGetFlowTemplate ::
  -- | 'id'
  Prelude.Text ->
  GetFlowTemplate
newGetFlowTemplate pId_ =
  GetFlowTemplate'
    { revisionNumber = Prelude.Nothing,
      id = pId_
    }

-- | The number of the workflow revision to retrieve.
getFlowTemplate_revisionNumber :: Lens.Lens' GetFlowTemplate (Prelude.Maybe Prelude.Integer)
getFlowTemplate_revisionNumber = Lens.lens (\GetFlowTemplate' {revisionNumber} -> revisionNumber) (\s@GetFlowTemplate' {} a -> s {revisionNumber = a} :: GetFlowTemplate)

-- | The ID of the workflow.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
getFlowTemplate_id :: Lens.Lens' GetFlowTemplate Prelude.Text
getFlowTemplate_id = Lens.lens (\GetFlowTemplate' {id} -> id) (\s@GetFlowTemplate' {} a -> s {id = a} :: GetFlowTemplate)

instance Core.AWSRequest GetFlowTemplate where
  type
    AWSResponse GetFlowTemplate =
      GetFlowTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFlowTemplateResponse'
            Prelude.<$> (x Core..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFlowTemplate

instance Prelude.NFData GetFlowTemplate

instance Core.ToHeaders GetFlowTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.GetFlowTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetFlowTemplate where
  toJSON GetFlowTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("revisionNumber" Core..=)
              Prelude.<$> revisionNumber,
            Prelude.Just ("id" Core..= id)
          ]
      )

instance Core.ToPath GetFlowTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery GetFlowTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFlowTemplateResponse' smart constructor.
data GetFlowTemplateResponse = GetFlowTemplateResponse'
  { -- | The object that describes the specified workflow.
    description :: Prelude.Maybe FlowTemplateDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFlowTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getFlowTemplateResponse_description' - The object that describes the specified workflow.
--
-- 'httpStatus', 'getFlowTemplateResponse_httpStatus' - The response's http status code.
newGetFlowTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFlowTemplateResponse
newGetFlowTemplateResponse pHttpStatus_ =
  GetFlowTemplateResponse'
    { description =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The object that describes the specified workflow.
getFlowTemplateResponse_description :: Lens.Lens' GetFlowTemplateResponse (Prelude.Maybe FlowTemplateDescription)
getFlowTemplateResponse_description = Lens.lens (\GetFlowTemplateResponse' {description} -> description) (\s@GetFlowTemplateResponse' {} a -> s {description = a} :: GetFlowTemplateResponse)

-- | The response's http status code.
getFlowTemplateResponse_httpStatus :: Lens.Lens' GetFlowTemplateResponse Prelude.Int
getFlowTemplateResponse_httpStatus = Lens.lens (\GetFlowTemplateResponse' {httpStatus} -> httpStatus) (\s@GetFlowTemplateResponse' {} a -> s {httpStatus = a} :: GetFlowTemplateResponse)

instance Prelude.NFData GetFlowTemplateResponse
