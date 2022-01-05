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
-- Module      : Amazonka.IoTThingsGraph.UpdateFlowTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified workflow. All deployed systems and system
-- instances that use the workflow will see the changes in the flow when it
-- is redeployed. If you don\'t want this behavior, copy the workflow
-- (creating a new workflow with a different ID), and update the copy. The
-- workflow can contain only entities in the specified namespace.
module Amazonka.IoTThingsGraph.UpdateFlowTemplate
  ( -- * Creating a Request
    UpdateFlowTemplate (..),
    newUpdateFlowTemplate,

    -- * Request Lenses
    updateFlowTemplate_compatibleNamespaceVersion,
    updateFlowTemplate_id,
    updateFlowTemplate_definition,

    -- * Destructuring the Response
    UpdateFlowTemplateResponse (..),
    newUpdateFlowTemplateResponse,

    -- * Response Lenses
    updateFlowTemplateResponse_summary,
    updateFlowTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFlowTemplate' smart constructor.
data UpdateFlowTemplate = UpdateFlowTemplate'
  { -- | The version of the user\'s namespace.
    --
    -- If no value is specified, the latest version is used by default. Use the
    -- @GetFlowTemplateRevisions@ if you want to find earlier revisions of the
    -- flow to update.
    compatibleNamespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the workflow to be updated.
    --
    -- The ID should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
    id :: Prelude.Text,
    -- | The @DefinitionDocument@ that contains the updated workflow definition.
    definition :: DefinitionDocument
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFlowTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleNamespaceVersion', 'updateFlowTemplate_compatibleNamespaceVersion' - The version of the user\'s namespace.
--
-- If no value is specified, the latest version is used by default. Use the
-- @GetFlowTemplateRevisions@ if you want to find earlier revisions of the
-- flow to update.
--
-- 'id', 'updateFlowTemplate_id' - The ID of the workflow to be updated.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
--
-- 'definition', 'updateFlowTemplate_definition' - The @DefinitionDocument@ that contains the updated workflow definition.
newUpdateFlowTemplate ::
  -- | 'id'
  Prelude.Text ->
  -- | 'definition'
  DefinitionDocument ->
  UpdateFlowTemplate
newUpdateFlowTemplate pId_ pDefinition_ =
  UpdateFlowTemplate'
    { compatibleNamespaceVersion =
        Prelude.Nothing,
      id = pId_,
      definition = pDefinition_
    }

-- | The version of the user\'s namespace.
--
-- If no value is specified, the latest version is used by default. Use the
-- @GetFlowTemplateRevisions@ if you want to find earlier revisions of the
-- flow to update.
updateFlowTemplate_compatibleNamespaceVersion :: Lens.Lens' UpdateFlowTemplate (Prelude.Maybe Prelude.Integer)
updateFlowTemplate_compatibleNamespaceVersion = Lens.lens (\UpdateFlowTemplate' {compatibleNamespaceVersion} -> compatibleNamespaceVersion) (\s@UpdateFlowTemplate' {} a -> s {compatibleNamespaceVersion = a} :: UpdateFlowTemplate)

-- | The ID of the workflow to be updated.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
updateFlowTemplate_id :: Lens.Lens' UpdateFlowTemplate Prelude.Text
updateFlowTemplate_id = Lens.lens (\UpdateFlowTemplate' {id} -> id) (\s@UpdateFlowTemplate' {} a -> s {id = a} :: UpdateFlowTemplate)

-- | The @DefinitionDocument@ that contains the updated workflow definition.
updateFlowTemplate_definition :: Lens.Lens' UpdateFlowTemplate DefinitionDocument
updateFlowTemplate_definition = Lens.lens (\UpdateFlowTemplate' {definition} -> definition) (\s@UpdateFlowTemplate' {} a -> s {definition = a} :: UpdateFlowTemplate)

instance Core.AWSRequest UpdateFlowTemplate where
  type
    AWSResponse UpdateFlowTemplate =
      UpdateFlowTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFlowTemplateResponse'
            Prelude.<$> (x Core..?> "summary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFlowTemplate where
  hashWithSalt _salt UpdateFlowTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` compatibleNamespaceVersion
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` definition

instance Prelude.NFData UpdateFlowTemplate where
  rnf UpdateFlowTemplate' {..} =
    Prelude.rnf compatibleNamespaceVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf definition

instance Core.ToHeaders UpdateFlowTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.UpdateFlowTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateFlowTemplate where
  toJSON UpdateFlowTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("compatibleNamespaceVersion" Core..=)
              Prelude.<$> compatibleNamespaceVersion,
            Prelude.Just ("id" Core..= id),
            Prelude.Just ("definition" Core..= definition)
          ]
      )

instance Core.ToPath UpdateFlowTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateFlowTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFlowTemplateResponse' smart constructor.
data UpdateFlowTemplateResponse = UpdateFlowTemplateResponse'
  { -- | An object containing summary information about the updated workflow.
    summary :: Prelude.Maybe FlowTemplateSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFlowTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'updateFlowTemplateResponse_summary' - An object containing summary information about the updated workflow.
--
-- 'httpStatus', 'updateFlowTemplateResponse_httpStatus' - The response's http status code.
newUpdateFlowTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFlowTemplateResponse
newUpdateFlowTemplateResponse pHttpStatus_ =
  UpdateFlowTemplateResponse'
    { summary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object containing summary information about the updated workflow.
updateFlowTemplateResponse_summary :: Lens.Lens' UpdateFlowTemplateResponse (Prelude.Maybe FlowTemplateSummary)
updateFlowTemplateResponse_summary = Lens.lens (\UpdateFlowTemplateResponse' {summary} -> summary) (\s@UpdateFlowTemplateResponse' {} a -> s {summary = a} :: UpdateFlowTemplateResponse)

-- | The response's http status code.
updateFlowTemplateResponse_httpStatus :: Lens.Lens' UpdateFlowTemplateResponse Prelude.Int
updateFlowTemplateResponse_httpStatus = Lens.lens (\UpdateFlowTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateFlowTemplateResponse' {} a -> s {httpStatus = a} :: UpdateFlowTemplateResponse)

instance Prelude.NFData UpdateFlowTemplateResponse where
  rnf UpdateFlowTemplateResponse' {..} =
    Prelude.rnf summary
      `Prelude.seq` Prelude.rnf httpStatus
