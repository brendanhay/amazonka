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
-- Module      : Amazonka.IoTThingsGraph.CreateFlowTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a workflow template. Workflows can be created only in the
-- user\'s namespace. (The public namespace contains only entities.) The
-- workflow can contain only entities in the specified namespace. The
-- workflow is validated against the entities in the latest version of the
-- user\'s namespace unless another namespace version is specified in the
-- request.
module Amazonka.IoTThingsGraph.CreateFlowTemplate
  ( -- * Creating a Request
    CreateFlowTemplate (..),
    newCreateFlowTemplate,

    -- * Request Lenses
    createFlowTemplate_compatibleNamespaceVersion,
    createFlowTemplate_definition,

    -- * Destructuring the Response
    CreateFlowTemplateResponse (..),
    newCreateFlowTemplateResponse,

    -- * Response Lenses
    createFlowTemplateResponse_summary,
    createFlowTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFlowTemplate' smart constructor.
data CreateFlowTemplate = CreateFlowTemplate'
  { -- | The namespace version in which the workflow is to be created.
    --
    -- If no value is specified, the latest version is used by default.
    compatibleNamespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | The workflow @DefinitionDocument@.
    definition :: DefinitionDocument
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFlowTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleNamespaceVersion', 'createFlowTemplate_compatibleNamespaceVersion' - The namespace version in which the workflow is to be created.
--
-- If no value is specified, the latest version is used by default.
--
-- 'definition', 'createFlowTemplate_definition' - The workflow @DefinitionDocument@.
newCreateFlowTemplate ::
  -- | 'definition'
  DefinitionDocument ->
  CreateFlowTemplate
newCreateFlowTemplate pDefinition_ =
  CreateFlowTemplate'
    { compatibleNamespaceVersion =
        Prelude.Nothing,
      definition = pDefinition_
    }

-- | The namespace version in which the workflow is to be created.
--
-- If no value is specified, the latest version is used by default.
createFlowTemplate_compatibleNamespaceVersion :: Lens.Lens' CreateFlowTemplate (Prelude.Maybe Prelude.Integer)
createFlowTemplate_compatibleNamespaceVersion = Lens.lens (\CreateFlowTemplate' {compatibleNamespaceVersion} -> compatibleNamespaceVersion) (\s@CreateFlowTemplate' {} a -> s {compatibleNamespaceVersion = a} :: CreateFlowTemplate)

-- | The workflow @DefinitionDocument@.
createFlowTemplate_definition :: Lens.Lens' CreateFlowTemplate DefinitionDocument
createFlowTemplate_definition = Lens.lens (\CreateFlowTemplate' {definition} -> definition) (\s@CreateFlowTemplate' {} a -> s {definition = a} :: CreateFlowTemplate)

instance Core.AWSRequest CreateFlowTemplate where
  type
    AWSResponse CreateFlowTemplate =
      CreateFlowTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFlowTemplateResponse'
            Prelude.<$> (x Core..?> "summary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFlowTemplate

instance Prelude.NFData CreateFlowTemplate

instance Core.ToHeaders CreateFlowTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.CreateFlowTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFlowTemplate where
  toJSON CreateFlowTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("compatibleNamespaceVersion" Core..=)
              Prelude.<$> compatibleNamespaceVersion,
            Prelude.Just ("definition" Core..= definition)
          ]
      )

instance Core.ToPath CreateFlowTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateFlowTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFlowTemplateResponse' smart constructor.
data CreateFlowTemplateResponse = CreateFlowTemplateResponse'
  { -- | The summary object that describes the created workflow.
    summary :: Prelude.Maybe FlowTemplateSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFlowTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'createFlowTemplateResponse_summary' - The summary object that describes the created workflow.
--
-- 'httpStatus', 'createFlowTemplateResponse_httpStatus' - The response's http status code.
newCreateFlowTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFlowTemplateResponse
newCreateFlowTemplateResponse pHttpStatus_ =
  CreateFlowTemplateResponse'
    { summary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The summary object that describes the created workflow.
createFlowTemplateResponse_summary :: Lens.Lens' CreateFlowTemplateResponse (Prelude.Maybe FlowTemplateSummary)
createFlowTemplateResponse_summary = Lens.lens (\CreateFlowTemplateResponse' {summary} -> summary) (\s@CreateFlowTemplateResponse' {} a -> s {summary = a} :: CreateFlowTemplateResponse)

-- | The response's http status code.
createFlowTemplateResponse_httpStatus :: Lens.Lens' CreateFlowTemplateResponse Prelude.Int
createFlowTemplateResponse_httpStatus = Lens.lens (\CreateFlowTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateFlowTemplateResponse' {} a -> s {httpStatus = a} :: CreateFlowTemplateResponse)

instance Prelude.NFData CreateFlowTemplateResponse
