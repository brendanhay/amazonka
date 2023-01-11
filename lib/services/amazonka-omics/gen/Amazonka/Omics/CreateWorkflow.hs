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
-- Module      : Amazonka.Omics.CreateWorkflow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a workflow.
module Amazonka.Omics.CreateWorkflow
  ( -- * Creating a Request
    CreateWorkflow (..),
    newCreateWorkflow,

    -- * Request Lenses
    createWorkflow_definitionUri,
    createWorkflow_definitionZip,
    createWorkflow_description,
    createWorkflow_engine,
    createWorkflow_main,
    createWorkflow_name,
    createWorkflow_parameterTemplate,
    createWorkflow_storageCapacity,
    createWorkflow_tags,
    createWorkflow_requestId,

    -- * Destructuring the Response
    CreateWorkflowResponse (..),
    newCreateWorkflowResponse,

    -- * Response Lenses
    createWorkflowResponse_arn,
    createWorkflowResponse_id,
    createWorkflowResponse_status,
    createWorkflowResponse_tags,
    createWorkflowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWorkflow' smart constructor.
data CreateWorkflow = CreateWorkflow'
  { -- | The URI of a definition for the workflow.
    definitionUri :: Prelude.Maybe Prelude.Text,
    -- | A ZIP archive for the workflow.
    definitionZip :: Prelude.Maybe Data.Base64,
    -- | A description for the workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | An engine for the workflow.
    engine :: Prelude.Maybe WorkflowEngine,
    -- | The path of the main definition file for the workflow.
    main :: Prelude.Maybe Prelude.Text,
    -- | A name for the workflow.
    name :: Prelude.Maybe Prelude.Text,
    -- | A parameter template for the workflow.
    parameterTemplate :: Prelude.Maybe (Prelude.HashMap Prelude.Text WorkflowParameter),
    -- | A storage capacity for the workflow.
    storageCapacity :: Prelude.Maybe Prelude.Natural,
    -- | Tags for the workflow.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A request ID for the workflow.
    requestId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definitionUri', 'createWorkflow_definitionUri' - The URI of a definition for the workflow.
--
-- 'definitionZip', 'createWorkflow_definitionZip' - A ZIP archive for the workflow.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'description', 'createWorkflow_description' - A description for the workflow.
--
-- 'engine', 'createWorkflow_engine' - An engine for the workflow.
--
-- 'main', 'createWorkflow_main' - The path of the main definition file for the workflow.
--
-- 'name', 'createWorkflow_name' - A name for the workflow.
--
-- 'parameterTemplate', 'createWorkflow_parameterTemplate' - A parameter template for the workflow.
--
-- 'storageCapacity', 'createWorkflow_storageCapacity' - A storage capacity for the workflow.
--
-- 'tags', 'createWorkflow_tags' - Tags for the workflow.
--
-- 'requestId', 'createWorkflow_requestId' - A request ID for the workflow.
newCreateWorkflow ::
  -- | 'requestId'
  Prelude.Text ->
  CreateWorkflow
newCreateWorkflow pRequestId_ =
  CreateWorkflow'
    { definitionUri = Prelude.Nothing,
      definitionZip = Prelude.Nothing,
      description = Prelude.Nothing,
      engine = Prelude.Nothing,
      main = Prelude.Nothing,
      name = Prelude.Nothing,
      parameterTemplate = Prelude.Nothing,
      storageCapacity = Prelude.Nothing,
      tags = Prelude.Nothing,
      requestId = pRequestId_
    }

-- | The URI of a definition for the workflow.
createWorkflow_definitionUri :: Lens.Lens' CreateWorkflow (Prelude.Maybe Prelude.Text)
createWorkflow_definitionUri = Lens.lens (\CreateWorkflow' {definitionUri} -> definitionUri) (\s@CreateWorkflow' {} a -> s {definitionUri = a} :: CreateWorkflow)

-- | A ZIP archive for the workflow.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
createWorkflow_definitionZip :: Lens.Lens' CreateWorkflow (Prelude.Maybe Prelude.ByteString)
createWorkflow_definitionZip = Lens.lens (\CreateWorkflow' {definitionZip} -> definitionZip) (\s@CreateWorkflow' {} a -> s {definitionZip = a} :: CreateWorkflow) Prelude.. Lens.mapping Data._Base64

-- | A description for the workflow.
createWorkflow_description :: Lens.Lens' CreateWorkflow (Prelude.Maybe Prelude.Text)
createWorkflow_description = Lens.lens (\CreateWorkflow' {description} -> description) (\s@CreateWorkflow' {} a -> s {description = a} :: CreateWorkflow)

-- | An engine for the workflow.
createWorkflow_engine :: Lens.Lens' CreateWorkflow (Prelude.Maybe WorkflowEngine)
createWorkflow_engine = Lens.lens (\CreateWorkflow' {engine} -> engine) (\s@CreateWorkflow' {} a -> s {engine = a} :: CreateWorkflow)

-- | The path of the main definition file for the workflow.
createWorkflow_main :: Lens.Lens' CreateWorkflow (Prelude.Maybe Prelude.Text)
createWorkflow_main = Lens.lens (\CreateWorkflow' {main} -> main) (\s@CreateWorkflow' {} a -> s {main = a} :: CreateWorkflow)

-- | A name for the workflow.
createWorkflow_name :: Lens.Lens' CreateWorkflow (Prelude.Maybe Prelude.Text)
createWorkflow_name = Lens.lens (\CreateWorkflow' {name} -> name) (\s@CreateWorkflow' {} a -> s {name = a} :: CreateWorkflow)

-- | A parameter template for the workflow.
createWorkflow_parameterTemplate :: Lens.Lens' CreateWorkflow (Prelude.Maybe (Prelude.HashMap Prelude.Text WorkflowParameter))
createWorkflow_parameterTemplate = Lens.lens (\CreateWorkflow' {parameterTemplate} -> parameterTemplate) (\s@CreateWorkflow' {} a -> s {parameterTemplate = a} :: CreateWorkflow) Prelude.. Lens.mapping Lens.coerced

-- | A storage capacity for the workflow.
createWorkflow_storageCapacity :: Lens.Lens' CreateWorkflow (Prelude.Maybe Prelude.Natural)
createWorkflow_storageCapacity = Lens.lens (\CreateWorkflow' {storageCapacity} -> storageCapacity) (\s@CreateWorkflow' {} a -> s {storageCapacity = a} :: CreateWorkflow)

-- | Tags for the workflow.
createWorkflow_tags :: Lens.Lens' CreateWorkflow (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorkflow_tags = Lens.lens (\CreateWorkflow' {tags} -> tags) (\s@CreateWorkflow' {} a -> s {tags = a} :: CreateWorkflow) Prelude.. Lens.mapping Lens.coerced

-- | A request ID for the workflow.
createWorkflow_requestId :: Lens.Lens' CreateWorkflow Prelude.Text
createWorkflow_requestId = Lens.lens (\CreateWorkflow' {requestId} -> requestId) (\s@CreateWorkflow' {} a -> s {requestId = a} :: CreateWorkflow)

instance Core.AWSRequest CreateWorkflow where
  type
    AWSResponse CreateWorkflow =
      CreateWorkflowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkflowResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkflow where
  hashWithSalt _salt CreateWorkflow' {..} =
    _salt `Prelude.hashWithSalt` definitionUri
      `Prelude.hashWithSalt` definitionZip
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` main
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` parameterTemplate
      `Prelude.hashWithSalt` storageCapacity
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` requestId

instance Prelude.NFData CreateWorkflow where
  rnf CreateWorkflow' {..} =
    Prelude.rnf definitionUri
      `Prelude.seq` Prelude.rnf definitionZip
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf main
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf parameterTemplate
      `Prelude.seq` Prelude.rnf storageCapacity
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf requestId

instance Data.ToHeaders CreateWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorkflow where
  toJSON CreateWorkflow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("definitionUri" Data..=) Prelude.<$> definitionUri,
            ("definitionZip" Data..=) Prelude.<$> definitionZip,
            ("description" Data..=) Prelude.<$> description,
            ("engine" Data..=) Prelude.<$> engine,
            ("main" Data..=) Prelude.<$> main,
            ("name" Data..=) Prelude.<$> name,
            ("parameterTemplate" Data..=)
              Prelude.<$> parameterTemplate,
            ("storageCapacity" Data..=)
              Prelude.<$> storageCapacity,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("requestId" Data..= requestId)
          ]
      )

instance Data.ToPath CreateWorkflow where
  toPath = Prelude.const "/workflow"

instance Data.ToQuery CreateWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkflowResponse' smart constructor.
data CreateWorkflowResponse = CreateWorkflowResponse'
  { -- | The workflow\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The workflow\'s ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The workflow\'s status.
    status :: Prelude.Maybe WorkflowStatus,
    -- | The workflow\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createWorkflowResponse_arn' - The workflow\'s ARN.
--
-- 'id', 'createWorkflowResponse_id' - The workflow\'s ID.
--
-- 'status', 'createWorkflowResponse_status' - The workflow\'s status.
--
-- 'tags', 'createWorkflowResponse_tags' - The workflow\'s tags.
--
-- 'httpStatus', 'createWorkflowResponse_httpStatus' - The response's http status code.
newCreateWorkflowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorkflowResponse
newCreateWorkflowResponse pHttpStatus_ =
  CreateWorkflowResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The workflow\'s ARN.
createWorkflowResponse_arn :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe Prelude.Text)
createWorkflowResponse_arn = Lens.lens (\CreateWorkflowResponse' {arn} -> arn) (\s@CreateWorkflowResponse' {} a -> s {arn = a} :: CreateWorkflowResponse)

-- | The workflow\'s ID.
createWorkflowResponse_id :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe Prelude.Text)
createWorkflowResponse_id = Lens.lens (\CreateWorkflowResponse' {id} -> id) (\s@CreateWorkflowResponse' {} a -> s {id = a} :: CreateWorkflowResponse)

-- | The workflow\'s status.
createWorkflowResponse_status :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe WorkflowStatus)
createWorkflowResponse_status = Lens.lens (\CreateWorkflowResponse' {status} -> status) (\s@CreateWorkflowResponse' {} a -> s {status = a} :: CreateWorkflowResponse)

-- | The workflow\'s tags.
createWorkflowResponse_tags :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorkflowResponse_tags = Lens.lens (\CreateWorkflowResponse' {tags} -> tags) (\s@CreateWorkflowResponse' {} a -> s {tags = a} :: CreateWorkflowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createWorkflowResponse_httpStatus :: Lens.Lens' CreateWorkflowResponse Prelude.Int
createWorkflowResponse_httpStatus = Lens.lens (\CreateWorkflowResponse' {httpStatus} -> httpStatus) (\s@CreateWorkflowResponse' {} a -> s {httpStatus = a} :: CreateWorkflowResponse)

instance Prelude.NFData CreateWorkflowResponse where
  rnf CreateWorkflowResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
