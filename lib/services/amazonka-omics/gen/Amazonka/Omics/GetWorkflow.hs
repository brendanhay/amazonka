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
-- Module      : Amazonka.Omics.GetWorkflow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a workflow.
module Amazonka.Omics.GetWorkflow
  ( -- * Creating a Request
    GetWorkflow (..),
    newGetWorkflow,

    -- * Request Lenses
    getWorkflow_export,
    getWorkflow_type,
    getWorkflow_id,

    -- * Destructuring the Response
    GetWorkflowResponse (..),
    newGetWorkflowResponse,

    -- * Response Lenses
    getWorkflowResponse_arn,
    getWorkflowResponse_creationTime,
    getWorkflowResponse_definition,
    getWorkflowResponse_description,
    getWorkflowResponse_digest,
    getWorkflowResponse_engine,
    getWorkflowResponse_id,
    getWorkflowResponse_main,
    getWorkflowResponse_name,
    getWorkflowResponse_parameterTemplate,
    getWorkflowResponse_status,
    getWorkflowResponse_statusMessage,
    getWorkflowResponse_storageCapacity,
    getWorkflowResponse_tags,
    getWorkflowResponse_type,
    getWorkflowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkflow' smart constructor.
data GetWorkflow = GetWorkflow'
  { -- | The export format for the workflow.
    export' :: Prelude.Maybe [WorkflowExport],
    -- | The workflow\'s type.
    type' :: Prelude.Maybe WorkflowType,
    -- | The workflow\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'export'', 'getWorkflow_export' - The export format for the workflow.
--
-- 'type'', 'getWorkflow_type' - The workflow\'s type.
--
-- 'id', 'getWorkflow_id' - The workflow\'s ID.
newGetWorkflow ::
  -- | 'id'
  Prelude.Text ->
  GetWorkflow
newGetWorkflow pId_ =
  GetWorkflow'
    { export' = Prelude.Nothing,
      type' = Prelude.Nothing,
      id = pId_
    }

-- | The export format for the workflow.
getWorkflow_export :: Lens.Lens' GetWorkflow (Prelude.Maybe [WorkflowExport])
getWorkflow_export = Lens.lens (\GetWorkflow' {export'} -> export') (\s@GetWorkflow' {} a -> s {export' = a} :: GetWorkflow) Prelude.. Lens.mapping Lens.coerced

-- | The workflow\'s type.
getWorkflow_type :: Lens.Lens' GetWorkflow (Prelude.Maybe WorkflowType)
getWorkflow_type = Lens.lens (\GetWorkflow' {type'} -> type') (\s@GetWorkflow' {} a -> s {type' = a} :: GetWorkflow)

-- | The workflow\'s ID.
getWorkflow_id :: Lens.Lens' GetWorkflow Prelude.Text
getWorkflow_id = Lens.lens (\GetWorkflow' {id} -> id) (\s@GetWorkflow' {} a -> s {id = a} :: GetWorkflow)

instance Core.AWSRequest GetWorkflow where
  type AWSResponse GetWorkflow = GetWorkflowResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "definition")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "digest")
            Prelude.<*> (x Data..?> "engine")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "main")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> ( x
                            Data..?> "parameterTemplate"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "statusMessage")
            Prelude.<*> (x Data..?> "storageCapacity")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflow where
  hashWithSalt _salt GetWorkflow' {..} =
    _salt
      `Prelude.hashWithSalt` export'
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetWorkflow where
  rnf GetWorkflow' {..} =
    Prelude.rnf export'
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetWorkflow where
  toPath GetWorkflow' {..} =
    Prelude.mconcat ["/workflow/", Data.toBS id]

instance Data.ToQuery GetWorkflow where
  toQuery GetWorkflow' {..} =
    Prelude.mconcat
      [ "export"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> export'),
        "type" Data.=: type'
      ]

-- | /See:/ 'newGetWorkflowResponse' smart constructor.
data GetWorkflowResponse = GetWorkflowResponse'
  { -- | The workflow\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the workflow was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The workflow\'s definition.
    definition :: Prelude.Maybe Prelude.Text,
    -- | The workflow\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The workflow\'s digest.
    digest :: Prelude.Maybe Prelude.Text,
    -- | The workflow\'s engine.
    engine :: Prelude.Maybe WorkflowEngine,
    -- | The workflow\'s ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The path of the main definition file for the workflow.
    main :: Prelude.Maybe Prelude.Text,
    -- | The workflow\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The workflow\'s parameter template.
    parameterTemplate :: Prelude.Maybe (Prelude.HashMap Prelude.Text WorkflowParameter),
    -- | The workflow\'s status.
    status :: Prelude.Maybe WorkflowStatus,
    -- | The workflow\'s status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The workflow\'s storage capacity.
    storageCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The workflow\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The workflow\'s type.
    type' :: Prelude.Maybe WorkflowType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getWorkflowResponse_arn' - The workflow\'s ARN.
--
-- 'creationTime', 'getWorkflowResponse_creationTime' - When the workflow was created.
--
-- 'definition', 'getWorkflowResponse_definition' - The workflow\'s definition.
--
-- 'description', 'getWorkflowResponse_description' - The workflow\'s description.
--
-- 'digest', 'getWorkflowResponse_digest' - The workflow\'s digest.
--
-- 'engine', 'getWorkflowResponse_engine' - The workflow\'s engine.
--
-- 'id', 'getWorkflowResponse_id' - The workflow\'s ID.
--
-- 'main', 'getWorkflowResponse_main' - The path of the main definition file for the workflow.
--
-- 'name', 'getWorkflowResponse_name' - The workflow\'s name.
--
-- 'parameterTemplate', 'getWorkflowResponse_parameterTemplate' - The workflow\'s parameter template.
--
-- 'status', 'getWorkflowResponse_status' - The workflow\'s status.
--
-- 'statusMessage', 'getWorkflowResponse_statusMessage' - The workflow\'s status message.
--
-- 'storageCapacity', 'getWorkflowResponse_storageCapacity' - The workflow\'s storage capacity.
--
-- 'tags', 'getWorkflowResponse_tags' - The workflow\'s tags.
--
-- 'type'', 'getWorkflowResponse_type' - The workflow\'s type.
--
-- 'httpStatus', 'getWorkflowResponse_httpStatus' - The response's http status code.
newGetWorkflowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowResponse
newGetWorkflowResponse pHttpStatus_ =
  GetWorkflowResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      definition = Prelude.Nothing,
      description = Prelude.Nothing,
      digest = Prelude.Nothing,
      engine = Prelude.Nothing,
      id = Prelude.Nothing,
      main = Prelude.Nothing,
      name = Prelude.Nothing,
      parameterTemplate = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      storageCapacity = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The workflow\'s ARN.
getWorkflowResponse_arn :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_arn = Lens.lens (\GetWorkflowResponse' {arn} -> arn) (\s@GetWorkflowResponse' {} a -> s {arn = a} :: GetWorkflowResponse)

-- | When the workflow was created.
getWorkflowResponse_creationTime :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowResponse_creationTime = Lens.lens (\GetWorkflowResponse' {creationTime} -> creationTime) (\s@GetWorkflowResponse' {} a -> s {creationTime = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Data._Time

-- | The workflow\'s definition.
getWorkflowResponse_definition :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_definition = Lens.lens (\GetWorkflowResponse' {definition} -> definition) (\s@GetWorkflowResponse' {} a -> s {definition = a} :: GetWorkflowResponse)

-- | The workflow\'s description.
getWorkflowResponse_description :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_description = Lens.lens (\GetWorkflowResponse' {description} -> description) (\s@GetWorkflowResponse' {} a -> s {description = a} :: GetWorkflowResponse)

-- | The workflow\'s digest.
getWorkflowResponse_digest :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_digest = Lens.lens (\GetWorkflowResponse' {digest} -> digest) (\s@GetWorkflowResponse' {} a -> s {digest = a} :: GetWorkflowResponse)

-- | The workflow\'s engine.
getWorkflowResponse_engine :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe WorkflowEngine)
getWorkflowResponse_engine = Lens.lens (\GetWorkflowResponse' {engine} -> engine) (\s@GetWorkflowResponse' {} a -> s {engine = a} :: GetWorkflowResponse)

-- | The workflow\'s ID.
getWorkflowResponse_id :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_id = Lens.lens (\GetWorkflowResponse' {id} -> id) (\s@GetWorkflowResponse' {} a -> s {id = a} :: GetWorkflowResponse)

-- | The path of the main definition file for the workflow.
getWorkflowResponse_main :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_main = Lens.lens (\GetWorkflowResponse' {main} -> main) (\s@GetWorkflowResponse' {} a -> s {main = a} :: GetWorkflowResponse)

-- | The workflow\'s name.
getWorkflowResponse_name :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_name = Lens.lens (\GetWorkflowResponse' {name} -> name) (\s@GetWorkflowResponse' {} a -> s {name = a} :: GetWorkflowResponse)

-- | The workflow\'s parameter template.
getWorkflowResponse_parameterTemplate :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text WorkflowParameter))
getWorkflowResponse_parameterTemplate = Lens.lens (\GetWorkflowResponse' {parameterTemplate} -> parameterTemplate) (\s@GetWorkflowResponse' {} a -> s {parameterTemplate = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The workflow\'s status.
getWorkflowResponse_status :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe WorkflowStatus)
getWorkflowResponse_status = Lens.lens (\GetWorkflowResponse' {status} -> status) (\s@GetWorkflowResponse' {} a -> s {status = a} :: GetWorkflowResponse)

-- | The workflow\'s status message.
getWorkflowResponse_statusMessage :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Text)
getWorkflowResponse_statusMessage = Lens.lens (\GetWorkflowResponse' {statusMessage} -> statusMessage) (\s@GetWorkflowResponse' {} a -> s {statusMessage = a} :: GetWorkflowResponse)

-- | The workflow\'s storage capacity.
getWorkflowResponse_storageCapacity :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe Prelude.Natural)
getWorkflowResponse_storageCapacity = Lens.lens (\GetWorkflowResponse' {storageCapacity} -> storageCapacity) (\s@GetWorkflowResponse' {} a -> s {storageCapacity = a} :: GetWorkflowResponse)

-- | The workflow\'s tags.
getWorkflowResponse_tags :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getWorkflowResponse_tags = Lens.lens (\GetWorkflowResponse' {tags} -> tags) (\s@GetWorkflowResponse' {} a -> s {tags = a} :: GetWorkflowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The workflow\'s type.
getWorkflowResponse_type :: Lens.Lens' GetWorkflowResponse (Prelude.Maybe WorkflowType)
getWorkflowResponse_type = Lens.lens (\GetWorkflowResponse' {type'} -> type') (\s@GetWorkflowResponse' {} a -> s {type' = a} :: GetWorkflowResponse)

-- | The response's http status code.
getWorkflowResponse_httpStatus :: Lens.Lens' GetWorkflowResponse Prelude.Int
getWorkflowResponse_httpStatus = Lens.lens (\GetWorkflowResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowResponse' {} a -> s {httpStatus = a} :: GetWorkflowResponse)

instance Prelude.NFData GetWorkflowResponse where
  rnf GetWorkflowResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf digest
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf main
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf parameterTemplate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf storageCapacity
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf httpStatus
