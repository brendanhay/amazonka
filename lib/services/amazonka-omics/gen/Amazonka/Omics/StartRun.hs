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
-- Module      : Amazonka.Omics.StartRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a run.
module Amazonka.Omics.StartRun
  ( -- * Creating a Request
    StartRun (..),
    newStartRun,

    -- * Request Lenses
    startRun_logLevel,
    startRun_name,
    startRun_outputUri,
    startRun_parameters,
    startRun_priority,
    startRun_runGroupId,
    startRun_runId,
    startRun_storageCapacity,
    startRun_tags,
    startRun_workflowId,
    startRun_workflowType,
    startRun_requestId,
    startRun_roleArn,

    -- * Destructuring the Response
    StartRunResponse (..),
    newStartRunResponse,

    -- * Response Lenses
    startRunResponse_arn,
    startRunResponse_id,
    startRunResponse_status,
    startRunResponse_tags,
    startRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartRun' smart constructor.
data StartRun = StartRun'
  { -- | A log level for the run.
    logLevel :: Prelude.Maybe RunLogLevel,
    -- | A name for the run.
    name :: Prelude.Maybe Prelude.Text,
    -- | An output URI for the run.
    outputUri :: Prelude.Maybe Prelude.Text,
    -- | Parameters for the run.
    parameters :: Prelude.Maybe RunParameters,
    -- | A priority for the run.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The run\'s group ID.
    runGroupId :: Prelude.Maybe Prelude.Text,
    -- | The run\'s ID.
    runId :: Prelude.Maybe Prelude.Text,
    -- | A storage capacity for the run.
    storageCapacity :: Prelude.Maybe Prelude.Natural,
    -- | Tags for the run.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The run\'s workflow ID.
    workflowId :: Prelude.Maybe Prelude.Text,
    -- | The run\'s workflows type.
    workflowType :: Prelude.Maybe WorkflowType,
    -- | A request ID for the run.
    requestId :: Prelude.Text,
    -- | A service role for the run.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logLevel', 'startRun_logLevel' - A log level for the run.
--
-- 'name', 'startRun_name' - A name for the run.
--
-- 'outputUri', 'startRun_outputUri' - An output URI for the run.
--
-- 'parameters', 'startRun_parameters' - Parameters for the run.
--
-- 'priority', 'startRun_priority' - A priority for the run.
--
-- 'runGroupId', 'startRun_runGroupId' - The run\'s group ID.
--
-- 'runId', 'startRun_runId' - The run\'s ID.
--
-- 'storageCapacity', 'startRun_storageCapacity' - A storage capacity for the run.
--
-- 'tags', 'startRun_tags' - Tags for the run.
--
-- 'workflowId', 'startRun_workflowId' - The run\'s workflow ID.
--
-- 'workflowType', 'startRun_workflowType' - The run\'s workflows type.
--
-- 'requestId', 'startRun_requestId' - A request ID for the run.
--
-- 'roleArn', 'startRun_roleArn' - A service role for the run.
newStartRun ::
  -- | 'requestId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  StartRun
newStartRun pRequestId_ pRoleArn_ =
  StartRun'
    { logLevel = Prelude.Nothing,
      name = Prelude.Nothing,
      outputUri = Prelude.Nothing,
      parameters = Prelude.Nothing,
      priority = Prelude.Nothing,
      runGroupId = Prelude.Nothing,
      runId = Prelude.Nothing,
      storageCapacity = Prelude.Nothing,
      tags = Prelude.Nothing,
      workflowId = Prelude.Nothing,
      workflowType = Prelude.Nothing,
      requestId = pRequestId_,
      roleArn = pRoleArn_
    }

-- | A log level for the run.
startRun_logLevel :: Lens.Lens' StartRun (Prelude.Maybe RunLogLevel)
startRun_logLevel = Lens.lens (\StartRun' {logLevel} -> logLevel) (\s@StartRun' {} a -> s {logLevel = a} :: StartRun)

-- | A name for the run.
startRun_name :: Lens.Lens' StartRun (Prelude.Maybe Prelude.Text)
startRun_name = Lens.lens (\StartRun' {name} -> name) (\s@StartRun' {} a -> s {name = a} :: StartRun)

-- | An output URI for the run.
startRun_outputUri :: Lens.Lens' StartRun (Prelude.Maybe Prelude.Text)
startRun_outputUri = Lens.lens (\StartRun' {outputUri} -> outputUri) (\s@StartRun' {} a -> s {outputUri = a} :: StartRun)

-- | Parameters for the run.
startRun_parameters :: Lens.Lens' StartRun (Prelude.Maybe RunParameters)
startRun_parameters = Lens.lens (\StartRun' {parameters} -> parameters) (\s@StartRun' {} a -> s {parameters = a} :: StartRun)

-- | A priority for the run.
startRun_priority :: Lens.Lens' StartRun (Prelude.Maybe Prelude.Natural)
startRun_priority = Lens.lens (\StartRun' {priority} -> priority) (\s@StartRun' {} a -> s {priority = a} :: StartRun)

-- | The run\'s group ID.
startRun_runGroupId :: Lens.Lens' StartRun (Prelude.Maybe Prelude.Text)
startRun_runGroupId = Lens.lens (\StartRun' {runGroupId} -> runGroupId) (\s@StartRun' {} a -> s {runGroupId = a} :: StartRun)

-- | The run\'s ID.
startRun_runId :: Lens.Lens' StartRun (Prelude.Maybe Prelude.Text)
startRun_runId = Lens.lens (\StartRun' {runId} -> runId) (\s@StartRun' {} a -> s {runId = a} :: StartRun)

-- | A storage capacity for the run.
startRun_storageCapacity :: Lens.Lens' StartRun (Prelude.Maybe Prelude.Natural)
startRun_storageCapacity = Lens.lens (\StartRun' {storageCapacity} -> storageCapacity) (\s@StartRun' {} a -> s {storageCapacity = a} :: StartRun)

-- | Tags for the run.
startRun_tags :: Lens.Lens' StartRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startRun_tags = Lens.lens (\StartRun' {tags} -> tags) (\s@StartRun' {} a -> s {tags = a} :: StartRun) Prelude.. Lens.mapping Lens.coerced

-- | The run\'s workflow ID.
startRun_workflowId :: Lens.Lens' StartRun (Prelude.Maybe Prelude.Text)
startRun_workflowId = Lens.lens (\StartRun' {workflowId} -> workflowId) (\s@StartRun' {} a -> s {workflowId = a} :: StartRun)

-- | The run\'s workflows type.
startRun_workflowType :: Lens.Lens' StartRun (Prelude.Maybe WorkflowType)
startRun_workflowType = Lens.lens (\StartRun' {workflowType} -> workflowType) (\s@StartRun' {} a -> s {workflowType = a} :: StartRun)

-- | A request ID for the run.
startRun_requestId :: Lens.Lens' StartRun Prelude.Text
startRun_requestId = Lens.lens (\StartRun' {requestId} -> requestId) (\s@StartRun' {} a -> s {requestId = a} :: StartRun)

-- | A service role for the run.
startRun_roleArn :: Lens.Lens' StartRun Prelude.Text
startRun_roleArn = Lens.lens (\StartRun' {roleArn} -> roleArn) (\s@StartRun' {} a -> s {roleArn = a} :: StartRun)

instance Core.AWSRequest StartRun where
  type AWSResponse StartRun = StartRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartRunResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartRun where
  hashWithSalt _salt StartRun' {..} =
    _salt
      `Prelude.hashWithSalt` logLevel
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outputUri
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` runGroupId
      `Prelude.hashWithSalt` runId
      `Prelude.hashWithSalt` storageCapacity
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` workflowType
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData StartRun where
  rnf StartRun' {..} =
    Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf outputUri
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf runGroupId
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf storageCapacity
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf workflowType
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders StartRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartRun where
  toJSON StartRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("logLevel" Data..=) Prelude.<$> logLevel,
            ("name" Data..=) Prelude.<$> name,
            ("outputUri" Data..=) Prelude.<$> outputUri,
            ("parameters" Data..=) Prelude.<$> parameters,
            ("priority" Data..=) Prelude.<$> priority,
            ("runGroupId" Data..=) Prelude.<$> runGroupId,
            ("runId" Data..=) Prelude.<$> runId,
            ("storageCapacity" Data..=)
              Prelude.<$> storageCapacity,
            ("tags" Data..=) Prelude.<$> tags,
            ("workflowId" Data..=) Prelude.<$> workflowId,
            ("workflowType" Data..=) Prelude.<$> workflowType,
            Prelude.Just ("requestId" Data..= requestId),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath StartRun where
  toPath = Prelude.const "/run"

instance Data.ToQuery StartRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartRunResponse' smart constructor.
data StartRunResponse = StartRunResponse'
  { -- | The run\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The run\'s ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The run\'s status.
    status :: Prelude.Maybe RunStatus,
    -- | The run\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'startRunResponse_arn' - The run\'s ARN.
--
-- 'id', 'startRunResponse_id' - The run\'s ID.
--
-- 'status', 'startRunResponse_status' - The run\'s status.
--
-- 'tags', 'startRunResponse_tags' - The run\'s tags.
--
-- 'httpStatus', 'startRunResponse_httpStatus' - The response's http status code.
newStartRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartRunResponse
newStartRunResponse pHttpStatus_ =
  StartRunResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The run\'s ARN.
startRunResponse_arn :: Lens.Lens' StartRunResponse (Prelude.Maybe Prelude.Text)
startRunResponse_arn = Lens.lens (\StartRunResponse' {arn} -> arn) (\s@StartRunResponse' {} a -> s {arn = a} :: StartRunResponse)

-- | The run\'s ID.
startRunResponse_id :: Lens.Lens' StartRunResponse (Prelude.Maybe Prelude.Text)
startRunResponse_id = Lens.lens (\StartRunResponse' {id} -> id) (\s@StartRunResponse' {} a -> s {id = a} :: StartRunResponse)

-- | The run\'s status.
startRunResponse_status :: Lens.Lens' StartRunResponse (Prelude.Maybe RunStatus)
startRunResponse_status = Lens.lens (\StartRunResponse' {status} -> status) (\s@StartRunResponse' {} a -> s {status = a} :: StartRunResponse)

-- | The run\'s tags.
startRunResponse_tags :: Lens.Lens' StartRunResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startRunResponse_tags = Lens.lens (\StartRunResponse' {tags} -> tags) (\s@StartRunResponse' {} a -> s {tags = a} :: StartRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
startRunResponse_httpStatus :: Lens.Lens' StartRunResponse Prelude.Int
startRunResponse_httpStatus = Lens.lens (\StartRunResponse' {httpStatus} -> httpStatus) (\s@StartRunResponse' {} a -> s {httpStatus = a} :: StartRunResponse)

instance Prelude.NFData StartRunResponse where
  rnf StartRunResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
