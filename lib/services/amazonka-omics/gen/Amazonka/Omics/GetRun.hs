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
-- Module      : Amazonka.Omics.GetRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a workflow run.
module Amazonka.Omics.GetRun
  ( -- * Creating a Request
    GetRun (..),
    newGetRun,

    -- * Request Lenses
    getRun_export,
    getRun_id,

    -- * Destructuring the Response
    GetRunResponse (..),
    newGetRunResponse,

    -- * Response Lenses
    getRunResponse_arn,
    getRunResponse_creationTime,
    getRunResponse_definition,
    getRunResponse_digest,
    getRunResponse_id,
    getRunResponse_logLevel,
    getRunResponse_name,
    getRunResponse_outputUri,
    getRunResponse_parameters,
    getRunResponse_priority,
    getRunResponse_resourceDigests,
    getRunResponse_roleArn,
    getRunResponse_runGroupId,
    getRunResponse_runId,
    getRunResponse_startTime,
    getRunResponse_startedBy,
    getRunResponse_status,
    getRunResponse_statusMessage,
    getRunResponse_stopTime,
    getRunResponse_storageCapacity,
    getRunResponse_tags,
    getRunResponse_workflowId,
    getRunResponse_workflowType,
    getRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRun' smart constructor.
data GetRun = GetRun'
  { -- | The run\'s export format.
    export' :: Prelude.Maybe [RunExport],
    -- | The run\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'export'', 'getRun_export' - The run\'s export format.
--
-- 'id', 'getRun_id' - The run\'s ID.
newGetRun ::
  -- | 'id'
  Prelude.Text ->
  GetRun
newGetRun pId_ =
  GetRun' {export' = Prelude.Nothing, id = pId_}

-- | The run\'s export format.
getRun_export :: Lens.Lens' GetRun (Prelude.Maybe [RunExport])
getRun_export = Lens.lens (\GetRun' {export'} -> export') (\s@GetRun' {} a -> s {export' = a} :: GetRun) Prelude.. Lens.mapping Lens.coerced

-- | The run\'s ID.
getRun_id :: Lens.Lens' GetRun Prelude.Text
getRun_id = Lens.lens (\GetRun' {id} -> id) (\s@GetRun' {} a -> s {id = a} :: GetRun)

instance Core.AWSRequest GetRun where
  type AWSResponse GetRun = GetRunResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRunResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "definition")
            Prelude.<*> (x Data..?> "digest")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "logLevel")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "outputUri")
            Prelude.<*> (x Data..?> "parameters")
            Prelude.<*> (x Data..?> "priority")
            Prelude.<*> ( x Data..?> "resourceDigests"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "roleArn")
            Prelude.<*> (x Data..?> "runGroupId")
            Prelude.<*> (x Data..?> "runId")
            Prelude.<*> (x Data..?> "startTime")
            Prelude.<*> (x Data..?> "startedBy")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "statusMessage")
            Prelude.<*> (x Data..?> "stopTime")
            Prelude.<*> (x Data..?> "storageCapacity")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "workflowId")
            Prelude.<*> (x Data..?> "workflowType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRun where
  hashWithSalt _salt GetRun' {..} =
    _salt `Prelude.hashWithSalt` export'
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetRun where
  rnf GetRun' {..} =
    Prelude.rnf export' `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRun where
  toPath GetRun' {..} =
    Prelude.mconcat ["/run/", Data.toBS id]

instance Data.ToQuery GetRun where
  toQuery GetRun' {..} =
    Prelude.mconcat
      [ "export"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> export')
      ]

-- | /See:/ 'newGetRunResponse' smart constructor.
data GetRunResponse = GetRunResponse'
  { -- | The run\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the run was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The run\'s definition.
    definition :: Prelude.Maybe Prelude.Text,
    -- | The run\'s digest.
    digest :: Prelude.Maybe Prelude.Text,
    -- | The run\'s ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The run\'s log level.
    logLevel :: Prelude.Maybe RunLogLevel,
    -- | The run\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The run\'s output URI.
    outputUri :: Prelude.Maybe Prelude.Text,
    -- | The run\'s parameters.
    parameters :: Prelude.Maybe RunParameters,
    -- | The run\'s priority.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The run\'s resource digests.
    resourceDigests :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The run\'s service role ARN.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The run\'s group ID.
    runGroupId :: Prelude.Maybe Prelude.Text,
    -- | The run\'s ID.
    runId :: Prelude.Maybe Prelude.Text,
    -- | When the run started.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | Who started the run.
    startedBy :: Prelude.Maybe Prelude.Text,
    -- | The run\'s status.
    status :: Prelude.Maybe RunStatus,
    -- | The run\'s status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The run\'s stop time.
    stopTime :: Prelude.Maybe Data.ISO8601,
    -- | The run\'s storage capacity.
    storageCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The run\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The run\'s workflow ID.
    workflowId :: Prelude.Maybe Prelude.Text,
    -- | The run\'s workflow type.
    workflowType :: Prelude.Maybe WorkflowType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getRunResponse_arn' - The run\'s ARN.
--
-- 'creationTime', 'getRunResponse_creationTime' - When the run was created.
--
-- 'definition', 'getRunResponse_definition' - The run\'s definition.
--
-- 'digest', 'getRunResponse_digest' - The run\'s digest.
--
-- 'id', 'getRunResponse_id' - The run\'s ID.
--
-- 'logLevel', 'getRunResponse_logLevel' - The run\'s log level.
--
-- 'name', 'getRunResponse_name' - The run\'s name.
--
-- 'outputUri', 'getRunResponse_outputUri' - The run\'s output URI.
--
-- 'parameters', 'getRunResponse_parameters' - The run\'s parameters.
--
-- 'priority', 'getRunResponse_priority' - The run\'s priority.
--
-- 'resourceDigests', 'getRunResponse_resourceDigests' - The run\'s resource digests.
--
-- 'roleArn', 'getRunResponse_roleArn' - The run\'s service role ARN.
--
-- 'runGroupId', 'getRunResponse_runGroupId' - The run\'s group ID.
--
-- 'runId', 'getRunResponse_runId' - The run\'s ID.
--
-- 'startTime', 'getRunResponse_startTime' - When the run started.
--
-- 'startedBy', 'getRunResponse_startedBy' - Who started the run.
--
-- 'status', 'getRunResponse_status' - The run\'s status.
--
-- 'statusMessage', 'getRunResponse_statusMessage' - The run\'s status message.
--
-- 'stopTime', 'getRunResponse_stopTime' - The run\'s stop time.
--
-- 'storageCapacity', 'getRunResponse_storageCapacity' - The run\'s storage capacity.
--
-- 'tags', 'getRunResponse_tags' - The run\'s tags.
--
-- 'workflowId', 'getRunResponse_workflowId' - The run\'s workflow ID.
--
-- 'workflowType', 'getRunResponse_workflowType' - The run\'s workflow type.
--
-- 'httpStatus', 'getRunResponse_httpStatus' - The response's http status code.
newGetRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRunResponse
newGetRunResponse pHttpStatus_ =
  GetRunResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      definition = Prelude.Nothing,
      digest = Prelude.Nothing,
      id = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      name = Prelude.Nothing,
      outputUri = Prelude.Nothing,
      parameters = Prelude.Nothing,
      priority = Prelude.Nothing,
      resourceDigests = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      runGroupId = Prelude.Nothing,
      runId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      stopTime = Prelude.Nothing,
      storageCapacity = Prelude.Nothing,
      tags = Prelude.Nothing,
      workflowId = Prelude.Nothing,
      workflowType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The run\'s ARN.
getRunResponse_arn :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Text)
getRunResponse_arn = Lens.lens (\GetRunResponse' {arn} -> arn) (\s@GetRunResponse' {} a -> s {arn = a} :: GetRunResponse)

-- | When the run was created.
getRunResponse_creationTime :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.UTCTime)
getRunResponse_creationTime = Lens.lens (\GetRunResponse' {creationTime} -> creationTime) (\s@GetRunResponse' {} a -> s {creationTime = a} :: GetRunResponse) Prelude.. Lens.mapping Data._Time

-- | The run\'s definition.
getRunResponse_definition :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Text)
getRunResponse_definition = Lens.lens (\GetRunResponse' {definition} -> definition) (\s@GetRunResponse' {} a -> s {definition = a} :: GetRunResponse)

-- | The run\'s digest.
getRunResponse_digest :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Text)
getRunResponse_digest = Lens.lens (\GetRunResponse' {digest} -> digest) (\s@GetRunResponse' {} a -> s {digest = a} :: GetRunResponse)

-- | The run\'s ID.
getRunResponse_id :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Text)
getRunResponse_id = Lens.lens (\GetRunResponse' {id} -> id) (\s@GetRunResponse' {} a -> s {id = a} :: GetRunResponse)

-- | The run\'s log level.
getRunResponse_logLevel :: Lens.Lens' GetRunResponse (Prelude.Maybe RunLogLevel)
getRunResponse_logLevel = Lens.lens (\GetRunResponse' {logLevel} -> logLevel) (\s@GetRunResponse' {} a -> s {logLevel = a} :: GetRunResponse)

-- | The run\'s name.
getRunResponse_name :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Text)
getRunResponse_name = Lens.lens (\GetRunResponse' {name} -> name) (\s@GetRunResponse' {} a -> s {name = a} :: GetRunResponse)

-- | The run\'s output URI.
getRunResponse_outputUri :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Text)
getRunResponse_outputUri = Lens.lens (\GetRunResponse' {outputUri} -> outputUri) (\s@GetRunResponse' {} a -> s {outputUri = a} :: GetRunResponse)

-- | The run\'s parameters.
getRunResponse_parameters :: Lens.Lens' GetRunResponse (Prelude.Maybe RunParameters)
getRunResponse_parameters = Lens.lens (\GetRunResponse' {parameters} -> parameters) (\s@GetRunResponse' {} a -> s {parameters = a} :: GetRunResponse)

-- | The run\'s priority.
getRunResponse_priority :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Natural)
getRunResponse_priority = Lens.lens (\GetRunResponse' {priority} -> priority) (\s@GetRunResponse' {} a -> s {priority = a} :: GetRunResponse)

-- | The run\'s resource digests.
getRunResponse_resourceDigests :: Lens.Lens' GetRunResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getRunResponse_resourceDigests = Lens.lens (\GetRunResponse' {resourceDigests} -> resourceDigests) (\s@GetRunResponse' {} a -> s {resourceDigests = a} :: GetRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | The run\'s service role ARN.
getRunResponse_roleArn :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Text)
getRunResponse_roleArn = Lens.lens (\GetRunResponse' {roleArn} -> roleArn) (\s@GetRunResponse' {} a -> s {roleArn = a} :: GetRunResponse)

-- | The run\'s group ID.
getRunResponse_runGroupId :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Text)
getRunResponse_runGroupId = Lens.lens (\GetRunResponse' {runGroupId} -> runGroupId) (\s@GetRunResponse' {} a -> s {runGroupId = a} :: GetRunResponse)

-- | The run\'s ID.
getRunResponse_runId :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Text)
getRunResponse_runId = Lens.lens (\GetRunResponse' {runId} -> runId) (\s@GetRunResponse' {} a -> s {runId = a} :: GetRunResponse)

-- | When the run started.
getRunResponse_startTime :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.UTCTime)
getRunResponse_startTime = Lens.lens (\GetRunResponse' {startTime} -> startTime) (\s@GetRunResponse' {} a -> s {startTime = a} :: GetRunResponse) Prelude.. Lens.mapping Data._Time

-- | Who started the run.
getRunResponse_startedBy :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Text)
getRunResponse_startedBy = Lens.lens (\GetRunResponse' {startedBy} -> startedBy) (\s@GetRunResponse' {} a -> s {startedBy = a} :: GetRunResponse)

-- | The run\'s status.
getRunResponse_status :: Lens.Lens' GetRunResponse (Prelude.Maybe RunStatus)
getRunResponse_status = Lens.lens (\GetRunResponse' {status} -> status) (\s@GetRunResponse' {} a -> s {status = a} :: GetRunResponse)

-- | The run\'s status message.
getRunResponse_statusMessage :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Text)
getRunResponse_statusMessage = Lens.lens (\GetRunResponse' {statusMessage} -> statusMessage) (\s@GetRunResponse' {} a -> s {statusMessage = a} :: GetRunResponse)

-- | The run\'s stop time.
getRunResponse_stopTime :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.UTCTime)
getRunResponse_stopTime = Lens.lens (\GetRunResponse' {stopTime} -> stopTime) (\s@GetRunResponse' {} a -> s {stopTime = a} :: GetRunResponse) Prelude.. Lens.mapping Data._Time

-- | The run\'s storage capacity.
getRunResponse_storageCapacity :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Natural)
getRunResponse_storageCapacity = Lens.lens (\GetRunResponse' {storageCapacity} -> storageCapacity) (\s@GetRunResponse' {} a -> s {storageCapacity = a} :: GetRunResponse)

-- | The run\'s tags.
getRunResponse_tags :: Lens.Lens' GetRunResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getRunResponse_tags = Lens.lens (\GetRunResponse' {tags} -> tags) (\s@GetRunResponse' {} a -> s {tags = a} :: GetRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | The run\'s workflow ID.
getRunResponse_workflowId :: Lens.Lens' GetRunResponse (Prelude.Maybe Prelude.Text)
getRunResponse_workflowId = Lens.lens (\GetRunResponse' {workflowId} -> workflowId) (\s@GetRunResponse' {} a -> s {workflowId = a} :: GetRunResponse)

-- | The run\'s workflow type.
getRunResponse_workflowType :: Lens.Lens' GetRunResponse (Prelude.Maybe WorkflowType)
getRunResponse_workflowType = Lens.lens (\GetRunResponse' {workflowType} -> workflowType) (\s@GetRunResponse' {} a -> s {workflowType = a} :: GetRunResponse)

-- | The response's http status code.
getRunResponse_httpStatus :: Lens.Lens' GetRunResponse Prelude.Int
getRunResponse_httpStatus = Lens.lens (\GetRunResponse' {httpStatus} -> httpStatus) (\s@GetRunResponse' {} a -> s {httpStatus = a} :: GetRunResponse)

instance Prelude.NFData GetRunResponse where
  rnf GetRunResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf digest
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf outputUri
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf resourceDigests
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf runGroupId
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf startedBy
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf stopTime
      `Prelude.seq` Prelude.rnf storageCapacity
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf workflowType
      `Prelude.seq` Prelude.rnf httpStatus
