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
-- Module      : Amazonka.MigrationHubOrchestrator.StopWorkflow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop an ongoing migration workflow.
module Amazonka.MigrationHubOrchestrator.StopWorkflow
  ( -- * Creating a Request
    StopWorkflow (..),
    newStopWorkflow,

    -- * Request Lenses
    stopWorkflow_id,

    -- * Destructuring the Response
    StopWorkflowResponse (..),
    newStopWorkflowResponse,

    -- * Response Lenses
    stopWorkflowResponse_arn,
    stopWorkflowResponse_id,
    stopWorkflowResponse_lastStopTime,
    stopWorkflowResponse_status,
    stopWorkflowResponse_statusMessage,
    stopWorkflowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopWorkflow' smart constructor.
data StopWorkflow = StopWorkflow'
  { -- | The ID of the migration workflow.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'stopWorkflow_id' - The ID of the migration workflow.
newStopWorkflow ::
  -- | 'id'
  Prelude.Text ->
  StopWorkflow
newStopWorkflow pId_ = StopWorkflow' {id = pId_}

-- | The ID of the migration workflow.
stopWorkflow_id :: Lens.Lens' StopWorkflow Prelude.Text
stopWorkflow_id = Lens.lens (\StopWorkflow' {id} -> id) (\s@StopWorkflow' {} a -> s {id = a} :: StopWorkflow)

instance Core.AWSRequest StopWorkflow where
  type AWSResponse StopWorkflow = StopWorkflowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopWorkflowResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "lastStopTime")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "statusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopWorkflow where
  hashWithSalt _salt StopWorkflow' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData StopWorkflow where
  rnf StopWorkflow' {..} = Prelude.rnf id

instance Data.ToHeaders StopWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopWorkflow where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopWorkflow where
  toPath StopWorkflow' {..} =
    Prelude.mconcat
      ["/migrationworkflow/", Data.toBS id, "/stop"]

instance Data.ToQuery StopWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopWorkflowResponse' smart constructor.
data StopWorkflowResponse = StopWorkflowResponse'
  { -- | The Amazon Resource Name (ARN) of the migration workflow.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the migration workflow.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time at which the migration workflow was stopped.
    lastStopTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the migration workflow.
    status :: Prelude.Maybe MigrationWorkflowStatusEnum,
    -- | The status message of the migration workflow.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'stopWorkflowResponse_arn' - The Amazon Resource Name (ARN) of the migration workflow.
--
-- 'id', 'stopWorkflowResponse_id' - The ID of the migration workflow.
--
-- 'lastStopTime', 'stopWorkflowResponse_lastStopTime' - The time at which the migration workflow was stopped.
--
-- 'status', 'stopWorkflowResponse_status' - The status of the migration workflow.
--
-- 'statusMessage', 'stopWorkflowResponse_statusMessage' - The status message of the migration workflow.
--
-- 'httpStatus', 'stopWorkflowResponse_httpStatus' - The response's http status code.
newStopWorkflowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopWorkflowResponse
newStopWorkflowResponse pHttpStatus_ =
  StopWorkflowResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      lastStopTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the migration workflow.
stopWorkflowResponse_arn :: Lens.Lens' StopWorkflowResponse (Prelude.Maybe Prelude.Text)
stopWorkflowResponse_arn = Lens.lens (\StopWorkflowResponse' {arn} -> arn) (\s@StopWorkflowResponse' {} a -> s {arn = a} :: StopWorkflowResponse)

-- | The ID of the migration workflow.
stopWorkflowResponse_id :: Lens.Lens' StopWorkflowResponse (Prelude.Maybe Prelude.Text)
stopWorkflowResponse_id = Lens.lens (\StopWorkflowResponse' {id} -> id) (\s@StopWorkflowResponse' {} a -> s {id = a} :: StopWorkflowResponse)

-- | The time at which the migration workflow was stopped.
stopWorkflowResponse_lastStopTime :: Lens.Lens' StopWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
stopWorkflowResponse_lastStopTime = Lens.lens (\StopWorkflowResponse' {lastStopTime} -> lastStopTime) (\s@StopWorkflowResponse' {} a -> s {lastStopTime = a} :: StopWorkflowResponse) Prelude.. Lens.mapping Data._Time

-- | The status of the migration workflow.
stopWorkflowResponse_status :: Lens.Lens' StopWorkflowResponse (Prelude.Maybe MigrationWorkflowStatusEnum)
stopWorkflowResponse_status = Lens.lens (\StopWorkflowResponse' {status} -> status) (\s@StopWorkflowResponse' {} a -> s {status = a} :: StopWorkflowResponse)

-- | The status message of the migration workflow.
stopWorkflowResponse_statusMessage :: Lens.Lens' StopWorkflowResponse (Prelude.Maybe Prelude.Text)
stopWorkflowResponse_statusMessage = Lens.lens (\StopWorkflowResponse' {statusMessage} -> statusMessage) (\s@StopWorkflowResponse' {} a -> s {statusMessage = a} :: StopWorkflowResponse)

-- | The response's http status code.
stopWorkflowResponse_httpStatus :: Lens.Lens' StopWorkflowResponse Prelude.Int
stopWorkflowResponse_httpStatus = Lens.lens (\StopWorkflowResponse' {httpStatus} -> httpStatus) (\s@StopWorkflowResponse' {} a -> s {httpStatus = a} :: StopWorkflowResponse)

instance Prelude.NFData StopWorkflowResponse where
  rnf StopWorkflowResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf lastStopTime `Prelude.seq`
          Prelude.rnf status `Prelude.seq`
            Prelude.rnf statusMessage `Prelude.seq`
              Prelude.rnf httpStatus
