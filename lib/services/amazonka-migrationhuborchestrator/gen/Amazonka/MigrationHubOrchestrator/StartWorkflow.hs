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
-- Module      : Amazonka.MigrationHubOrchestrator.StartWorkflow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start a migration workflow.
module Amazonka.MigrationHubOrchestrator.StartWorkflow
  ( -- * Creating a Request
    StartWorkflow (..),
    newStartWorkflow,

    -- * Request Lenses
    startWorkflow_id,

    -- * Destructuring the Response
    StartWorkflowResponse (..),
    newStartWorkflowResponse,

    -- * Response Lenses
    startWorkflowResponse_arn,
    startWorkflowResponse_id,
    startWorkflowResponse_lastStartTime,
    startWorkflowResponse_status,
    startWorkflowResponse_statusMessage,
    startWorkflowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartWorkflow' smart constructor.
data StartWorkflow = StartWorkflow'
  { -- | The ID of the migration workflow.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'startWorkflow_id' - The ID of the migration workflow.
newStartWorkflow ::
  -- | 'id'
  Prelude.Text ->
  StartWorkflow
newStartWorkflow pId_ = StartWorkflow' {id = pId_}

-- | The ID of the migration workflow.
startWorkflow_id :: Lens.Lens' StartWorkflow Prelude.Text
startWorkflow_id = Lens.lens (\StartWorkflow' {id} -> id) (\s@StartWorkflow' {} a -> s {id = a} :: StartWorkflow)

instance Core.AWSRequest StartWorkflow where
  type
    AWSResponse StartWorkflow =
      StartWorkflowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartWorkflowResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "lastStartTime")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "statusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartWorkflow where
  hashWithSalt _salt StartWorkflow' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData StartWorkflow where
  rnf StartWorkflow' {..} = Prelude.rnf id

instance Data.ToHeaders StartWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartWorkflow where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StartWorkflow where
  toPath StartWorkflow' {..} =
    Prelude.mconcat
      ["/migrationworkflow/", Data.toBS id, "/start"]

instance Data.ToQuery StartWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartWorkflowResponse' smart constructor.
data StartWorkflowResponse = StartWorkflowResponse'
  { -- | The Amazon Resource Name (ARN) of the migration workflow.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the migration workflow.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time at which the migration workflow was last started.
    lastStartTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the migration workflow.
    status :: Prelude.Maybe MigrationWorkflowStatusEnum,
    -- | The status message of the migration workflow.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'startWorkflowResponse_arn' - The Amazon Resource Name (ARN) of the migration workflow.
--
-- 'id', 'startWorkflowResponse_id' - The ID of the migration workflow.
--
-- 'lastStartTime', 'startWorkflowResponse_lastStartTime' - The time at which the migration workflow was last started.
--
-- 'status', 'startWorkflowResponse_status' - The status of the migration workflow.
--
-- 'statusMessage', 'startWorkflowResponse_statusMessage' - The status message of the migration workflow.
--
-- 'httpStatus', 'startWorkflowResponse_httpStatus' - The response's http status code.
newStartWorkflowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartWorkflowResponse
newStartWorkflowResponse pHttpStatus_ =
  StartWorkflowResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      lastStartTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the migration workflow.
startWorkflowResponse_arn :: Lens.Lens' StartWorkflowResponse (Prelude.Maybe Prelude.Text)
startWorkflowResponse_arn = Lens.lens (\StartWorkflowResponse' {arn} -> arn) (\s@StartWorkflowResponse' {} a -> s {arn = a} :: StartWorkflowResponse)

-- | The ID of the migration workflow.
startWorkflowResponse_id :: Lens.Lens' StartWorkflowResponse (Prelude.Maybe Prelude.Text)
startWorkflowResponse_id = Lens.lens (\StartWorkflowResponse' {id} -> id) (\s@StartWorkflowResponse' {} a -> s {id = a} :: StartWorkflowResponse)

-- | The time at which the migration workflow was last started.
startWorkflowResponse_lastStartTime :: Lens.Lens' StartWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
startWorkflowResponse_lastStartTime = Lens.lens (\StartWorkflowResponse' {lastStartTime} -> lastStartTime) (\s@StartWorkflowResponse' {} a -> s {lastStartTime = a} :: StartWorkflowResponse) Prelude.. Lens.mapping Data._Time

-- | The status of the migration workflow.
startWorkflowResponse_status :: Lens.Lens' StartWorkflowResponse (Prelude.Maybe MigrationWorkflowStatusEnum)
startWorkflowResponse_status = Lens.lens (\StartWorkflowResponse' {status} -> status) (\s@StartWorkflowResponse' {} a -> s {status = a} :: StartWorkflowResponse)

-- | The status message of the migration workflow.
startWorkflowResponse_statusMessage :: Lens.Lens' StartWorkflowResponse (Prelude.Maybe Prelude.Text)
startWorkflowResponse_statusMessage = Lens.lens (\StartWorkflowResponse' {statusMessage} -> statusMessage) (\s@StartWorkflowResponse' {} a -> s {statusMessage = a} :: StartWorkflowResponse)

-- | The response's http status code.
startWorkflowResponse_httpStatus :: Lens.Lens' StartWorkflowResponse Prelude.Int
startWorkflowResponse_httpStatus = Lens.lens (\StartWorkflowResponse' {httpStatus} -> httpStatus) (\s@StartWorkflowResponse' {} a -> s {httpStatus = a} :: StartWorkflowResponse)

instance Prelude.NFData StartWorkflowResponse where
  rnf StartWorkflowResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastStartTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf httpStatus
