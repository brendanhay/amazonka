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
-- Module      : Amazonka.MigrationHubOrchestrator.RetryWorkflowStep
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retry a failed step in a migration workflow.
module Amazonka.MigrationHubOrchestrator.RetryWorkflowStep
  ( -- * Creating a Request
    RetryWorkflowStep (..),
    newRetryWorkflowStep,

    -- * Request Lenses
    retryWorkflowStep_workflowId,
    retryWorkflowStep_stepGroupId,
    retryWorkflowStep_id,

    -- * Destructuring the Response
    RetryWorkflowStepResponse (..),
    newRetryWorkflowStepResponse,

    -- * Response Lenses
    retryWorkflowStepResponse_id,
    retryWorkflowStepResponse_status,
    retryWorkflowStepResponse_stepGroupId,
    retryWorkflowStepResponse_workflowId,
    retryWorkflowStepResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRetryWorkflowStep' smart constructor.
data RetryWorkflowStep = RetryWorkflowStep'
  { -- | The ID of the migration workflow.
    workflowId :: Prelude.Text,
    -- | The ID of the step group.
    stepGroupId :: Prelude.Text,
    -- | The ID of the step.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetryWorkflowStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowId', 'retryWorkflowStep_workflowId' - The ID of the migration workflow.
--
-- 'stepGroupId', 'retryWorkflowStep_stepGroupId' - The ID of the step group.
--
-- 'id', 'retryWorkflowStep_id' - The ID of the step.
newRetryWorkflowStep ::
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'stepGroupId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  RetryWorkflowStep
newRetryWorkflowStep pWorkflowId_ pStepGroupId_ pId_ =
  RetryWorkflowStep'
    { workflowId = pWorkflowId_,
      stepGroupId = pStepGroupId_,
      id = pId_
    }

-- | The ID of the migration workflow.
retryWorkflowStep_workflowId :: Lens.Lens' RetryWorkflowStep Prelude.Text
retryWorkflowStep_workflowId = Lens.lens (\RetryWorkflowStep' {workflowId} -> workflowId) (\s@RetryWorkflowStep' {} a -> s {workflowId = a} :: RetryWorkflowStep)

-- | The ID of the step group.
retryWorkflowStep_stepGroupId :: Lens.Lens' RetryWorkflowStep Prelude.Text
retryWorkflowStep_stepGroupId = Lens.lens (\RetryWorkflowStep' {stepGroupId} -> stepGroupId) (\s@RetryWorkflowStep' {} a -> s {stepGroupId = a} :: RetryWorkflowStep)

-- | The ID of the step.
retryWorkflowStep_id :: Lens.Lens' RetryWorkflowStep Prelude.Text
retryWorkflowStep_id = Lens.lens (\RetryWorkflowStep' {id} -> id) (\s@RetryWorkflowStep' {} a -> s {id = a} :: RetryWorkflowStep)

instance Core.AWSRequest RetryWorkflowStep where
  type
    AWSResponse RetryWorkflowStep =
      RetryWorkflowStepResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RetryWorkflowStepResponse'
            Prelude.<$> (x Data..?> "id")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "stepGroupId")
            Prelude.<*> (x Data..?> "workflowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RetryWorkflowStep where
  hashWithSalt _salt RetryWorkflowStep' {..} =
    _salt
      `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` stepGroupId
      `Prelude.hashWithSalt` id

instance Prelude.NFData RetryWorkflowStep where
  rnf RetryWorkflowStep' {..} =
    Prelude.rnf workflowId `Prelude.seq`
      Prelude.rnf stepGroupId `Prelude.seq`
        Prelude.rnf id

instance Data.ToHeaders RetryWorkflowStep where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RetryWorkflowStep where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath RetryWorkflowStep where
  toPath RetryWorkflowStep' {..} =
    Prelude.mconcat
      ["/retryworkflowstep/", Data.toBS id]

instance Data.ToQuery RetryWorkflowStep where
  toQuery RetryWorkflowStep' {..} =
    Prelude.mconcat
      [ "workflowId" Data.=: workflowId,
        "stepGroupId" Data.=: stepGroupId
      ]

-- | /See:/ 'newRetryWorkflowStepResponse' smart constructor.
data RetryWorkflowStepResponse = RetryWorkflowStepResponse'
  { -- | The ID of the step.
    id :: Prelude.Maybe Prelude.Text,
    -- | The status of the step.
    status :: Prelude.Maybe StepStatus,
    -- | The ID of the step group.
    stepGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the migration workflow.
    workflowId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetryWorkflowStepResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'retryWorkflowStepResponse_id' - The ID of the step.
--
-- 'status', 'retryWorkflowStepResponse_status' - The status of the step.
--
-- 'stepGroupId', 'retryWorkflowStepResponse_stepGroupId' - The ID of the step group.
--
-- 'workflowId', 'retryWorkflowStepResponse_workflowId' - The ID of the migration workflow.
--
-- 'httpStatus', 'retryWorkflowStepResponse_httpStatus' - The response's http status code.
newRetryWorkflowStepResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RetryWorkflowStepResponse
newRetryWorkflowStepResponse pHttpStatus_ =
  RetryWorkflowStepResponse'
    { id = Prelude.Nothing,
      status = Prelude.Nothing,
      stepGroupId = Prelude.Nothing,
      workflowId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the step.
retryWorkflowStepResponse_id :: Lens.Lens' RetryWorkflowStepResponse (Prelude.Maybe Prelude.Text)
retryWorkflowStepResponse_id = Lens.lens (\RetryWorkflowStepResponse' {id} -> id) (\s@RetryWorkflowStepResponse' {} a -> s {id = a} :: RetryWorkflowStepResponse)

-- | The status of the step.
retryWorkflowStepResponse_status :: Lens.Lens' RetryWorkflowStepResponse (Prelude.Maybe StepStatus)
retryWorkflowStepResponse_status = Lens.lens (\RetryWorkflowStepResponse' {status} -> status) (\s@RetryWorkflowStepResponse' {} a -> s {status = a} :: RetryWorkflowStepResponse)

-- | The ID of the step group.
retryWorkflowStepResponse_stepGroupId :: Lens.Lens' RetryWorkflowStepResponse (Prelude.Maybe Prelude.Text)
retryWorkflowStepResponse_stepGroupId = Lens.lens (\RetryWorkflowStepResponse' {stepGroupId} -> stepGroupId) (\s@RetryWorkflowStepResponse' {} a -> s {stepGroupId = a} :: RetryWorkflowStepResponse)

-- | The ID of the migration workflow.
retryWorkflowStepResponse_workflowId :: Lens.Lens' RetryWorkflowStepResponse (Prelude.Maybe Prelude.Text)
retryWorkflowStepResponse_workflowId = Lens.lens (\RetryWorkflowStepResponse' {workflowId} -> workflowId) (\s@RetryWorkflowStepResponse' {} a -> s {workflowId = a} :: RetryWorkflowStepResponse)

-- | The response's http status code.
retryWorkflowStepResponse_httpStatus :: Lens.Lens' RetryWorkflowStepResponse Prelude.Int
retryWorkflowStepResponse_httpStatus = Lens.lens (\RetryWorkflowStepResponse' {httpStatus} -> httpStatus) (\s@RetryWorkflowStepResponse' {} a -> s {httpStatus = a} :: RetryWorkflowStepResponse)

instance Prelude.NFData RetryWorkflowStepResponse where
  rnf RetryWorkflowStepResponse' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf status `Prelude.seq`
        Prelude.rnf stepGroupId `Prelude.seq`
          Prelude.rnf workflowId `Prelude.seq`
            Prelude.rnf httpStatus
