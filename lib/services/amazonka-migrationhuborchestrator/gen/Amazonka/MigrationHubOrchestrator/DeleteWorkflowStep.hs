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
-- Module      : Amazonka.MigrationHubOrchestrator.DeleteWorkflowStep
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a step in a migration workflow. Pause the workflow to delete a
-- running step.
module Amazonka.MigrationHubOrchestrator.DeleteWorkflowStep
  ( -- * Creating a Request
    DeleteWorkflowStep (..),
    newDeleteWorkflowStep,

    -- * Request Lenses
    deleteWorkflowStep_id,
    deleteWorkflowStep_stepGroupId,
    deleteWorkflowStep_workflowId,

    -- * Destructuring the Response
    DeleteWorkflowStepResponse (..),
    newDeleteWorkflowStepResponse,

    -- * Response Lenses
    deleteWorkflowStepResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWorkflowStep' smart constructor.
data DeleteWorkflowStep = DeleteWorkflowStep'
  { -- | The ID of the step you want to delete.
    id :: Prelude.Text,
    -- | The ID of the step group that contains the step you want to delete.
    stepGroupId :: Prelude.Text,
    -- | The ID of the migration workflow.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkflowStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteWorkflowStep_id' - The ID of the step you want to delete.
--
-- 'stepGroupId', 'deleteWorkflowStep_stepGroupId' - The ID of the step group that contains the step you want to delete.
--
-- 'workflowId', 'deleteWorkflowStep_workflowId' - The ID of the migration workflow.
newDeleteWorkflowStep ::
  -- | 'id'
  Prelude.Text ->
  -- | 'stepGroupId'
  Prelude.Text ->
  -- | 'workflowId'
  Prelude.Text ->
  DeleteWorkflowStep
newDeleteWorkflowStep pId_ pStepGroupId_ pWorkflowId_ =
  DeleteWorkflowStep'
    { id = pId_,
      stepGroupId = pStepGroupId_,
      workflowId = pWorkflowId_
    }

-- | The ID of the step you want to delete.
deleteWorkflowStep_id :: Lens.Lens' DeleteWorkflowStep Prelude.Text
deleteWorkflowStep_id = Lens.lens (\DeleteWorkflowStep' {id} -> id) (\s@DeleteWorkflowStep' {} a -> s {id = a} :: DeleteWorkflowStep)

-- | The ID of the step group that contains the step you want to delete.
deleteWorkflowStep_stepGroupId :: Lens.Lens' DeleteWorkflowStep Prelude.Text
deleteWorkflowStep_stepGroupId = Lens.lens (\DeleteWorkflowStep' {stepGroupId} -> stepGroupId) (\s@DeleteWorkflowStep' {} a -> s {stepGroupId = a} :: DeleteWorkflowStep)

-- | The ID of the migration workflow.
deleteWorkflowStep_workflowId :: Lens.Lens' DeleteWorkflowStep Prelude.Text
deleteWorkflowStep_workflowId = Lens.lens (\DeleteWorkflowStep' {workflowId} -> workflowId) (\s@DeleteWorkflowStep' {} a -> s {workflowId = a} :: DeleteWorkflowStep)

instance Core.AWSRequest DeleteWorkflowStep where
  type
    AWSResponse DeleteWorkflowStep =
      DeleteWorkflowStepResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkflowStepResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorkflowStep where
  hashWithSalt _salt DeleteWorkflowStep' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` stepGroupId
      `Prelude.hashWithSalt` workflowId

instance Prelude.NFData DeleteWorkflowStep where
  rnf DeleteWorkflowStep' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf stepGroupId `Prelude.seq`
        Prelude.rnf workflowId

instance Data.ToHeaders DeleteWorkflowStep where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteWorkflowStep where
  toPath DeleteWorkflowStep' {..} =
    Prelude.mconcat ["/workflowstep/", Data.toBS id]

instance Data.ToQuery DeleteWorkflowStep where
  toQuery DeleteWorkflowStep' {..} =
    Prelude.mconcat
      [ "stepGroupId" Data.=: stepGroupId,
        "workflowId" Data.=: workflowId
      ]

-- | /See:/ 'newDeleteWorkflowStepResponse' smart constructor.
data DeleteWorkflowStepResponse = DeleteWorkflowStepResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkflowStepResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkflowStepResponse_httpStatus' - The response's http status code.
newDeleteWorkflowStepResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWorkflowStepResponse
newDeleteWorkflowStepResponse pHttpStatus_ =
  DeleteWorkflowStepResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteWorkflowStepResponse_httpStatus :: Lens.Lens' DeleteWorkflowStepResponse Prelude.Int
deleteWorkflowStepResponse_httpStatus = Lens.lens (\DeleteWorkflowStepResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkflowStepResponse' {} a -> s {httpStatus = a} :: DeleteWorkflowStepResponse)

instance Prelude.NFData DeleteWorkflowStepResponse where
  rnf DeleteWorkflowStepResponse' {..} =
    Prelude.rnf httpStatus
