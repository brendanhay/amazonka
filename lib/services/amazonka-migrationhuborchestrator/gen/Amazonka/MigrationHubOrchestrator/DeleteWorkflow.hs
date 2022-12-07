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
-- Module      : Amazonka.MigrationHubOrchestrator.DeleteWorkflow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a migration workflow. You must pause a running workflow in
-- Migration Hub Orchestrator console to delete it.
module Amazonka.MigrationHubOrchestrator.DeleteWorkflow
  ( -- * Creating a Request
    DeleteWorkflow (..),
    newDeleteWorkflow,

    -- * Request Lenses
    deleteWorkflow_id,

    -- * Destructuring the Response
    DeleteWorkflowResponse (..),
    newDeleteWorkflowResponse,

    -- * Response Lenses
    deleteWorkflowResponse_arn,
    deleteWorkflowResponse_status,
    deleteWorkflowResponse_id,
    deleteWorkflowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWorkflow' smart constructor.
data DeleteWorkflow = DeleteWorkflow'
  { -- | The ID of the migration workflow you want to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteWorkflow_id' - The ID of the migration workflow you want to delete.
newDeleteWorkflow ::
  -- | 'id'
  Prelude.Text ->
  DeleteWorkflow
newDeleteWorkflow pId_ = DeleteWorkflow' {id = pId_}

-- | The ID of the migration workflow you want to delete.
deleteWorkflow_id :: Lens.Lens' DeleteWorkflow Prelude.Text
deleteWorkflow_id = Lens.lens (\DeleteWorkflow' {id} -> id) (\s@DeleteWorkflow' {} a -> s {id = a} :: DeleteWorkflow)

instance Core.AWSRequest DeleteWorkflow where
  type
    AWSResponse DeleteWorkflow =
      DeleteWorkflowResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteWorkflowResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorkflow where
  hashWithSalt _salt DeleteWorkflow' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteWorkflow where
  rnf DeleteWorkflow' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteWorkflow where
  toPath DeleteWorkflow' {..} =
    Prelude.mconcat
      ["/migrationworkflow/", Data.toBS id]

instance Data.ToQuery DeleteWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkflowResponse' smart constructor.
data DeleteWorkflowResponse = DeleteWorkflowResponse'
  { -- | The Amazon Resource Name (ARN) of the migration workflow.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the migration workflow.
    status :: Prelude.Maybe MigrationWorkflowStatusEnum,
    -- | The ID of the migration workflow.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteWorkflowResponse_arn' - The Amazon Resource Name (ARN) of the migration workflow.
--
-- 'status', 'deleteWorkflowResponse_status' - The status of the migration workflow.
--
-- 'id', 'deleteWorkflowResponse_id' - The ID of the migration workflow.
--
-- 'httpStatus', 'deleteWorkflowResponse_httpStatus' - The response's http status code.
newDeleteWorkflowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWorkflowResponse
newDeleteWorkflowResponse pHttpStatus_ =
  DeleteWorkflowResponse'
    { arn = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the migration workflow.
deleteWorkflowResponse_arn :: Lens.Lens' DeleteWorkflowResponse (Prelude.Maybe Prelude.Text)
deleteWorkflowResponse_arn = Lens.lens (\DeleteWorkflowResponse' {arn} -> arn) (\s@DeleteWorkflowResponse' {} a -> s {arn = a} :: DeleteWorkflowResponse)

-- | The status of the migration workflow.
deleteWorkflowResponse_status :: Lens.Lens' DeleteWorkflowResponse (Prelude.Maybe MigrationWorkflowStatusEnum)
deleteWorkflowResponse_status = Lens.lens (\DeleteWorkflowResponse' {status} -> status) (\s@DeleteWorkflowResponse' {} a -> s {status = a} :: DeleteWorkflowResponse)

-- | The ID of the migration workflow.
deleteWorkflowResponse_id :: Lens.Lens' DeleteWorkflowResponse (Prelude.Maybe Prelude.Text)
deleteWorkflowResponse_id = Lens.lens (\DeleteWorkflowResponse' {id} -> id) (\s@DeleteWorkflowResponse' {} a -> s {id = a} :: DeleteWorkflowResponse)

-- | The response's http status code.
deleteWorkflowResponse_httpStatus :: Lens.Lens' DeleteWorkflowResponse Prelude.Int
deleteWorkflowResponse_httpStatus = Lens.lens (\DeleteWorkflowResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkflowResponse' {} a -> s {httpStatus = a} :: DeleteWorkflowResponse)

instance Prelude.NFData DeleteWorkflowResponse where
  rnf DeleteWorkflowResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
