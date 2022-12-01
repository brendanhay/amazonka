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
-- Module      : Amazonka.MigrationHubOrchestrator.DeleteWorkflowStepGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a step group in a migration workflow.
module Amazonka.MigrationHubOrchestrator.DeleteWorkflowStepGroup
  ( -- * Creating a Request
    DeleteWorkflowStepGroup (..),
    newDeleteWorkflowStepGroup,

    -- * Request Lenses
    deleteWorkflowStepGroup_workflowId,
    deleteWorkflowStepGroup_id,

    -- * Destructuring the Response
    DeleteWorkflowStepGroupResponse (..),
    newDeleteWorkflowStepGroupResponse,

    -- * Response Lenses
    deleteWorkflowStepGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWorkflowStepGroup' smart constructor.
data DeleteWorkflowStepGroup = DeleteWorkflowStepGroup'
  { -- | The ID of the migration workflow.
    workflowId :: Prelude.Text,
    -- | The ID of the step group you want to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkflowStepGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowId', 'deleteWorkflowStepGroup_workflowId' - The ID of the migration workflow.
--
-- 'id', 'deleteWorkflowStepGroup_id' - The ID of the step group you want to delete.
newDeleteWorkflowStepGroup ::
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  DeleteWorkflowStepGroup
newDeleteWorkflowStepGroup pWorkflowId_ pId_ =
  DeleteWorkflowStepGroup'
    { workflowId = pWorkflowId_,
      id = pId_
    }

-- | The ID of the migration workflow.
deleteWorkflowStepGroup_workflowId :: Lens.Lens' DeleteWorkflowStepGroup Prelude.Text
deleteWorkflowStepGroup_workflowId = Lens.lens (\DeleteWorkflowStepGroup' {workflowId} -> workflowId) (\s@DeleteWorkflowStepGroup' {} a -> s {workflowId = a} :: DeleteWorkflowStepGroup)

-- | The ID of the step group you want to delete.
deleteWorkflowStepGroup_id :: Lens.Lens' DeleteWorkflowStepGroup Prelude.Text
deleteWorkflowStepGroup_id = Lens.lens (\DeleteWorkflowStepGroup' {id} -> id) (\s@DeleteWorkflowStepGroup' {} a -> s {id = a} :: DeleteWorkflowStepGroup)

instance Core.AWSRequest DeleteWorkflowStepGroup where
  type
    AWSResponse DeleteWorkflowStepGroup =
      DeleteWorkflowStepGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkflowStepGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorkflowStepGroup where
  hashWithSalt _salt DeleteWorkflowStepGroup' {..} =
    _salt `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteWorkflowStepGroup where
  rnf DeleteWorkflowStepGroup' {..} =
    Prelude.rnf workflowId `Prelude.seq` Prelude.rnf id

instance Core.ToHeaders DeleteWorkflowStepGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteWorkflowStepGroup where
  toPath DeleteWorkflowStepGroup' {..} =
    Prelude.mconcat
      ["/workflowstepgroup/", Core.toBS id]

instance Core.ToQuery DeleteWorkflowStepGroup where
  toQuery DeleteWorkflowStepGroup' {..} =
    Prelude.mconcat ["workflowId" Core.=: workflowId]

-- | /See:/ 'newDeleteWorkflowStepGroupResponse' smart constructor.
data DeleteWorkflowStepGroupResponse = DeleteWorkflowStepGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkflowStepGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkflowStepGroupResponse_httpStatus' - The response's http status code.
newDeleteWorkflowStepGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWorkflowStepGroupResponse
newDeleteWorkflowStepGroupResponse pHttpStatus_ =
  DeleteWorkflowStepGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteWorkflowStepGroupResponse_httpStatus :: Lens.Lens' DeleteWorkflowStepGroupResponse Prelude.Int
deleteWorkflowStepGroupResponse_httpStatus = Lens.lens (\DeleteWorkflowStepGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkflowStepGroupResponse' {} a -> s {httpStatus = a} :: DeleteWorkflowStepGroupResponse)

instance
  Prelude.NFData
    DeleteWorkflowStepGroupResponse
  where
  rnf DeleteWorkflowStepGroupResponse' {..} =
    Prelude.rnf httpStatus
