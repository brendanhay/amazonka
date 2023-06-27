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
-- Module      : Amazonka.ECS.DeleteTaskDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more task definitions.
--
-- You must deregister a task definition revision before you delete it. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeregisterTaskDefinition.html DeregisterTaskDefinition>.
--
-- When you delete a task definition revision, it is immediately
-- transitions from the @INACTIVE@ to @DELETE_IN_PROGRESS@. Existing tasks
-- and services that reference a @DELETE_IN_PROGRESS@ task definition
-- revision continue to run without disruption. Existing services that
-- reference a @DELETE_IN_PROGRESS@ task definition revision can still
-- scale up or down by modifying the service\'s desired count.
--
-- You can\'t use a @DELETE_IN_PROGRESS@ task definition revision to run
-- new tasks or create new services. You also can\'t update an existing
-- service to reference a @DELETE_IN_PROGRESS@ task definition revision.
--
-- A task definition revision will stay in @DELETE_IN_PROGRESS@ status
-- until all the associated tasks and services have been terminated.
--
-- When you delete all @INACTIVE@ task definition revisions, the task
-- definition name is not displayed in the console and not returned in the
-- API. If a task definition revisions are in the @DELETE_IN_PROGRESS@
-- state, the task definition name is displayed in the console and returned
-- in the API. The task definition name is retained by Amazon ECS and the
-- revision is incremented the next time you create a task definition with
-- that name.
module Amazonka.ECS.DeleteTaskDefinitions
  ( -- * Creating a Request
    DeleteTaskDefinitions (..),
    newDeleteTaskDefinitions,

    -- * Request Lenses
    deleteTaskDefinitions_taskDefinitions,

    -- * Destructuring the Response
    DeleteTaskDefinitionsResponse (..),
    newDeleteTaskDefinitionsResponse,

    -- * Response Lenses
    deleteTaskDefinitionsResponse_failures,
    deleteTaskDefinitionsResponse_taskDefinitions,
    deleteTaskDefinitionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTaskDefinitions' smart constructor.
data DeleteTaskDefinitions = DeleteTaskDefinitions'
  { -- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
    -- Name (ARN) of the task definition to delete. You must specify a
    -- @revision@.
    --
    -- You can specify up to 10 task definitions as a comma separated list.
    taskDefinitions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTaskDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskDefinitions', 'deleteTaskDefinitions_taskDefinitions' - The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition to delete. You must specify a
-- @revision@.
--
-- You can specify up to 10 task definitions as a comma separated list.
newDeleteTaskDefinitions ::
  DeleteTaskDefinitions
newDeleteTaskDefinitions =
  DeleteTaskDefinitions'
    { taskDefinitions =
        Prelude.mempty
    }

-- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition to delete. You must specify a
-- @revision@.
--
-- You can specify up to 10 task definitions as a comma separated list.
deleteTaskDefinitions_taskDefinitions :: Lens.Lens' DeleteTaskDefinitions [Prelude.Text]
deleteTaskDefinitions_taskDefinitions = Lens.lens (\DeleteTaskDefinitions' {taskDefinitions} -> taskDefinitions) (\s@DeleteTaskDefinitions' {} a -> s {taskDefinitions = a} :: DeleteTaskDefinitions) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteTaskDefinitions where
  type
    AWSResponse DeleteTaskDefinitions =
      DeleteTaskDefinitionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTaskDefinitionsResponse'
            Prelude.<$> (x Data..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "taskDefinitions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTaskDefinitions where
  hashWithSalt _salt DeleteTaskDefinitions' {..} =
    _salt `Prelude.hashWithSalt` taskDefinitions

instance Prelude.NFData DeleteTaskDefinitions where
  rnf DeleteTaskDefinitions' {..} =
    Prelude.rnf taskDefinitions

instance Data.ToHeaders DeleteTaskDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.DeleteTaskDefinitions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTaskDefinitions where
  toJSON DeleteTaskDefinitions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("taskDefinitions" Data..= taskDefinitions)
          ]
      )

instance Data.ToPath DeleteTaskDefinitions where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTaskDefinitions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTaskDefinitionsResponse' smart constructor.
data DeleteTaskDefinitionsResponse = DeleteTaskDefinitionsResponse'
  { -- | Any failures associated with the call.
    failures :: Prelude.Maybe [Failure],
    -- | The list of deleted task definitions.
    taskDefinitions :: Prelude.Maybe [TaskDefinition],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTaskDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'deleteTaskDefinitionsResponse_failures' - Any failures associated with the call.
--
-- 'taskDefinitions', 'deleteTaskDefinitionsResponse_taskDefinitions' - The list of deleted task definitions.
--
-- 'httpStatus', 'deleteTaskDefinitionsResponse_httpStatus' - The response's http status code.
newDeleteTaskDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTaskDefinitionsResponse
newDeleteTaskDefinitionsResponse pHttpStatus_ =
  DeleteTaskDefinitionsResponse'
    { failures =
        Prelude.Nothing,
      taskDefinitions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any failures associated with the call.
deleteTaskDefinitionsResponse_failures :: Lens.Lens' DeleteTaskDefinitionsResponse (Prelude.Maybe [Failure])
deleteTaskDefinitionsResponse_failures = Lens.lens (\DeleteTaskDefinitionsResponse' {failures} -> failures) (\s@DeleteTaskDefinitionsResponse' {} a -> s {failures = a} :: DeleteTaskDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of deleted task definitions.
deleteTaskDefinitionsResponse_taskDefinitions :: Lens.Lens' DeleteTaskDefinitionsResponse (Prelude.Maybe [TaskDefinition])
deleteTaskDefinitionsResponse_taskDefinitions = Lens.lens (\DeleteTaskDefinitionsResponse' {taskDefinitions} -> taskDefinitions) (\s@DeleteTaskDefinitionsResponse' {} a -> s {taskDefinitions = a} :: DeleteTaskDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteTaskDefinitionsResponse_httpStatus :: Lens.Lens' DeleteTaskDefinitionsResponse Prelude.Int
deleteTaskDefinitionsResponse_httpStatus = Lens.lens (\DeleteTaskDefinitionsResponse' {httpStatus} -> httpStatus) (\s@DeleteTaskDefinitionsResponse' {} a -> s {httpStatus = a} :: DeleteTaskDefinitionsResponse)

instance Prelude.NFData DeleteTaskDefinitionsResponse where
  rnf DeleteTaskDefinitionsResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf taskDefinitions
      `Prelude.seq` Prelude.rnf httpStatus
