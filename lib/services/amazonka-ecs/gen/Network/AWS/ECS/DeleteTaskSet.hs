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
-- Module      : Network.AWS.ECS.DeleteTaskSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified task set within a service. This is used when a
-- service uses the @EXTERNAL@ deployment controller type. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Network.AWS.ECS.DeleteTaskSet
  ( -- * Creating a Request
    DeleteTaskSet (..),
    newDeleteTaskSet,

    -- * Request Lenses
    deleteTaskSet_force,
    deleteTaskSet_cluster,
    deleteTaskSet_service,
    deleteTaskSet_taskSet,

    -- * Destructuring the Response
    DeleteTaskSetResponse (..),
    newDeleteTaskSetResponse,

    -- * Response Lenses
    deleteTaskSetResponse_taskSet,
    deleteTaskSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTaskSet' smart constructor.
data DeleteTaskSet = DeleteTaskSet'
  { -- | If @true@, this allows you to delete a task set even if it hasn\'t been
    -- scaled down to zero.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the service that the task set exists in to delete.
    cluster :: Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the service that
    -- hosts the task set to delete.
    service :: Prelude.Text,
    -- | The task set ID or full Amazon Resource Name (ARN) of the task set to
    -- delete.
    taskSet :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTaskSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'deleteTaskSet_force' - If @true@, this allows you to delete a task set even if it hasn\'t been
-- scaled down to zero.
--
-- 'cluster', 'deleteTaskSet_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task set exists in to delete.
--
-- 'service', 'deleteTaskSet_service' - The short name or full Amazon Resource Name (ARN) of the service that
-- hosts the task set to delete.
--
-- 'taskSet', 'deleteTaskSet_taskSet' - The task set ID or full Amazon Resource Name (ARN) of the task set to
-- delete.
newDeleteTaskSet ::
  -- | 'cluster'
  Prelude.Text ->
  -- | 'service'
  Prelude.Text ->
  -- | 'taskSet'
  Prelude.Text ->
  DeleteTaskSet
newDeleteTaskSet pCluster_ pService_ pTaskSet_ =
  DeleteTaskSet'
    { force = Prelude.Nothing,
      cluster = pCluster_,
      service = pService_,
      taskSet = pTaskSet_
    }

-- | If @true@, this allows you to delete a task set even if it hasn\'t been
-- scaled down to zero.
deleteTaskSet_force :: Lens.Lens' DeleteTaskSet (Prelude.Maybe Prelude.Bool)
deleteTaskSet_force = Lens.lens (\DeleteTaskSet' {force} -> force) (\s@DeleteTaskSet' {} a -> s {force = a} :: DeleteTaskSet)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task set exists in to delete.
deleteTaskSet_cluster :: Lens.Lens' DeleteTaskSet Prelude.Text
deleteTaskSet_cluster = Lens.lens (\DeleteTaskSet' {cluster} -> cluster) (\s@DeleteTaskSet' {} a -> s {cluster = a} :: DeleteTaskSet)

-- | The short name or full Amazon Resource Name (ARN) of the service that
-- hosts the task set to delete.
deleteTaskSet_service :: Lens.Lens' DeleteTaskSet Prelude.Text
deleteTaskSet_service = Lens.lens (\DeleteTaskSet' {service} -> service) (\s@DeleteTaskSet' {} a -> s {service = a} :: DeleteTaskSet)

-- | The task set ID or full Amazon Resource Name (ARN) of the task set to
-- delete.
deleteTaskSet_taskSet :: Lens.Lens' DeleteTaskSet Prelude.Text
deleteTaskSet_taskSet = Lens.lens (\DeleteTaskSet' {taskSet} -> taskSet) (\s@DeleteTaskSet' {} a -> s {taskSet = a} :: DeleteTaskSet)

instance Core.AWSRequest DeleteTaskSet where
  type
    AWSResponse DeleteTaskSet =
      DeleteTaskSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTaskSetResponse'
            Prelude.<$> (x Core..?> "taskSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTaskSet

instance Prelude.NFData DeleteTaskSet

instance Core.ToHeaders DeleteTaskSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DeleteTaskSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteTaskSet where
  toJSON DeleteTaskSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("force" Core..=) Prelude.<$> force,
            Prelude.Just ("cluster" Core..= cluster),
            Prelude.Just ("service" Core..= service),
            Prelude.Just ("taskSet" Core..= taskSet)
          ]
      )

instance Core.ToPath DeleteTaskSet where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteTaskSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTaskSetResponse' smart constructor.
data DeleteTaskSetResponse = DeleteTaskSetResponse'
  { -- | Details about the task set.
    taskSet :: Prelude.Maybe TaskSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTaskSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskSet', 'deleteTaskSetResponse_taskSet' - Details about the task set.
--
-- 'httpStatus', 'deleteTaskSetResponse_httpStatus' - The response's http status code.
newDeleteTaskSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTaskSetResponse
newDeleteTaskSetResponse pHttpStatus_ =
  DeleteTaskSetResponse'
    { taskSet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the task set.
deleteTaskSetResponse_taskSet :: Lens.Lens' DeleteTaskSetResponse (Prelude.Maybe TaskSet)
deleteTaskSetResponse_taskSet = Lens.lens (\DeleteTaskSetResponse' {taskSet} -> taskSet) (\s@DeleteTaskSetResponse' {} a -> s {taskSet = a} :: DeleteTaskSetResponse)

-- | The response's http status code.
deleteTaskSetResponse_httpStatus :: Lens.Lens' DeleteTaskSetResponse Prelude.Int
deleteTaskSetResponse_httpStatus = Lens.lens (\DeleteTaskSetResponse' {httpStatus} -> httpStatus) (\s@DeleteTaskSetResponse' {} a -> s {httpStatus = a} :: DeleteTaskSetResponse)

instance Prelude.NFData DeleteTaskSetResponse
