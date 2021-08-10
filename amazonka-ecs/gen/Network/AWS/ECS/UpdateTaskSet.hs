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
-- Module      : Network.AWS.ECS.UpdateTaskSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a task set. This is used when a service uses the @EXTERNAL@
-- deployment controller type. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Network.AWS.ECS.UpdateTaskSet
  ( -- * Creating a Request
    UpdateTaskSet (..),
    newUpdateTaskSet,

    -- * Request Lenses
    updateTaskSet_cluster,
    updateTaskSet_service,
    updateTaskSet_taskSet,
    updateTaskSet_scale,

    -- * Destructuring the Response
    UpdateTaskSetResponse (..),
    newUpdateTaskSetResponse,

    -- * Response Lenses
    updateTaskSetResponse_taskSet,
    updateTaskSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTaskSet' smart constructor.
data UpdateTaskSet = UpdateTaskSet'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the service that the task set exists in.
    cluster :: Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the service that
    -- the task set exists in.
    service :: Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the task set to
    -- update.
    taskSet :: Prelude.Text,
    scale :: Scale
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTaskSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'updateTaskSet_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task set exists in.
--
-- 'service', 'updateTaskSet_service' - The short name or full Amazon Resource Name (ARN) of the service that
-- the task set exists in.
--
-- 'taskSet', 'updateTaskSet_taskSet' - The short name or full Amazon Resource Name (ARN) of the task set to
-- update.
--
-- 'scale', 'updateTaskSet_scale' - Undocumented member.
newUpdateTaskSet ::
  -- | 'cluster'
  Prelude.Text ->
  -- | 'service'
  Prelude.Text ->
  -- | 'taskSet'
  Prelude.Text ->
  -- | 'scale'
  Scale ->
  UpdateTaskSet
newUpdateTaskSet
  pCluster_
  pService_
  pTaskSet_
  pScale_ =
    UpdateTaskSet'
      { cluster = pCluster_,
        service = pService_,
        taskSet = pTaskSet_,
        scale = pScale_
      }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task set exists in.
updateTaskSet_cluster :: Lens.Lens' UpdateTaskSet Prelude.Text
updateTaskSet_cluster = Lens.lens (\UpdateTaskSet' {cluster} -> cluster) (\s@UpdateTaskSet' {} a -> s {cluster = a} :: UpdateTaskSet)

-- | The short name or full Amazon Resource Name (ARN) of the service that
-- the task set exists in.
updateTaskSet_service :: Lens.Lens' UpdateTaskSet Prelude.Text
updateTaskSet_service = Lens.lens (\UpdateTaskSet' {service} -> service) (\s@UpdateTaskSet' {} a -> s {service = a} :: UpdateTaskSet)

-- | The short name or full Amazon Resource Name (ARN) of the task set to
-- update.
updateTaskSet_taskSet :: Lens.Lens' UpdateTaskSet Prelude.Text
updateTaskSet_taskSet = Lens.lens (\UpdateTaskSet' {taskSet} -> taskSet) (\s@UpdateTaskSet' {} a -> s {taskSet = a} :: UpdateTaskSet)

-- | Undocumented member.
updateTaskSet_scale :: Lens.Lens' UpdateTaskSet Scale
updateTaskSet_scale = Lens.lens (\UpdateTaskSet' {scale} -> scale) (\s@UpdateTaskSet' {} a -> s {scale = a} :: UpdateTaskSet)

instance Core.AWSRequest UpdateTaskSet where
  type
    AWSResponse UpdateTaskSet =
      UpdateTaskSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTaskSetResponse'
            Prelude.<$> (x Core..?> "taskSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTaskSet

instance Prelude.NFData UpdateTaskSet

instance Core.ToHeaders UpdateTaskSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.UpdateTaskSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateTaskSet where
  toJSON UpdateTaskSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("cluster" Core..= cluster),
            Prelude.Just ("service" Core..= service),
            Prelude.Just ("taskSet" Core..= taskSet),
            Prelude.Just ("scale" Core..= scale)
          ]
      )

instance Core.ToPath UpdateTaskSet where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateTaskSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTaskSetResponse' smart constructor.
data UpdateTaskSetResponse = UpdateTaskSetResponse'
  { taskSet :: Prelude.Maybe TaskSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTaskSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskSet', 'updateTaskSetResponse_taskSet' - Undocumented member.
--
-- 'httpStatus', 'updateTaskSetResponse_httpStatus' - The response's http status code.
newUpdateTaskSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTaskSetResponse
newUpdateTaskSetResponse pHttpStatus_ =
  UpdateTaskSetResponse'
    { taskSet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateTaskSetResponse_taskSet :: Lens.Lens' UpdateTaskSetResponse (Prelude.Maybe TaskSet)
updateTaskSetResponse_taskSet = Lens.lens (\UpdateTaskSetResponse' {taskSet} -> taskSet) (\s@UpdateTaskSetResponse' {} a -> s {taskSet = a} :: UpdateTaskSetResponse)

-- | The response's http status code.
updateTaskSetResponse_httpStatus :: Lens.Lens' UpdateTaskSetResponse Prelude.Int
updateTaskSetResponse_httpStatus = Lens.lens (\UpdateTaskSetResponse' {httpStatus} -> httpStatus) (\s@UpdateTaskSetResponse' {} a -> s {httpStatus = a} :: UpdateTaskSetResponse)

instance Prelude.NFData UpdateTaskSetResponse
