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
-- Module      : Amazonka.ECS.UpdateTaskSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a task set. This is used when a service uses the @EXTERNAL@
-- deployment controller type. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Amazonka.ECS.UpdateTaskSet
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTaskSet' smart constructor.
data UpdateTaskSet = UpdateTaskSet'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the service that the task set is found in.
    cluster :: Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the service that
    -- the task set is found in.
    service :: Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the task set to
    -- update.
    taskSet :: Prelude.Text,
    -- | A floating-point percentage of the desired number of tasks to place and
    -- keep running in the task set.
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
-- hosts the service that the task set is found in.
--
-- 'service', 'updateTaskSet_service' - The short name or full Amazon Resource Name (ARN) of the service that
-- the task set is found in.
--
-- 'taskSet', 'updateTaskSet_taskSet' - The short name or full Amazon Resource Name (ARN) of the task set to
-- update.
--
-- 'scale', 'updateTaskSet_scale' - A floating-point percentage of the desired number of tasks to place and
-- keep running in the task set.
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
-- hosts the service that the task set is found in.
updateTaskSet_cluster :: Lens.Lens' UpdateTaskSet Prelude.Text
updateTaskSet_cluster = Lens.lens (\UpdateTaskSet' {cluster} -> cluster) (\s@UpdateTaskSet' {} a -> s {cluster = a} :: UpdateTaskSet)

-- | The short name or full Amazon Resource Name (ARN) of the service that
-- the task set is found in.
updateTaskSet_service :: Lens.Lens' UpdateTaskSet Prelude.Text
updateTaskSet_service = Lens.lens (\UpdateTaskSet' {service} -> service) (\s@UpdateTaskSet' {} a -> s {service = a} :: UpdateTaskSet)

-- | The short name or full Amazon Resource Name (ARN) of the task set to
-- update.
updateTaskSet_taskSet :: Lens.Lens' UpdateTaskSet Prelude.Text
updateTaskSet_taskSet = Lens.lens (\UpdateTaskSet' {taskSet} -> taskSet) (\s@UpdateTaskSet' {} a -> s {taskSet = a} :: UpdateTaskSet)

-- | A floating-point percentage of the desired number of tasks to place and
-- keep running in the task set.
updateTaskSet_scale :: Lens.Lens' UpdateTaskSet Scale
updateTaskSet_scale = Lens.lens (\UpdateTaskSet' {scale} -> scale) (\s@UpdateTaskSet' {} a -> s {scale = a} :: UpdateTaskSet)

instance Core.AWSRequest UpdateTaskSet where
  type
    AWSResponse UpdateTaskSet =
      UpdateTaskSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTaskSetResponse'
            Prelude.<$> (x Data..?> "taskSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTaskSet where
  hashWithSalt _salt UpdateTaskSet' {..} =
    _salt
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` service
      `Prelude.hashWithSalt` taskSet
      `Prelude.hashWithSalt` scale

instance Prelude.NFData UpdateTaskSet where
  rnf UpdateTaskSet' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf service
      `Prelude.seq` Prelude.rnf taskSet
      `Prelude.seq` Prelude.rnf scale

instance Data.ToHeaders UpdateTaskSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.UpdateTaskSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTaskSet where
  toJSON UpdateTaskSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("cluster" Data..= cluster),
            Prelude.Just ("service" Data..= service),
            Prelude.Just ("taskSet" Data..= taskSet),
            Prelude.Just ("scale" Data..= scale)
          ]
      )

instance Data.ToPath UpdateTaskSet where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTaskSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTaskSetResponse' smart constructor.
data UpdateTaskSetResponse = UpdateTaskSetResponse'
  { -- | Details about the task set.
    taskSet :: Prelude.Maybe TaskSet,
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
-- 'taskSet', 'updateTaskSetResponse_taskSet' - Details about the task set.
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

-- | Details about the task set.
updateTaskSetResponse_taskSet :: Lens.Lens' UpdateTaskSetResponse (Prelude.Maybe TaskSet)
updateTaskSetResponse_taskSet = Lens.lens (\UpdateTaskSetResponse' {taskSet} -> taskSet) (\s@UpdateTaskSetResponse' {} a -> s {taskSet = a} :: UpdateTaskSetResponse)

-- | The response's http status code.
updateTaskSetResponse_httpStatus :: Lens.Lens' UpdateTaskSetResponse Prelude.Int
updateTaskSetResponse_httpStatus = Lens.lens (\UpdateTaskSetResponse' {httpStatus} -> httpStatus) (\s@UpdateTaskSetResponse' {} a -> s {httpStatus = a} :: UpdateTaskSetResponse)

instance Prelude.NFData UpdateTaskSetResponse where
  rnf UpdateTaskSetResponse' {..} =
    Prelude.rnf taskSet
      `Prelude.seq` Prelude.rnf httpStatus
