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
-- Module      : Amazonka.ECS.UpdateContainerInstancesState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the status of an Amazon ECS container instance.
--
-- Once a container instance has reached an @ACTIVE@ state, you can change
-- the status of a container instance to @DRAINING@ to manually remove an
-- instance from a cluster, for example to perform system updates, update
-- the Docker daemon, or scale down the cluster size.
--
-- A container instance can\'t be changed to @DRAINING@ until it has
-- reached an @ACTIVE@ status. If the instance is in any other status, an
-- error will be received.
--
-- When you set a container instance to @DRAINING@, Amazon ECS prevents new
-- tasks from being scheduled for placement on the container instance and
-- replacement service tasks are started on other container instances in
-- the cluster if the resources are available. Service tasks on the
-- container instance that are in the @PENDING@ state are stopped
-- immediately.
--
-- Service tasks on the container instance that are in the @RUNNING@ state
-- are stopped and replaced according to the service\'s deployment
-- configuration parameters, @minimumHealthyPercent@ and @maximumPercent@.
-- You can change the deployment configuration of your service using
-- UpdateService.
--
-- -   If @minimumHealthyPercent@ is below 100%, the scheduler can ignore
--     @desiredCount@ temporarily during task replacement. For example,
--     @desiredCount@ is four tasks, a minimum of 50% allows the scheduler
--     to stop two existing tasks before starting two new tasks. If the
--     minimum is 100%, the service scheduler can\'t remove existing tasks
--     until the replacement tasks are considered healthy. Tasks for
--     services that do not use a load balancer are considered healthy if
--     they\'re in the @RUNNING@ state. Tasks for services that use a load
--     balancer are considered healthy if they\'re in the @RUNNING@ state
--     and are reported as healthy by the load balancer.
--
-- -   The @maximumPercent@ parameter represents an upper limit on the
--     number of running tasks during task replacement. You can use this to
--     define the replacement batch size. For example, if @desiredCount@ is
--     four tasks, a maximum of 200% starts four new tasks before stopping
--     the four tasks to be drained, provided that the cluster resources
--     required to do this are available. If the maximum is 100%, then
--     replacement tasks can\'t start until the draining tasks have
--     stopped.
--
-- Any @PENDING@ or @RUNNING@ tasks that do not belong to a service aren\'t
-- affected. You must wait for them to finish or stop them manually.
--
-- A container instance has completed draining when it has no more
-- @RUNNING@ tasks. You can verify this using ListTasks.
--
-- When a container instance has been drained, you can set a container
-- instance to @ACTIVE@ status and once it has reached that status the
-- Amazon ECS scheduler can begin scheduling tasks on the instance again.
module Amazonka.ECS.UpdateContainerInstancesState
  ( -- * Creating a Request
    UpdateContainerInstancesState (..),
    newUpdateContainerInstancesState,

    -- * Request Lenses
    updateContainerInstancesState_cluster,
    updateContainerInstancesState_containerInstances,
    updateContainerInstancesState_status,

    -- * Destructuring the Response
    UpdateContainerInstancesStateResponse (..),
    newUpdateContainerInstancesStateResponse,

    -- * Response Lenses
    updateContainerInstancesStateResponse_containerInstances,
    updateContainerInstancesStateResponse_failures,
    updateContainerInstancesStateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContainerInstancesState' smart constructor.
data UpdateContainerInstancesState = UpdateContainerInstancesState'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the container instance to update. If you do not specify a cluster,
    -- the default cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 10 container instance IDs or full ARN entries.
    containerInstances :: [Prelude.Text],
    -- | The container instance state to update the container instance with. The
    -- only valid values for this action are @ACTIVE@ and @DRAINING@. A
    -- container instance can only be updated to @DRAINING@ status once it has
    -- reached an @ACTIVE@ state. If a container instance is in @REGISTERING@,
    -- @DEREGISTERING@, or @REGISTRATION_FAILED@ state you can describe the
    -- container instance but can\'t update the container instance state.
    status :: ContainerInstanceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContainerInstancesState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'updateContainerInstancesState_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instance to update. If you do not specify a cluster,
-- the default cluster is assumed.
--
-- 'containerInstances', 'updateContainerInstancesState_containerInstances' - A list of up to 10 container instance IDs or full ARN entries.
--
-- 'status', 'updateContainerInstancesState_status' - The container instance state to update the container instance with. The
-- only valid values for this action are @ACTIVE@ and @DRAINING@. A
-- container instance can only be updated to @DRAINING@ status once it has
-- reached an @ACTIVE@ state. If a container instance is in @REGISTERING@,
-- @DEREGISTERING@, or @REGISTRATION_FAILED@ state you can describe the
-- container instance but can\'t update the container instance state.
newUpdateContainerInstancesState ::
  -- | 'status'
  ContainerInstanceStatus ->
  UpdateContainerInstancesState
newUpdateContainerInstancesState pStatus_ =
  UpdateContainerInstancesState'
    { cluster =
        Prelude.Nothing,
      containerInstances = Prelude.mempty,
      status = pStatus_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instance to update. If you do not specify a cluster,
-- the default cluster is assumed.
updateContainerInstancesState_cluster :: Lens.Lens' UpdateContainerInstancesState (Prelude.Maybe Prelude.Text)
updateContainerInstancesState_cluster = Lens.lens (\UpdateContainerInstancesState' {cluster} -> cluster) (\s@UpdateContainerInstancesState' {} a -> s {cluster = a} :: UpdateContainerInstancesState)

-- | A list of up to 10 container instance IDs or full ARN entries.
updateContainerInstancesState_containerInstances :: Lens.Lens' UpdateContainerInstancesState [Prelude.Text]
updateContainerInstancesState_containerInstances = Lens.lens (\UpdateContainerInstancesState' {containerInstances} -> containerInstances) (\s@UpdateContainerInstancesState' {} a -> s {containerInstances = a} :: UpdateContainerInstancesState) Prelude.. Lens.coerced

-- | The container instance state to update the container instance with. The
-- only valid values for this action are @ACTIVE@ and @DRAINING@. A
-- container instance can only be updated to @DRAINING@ status once it has
-- reached an @ACTIVE@ state. If a container instance is in @REGISTERING@,
-- @DEREGISTERING@, or @REGISTRATION_FAILED@ state you can describe the
-- container instance but can\'t update the container instance state.
updateContainerInstancesState_status :: Lens.Lens' UpdateContainerInstancesState ContainerInstanceStatus
updateContainerInstancesState_status = Lens.lens (\UpdateContainerInstancesState' {status} -> status) (\s@UpdateContainerInstancesState' {} a -> s {status = a} :: UpdateContainerInstancesState)

instance
  Core.AWSRequest
    UpdateContainerInstancesState
  where
  type
    AWSResponse UpdateContainerInstancesState =
      UpdateContainerInstancesStateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContainerInstancesStateResponse'
            Prelude.<$> ( x
                            Data..?> "containerInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateContainerInstancesState
  where
  hashWithSalt _salt UpdateContainerInstancesState' {..} =
    _salt
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` containerInstances
      `Prelude.hashWithSalt` status

instance Prelude.NFData UpdateContainerInstancesState where
  rnf UpdateContainerInstancesState' {..} =
    Prelude.rnf cluster `Prelude.seq`
      Prelude.rnf containerInstances `Prelude.seq`
        Prelude.rnf status

instance Data.ToHeaders UpdateContainerInstancesState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.UpdateContainerInstancesState" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateContainerInstancesState where
  toJSON UpdateContainerInstancesState' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cluster" Data..=) Prelude.<$> cluster,
            Prelude.Just
              ("containerInstances" Data..= containerInstances),
            Prelude.Just ("status" Data..= status)
          ]
      )

instance Data.ToPath UpdateContainerInstancesState where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateContainerInstancesState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContainerInstancesStateResponse' smart constructor.
data UpdateContainerInstancesStateResponse = UpdateContainerInstancesStateResponse'
  { -- | The list of container instances.
    containerInstances :: Prelude.Maybe [ContainerInstance],
    -- | Any failures associated with the call.
    failures :: Prelude.Maybe [Failure],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContainerInstancesStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerInstances', 'updateContainerInstancesStateResponse_containerInstances' - The list of container instances.
--
-- 'failures', 'updateContainerInstancesStateResponse_failures' - Any failures associated with the call.
--
-- 'httpStatus', 'updateContainerInstancesStateResponse_httpStatus' - The response's http status code.
newUpdateContainerInstancesStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateContainerInstancesStateResponse
newUpdateContainerInstancesStateResponse pHttpStatus_ =
  UpdateContainerInstancesStateResponse'
    { containerInstances =
        Prelude.Nothing,
      failures = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of container instances.
updateContainerInstancesStateResponse_containerInstances :: Lens.Lens' UpdateContainerInstancesStateResponse (Prelude.Maybe [ContainerInstance])
updateContainerInstancesStateResponse_containerInstances = Lens.lens (\UpdateContainerInstancesStateResponse' {containerInstances} -> containerInstances) (\s@UpdateContainerInstancesStateResponse' {} a -> s {containerInstances = a} :: UpdateContainerInstancesStateResponse) Prelude.. Lens.mapping Lens.coerced

-- | Any failures associated with the call.
updateContainerInstancesStateResponse_failures :: Lens.Lens' UpdateContainerInstancesStateResponse (Prelude.Maybe [Failure])
updateContainerInstancesStateResponse_failures = Lens.lens (\UpdateContainerInstancesStateResponse' {failures} -> failures) (\s@UpdateContainerInstancesStateResponse' {} a -> s {failures = a} :: UpdateContainerInstancesStateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateContainerInstancesStateResponse_httpStatus :: Lens.Lens' UpdateContainerInstancesStateResponse Prelude.Int
updateContainerInstancesStateResponse_httpStatus = Lens.lens (\UpdateContainerInstancesStateResponse' {httpStatus} -> httpStatus) (\s@UpdateContainerInstancesStateResponse' {} a -> s {httpStatus = a} :: UpdateContainerInstancesStateResponse)

instance
  Prelude.NFData
    UpdateContainerInstancesStateResponse
  where
  rnf UpdateContainerInstancesStateResponse' {..} =
    Prelude.rnf containerInstances `Prelude.seq`
      Prelude.rnf failures `Prelude.seq`
        Prelude.rnf httpStatus
