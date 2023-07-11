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
-- Module      : Amazonka.ECS.DeregisterContainerInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon ECS container instance from the specified cluster.
-- This instance is no longer available to run tasks.
--
-- If you intend to use the container instance for some other purpose after
-- deregistration, we recommend that you stop all of the tasks running on
-- the container instance before deregistration. That prevents any orphaned
-- tasks from consuming resources.
--
-- Deregistering a container instance removes the instance from a cluster,
-- but it doesn\'t terminate the EC2 instance. If you are finished using
-- the instance, be sure to terminate it in the Amazon EC2 console to stop
-- billing.
--
-- If you terminate a running container instance, Amazon ECS automatically
-- deregisters the instance from your cluster (stopped container instances
-- or instances with disconnected agents aren\'t automatically deregistered
-- when terminated).
module Amazonka.ECS.DeregisterContainerInstance
  ( -- * Creating a Request
    DeregisterContainerInstance (..),
    newDeregisterContainerInstance,

    -- * Request Lenses
    deregisterContainerInstance_cluster,
    deregisterContainerInstance_force,
    deregisterContainerInstance_containerInstance,

    -- * Destructuring the Response
    DeregisterContainerInstanceResponse (..),
    newDeregisterContainerInstanceResponse,

    -- * Response Lenses
    deregisterContainerInstanceResponse_containerInstance,
    deregisterContainerInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterContainerInstance' smart constructor.
data DeregisterContainerInstance = DeregisterContainerInstance'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the container instance to deregister. If you do not specify a
    -- cluster, the default cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Forces the container instance to be deregistered. If you have tasks
    -- running on the container instance when you deregister it with the
    -- @force@ option, these tasks remain running until you terminate the
    -- instance or the tasks stop through some other means, but they\'re
    -- orphaned (no longer monitored or accounted for by Amazon ECS). If an
    -- orphaned task on your container instance is part of an Amazon ECS
    -- service, then the service scheduler starts another copy of that task, on
    -- a different container instance if possible.
    --
    -- Any containers in orphaned service tasks that are registered with a
    -- Classic Load Balancer or an Application Load Balancer target group are
    -- deregistered. They begin connection draining according to the settings
    -- on the load balancer or target group.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The container instance ID or full ARN of the container instance to
    -- deregister. For more information about the ARN format, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids Amazon Resource Name (ARN)>
    -- in the /Amazon ECS Developer Guide/.
    containerInstance :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterContainerInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'deregisterContainerInstance_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instance to deregister. If you do not specify a
-- cluster, the default cluster is assumed.
--
-- 'force', 'deregisterContainerInstance_force' - Forces the container instance to be deregistered. If you have tasks
-- running on the container instance when you deregister it with the
-- @force@ option, these tasks remain running until you terminate the
-- instance or the tasks stop through some other means, but they\'re
-- orphaned (no longer monitored or accounted for by Amazon ECS). If an
-- orphaned task on your container instance is part of an Amazon ECS
-- service, then the service scheduler starts another copy of that task, on
-- a different container instance if possible.
--
-- Any containers in orphaned service tasks that are registered with a
-- Classic Load Balancer or an Application Load Balancer target group are
-- deregistered. They begin connection draining according to the settings
-- on the load balancer or target group.
--
-- 'containerInstance', 'deregisterContainerInstance_containerInstance' - The container instance ID or full ARN of the container instance to
-- deregister. For more information about the ARN format, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids Amazon Resource Name (ARN)>
-- in the /Amazon ECS Developer Guide/.
newDeregisterContainerInstance ::
  -- | 'containerInstance'
  Prelude.Text ->
  DeregisterContainerInstance
newDeregisterContainerInstance pContainerInstance_ =
  DeregisterContainerInstance'
    { cluster =
        Prelude.Nothing,
      force = Prelude.Nothing,
      containerInstance = pContainerInstance_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instance to deregister. If you do not specify a
-- cluster, the default cluster is assumed.
deregisterContainerInstance_cluster :: Lens.Lens' DeregisterContainerInstance (Prelude.Maybe Prelude.Text)
deregisterContainerInstance_cluster = Lens.lens (\DeregisterContainerInstance' {cluster} -> cluster) (\s@DeregisterContainerInstance' {} a -> s {cluster = a} :: DeregisterContainerInstance)

-- | Forces the container instance to be deregistered. If you have tasks
-- running on the container instance when you deregister it with the
-- @force@ option, these tasks remain running until you terminate the
-- instance or the tasks stop through some other means, but they\'re
-- orphaned (no longer monitored or accounted for by Amazon ECS). If an
-- orphaned task on your container instance is part of an Amazon ECS
-- service, then the service scheduler starts another copy of that task, on
-- a different container instance if possible.
--
-- Any containers in orphaned service tasks that are registered with a
-- Classic Load Balancer or an Application Load Balancer target group are
-- deregistered. They begin connection draining according to the settings
-- on the load balancer or target group.
deregisterContainerInstance_force :: Lens.Lens' DeregisterContainerInstance (Prelude.Maybe Prelude.Bool)
deregisterContainerInstance_force = Lens.lens (\DeregisterContainerInstance' {force} -> force) (\s@DeregisterContainerInstance' {} a -> s {force = a} :: DeregisterContainerInstance)

-- | The container instance ID or full ARN of the container instance to
-- deregister. For more information about the ARN format, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids Amazon Resource Name (ARN)>
-- in the /Amazon ECS Developer Guide/.
deregisterContainerInstance_containerInstance :: Lens.Lens' DeregisterContainerInstance Prelude.Text
deregisterContainerInstance_containerInstance = Lens.lens (\DeregisterContainerInstance' {containerInstance} -> containerInstance) (\s@DeregisterContainerInstance' {} a -> s {containerInstance = a} :: DeregisterContainerInstance)

instance Core.AWSRequest DeregisterContainerInstance where
  type
    AWSResponse DeregisterContainerInstance =
      DeregisterContainerInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterContainerInstanceResponse'
            Prelude.<$> (x Data..?> "containerInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterContainerInstance where
  hashWithSalt _salt DeregisterContainerInstance' {..} =
    _salt
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` containerInstance

instance Prelude.NFData DeregisterContainerInstance where
  rnf DeregisterContainerInstance' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf force
      `Prelude.seq` Prelude.rnf containerInstance

instance Data.ToHeaders DeregisterContainerInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.DeregisterContainerInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterContainerInstance where
  toJSON DeregisterContainerInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cluster" Data..=) Prelude.<$> cluster,
            ("force" Data..=) Prelude.<$> force,
            Prelude.Just
              ("containerInstance" Data..= containerInstance)
          ]
      )

instance Data.ToPath DeregisterContainerInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterContainerInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterContainerInstanceResponse' smart constructor.
data DeregisterContainerInstanceResponse = DeregisterContainerInstanceResponse'
  { -- | The container instance that was deregistered.
    containerInstance :: Prelude.Maybe ContainerInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterContainerInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerInstance', 'deregisterContainerInstanceResponse_containerInstance' - The container instance that was deregistered.
--
-- 'httpStatus', 'deregisterContainerInstanceResponse_httpStatus' - The response's http status code.
newDeregisterContainerInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterContainerInstanceResponse
newDeregisterContainerInstanceResponse pHttpStatus_ =
  DeregisterContainerInstanceResponse'
    { containerInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The container instance that was deregistered.
deregisterContainerInstanceResponse_containerInstance :: Lens.Lens' DeregisterContainerInstanceResponse (Prelude.Maybe ContainerInstance)
deregisterContainerInstanceResponse_containerInstance = Lens.lens (\DeregisterContainerInstanceResponse' {containerInstance} -> containerInstance) (\s@DeregisterContainerInstanceResponse' {} a -> s {containerInstance = a} :: DeregisterContainerInstanceResponse)

-- | The response's http status code.
deregisterContainerInstanceResponse_httpStatus :: Lens.Lens' DeregisterContainerInstanceResponse Prelude.Int
deregisterContainerInstanceResponse_httpStatus = Lens.lens (\DeregisterContainerInstanceResponse' {httpStatus} -> httpStatus) (\s@DeregisterContainerInstanceResponse' {} a -> s {httpStatus = a} :: DeregisterContainerInstanceResponse)

instance
  Prelude.NFData
    DeregisterContainerInstanceResponse
  where
  rnf DeregisterContainerInstanceResponse' {..} =
    Prelude.rnf containerInstance
      `Prelude.seq` Prelude.rnf httpStatus
