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
-- Module      : Network.AWS.ECS.DeregisterContainerInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon ECS container instance from the specified cluster.
-- This instance is no longer available to run tasks.
--
-- If you intend to use the container instance for some other purpose after
-- deregistration, you should stop all of the tasks running on the
-- container instance before deregistration. That prevents any orphaned
-- tasks from consuming resources.
--
-- Deregistering a container instance removes the instance from a cluster,
-- but it does not terminate the EC2 instance. If you are finished using
-- the instance, be sure to terminate it in the Amazon EC2 console to stop
-- billing.
--
-- If you terminate a running container instance, Amazon ECS automatically
-- deregisters the instance from your cluster (stopped container instances
-- or instances with disconnected agents are not automatically deregistered
-- when terminated).
module Network.AWS.ECS.DeregisterContainerInstance
  ( -- * Creating a Request
    DeregisterContainerInstance (..),
    newDeregisterContainerInstance,

    -- * Request Lenses
    deregisterContainerInstance_force,
    deregisterContainerInstance_cluster,
    deregisterContainerInstance_containerInstance,

    -- * Destructuring the Response
    DeregisterContainerInstanceResponse (..),
    newDeregisterContainerInstanceResponse,

    -- * Response Lenses
    deregisterContainerInstanceResponse_containerInstance,
    deregisterContainerInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterContainerInstance' smart constructor.
data DeregisterContainerInstance = DeregisterContainerInstance'
  { -- | Forces the deregistration of the container instance. If you have tasks
    -- running on the container instance when you deregister it with the
    -- @force@ option, these tasks remain running until you terminate the
    -- instance or the tasks stop through some other means, but they are
    -- orphaned (no longer monitored or accounted for by Amazon ECS). If an
    -- orphaned task on your container instance is part of an Amazon ECS
    -- service, then the service scheduler starts another copy of that task, on
    -- a different container instance if possible.
    --
    -- Any containers in orphaned service tasks that are registered with a
    -- Classic Load Balancer or an Application Load Balancer target group are
    -- deregistered. They begin connection draining according to the settings
    -- on the load balancer or target group.
    force :: Core.Maybe Core.Bool,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the container instance to deregister. If you do not specify a
    -- cluster, the default cluster is assumed.
    cluster :: Core.Maybe Core.Text,
    -- | The container instance ID or full ARN of the container instance to
    -- deregister. The ARN contains the @arn:aws:ecs@ namespace, followed by
    -- the Region of the container instance, the AWS account ID of the
    -- container instance owner, the @container-instance@ namespace, and then
    -- the container instance ID. For example,
    -- @arn:aws:ecs:region:aws_account_id:container-instance\/container_instance_ID@.
    containerInstance :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterContainerInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'deregisterContainerInstance_force' - Forces the deregistration of the container instance. If you have tasks
-- running on the container instance when you deregister it with the
-- @force@ option, these tasks remain running until you terminate the
-- instance or the tasks stop through some other means, but they are
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
-- 'cluster', 'deregisterContainerInstance_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instance to deregister. If you do not specify a
-- cluster, the default cluster is assumed.
--
-- 'containerInstance', 'deregisterContainerInstance_containerInstance' - The container instance ID or full ARN of the container instance to
-- deregister. The ARN contains the @arn:aws:ecs@ namespace, followed by
-- the Region of the container instance, the AWS account ID of the
-- container instance owner, the @container-instance@ namespace, and then
-- the container instance ID. For example,
-- @arn:aws:ecs:region:aws_account_id:container-instance\/container_instance_ID@.
newDeregisterContainerInstance ::
  -- | 'containerInstance'
  Core.Text ->
  DeregisterContainerInstance
newDeregisterContainerInstance pContainerInstance_ =
  DeregisterContainerInstance'
    { force = Core.Nothing,
      cluster = Core.Nothing,
      containerInstance = pContainerInstance_
    }

-- | Forces the deregistration of the container instance. If you have tasks
-- running on the container instance when you deregister it with the
-- @force@ option, these tasks remain running until you terminate the
-- instance or the tasks stop through some other means, but they are
-- orphaned (no longer monitored or accounted for by Amazon ECS). If an
-- orphaned task on your container instance is part of an Amazon ECS
-- service, then the service scheduler starts another copy of that task, on
-- a different container instance if possible.
--
-- Any containers in orphaned service tasks that are registered with a
-- Classic Load Balancer or an Application Load Balancer target group are
-- deregistered. They begin connection draining according to the settings
-- on the load balancer or target group.
deregisterContainerInstance_force :: Lens.Lens' DeregisterContainerInstance (Core.Maybe Core.Bool)
deregisterContainerInstance_force = Lens.lens (\DeregisterContainerInstance' {force} -> force) (\s@DeregisterContainerInstance' {} a -> s {force = a} :: DeregisterContainerInstance)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instance to deregister. If you do not specify a
-- cluster, the default cluster is assumed.
deregisterContainerInstance_cluster :: Lens.Lens' DeregisterContainerInstance (Core.Maybe Core.Text)
deregisterContainerInstance_cluster = Lens.lens (\DeregisterContainerInstance' {cluster} -> cluster) (\s@DeregisterContainerInstance' {} a -> s {cluster = a} :: DeregisterContainerInstance)

-- | The container instance ID or full ARN of the container instance to
-- deregister. The ARN contains the @arn:aws:ecs@ namespace, followed by
-- the Region of the container instance, the AWS account ID of the
-- container instance owner, the @container-instance@ namespace, and then
-- the container instance ID. For example,
-- @arn:aws:ecs:region:aws_account_id:container-instance\/container_instance_ID@.
deregisterContainerInstance_containerInstance :: Lens.Lens' DeregisterContainerInstance Core.Text
deregisterContainerInstance_containerInstance = Lens.lens (\DeregisterContainerInstance' {containerInstance} -> containerInstance) (\s@DeregisterContainerInstance' {} a -> s {containerInstance = a} :: DeregisterContainerInstance)

instance Core.AWSRequest DeregisterContainerInstance where
  type
    AWSResponse DeregisterContainerInstance =
      DeregisterContainerInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterContainerInstanceResponse'
            Core.<$> (x Core..?> "containerInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeregisterContainerInstance

instance Core.NFData DeregisterContainerInstance

instance Core.ToHeaders DeregisterContainerInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DeregisterContainerInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterContainerInstance where
  toJSON DeregisterContainerInstance' {..} =
    Core.object
      ( Core.catMaybes
          [ ("force" Core..=) Core.<$> force,
            ("cluster" Core..=) Core.<$> cluster,
            Core.Just
              ("containerInstance" Core..= containerInstance)
          ]
      )

instance Core.ToPath DeregisterContainerInstance where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterContainerInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterContainerInstanceResponse' smart constructor.
data DeregisterContainerInstanceResponse = DeregisterContainerInstanceResponse'
  { -- | The container instance that was deregistered.
    containerInstance :: Core.Maybe ContainerInstance,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeregisterContainerInstanceResponse
newDeregisterContainerInstanceResponse pHttpStatus_ =
  DeregisterContainerInstanceResponse'
    { containerInstance =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The container instance that was deregistered.
deregisterContainerInstanceResponse_containerInstance :: Lens.Lens' DeregisterContainerInstanceResponse (Core.Maybe ContainerInstance)
deregisterContainerInstanceResponse_containerInstance = Lens.lens (\DeregisterContainerInstanceResponse' {containerInstance} -> containerInstance) (\s@DeregisterContainerInstanceResponse' {} a -> s {containerInstance = a} :: DeregisterContainerInstanceResponse)

-- | The response's http status code.
deregisterContainerInstanceResponse_httpStatus :: Lens.Lens' DeregisterContainerInstanceResponse Core.Int
deregisterContainerInstanceResponse_httpStatus = Lens.lens (\DeregisterContainerInstanceResponse' {httpStatus} -> httpStatus) (\s@DeregisterContainerInstanceResponse' {} a -> s {httpStatus = a} :: DeregisterContainerInstanceResponse)

instance
  Core.NFData
    DeregisterContainerInstanceResponse
