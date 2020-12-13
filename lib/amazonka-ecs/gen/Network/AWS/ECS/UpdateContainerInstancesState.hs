{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateContainerInstancesState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the status of an Amazon ECS container instance.
--
-- Once a container instance has reached an @ACTIVE@ state, you can change the status of a container instance to @DRAINING@ to manually remove an instance from a cluster, for example to perform system updates, update the Docker daemon, or scale down the cluster size.
-- /Important:/ A container instance cannot be changed to @DRAINING@ until it has reached an @ACTIVE@ status. If the instance is in any other status, an error will be received.
-- When you set a container instance to @DRAINING@ , Amazon ECS prevents new tasks from being scheduled for placement on the container instance and replacement service tasks are started on other container instances in the cluster if the resources are available. Service tasks on the container instance that are in the @PENDING@ state are stopped immediately.
-- Service tasks on the container instance that are in the @RUNNING@ state are stopped and replaced according to the service's deployment configuration parameters, @minimumHealthyPercent@ and @maximumPercent@ . You can change the deployment configuration of your service using 'UpdateService' .
--
--     * If @minimumHealthyPercent@ is below 100%, the scheduler can ignore @desiredCount@ temporarily during task replacement. For example, @desiredCount@ is four tasks, a minimum of 50% allows the scheduler to stop two existing tasks before starting two new tasks. If the minimum is 100%, the service scheduler can't remove existing tasks until the replacement tasks are considered healthy. Tasks for services that do not use a load balancer are considered healthy if they are in the @RUNNING@ state. Tasks for services that use a load balancer are considered healthy if they are in the @RUNNING@ state and the container instance they are hosted on is reported as healthy by the load balancer.
--
--
--     * The @maximumPercent@ parameter represents an upper limit on the number of running tasks during task replacement, which enables you to define the replacement batch size. For example, if @desiredCount@ is four tasks, a maximum of 200% starts four new tasks before stopping the four tasks to be drained, provided that the cluster resources required to do this are available. If the maximum is 100%, then replacement tasks can't start until the draining tasks have stopped.
--
--
-- Any @PENDING@ or @RUNNING@ tasks that do not belong to a service are not affected. You must wait for them to finish or stop them manually.
-- A container instance has completed draining when it has no more @RUNNING@ tasks. You can verify this using 'ListTasks' .
-- When a container instance has been drained, you can set a container instance to @ACTIVE@ status and once it has reached that status the Amazon ECS scheduler can begin scheduling tasks on the instance again.
module Network.AWS.ECS.UpdateContainerInstancesState
  ( -- * Creating a request
    UpdateContainerInstancesState (..),
    mkUpdateContainerInstancesState,

    -- ** Request lenses
    ucisStatus,
    ucisCluster,
    ucisContainerInstances,

    -- * Destructuring the response
    UpdateContainerInstancesStateResponse (..),
    mkUpdateContainerInstancesStateResponse,

    -- ** Response lenses
    ucisrsFailures,
    ucisrsContainerInstances,
    ucisrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateContainerInstancesState' smart constructor.
data UpdateContainerInstancesState = UpdateContainerInstancesState'
  { -- | The container instance state with which to update the container instance. The only valid values for this action are @ACTIVE@ and @DRAINING@ . A container instance can only be updated to @DRAINING@ status once it has reached an @ACTIVE@ state. If a container instance is in @REGISTERING@ , @DEREGISTERING@ , or @REGISTRATION_FAILED@ state you can describe the container instance but will be unable to update the container instance state.
    status :: ContainerInstanceStatus,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instance to update. If you do not specify a cluster, the default cluster is assumed.
    cluster :: Lude.Maybe Lude.Text,
    -- | A list of container instance IDs or full ARN entries.
    containerInstances :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContainerInstancesState' with the minimum fields required to make a request.
--
-- * 'status' - The container instance state with which to update the container instance. The only valid values for this action are @ACTIVE@ and @DRAINING@ . A container instance can only be updated to @DRAINING@ status once it has reached an @ACTIVE@ state. If a container instance is in @REGISTERING@ , @DEREGISTERING@ , or @REGISTRATION_FAILED@ state you can describe the container instance but will be unable to update the container instance state.
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instance to update. If you do not specify a cluster, the default cluster is assumed.
-- * 'containerInstances' - A list of container instance IDs or full ARN entries.
mkUpdateContainerInstancesState ::
  -- | 'status'
  ContainerInstanceStatus ->
  UpdateContainerInstancesState
mkUpdateContainerInstancesState pStatus_ =
  UpdateContainerInstancesState'
    { status = pStatus_,
      cluster = Lude.Nothing,
      containerInstances = Lude.mempty
    }

-- | The container instance state with which to update the container instance. The only valid values for this action are @ACTIVE@ and @DRAINING@ . A container instance can only be updated to @DRAINING@ status once it has reached an @ACTIVE@ state. If a container instance is in @REGISTERING@ , @DEREGISTERING@ , or @REGISTRATION_FAILED@ state you can describe the container instance but will be unable to update the container instance state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucisStatus :: Lens.Lens' UpdateContainerInstancesState ContainerInstanceStatus
ucisStatus = Lens.lens (status :: UpdateContainerInstancesState -> ContainerInstanceStatus) (\s a -> s {status = a} :: UpdateContainerInstancesState)
{-# DEPRECATED ucisStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instance to update. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucisCluster :: Lens.Lens' UpdateContainerInstancesState (Lude.Maybe Lude.Text)
ucisCluster = Lens.lens (cluster :: UpdateContainerInstancesState -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: UpdateContainerInstancesState)
{-# DEPRECATED ucisCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | A list of container instance IDs or full ARN entries.
--
-- /Note:/ Consider using 'containerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucisContainerInstances :: Lens.Lens' UpdateContainerInstancesState [Lude.Text]
ucisContainerInstances = Lens.lens (containerInstances :: UpdateContainerInstancesState -> [Lude.Text]) (\s a -> s {containerInstances = a} :: UpdateContainerInstancesState)
{-# DEPRECATED ucisContainerInstances "Use generic-lens or generic-optics with 'containerInstances' instead." #-}

instance Lude.AWSRequest UpdateContainerInstancesState where
  type
    Rs UpdateContainerInstancesState =
      UpdateContainerInstancesStateResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateContainerInstancesStateResponse'
            Lude.<$> (x Lude..?> "failures" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "containerInstances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateContainerInstancesState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.UpdateContainerInstancesState" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateContainerInstancesState where
  toJSON UpdateContainerInstancesState' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("status" Lude..= status),
            ("cluster" Lude..=) Lude.<$> cluster,
            Lude.Just ("containerInstances" Lude..= containerInstances)
          ]
      )

instance Lude.ToPath UpdateContainerInstancesState where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateContainerInstancesState where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateContainerInstancesStateResponse' smart constructor.
data UpdateContainerInstancesStateResponse = UpdateContainerInstancesStateResponse'
  { -- | Any failures associated with the call.
    failures :: Lude.Maybe [Failure],
    -- | The list of container instances.
    containerInstances :: Lude.Maybe [ContainerInstance],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContainerInstancesStateResponse' with the minimum fields required to make a request.
--
-- * 'failures' - Any failures associated with the call.
-- * 'containerInstances' - The list of container instances.
-- * 'responseStatus' - The response status code.
mkUpdateContainerInstancesStateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateContainerInstancesStateResponse
mkUpdateContainerInstancesStateResponse pResponseStatus_ =
  UpdateContainerInstancesStateResponse'
    { failures = Lude.Nothing,
      containerInstances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucisrsFailures :: Lens.Lens' UpdateContainerInstancesStateResponse (Lude.Maybe [Failure])
ucisrsFailures = Lens.lens (failures :: UpdateContainerInstancesStateResponse -> Lude.Maybe [Failure]) (\s a -> s {failures = a} :: UpdateContainerInstancesStateResponse)
{-# DEPRECATED ucisrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The list of container instances.
--
-- /Note:/ Consider using 'containerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucisrsContainerInstances :: Lens.Lens' UpdateContainerInstancesStateResponse (Lude.Maybe [ContainerInstance])
ucisrsContainerInstances = Lens.lens (containerInstances :: UpdateContainerInstancesStateResponse -> Lude.Maybe [ContainerInstance]) (\s a -> s {containerInstances = a} :: UpdateContainerInstancesStateResponse)
{-# DEPRECATED ucisrsContainerInstances "Use generic-lens or generic-optics with 'containerInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucisrsResponseStatus :: Lens.Lens' UpdateContainerInstancesStateResponse Lude.Int
ucisrsResponseStatus = Lens.lens (responseStatus :: UpdateContainerInstancesStateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateContainerInstancesStateResponse)
{-# DEPRECATED ucisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
