{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeregisterContainerInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon ECS container instance from the specified cluster. This instance is no longer available to run tasks.
--
-- If you intend to use the container instance for some other purpose after deregistration, you should stop all of the tasks running on the container instance before deregistration. That prevents any orphaned tasks from consuming resources.
-- Deregistering a container instance removes the instance from a cluster, but it does not terminate the EC2 instance. If you are finished using the instance, be sure to terminate it in the Amazon EC2 console to stop billing.
module Network.AWS.ECS.DeregisterContainerInstance
  ( -- * Creating a request
    DeregisterContainerInstance (..),
    mkDeregisterContainerInstance,

    -- ** Request lenses
    derCluster,
    derForce,
    derContainerInstance,

    -- * Destructuring the response
    DeregisterContainerInstanceResponse (..),
    mkDeregisterContainerInstanceResponse,

    -- ** Response lenses
    dcirsContainerInstance,
    dcirsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterContainerInstance' smart constructor.
data DeregisterContainerInstance = DeregisterContainerInstance'
  { cluster ::
      Lude.Maybe Lude.Text,
    force :: Lude.Maybe Lude.Bool,
    containerInstance :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterContainerInstance' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instance to deregister. If you do not specify a cluster, the default cluster is assumed.
-- * 'containerInstance' - The container instance ID or full ARN of the container instance to deregister. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:region:aws_account_id:container-instance/container_instance_ID@ .
-- * 'force' - Forces the deregistration of the container instance. If you have tasks running on the container instance when you deregister it with the @force@ option, these tasks remain running until you terminate the instance or the tasks stop through some other means, but they are orphaned (no longer monitored or accounted for by Amazon ECS). If an orphaned task on your container instance is part of an Amazon ECS service, then the service scheduler starts another copy of that task, on a different container instance if possible.
--
-- Any containers in orphaned service tasks that are registered with a Classic Load Balancer or an Application Load Balancer target group are deregistered. They begin connection draining according to the settings on the load balancer or target group.
mkDeregisterContainerInstance ::
  -- | 'containerInstance'
  Lude.Text ->
  DeregisterContainerInstance
mkDeregisterContainerInstance pContainerInstance_ =
  DeregisterContainerInstance'
    { cluster = Lude.Nothing,
      force = Lude.Nothing,
      containerInstance = pContainerInstance_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instance to deregister. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derCluster :: Lens.Lens' DeregisterContainerInstance (Lude.Maybe Lude.Text)
derCluster = Lens.lens (cluster :: DeregisterContainerInstance -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: DeregisterContainerInstance)
{-# DEPRECATED derCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | Forces the deregistration of the container instance. If you have tasks running on the container instance when you deregister it with the @force@ option, these tasks remain running until you terminate the instance or the tasks stop through some other means, but they are orphaned (no longer monitored or accounted for by Amazon ECS). If an orphaned task on your container instance is part of an Amazon ECS service, then the service scheduler starts another copy of that task, on a different container instance if possible.
--
-- Any containers in orphaned service tasks that are registered with a Classic Load Balancer or an Application Load Balancer target group are deregistered. They begin connection draining according to the settings on the load balancer or target group.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derForce :: Lens.Lens' DeregisterContainerInstance (Lude.Maybe Lude.Bool)
derForce = Lens.lens (force :: DeregisterContainerInstance -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: DeregisterContainerInstance)
{-# DEPRECATED derForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The container instance ID or full ARN of the container instance to deregister. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:region:aws_account_id:container-instance/container_instance_ID@ .
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derContainerInstance :: Lens.Lens' DeregisterContainerInstance Lude.Text
derContainerInstance = Lens.lens (containerInstance :: DeregisterContainerInstance -> Lude.Text) (\s a -> s {containerInstance = a} :: DeregisterContainerInstance)
{-# DEPRECATED derContainerInstance "Use generic-lens or generic-optics with 'containerInstance' instead." #-}

instance Lude.AWSRequest DeregisterContainerInstance where
  type
    Rs DeregisterContainerInstance =
      DeregisterContainerInstanceResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeregisterContainerInstanceResponse'
            Lude.<$> (x Lude..?> "containerInstance")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterContainerInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DeregisterContainerInstance" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterContainerInstance where
  toJSON DeregisterContainerInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cluster" Lude..=) Lude.<$> cluster,
            ("force" Lude..=) Lude.<$> force,
            Lude.Just ("containerInstance" Lude..= containerInstance)
          ]
      )

instance Lude.ToPath DeregisterContainerInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterContainerInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterContainerInstanceResponse' smart constructor.
data DeregisterContainerInstanceResponse = DeregisterContainerInstanceResponse'
  { containerInstance ::
      Lude.Maybe
        ContainerInstance,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterContainerInstanceResponse' with the minimum fields required to make a request.
--
-- * 'containerInstance' - The container instance that was deregistered.
-- * 'responseStatus' - The response status code.
mkDeregisterContainerInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterContainerInstanceResponse
mkDeregisterContainerInstanceResponse pResponseStatus_ =
  DeregisterContainerInstanceResponse'
    { containerInstance =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The container instance that was deregistered.
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirsContainerInstance :: Lens.Lens' DeregisterContainerInstanceResponse (Lude.Maybe ContainerInstance)
dcirsContainerInstance = Lens.lens (containerInstance :: DeregisterContainerInstanceResponse -> Lude.Maybe ContainerInstance) (\s a -> s {containerInstance = a} :: DeregisterContainerInstanceResponse)
{-# DEPRECATED dcirsContainerInstance "Use generic-lens or generic-optics with 'containerInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcirsResponseStatus :: Lens.Lens' DeregisterContainerInstanceResponse Lude.Int
dcirsResponseStatus = Lens.lens (responseStatus :: DeregisterContainerInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterContainerInstanceResponse)
{-# DEPRECATED dcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
