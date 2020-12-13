{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeregisterTaskDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified task definition by family and revision. Upon deregistration, the task definition is marked as @INACTIVE@ . Existing tasks and services that reference an @INACTIVE@ task definition continue to run without disruption. Existing services that reference an @INACTIVE@ task definition can still scale up or down by modifying the service's desired count.
--
-- You cannot use an @INACTIVE@ task definition to run new tasks or create new services, and you cannot update an existing service to reference an @INACTIVE@ task definition. However, there may be up to a 10-minute window following deregistration where these restrictions have not yet taken effect.
module Network.AWS.ECS.DeregisterTaskDefinition
  ( -- * Creating a request
    DeregisterTaskDefinition (..),
    mkDeregisterTaskDefinition,

    -- ** Request lenses
    dtdfTaskDefinition,

    -- * Destructuring the response
    DeregisterTaskDefinitionResponse (..),
    mkDeregisterTaskDefinitionResponse,

    -- ** Response lenses
    dtdrsTaskDefinition,
    dtdrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterTaskDefinition' smart constructor.
newtype DeregisterTaskDefinition = DeregisterTaskDefinition'
  { -- | The @family@ and @revision@ (@family:revision@ ) or full Amazon Resource Name (ARN) of the task definition to deregister. You must specify a @revision@ .
    taskDefinition :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterTaskDefinition' with the minimum fields required to make a request.
--
-- * 'taskDefinition' - The @family@ and @revision@ (@family:revision@ ) or full Amazon Resource Name (ARN) of the task definition to deregister. You must specify a @revision@ .
mkDeregisterTaskDefinition ::
  -- | 'taskDefinition'
  Lude.Text ->
  DeregisterTaskDefinition
mkDeregisterTaskDefinition pTaskDefinition_ =
  DeregisterTaskDefinition' {taskDefinition = pTaskDefinition_}

-- | The @family@ and @revision@ (@family:revision@ ) or full Amazon Resource Name (ARN) of the task definition to deregister. You must specify a @revision@ .
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdfTaskDefinition :: Lens.Lens' DeregisterTaskDefinition Lude.Text
dtdfTaskDefinition = Lens.lens (taskDefinition :: DeregisterTaskDefinition -> Lude.Text) (\s a -> s {taskDefinition = a} :: DeregisterTaskDefinition)
{-# DEPRECATED dtdfTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

instance Lude.AWSRequest DeregisterTaskDefinition where
  type Rs DeregisterTaskDefinition = DeregisterTaskDefinitionResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeregisterTaskDefinitionResponse'
            Lude.<$> (x Lude..?> "taskDefinition")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterTaskDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DeregisterTaskDefinition" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterTaskDefinition where
  toJSON DeregisterTaskDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("taskDefinition" Lude..= taskDefinition)]
      )

instance Lude.ToPath DeregisterTaskDefinition where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterTaskDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterTaskDefinitionResponse' smart constructor.
data DeregisterTaskDefinitionResponse = DeregisterTaskDefinitionResponse'
  { -- | The full description of the deregistered task.
    taskDefinition :: Lude.Maybe TaskDefinition,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterTaskDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'taskDefinition' - The full description of the deregistered task.
-- * 'responseStatus' - The response status code.
mkDeregisterTaskDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterTaskDefinitionResponse
mkDeregisterTaskDefinitionResponse pResponseStatus_ =
  DeregisterTaskDefinitionResponse'
    { taskDefinition = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The full description of the deregistered task.
--
-- /Note:/ Consider using 'taskDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdrsTaskDefinition :: Lens.Lens' DeregisterTaskDefinitionResponse (Lude.Maybe TaskDefinition)
dtdrsTaskDefinition = Lens.lens (taskDefinition :: DeregisterTaskDefinitionResponse -> Lude.Maybe TaskDefinition) (\s a -> s {taskDefinition = a} :: DeregisterTaskDefinitionResponse)
{-# DEPRECATED dtdrsTaskDefinition "Use generic-lens or generic-optics with 'taskDefinition' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtdrsResponseStatus :: Lens.Lens' DeregisterTaskDefinitionResponse Lude.Int
dtdrsResponseStatus = Lens.lens (responseStatus :: DeregisterTaskDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterTaskDefinitionResponse)
{-# DEPRECATED dtdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
