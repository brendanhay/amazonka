{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a task set. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.UpdateTaskSet
  ( -- * Creating a request
    UpdateTaskSet (..),
    mkUpdateTaskSet,

    -- ** Request lenses
    utsCluster,
    utsService,
    utsTaskSet,
    utsScale,

    -- * Destructuring the response
    UpdateTaskSetResponse (..),
    mkUpdateTaskSetResponse,

    -- ** Response lenses
    utsrsTaskSet,
    utsrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateTaskSet' smart constructor.
data UpdateTaskSet = UpdateTaskSet'
  { cluster :: Lude.Text,
    service :: Lude.Text,
    taskSet :: Lude.Text,
    scale :: Scale
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTaskSet' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
-- * 'scale' - Undocumented field.
-- * 'service' - The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
-- * 'taskSet' - The short name or full Amazon Resource Name (ARN) of the task set to update.
mkUpdateTaskSet ::
  -- | 'cluster'
  Lude.Text ->
  -- | 'service'
  Lude.Text ->
  -- | 'taskSet'
  Lude.Text ->
  -- | 'scale'
  Scale ->
  UpdateTaskSet
mkUpdateTaskSet pCluster_ pService_ pTaskSet_ pScale_ =
  UpdateTaskSet'
    { cluster = pCluster_,
      service = pService_,
      taskSet = pTaskSet_,
      scale = pScale_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsCluster :: Lens.Lens' UpdateTaskSet Lude.Text
utsCluster = Lens.lens (cluster :: UpdateTaskSet -> Lude.Text) (\s a -> s {cluster = a} :: UpdateTaskSet)
{-# DEPRECATED utsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsService :: Lens.Lens' UpdateTaskSet Lude.Text
utsService = Lens.lens (service :: UpdateTaskSet -> Lude.Text) (\s a -> s {service = a} :: UpdateTaskSet)
{-# DEPRECATED utsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the task set to update.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsTaskSet :: Lens.Lens' UpdateTaskSet Lude.Text
utsTaskSet = Lens.lens (taskSet :: UpdateTaskSet -> Lude.Text) (\s a -> s {taskSet = a} :: UpdateTaskSet)
{-# DEPRECATED utsTaskSet "Use generic-lens or generic-optics with 'taskSet' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsScale :: Lens.Lens' UpdateTaskSet Scale
utsScale = Lens.lens (scale :: UpdateTaskSet -> Scale) (\s a -> s {scale = a} :: UpdateTaskSet)
{-# DEPRECATED utsScale "Use generic-lens or generic-optics with 'scale' instead." #-}

instance Lude.AWSRequest UpdateTaskSet where
  type Rs UpdateTaskSet = UpdateTaskSetResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateTaskSetResponse'
            Lude.<$> (x Lude..?> "taskSet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTaskSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.UpdateTaskSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTaskSet where
  toJSON UpdateTaskSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("cluster" Lude..= cluster),
            Lude.Just ("service" Lude..= service),
            Lude.Just ("taskSet" Lude..= taskSet),
            Lude.Just ("scale" Lude..= scale)
          ]
      )

instance Lude.ToPath UpdateTaskSet where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTaskSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTaskSetResponse' smart constructor.
data UpdateTaskSetResponse = UpdateTaskSetResponse'
  { taskSet ::
      Lude.Maybe TaskSet,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTaskSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'taskSet' - Undocumented field.
mkUpdateTaskSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTaskSetResponse
mkUpdateTaskSetResponse pResponseStatus_ =
  UpdateTaskSetResponse'
    { taskSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsrsTaskSet :: Lens.Lens' UpdateTaskSetResponse (Lude.Maybe TaskSet)
utsrsTaskSet = Lens.lens (taskSet :: UpdateTaskSetResponse -> Lude.Maybe TaskSet) (\s a -> s {taskSet = a} :: UpdateTaskSetResponse)
{-# DEPRECATED utsrsTaskSet "Use generic-lens or generic-optics with 'taskSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsrsResponseStatus :: Lens.Lens' UpdateTaskSetResponse Lude.Int
utsrsResponseStatus = Lens.lens (responseStatus :: UpdateTaskSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTaskSetResponse)
{-# DEPRECATED utsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
