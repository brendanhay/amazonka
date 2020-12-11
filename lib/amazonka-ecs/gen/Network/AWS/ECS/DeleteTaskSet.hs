{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified task set within a service. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.DeleteTaskSet
  ( -- * Creating a request
    DeleteTaskSet (..),
    mkDeleteTaskSet,

    -- ** Request lenses
    dtsForce,
    dtsCluster,
    dtsService,
    dtsTaskSet,

    -- * Destructuring the response
    DeleteTaskSetResponse (..),
    mkDeleteTaskSetResponse,

    -- ** Response lenses
    dtsrsTaskSet,
    dtsrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTaskSet' smart constructor.
data DeleteTaskSet = DeleteTaskSet'
  { force :: Lude.Maybe Lude.Bool,
    cluster :: Lude.Text,
    service :: Lude.Text,
    taskSet :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTaskSet' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in to delete.
-- * 'force' - If @true@ , this allows you to delete a task set even if it hasn't been scaled down to zero.
-- * 'service' - The short name or full Amazon Resource Name (ARN) of the service that hosts the task set to delete.
-- * 'taskSet' - The task set ID or full Amazon Resource Name (ARN) of the task set to delete.
mkDeleteTaskSet ::
  -- | 'cluster'
  Lude.Text ->
  -- | 'service'
  Lude.Text ->
  -- | 'taskSet'
  Lude.Text ->
  DeleteTaskSet
mkDeleteTaskSet pCluster_ pService_ pTaskSet_ =
  DeleteTaskSet'
    { force = Lude.Nothing,
      cluster = pCluster_,
      service = pService_,
      taskSet = pTaskSet_
    }

-- | If @true@ , this allows you to delete a task set even if it hasn't been scaled down to zero.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsForce :: Lens.Lens' DeleteTaskSet (Lude.Maybe Lude.Bool)
dtsForce = Lens.lens (force :: DeleteTaskSet -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: DeleteTaskSet)
{-# DEPRECATED dtsForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in to delete.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsCluster :: Lens.Lens' DeleteTaskSet Lude.Text
dtsCluster = Lens.lens (cluster :: DeleteTaskSet -> Lude.Text) (\s a -> s {cluster = a} :: DeleteTaskSet)
{-# DEPRECATED dtsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the service that hosts the task set to delete.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsService :: Lens.Lens' DeleteTaskSet Lude.Text
dtsService = Lens.lens (service :: DeleteTaskSet -> Lude.Text) (\s a -> s {service = a} :: DeleteTaskSet)
{-# DEPRECATED dtsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The task set ID or full Amazon Resource Name (ARN) of the task set to delete.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsTaskSet :: Lens.Lens' DeleteTaskSet Lude.Text
dtsTaskSet = Lens.lens (taskSet :: DeleteTaskSet -> Lude.Text) (\s a -> s {taskSet = a} :: DeleteTaskSet)
{-# DEPRECATED dtsTaskSet "Use generic-lens or generic-optics with 'taskSet' instead." #-}

instance Lude.AWSRequest DeleteTaskSet where
  type Rs DeleteTaskSet = DeleteTaskSetResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteTaskSetResponse'
            Lude.<$> (x Lude..?> "taskSet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTaskSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DeleteTaskSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTaskSet where
  toJSON DeleteTaskSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("force" Lude..=) Lude.<$> force,
            Lude.Just ("cluster" Lude..= cluster),
            Lude.Just ("service" Lude..= service),
            Lude.Just ("taskSet" Lude..= taskSet)
          ]
      )

instance Lude.ToPath DeleteTaskSet where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTaskSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTaskSetResponse' smart constructor.
data DeleteTaskSetResponse = DeleteTaskSetResponse'
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

-- | Creates a value of 'DeleteTaskSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'taskSet' - Undocumented field.
mkDeleteTaskSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTaskSetResponse
mkDeleteTaskSetResponse pResponseStatus_ =
  DeleteTaskSetResponse'
    { taskSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrsTaskSet :: Lens.Lens' DeleteTaskSetResponse (Lude.Maybe TaskSet)
dtsrsTaskSet = Lens.lens (taskSet :: DeleteTaskSetResponse -> Lude.Maybe TaskSet) (\s a -> s {taskSet = a} :: DeleteTaskSetResponse)
{-# DEPRECATED dtsrsTaskSet "Use generic-lens or generic-optics with 'taskSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrsResponseStatus :: Lens.Lens' DeleteTaskSetResponse Lude.Int
dtsrsResponseStatus = Lens.lens (responseStatus :: DeleteTaskSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTaskSetResponse)
{-# DEPRECATED dtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
