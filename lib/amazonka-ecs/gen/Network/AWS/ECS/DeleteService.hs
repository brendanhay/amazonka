{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified service within a cluster. You can delete a service if you have no running tasks in it and the desired task count is zero. If the service is actively maintaining tasks, you cannot delete it, and you must update the service to a desired task count of zero. For more information, see 'UpdateService' .
--
-- /Important:/ If you attempt to create a new service with the same name as an existing service in either @ACTIVE@ or @DRAINING@ status, you receive an error.
module Network.AWS.ECS.DeleteService
  ( -- * Creating a request
    DeleteService (..),
    mkDeleteService,

    -- ** Request lenses
    dsCluster,
    dsForce,
    dsService,

    -- * Destructuring the response
    DeleteServiceResponse (..),
    mkDeleteServiceResponse,

    -- ** Response lenses
    dsrsService,
    dsrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteService' smart constructor.
data DeleteService = DeleteService'
  { cluster ::
      Lude.Maybe Lude.Text,
    force :: Lude.Maybe Lude.Bool,
    service :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteService' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service to delete. If you do not specify a cluster, the default cluster is assumed.
-- * 'force' - If @true@ , allows you to delete a service even if it has not been scaled down to zero tasks. It is only necessary to use this if the service is using the @REPLICA@ scheduling strategy.
-- * 'service' - The name of the service to delete.
mkDeleteService ::
  -- | 'service'
  Lude.Text ->
  DeleteService
mkDeleteService pService_ =
  DeleteService'
    { cluster = Lude.Nothing,
      force = Lude.Nothing,
      service = pService_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service to delete. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCluster :: Lens.Lens' DeleteService (Lude.Maybe Lude.Text)
dsCluster = Lens.lens (cluster :: DeleteService -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: DeleteService)
{-# DEPRECATED dsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | If @true@ , allows you to delete a service even if it has not been scaled down to zero tasks. It is only necessary to use this if the service is using the @REPLICA@ scheduling strategy.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsForce :: Lens.Lens' DeleteService (Lude.Maybe Lude.Bool)
dsForce = Lens.lens (force :: DeleteService -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: DeleteService)
{-# DEPRECATED dsForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The name of the service to delete.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsService :: Lens.Lens' DeleteService Lude.Text
dsService = Lens.lens (service :: DeleteService -> Lude.Text) (\s a -> s {service = a} :: DeleteService)
{-# DEPRECATED dsService "Use generic-lens or generic-optics with 'service' instead." #-}

instance Lude.AWSRequest DeleteService where
  type Rs DeleteService = DeleteServiceResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteServiceResponse'
            Lude.<$> (x Lude..?> "service") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteService where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DeleteService" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteService where
  toJSON DeleteService' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cluster" Lude..=) Lude.<$> cluster,
            ("force" Lude..=) Lude.<$> force,
            Lude.Just ("service" Lude..= service)
          ]
      )

instance Lude.ToPath DeleteService where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteService where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteServiceResponse' smart constructor.
data DeleteServiceResponse = DeleteServiceResponse'
  { service ::
      Lude.Maybe ContainerService,
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

-- | Creates a value of 'DeleteServiceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'service' - The full description of the deleted service.
mkDeleteServiceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteServiceResponse
mkDeleteServiceResponse pResponseStatus_ =
  DeleteServiceResponse'
    { service = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The full description of the deleted service.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsService :: Lens.Lens' DeleteServiceResponse (Lude.Maybe ContainerService)
dsrsService = Lens.lens (service :: DeleteServiceResponse -> Lude.Maybe ContainerService) (\s a -> s {service = a} :: DeleteServiceResponse)
{-# DEPRECATED dsrsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DeleteServiceResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DeleteServiceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteServiceResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
