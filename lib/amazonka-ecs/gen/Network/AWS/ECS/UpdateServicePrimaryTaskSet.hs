{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateServicePrimaryTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies which task set in a service is the primary task set. Any parameters that are updated on the primary task set in a service will transition to the service. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.UpdateServicePrimaryTaskSet
  ( -- * Creating a request
    UpdateServicePrimaryTaskSet (..),
    mkUpdateServicePrimaryTaskSet,

    -- ** Request lenses
    usptsCluster,
    usptsService,
    usptsPrimaryTaskSet,

    -- * Destructuring the response
    UpdateServicePrimaryTaskSetResponse (..),
    mkUpdateServicePrimaryTaskSetResponse,

    -- ** Response lenses
    usptsrsTaskSet,
    usptsrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateServicePrimaryTaskSet' smart constructor.
data UpdateServicePrimaryTaskSet = UpdateServicePrimaryTaskSet'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
    cluster :: Lude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
    service :: Lude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the task set to set as the primary task set in the deployment.
    primaryTaskSet :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServicePrimaryTaskSet' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
-- * 'service' - The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
-- * 'primaryTaskSet' - The short name or full Amazon Resource Name (ARN) of the task set to set as the primary task set in the deployment.
mkUpdateServicePrimaryTaskSet ::
  -- | 'cluster'
  Lude.Text ->
  -- | 'service'
  Lude.Text ->
  -- | 'primaryTaskSet'
  Lude.Text ->
  UpdateServicePrimaryTaskSet
mkUpdateServicePrimaryTaskSet pCluster_ pService_ pPrimaryTaskSet_ =
  UpdateServicePrimaryTaskSet'
    { cluster = pCluster_,
      service = pService_,
      primaryTaskSet = pPrimaryTaskSet_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsCluster :: Lens.Lens' UpdateServicePrimaryTaskSet Lude.Text
usptsCluster = Lens.lens (cluster :: UpdateServicePrimaryTaskSet -> Lude.Text) (\s a -> s {cluster = a} :: UpdateServicePrimaryTaskSet)
{-# DEPRECATED usptsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsService :: Lens.Lens' UpdateServicePrimaryTaskSet Lude.Text
usptsService = Lens.lens (service :: UpdateServicePrimaryTaskSet -> Lude.Text) (\s a -> s {service = a} :: UpdateServicePrimaryTaskSet)
{-# DEPRECATED usptsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the task set to set as the primary task set in the deployment.
--
-- /Note:/ Consider using 'primaryTaskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsPrimaryTaskSet :: Lens.Lens' UpdateServicePrimaryTaskSet Lude.Text
usptsPrimaryTaskSet = Lens.lens (primaryTaskSet :: UpdateServicePrimaryTaskSet -> Lude.Text) (\s a -> s {primaryTaskSet = a} :: UpdateServicePrimaryTaskSet)
{-# DEPRECATED usptsPrimaryTaskSet "Use generic-lens or generic-optics with 'primaryTaskSet' instead." #-}

instance Lude.AWSRequest UpdateServicePrimaryTaskSet where
  type
    Rs UpdateServicePrimaryTaskSet =
      UpdateServicePrimaryTaskSetResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateServicePrimaryTaskSetResponse'
            Lude.<$> (x Lude..?> "taskSet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateServicePrimaryTaskSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.UpdateServicePrimaryTaskSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateServicePrimaryTaskSet where
  toJSON UpdateServicePrimaryTaskSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("cluster" Lude..= cluster),
            Lude.Just ("service" Lude..= service),
            Lude.Just ("primaryTaskSet" Lude..= primaryTaskSet)
          ]
      )

instance Lude.ToPath UpdateServicePrimaryTaskSet where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateServicePrimaryTaskSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateServicePrimaryTaskSetResponse' smart constructor.
data UpdateServicePrimaryTaskSetResponse = UpdateServicePrimaryTaskSetResponse'
  { taskSet :: Lude.Maybe TaskSet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServicePrimaryTaskSetResponse' with the minimum fields required to make a request.
--
-- * 'taskSet' -
-- * 'responseStatus' - The response status code.
mkUpdateServicePrimaryTaskSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateServicePrimaryTaskSetResponse
mkUpdateServicePrimaryTaskSetResponse pResponseStatus_ =
  UpdateServicePrimaryTaskSetResponse'
    { taskSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'taskSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsrsTaskSet :: Lens.Lens' UpdateServicePrimaryTaskSetResponse (Lude.Maybe TaskSet)
usptsrsTaskSet = Lens.lens (taskSet :: UpdateServicePrimaryTaskSetResponse -> Lude.Maybe TaskSet) (\s a -> s {taskSet = a} :: UpdateServicePrimaryTaskSetResponse)
{-# DEPRECATED usptsrsTaskSet "Use generic-lens or generic-optics with 'taskSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usptsrsResponseStatus :: Lens.Lens' UpdateServicePrimaryTaskSetResponse Lude.Int
usptsrsResponseStatus = Lens.lens (responseStatus :: UpdateServicePrimaryTaskSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateServicePrimaryTaskSetResponse)
{-# DEPRECATED usptsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
