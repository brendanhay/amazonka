{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteCapacityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified capacity provider.
--
-- Prior to a capacity provider being deleted, the capacity provider must be removed from the capacity provider strategy from all services. The 'UpdateService' API can be used to remove a capacity provider from a service's capacity provider strategy. When updating a service, the @forceNewDeployment@ option can be used to ensure that any tasks using the Amazon EC2 instance capacity provided by the capacity provider are transitioned to use the capacity from the remaining capacity providers. Only capacity providers that are not associated with a cluster can be deleted. To remove a capacity provider from a cluster, you can either use 'PutClusterCapacityProviders' or delete the cluster.
module Network.AWS.ECS.DeleteCapacityProvider
  ( -- * Creating a request
    DeleteCapacityProvider (..),
    mkDeleteCapacityProvider,

    -- ** Request lenses
    dcpCapacityProvider,

    -- * Destructuring the response
    DeleteCapacityProviderResponse (..),
    mkDeleteCapacityProviderResponse,

    -- ** Response lenses
    dcpfrsCapacityProvider,
    dcpfrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCapacityProvider' smart constructor.
newtype DeleteCapacityProvider = DeleteCapacityProvider'
  { -- | The short name or full Amazon Resource Name (ARN) of the capacity provider to delete.
    capacityProvider :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCapacityProvider' with the minimum fields required to make a request.
--
-- * 'capacityProvider' - The short name or full Amazon Resource Name (ARN) of the capacity provider to delete.
mkDeleteCapacityProvider ::
  -- | 'capacityProvider'
  Lude.Text ->
  DeleteCapacityProvider
mkDeleteCapacityProvider pCapacityProvider_ =
  DeleteCapacityProvider' {capacityProvider = pCapacityProvider_}

-- | The short name or full Amazon Resource Name (ARN) of the capacity provider to delete.
--
-- /Note:/ Consider using 'capacityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpCapacityProvider :: Lens.Lens' DeleteCapacityProvider Lude.Text
dcpCapacityProvider = Lens.lens (capacityProvider :: DeleteCapacityProvider -> Lude.Text) (\s a -> s {capacityProvider = a} :: DeleteCapacityProvider)
{-# DEPRECATED dcpCapacityProvider "Use generic-lens or generic-optics with 'capacityProvider' instead." #-}

instance Lude.AWSRequest DeleteCapacityProvider where
  type Rs DeleteCapacityProvider = DeleteCapacityProviderResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteCapacityProviderResponse'
            Lude.<$> (x Lude..?> "capacityProvider")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCapacityProvider where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DeleteCapacityProvider" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCapacityProvider where
  toJSON DeleteCapacityProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("capacityProvider" Lude..= capacityProvider)]
      )

instance Lude.ToPath DeleteCapacityProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCapacityProvider where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCapacityProviderResponse' smart constructor.
data DeleteCapacityProviderResponse = DeleteCapacityProviderResponse'
  { capacityProvider :: Lude.Maybe CapacityProvider,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCapacityProviderResponse' with the minimum fields required to make a request.
--
-- * 'capacityProvider' -
-- * 'responseStatus' - The response status code.
mkDeleteCapacityProviderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCapacityProviderResponse
mkDeleteCapacityProviderResponse pResponseStatus_ =
  DeleteCapacityProviderResponse'
    { capacityProvider = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'capacityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpfrsCapacityProvider :: Lens.Lens' DeleteCapacityProviderResponse (Lude.Maybe CapacityProvider)
dcpfrsCapacityProvider = Lens.lens (capacityProvider :: DeleteCapacityProviderResponse -> Lude.Maybe CapacityProvider) (\s a -> s {capacityProvider = a} :: DeleteCapacityProviderResponse)
{-# DEPRECATED dcpfrsCapacityProvider "Use generic-lens or generic-optics with 'capacityProvider' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpfrsResponseStatus :: Lens.Lens' DeleteCapacityProviderResponse Lude.Int
dcpfrsResponseStatus = Lens.lens (responseStatus :: DeleteCapacityProviderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCapacityProviderResponse)
{-# DEPRECATED dcpfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
