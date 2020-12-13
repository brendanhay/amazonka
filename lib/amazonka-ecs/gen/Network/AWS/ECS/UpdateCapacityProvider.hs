{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateCapacityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters for a capacity provider.
module Network.AWS.ECS.UpdateCapacityProvider
  ( -- * Creating a request
    UpdateCapacityProvider (..),
    mkUpdateCapacityProvider,

    -- ** Request lenses
    ucpAutoScalingGroupProvider,
    ucpName,

    -- * Destructuring the response
    UpdateCapacityProviderResponse (..),
    mkUpdateCapacityProviderResponse,

    -- ** Response lenses
    ucprsCapacityProvider,
    ucprsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateCapacityProvider' smart constructor.
data UpdateCapacityProvider = UpdateCapacityProvider'
  { -- | The name of the capacity provider to update.
    autoScalingGroupProvider :: AutoScalingGroupProviderUpdate,
    -- | An object representing the parameters to update for the Auto Scaling group capacity provider.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCapacityProvider' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupProvider' - The name of the capacity provider to update.
-- * 'name' - An object representing the parameters to update for the Auto Scaling group capacity provider.
mkUpdateCapacityProvider ::
  -- | 'autoScalingGroupProvider'
  AutoScalingGroupProviderUpdate ->
  -- | 'name'
  Lude.Text ->
  UpdateCapacityProvider
mkUpdateCapacityProvider pAutoScalingGroupProvider_ pName_ =
  UpdateCapacityProvider'
    { autoScalingGroupProvider =
        pAutoScalingGroupProvider_,
      name = pName_
    }

-- | The name of the capacity provider to update.
--
-- /Note:/ Consider using 'autoScalingGroupProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpAutoScalingGroupProvider :: Lens.Lens' UpdateCapacityProvider AutoScalingGroupProviderUpdate
ucpAutoScalingGroupProvider = Lens.lens (autoScalingGroupProvider :: UpdateCapacityProvider -> AutoScalingGroupProviderUpdate) (\s a -> s {autoScalingGroupProvider = a} :: UpdateCapacityProvider)
{-# DEPRECATED ucpAutoScalingGroupProvider "Use generic-lens or generic-optics with 'autoScalingGroupProvider' instead." #-}

-- | An object representing the parameters to update for the Auto Scaling group capacity provider.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpName :: Lens.Lens' UpdateCapacityProvider Lude.Text
ucpName = Lens.lens (name :: UpdateCapacityProvider -> Lude.Text) (\s a -> s {name = a} :: UpdateCapacityProvider)
{-# DEPRECATED ucpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateCapacityProvider where
  type Rs UpdateCapacityProvider = UpdateCapacityProviderResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateCapacityProviderResponse'
            Lude.<$> (x Lude..?> "capacityProvider")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateCapacityProvider where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.UpdateCapacityProvider" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateCapacityProvider where
  toJSON UpdateCapacityProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("autoScalingGroupProvider" Lude..= autoScalingGroupProvider),
            Lude.Just ("name" Lude..= name)
          ]
      )

instance Lude.ToPath UpdateCapacityProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateCapacityProvider where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCapacityProviderResponse' smart constructor.
data UpdateCapacityProviderResponse = UpdateCapacityProviderResponse'
  { capacityProvider :: Lude.Maybe CapacityProvider,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCapacityProviderResponse' with the minimum fields required to make a request.
--
-- * 'capacityProvider' -
-- * 'responseStatus' - The response status code.
mkUpdateCapacityProviderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateCapacityProviderResponse
mkUpdateCapacityProviderResponse pResponseStatus_ =
  UpdateCapacityProviderResponse'
    { capacityProvider = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'capacityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprsCapacityProvider :: Lens.Lens' UpdateCapacityProviderResponse (Lude.Maybe CapacityProvider)
ucprsCapacityProvider = Lens.lens (capacityProvider :: UpdateCapacityProviderResponse -> Lude.Maybe CapacityProvider) (\s a -> s {capacityProvider = a} :: UpdateCapacityProviderResponse)
{-# DEPRECATED ucprsCapacityProvider "Use generic-lens or generic-optics with 'capacityProvider' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprsResponseStatus :: Lens.Lens' UpdateCapacityProviderResponse Lude.Int
ucprsResponseStatus = Lens.lens (responseStatus :: UpdateCapacityProviderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateCapacityProviderResponse)
{-# DEPRECATED ucprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
