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
    ucpName,
    ucpAutoScalingGroupProvider,

    -- * Destructuring the response
    UpdateCapacityProviderResponse (..),
    mkUpdateCapacityProviderResponse,

    -- ** Response lenses
    ucprrsCapacityProvider,
    ucprrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCapacityProvider' smart constructor.
data UpdateCapacityProvider = UpdateCapacityProvider'
  { -- | An object representing the parameters to update for the Auto Scaling group capacity provider.
    name :: Types.Name,
    -- | The name of the capacity provider to update.
    autoScalingGroupProvider :: Types.AutoScalingGroupProviderUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCapacityProvider' value with any optional fields omitted.
mkUpdateCapacityProvider ::
  -- | 'name'
  Types.Name ->
  -- | 'autoScalingGroupProvider'
  Types.AutoScalingGroupProviderUpdate ->
  UpdateCapacityProvider
mkUpdateCapacityProvider name autoScalingGroupProvider =
  UpdateCapacityProvider' {name, autoScalingGroupProvider}

-- | An object representing the parameters to update for the Auto Scaling group capacity provider.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpName :: Lens.Lens' UpdateCapacityProvider Types.Name
ucpName = Lens.field @"name"
{-# DEPRECATED ucpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the capacity provider to update.
--
-- /Note:/ Consider using 'autoScalingGroupProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpAutoScalingGroupProvider :: Lens.Lens' UpdateCapacityProvider Types.AutoScalingGroupProviderUpdate
ucpAutoScalingGroupProvider = Lens.field @"autoScalingGroupProvider"
{-# DEPRECATED ucpAutoScalingGroupProvider "Use generic-lens or generic-optics with 'autoScalingGroupProvider' instead." #-}

instance Core.FromJSON UpdateCapacityProvider where
  toJSON UpdateCapacityProvider {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just
              ("autoScalingGroupProvider" Core..= autoScalingGroupProvider)
          ]
      )

instance Core.AWSRequest UpdateCapacityProvider where
  type Rs UpdateCapacityProvider = UpdateCapacityProviderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.UpdateCapacityProvider"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCapacityProviderResponse'
            Core.<$> (x Core..:? "capacityProvider")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateCapacityProviderResponse' smart constructor.
data UpdateCapacityProviderResponse = UpdateCapacityProviderResponse'
  { capacityProvider :: Core.Maybe Types.CapacityProvider,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCapacityProviderResponse' value with any optional fields omitted.
mkUpdateCapacityProviderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateCapacityProviderResponse
mkUpdateCapacityProviderResponse responseStatus =
  UpdateCapacityProviderResponse'
    { capacityProvider = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'capacityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprrsCapacityProvider :: Lens.Lens' UpdateCapacityProviderResponse (Core.Maybe Types.CapacityProvider)
ucprrsCapacityProvider = Lens.field @"capacityProvider"
{-# DEPRECATED ucprrsCapacityProvider "Use generic-lens or generic-optics with 'capacityProvider' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprrsResponseStatus :: Lens.Lens' UpdateCapacityProviderResponse Core.Int
ucprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
