{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes your Amazon Lightsail content delivery network (CDN) distribution.
module Network.AWS.Lightsail.DeleteDistribution
  ( -- * Creating a request
    DeleteDistribution (..),
    mkDeleteDistribution,

    -- ** Request lenses
    ddDistributionName,

    -- * Destructuring the response
    DeleteDistributionResponse (..),
    mkDeleteDistributionResponse,

    -- ** Response lenses
    ddrfrsOperation,
    ddrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDistribution' smart constructor.
newtype DeleteDistribution = DeleteDistribution'
  { -- | The name of the distribution to delete.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
    distributionName :: Core.Maybe Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDistribution' value with any optional fields omitted.
mkDeleteDistribution ::
  DeleteDistribution
mkDeleteDistribution =
  DeleteDistribution' {distributionName = Core.Nothing}

-- | The name of the distribution to delete.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDistributionName :: Lens.Lens' DeleteDistribution (Core.Maybe Types.ResourceName)
ddDistributionName = Lens.field @"distributionName"
{-# DEPRECATED ddDistributionName "Use generic-lens or generic-optics with 'distributionName' instead." #-}

instance Core.FromJSON DeleteDistribution where
  toJSON DeleteDistribution {..} =
    Core.object
      ( Core.catMaybes
          [("distributionName" Core..=) Core.<$> distributionName]
      )

instance Core.AWSRequest DeleteDistribution where
  type Rs DeleteDistribution = DeleteDistributionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.DeleteDistribution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDistributionResponse'
            Core.<$> (x Core..:? "operation") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDistributionResponse' smart constructor.
data DeleteDistributionResponse = DeleteDistributionResponse'
  { -- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Core.Maybe Types.Operation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteDistributionResponse' value with any optional fields omitted.
mkDeleteDistributionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDistributionResponse
mkDeleteDistributionResponse responseStatus =
  DeleteDistributionResponse'
    { operation = Core.Nothing,
      responseStatus
    }

-- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsOperation :: Lens.Lens' DeleteDistributionResponse (Core.Maybe Types.Operation)
ddrfrsOperation = Lens.field @"operation"
{-# DEPRECATED ddrfrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsResponseStatus :: Lens.Lens' DeleteDistributionResponse Core.Int
ddrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
