{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UpdateDistributionBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bundle of your Amazon Lightsail content delivery network (CDN) distribution.
--
-- A distribution bundle specifies the monthly network transfer quota and monthly cost of your dsitribution.
-- Update your distribution's bundle if your distribution is going over its monthly network transfer quota and is incurring an overage fee.
-- You can update your distribution's bundle only one time within your monthly AWS billing cycle. To determine if you can update your distribution's bundle, use the @GetDistributions@ action. The @ableToUpdateBundle@ parameter in the result will indicate whether you can currently update your distribution's bundle.
module Network.AWS.Lightsail.UpdateDistributionBundle
  ( -- * Creating a request
    UpdateDistributionBundle (..),
    mkUpdateDistributionBundle,

    -- ** Request lenses
    udbBundleId,
    udbDistributionName,

    -- * Destructuring the response
    UpdateDistributionBundleResponse (..),
    mkUpdateDistributionBundleResponse,

    -- ** Response lenses
    udbrrsOperation,
    udbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDistributionBundle' smart constructor.
data UpdateDistributionBundle = UpdateDistributionBundle'
  { -- | The bundle ID of the new bundle to apply to your distribution.
    --
    -- Use the @GetDistributionBundles@ action to get a list of distribution bundle IDs that you can specify.
    bundleId :: Core.Maybe Types.String,
    -- | The name of the distribution for which to update the bundle.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
    distributionName :: Core.Maybe Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDistributionBundle' value with any optional fields omitted.
mkUpdateDistributionBundle ::
  UpdateDistributionBundle
mkUpdateDistributionBundle =
  UpdateDistributionBundle'
    { bundleId = Core.Nothing,
      distributionName = Core.Nothing
    }

-- | The bundle ID of the new bundle to apply to your distribution.
--
-- Use the @GetDistributionBundles@ action to get a list of distribution bundle IDs that you can specify.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udbBundleId :: Lens.Lens' UpdateDistributionBundle (Core.Maybe Types.String)
udbBundleId = Lens.field @"bundleId"
{-# DEPRECATED udbBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The name of the distribution for which to update the bundle.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udbDistributionName :: Lens.Lens' UpdateDistributionBundle (Core.Maybe Types.ResourceName)
udbDistributionName = Lens.field @"distributionName"
{-# DEPRECATED udbDistributionName "Use generic-lens or generic-optics with 'distributionName' instead." #-}

instance Core.FromJSON UpdateDistributionBundle where
  toJSON UpdateDistributionBundle {..} =
    Core.object
      ( Core.catMaybes
          [ ("bundleId" Core..=) Core.<$> bundleId,
            ("distributionName" Core..=) Core.<$> distributionName
          ]
      )

instance Core.AWSRequest UpdateDistributionBundle where
  type Rs UpdateDistributionBundle = UpdateDistributionBundleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.UpdateDistributionBundle")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDistributionBundleResponse'
            Core.<$> (x Core..:? "operation") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateDistributionBundleResponse' smart constructor.
data UpdateDistributionBundleResponse = UpdateDistributionBundleResponse'
  { operation :: Core.Maybe Types.Operation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateDistributionBundleResponse' value with any optional fields omitted.
mkUpdateDistributionBundleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDistributionBundleResponse
mkUpdateDistributionBundleResponse responseStatus =
  UpdateDistributionBundleResponse'
    { operation = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udbrrsOperation :: Lens.Lens' UpdateDistributionBundleResponse (Core.Maybe Types.Operation)
udbrrsOperation = Lens.field @"operation"
{-# DEPRECATED udbrrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udbrrsResponseStatus :: Lens.Lens' UpdateDistributionBundleResponse Core.Int
udbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
