{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDistributionBundles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list bundles that can be applied to you Amazon Lightsail content delivery network (CDN) distributions.
--
-- A distribution bundle specifies the monthly network transfer quota and monthly cost of your dsitribution.
module Network.AWS.Lightsail.GetDistributionBundles
  ( -- * Creating a request
    GetDistributionBundles (..),
    mkGetDistributionBundles,

    -- * Destructuring the response
    GetDistributionBundlesResponse (..),
    mkGetDistributionBundlesResponse,

    -- ** Response lenses
    gdbrrsBundles,
    gdbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDistributionBundles' smart constructor.
data GetDistributionBundles = GetDistributionBundles'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDistributionBundles' value with any optional fields omitted.
mkGetDistributionBundles ::
  GetDistributionBundles
mkGetDistributionBundles = GetDistributionBundles'

instance Core.FromJSON GetDistributionBundles where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetDistributionBundles where
  type Rs GetDistributionBundles = GetDistributionBundlesResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.GetDistributionBundles")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDistributionBundlesResponse'
            Core.<$> (x Core..:? "bundles") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDistributionBundlesResponse' smart constructor.
data GetDistributionBundlesResponse = GetDistributionBundlesResponse'
  { -- | An object that describes a distribution bundle.
    bundles :: Core.Maybe [Types.DistributionBundle],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDistributionBundlesResponse' value with any optional fields omitted.
mkGetDistributionBundlesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDistributionBundlesResponse
mkGetDistributionBundlesResponse responseStatus =
  GetDistributionBundlesResponse'
    { bundles = Core.Nothing,
      responseStatus
    }

-- | An object that describes a distribution bundle.
--
-- /Note:/ Consider using 'bundles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdbrrsBundles :: Lens.Lens' GetDistributionBundlesResponse (Core.Maybe [Types.DistributionBundle])
gdbrrsBundles = Lens.field @"bundles"
{-# DEPRECATED gdbrrsBundles "Use generic-lens or generic-optics with 'bundles' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdbrrsResponseStatus :: Lens.Lens' GetDistributionBundlesResponse Core.Int
gdbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
