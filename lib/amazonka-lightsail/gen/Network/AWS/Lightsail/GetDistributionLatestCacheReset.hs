{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDistributionLatestCacheReset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the timestamp and status of the last cache reset of a specific Amazon Lightsail content delivery network (CDN) distribution.
module Network.AWS.Lightsail.GetDistributionLatestCacheReset
  ( -- * Creating a request
    GetDistributionLatestCacheReset (..),
    mkGetDistributionLatestCacheReset,

    -- ** Request lenses
    gdlcrDistributionName,

    -- * Destructuring the response
    GetDistributionLatestCacheResetResponse (..),
    mkGetDistributionLatestCacheResetResponse,

    -- ** Response lenses
    gdlcrrrsCreateTime,
    gdlcrrrsStatus,
    gdlcrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDistributionLatestCacheReset' smart constructor.
newtype GetDistributionLatestCacheReset = GetDistributionLatestCacheReset'
  { -- | The name of the distribution for which to return the timestamp of the last cache reset.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
    -- When omitted, the response includes the latest cache reset timestamp of all your distributions.
    distributionName :: Core.Maybe Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDistributionLatestCacheReset' value with any optional fields omitted.
mkGetDistributionLatestCacheReset ::
  GetDistributionLatestCacheReset
mkGetDistributionLatestCacheReset =
  GetDistributionLatestCacheReset' {distributionName = Core.Nothing}

-- | The name of the distribution for which to return the timestamp of the last cache reset.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
-- When omitted, the response includes the latest cache reset timestamp of all your distributions.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlcrDistributionName :: Lens.Lens' GetDistributionLatestCacheReset (Core.Maybe Types.ResourceName)
gdlcrDistributionName = Lens.field @"distributionName"
{-# DEPRECATED gdlcrDistributionName "Use generic-lens or generic-optics with 'distributionName' instead." #-}

instance Core.FromJSON GetDistributionLatestCacheReset where
  toJSON GetDistributionLatestCacheReset {..} =
    Core.object
      ( Core.catMaybes
          [("distributionName" Core..=) Core.<$> distributionName]
      )

instance Core.AWSRequest GetDistributionLatestCacheReset where
  type
    Rs GetDistributionLatestCacheReset =
      GetDistributionLatestCacheResetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Lightsail_20161128.GetDistributionLatestCacheReset"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDistributionLatestCacheResetResponse'
            Core.<$> (x Core..:? "createTime")
            Core.<*> (x Core..:? "status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDistributionLatestCacheResetResponse' smart constructor.
data GetDistributionLatestCacheResetResponse = GetDistributionLatestCacheResetResponse'
  { -- | The timestamp of the last cache reset (e.g., @1479734909.17@ ) in Unix time format.
    createTime :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the last cache reset.
    status :: Core.Maybe Types.Status,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDistributionLatestCacheResetResponse' value with any optional fields omitted.
mkGetDistributionLatestCacheResetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDistributionLatestCacheResetResponse
mkGetDistributionLatestCacheResetResponse responseStatus =
  GetDistributionLatestCacheResetResponse'
    { createTime =
        Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | The timestamp of the last cache reset (e.g., @1479734909.17@ ) in Unix time format.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlcrrrsCreateTime :: Lens.Lens' GetDistributionLatestCacheResetResponse (Core.Maybe Core.NominalDiffTime)
gdlcrrrsCreateTime = Lens.field @"createTime"
{-# DEPRECATED gdlcrrrsCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The status of the last cache reset.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlcrrrsStatus :: Lens.Lens' GetDistributionLatestCacheResetResponse (Core.Maybe Types.Status)
gdlcrrrsStatus = Lens.field @"status"
{-# DEPRECATED gdlcrrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlcrrrsResponseStatus :: Lens.Lens' GetDistributionLatestCacheResetResponse Core.Int
gdlcrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdlcrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
