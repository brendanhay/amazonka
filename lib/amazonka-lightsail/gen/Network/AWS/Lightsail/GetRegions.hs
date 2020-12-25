{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRegions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all valid regions for Amazon Lightsail. Use the @include availability zones@ parameter to also return the Availability Zones in a region.
module Network.AWS.Lightsail.GetRegions
  ( -- * Creating a request
    GetRegions (..),
    mkGetRegions,

    -- ** Request lenses
    grIncludeAvailabilityZones,
    grIncludeRelationalDatabaseAvailabilityZones,

    -- * Destructuring the response
    GetRegionsResponse (..),
    mkGetRegionsResponse,

    -- ** Response lenses
    grrrsRegions,
    grrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRegions' smart constructor.
data GetRegions = GetRegions'
  { -- | A Boolean value indicating whether to also include Availability Zones in your get regions request. Availability Zones are indicated with a letter: e.g., @us-east-2a@ .
    includeAvailabilityZones :: Core.Maybe Core.Bool,
    -- | A Boolean value indicating whether to also include Availability Zones for databases in your get regions request. Availability Zones are indicated with a letter (e.g., @us-east-2a@ ).
    includeRelationalDatabaseAvailabilityZones :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRegions' value with any optional fields omitted.
mkGetRegions ::
  GetRegions
mkGetRegions =
  GetRegions'
    { includeAvailabilityZones = Core.Nothing,
      includeRelationalDatabaseAvailabilityZones = Core.Nothing
    }

-- | A Boolean value indicating whether to also include Availability Zones in your get regions request. Availability Zones are indicated with a letter: e.g., @us-east-2a@ .
--
-- /Note:/ Consider using 'includeAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grIncludeAvailabilityZones :: Lens.Lens' GetRegions (Core.Maybe Core.Bool)
grIncludeAvailabilityZones = Lens.field @"includeAvailabilityZones"
{-# DEPRECATED grIncludeAvailabilityZones "Use generic-lens or generic-optics with 'includeAvailabilityZones' instead." #-}

-- | A Boolean value indicating whether to also include Availability Zones for databases in your get regions request. Availability Zones are indicated with a letter (e.g., @us-east-2a@ ).
--
-- /Note:/ Consider using 'includeRelationalDatabaseAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grIncludeRelationalDatabaseAvailabilityZones :: Lens.Lens' GetRegions (Core.Maybe Core.Bool)
grIncludeRelationalDatabaseAvailabilityZones = Lens.field @"includeRelationalDatabaseAvailabilityZones"
{-# DEPRECATED grIncludeRelationalDatabaseAvailabilityZones "Use generic-lens or generic-optics with 'includeRelationalDatabaseAvailabilityZones' instead." #-}

instance Core.FromJSON GetRegions where
  toJSON GetRegions {..} =
    Core.object
      ( Core.catMaybes
          [ ("includeAvailabilityZones" Core..=)
              Core.<$> includeAvailabilityZones,
            ("includeRelationalDatabaseAvailabilityZones" Core..=)
              Core.<$> includeRelationalDatabaseAvailabilityZones
          ]
      )

instance Core.AWSRequest GetRegions where
  type Rs GetRegions = GetRegionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetRegions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegionsResponse'
            Core.<$> (x Core..:? "regions") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRegionsResponse' smart constructor.
data GetRegionsResponse = GetRegionsResponse'
  { -- | An array of key-value pairs containing information about your get regions request.
    regions :: Core.Maybe [Types.RegionInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRegionsResponse' value with any optional fields omitted.
mkGetRegionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRegionsResponse
mkGetRegionsResponse responseStatus =
  GetRegionsResponse' {regions = Core.Nothing, responseStatus}

-- | An array of key-value pairs containing information about your get regions request.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsRegions :: Lens.Lens' GetRegionsResponse (Core.Maybe [Types.RegionInfo])
grrrsRegions = Lens.field @"regions"
{-# DEPRECATED grrrsRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetRegionsResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
