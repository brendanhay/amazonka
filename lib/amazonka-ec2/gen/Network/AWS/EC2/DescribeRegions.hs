{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeRegions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Regions that are enabled for your account, or all Regions.
--
-- For a list of the Regions supported by Amazon EC2, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#ec2_region Regions and Endpoints> .
-- For information about enabling and disabling Regions for your account, see <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html Managing AWS Regions> in the /AWS General Reference/ .
module Network.AWS.EC2.DescribeRegions
  ( -- * Creating a request
    DescribeRegions (..),
    mkDescribeRegions,

    -- ** Request lenses
    drsAllRegions,
    drsDryRun,
    drsFilters,
    drsRegionNames,

    -- * Destructuring the response
    DescribeRegionsResponse (..),
    mkDescribeRegionsResponse,

    -- ** Response lenses
    drrrsRegions,
    drrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRegions' smart constructor.
data DescribeRegions = DescribeRegions'
  { -- | Indicates whether to display all Regions, including Regions that are disabled for your account.
    allRegions :: Core.Maybe Core.Bool,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The filters.
    --
    --
    --     * @endpoint@ - The endpoint of the Region (for example, @ec2.us-east-1.amazonaws.com@ ).
    --
    --
    --     * @opt-in-status@ - The opt-in status of the Region (@opt-in-not-required@ | @opted-in@ | @not-opted-in@ ).
    --
    --
    --     * @region-name@ - The name of the Region (for example, @us-east-1@ ).
    filters :: Core.Maybe [Types.Filter],
    -- | The names of the Regions. You can specify any Regions, whether they are enabled and disabled for your account.
    regionNames :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRegions' value with any optional fields omitted.
mkDescribeRegions ::
  DescribeRegions
mkDescribeRegions =
  DescribeRegions'
    { allRegions = Core.Nothing,
      dryRun = Core.Nothing,
      filters = Core.Nothing,
      regionNames = Core.Nothing
    }

-- | Indicates whether to display all Regions, including Regions that are disabled for your account.
--
-- /Note:/ Consider using 'allRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsAllRegions :: Lens.Lens' DescribeRegions (Core.Maybe Core.Bool)
drsAllRegions = Lens.field @"allRegions"
{-# DEPRECATED drsAllRegions "Use generic-lens or generic-optics with 'allRegions' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDryRun :: Lens.Lens' DescribeRegions (Core.Maybe Core.Bool)
drsDryRun = Lens.field @"dryRun"
{-# DEPRECATED drsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The filters.
--
--
--     * @endpoint@ - The endpoint of the Region (for example, @ec2.us-east-1.amazonaws.com@ ).
--
--
--     * @opt-in-status@ - The opt-in status of the Region (@opt-in-not-required@ | @opted-in@ | @not-opted-in@ ).
--
--
--     * @region-name@ - The name of the Region (for example, @us-east-1@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsFilters :: Lens.Lens' DescribeRegions (Core.Maybe [Types.Filter])
drsFilters = Lens.field @"filters"
{-# DEPRECATED drsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The names of the Regions. You can specify any Regions, whether they are enabled and disabled for your account.
--
-- /Note:/ Consider using 'regionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRegionNames :: Lens.Lens' DescribeRegions (Core.Maybe [Types.String])
drsRegionNames = Lens.field @"regionNames"
{-# DEPRECATED drsRegionNames "Use generic-lens or generic-optics with 'regionNames' instead." #-}

instance Core.AWSRequest DescribeRegions where
  type Rs DescribeRegions = DescribeRegionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeRegions")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "AllRegions" Core.<$> allRegions)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryList "RegionName" Core.<$> regionNames)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeRegionsResponse'
            Core.<$> (x Core..@? "regionInfo" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeRegionsResponse' smart constructor.
data DescribeRegionsResponse = DescribeRegionsResponse'
  { -- | Information about the Regions.
    regions :: Core.Maybe [Types.RegionInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRegionsResponse' value with any optional fields omitted.
mkDescribeRegionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeRegionsResponse
mkDescribeRegionsResponse responseStatus =
  DescribeRegionsResponse' {regions = Core.Nothing, responseStatus}

-- | Information about the Regions.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsRegions :: Lens.Lens' DescribeRegionsResponse (Core.Maybe [Types.RegionInfo])
drrrsRegions = Lens.field @"regions"
{-# DEPRECATED drrrsRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DescribeRegionsResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
