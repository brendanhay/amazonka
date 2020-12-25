{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeRegions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the Regions that are configured for multi-Region replication.
module Network.AWS.DirectoryService.DescribeRegions
  ( -- * Creating a request
    DescribeRegions (..),
    mkDescribeRegions,

    -- ** Request lenses
    drsDirectoryId,
    drsNextToken,
    drsRegionName,

    -- * Destructuring the response
    DescribeRegionsResponse (..),
    mkDescribeRegionsResponse,

    -- ** Response lenses
    drrfrsNextToken,
    drrfrsRegionsDescription,
    drrfrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRegions' smart constructor.
data DescribeRegions = DescribeRegions'
  { -- | The identifier of the directory.
    directoryId :: Types.DirectoryId,
    -- | The /DescribeRegionsResult.NextToken/ value from a previous call to 'DescribeRegions' . Pass null if this is the first call.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The name of the Region. For example, @us-east-1@ .
    regionName :: Core.Maybe Types.RegionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRegions' value with any optional fields omitted.
mkDescribeRegions ::
  -- | 'directoryId'
  Types.DirectoryId ->
  DescribeRegions
mkDescribeRegions directoryId =
  DescribeRegions'
    { directoryId,
      nextToken = Core.Nothing,
      regionName = Core.Nothing
    }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDirectoryId :: Lens.Lens' DescribeRegions Types.DirectoryId
drsDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED drsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The /DescribeRegionsResult.NextToken/ value from a previous call to 'DescribeRegions' . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeRegions (Core.Maybe Types.NextToken)
drsNextToken = Lens.field @"nextToken"
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the Region. For example, @us-east-1@ .
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRegionName :: Lens.Lens' DescribeRegions (Core.Maybe Types.RegionName)
drsRegionName = Lens.field @"regionName"
{-# DEPRECATED drsRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Core.FromJSON DescribeRegions where
  toJSON DescribeRegions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            ("NextToken" Core..=) Core.<$> nextToken,
            ("RegionName" Core..=) Core.<$> regionName
          ]
      )

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
            ("X-Amz-Target", "DirectoryService_20150416.DescribeRegions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRegionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "RegionsDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeRegionsResponse' smart constructor.
data DescribeRegionsResponse = DescribeRegionsResponse'
  { -- | If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'DescribeRegions' to retrieve the next set of items.
    nextToken :: Core.Maybe Types.NextToken,
    -- | List of regional information related to the directory per replicated Region.
    regionsDescription :: Core.Maybe [Types.RegionDescription],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeRegionsResponse' value with any optional fields omitted.
mkDescribeRegionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeRegionsResponse
mkDescribeRegionsResponse responseStatus =
  DescribeRegionsResponse'
    { nextToken = Core.Nothing,
      regionsDescription = Core.Nothing,
      responseStatus
    }

-- | If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'DescribeRegions' to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsNextToken :: Lens.Lens' DescribeRegionsResponse (Core.Maybe Types.NextToken)
drrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED drrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of regional information related to the directory per replicated Region.
--
-- /Note:/ Consider using 'regionsDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsRegionsDescription :: Lens.Lens' DescribeRegionsResponse (Core.Maybe [Types.RegionDescription])
drrfrsRegionsDescription = Lens.field @"regionsDescription"
{-# DEPRECATED drrfrsRegionsDescription "Use generic-lens or generic-optics with 'regionsDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsResponseStatus :: Lens.Lens' DescribeRegionsResponse Core.Int
drrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
