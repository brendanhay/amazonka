{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeRegions (..)
    , mkDescribeRegions
    -- ** Request lenses
    , drsDirectoryId
    , drsNextToken
    , drsRegionName

    -- * Destructuring the response
    , DescribeRegionsResponse (..)
    , mkDescribeRegionsResponse
    -- ** Response lenses
    , drrfrsNextToken
    , drrfrsRegionsDescription
    , drrfrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRegions' smart constructor.
data DescribeRegions = DescribeRegions'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The /DescribeRegionsResult.NextToken/ value from a previous call to 'DescribeRegions' . Pass null if this is the first call.
  , regionName :: Core.Maybe Types.RegionName
    -- ^ The name of the Region. For example, @us-east-1@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRegions' value with any optional fields omitted.
mkDescribeRegions
    :: Types.DirectoryId -- ^ 'directoryId'
    -> DescribeRegions
mkDescribeRegions directoryId
  = DescribeRegions'{directoryId, nextToken = Core.Nothing,
                     regionName = Core.Nothing}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDirectoryId :: Lens.Lens' DescribeRegions Types.DirectoryId
drsDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE drsDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The /DescribeRegionsResult.NextToken/ value from a previous call to 'DescribeRegions' . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeRegions (Core.Maybe Types.NextToken)
drsNextToken = Lens.field @"nextToken"
{-# INLINEABLE drsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The name of the Region. For example, @us-east-1@ .
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRegionName :: Lens.Lens' DescribeRegions (Core.Maybe Types.RegionName)
drsRegionName = Lens.field @"regionName"
{-# INLINEABLE drsRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

instance Core.ToQuery DescribeRegions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeRegions where
        toHeaders DescribeRegions{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.DescribeRegions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeRegions where
        toJSON DescribeRegions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("RegionName" Core..=) Core.<$> regionName])

instance Core.AWSRequest DescribeRegions where
        type Rs DescribeRegions = DescribeRegionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeRegionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "RegionsDescription"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeRegionsResponse' smart constructor.
data DescribeRegionsResponse = DescribeRegionsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'DescribeRegions' to retrieve the next set of items.
  , regionsDescription :: Core.Maybe [Types.RegionDescription]
    -- ^ List of regional information related to the directory per replicated Region.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeRegionsResponse' value with any optional fields omitted.
mkDescribeRegionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeRegionsResponse
mkDescribeRegionsResponse responseStatus
  = DescribeRegionsResponse'{nextToken = Core.Nothing,
                             regionsDescription = Core.Nothing, responseStatus}

-- | If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'DescribeRegions' to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsNextToken :: Lens.Lens' DescribeRegionsResponse (Core.Maybe Types.NextToken)
drrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE drrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | List of regional information related to the directory per replicated Region.
--
-- /Note:/ Consider using 'regionsDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsRegionsDescription :: Lens.Lens' DescribeRegionsResponse (Core.Maybe [Types.RegionDescription])
drrfrsRegionsDescription = Lens.field @"regionsDescription"
{-# INLINEABLE drrfrsRegionsDescription #-}
{-# DEPRECATED regionsDescription "Use generic-lens or generic-optics with 'regionsDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsResponseStatus :: Lens.Lens' DescribeRegionsResponse Core.Int
drrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
