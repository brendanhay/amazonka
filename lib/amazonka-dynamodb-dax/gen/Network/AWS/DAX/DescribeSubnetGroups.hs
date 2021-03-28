{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DescribeSubnetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of subnet group descriptions. If a subnet group name is specified, the list will contain only the description of that group.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeSubnetGroups
    (
    -- * Creating a request
      DescribeSubnetGroups (..)
    , mkDescribeSubnetGroups
    -- ** Request lenses
    , dsgMaxResults
    , dsgNextToken
    , dsgSubnetGroupNames

    -- * Destructuring the response
    , DescribeSubnetGroupsResponse (..)
    , mkDescribeSubnetGroupsResponse
    -- ** Response lenses
    , dsgrfrsNextToken
    , dsgrfrsSubnetGroups
    , dsgrfrsResponseStatus
    ) where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSubnetGroups' smart constructor.
data DescribeSubnetGroups = DescribeSubnetGroups'
  { maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
  , subnetGroupNames :: Core.Maybe [Core.Text]
    -- ^ The name of the subnet group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubnetGroups' value with any optional fields omitted.
mkDescribeSubnetGroups
    :: DescribeSubnetGroups
mkDescribeSubnetGroups
  = DescribeSubnetGroups'{maxResults = Core.Nothing,
                          nextToken = Core.Nothing, subnetGroupNames = Core.Nothing}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgMaxResults :: Lens.Lens' DescribeSubnetGroups (Core.Maybe Core.Int)
dsgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dsgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgNextToken :: Lens.Lens' DescribeSubnetGroups (Core.Maybe Core.Text)
dsgNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The name of the subnet group.
--
-- /Note:/ Consider using 'subnetGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgSubnetGroupNames :: Lens.Lens' DescribeSubnetGroups (Core.Maybe [Core.Text])
dsgSubnetGroupNames = Lens.field @"subnetGroupNames"
{-# INLINEABLE dsgSubnetGroupNames #-}
{-# DEPRECATED subnetGroupNames "Use generic-lens or generic-optics with 'subnetGroupNames' instead"  #-}

instance Core.ToQuery DescribeSubnetGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSubnetGroups where
        toHeaders DescribeSubnetGroups{..}
          = Core.pure ("X-Amz-Target", "AmazonDAXV3.DescribeSubnetGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeSubnetGroups where
        toJSON DescribeSubnetGroups{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SubnetGroupNames" Core..=) Core.<$> subnetGroupNames])

instance Core.AWSRequest DescribeSubnetGroups where
        type Rs DescribeSubnetGroups = DescribeSubnetGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSubnetGroupsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "SubnetGroups"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSubnetGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"subnetGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeSubnetGroupsResponse' smart constructor.
data DescribeSubnetGroupsResponse = DescribeSubnetGroupsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ Provides an identifier to allow retrieval of paginated results.
  , subnetGroups :: Core.Maybe [Types.SubnetGroup]
    -- ^ An array of subnet groups. Each element in the array represents a single subnet group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubnetGroupsResponse' value with any optional fields omitted.
mkDescribeSubnetGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSubnetGroupsResponse
mkDescribeSubnetGroupsResponse responseStatus
  = DescribeSubnetGroupsResponse'{nextToken = Core.Nothing,
                                  subnetGroups = Core.Nothing, responseStatus}

-- | Provides an identifier to allow retrieval of paginated results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrfrsNextToken :: Lens.Lens' DescribeSubnetGroupsResponse (Core.Maybe Core.Text)
dsgrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsgrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of subnet groups. Each element in the array represents a single subnet group.
--
-- /Note:/ Consider using 'subnetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrfrsSubnetGroups :: Lens.Lens' DescribeSubnetGroupsResponse (Core.Maybe [Types.SubnetGroup])
dsgrfrsSubnetGroups = Lens.field @"subnetGroups"
{-# INLINEABLE dsgrfrsSubnetGroups #-}
{-# DEPRECATED subnetGroups "Use generic-lens or generic-optics with 'subnetGroups' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrfrsResponseStatus :: Lens.Lens' DescribeSubnetGroupsResponse Core.Int
dsgrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsgrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
