{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBSecurityGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBSecurityGroup@ descriptions. If a @DBSecurityGroupName@ is specified, the list will contain only the descriptions of the specified DB security group. 
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBSecurityGroups
    (
    -- * Creating a request
      DescribeDBSecurityGroups (..)
    , mkDescribeDBSecurityGroups
    -- ** Request lenses
    , ddbsgsDBSecurityGroupName
    , ddbsgsFilters
    , ddbsgsMarker
    , ddbsgsMaxRecords

    -- * Destructuring the response
    , DescribeDBSecurityGroupsResponse (..)
    , mkDescribeDBSecurityGroupsResponse
    -- ** Response lenses
    , ddbsgrfrsDBSecurityGroups
    , ddbsgrfrsMarker
    , ddbsgrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeDBSecurityGroups' smart constructor.
data DescribeDBSecurityGroups = DescribeDBSecurityGroups'
  { dBSecurityGroupName :: Core.Maybe Core.Text
    -- ^ The name of the DB security group to return details for.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ This parameter isn't currently supported.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeDBSecurityGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBSecurityGroups' value with any optional fields omitted.
mkDescribeDBSecurityGroups
    :: DescribeDBSecurityGroups
mkDescribeDBSecurityGroups
  = DescribeDBSecurityGroups'{dBSecurityGroupName = Core.Nothing,
                              filters = Core.Nothing, marker = Core.Nothing,
                              maxRecords = Core.Nothing}

-- | The name of the DB security group to return details for.
--
-- /Note:/ Consider using 'dBSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgsDBSecurityGroupName :: Lens.Lens' DescribeDBSecurityGroups (Core.Maybe Core.Text)
ddbsgsDBSecurityGroupName = Lens.field @"dBSecurityGroupName"
{-# INLINEABLE ddbsgsDBSecurityGroupName #-}
{-# DEPRECATED dBSecurityGroupName "Use generic-lens or generic-optics with 'dBSecurityGroupName' instead"  #-}

-- | This parameter isn't currently supported.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgsFilters :: Lens.Lens' DescribeDBSecurityGroups (Core.Maybe [Types.Filter])
ddbsgsFilters = Lens.field @"filters"
{-# INLINEABLE ddbsgsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeDBSecurityGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgsMarker :: Lens.Lens' DescribeDBSecurityGroups (Core.Maybe Core.Text)
ddbsgsMarker = Lens.field @"marker"
{-# INLINEABLE ddbsgsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgsMaxRecords :: Lens.Lens' DescribeDBSecurityGroups (Core.Maybe Core.Int)
ddbsgsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddbsgsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeDBSecurityGroups where
        toQuery DescribeDBSecurityGroups{..}
          = Core.toQueryPair "Action"
              ("DescribeDBSecurityGroups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBSecurityGroupName")
                dBSecurityGroupName
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeDBSecurityGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBSecurityGroups where
        type Rs DescribeDBSecurityGroups = DescribeDBSecurityGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeDBSecurityGroupsResult"
              (\ s h x ->
                 DescribeDBSecurityGroupsResponse' Core.<$>
                   (x Core..@? "DBSecurityGroups" Core..<@>
                      Core.parseXMLList "DBSecurityGroup")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBSecurityGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"dBSecurityGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the result of a successful invocation of the @DescribeDBSecurityGroups@ action. 
--
-- /See:/ 'mkDescribeDBSecurityGroupsResponse' smart constructor.
data DescribeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse'
  { dBSecurityGroups :: Core.Maybe [Types.DBSecurityGroup]
    -- ^ A list of @DBSecurityGroup@ instances. 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBSecurityGroupsResponse' value with any optional fields omitted.
mkDescribeDBSecurityGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBSecurityGroupsResponse
mkDescribeDBSecurityGroupsResponse responseStatus
  = DescribeDBSecurityGroupsResponse'{dBSecurityGroups =
                                        Core.Nothing,
                                      marker = Core.Nothing, responseStatus}

-- | A list of @DBSecurityGroup@ instances. 
--
-- /Note:/ Consider using 'dBSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgrfrsDBSecurityGroups :: Lens.Lens' DescribeDBSecurityGroupsResponse (Core.Maybe [Types.DBSecurityGroup])
ddbsgrfrsDBSecurityGroups = Lens.field @"dBSecurityGroups"
{-# INLINEABLE ddbsgrfrsDBSecurityGroups #-}
{-# DEPRECATED dBSecurityGroups "Use generic-lens or generic-optics with 'dBSecurityGroups' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgrfrsMarker :: Lens.Lens' DescribeDBSecurityGroupsResponse (Core.Maybe Core.Text)
ddbsgrfrsMarker = Lens.field @"marker"
{-# INLINEABLE ddbsgrfrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsgrfrsResponseStatus :: Lens.Lens' DescribeDBSecurityGroupsResponse Core.Int
ddbsgrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbsgrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
