{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about provisioned RDS instances. This API supports pagination.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBInstances
    (
    -- * Creating a request
      DescribeDBInstances (..)
    , mkDescribeDBInstances
    -- ** Request lenses
    , ddbisDBInstanceIdentifier
    , ddbisFilters
    , ddbisMarker
    , ddbisMaxRecords

    -- * Destructuring the response
    , DescribeDBInstancesResponse (..)
    , mkDescribeDBInstancesResponse
    -- ** Response lenses
    , ddbirfrsDBInstances
    , ddbirfrsMarker
    , ddbirfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeDBInstances' smart constructor.
data DescribeDBInstances = DescribeDBInstances'
  { dBInstanceIdentifier :: Core.Maybe Core.Text
    -- ^ The user-supplied instance identifier. If this parameter is specified, information from only the specific DB instance is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBInstance.
--
--
  , filters :: Core.Maybe [Types.Filter]
    -- ^ A filter that specifies one or more DB instances to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB instances associated with the DB clusters identified by these ARNs.
--
--
--     * @db-instance-id@ - Accepts DB instance identifiers and DB instance Amazon Resource Names (ARNs). The results list will only include information about the DB instances identified by these ARNs.
--
--
--     * @dbi-resource-id@ - Accepts DB instance resource identifiers. The results list will only include information about the DB instances identified by these DB instance resource identifiers.
--
--
--     * @domain@ - Accepts Active Directory directory IDs. The results list will only include information about the DB instances associated with these domains.
--
--
--     * @engine@ - Accepts engine names. The results list will only include information about the DB instances for these engines.
--
--
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeDBInstances@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBInstances' value with any optional fields omitted.
mkDescribeDBInstances
    :: DescribeDBInstances
mkDescribeDBInstances
  = DescribeDBInstances'{dBInstanceIdentifier = Core.Nothing,
                         filters = Core.Nothing, marker = Core.Nothing,
                         maxRecords = Core.Nothing}

-- | The user-supplied instance identifier. If this parameter is specified, information from only the specific DB instance is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbisDBInstanceIdentifier :: Lens.Lens' DescribeDBInstances (Core.Maybe Core.Text)
ddbisDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE ddbisDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

-- | A filter that specifies one or more DB instances to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB instances associated with the DB clusters identified by these ARNs.
--
--
--     * @db-instance-id@ - Accepts DB instance identifiers and DB instance Amazon Resource Names (ARNs). The results list will only include information about the DB instances identified by these ARNs.
--
--
--     * @dbi-resource-id@ - Accepts DB instance resource identifiers. The results list will only include information about the DB instances identified by these DB instance resource identifiers.
--
--
--     * @domain@ - Accepts Active Directory directory IDs. The results list will only include information about the DB instances associated with these domains.
--
--
--     * @engine@ - Accepts engine names. The results list will only include information about the DB instances for these engines.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbisFilters :: Lens.Lens' DescribeDBInstances (Core.Maybe [Types.Filter])
ddbisFilters = Lens.field @"filters"
{-# INLINEABLE ddbisFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeDBInstances@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbisMarker :: Lens.Lens' DescribeDBInstances (Core.Maybe Core.Text)
ddbisMarker = Lens.field @"marker"
{-# INLINEABLE ddbisMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbisMaxRecords :: Lens.Lens' DescribeDBInstances (Core.Maybe Core.Int)
ddbisMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddbisMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeDBInstances where
        toQuery DescribeDBInstances{..}
          = Core.toQueryPair "Action" ("DescribeDBInstances" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBInstanceIdentifier")
                dBInstanceIdentifier
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeDBInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBInstances where
        type Rs DescribeDBInstances = DescribeDBInstancesResponse
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
          = Response.receiveXMLWrapper "DescribeDBInstancesResult"
              (\ s h x ->
                 DescribeDBInstancesResponse' Core.<$>
                   (x Core..@? "DBInstances" Core..<@> Core.parseXMLList "DBInstance")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBInstances where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"dBInstances" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the result of a successful invocation of the @DescribeDBInstances@ action. 
--
-- /See:/ 'mkDescribeDBInstancesResponse' smart constructor.
data DescribeDBInstancesResponse = DescribeDBInstancesResponse'
  { dBInstances :: Core.Maybe [Types.DBInstance]
    -- ^ A list of @DBInstance@ instances. 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDBInstancesResponse' value with any optional fields omitted.
mkDescribeDBInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBInstancesResponse
mkDescribeDBInstancesResponse responseStatus
  = DescribeDBInstancesResponse'{dBInstances = Core.Nothing,
                                 marker = Core.Nothing, responseStatus}

-- | A list of @DBInstance@ instances. 
--
-- /Note:/ Consider using 'dBInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbirfrsDBInstances :: Lens.Lens' DescribeDBInstancesResponse (Core.Maybe [Types.DBInstance])
ddbirfrsDBInstances = Lens.field @"dBInstances"
{-# INLINEABLE ddbirfrsDBInstances #-}
{-# DEPRECATED dBInstances "Use generic-lens or generic-optics with 'dBInstances' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbirfrsMarker :: Lens.Lens' DescribeDBInstancesResponse (Core.Maybe Core.Text)
ddbirfrsMarker = Lens.field @"marker"
{-# INLINEABLE ddbirfrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbirfrsResponseStatus :: Lens.Lens' DescribeDBInstancesResponse Core.Int
ddbirfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbirfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
