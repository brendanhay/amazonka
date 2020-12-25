{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeDBInstances (..),
    mkDescribeDBInstances,

    -- ** Request lenses
    ddbisDBInstanceIdentifier,
    ddbisFilters,
    ddbisMarker,
    ddbisMaxRecords,

    -- * Destructuring the response
    DescribeDBInstancesResponse (..),
    mkDescribeDBInstancesResponse,

    -- ** Response lenses
    ddbirfrsDBInstances,
    ddbirfrsMarker,
    ddbirfrsResponseStatus,
  )
where

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
  { -- | The user-supplied instance identifier. If this parameter is specified, information from only the specific DB instance is returned. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * If supplied, must match the identifier of an existing DBInstance.
    dBInstanceIdentifier :: Core.Maybe Types.String,
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
    filters :: Core.Maybe [Types.Filter],
    -- | An optional pagination token provided by a previous @DescribeDBInstances@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBInstances' value with any optional fields omitted.
mkDescribeDBInstances ::
  DescribeDBInstances
mkDescribeDBInstances =
  DescribeDBInstances'
    { dBInstanceIdentifier = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The user-supplied instance identifier. If this parameter is specified, information from only the specific DB instance is returned. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbisDBInstanceIdentifier :: Lens.Lens' DescribeDBInstances (Core.Maybe Types.String)
ddbisDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED ddbisDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

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
{-# DEPRECATED ddbisFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBInstances@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbisMarker :: Lens.Lens' DescribeDBInstances (Core.Maybe Types.String)
ddbisMarker = Lens.field @"marker"
{-# DEPRECATED ddbisMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbisMaxRecords :: Lens.Lens' DescribeDBInstances (Core.Maybe Core.Int)
ddbisMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED ddbisMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Core.AWSRequest DescribeDBInstances where
  type Rs DescribeDBInstances = DescribeDBInstancesResponse
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
            ( Core.pure ("Action", "DescribeDBInstances")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue "DBInstanceIdentifier"
                            Core.<$> dBInstanceIdentifier
                        )
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeDBInstancesResult"
      ( \s h x ->
          DescribeDBInstancesResponse'
            Core.<$> (x Core..@? "DBInstances" Core..<@> Core.parseXMLList "DBInstance")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDBInstances where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"dBInstances" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the result of a successful invocation of the @DescribeDBInstances@ action.
--
-- /See:/ 'mkDescribeDBInstancesResponse' smart constructor.
data DescribeDBInstancesResponse = DescribeDBInstancesResponse'
  { -- | A list of @DBInstance@ instances.
    dBInstances :: Core.Maybe [Types.DBInstance],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDBInstancesResponse' value with any optional fields omitted.
mkDescribeDBInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDBInstancesResponse
mkDescribeDBInstancesResponse responseStatus =
  DescribeDBInstancesResponse'
    { dBInstances = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of @DBInstance@ instances.
--
-- /Note:/ Consider using 'dBInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbirfrsDBInstances :: Lens.Lens' DescribeDBInstancesResponse (Core.Maybe [Types.DBInstance])
ddbirfrsDBInstances = Lens.field @"dBInstances"
{-# DEPRECATED ddbirfrsDBInstances "Use generic-lens or generic-optics with 'dBInstances' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbirfrsMarker :: Lens.Lens' DescribeDBInstancesResponse (Core.Maybe Types.String)
ddbirfrsMarker = Lens.field @"marker"
{-# DEPRECATED ddbirfrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbirfrsResponseStatus :: Lens.Lens' DescribeDBInstancesResponse Core.Int
ddbirfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbirfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
