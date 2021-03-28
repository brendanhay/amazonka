{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBInstanceAutomatedBackups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays backups for both current and deleted instances. For example, use this operation to find details about automated backups for previously deleted instances. Current instances with retention periods greater than zero (0) are returned for both the @DescribeDBInstanceAutomatedBackups@ and @DescribeDBInstances@ operations.
--
-- All parameters are optional.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBInstanceAutomatedBackups
    (
    -- * Creating a request
      DescribeDBInstanceAutomatedBackups (..)
    , mkDescribeDBInstanceAutomatedBackups
    -- ** Request lenses
    , dDBInstanceIdentifier
    , dDbiResourceId
    , dFilters
    , dMarker
    , dMaxRecords

    -- * Destructuring the response
    , DescribeDBInstanceAutomatedBackupsResponse (..)
    , mkDescribeDBInstanceAutomatedBackupsResponse
    -- ** Response lenses
    , ddbiabrfrsDBInstanceAutomatedBackups
    , ddbiabrfrsMarker
    , ddbiabrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Parameter input for DescribeDBInstanceAutomatedBackups. 
--
-- /See:/ 'mkDescribeDBInstanceAutomatedBackups' smart constructor.
data DescribeDBInstanceAutomatedBackups = DescribeDBInstanceAutomatedBackups'
  { dBInstanceIdentifier :: Core.Maybe Core.Text
    -- ^ (Optional) The user-supplied instance identifier. If this parameter is specified, it must match the identifier of an existing DB instance. It returns information from the specific DB instance' automated backup. This parameter isn't case-sensitive. 
  , dbiResourceId :: Core.Maybe Core.Text
    -- ^ The resource ID of the DB instance that is the source of the automated backup. This parameter isn't case-sensitive. 
  , filters :: Core.Maybe [Types.Filter]
    -- ^ A filter that specifies which resources to return based on status.
--
-- Supported filters are the following:
--
--     * @status@ 
--
--     * @active@ - automated backups for current instances
--
--
--     * @retained@ - automated backups for deleted instances
--
--
--     * @creating@ - automated backups that are waiting for the first automated snapshot to be available
--
--
--
--
--     * @db-instance-id@ - Accepts DB instance identifiers and Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance automated backupss identified by these ARNs.
--
--
--     * @dbi-resource-id@ - Accepts DB instance resource identifiers and DB Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance resources identified by these ARNs.
--
--
-- Returns all resources by default. The status for each resource is specified in the response.
  , marker :: Core.Maybe Core.Text
    -- ^ The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBInstanceAutomatedBackups' value with any optional fields omitted.
mkDescribeDBInstanceAutomatedBackups
    :: DescribeDBInstanceAutomatedBackups
mkDescribeDBInstanceAutomatedBackups
  = DescribeDBInstanceAutomatedBackups'{dBInstanceIdentifier =
                                          Core.Nothing,
                                        dbiResourceId = Core.Nothing, filters = Core.Nothing,
                                        marker = Core.Nothing, maxRecords = Core.Nothing}

-- | (Optional) The user-supplied instance identifier. If this parameter is specified, it must match the identifier of an existing DB instance. It returns information from the specific DB instance' automated backup. This parameter isn't case-sensitive. 
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBInstanceIdentifier :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Core.Maybe Core.Text)
dDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE dDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

-- | The resource ID of the DB instance that is the source of the automated backup. This parameter isn't case-sensitive. 
--
-- /Note:/ Consider using 'dbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDbiResourceId :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Core.Maybe Core.Text)
dDbiResourceId = Lens.field @"dbiResourceId"
{-# INLINEABLE dDbiResourceId #-}
{-# DEPRECATED dbiResourceId "Use generic-lens or generic-optics with 'dbiResourceId' instead"  #-}

-- | A filter that specifies which resources to return based on status.
--
-- Supported filters are the following:
--
--     * @status@ 
--
--     * @active@ - automated backups for current instances
--
--
--     * @retained@ - automated backups for deleted instances
--
--
--     * @creating@ - automated backups that are waiting for the first automated snapshot to be available
--
--
--
--
--     * @db-instance-id@ - Accepts DB instance identifiers and Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance automated backupss identified by these ARNs.
--
--
--     * @dbi-resource-id@ - Accepts DB instance resource identifiers and DB Amazon Resource Names (ARNs) for DB instances. The results list includes only information about the DB instance resources identified by these ARNs.
--
--
-- Returns all resources by default. The status for each resource is specified in the response.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFilters :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Core.Maybe [Types.Filter])
dFilters = Lens.field @"filters"
{-# INLINEABLE dFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The pagination token provided in the previous request. If this parameter is specified the response includes only records beyond the marker, up to @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMarker :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Core.Maybe Core.Text)
dMarker = Lens.field @"marker"
{-# INLINEABLE dMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxRecords :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Core.Maybe Core.Int)
dMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeDBInstanceAutomatedBackups where
        toQuery DescribeDBInstanceAutomatedBackups{..}
          = Core.toQueryPair "Action"
              ("DescribeDBInstanceAutomatedBackups" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBInstanceIdentifier")
                dBInstanceIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DbiResourceId")
                dbiResourceId
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords

instance Core.ToHeaders DescribeDBInstanceAutomatedBackups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBInstanceAutomatedBackups where
        type Rs DescribeDBInstanceAutomatedBackups =
             DescribeDBInstanceAutomatedBackupsResponse
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
          = Response.receiveXMLWrapper
              "DescribeDBInstanceAutomatedBackupsResult"
              (\ s h x ->
                 DescribeDBInstanceAutomatedBackupsResponse' Core.<$>
                   (x Core..@? "DBInstanceAutomatedBackups" Core..<@>
                      Core.parseXMLList "DBInstanceAutomatedBackup")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBInstanceAutomatedBackups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"dBInstanceAutomatedBackups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the result of a successful invocation of the @DescribeDBInstanceAutomatedBackups@ action. 
--
-- /See:/ 'mkDescribeDBInstanceAutomatedBackupsResponse' smart constructor.
data DescribeDBInstanceAutomatedBackupsResponse = DescribeDBInstanceAutomatedBackupsResponse'
  { dBInstanceAutomatedBackups :: Core.Maybe [Types.DBInstanceAutomatedBackup]
    -- ^ A list of @DBInstanceAutomatedBackup@ instances. 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDBInstanceAutomatedBackupsResponse' value with any optional fields omitted.
mkDescribeDBInstanceAutomatedBackupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBInstanceAutomatedBackupsResponse
mkDescribeDBInstanceAutomatedBackupsResponse responseStatus
  = DescribeDBInstanceAutomatedBackupsResponse'{dBInstanceAutomatedBackups
                                                  = Core.Nothing,
                                                marker = Core.Nothing, responseStatus}

-- | A list of @DBInstanceAutomatedBackup@ instances. 
--
-- /Note:/ Consider using 'dBInstanceAutomatedBackups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiabrfrsDBInstanceAutomatedBackups :: Lens.Lens' DescribeDBInstanceAutomatedBackupsResponse (Core.Maybe [Types.DBInstanceAutomatedBackup])
ddbiabrfrsDBInstanceAutomatedBackups = Lens.field @"dBInstanceAutomatedBackups"
{-# INLINEABLE ddbiabrfrsDBInstanceAutomatedBackups #-}
{-# DEPRECATED dBInstanceAutomatedBackups "Use generic-lens or generic-optics with 'dBInstanceAutomatedBackups' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiabrfrsMarker :: Lens.Lens' DescribeDBInstanceAutomatedBackupsResponse (Core.Maybe Core.Text)
ddbiabrfrsMarker = Lens.field @"marker"
{-# INLINEABLE ddbiabrfrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiabrfrsResponseStatus :: Lens.Lens' DescribeDBInstanceAutomatedBackupsResponse Core.Int
ddbiabrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbiabrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
