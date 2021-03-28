{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB cluster snapshots. This API action supports pagination.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./ 
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterSnapshots
    (
    -- * Creating a request
      DescribeDBClusterSnapshots (..)
    , mkDescribeDBClusterSnapshots
    -- ** Request lenses
    , ddbcssDBClusterIdentifier
    , ddbcssDBClusterSnapshotIdentifier
    , ddbcssFilters
    , ddbcssIncludePublic
    , ddbcssIncludeShared
    , ddbcssMarker
    , ddbcssMaxRecords
    , ddbcssSnapshotType

    -- * Destructuring the response
    , DescribeDBClusterSnapshotsResponse (..)
    , mkDescribeDBClusterSnapshotsResponse
    -- ** Response lenses
    , ddbcsrfrsDBClusterSnapshots
    , ddbcsrfrsMarker
    , ddbcsrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeDBClusterSnapshots' smart constructor.
data DescribeDBClusterSnapshots = DescribeDBClusterSnapshots'
  { dBClusterIdentifier :: Core.Maybe Core.Text
    -- ^ The ID of the DB cluster to retrieve the list of DB cluster snapshots for. This parameter can't be used in conjunction with the @DBClusterSnapshotIdentifier@ parameter. This parameter isn't case-sensitive. 
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBCluster.
--
--
  , dBClusterSnapshotIdentifier :: Core.Maybe Core.Text
    -- ^ A specific DB cluster snapshot identifier to describe. This parameter can't be used in conjunction with the @DBClusterIdentifier@ parameter. This value is stored as a lowercase string. 
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBClusterSnapshot.
--
--
--     * If this identifier is for an automated snapshot, the @SnapshotType@ parameter must also be specified.
--
--
  , filters :: Core.Maybe [Types.Filter]
    -- ^ A filter that specifies one or more DB cluster snapshots to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs).
--
--
--     * @db-cluster-snapshot-id@ - Accepts DB cluster snapshot identifiers.
--
--
--     * @snapshot-type@ - Accepts types of DB cluster snapshots.
--
--
--     * @engine@ - Accepts names of database engines.
--
--
  , includePublic :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to include manual DB cluster snapshots that are public and can be copied or restored by any AWS account. By default, the public snapshots are not included.
--
-- You can share a manual DB cluster snapshot as public by using the 'ModifyDBClusterSnapshotAttribute' API action.
  , includeShared :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to include shared manual DB cluster snapshots from other AWS accounts that this AWS account has been given permission to copy or restore. By default, these snapshots are not included.
--
-- You can give an AWS account permission to restore a manual DB cluster snapshot from another AWS account by the @ModifyDBClusterSnapshotAttribute@ API action.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeDBClusterSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  , snapshotType :: Core.Maybe Core.Text
    -- ^ The type of DB cluster snapshots to be returned. You can specify one of the following values:
--
--
--     * @automated@ - Return all DB cluster snapshots that have been automatically taken by Amazon RDS for my AWS account.
--
--
--     * @manual@ - Return all DB cluster snapshots that have been taken by my AWS account.
--
--
--     * @shared@ - Return all manual DB cluster snapshots that have been shared to my AWS account.
--
--
--     * @public@ - Return all DB cluster snapshots that have been marked as public.
--
--
-- If you don't specify a @SnapshotType@ value, then both automated and manual DB cluster snapshots are returned. You can include shared DB cluster snapshots with these results by enabling the @IncludeShared@ parameter. You can include public DB cluster snapshots with these results by enabling the @IncludePublic@ parameter.
-- The @IncludeShared@ and @IncludePublic@ parameters don't apply for @SnapshotType@ values of @manual@ or @automated@ . The @IncludePublic@ parameter doesn't apply when @SnapshotType@ is set to @shared@ . The @IncludeShared@ parameter doesn't apply when @SnapshotType@ is set to @public@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBClusterSnapshots' value with any optional fields omitted.
mkDescribeDBClusterSnapshots
    :: DescribeDBClusterSnapshots
mkDescribeDBClusterSnapshots
  = DescribeDBClusterSnapshots'{dBClusterIdentifier = Core.Nothing,
                                dBClusterSnapshotIdentifier = Core.Nothing, filters = Core.Nothing,
                                includePublic = Core.Nothing, includeShared = Core.Nothing,
                                marker = Core.Nothing, maxRecords = Core.Nothing,
                                snapshotType = Core.Nothing}

-- | The ID of the DB cluster to retrieve the list of DB cluster snapshots for. This parameter can't be used in conjunction with the @DBClusterSnapshotIdentifier@ parameter. This parameter isn't case-sensitive. 
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBCluster.
--
--
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcssDBClusterIdentifier :: Lens.Lens' DescribeDBClusterSnapshots (Core.Maybe Core.Text)
ddbcssDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# INLINEABLE ddbcssDBClusterIdentifier #-}
{-# DEPRECATED dBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead"  #-}

-- | A specific DB cluster snapshot identifier to describe. This parameter can't be used in conjunction with the @DBClusterIdentifier@ parameter. This value is stored as a lowercase string. 
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBClusterSnapshot.
--
--
--     * If this identifier is for an automated snapshot, the @SnapshotType@ parameter must also be specified.
--
--
--
-- /Note:/ Consider using 'dBClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcssDBClusterSnapshotIdentifier :: Lens.Lens' DescribeDBClusterSnapshots (Core.Maybe Core.Text)
ddbcssDBClusterSnapshotIdentifier = Lens.field @"dBClusterSnapshotIdentifier"
{-# INLINEABLE ddbcssDBClusterSnapshotIdentifier #-}
{-# DEPRECATED dBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dBClusterSnapshotIdentifier' instead"  #-}

-- | A filter that specifies one or more DB cluster snapshots to describe.
--
-- Supported filters:
--
--     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs).
--
--
--     * @db-cluster-snapshot-id@ - Accepts DB cluster snapshot identifiers.
--
--
--     * @snapshot-type@ - Accepts types of DB cluster snapshots.
--
--
--     * @engine@ - Accepts names of database engines.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcssFilters :: Lens.Lens' DescribeDBClusterSnapshots (Core.Maybe [Types.Filter])
ddbcssFilters = Lens.field @"filters"
{-# INLINEABLE ddbcssFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | A value that indicates whether to include manual DB cluster snapshots that are public and can be copied or restored by any AWS account. By default, the public snapshots are not included.
--
-- You can share a manual DB cluster snapshot as public by using the 'ModifyDBClusterSnapshotAttribute' API action.
--
-- /Note:/ Consider using 'includePublic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcssIncludePublic :: Lens.Lens' DescribeDBClusterSnapshots (Core.Maybe Core.Bool)
ddbcssIncludePublic = Lens.field @"includePublic"
{-# INLINEABLE ddbcssIncludePublic #-}
{-# DEPRECATED includePublic "Use generic-lens or generic-optics with 'includePublic' instead"  #-}

-- | A value that indicates whether to include shared manual DB cluster snapshots from other AWS accounts that this AWS account has been given permission to copy or restore. By default, these snapshots are not included.
--
-- You can give an AWS account permission to restore a manual DB cluster snapshot from another AWS account by the @ModifyDBClusterSnapshotAttribute@ API action.
--
-- /Note:/ Consider using 'includeShared' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcssIncludeShared :: Lens.Lens' DescribeDBClusterSnapshots (Core.Maybe Core.Bool)
ddbcssIncludeShared = Lens.field @"includeShared"
{-# INLINEABLE ddbcssIncludeShared #-}
{-# DEPRECATED includeShared "Use generic-lens or generic-optics with 'includeShared' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeDBClusterSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcssMarker :: Lens.Lens' DescribeDBClusterSnapshots (Core.Maybe Core.Text)
ddbcssMarker = Lens.field @"marker"
{-# INLINEABLE ddbcssMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcssMaxRecords :: Lens.Lens' DescribeDBClusterSnapshots (Core.Maybe Core.Int)
ddbcssMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddbcssMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The type of DB cluster snapshots to be returned. You can specify one of the following values:
--
--
--     * @automated@ - Return all DB cluster snapshots that have been automatically taken by Amazon RDS for my AWS account.
--
--
--     * @manual@ - Return all DB cluster snapshots that have been taken by my AWS account.
--
--
--     * @shared@ - Return all manual DB cluster snapshots that have been shared to my AWS account.
--
--
--     * @public@ - Return all DB cluster snapshots that have been marked as public.
--
--
-- If you don't specify a @SnapshotType@ value, then both automated and manual DB cluster snapshots are returned. You can include shared DB cluster snapshots with these results by enabling the @IncludeShared@ parameter. You can include public DB cluster snapshots with these results by enabling the @IncludePublic@ parameter.
-- The @IncludeShared@ and @IncludePublic@ parameters don't apply for @SnapshotType@ values of @manual@ or @automated@ . The @IncludePublic@ parameter doesn't apply when @SnapshotType@ is set to @shared@ . The @IncludeShared@ parameter doesn't apply when @SnapshotType@ is set to @public@ .
--
-- /Note:/ Consider using 'snapshotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcssSnapshotType :: Lens.Lens' DescribeDBClusterSnapshots (Core.Maybe Core.Text)
ddbcssSnapshotType = Lens.field @"snapshotType"
{-# INLINEABLE ddbcssSnapshotType #-}
{-# DEPRECATED snapshotType "Use generic-lens or generic-optics with 'snapshotType' instead"  #-}

instance Core.ToQuery DescribeDBClusterSnapshots where
        toQuery DescribeDBClusterSnapshots{..}
          = Core.toQueryPair "Action"
              ("DescribeDBClusterSnapshots" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBClusterIdentifier")
                dBClusterIdentifier
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "DBClusterSnapshotIdentifier")
                dBClusterSnapshotIdentifier
              Core.<>
              Core.toQueryPair "Filters"
                (Core.maybe Core.mempty (Core.toQueryList "Filter") filters)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IncludePublic")
                includePublic
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IncludeShared")
                includeShared
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SnapshotType")
                snapshotType

instance Core.ToHeaders DescribeDBClusterSnapshots where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBClusterSnapshots where
        type Rs DescribeDBClusterSnapshots =
             DescribeDBClusterSnapshotsResponse
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
          = Response.receiveXMLWrapper "DescribeDBClusterSnapshotsResult"
              (\ s h x ->
                 DescribeDBClusterSnapshotsResponse' Core.<$>
                   (x Core..@? "DBClusterSnapshots" Core..<@>
                      Core.parseXMLList "DBClusterSnapshot")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBClusterSnapshots where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"dBClusterSnapshots" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Provides a list of DB cluster snapshots for the user as the result of a call to the @DescribeDBClusterSnapshots@ action. 
--
-- /See:/ 'mkDescribeDBClusterSnapshotsResponse' smart constructor.
data DescribeDBClusterSnapshotsResponse = DescribeDBClusterSnapshotsResponse'
  { dBClusterSnapshots :: Core.Maybe [Types.DBClusterSnapshot]
    -- ^ Provides a list of DB cluster snapshots for the user.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeDBClusterSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDBClusterSnapshotsResponse' value with any optional fields omitted.
mkDescribeDBClusterSnapshotsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBClusterSnapshotsResponse
mkDescribeDBClusterSnapshotsResponse responseStatus
  = DescribeDBClusterSnapshotsResponse'{dBClusterSnapshots =
                                          Core.Nothing,
                                        marker = Core.Nothing, responseStatus}

-- | Provides a list of DB cluster snapshots for the user.
--
-- /Note:/ Consider using 'dBClusterSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsrfrsDBClusterSnapshots :: Lens.Lens' DescribeDBClusterSnapshotsResponse (Core.Maybe [Types.DBClusterSnapshot])
ddbcsrfrsDBClusterSnapshots = Lens.field @"dBClusterSnapshots"
{-# INLINEABLE ddbcsrfrsDBClusterSnapshots #-}
{-# DEPRECATED dBClusterSnapshots "Use generic-lens or generic-optics with 'dBClusterSnapshots' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeDBClusterSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsrfrsMarker :: Lens.Lens' DescribeDBClusterSnapshotsResponse (Core.Maybe Core.Text)
ddbcsrfrsMarker = Lens.field @"marker"
{-# INLINEABLE ddbcsrfrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsrfrsResponseStatus :: Lens.Lens' DescribeDBClusterSnapshotsResponse Core.Int
ddbcsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbcsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
