{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB snapshots. This API action supports pagination.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBSnapshots
    (
    -- * Creating a request
      DescribeDBSnapshots (..)
    , mkDescribeDBSnapshots
    -- ** Request lenses
    , ddbsDBInstanceIdentifier
    , ddbsDBSnapshotIdentifier
    , ddbsDbiResourceId
    , ddbsFilters
    , ddbsIncludePublic
    , ddbsIncludeShared
    , ddbsMarker
    , ddbsMaxRecords
    , ddbsSnapshotType

    -- * Destructuring the response
    , DescribeDBSnapshotsResponse (..)
    , mkDescribeDBSnapshotsResponse
    -- ** Response lenses
    , ddbsrrsDBSnapshots
    , ddbsrrsMarker
    , ddbsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeDBSnapshots' smart constructor.
data DescribeDBSnapshots = DescribeDBSnapshots'
  { dBInstanceIdentifier :: Core.Maybe Core.Text
    -- ^ The ID of the DB instance to retrieve the list of DB snapshots for. This parameter can't be used in conjunction with @DBSnapshotIdentifier@ . This parameter isn't case-sensitive. 
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBInstance.
--
--
  , dBSnapshotIdentifier :: Core.Maybe Core.Text
    -- ^ A specific DB snapshot identifier to describe. This parameter can't be used in conjunction with @DBInstanceIdentifier@ . This value is stored as a lowercase string. 
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBSnapshot.
--
--
--     * If this identifier is for an automated snapshot, the @SnapshotType@ parameter must also be specified.
--
--
  , dbiResourceId :: Core.Maybe Core.Text
    -- ^ A specific DB resource ID to describe.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ A filter that specifies one or more DB snapshots to describe.
--
-- Supported filters:
--
--     * @db-instance-id@ - Accepts DB instance identifiers and DB instance Amazon Resource Names (ARNs).
--
--
--     * @db-snapshot-id@ - Accepts DB snapshot identifiers.
--
--
--     * @dbi-resource-id@ - Accepts identifiers of source DB instances.
--
--
--     * @snapshot-type@ - Accepts types of DB snapshots.
--
--
--     * @engine@ - Accepts names of database engines.
--
--
  , includePublic :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to include manual DB cluster snapshots that are public and can be copied or restored by any AWS account. By default, the public snapshots are not included.
--
-- You can share a manual DB snapshot as public by using the 'ModifyDBSnapshotAttribute' API.
  , includeShared :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to include shared manual DB cluster snapshots from other AWS accounts that this AWS account has been given permission to copy or restore. By default, these snapshots are not included.
--
-- You can give an AWS account permission to restore a manual DB snapshot from another AWS account by using the @ModifyDBSnapshotAttribute@ API action.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous @DescribeDBSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  , snapshotType :: Core.Maybe Core.Text
    -- ^ The type of snapshots to be returned. You can specify one of the following values:
--
--
--     * @automated@ - Return all DB snapshots that have been automatically taken by Amazon RDS for my AWS account.
--
--
--     * @manual@ - Return all DB snapshots that have been taken by my AWS account.
--
--
--     * @shared@ - Return all manual DB snapshots that have been shared to my AWS account.
--
--
--     * @public@ - Return all DB snapshots that have been marked as public.
--
--
--     * @awsbackup@ - Return the DB snapshots managed by the AWS Backup service.
-- For information about AWS Backup, see the <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html /AWS Backup Developer Guide./ > 
-- The @awsbackup@ type does not apply to Aurora.
--
--
-- If you don't specify a @SnapshotType@ value, then both automated and manual snapshots are returned. Shared and public DB snapshots are not included in the returned results by default. You can include shared snapshots with these results by enabling the @IncludeShared@ parameter. You can include public snapshots with these results by enabling the @IncludePublic@ parameter.
-- The @IncludeShared@ and @IncludePublic@ parameters don't apply for @SnapshotType@ values of @manual@ or @automated@ . The @IncludePublic@ parameter doesn't apply when @SnapshotType@ is set to @shared@ . The @IncludeShared@ parameter doesn't apply when @SnapshotType@ is set to @public@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBSnapshots' value with any optional fields omitted.
mkDescribeDBSnapshots
    :: DescribeDBSnapshots
mkDescribeDBSnapshots
  = DescribeDBSnapshots'{dBInstanceIdentifier = Core.Nothing,
                         dBSnapshotIdentifier = Core.Nothing, dbiResourceId = Core.Nothing,
                         filters = Core.Nothing, includePublic = Core.Nothing,
                         includeShared = Core.Nothing, marker = Core.Nothing,
                         maxRecords = Core.Nothing, snapshotType = Core.Nothing}

-- | The ID of the DB instance to retrieve the list of DB snapshots for. This parameter can't be used in conjunction with @DBSnapshotIdentifier@ . This parameter isn't case-sensitive. 
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsDBInstanceIdentifier :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Core.Text)
ddbsDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE ddbsDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

-- | A specific DB snapshot identifier to describe. This parameter can't be used in conjunction with @DBInstanceIdentifier@ . This value is stored as a lowercase string. 
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBSnapshot.
--
--
--     * If this identifier is for an automated snapshot, the @SnapshotType@ parameter must also be specified.
--
--
--
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsDBSnapshotIdentifier :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Core.Text)
ddbsDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# INLINEABLE ddbsDBSnapshotIdentifier #-}
{-# DEPRECATED dBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead"  #-}

-- | A specific DB resource ID to describe.
--
-- /Note:/ Consider using 'dbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsDbiResourceId :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Core.Text)
ddbsDbiResourceId = Lens.field @"dbiResourceId"
{-# INLINEABLE ddbsDbiResourceId #-}
{-# DEPRECATED dbiResourceId "Use generic-lens or generic-optics with 'dbiResourceId' instead"  #-}

-- | A filter that specifies one or more DB snapshots to describe.
--
-- Supported filters:
--
--     * @db-instance-id@ - Accepts DB instance identifiers and DB instance Amazon Resource Names (ARNs).
--
--
--     * @db-snapshot-id@ - Accepts DB snapshot identifiers.
--
--
--     * @dbi-resource-id@ - Accepts identifiers of source DB instances.
--
--
--     * @snapshot-type@ - Accepts types of DB snapshots.
--
--
--     * @engine@ - Accepts names of database engines.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsFilters :: Lens.Lens' DescribeDBSnapshots (Core.Maybe [Types.Filter])
ddbsFilters = Lens.field @"filters"
{-# INLINEABLE ddbsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | A value that indicates whether to include manual DB cluster snapshots that are public and can be copied or restored by any AWS account. By default, the public snapshots are not included.
--
-- You can share a manual DB snapshot as public by using the 'ModifyDBSnapshotAttribute' API.
--
-- /Note:/ Consider using 'includePublic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsIncludePublic :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Core.Bool)
ddbsIncludePublic = Lens.field @"includePublic"
{-# INLINEABLE ddbsIncludePublic #-}
{-# DEPRECATED includePublic "Use generic-lens or generic-optics with 'includePublic' instead"  #-}

-- | A value that indicates whether to include shared manual DB cluster snapshots from other AWS accounts that this AWS account has been given permission to copy or restore. By default, these snapshots are not included.
--
-- You can give an AWS account permission to restore a manual DB snapshot from another AWS account by using the @ModifyDBSnapshotAttribute@ API action.
--
-- /Note:/ Consider using 'includeShared' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsIncludeShared :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Core.Bool)
ddbsIncludeShared = Lens.field @"includeShared"
{-# INLINEABLE ddbsIncludeShared #-}
{-# DEPRECATED includeShared "Use generic-lens or generic-optics with 'includeShared' instead"  #-}

-- | An optional pagination token provided by a previous @DescribeDBSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsMarker :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Core.Text)
ddbsMarker = Lens.field @"marker"
{-# INLINEABLE ddbsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsMaxRecords :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Core.Int)
ddbsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE ddbsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The type of snapshots to be returned. You can specify one of the following values:
--
--
--     * @automated@ - Return all DB snapshots that have been automatically taken by Amazon RDS for my AWS account.
--
--
--     * @manual@ - Return all DB snapshots that have been taken by my AWS account.
--
--
--     * @shared@ - Return all manual DB snapshots that have been shared to my AWS account.
--
--
--     * @public@ - Return all DB snapshots that have been marked as public.
--
--
--     * @awsbackup@ - Return the DB snapshots managed by the AWS Backup service.
-- For information about AWS Backup, see the <https://docs.aws.amazon.com/aws-backup/latest/devguide/whatisbackup.html /AWS Backup Developer Guide./ > 
-- The @awsbackup@ type does not apply to Aurora.
--
--
-- If you don't specify a @SnapshotType@ value, then both automated and manual snapshots are returned. Shared and public DB snapshots are not included in the returned results by default. You can include shared snapshots with these results by enabling the @IncludeShared@ parameter. You can include public snapshots with these results by enabling the @IncludePublic@ parameter.
-- The @IncludeShared@ and @IncludePublic@ parameters don't apply for @SnapshotType@ values of @manual@ or @automated@ . The @IncludePublic@ parameter doesn't apply when @SnapshotType@ is set to @shared@ . The @IncludeShared@ parameter doesn't apply when @SnapshotType@ is set to @public@ .
--
-- /Note:/ Consider using 'snapshotType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsSnapshotType :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Core.Text)
ddbsSnapshotType = Lens.field @"snapshotType"
{-# INLINEABLE ddbsSnapshotType #-}
{-# DEPRECATED snapshotType "Use generic-lens or generic-optics with 'snapshotType' instead"  #-}

instance Core.ToQuery DescribeDBSnapshots where
        toQuery DescribeDBSnapshots{..}
          = Core.toQueryPair "Action" ("DescribeDBSnapshots" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBInstanceIdentifier")
                dBInstanceIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBSnapshotIdentifier")
                dBSnapshotIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DbiResourceId")
                dbiResourceId
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

instance Core.ToHeaders DescribeDBSnapshots where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBSnapshots where
        type Rs DescribeDBSnapshots = DescribeDBSnapshotsResponse
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
          = Response.receiveXMLWrapper "DescribeDBSnapshotsResult"
              (\ s h x ->
                 DescribeDBSnapshotsResponse' Core.<$>
                   (x Core..@? "DBSnapshots" Core..<@> Core.parseXMLList "DBSnapshot")
                     Core.<*> x Core..@? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDBSnapshots where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"dBSnapshots" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | Contains the result of a successful invocation of the @DescribeDBSnapshots@ action. 
--
-- /See:/ 'mkDescribeDBSnapshotsResponse' smart constructor.
data DescribeDBSnapshotsResponse = DescribeDBSnapshotsResponse'
  { dBSnapshots :: Core.Maybe [Types.DBSnapshot]
    -- ^ A list of @DBSnapshot@ instances. 
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDBSnapshotsResponse' value with any optional fields omitted.
mkDescribeDBSnapshotsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBSnapshotsResponse
mkDescribeDBSnapshotsResponse responseStatus
  = DescribeDBSnapshotsResponse'{dBSnapshots = Core.Nothing,
                                 marker = Core.Nothing, responseStatus}

-- | A list of @DBSnapshot@ instances. 
--
-- /Note:/ Consider using 'dBSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsrrsDBSnapshots :: Lens.Lens' DescribeDBSnapshotsResponse (Core.Maybe [Types.DBSnapshot])
ddbsrrsDBSnapshots = Lens.field @"dBSnapshots"
{-# INLINEABLE ddbsrrsDBSnapshots #-}
{-# DEPRECATED dBSnapshots "Use generic-lens or generic-optics with 'dBSnapshots' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsrrsMarker :: Lens.Lens' DescribeDBSnapshotsResponse (Core.Maybe Core.Text)
ddbsrrsMarker = Lens.field @"marker"
{-# INLINEABLE ddbsrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsrrsResponseStatus :: Lens.Lens' DescribeDBSnapshotsResponse Core.Int
ddbsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
