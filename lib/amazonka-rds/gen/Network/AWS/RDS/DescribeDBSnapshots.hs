{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeDBSnapshots (..),
    mkDescribeDBSnapshots,

    -- ** Request lenses
    ddbsDBInstanceIdentifier,
    ddbsDBSnapshotIdentifier,
    ddbsDbiResourceId,
    ddbsFilters,
    ddbsIncludePublic,
    ddbsIncludeShared,
    ddbsMarker,
    ddbsMaxRecords,
    ddbsSnapshotType,

    -- * Destructuring the response
    DescribeDBSnapshotsResponse (..),
    mkDescribeDBSnapshotsResponse,

    -- ** Response lenses
    ddbsrrsDBSnapshots,
    ddbsrrsMarker,
    ddbsrrsResponseStatus,
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
-- /See:/ 'mkDescribeDBSnapshots' smart constructor.
data DescribeDBSnapshots = DescribeDBSnapshots'
  { -- | The ID of the DB instance to retrieve the list of DB snapshots for. This parameter can't be used in conjunction with @DBSnapshotIdentifier@ . This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * If supplied, must match the identifier of an existing DBInstance.
    dBInstanceIdentifier :: Core.Maybe Types.DBInstanceIdentifier,
    -- | A specific DB snapshot identifier to describe. This parameter can't be used in conjunction with @DBInstanceIdentifier@ . This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * If supplied, must match the identifier of an existing DBSnapshot.
    --
    --
    --     * If this identifier is for an automated snapshot, the @SnapshotType@ parameter must also be specified.
    dBSnapshotIdentifier :: Core.Maybe Types.DBSnapshotIdentifier,
    -- | A specific DB resource ID to describe.
    dbiResourceId :: Core.Maybe Types.DbiResourceId,
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
    filters :: Core.Maybe [Types.Filter],
    -- | A value that indicates whether to include manual DB cluster snapshots that are public and can be copied or restored by any AWS account. By default, the public snapshots are not included.
    --
    -- You can share a manual DB snapshot as public by using the 'ModifyDBSnapshotAttribute' API.
    includePublic :: Core.Maybe Core.Bool,
    -- | A value that indicates whether to include shared manual DB cluster snapshots from other AWS accounts that this AWS account has been given permission to copy or restore. By default, these snapshots are not included.
    --
    -- You can give an AWS account permission to restore a manual DB snapshot from another AWS account by using the @ModifyDBSnapshotAttribute@ API action.
    includeShared :: Core.Maybe Core.Bool,
    -- | An optional pagination token provided by a previous @DescribeDBSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
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
    snapshotType :: Core.Maybe Types.SnapshotType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBSnapshots' value with any optional fields omitted.
mkDescribeDBSnapshots ::
  DescribeDBSnapshots
mkDescribeDBSnapshots =
  DescribeDBSnapshots'
    { dBInstanceIdentifier = Core.Nothing,
      dBSnapshotIdentifier = Core.Nothing,
      dbiResourceId = Core.Nothing,
      filters = Core.Nothing,
      includePublic = Core.Nothing,
      includeShared = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      snapshotType = Core.Nothing
    }

-- | The ID of the DB instance to retrieve the list of DB snapshots for. This parameter can't be used in conjunction with @DBSnapshotIdentifier@ . This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsDBInstanceIdentifier :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Types.DBInstanceIdentifier)
ddbsDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# DEPRECATED ddbsDBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead." #-}

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
ddbsDBSnapshotIdentifier :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Types.DBSnapshotIdentifier)
ddbsDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# DEPRECATED ddbsDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead." #-}

-- | A specific DB resource ID to describe.
--
-- /Note:/ Consider using 'dbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsDbiResourceId :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Types.DbiResourceId)
ddbsDbiResourceId = Lens.field @"dbiResourceId"
{-# DEPRECATED ddbsDbiResourceId "Use generic-lens or generic-optics with 'dbiResourceId' instead." #-}

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
{-# DEPRECATED ddbsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A value that indicates whether to include manual DB cluster snapshots that are public and can be copied or restored by any AWS account. By default, the public snapshots are not included.
--
-- You can share a manual DB snapshot as public by using the 'ModifyDBSnapshotAttribute' API.
--
-- /Note:/ Consider using 'includePublic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsIncludePublic :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Core.Bool)
ddbsIncludePublic = Lens.field @"includePublic"
{-# DEPRECATED ddbsIncludePublic "Use generic-lens or generic-optics with 'includePublic' instead." #-}

-- | A value that indicates whether to include shared manual DB cluster snapshots from other AWS accounts that this AWS account has been given permission to copy or restore. By default, these snapshots are not included.
--
-- You can give an AWS account permission to restore a manual DB snapshot from another AWS account by using the @ModifyDBSnapshotAttribute@ API action.
--
-- /Note:/ Consider using 'includeShared' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsIncludeShared :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Core.Bool)
ddbsIncludeShared = Lens.field @"includeShared"
{-# DEPRECATED ddbsIncludeShared "Use generic-lens or generic-optics with 'includeShared' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsMarker :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Types.Marker)
ddbsMarker = Lens.field @"marker"
{-# DEPRECATED ddbsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsMaxRecords :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Core.Int)
ddbsMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED ddbsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

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
ddbsSnapshotType :: Lens.Lens' DescribeDBSnapshots (Core.Maybe Types.SnapshotType)
ddbsSnapshotType = Lens.field @"snapshotType"
{-# DEPRECATED ddbsSnapshotType "Use generic-lens or generic-optics with 'snapshotType' instead." #-}

instance Core.AWSRequest DescribeDBSnapshots where
  type Rs DescribeDBSnapshots = DescribeDBSnapshotsResponse
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
            ( Core.pure ("Action", "DescribeDBSnapshots")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue "DBInstanceIdentifier"
                            Core.<$> dBInstanceIdentifier
                        )
                Core.<> ( Core.toQueryValue "DBSnapshotIdentifier"
                            Core.<$> dBSnapshotIdentifier
                        )
                Core.<> (Core.toQueryValue "DbiResourceId" Core.<$> dbiResourceId)
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "Filter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "IncludePublic" Core.<$> includePublic)
                Core.<> (Core.toQueryValue "IncludeShared" Core.<$> includeShared)
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "SnapshotType" Core.<$> snapshotType)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeDBSnapshotsResult"
      ( \s h x ->
          DescribeDBSnapshotsResponse'
            Core.<$> (x Core..@? "DBSnapshots" Core..<@> Core.parseXMLList "DBSnapshot")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDBSnapshots where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"dBSnapshots" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | Contains the result of a successful invocation of the @DescribeDBSnapshots@ action.
--
-- /See:/ 'mkDescribeDBSnapshotsResponse' smart constructor.
data DescribeDBSnapshotsResponse = DescribeDBSnapshotsResponse'
  { -- | A list of @DBSnapshot@ instances.
    dBSnapshots :: Core.Maybe [Types.DBSnapshot],
    -- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDBSnapshotsResponse' value with any optional fields omitted.
mkDescribeDBSnapshotsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDBSnapshotsResponse
mkDescribeDBSnapshotsResponse responseStatus =
  DescribeDBSnapshotsResponse'
    { dBSnapshots = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | A list of @DBSnapshot@ instances.
--
-- /Note:/ Consider using 'dBSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsrrsDBSnapshots :: Lens.Lens' DescribeDBSnapshotsResponse (Core.Maybe [Types.DBSnapshot])
ddbsrrsDBSnapshots = Lens.field @"dBSnapshots"
{-# DEPRECATED ddbsrrsDBSnapshots "Use generic-lens or generic-optics with 'dBSnapshots' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsrrsMarker :: Lens.Lens' DescribeDBSnapshotsResponse (Core.Maybe Types.String)
ddbsrrsMarker = Lens.field @"marker"
{-# DEPRECATED ddbsrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsrrsResponseStatus :: Lens.Lens' DescribeDBSnapshotsResponse Core.Int
ddbsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
