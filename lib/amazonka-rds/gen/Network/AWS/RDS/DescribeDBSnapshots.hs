{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ddsIncludeShared,
    ddsFilters,
    ddsDBSnapshotIdentifier,
    ddsSnapshotType,
    ddsDBInstanceIdentifier,
    ddsMarker,
    ddsMaxRecords,
    ddsIncludePublic,
    ddsDBiResourceId,

    -- * Destructuring the response
    DescribeDBSnapshotsResponse (..),
    mkDescribeDBSnapshotsResponse,

    -- ** Response lenses
    ddsrsMarker,
    ddsrsDBSnapshots,
    ddsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeDBSnapshots' smart constructor.
data DescribeDBSnapshots = DescribeDBSnapshots'
  { includeShared ::
      Lude.Maybe Lude.Bool,
    filters :: Lude.Maybe [Filter],
    dbSnapshotIdentifier :: Lude.Maybe Lude.Text,
    snapshotType :: Lude.Maybe Lude.Text,
    dbInstanceIdentifier :: Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    includePublic :: Lude.Maybe Lude.Bool,
    dbiResourceId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBSnapshots' with the minimum fields required to make a request.
--
-- * 'dbInstanceIdentifier' - The ID of the DB instance to retrieve the list of DB snapshots for. This parameter can't be used in conjunction with @DBSnapshotIdentifier@ . This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBInstance.
--
--
-- * 'dbSnapshotIdentifier' - A specific DB snapshot identifier to describe. This parameter can't be used in conjunction with @DBInstanceIdentifier@ . This value is stored as a lowercase string.
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBSnapshot.
--
--
--     * If this identifier is for an automated snapshot, the @SnapshotType@ parameter must also be specified.
--
--
-- * 'dbiResourceId' - A specific DB resource ID to describe.
-- * 'filters' - A filter that specifies one or more DB snapshots to describe.
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
-- * 'includePublic' - A value that indicates whether to include manual DB cluster snapshots that are public and can be copied or restored by any AWS account. By default, the public snapshots are not included.
--
-- You can share a manual DB snapshot as public by using the 'ModifyDBSnapshotAttribute' API.
-- * 'includeShared' - A value that indicates whether to include shared manual DB cluster snapshots from other AWS accounts that this AWS account has been given permission to copy or restore. By default, these snapshots are not included.
--
-- You can give an AWS account permission to restore a manual DB snapshot from another AWS account by using the @ModifyDBSnapshotAttribute@ API action.
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'snapshotType' - The type of snapshots to be returned. You can specify one of the following values:
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
mkDescribeDBSnapshots ::
  DescribeDBSnapshots
mkDescribeDBSnapshots =
  DescribeDBSnapshots'
    { includeShared = Lude.Nothing,
      filters = Lude.Nothing,
      dbSnapshotIdentifier = Lude.Nothing,
      snapshotType = Lude.Nothing,
      dbInstanceIdentifier = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      includePublic = Lude.Nothing,
      dbiResourceId = Lude.Nothing
    }

-- | A value that indicates whether to include shared manual DB cluster snapshots from other AWS accounts that this AWS account has been given permission to copy or restore. By default, these snapshots are not included.
--
-- You can give an AWS account permission to restore a manual DB snapshot from another AWS account by using the @ModifyDBSnapshotAttribute@ API action.
--
-- /Note:/ Consider using 'includeShared' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsIncludeShared :: Lens.Lens' DescribeDBSnapshots (Lude.Maybe Lude.Bool)
ddsIncludeShared = Lens.lens (includeShared :: DescribeDBSnapshots -> Lude.Maybe Lude.Bool) (\s a -> s {includeShared = a} :: DescribeDBSnapshots)
{-# DEPRECATED ddsIncludeShared "Use generic-lens or generic-optics with 'includeShared' instead." #-}

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
ddsFilters :: Lens.Lens' DescribeDBSnapshots (Lude.Maybe [Filter])
ddsFilters = Lens.lens (filters :: DescribeDBSnapshots -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBSnapshots)
{-# DEPRECATED ddsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

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
-- /Note:/ Consider using 'dbSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsDBSnapshotIdentifier :: Lens.Lens' DescribeDBSnapshots (Lude.Maybe Lude.Text)
ddsDBSnapshotIdentifier = Lens.lens (dbSnapshotIdentifier :: DescribeDBSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {dbSnapshotIdentifier = a} :: DescribeDBSnapshots)
{-# DEPRECATED ddsDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dbSnapshotIdentifier' instead." #-}

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
ddsSnapshotType :: Lens.Lens' DescribeDBSnapshots (Lude.Maybe Lude.Text)
ddsSnapshotType = Lens.lens (snapshotType :: DescribeDBSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {snapshotType = a} :: DescribeDBSnapshots)
{-# DEPRECATED ddsSnapshotType "Use generic-lens or generic-optics with 'snapshotType' instead." #-}

-- | The ID of the DB instance to retrieve the list of DB snapshots for. This parameter can't be used in conjunction with @DBSnapshotIdentifier@ . This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBInstance.
--
--
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsDBInstanceIdentifier :: Lens.Lens' DescribeDBSnapshots (Lude.Maybe Lude.Text)
ddsDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: DescribeDBSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: DescribeDBSnapshots)
{-# DEPRECATED ddsDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsMarker :: Lens.Lens' DescribeDBSnapshots (Lude.Maybe Lude.Text)
ddsMarker = Lens.lens (marker :: DescribeDBSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBSnapshots)
{-# DEPRECATED ddsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsMaxRecords :: Lens.Lens' DescribeDBSnapshots (Lude.Maybe Lude.Int)
ddsMaxRecords = Lens.lens (maxRecords :: DescribeDBSnapshots -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBSnapshots)
{-# DEPRECATED ddsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A value that indicates whether to include manual DB cluster snapshots that are public and can be copied or restored by any AWS account. By default, the public snapshots are not included.
--
-- You can share a manual DB snapshot as public by using the 'ModifyDBSnapshotAttribute' API.
--
-- /Note:/ Consider using 'includePublic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsIncludePublic :: Lens.Lens' DescribeDBSnapshots (Lude.Maybe Lude.Bool)
ddsIncludePublic = Lens.lens (includePublic :: DescribeDBSnapshots -> Lude.Maybe Lude.Bool) (\s a -> s {includePublic = a} :: DescribeDBSnapshots)
{-# DEPRECATED ddsIncludePublic "Use generic-lens or generic-optics with 'includePublic' instead." #-}

-- | A specific DB resource ID to describe.
--
-- /Note:/ Consider using 'dbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsDBiResourceId :: Lens.Lens' DescribeDBSnapshots (Lude.Maybe Lude.Text)
ddsDBiResourceId = Lens.lens (dbiResourceId :: DescribeDBSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {dbiResourceId = a} :: DescribeDBSnapshots)
{-# DEPRECATED ddsDBiResourceId "Use generic-lens or generic-optics with 'dbiResourceId' instead." #-}

instance Page.AWSPager DescribeDBSnapshots where
  page rq rs
    | Page.stop (rs Lens.^. ddsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddsrsDBSnapshots) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& ddsMarker Lens..~ rs Lens.^. ddsrsMarker

instance Lude.AWSRequest DescribeDBSnapshots where
  type Rs DescribeDBSnapshots = DescribeDBSnapshotsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBSnapshotsResult"
      ( \s h x ->
          DescribeDBSnapshotsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "DBSnapshots" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "DBSnapshot")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBSnapshots where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBSnapshots where
  toQuery DescribeDBSnapshots' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeDBSnapshots" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "IncludeShared" Lude.=: includeShared,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "DBSnapshotIdentifier" Lude.=: dbSnapshotIdentifier,
        "SnapshotType" Lude.=: snapshotType,
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "IncludePublic" Lude.=: includePublic,
        "DbiResourceId" Lude.=: dbiResourceId
      ]

-- | Contains the result of a successful invocation of the @DescribeDBSnapshots@ action.
--
-- /See:/ 'mkDescribeDBSnapshotsResponse' smart constructor.
data DescribeDBSnapshotsResponse = DescribeDBSnapshotsResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    dbSnapshots ::
      Lude.Maybe [DBSnapshot],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'dbSnapshots' - A list of @DBSnapshot@ instances.
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'responseStatus' - The response status code.
mkDescribeDBSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBSnapshotsResponse
mkDescribeDBSnapshotsResponse pResponseStatus_ =
  DescribeDBSnapshotsResponse'
    { marker = Lude.Nothing,
      dbSnapshots = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrsMarker :: Lens.Lens' DescribeDBSnapshotsResponse (Lude.Maybe Lude.Text)
ddsrsMarker = Lens.lens (marker :: DescribeDBSnapshotsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBSnapshotsResponse)
{-# DEPRECATED ddsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of @DBSnapshot@ instances.
--
-- /Note:/ Consider using 'dbSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrsDBSnapshots :: Lens.Lens' DescribeDBSnapshotsResponse (Lude.Maybe [DBSnapshot])
ddsrsDBSnapshots = Lens.lens (dbSnapshots :: DescribeDBSnapshotsResponse -> Lude.Maybe [DBSnapshot]) (\s a -> s {dbSnapshots = a} :: DescribeDBSnapshotsResponse)
{-# DEPRECATED ddsrsDBSnapshots "Use generic-lens or generic-optics with 'dbSnapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrsResponseStatus :: Lens.Lens' DescribeDBSnapshotsResponse Lude.Int
ddsrsResponseStatus = Lens.lens (responseStatus :: DescribeDBSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBSnapshotsResponse)
{-# DEPRECATED ddsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
