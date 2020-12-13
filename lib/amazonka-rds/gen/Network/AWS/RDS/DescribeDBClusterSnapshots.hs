{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeDBClusterSnapshots (..),
    mkDescribeDBClusterSnapshots,

    -- ** Request lenses
    ddbcsDBClusterIdentifier,
    ddbcsIncludeShared,
    ddbcsDBClusterSnapshotIdentifier,
    ddbcsFilters,
    ddbcsSnapshotType,
    ddbcsMarker,
    ddbcsMaxRecords,
    ddbcsIncludePublic,

    -- * Destructuring the response
    DescribeDBClusterSnapshotsResponse (..),
    mkDescribeDBClusterSnapshotsResponse,

    -- ** Response lenses
    ddbcsrsMarker,
    ddbcsrsDBClusterSnapshots,
    ddbcsrsResponseStatus,
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
-- /See:/ 'mkDescribeDBClusterSnapshots' smart constructor.
data DescribeDBClusterSnapshots = DescribeDBClusterSnapshots'
  { -- | The ID of the DB cluster to retrieve the list of DB cluster snapshots for. This parameter can't be used in conjunction with the @DBClusterSnapshotIdentifier@ parameter. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * If supplied, must match the identifier of an existing DBCluster.
    dbClusterIdentifier :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether to include shared manual DB cluster snapshots from other AWS accounts that this AWS account has been given permission to copy or restore. By default, these snapshots are not included.
    --
    -- You can give an AWS account permission to restore a manual DB cluster snapshot from another AWS account by the @ModifyDBClusterSnapshotAttribute@ API action.
    includeShared :: Lude.Maybe Lude.Bool,
    -- | A specific DB cluster snapshot identifier to describe. This parameter can't be used in conjunction with the @DBClusterIdentifier@ parameter. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * If supplied, must match the identifier of an existing DBClusterSnapshot.
    --
    --
    --     * If this identifier is for an automated snapshot, the @SnapshotType@ parameter must also be specified.
    dbClusterSnapshotIdentifier :: Lude.Maybe Lude.Text,
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
    filters :: Lude.Maybe [Filter],
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
    snapshotType :: Lude.Maybe Lude.Text,
    -- | An optional pagination token provided by a previous @DescribeDBClusterSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
    --
    -- Default: 100
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | A value that indicates whether to include manual DB cluster snapshots that are public and can be copied or restored by any AWS account. By default, the public snapshots are not included.
    --
    -- You can share a manual DB cluster snapshot as public by using the 'ModifyDBClusterSnapshotAttribute' API action.
    includePublic :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBClusterSnapshots' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifier' - The ID of the DB cluster to retrieve the list of DB cluster snapshots for. This parameter can't be used in conjunction with the @DBClusterSnapshotIdentifier@ parameter. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBCluster.
--
--
-- * 'includeShared' - A value that indicates whether to include shared manual DB cluster snapshots from other AWS accounts that this AWS account has been given permission to copy or restore. By default, these snapshots are not included.
--
-- You can give an AWS account permission to restore a manual DB cluster snapshot from another AWS account by the @ModifyDBClusterSnapshotAttribute@ API action.
-- * 'dbClusterSnapshotIdentifier' - A specific DB cluster snapshot identifier to describe. This parameter can't be used in conjunction with the @DBClusterIdentifier@ parameter. This value is stored as a lowercase string.
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBClusterSnapshot.
--
--
--     * If this identifier is for an automated snapshot, the @SnapshotType@ parameter must also be specified.
--
--
-- * 'filters' - A filter that specifies one or more DB cluster snapshots to describe.
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
-- * 'snapshotType' - The type of DB cluster snapshots to be returned. You can specify one of the following values:
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
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBClusterSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'includePublic' - A value that indicates whether to include manual DB cluster snapshots that are public and can be copied or restored by any AWS account. By default, the public snapshots are not included.
--
-- You can share a manual DB cluster snapshot as public by using the 'ModifyDBClusterSnapshotAttribute' API action.
mkDescribeDBClusterSnapshots ::
  DescribeDBClusterSnapshots
mkDescribeDBClusterSnapshots =
  DescribeDBClusterSnapshots'
    { dbClusterIdentifier = Lude.Nothing,
      includeShared = Lude.Nothing,
      dbClusterSnapshotIdentifier = Lude.Nothing,
      filters = Lude.Nothing,
      snapshotType = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      includePublic = Lude.Nothing
    }

-- | The ID of the DB cluster to retrieve the list of DB cluster snapshots for. This parameter can't be used in conjunction with the @DBClusterSnapshotIdentifier@ parameter. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * If supplied, must match the identifier of an existing DBCluster.
--
--
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsDBClusterIdentifier :: Lens.Lens' DescribeDBClusterSnapshots (Lude.Maybe Lude.Text)
ddbcsDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: DescribeDBClusterSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: DescribeDBClusterSnapshots)
{-# DEPRECATED ddbcsDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | A value that indicates whether to include shared manual DB cluster snapshots from other AWS accounts that this AWS account has been given permission to copy or restore. By default, these snapshots are not included.
--
-- You can give an AWS account permission to restore a manual DB cluster snapshot from another AWS account by the @ModifyDBClusterSnapshotAttribute@ API action.
--
-- /Note:/ Consider using 'includeShared' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsIncludeShared :: Lens.Lens' DescribeDBClusterSnapshots (Lude.Maybe Lude.Bool)
ddbcsIncludeShared = Lens.lens (includeShared :: DescribeDBClusterSnapshots -> Lude.Maybe Lude.Bool) (\s a -> s {includeShared = a} :: DescribeDBClusterSnapshots)
{-# DEPRECATED ddbcsIncludeShared "Use generic-lens or generic-optics with 'includeShared' instead." #-}

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
-- /Note:/ Consider using 'dbClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsDBClusterSnapshotIdentifier :: Lens.Lens' DescribeDBClusterSnapshots (Lude.Maybe Lude.Text)
ddbcsDBClusterSnapshotIdentifier = Lens.lens (dbClusterSnapshotIdentifier :: DescribeDBClusterSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterSnapshotIdentifier = a} :: DescribeDBClusterSnapshots)
{-# DEPRECATED ddbcsDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dbClusterSnapshotIdentifier' instead." #-}

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
ddbcsFilters :: Lens.Lens' DescribeDBClusterSnapshots (Lude.Maybe [Filter])
ddbcsFilters = Lens.lens (filters :: DescribeDBClusterSnapshots -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeDBClusterSnapshots)
{-# DEPRECATED ddbcsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

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
ddbcsSnapshotType :: Lens.Lens' DescribeDBClusterSnapshots (Lude.Maybe Lude.Text)
ddbcsSnapshotType = Lens.lens (snapshotType :: DescribeDBClusterSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {snapshotType = a} :: DescribeDBClusterSnapshots)
{-# DEPRECATED ddbcsSnapshotType "Use generic-lens or generic-optics with 'snapshotType' instead." #-}

-- | An optional pagination token provided by a previous @DescribeDBClusterSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsMarker :: Lens.Lens' DescribeDBClusterSnapshots (Lude.Maybe Lude.Text)
ddbcsMarker = Lens.lens (marker :: DescribeDBClusterSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBClusterSnapshots)
{-# DEPRECATED ddbcsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsMaxRecords :: Lens.Lens' DescribeDBClusterSnapshots (Lude.Maybe Lude.Int)
ddbcsMaxRecords = Lens.lens (maxRecords :: DescribeDBClusterSnapshots -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeDBClusterSnapshots)
{-# DEPRECATED ddbcsMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A value that indicates whether to include manual DB cluster snapshots that are public and can be copied or restored by any AWS account. By default, the public snapshots are not included.
--
-- You can share a manual DB cluster snapshot as public by using the 'ModifyDBClusterSnapshotAttribute' API action.
--
-- /Note:/ Consider using 'includePublic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsIncludePublic :: Lens.Lens' DescribeDBClusterSnapshots (Lude.Maybe Lude.Bool)
ddbcsIncludePublic = Lens.lens (includePublic :: DescribeDBClusterSnapshots -> Lude.Maybe Lude.Bool) (\s a -> s {includePublic = a} :: DescribeDBClusterSnapshots)
{-# DEPRECATED ddbcsIncludePublic "Use generic-lens or generic-optics with 'includePublic' instead." #-}

instance Page.AWSPager DescribeDBClusterSnapshots where
  page rq rs
    | Page.stop (rs Lens.^. ddbcsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ddbcsrsDBClusterSnapshots) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddbcsMarker Lens..~ rs Lens.^. ddbcsrsMarker

instance Lude.AWSRequest DescribeDBClusterSnapshots where
  type
    Rs DescribeDBClusterSnapshots =
      DescribeDBClusterSnapshotsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBClusterSnapshotsResult"
      ( \s h x ->
          DescribeDBClusterSnapshotsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "DBClusterSnapshots" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "DBClusterSnapshot")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBClusterSnapshots where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBClusterSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBClusterSnapshots where
  toQuery DescribeDBClusterSnapshots' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeDBClusterSnapshots" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "IncludeShared" Lude.=: includeShared,
        "DBClusterSnapshotIdentifier" Lude.=: dbClusterSnapshotIdentifier,
        "Filters"
          Lude.=: Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "SnapshotType" Lude.=: snapshotType,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "IncludePublic" Lude.=: includePublic
      ]

-- | Provides a list of DB cluster snapshots for the user as the result of a call to the @DescribeDBClusterSnapshots@ action.
--
-- /See:/ 'mkDescribeDBClusterSnapshotsResponse' smart constructor.
data DescribeDBClusterSnapshotsResponse = DescribeDBClusterSnapshotsResponse'
  { -- | An optional pagination token provided by a previous @DescribeDBClusterSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | Provides a list of DB cluster snapshots for the user.
    dbClusterSnapshots :: Lude.Maybe [DBClusterSnapshot],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBClusterSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous @DescribeDBClusterSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'dbClusterSnapshots' - Provides a list of DB cluster snapshots for the user.
-- * 'responseStatus' - The response status code.
mkDescribeDBClusterSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBClusterSnapshotsResponse
mkDescribeDBClusterSnapshotsResponse pResponseStatus_ =
  DescribeDBClusterSnapshotsResponse'
    { marker = Lude.Nothing,
      dbClusterSnapshots = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous @DescribeDBClusterSnapshots@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsrsMarker :: Lens.Lens' DescribeDBClusterSnapshotsResponse (Lude.Maybe Lude.Text)
ddbcsrsMarker = Lens.lens (marker :: DescribeDBClusterSnapshotsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeDBClusterSnapshotsResponse)
{-# DEPRECATED ddbcsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Provides a list of DB cluster snapshots for the user.
--
-- /Note:/ Consider using 'dbClusterSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsrsDBClusterSnapshots :: Lens.Lens' DescribeDBClusterSnapshotsResponse (Lude.Maybe [DBClusterSnapshot])
ddbcsrsDBClusterSnapshots = Lens.lens (dbClusterSnapshots :: DescribeDBClusterSnapshotsResponse -> Lude.Maybe [DBClusterSnapshot]) (\s a -> s {dbClusterSnapshots = a} :: DescribeDBClusterSnapshotsResponse)
{-# DEPRECATED ddbcsrsDBClusterSnapshots "Use generic-lens or generic-optics with 'dbClusterSnapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsrsResponseStatus :: Lens.Lens' DescribeDBClusterSnapshotsResponse Lude.Int
ddbcsrsResponseStatus = Lens.lens (responseStatus :: DescribeDBClusterSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBClusterSnapshotsResponse)
{-# DEPRECATED ddbcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
