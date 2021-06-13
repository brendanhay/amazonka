{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB cluster snapshots. This API action supports
-- pagination.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterSnapshots
  ( -- * Creating a Request
    DescribeDBClusterSnapshots (..),
    newDescribeDBClusterSnapshots,

    -- * Request Lenses
    describeDBClusterSnapshots_includeShared,
    describeDBClusterSnapshots_dbClusterIdentifier,
    describeDBClusterSnapshots_snapshotType,
    describeDBClusterSnapshots_filters,
    describeDBClusterSnapshots_dbClusterSnapshotIdentifier,
    describeDBClusterSnapshots_includePublic,
    describeDBClusterSnapshots_marker,
    describeDBClusterSnapshots_maxRecords,

    -- * Destructuring the Response
    DescribeDBClusterSnapshotsResponse (..),
    newDescribeDBClusterSnapshotsResponse,

    -- * Response Lenses
    describeDBClusterSnapshotsResponse_dbClusterSnapshots,
    describeDBClusterSnapshotsResponse_marker,
    describeDBClusterSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBClusterSnapshots' smart constructor.
data DescribeDBClusterSnapshots = DescribeDBClusterSnapshots'
  { -- | A value that indicates whether to include shared manual DB cluster
    -- snapshots from other AWS accounts that this AWS account has been given
    -- permission to copy or restore. By default, these snapshots are not
    -- included.
    --
    -- You can give an AWS account permission to restore a manual DB cluster
    -- snapshot from another AWS account by the
    -- @ModifyDBClusterSnapshotAttribute@ API action.
    includeShared :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the DB cluster to retrieve the list of DB cluster snapshots
    -- for. This parameter can\'t be used in conjunction with the
    -- @DBClusterSnapshotIdentifier@ parameter. This parameter isn\'t
    -- case-sensitive.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the identifier of an existing DBCluster.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The type of DB cluster snapshots to be returned. You can specify one of
    -- the following values:
    --
    -- -   @automated@ - Return all DB cluster snapshots that have been
    --     automatically taken by Amazon RDS for my AWS account.
    --
    -- -   @manual@ - Return all DB cluster snapshots that have been taken by
    --     my AWS account.
    --
    -- -   @shared@ - Return all manual DB cluster snapshots that have been
    --     shared to my AWS account.
    --
    -- -   @public@ - Return all DB cluster snapshots that have been marked as
    --     public.
    --
    -- If you don\'t specify a @SnapshotType@ value, then both automated and
    -- manual DB cluster snapshots are returned. You can include shared DB
    -- cluster snapshots with these results by enabling the @IncludeShared@
    -- parameter. You can include public DB cluster snapshots with these
    -- results by enabling the @IncludePublic@ parameter.
    --
    -- The @IncludeShared@ and @IncludePublic@ parameters don\'t apply for
    -- @SnapshotType@ values of @manual@ or @automated@. The @IncludePublic@
    -- parameter doesn\'t apply when @SnapshotType@ is set to @shared@. The
    -- @IncludeShared@ parameter doesn\'t apply when @SnapshotType@ is set to
    -- @public@.
    snapshotType :: Prelude.Maybe Prelude.Text,
    -- | A filter that specifies one or more DB cluster snapshots to describe.
    --
    -- Supported filters:
    --
    -- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
    --     Amazon Resource Names (ARNs).
    --
    -- -   @db-cluster-snapshot-id@ - Accepts DB cluster snapshot identifiers.
    --
    -- -   @snapshot-type@ - Accepts types of DB cluster snapshots.
    --
    -- -   @engine@ - Accepts names of database engines.
    filters :: Prelude.Maybe [Filter],
    -- | A specific DB cluster snapshot identifier to describe. This parameter
    -- can\'t be used in conjunction with the @DBClusterIdentifier@ parameter.
    -- This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the identifier of an existing
    --     DBClusterSnapshot.
    --
    -- -   If this identifier is for an automated snapshot, the @SnapshotType@
    --     parameter must also be specified.
    dbClusterSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to include manual DB cluster snapshots
    -- that are public and can be copied or restored by any AWS account. By
    -- default, the public snapshots are not included.
    --
    -- You can share a manual DB cluster snapshot as public by using the
    -- ModifyDBClusterSnapshotAttribute API action.
    includePublic :: Prelude.Maybe Prelude.Bool,
    -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterSnapshots@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBClusterSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeShared', 'describeDBClusterSnapshots_includeShared' - A value that indicates whether to include shared manual DB cluster
-- snapshots from other AWS accounts that this AWS account has been given
-- permission to copy or restore. By default, these snapshots are not
-- included.
--
-- You can give an AWS account permission to restore a manual DB cluster
-- snapshot from another AWS account by the
-- @ModifyDBClusterSnapshotAttribute@ API action.
--
-- 'dbClusterIdentifier', 'describeDBClusterSnapshots_dbClusterIdentifier' - The ID of the DB cluster to retrieve the list of DB cluster snapshots
-- for. This parameter can\'t be used in conjunction with the
-- @DBClusterSnapshotIdentifier@ parameter. This parameter isn\'t
-- case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match the identifier of an existing DBCluster.
--
-- 'snapshotType', 'describeDBClusterSnapshots_snapshotType' - The type of DB cluster snapshots to be returned. You can specify one of
-- the following values:
--
-- -   @automated@ - Return all DB cluster snapshots that have been
--     automatically taken by Amazon RDS for my AWS account.
--
-- -   @manual@ - Return all DB cluster snapshots that have been taken by
--     my AWS account.
--
-- -   @shared@ - Return all manual DB cluster snapshots that have been
--     shared to my AWS account.
--
-- -   @public@ - Return all DB cluster snapshots that have been marked as
--     public.
--
-- If you don\'t specify a @SnapshotType@ value, then both automated and
-- manual DB cluster snapshots are returned. You can include shared DB
-- cluster snapshots with these results by enabling the @IncludeShared@
-- parameter. You can include public DB cluster snapshots with these
-- results by enabling the @IncludePublic@ parameter.
--
-- The @IncludeShared@ and @IncludePublic@ parameters don\'t apply for
-- @SnapshotType@ values of @manual@ or @automated@. The @IncludePublic@
-- parameter doesn\'t apply when @SnapshotType@ is set to @shared@. The
-- @IncludeShared@ parameter doesn\'t apply when @SnapshotType@ is set to
-- @public@.
--
-- 'filters', 'describeDBClusterSnapshots_filters' - A filter that specifies one or more DB cluster snapshots to describe.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs).
--
-- -   @db-cluster-snapshot-id@ - Accepts DB cluster snapshot identifiers.
--
-- -   @snapshot-type@ - Accepts types of DB cluster snapshots.
--
-- -   @engine@ - Accepts names of database engines.
--
-- 'dbClusterSnapshotIdentifier', 'describeDBClusterSnapshots_dbClusterSnapshotIdentifier' - A specific DB cluster snapshot identifier to describe. This parameter
-- can\'t be used in conjunction with the @DBClusterIdentifier@ parameter.
-- This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   If supplied, must match the identifier of an existing
--     DBClusterSnapshot.
--
-- -   If this identifier is for an automated snapshot, the @SnapshotType@
--     parameter must also be specified.
--
-- 'includePublic', 'describeDBClusterSnapshots_includePublic' - A value that indicates whether to include manual DB cluster snapshots
-- that are public and can be copied or restored by any AWS account. By
-- default, the public snapshots are not included.
--
-- You can share a manual DB cluster snapshot as public by using the
-- ModifyDBClusterSnapshotAttribute API action.
--
-- 'marker', 'describeDBClusterSnapshots_marker' - An optional pagination token provided by a previous
-- @DescribeDBClusterSnapshots@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBClusterSnapshots_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBClusterSnapshots ::
  DescribeDBClusterSnapshots
newDescribeDBClusterSnapshots =
  DescribeDBClusterSnapshots'
    { includeShared =
        Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      snapshotType = Prelude.Nothing,
      filters = Prelude.Nothing,
      dbClusterSnapshotIdentifier = Prelude.Nothing,
      includePublic = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | A value that indicates whether to include shared manual DB cluster
-- snapshots from other AWS accounts that this AWS account has been given
-- permission to copy or restore. By default, these snapshots are not
-- included.
--
-- You can give an AWS account permission to restore a manual DB cluster
-- snapshot from another AWS account by the
-- @ModifyDBClusterSnapshotAttribute@ API action.
describeDBClusterSnapshots_includeShared :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Bool)
describeDBClusterSnapshots_includeShared = Lens.lens (\DescribeDBClusterSnapshots' {includeShared} -> includeShared) (\s@DescribeDBClusterSnapshots' {} a -> s {includeShared = a} :: DescribeDBClusterSnapshots)

-- | The ID of the DB cluster to retrieve the list of DB cluster snapshots
-- for. This parameter can\'t be used in conjunction with the
-- @DBClusterSnapshotIdentifier@ parameter. This parameter isn\'t
-- case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match the identifier of an existing DBCluster.
describeDBClusterSnapshots_dbClusterIdentifier :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Text)
describeDBClusterSnapshots_dbClusterIdentifier = Lens.lens (\DescribeDBClusterSnapshots' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DescribeDBClusterSnapshots' {} a -> s {dbClusterIdentifier = a} :: DescribeDBClusterSnapshots)

-- | The type of DB cluster snapshots to be returned. You can specify one of
-- the following values:
--
-- -   @automated@ - Return all DB cluster snapshots that have been
--     automatically taken by Amazon RDS for my AWS account.
--
-- -   @manual@ - Return all DB cluster snapshots that have been taken by
--     my AWS account.
--
-- -   @shared@ - Return all manual DB cluster snapshots that have been
--     shared to my AWS account.
--
-- -   @public@ - Return all DB cluster snapshots that have been marked as
--     public.
--
-- If you don\'t specify a @SnapshotType@ value, then both automated and
-- manual DB cluster snapshots are returned. You can include shared DB
-- cluster snapshots with these results by enabling the @IncludeShared@
-- parameter. You can include public DB cluster snapshots with these
-- results by enabling the @IncludePublic@ parameter.
--
-- The @IncludeShared@ and @IncludePublic@ parameters don\'t apply for
-- @SnapshotType@ values of @manual@ or @automated@. The @IncludePublic@
-- parameter doesn\'t apply when @SnapshotType@ is set to @shared@. The
-- @IncludeShared@ parameter doesn\'t apply when @SnapshotType@ is set to
-- @public@.
describeDBClusterSnapshots_snapshotType :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Text)
describeDBClusterSnapshots_snapshotType = Lens.lens (\DescribeDBClusterSnapshots' {snapshotType} -> snapshotType) (\s@DescribeDBClusterSnapshots' {} a -> s {snapshotType = a} :: DescribeDBClusterSnapshots)

-- | A filter that specifies one or more DB cluster snapshots to describe.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs).
--
-- -   @db-cluster-snapshot-id@ - Accepts DB cluster snapshot identifiers.
--
-- -   @snapshot-type@ - Accepts types of DB cluster snapshots.
--
-- -   @engine@ - Accepts names of database engines.
describeDBClusterSnapshots_filters :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe [Filter])
describeDBClusterSnapshots_filters = Lens.lens (\DescribeDBClusterSnapshots' {filters} -> filters) (\s@DescribeDBClusterSnapshots' {} a -> s {filters = a} :: DescribeDBClusterSnapshots) Prelude.. Lens.mapping Lens._Coerce

-- | A specific DB cluster snapshot identifier to describe. This parameter
-- can\'t be used in conjunction with the @DBClusterIdentifier@ parameter.
-- This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   If supplied, must match the identifier of an existing
--     DBClusterSnapshot.
--
-- -   If this identifier is for an automated snapshot, the @SnapshotType@
--     parameter must also be specified.
describeDBClusterSnapshots_dbClusterSnapshotIdentifier :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Text)
describeDBClusterSnapshots_dbClusterSnapshotIdentifier = Lens.lens (\DescribeDBClusterSnapshots' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@DescribeDBClusterSnapshots' {} a -> s {dbClusterSnapshotIdentifier = a} :: DescribeDBClusterSnapshots)

-- | A value that indicates whether to include manual DB cluster snapshots
-- that are public and can be copied or restored by any AWS account. By
-- default, the public snapshots are not included.
--
-- You can share a manual DB cluster snapshot as public by using the
-- ModifyDBClusterSnapshotAttribute API action.
describeDBClusterSnapshots_includePublic :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Bool)
describeDBClusterSnapshots_includePublic = Lens.lens (\DescribeDBClusterSnapshots' {includePublic} -> includePublic) (\s@DescribeDBClusterSnapshots' {} a -> s {includePublic = a} :: DescribeDBClusterSnapshots)

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterSnapshots@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBClusterSnapshots_marker :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Text)
describeDBClusterSnapshots_marker = Lens.lens (\DescribeDBClusterSnapshots' {marker} -> marker) (\s@DescribeDBClusterSnapshots' {} a -> s {marker = a} :: DescribeDBClusterSnapshots)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusterSnapshots_maxRecords :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Int)
describeDBClusterSnapshots_maxRecords = Lens.lens (\DescribeDBClusterSnapshots' {maxRecords} -> maxRecords) (\s@DescribeDBClusterSnapshots' {} a -> s {maxRecords = a} :: DescribeDBClusterSnapshots)

instance Core.AWSPager DescribeDBClusterSnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBClusterSnapshotsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBClusterSnapshotsResponse_dbClusterSnapshots
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDBClusterSnapshots_marker
          Lens..~ rs
          Lens.^? describeDBClusterSnapshotsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBClusterSnapshots where
  type
    AWSResponse DescribeDBClusterSnapshots =
      DescribeDBClusterSnapshotsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterSnapshotsResult"
      ( \s h x ->
          DescribeDBClusterSnapshotsResponse'
            Prelude.<$> ( x Core..@? "DBClusterSnapshots"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "DBClusterSnapshot")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBClusterSnapshots

instance Prelude.NFData DescribeDBClusterSnapshots

instance Core.ToHeaders DescribeDBClusterSnapshots where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeDBClusterSnapshots where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDBClusterSnapshots where
  toQuery DescribeDBClusterSnapshots' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeDBClusterSnapshots" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "IncludeShared" Core.=: includeShared,
        "DBClusterIdentifier" Core.=: dbClusterIdentifier,
        "SnapshotType" Core.=: snapshotType,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "DBClusterSnapshotIdentifier"
          Core.=: dbClusterSnapshotIdentifier,
        "IncludePublic" Core.=: includePublic,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Provides a list of DB cluster snapshots for the user as the result of a
-- call to the @DescribeDBClusterSnapshots@ action.
--
-- /See:/ 'newDescribeDBClusterSnapshotsResponse' smart constructor.
data DescribeDBClusterSnapshotsResponse = DescribeDBClusterSnapshotsResponse'
  { -- | Provides a list of DB cluster snapshots for the user.
    dbClusterSnapshots :: Prelude.Maybe [DBClusterSnapshot],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterSnapshots@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBClusterSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterSnapshots', 'describeDBClusterSnapshotsResponse_dbClusterSnapshots' - Provides a list of DB cluster snapshots for the user.
--
-- 'marker', 'describeDBClusterSnapshotsResponse_marker' - An optional pagination token provided by a previous
-- @DescribeDBClusterSnapshots@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBClusterSnapshotsResponse_httpStatus' - The response's http status code.
newDescribeDBClusterSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBClusterSnapshotsResponse
newDescribeDBClusterSnapshotsResponse pHttpStatus_ =
  DescribeDBClusterSnapshotsResponse'
    { dbClusterSnapshots =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides a list of DB cluster snapshots for the user.
describeDBClusterSnapshotsResponse_dbClusterSnapshots :: Lens.Lens' DescribeDBClusterSnapshotsResponse (Prelude.Maybe [DBClusterSnapshot])
describeDBClusterSnapshotsResponse_dbClusterSnapshots = Lens.lens (\DescribeDBClusterSnapshotsResponse' {dbClusterSnapshots} -> dbClusterSnapshots) (\s@DescribeDBClusterSnapshotsResponse' {} a -> s {dbClusterSnapshots = a} :: DescribeDBClusterSnapshotsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterSnapshots@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBClusterSnapshotsResponse_marker :: Lens.Lens' DescribeDBClusterSnapshotsResponse (Prelude.Maybe Prelude.Text)
describeDBClusterSnapshotsResponse_marker = Lens.lens (\DescribeDBClusterSnapshotsResponse' {marker} -> marker) (\s@DescribeDBClusterSnapshotsResponse' {} a -> s {marker = a} :: DescribeDBClusterSnapshotsResponse)

-- | The response's http status code.
describeDBClusterSnapshotsResponse_httpStatus :: Lens.Lens' DescribeDBClusterSnapshotsResponse Prelude.Int
describeDBClusterSnapshotsResponse_httpStatus = Lens.lens (\DescribeDBClusterSnapshotsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterSnapshotsResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterSnapshotsResponse)

instance
  Prelude.NFData
    DescribeDBClusterSnapshotsResponse
