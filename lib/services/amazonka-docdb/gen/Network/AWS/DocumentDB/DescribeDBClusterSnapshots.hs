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
-- Module      : Network.AWS.DocumentDB.DescribeDBClusterSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about cluster snapshots. This API operation supports
-- pagination.
--
-- This operation returns paginated results.
module Network.AWS.DocumentDB.DescribeDBClusterSnapshots
  ( -- * Creating a Request
    DescribeDBClusterSnapshots (..),
    newDescribeDBClusterSnapshots,

    -- * Request Lenses
    describeDBClusterSnapshots_dbClusterIdentifier,
    describeDBClusterSnapshots_includeShared,
    describeDBClusterSnapshots_dbClusterSnapshotIdentifier,
    describeDBClusterSnapshots_filters,
    describeDBClusterSnapshots_snapshotType,
    describeDBClusterSnapshots_marker,
    describeDBClusterSnapshots_maxRecords,
    describeDBClusterSnapshots_includePublic,

    -- * Destructuring the Response
    DescribeDBClusterSnapshotsResponse (..),
    newDescribeDBClusterSnapshotsResponse,

    -- * Response Lenses
    describeDBClusterSnapshotsResponse_marker,
    describeDBClusterSnapshotsResponse_dbClusterSnapshots,
    describeDBClusterSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DocumentDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input to DescribeDBClusterSnapshots.
--
-- /See:/ 'newDescribeDBClusterSnapshots' smart constructor.
data DescribeDBClusterSnapshots = DescribeDBClusterSnapshots'
  { -- | The ID of the cluster to retrieve the list of cluster snapshots for.
    -- This parameter can\'t be used with the @DBClusterSnapshotIdentifier@
    -- parameter. This parameter is not case sensitive.
    --
    -- Constraints:
    --
    -- -   If provided, must match the identifier of an existing @DBCluster@.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Set to @true@ to include shared manual cluster snapshots from other
    -- accounts that this account has been given permission to copy or restore,
    -- and otherwise @false@. The default is @false@.
    includeShared :: Prelude.Maybe Prelude.Bool,
    -- | A specific cluster snapshot identifier to describe. This parameter
    -- can\'t be used with the @DBClusterIdentifier@ parameter. This value is
    -- stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   If provided, must match the identifier of an existing
    --     @DBClusterSnapshot@.
    --
    -- -   If this identifier is for an automated snapshot, the @SnapshotType@
    --     parameter must also be specified.
    dbClusterSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | This parameter is not currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The type of cluster snapshots to be returned. You can specify one of the
    -- following values:
    --
    -- -   @automated@ - Return all cluster snapshots that Amazon DocumentDB
    --     has automatically created for your account.
    --
    -- -   @manual@ - Return all cluster snapshots that you have manually
    --     created for your account.
    --
    -- -   @shared@ - Return all manual cluster snapshots that have been shared
    --     to your account.
    --
    -- -   @public@ - Return all cluster snapshots that have been marked as
    --     public.
    --
    -- If you don\'t specify a @SnapshotType@ value, then both automated and
    -- manual cluster snapshots are returned. You can include shared cluster
    -- snapshots with these results by setting the @IncludeShared@ parameter to
    -- @true@. You can include public cluster snapshots with these results by
    -- setting the@IncludePublic@ parameter to @true@.
    --
    -- The @IncludeShared@ and @IncludePublic@ parameters don\'t apply for
    -- @SnapshotType@ values of @manual@ or @automated@. The @IncludePublic@
    -- parameter doesn\'t apply when @SnapshotType@ is set to @shared@. The
    -- @IncludeShared@ parameter doesn\'t apply when @SnapshotType@ is set to
    -- @public@.
    snapshotType :: Prelude.Maybe Prelude.Text,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- (marker) is included in the response so that the remaining results can
    -- be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | Set to @true@ to include manual cluster snapshots that are public and
    -- can be copied or restored by any account, and otherwise @false@. The
    -- default is @false@.
    includePublic :: Prelude.Maybe Prelude.Bool
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
-- 'dbClusterIdentifier', 'describeDBClusterSnapshots_dbClusterIdentifier' - The ID of the cluster to retrieve the list of cluster snapshots for.
-- This parameter can\'t be used with the @DBClusterSnapshotIdentifier@
-- parameter. This parameter is not case sensitive.
--
-- Constraints:
--
-- -   If provided, must match the identifier of an existing @DBCluster@.
--
-- 'includeShared', 'describeDBClusterSnapshots_includeShared' - Set to @true@ to include shared manual cluster snapshots from other
-- accounts that this account has been given permission to copy or restore,
-- and otherwise @false@. The default is @false@.
--
-- 'dbClusterSnapshotIdentifier', 'describeDBClusterSnapshots_dbClusterSnapshotIdentifier' - A specific cluster snapshot identifier to describe. This parameter
-- can\'t be used with the @DBClusterIdentifier@ parameter. This value is
-- stored as a lowercase string.
--
-- Constraints:
--
-- -   If provided, must match the identifier of an existing
--     @DBClusterSnapshot@.
--
-- -   If this identifier is for an automated snapshot, the @SnapshotType@
--     parameter must also be specified.
--
-- 'filters', 'describeDBClusterSnapshots_filters' - This parameter is not currently supported.
--
-- 'snapshotType', 'describeDBClusterSnapshots_snapshotType' - The type of cluster snapshots to be returned. You can specify one of the
-- following values:
--
-- -   @automated@ - Return all cluster snapshots that Amazon DocumentDB
--     has automatically created for your account.
--
-- -   @manual@ - Return all cluster snapshots that you have manually
--     created for your account.
--
-- -   @shared@ - Return all manual cluster snapshots that have been shared
--     to your account.
--
-- -   @public@ - Return all cluster snapshots that have been marked as
--     public.
--
-- If you don\'t specify a @SnapshotType@ value, then both automated and
-- manual cluster snapshots are returned. You can include shared cluster
-- snapshots with these results by setting the @IncludeShared@ parameter to
-- @true@. You can include public cluster snapshots with these results by
-- setting the@IncludePublic@ parameter to @true@.
--
-- The @IncludeShared@ and @IncludePublic@ parameters don\'t apply for
-- @SnapshotType@ values of @manual@ or @automated@. The @IncludePublic@
-- parameter doesn\'t apply when @SnapshotType@ is set to @shared@. The
-- @IncludeShared@ parameter doesn\'t apply when @SnapshotType@ is set to
-- @public@.
--
-- 'marker', 'describeDBClusterSnapshots_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBClusterSnapshots_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- (marker) is included in the response so that the remaining results can
-- be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'includePublic', 'describeDBClusterSnapshots_includePublic' - Set to @true@ to include manual cluster snapshots that are public and
-- can be copied or restored by any account, and otherwise @false@. The
-- default is @false@.
newDescribeDBClusterSnapshots ::
  DescribeDBClusterSnapshots
newDescribeDBClusterSnapshots =
  DescribeDBClusterSnapshots'
    { dbClusterIdentifier =
        Prelude.Nothing,
      includeShared = Prelude.Nothing,
      dbClusterSnapshotIdentifier = Prelude.Nothing,
      filters = Prelude.Nothing,
      snapshotType = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      includePublic = Prelude.Nothing
    }

-- | The ID of the cluster to retrieve the list of cluster snapshots for.
-- This parameter can\'t be used with the @DBClusterSnapshotIdentifier@
-- parameter. This parameter is not case sensitive.
--
-- Constraints:
--
-- -   If provided, must match the identifier of an existing @DBCluster@.
describeDBClusterSnapshots_dbClusterIdentifier :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Text)
describeDBClusterSnapshots_dbClusterIdentifier = Lens.lens (\DescribeDBClusterSnapshots' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DescribeDBClusterSnapshots' {} a -> s {dbClusterIdentifier = a} :: DescribeDBClusterSnapshots)

-- | Set to @true@ to include shared manual cluster snapshots from other
-- accounts that this account has been given permission to copy or restore,
-- and otherwise @false@. The default is @false@.
describeDBClusterSnapshots_includeShared :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Bool)
describeDBClusterSnapshots_includeShared = Lens.lens (\DescribeDBClusterSnapshots' {includeShared} -> includeShared) (\s@DescribeDBClusterSnapshots' {} a -> s {includeShared = a} :: DescribeDBClusterSnapshots)

-- | A specific cluster snapshot identifier to describe. This parameter
-- can\'t be used with the @DBClusterIdentifier@ parameter. This value is
-- stored as a lowercase string.
--
-- Constraints:
--
-- -   If provided, must match the identifier of an existing
--     @DBClusterSnapshot@.
--
-- -   If this identifier is for an automated snapshot, the @SnapshotType@
--     parameter must also be specified.
describeDBClusterSnapshots_dbClusterSnapshotIdentifier :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Text)
describeDBClusterSnapshots_dbClusterSnapshotIdentifier = Lens.lens (\DescribeDBClusterSnapshots' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@DescribeDBClusterSnapshots' {} a -> s {dbClusterSnapshotIdentifier = a} :: DescribeDBClusterSnapshots)

-- | This parameter is not currently supported.
describeDBClusterSnapshots_filters :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe [Filter])
describeDBClusterSnapshots_filters = Lens.lens (\DescribeDBClusterSnapshots' {filters} -> filters) (\s@DescribeDBClusterSnapshots' {} a -> s {filters = a} :: DescribeDBClusterSnapshots) Prelude.. Lens.mapping Lens.coerced

-- | The type of cluster snapshots to be returned. You can specify one of the
-- following values:
--
-- -   @automated@ - Return all cluster snapshots that Amazon DocumentDB
--     has automatically created for your account.
--
-- -   @manual@ - Return all cluster snapshots that you have manually
--     created for your account.
--
-- -   @shared@ - Return all manual cluster snapshots that have been shared
--     to your account.
--
-- -   @public@ - Return all cluster snapshots that have been marked as
--     public.
--
-- If you don\'t specify a @SnapshotType@ value, then both automated and
-- manual cluster snapshots are returned. You can include shared cluster
-- snapshots with these results by setting the @IncludeShared@ parameter to
-- @true@. You can include public cluster snapshots with these results by
-- setting the@IncludePublic@ parameter to @true@.
--
-- The @IncludeShared@ and @IncludePublic@ parameters don\'t apply for
-- @SnapshotType@ values of @manual@ or @automated@. The @IncludePublic@
-- parameter doesn\'t apply when @SnapshotType@ is set to @shared@. The
-- @IncludeShared@ parameter doesn\'t apply when @SnapshotType@ is set to
-- @public@.
describeDBClusterSnapshots_snapshotType :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Text)
describeDBClusterSnapshots_snapshotType = Lens.lens (\DescribeDBClusterSnapshots' {snapshotType} -> snapshotType) (\s@DescribeDBClusterSnapshots' {} a -> s {snapshotType = a} :: DescribeDBClusterSnapshots)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBClusterSnapshots_marker :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Text)
describeDBClusterSnapshots_marker = Lens.lens (\DescribeDBClusterSnapshots' {marker} -> marker) (\s@DescribeDBClusterSnapshots' {} a -> s {marker = a} :: DescribeDBClusterSnapshots)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- (marker) is included in the response so that the remaining results can
-- be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusterSnapshots_maxRecords :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Int)
describeDBClusterSnapshots_maxRecords = Lens.lens (\DescribeDBClusterSnapshots' {maxRecords} -> maxRecords) (\s@DescribeDBClusterSnapshots' {} a -> s {maxRecords = a} :: DescribeDBClusterSnapshots)

-- | Set to @true@ to include manual cluster snapshots that are public and
-- can be copied or restored by any account, and otherwise @false@. The
-- default is @false@.
describeDBClusterSnapshots_includePublic :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Bool)
describeDBClusterSnapshots_includePublic = Lens.lens (\DescribeDBClusterSnapshots' {includePublic} -> includePublic) (\s@DescribeDBClusterSnapshots' {} a -> s {includePublic = a} :: DescribeDBClusterSnapshots)

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
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> ( x Core..@? "DBClusterSnapshots"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "DBClusterSnapshot")
                        )
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
        "DBClusterIdentifier" Core.=: dbClusterIdentifier,
        "IncludeShared" Core.=: includeShared,
        "DBClusterSnapshotIdentifier"
          Core.=: dbClusterSnapshotIdentifier,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "SnapshotType" Core.=: snapshotType,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "IncludePublic" Core.=: includePublic
      ]

-- | Represents the output of DescribeDBClusterSnapshots.
--
-- /See:/ 'newDescribeDBClusterSnapshotsResponse' smart constructor.
data DescribeDBClusterSnapshotsResponse = DescribeDBClusterSnapshotsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Provides a list of cluster snapshots.
    dbClusterSnapshots :: Prelude.Maybe [DBClusterSnapshot],
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
-- 'marker', 'describeDBClusterSnapshotsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'dbClusterSnapshots', 'describeDBClusterSnapshotsResponse_dbClusterSnapshots' - Provides a list of cluster snapshots.
--
-- 'httpStatus', 'describeDBClusterSnapshotsResponse_httpStatus' - The response's http status code.
newDescribeDBClusterSnapshotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBClusterSnapshotsResponse
newDescribeDBClusterSnapshotsResponse pHttpStatus_ =
  DescribeDBClusterSnapshotsResponse'
    { marker =
        Prelude.Nothing,
      dbClusterSnapshots = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBClusterSnapshotsResponse_marker :: Lens.Lens' DescribeDBClusterSnapshotsResponse (Prelude.Maybe Prelude.Text)
describeDBClusterSnapshotsResponse_marker = Lens.lens (\DescribeDBClusterSnapshotsResponse' {marker} -> marker) (\s@DescribeDBClusterSnapshotsResponse' {} a -> s {marker = a} :: DescribeDBClusterSnapshotsResponse)

-- | Provides a list of cluster snapshots.
describeDBClusterSnapshotsResponse_dbClusterSnapshots :: Lens.Lens' DescribeDBClusterSnapshotsResponse (Prelude.Maybe [DBClusterSnapshot])
describeDBClusterSnapshotsResponse_dbClusterSnapshots = Lens.lens (\DescribeDBClusterSnapshotsResponse' {dbClusterSnapshots} -> dbClusterSnapshots) (\s@DescribeDBClusterSnapshotsResponse' {} a -> s {dbClusterSnapshots = a} :: DescribeDBClusterSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDBClusterSnapshotsResponse_httpStatus :: Lens.Lens' DescribeDBClusterSnapshotsResponse Prelude.Int
describeDBClusterSnapshotsResponse_httpStatus = Lens.lens (\DescribeDBClusterSnapshotsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterSnapshotsResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterSnapshotsResponse)

instance
  Prelude.NFData
    DescribeDBClusterSnapshotsResponse
