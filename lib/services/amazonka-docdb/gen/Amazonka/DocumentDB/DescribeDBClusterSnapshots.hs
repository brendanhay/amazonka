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
-- Module      : Amazonka.DocumentDB.DescribeDBClusterSnapshots
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about cluster snapshots. This API operation supports
-- pagination.
--
-- This operation returns paginated results.
module Amazonka.DocumentDB.DescribeDBClusterSnapshots
  ( -- * Creating a Request
    DescribeDBClusterSnapshots (..),
    newDescribeDBClusterSnapshots,

    -- * Request Lenses
    describeDBClusterSnapshots_dbClusterIdentifier,
    describeDBClusterSnapshots_dbClusterSnapshotIdentifier,
    describeDBClusterSnapshots_filters,
    describeDBClusterSnapshots_includePublic,
    describeDBClusterSnapshots_includeShared,
    describeDBClusterSnapshots_marker,
    describeDBClusterSnapshots_maxRecords,
    describeDBClusterSnapshots_snapshotType,

    -- * Destructuring the Response
    DescribeDBClusterSnapshotsResponse (..),
    newDescribeDBClusterSnapshotsResponse,

    -- * Response Lenses
    describeDBClusterSnapshotsResponse_dbClusterSnapshots,
    describeDBClusterSnapshotsResponse_marker,
    describeDBClusterSnapshotsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    -- | Set to @true@ to include manual cluster snapshots that are public and
    -- can be copied or restored by any Amazon Web Services account, and
    -- otherwise @false@. The default is @false@.
    includePublic :: Prelude.Maybe Prelude.Bool,
    -- | Set to @true@ to include shared manual cluster snapshots from other
    -- Amazon Web Services accounts that this Amazon Web Services account has
    -- been given permission to copy or restore, and otherwise @false@. The
    -- default is @false@.
    includeShared :: Prelude.Maybe Prelude.Bool,
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
    -- | The type of cluster snapshots to be returned. You can specify one of the
    -- following values:
    --
    -- -   @automated@ - Return all cluster snapshots that Amazon DocumentDB
    --     has automatically created for your Amazon Web Services account.
    --
    -- -   @manual@ - Return all cluster snapshots that you have manually
    --     created for your Amazon Web Services account.
    --
    -- -   @shared@ - Return all manual cluster snapshots that have been shared
    --     to your Amazon Web Services account.
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
    snapshotType :: Prelude.Maybe Prelude.Text
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
-- 'includePublic', 'describeDBClusterSnapshots_includePublic' - Set to @true@ to include manual cluster snapshots that are public and
-- can be copied or restored by any Amazon Web Services account, and
-- otherwise @false@. The default is @false@.
--
-- 'includeShared', 'describeDBClusterSnapshots_includeShared' - Set to @true@ to include shared manual cluster snapshots from other
-- Amazon Web Services accounts that this Amazon Web Services account has
-- been given permission to copy or restore, and otherwise @false@. The
-- default is @false@.
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
-- 'snapshotType', 'describeDBClusterSnapshots_snapshotType' - The type of cluster snapshots to be returned. You can specify one of the
-- following values:
--
-- -   @automated@ - Return all cluster snapshots that Amazon DocumentDB
--     has automatically created for your Amazon Web Services account.
--
-- -   @manual@ - Return all cluster snapshots that you have manually
--     created for your Amazon Web Services account.
--
-- -   @shared@ - Return all manual cluster snapshots that have been shared
--     to your Amazon Web Services account.
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
newDescribeDBClusterSnapshots ::
  DescribeDBClusterSnapshots
newDescribeDBClusterSnapshots =
  DescribeDBClusterSnapshots'
    { dbClusterIdentifier =
        Prelude.Nothing,
      dbClusterSnapshotIdentifier = Prelude.Nothing,
      filters = Prelude.Nothing,
      includePublic = Prelude.Nothing,
      includeShared = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      snapshotType = Prelude.Nothing
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

-- | Set to @true@ to include manual cluster snapshots that are public and
-- can be copied or restored by any Amazon Web Services account, and
-- otherwise @false@. The default is @false@.
describeDBClusterSnapshots_includePublic :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Bool)
describeDBClusterSnapshots_includePublic = Lens.lens (\DescribeDBClusterSnapshots' {includePublic} -> includePublic) (\s@DescribeDBClusterSnapshots' {} a -> s {includePublic = a} :: DescribeDBClusterSnapshots)

-- | Set to @true@ to include shared manual cluster snapshots from other
-- Amazon Web Services accounts that this Amazon Web Services account has
-- been given permission to copy or restore, and otherwise @false@. The
-- default is @false@.
describeDBClusterSnapshots_includeShared :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Bool)
describeDBClusterSnapshots_includeShared = Lens.lens (\DescribeDBClusterSnapshots' {includeShared} -> includeShared) (\s@DescribeDBClusterSnapshots' {} a -> s {includeShared = a} :: DescribeDBClusterSnapshots)

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

-- | The type of cluster snapshots to be returned. You can specify one of the
-- following values:
--
-- -   @automated@ - Return all cluster snapshots that Amazon DocumentDB
--     has automatically created for your Amazon Web Services account.
--
-- -   @manual@ - Return all cluster snapshots that you have manually
--     created for your Amazon Web Services account.
--
-- -   @shared@ - Return all manual cluster snapshots that have been shared
--     to your Amazon Web Services account.
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterSnapshotsResult"
      ( \s h x ->
          DescribeDBClusterSnapshotsResponse'
            Prelude.<$> ( x Data..@? "DBClusterSnapshots"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "DBClusterSnapshot")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBClusterSnapshots where
  hashWithSalt _salt DescribeDBClusterSnapshots' {..} =
    _salt `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` dbClusterSnapshotIdentifier
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` includePublic
      `Prelude.hashWithSalt` includeShared
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` snapshotType

instance Prelude.NFData DescribeDBClusterSnapshots where
  rnf DescribeDBClusterSnapshots' {..} =
    Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf dbClusterSnapshotIdentifier
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf includePublic
      `Prelude.seq` Prelude.rnf includeShared
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf snapshotType

instance Data.ToHeaders DescribeDBClusterSnapshots where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDBClusterSnapshots where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDBClusterSnapshots where
  toQuery DescribeDBClusterSnapshots' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeDBClusterSnapshots" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterIdentifier" Data.=: dbClusterIdentifier,
        "DBClusterSnapshotIdentifier"
          Data.=: dbClusterSnapshotIdentifier,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "IncludePublic" Data.=: includePublic,
        "IncludeShared" Data.=: includeShared,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "SnapshotType" Data.=: snapshotType
      ]

-- | Represents the output of DescribeDBClusterSnapshots.
--
-- /See:/ 'newDescribeDBClusterSnapshotsResponse' smart constructor.
data DescribeDBClusterSnapshotsResponse = DescribeDBClusterSnapshotsResponse'
  { -- | Provides a list of cluster snapshots.
    dbClusterSnapshots :: Prelude.Maybe [DBClusterSnapshot],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
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
-- 'dbClusterSnapshots', 'describeDBClusterSnapshotsResponse_dbClusterSnapshots' - Provides a list of cluster snapshots.
--
-- 'marker', 'describeDBClusterSnapshotsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
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

-- | Provides a list of cluster snapshots.
describeDBClusterSnapshotsResponse_dbClusterSnapshots :: Lens.Lens' DescribeDBClusterSnapshotsResponse (Prelude.Maybe [DBClusterSnapshot])
describeDBClusterSnapshotsResponse_dbClusterSnapshots = Lens.lens (\DescribeDBClusterSnapshotsResponse' {dbClusterSnapshots} -> dbClusterSnapshots) (\s@DescribeDBClusterSnapshotsResponse' {} a -> s {dbClusterSnapshots = a} :: DescribeDBClusterSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBClusterSnapshotsResponse_marker :: Lens.Lens' DescribeDBClusterSnapshotsResponse (Prelude.Maybe Prelude.Text)
describeDBClusterSnapshotsResponse_marker = Lens.lens (\DescribeDBClusterSnapshotsResponse' {marker} -> marker) (\s@DescribeDBClusterSnapshotsResponse' {} a -> s {marker = a} :: DescribeDBClusterSnapshotsResponse)

-- | The response's http status code.
describeDBClusterSnapshotsResponse_httpStatus :: Lens.Lens' DescribeDBClusterSnapshotsResponse Prelude.Int
describeDBClusterSnapshotsResponse_httpStatus = Lens.lens (\DescribeDBClusterSnapshotsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterSnapshotsResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterSnapshotsResponse)

instance
  Prelude.NFData
    DescribeDBClusterSnapshotsResponse
  where
  rnf DescribeDBClusterSnapshotsResponse' {..} =
    Prelude.rnf dbClusterSnapshots
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
