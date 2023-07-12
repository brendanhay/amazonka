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
-- Module      : Amazonka.Neptune.DescribeDBClusterSnapshots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB cluster snapshots. This API action supports
-- pagination.
--
-- This operation returns paginated results.
module Amazonka.Neptune.DescribeDBClusterSnapshots
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
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDBClusterSnapshots' smart constructor.
data DescribeDBClusterSnapshots = DescribeDBClusterSnapshots'
  { -- | The ID of the DB cluster to retrieve the list of DB cluster snapshots
    -- for. This parameter can\'t be used in conjunction with the
    -- @DBClusterSnapshotIdentifier@ parameter. This parameter is not
    -- case-sensitive.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the identifier of an existing DBCluster.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
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
    -- | This parameter is not currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | True to include manual DB cluster snapshots that are public and can be
    -- copied or restored by any Amazon account, and otherwise false. The
    -- default is @false@. The default is false.
    --
    -- You can share a manual DB cluster snapshot as public by using the
    -- ModifyDBClusterSnapshotAttribute API action.
    includePublic :: Prelude.Maybe Prelude.Bool,
    -- | True to include shared manual DB cluster snapshots from other Amazon
    -- accounts that this Amazon account has been given permission to copy or
    -- restore, and otherwise false. The default is @false@.
    --
    -- You can give an Amazon account permission to restore a manual DB cluster
    -- snapshot from another Amazon account by the
    -- ModifyDBClusterSnapshotAttribute API action.
    includeShared :: Prelude.Maybe Prelude.Bool,
    -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterSnapshots@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The type of DB cluster snapshots to be returned. You can specify one of
    -- the following values:
    --
    -- -   @automated@ - Return all DB cluster snapshots that have been
    --     automatically taken by Amazon Neptune for my Amazon account.
    --
    -- -   @manual@ - Return all DB cluster snapshots that have been taken by
    --     my Amazon account.
    --
    -- -   @shared@ - Return all manual DB cluster snapshots that have been
    --     shared to my Amazon account.
    --
    -- -   @public@ - Return all DB cluster snapshots that have been marked as
    --     public.
    --
    -- If you don\'t specify a @SnapshotType@ value, then both automated and
    -- manual DB cluster snapshots are returned. You can include shared DB
    -- cluster snapshots with these results by setting the @IncludeShared@
    -- parameter to @true@. You can include public DB cluster snapshots with
    -- these results by setting the @IncludePublic@ parameter to @true@.
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
-- 'dbClusterIdentifier', 'describeDBClusterSnapshots_dbClusterIdentifier' - The ID of the DB cluster to retrieve the list of DB cluster snapshots
-- for. This parameter can\'t be used in conjunction with the
-- @DBClusterSnapshotIdentifier@ parameter. This parameter is not
-- case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match the identifier of an existing DBCluster.
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
-- 'filters', 'describeDBClusterSnapshots_filters' - This parameter is not currently supported.
--
-- 'includePublic', 'describeDBClusterSnapshots_includePublic' - True to include manual DB cluster snapshots that are public and can be
-- copied or restored by any Amazon account, and otherwise false. The
-- default is @false@. The default is false.
--
-- You can share a manual DB cluster snapshot as public by using the
-- ModifyDBClusterSnapshotAttribute API action.
--
-- 'includeShared', 'describeDBClusterSnapshots_includeShared' - True to include shared manual DB cluster snapshots from other Amazon
-- accounts that this Amazon account has been given permission to copy or
-- restore, and otherwise false. The default is @false@.
--
-- You can give an Amazon account permission to restore a manual DB cluster
-- snapshot from another Amazon account by the
-- ModifyDBClusterSnapshotAttribute API action.
--
-- 'marker', 'describeDBClusterSnapshots_marker' - An optional pagination token provided by a previous
-- @DescribeDBClusterSnapshots@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBClusterSnapshots_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'snapshotType', 'describeDBClusterSnapshots_snapshotType' - The type of DB cluster snapshots to be returned. You can specify one of
-- the following values:
--
-- -   @automated@ - Return all DB cluster snapshots that have been
--     automatically taken by Amazon Neptune for my Amazon account.
--
-- -   @manual@ - Return all DB cluster snapshots that have been taken by
--     my Amazon account.
--
-- -   @shared@ - Return all manual DB cluster snapshots that have been
--     shared to my Amazon account.
--
-- -   @public@ - Return all DB cluster snapshots that have been marked as
--     public.
--
-- If you don\'t specify a @SnapshotType@ value, then both automated and
-- manual DB cluster snapshots are returned. You can include shared DB
-- cluster snapshots with these results by setting the @IncludeShared@
-- parameter to @true@. You can include public DB cluster snapshots with
-- these results by setting the @IncludePublic@ parameter to @true@.
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

-- | The ID of the DB cluster to retrieve the list of DB cluster snapshots
-- for. This parameter can\'t be used in conjunction with the
-- @DBClusterSnapshotIdentifier@ parameter. This parameter is not
-- case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match the identifier of an existing DBCluster.
describeDBClusterSnapshots_dbClusterIdentifier :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Text)
describeDBClusterSnapshots_dbClusterIdentifier = Lens.lens (\DescribeDBClusterSnapshots' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DescribeDBClusterSnapshots' {} a -> s {dbClusterIdentifier = a} :: DescribeDBClusterSnapshots)

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

-- | This parameter is not currently supported.
describeDBClusterSnapshots_filters :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe [Filter])
describeDBClusterSnapshots_filters = Lens.lens (\DescribeDBClusterSnapshots' {filters} -> filters) (\s@DescribeDBClusterSnapshots' {} a -> s {filters = a} :: DescribeDBClusterSnapshots) Prelude.. Lens.mapping Lens.coerced

-- | True to include manual DB cluster snapshots that are public and can be
-- copied or restored by any Amazon account, and otherwise false. The
-- default is @false@. The default is false.
--
-- You can share a manual DB cluster snapshot as public by using the
-- ModifyDBClusterSnapshotAttribute API action.
describeDBClusterSnapshots_includePublic :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Bool)
describeDBClusterSnapshots_includePublic = Lens.lens (\DescribeDBClusterSnapshots' {includePublic} -> includePublic) (\s@DescribeDBClusterSnapshots' {} a -> s {includePublic = a} :: DescribeDBClusterSnapshots)

-- | True to include shared manual DB cluster snapshots from other Amazon
-- accounts that this Amazon account has been given permission to copy or
-- restore, and otherwise false. The default is @false@.
--
-- You can give an Amazon account permission to restore a manual DB cluster
-- snapshot from another Amazon account by the
-- ModifyDBClusterSnapshotAttribute API action.
describeDBClusterSnapshots_includeShared :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Bool)
describeDBClusterSnapshots_includeShared = Lens.lens (\DescribeDBClusterSnapshots' {includeShared} -> includeShared) (\s@DescribeDBClusterSnapshots' {} a -> s {includeShared = a} :: DescribeDBClusterSnapshots)

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterSnapshots@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBClusterSnapshots_marker :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Text)
describeDBClusterSnapshots_marker = Lens.lens (\DescribeDBClusterSnapshots' {marker} -> marker) (\s@DescribeDBClusterSnapshots' {} a -> s {marker = a} :: DescribeDBClusterSnapshots)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusterSnapshots_maxRecords :: Lens.Lens' DescribeDBClusterSnapshots (Prelude.Maybe Prelude.Int)
describeDBClusterSnapshots_maxRecords = Lens.lens (\DescribeDBClusterSnapshots' {maxRecords} -> maxRecords) (\s@DescribeDBClusterSnapshots' {} a -> s {maxRecords = a} :: DescribeDBClusterSnapshots)

-- | The type of DB cluster snapshots to be returned. You can specify one of
-- the following values:
--
-- -   @automated@ - Return all DB cluster snapshots that have been
--     automatically taken by Amazon Neptune for my Amazon account.
--
-- -   @manual@ - Return all DB cluster snapshots that have been taken by
--     my Amazon account.
--
-- -   @shared@ - Return all manual DB cluster snapshots that have been
--     shared to my Amazon account.
--
-- -   @public@ - Return all DB cluster snapshots that have been marked as
--     public.
--
-- If you don\'t specify a @SnapshotType@ value, then both automated and
-- manual DB cluster snapshots are returned. You can include shared DB
-- cluster snapshots with these results by setting the @IncludeShared@
-- parameter to @true@. You can include public DB cluster snapshots with
-- these results by setting the @IncludePublic@ parameter to @true@.
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
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> ( x
                            Data..@? "DBClusterSnapshots"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "DBClusterSnapshot")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBClusterSnapshots where
  hashWithSalt _salt DescribeDBClusterSnapshots' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterIdentifier
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

-- | /See:/ 'newDescribeDBClusterSnapshotsResponse' smart constructor.
data DescribeDBClusterSnapshotsResponse = DescribeDBClusterSnapshotsResponse'
  { -- | Provides a list of DB cluster snapshots for the user.
    dbClusterSnapshots :: Prelude.Maybe [DBClusterSnapshot],
    -- | An optional pagination token provided by a previous
    -- DescribeDBClusterSnapshots request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
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
-- DescribeDBClusterSnapshots request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
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
describeDBClusterSnapshotsResponse_dbClusterSnapshots = Lens.lens (\DescribeDBClusterSnapshotsResponse' {dbClusterSnapshots} -> dbClusterSnapshots) (\s@DescribeDBClusterSnapshotsResponse' {} a -> s {dbClusterSnapshots = a} :: DescribeDBClusterSnapshotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- DescribeDBClusterSnapshots request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
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
