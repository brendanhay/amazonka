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
-- Module      : Network.AWS.RDS.DescribeDBClusterBacktracks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about backtracks for a DB cluster.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora MySQL DB clusters.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterBacktracks
  ( -- * Creating a Request
    DescribeDBClusterBacktracks (..),
    newDescribeDBClusterBacktracks,

    -- * Request Lenses
    describeDBClusterBacktracks_backtrackIdentifier,
    describeDBClusterBacktracks_filters,
    describeDBClusterBacktracks_marker,
    describeDBClusterBacktracks_maxRecords,
    describeDBClusterBacktracks_dbClusterIdentifier,

    -- * Destructuring the Response
    DescribeDBClusterBacktracksResponse (..),
    newDescribeDBClusterBacktracksResponse,

    -- * Response Lenses
    describeDBClusterBacktracksResponse_dbClusterBacktracks,
    describeDBClusterBacktracksResponse_marker,
    describeDBClusterBacktracksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBClusterBacktracks' smart constructor.
data DescribeDBClusterBacktracks = DescribeDBClusterBacktracks'
  { -- | If specified, this value is the backtrack identifier of the backtrack to
    -- be described.
    --
    -- Constraints:
    --
    -- -   Must contain a valid universally unique identifier (UUID). For more
    --     information about UUIDs, see
    --     <http://www.ietf.org/rfc/rfc4122.txt A Universally Unique Identifier (UUID) URN Namespace>.
    --
    -- Example: @123e4567-e89b-12d3-a456-426655440000@
    backtrackIdentifier :: Core.Maybe Core.Text,
    -- | A filter that specifies one or more DB clusters to describe. Supported
    -- filters include the following:
    --
    -- -   @db-cluster-backtrack-id@ - Accepts backtrack identifiers. The
    --     results list includes information about only the backtracks
    --     identified by these identifiers.
    --
    -- -   @db-cluster-backtrack-status@ - Accepts any of the following
    --     backtrack status values:
    --
    --     -   @applying@
    --
    --     -   @completed@
    --
    --     -   @failed@
    --
    --     -   @pending@
    --
    --     The results list includes information about only the backtracks
    --     identified by these values.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterBacktracks@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The DB cluster identifier of the DB cluster to be described. This
    -- parameter is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @my-cluster1@
    dbClusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBClusterBacktracks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backtrackIdentifier', 'describeDBClusterBacktracks_backtrackIdentifier' - If specified, this value is the backtrack identifier of the backtrack to
-- be described.
--
-- Constraints:
--
-- -   Must contain a valid universally unique identifier (UUID). For more
--     information about UUIDs, see
--     <http://www.ietf.org/rfc/rfc4122.txt A Universally Unique Identifier (UUID) URN Namespace>.
--
-- Example: @123e4567-e89b-12d3-a456-426655440000@
--
-- 'filters', 'describeDBClusterBacktracks_filters' - A filter that specifies one or more DB clusters to describe. Supported
-- filters include the following:
--
-- -   @db-cluster-backtrack-id@ - Accepts backtrack identifiers. The
--     results list includes information about only the backtracks
--     identified by these identifiers.
--
-- -   @db-cluster-backtrack-status@ - Accepts any of the following
--     backtrack status values:
--
--     -   @applying@
--
--     -   @completed@
--
--     -   @failed@
--
--     -   @pending@
--
--     The results list includes information about only the backtracks
--     identified by these values.
--
-- 'marker', 'describeDBClusterBacktracks_marker' - An optional pagination token provided by a previous
-- @DescribeDBClusterBacktracks@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBClusterBacktracks_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'dbClusterIdentifier', 'describeDBClusterBacktracks_dbClusterIdentifier' - The DB cluster identifier of the DB cluster to be described. This
-- parameter is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
newDescribeDBClusterBacktracks ::
  -- | 'dbClusterIdentifier'
  Core.Text ->
  DescribeDBClusterBacktracks
newDescribeDBClusterBacktracks pDBClusterIdentifier_ =
  DescribeDBClusterBacktracks'
    { backtrackIdentifier =
        Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_
    }

-- | If specified, this value is the backtrack identifier of the backtrack to
-- be described.
--
-- Constraints:
--
-- -   Must contain a valid universally unique identifier (UUID). For more
--     information about UUIDs, see
--     <http://www.ietf.org/rfc/rfc4122.txt A Universally Unique Identifier (UUID) URN Namespace>.
--
-- Example: @123e4567-e89b-12d3-a456-426655440000@
describeDBClusterBacktracks_backtrackIdentifier :: Lens.Lens' DescribeDBClusterBacktracks (Core.Maybe Core.Text)
describeDBClusterBacktracks_backtrackIdentifier = Lens.lens (\DescribeDBClusterBacktracks' {backtrackIdentifier} -> backtrackIdentifier) (\s@DescribeDBClusterBacktracks' {} a -> s {backtrackIdentifier = a} :: DescribeDBClusterBacktracks)

-- | A filter that specifies one or more DB clusters to describe. Supported
-- filters include the following:
--
-- -   @db-cluster-backtrack-id@ - Accepts backtrack identifiers. The
--     results list includes information about only the backtracks
--     identified by these identifiers.
--
-- -   @db-cluster-backtrack-status@ - Accepts any of the following
--     backtrack status values:
--
--     -   @applying@
--
--     -   @completed@
--
--     -   @failed@
--
--     -   @pending@
--
--     The results list includes information about only the backtracks
--     identified by these values.
describeDBClusterBacktracks_filters :: Lens.Lens' DescribeDBClusterBacktracks (Core.Maybe [Filter])
describeDBClusterBacktracks_filters = Lens.lens (\DescribeDBClusterBacktracks' {filters} -> filters) (\s@DescribeDBClusterBacktracks' {} a -> s {filters = a} :: DescribeDBClusterBacktracks) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterBacktracks@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBClusterBacktracks_marker :: Lens.Lens' DescribeDBClusterBacktracks (Core.Maybe Core.Text)
describeDBClusterBacktracks_marker = Lens.lens (\DescribeDBClusterBacktracks' {marker} -> marker) (\s@DescribeDBClusterBacktracks' {} a -> s {marker = a} :: DescribeDBClusterBacktracks)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusterBacktracks_maxRecords :: Lens.Lens' DescribeDBClusterBacktracks (Core.Maybe Core.Int)
describeDBClusterBacktracks_maxRecords = Lens.lens (\DescribeDBClusterBacktracks' {maxRecords} -> maxRecords) (\s@DescribeDBClusterBacktracks' {} a -> s {maxRecords = a} :: DescribeDBClusterBacktracks)

-- | The DB cluster identifier of the DB cluster to be described. This
-- parameter is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
describeDBClusterBacktracks_dbClusterIdentifier :: Lens.Lens' DescribeDBClusterBacktracks Core.Text
describeDBClusterBacktracks_dbClusterIdentifier = Lens.lens (\DescribeDBClusterBacktracks' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DescribeDBClusterBacktracks' {} a -> s {dbClusterIdentifier = a} :: DescribeDBClusterBacktracks)

instance Core.AWSPager DescribeDBClusterBacktracks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBClusterBacktracksResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBClusterBacktracksResponse_dbClusterBacktracks
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBClusterBacktracks_marker
          Lens..~ rs
          Lens.^? describeDBClusterBacktracksResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeDBClusterBacktracks where
  type
    AWSResponse DescribeDBClusterBacktracks =
      DescribeDBClusterBacktracksResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterBacktracksResult"
      ( \s h x ->
          DescribeDBClusterBacktracksResponse'
            Core.<$> ( x Core..@? "DBClusterBacktracks"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "DBClusterBacktrack")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBClusterBacktracks

instance Core.NFData DescribeDBClusterBacktracks

instance Core.ToHeaders DescribeDBClusterBacktracks where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBClusterBacktracks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBClusterBacktracks where
  toQuery DescribeDBClusterBacktracks' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBClusterBacktracks" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "BacktrackIdentifier" Core.=: backtrackIdentifier,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "DBClusterIdentifier" Core.=: dbClusterIdentifier
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBClusterBacktracks@ action.
--
-- /See:/ 'newDescribeDBClusterBacktracksResponse' smart constructor.
data DescribeDBClusterBacktracksResponse = DescribeDBClusterBacktracksResponse'
  { -- | Contains a list of backtracks for the user.
    dbClusterBacktracks :: Core.Maybe [DBClusterBacktrack],
    -- | A pagination token that can be used in a later
    -- @DescribeDBClusterBacktracks@ request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBClusterBacktracksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterBacktracks', 'describeDBClusterBacktracksResponse_dbClusterBacktracks' - Contains a list of backtracks for the user.
--
-- 'marker', 'describeDBClusterBacktracksResponse_marker' - A pagination token that can be used in a later
-- @DescribeDBClusterBacktracks@ request.
--
-- 'httpStatus', 'describeDBClusterBacktracksResponse_httpStatus' - The response's http status code.
newDescribeDBClusterBacktracksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBClusterBacktracksResponse
newDescribeDBClusterBacktracksResponse pHttpStatus_ =
  DescribeDBClusterBacktracksResponse'
    { dbClusterBacktracks =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains a list of backtracks for the user.
describeDBClusterBacktracksResponse_dbClusterBacktracks :: Lens.Lens' DescribeDBClusterBacktracksResponse (Core.Maybe [DBClusterBacktrack])
describeDBClusterBacktracksResponse_dbClusterBacktracks = Lens.lens (\DescribeDBClusterBacktracksResponse' {dbClusterBacktracks} -> dbClusterBacktracks) (\s@DescribeDBClusterBacktracksResponse' {} a -> s {dbClusterBacktracks = a} :: DescribeDBClusterBacktracksResponse) Core.. Lens.mapping Lens._Coerce

-- | A pagination token that can be used in a later
-- @DescribeDBClusterBacktracks@ request.
describeDBClusterBacktracksResponse_marker :: Lens.Lens' DescribeDBClusterBacktracksResponse (Core.Maybe Core.Text)
describeDBClusterBacktracksResponse_marker = Lens.lens (\DescribeDBClusterBacktracksResponse' {marker} -> marker) (\s@DescribeDBClusterBacktracksResponse' {} a -> s {marker = a} :: DescribeDBClusterBacktracksResponse)

-- | The response's http status code.
describeDBClusterBacktracksResponse_httpStatus :: Lens.Lens' DescribeDBClusterBacktracksResponse Core.Int
describeDBClusterBacktracksResponse_httpStatus = Lens.lens (\DescribeDBClusterBacktracksResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterBacktracksResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterBacktracksResponse)

instance
  Core.NFData
    DescribeDBClusterBacktracksResponse
