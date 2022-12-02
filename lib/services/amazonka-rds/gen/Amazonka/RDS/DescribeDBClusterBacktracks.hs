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
-- Module      : Amazonka.RDS.DescribeDBClusterBacktracks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about backtracks for a DB cluster.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- This action only applies to Aurora MySQL DB clusters.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribeDBClusterBacktracks
  ( -- * Creating a Request
    DescribeDBClusterBacktracks (..),
    newDescribeDBClusterBacktracks,

    -- * Request Lenses
    describeDBClusterBacktracks_marker,
    describeDBClusterBacktracks_filters,
    describeDBClusterBacktracks_backtrackIdentifier,
    describeDBClusterBacktracks_maxRecords,
    describeDBClusterBacktracks_dbClusterIdentifier,

    -- * Destructuring the Response
    DescribeDBClusterBacktracksResponse (..),
    newDescribeDBClusterBacktracksResponse,

    -- * Response Lenses
    describeDBClusterBacktracksResponse_marker,
    describeDBClusterBacktracksResponse_dbClusterBacktracks,
    describeDBClusterBacktracksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeDBClusterBacktracks' smart constructor.
data DescribeDBClusterBacktracks = DescribeDBClusterBacktracks'
  { -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterBacktracks@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
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
    filters :: Prelude.Maybe [Filter],
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
    backtrackIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
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
    dbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBClusterBacktracks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDBClusterBacktracks_marker' - An optional pagination token provided by a previous
-- @DescribeDBClusterBacktracks@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
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
  Prelude.Text ->
  DescribeDBClusterBacktracks
newDescribeDBClusterBacktracks pDBClusterIdentifier_ =
  DescribeDBClusterBacktracks'
    { marker =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      backtrackIdentifier = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_
    }

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterBacktracks@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBClusterBacktracks_marker :: Lens.Lens' DescribeDBClusterBacktracks (Prelude.Maybe Prelude.Text)
describeDBClusterBacktracks_marker = Lens.lens (\DescribeDBClusterBacktracks' {marker} -> marker) (\s@DescribeDBClusterBacktracks' {} a -> s {marker = a} :: DescribeDBClusterBacktracks)

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
describeDBClusterBacktracks_filters :: Lens.Lens' DescribeDBClusterBacktracks (Prelude.Maybe [Filter])
describeDBClusterBacktracks_filters = Lens.lens (\DescribeDBClusterBacktracks' {filters} -> filters) (\s@DescribeDBClusterBacktracks' {} a -> s {filters = a} :: DescribeDBClusterBacktracks) Prelude.. Lens.mapping Lens.coerced

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
describeDBClusterBacktracks_backtrackIdentifier :: Lens.Lens' DescribeDBClusterBacktracks (Prelude.Maybe Prelude.Text)
describeDBClusterBacktracks_backtrackIdentifier = Lens.lens (\DescribeDBClusterBacktracks' {backtrackIdentifier} -> backtrackIdentifier) (\s@DescribeDBClusterBacktracks' {} a -> s {backtrackIdentifier = a} :: DescribeDBClusterBacktracks)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusterBacktracks_maxRecords :: Lens.Lens' DescribeDBClusterBacktracks (Prelude.Maybe Prelude.Int)
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
describeDBClusterBacktracks_dbClusterIdentifier :: Lens.Lens' DescribeDBClusterBacktracks Prelude.Text
describeDBClusterBacktracks_dbClusterIdentifier = Lens.lens (\DescribeDBClusterBacktracks' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DescribeDBClusterBacktracks' {} a -> s {dbClusterIdentifier = a} :: DescribeDBClusterBacktracks)

instance Core.AWSPager DescribeDBClusterBacktracks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBClusterBacktracksResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBClusterBacktracksResponse_dbClusterBacktracks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDBClusterBacktracks_marker
          Lens..~ rs
          Lens.^? describeDBClusterBacktracksResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBClusterBacktracks where
  type
    AWSResponse DescribeDBClusterBacktracks =
      DescribeDBClusterBacktracksResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterBacktracksResult"
      ( \s h x ->
          DescribeDBClusterBacktracksResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "DBClusterBacktracks"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "DBClusterBacktrack")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBClusterBacktracks where
  hashWithSalt _salt DescribeDBClusterBacktracks' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` backtrackIdentifier
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` dbClusterIdentifier

instance Prelude.NFData DescribeDBClusterBacktracks where
  rnf DescribeDBClusterBacktracks' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf backtrackIdentifier
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf dbClusterIdentifier

instance Data.ToHeaders DescribeDBClusterBacktracks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDBClusterBacktracks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDBClusterBacktracks where
  toQuery DescribeDBClusterBacktracks' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeDBClusterBacktracks" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "BacktrackIdentifier" Data.=: backtrackIdentifier,
        "MaxRecords" Data.=: maxRecords,
        "DBClusterIdentifier" Data.=: dbClusterIdentifier
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBClusterBacktracks@ action.
--
-- /See:/ 'newDescribeDBClusterBacktracksResponse' smart constructor.
data DescribeDBClusterBacktracksResponse = DescribeDBClusterBacktracksResponse'
  { -- | A pagination token that can be used in a later
    -- @DescribeDBClusterBacktracks@ request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Contains a list of backtracks for the user.
    dbClusterBacktracks :: Prelude.Maybe [DBClusterBacktrack],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBClusterBacktracksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDBClusterBacktracksResponse_marker' - A pagination token that can be used in a later
-- @DescribeDBClusterBacktracks@ request.
--
-- 'dbClusterBacktracks', 'describeDBClusterBacktracksResponse_dbClusterBacktracks' - Contains a list of backtracks for the user.
--
-- 'httpStatus', 'describeDBClusterBacktracksResponse_httpStatus' - The response's http status code.
newDescribeDBClusterBacktracksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBClusterBacktracksResponse
newDescribeDBClusterBacktracksResponse pHttpStatus_ =
  DescribeDBClusterBacktracksResponse'
    { marker =
        Prelude.Nothing,
      dbClusterBacktracks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token that can be used in a later
-- @DescribeDBClusterBacktracks@ request.
describeDBClusterBacktracksResponse_marker :: Lens.Lens' DescribeDBClusterBacktracksResponse (Prelude.Maybe Prelude.Text)
describeDBClusterBacktracksResponse_marker = Lens.lens (\DescribeDBClusterBacktracksResponse' {marker} -> marker) (\s@DescribeDBClusterBacktracksResponse' {} a -> s {marker = a} :: DescribeDBClusterBacktracksResponse)

-- | Contains a list of backtracks for the user.
describeDBClusterBacktracksResponse_dbClusterBacktracks :: Lens.Lens' DescribeDBClusterBacktracksResponse (Prelude.Maybe [DBClusterBacktrack])
describeDBClusterBacktracksResponse_dbClusterBacktracks = Lens.lens (\DescribeDBClusterBacktracksResponse' {dbClusterBacktracks} -> dbClusterBacktracks) (\s@DescribeDBClusterBacktracksResponse' {} a -> s {dbClusterBacktracks = a} :: DescribeDBClusterBacktracksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDBClusterBacktracksResponse_httpStatus :: Lens.Lens' DescribeDBClusterBacktracksResponse Prelude.Int
describeDBClusterBacktracksResponse_httpStatus = Lens.lens (\DescribeDBClusterBacktracksResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterBacktracksResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterBacktracksResponse)

instance
  Prelude.NFData
    DescribeDBClusterBacktracksResponse
  where
  rnf DescribeDBClusterBacktracksResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf dbClusterBacktracks
      `Prelude.seq` Prelude.rnf httpStatus
