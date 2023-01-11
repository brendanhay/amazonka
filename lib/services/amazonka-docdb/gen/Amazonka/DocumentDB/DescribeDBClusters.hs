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
-- Module      : Amazonka.DocumentDB.DescribeDBClusters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about provisioned Amazon DocumentDB clusters. This
-- API operation supports pagination. For certain management features such
-- as cluster and instance lifecycle management, Amazon DocumentDB
-- leverages operational technology that is shared with Amazon RDS and
-- Amazon Neptune. Use the @filterName=engine,Values=docdb@ filter
-- parameter to return only Amazon DocumentDB clusters.
--
-- This operation returns paginated results.
module Amazonka.DocumentDB.DescribeDBClusters
  ( -- * Creating a Request
    DescribeDBClusters (..),
    newDescribeDBClusters,

    -- * Request Lenses
    describeDBClusters_dbClusterIdentifier,
    describeDBClusters_filters,
    describeDBClusters_marker,
    describeDBClusters_maxRecords,

    -- * Destructuring the Response
    DescribeDBClustersResponse (..),
    newDescribeDBClustersResponse,

    -- * Response Lenses
    describeDBClustersResponse_dbClusters,
    describeDBClustersResponse_marker,
    describeDBClustersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to DescribeDBClusters.
--
-- /See:/ 'newDescribeDBClusters' smart constructor.
data DescribeDBClusters = DescribeDBClusters'
  { -- | The user-provided cluster identifier. If this parameter is specified,
    -- information from only the specific cluster is returned. This parameter
    -- isn\'t case sensitive.
    --
    -- Constraints:
    --
    -- -   If provided, must match an existing @DBClusterIdentifier@.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A filter that specifies one or more clusters to describe.
    --
    -- Supported filters:
    --
    -- -   @db-cluster-id@ - Accepts cluster identifiers and cluster Amazon
    --     Resource Names (ARNs). The results list only includes information
    --     about the clusters identified by these ARNs.
    filters :: Prelude.Maybe [Filter],
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
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterIdentifier', 'describeDBClusters_dbClusterIdentifier' - The user-provided cluster identifier. If this parameter is specified,
-- information from only the specific cluster is returned. This parameter
-- isn\'t case sensitive.
--
-- Constraints:
--
-- -   If provided, must match an existing @DBClusterIdentifier@.
--
-- 'filters', 'describeDBClusters_filters' - A filter that specifies one or more clusters to describe.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts cluster identifiers and cluster Amazon
--     Resource Names (ARNs). The results list only includes information
--     about the clusters identified by these ARNs.
--
-- 'marker', 'describeDBClusters_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBClusters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- (marker) is included in the response so that the remaining results can
-- be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBClusters ::
  DescribeDBClusters
newDescribeDBClusters =
  DescribeDBClusters'
    { dbClusterIdentifier =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The user-provided cluster identifier. If this parameter is specified,
-- information from only the specific cluster is returned. This parameter
-- isn\'t case sensitive.
--
-- Constraints:
--
-- -   If provided, must match an existing @DBClusterIdentifier@.
describeDBClusters_dbClusterIdentifier :: Lens.Lens' DescribeDBClusters (Prelude.Maybe Prelude.Text)
describeDBClusters_dbClusterIdentifier = Lens.lens (\DescribeDBClusters' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DescribeDBClusters' {} a -> s {dbClusterIdentifier = a} :: DescribeDBClusters)

-- | A filter that specifies one or more clusters to describe.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts cluster identifiers and cluster Amazon
--     Resource Names (ARNs). The results list only includes information
--     about the clusters identified by these ARNs.
describeDBClusters_filters :: Lens.Lens' DescribeDBClusters (Prelude.Maybe [Filter])
describeDBClusters_filters = Lens.lens (\DescribeDBClusters' {filters} -> filters) (\s@DescribeDBClusters' {} a -> s {filters = a} :: DescribeDBClusters) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBClusters_marker :: Lens.Lens' DescribeDBClusters (Prelude.Maybe Prelude.Text)
describeDBClusters_marker = Lens.lens (\DescribeDBClusters' {marker} -> marker) (\s@DescribeDBClusters' {} a -> s {marker = a} :: DescribeDBClusters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- (marker) is included in the response so that the remaining results can
-- be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusters_maxRecords :: Lens.Lens' DescribeDBClusters (Prelude.Maybe Prelude.Int)
describeDBClusters_maxRecords = Lens.lens (\DescribeDBClusters' {maxRecords} -> maxRecords) (\s@DescribeDBClusters' {} a -> s {maxRecords = a} :: DescribeDBClusters)

instance Core.AWSPager DescribeDBClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBClustersResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBClustersResponse_dbClusters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDBClusters_marker
          Lens..~ rs
          Lens.^? describeDBClustersResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBClusters where
  type
    AWSResponse DescribeDBClusters =
      DescribeDBClustersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBClustersResult"
      ( \s h x ->
          DescribeDBClustersResponse'
            Prelude.<$> ( x Data..@? "DBClusters" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "DBCluster")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBClusters where
  hashWithSalt _salt DescribeDBClusters' {..} =
    _salt `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeDBClusters where
  rnf DescribeDBClusters' {..} =
    Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeDBClusters where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDBClusters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDBClusters where
  toQuery DescribeDBClusters' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeDBClusters" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterIdentifier" Data.=: dbClusterIdentifier,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
      ]

-- | Represents the output of DescribeDBClusters.
--
-- /See:/ 'newDescribeDBClustersResponse' smart constructor.
data DescribeDBClustersResponse = DescribeDBClustersResponse'
  { -- | A list of clusters.
    dbClusters :: Prelude.Maybe [DBCluster],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusters', 'describeDBClustersResponse_dbClusters' - A list of clusters.
--
-- 'marker', 'describeDBClustersResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBClustersResponse_httpStatus' - The response's http status code.
newDescribeDBClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBClustersResponse
newDescribeDBClustersResponse pHttpStatus_ =
  DescribeDBClustersResponse'
    { dbClusters =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of clusters.
describeDBClustersResponse_dbClusters :: Lens.Lens' DescribeDBClustersResponse (Prelude.Maybe [DBCluster])
describeDBClustersResponse_dbClusters = Lens.lens (\DescribeDBClustersResponse' {dbClusters} -> dbClusters) (\s@DescribeDBClustersResponse' {} a -> s {dbClusters = a} :: DescribeDBClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBClustersResponse_marker :: Lens.Lens' DescribeDBClustersResponse (Prelude.Maybe Prelude.Text)
describeDBClustersResponse_marker = Lens.lens (\DescribeDBClustersResponse' {marker} -> marker) (\s@DescribeDBClustersResponse' {} a -> s {marker = a} :: DescribeDBClustersResponse)

-- | The response's http status code.
describeDBClustersResponse_httpStatus :: Lens.Lens' DescribeDBClustersResponse Prelude.Int
describeDBClustersResponse_httpStatus = Lens.lens (\DescribeDBClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClustersResponse' {} a -> s {httpStatus = a} :: DescribeDBClustersResponse)

instance Prelude.NFData DescribeDBClustersResponse where
  rnf DescribeDBClustersResponse' {..} =
    Prelude.rnf dbClusters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
