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
-- Module      : Amazonka.RDS.DescribeDBClusters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Amazon Aurora DB clusters and Multi-AZ DB
-- clusters. This API supports pagination.
--
-- For more information on Amazon Aurora DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide/.
--
-- This operation can also return information for Amazon Neptune DB
-- instances and Amazon DocumentDB instances.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribeDBClusters
  ( -- * Creating a Request
    DescribeDBClusters (..),
    newDescribeDBClusters,

    -- * Request Lenses
    describeDBClusters_dbClusterIdentifier,
    describeDBClusters_filters,
    describeDBClusters_includeShared,
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
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeDBClusters' smart constructor.
data DescribeDBClusters = DescribeDBClusters'
  { -- | The user-supplied DB cluster identifier or the Amazon Resource Name
    -- (ARN) of the DB cluster. If this parameter is specified, information
    -- from only the specific DB cluster is returned. This parameter isn\'t
    -- case-sensitive.
    --
    -- Constraints:
    --
    -- -   If supplied, must match an existing DBClusterIdentifier.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A filter that specifies one or more DB clusters to describe.
    --
    -- Supported filters:
    --
    -- -   @clone-group-id@ - Accepts clone group identifiers. The results list
    --     only includes information about the DB clusters associated with
    --     these clone groups.
    --
    -- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
    --     Amazon Resource Names (ARNs). The results list only includes
    --     information about the DB clusters identified by these ARNs.
    --
    -- -   @domain@ - Accepts Active Directory directory IDs. The results list
    --     only includes information about the DB clusters associated with
    --     these domains.
    --
    -- -   @engine@ - Accepts engine names. The results list only includes
    --     information about the DB clusters for these engines.
    filters :: Prelude.Maybe [Filter],
    -- | Optional Boolean parameter that specifies whether the output includes
    -- information about clusters shared from other Amazon Web Services
    -- accounts.
    includeShared :: Prelude.Maybe Prelude.Bool,
    -- | An optional pagination token provided by a previous @DescribeDBClusters@
    -- request. If this parameter is specified, the response includes only
    -- records beyond the marker, up to the value specified by @MaxRecords@.
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
-- Create a value of 'DescribeDBClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterIdentifier', 'describeDBClusters_dbClusterIdentifier' - The user-supplied DB cluster identifier or the Amazon Resource Name
-- (ARN) of the DB cluster. If this parameter is specified, information
-- from only the specific DB cluster is returned. This parameter isn\'t
-- case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match an existing DBClusterIdentifier.
--
-- 'filters', 'describeDBClusters_filters' - A filter that specifies one or more DB clusters to describe.
--
-- Supported filters:
--
-- -   @clone-group-id@ - Accepts clone group identifiers. The results list
--     only includes information about the DB clusters associated with
--     these clone groups.
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs). The results list only includes
--     information about the DB clusters identified by these ARNs.
--
-- -   @domain@ - Accepts Active Directory directory IDs. The results list
--     only includes information about the DB clusters associated with
--     these domains.
--
-- -   @engine@ - Accepts engine names. The results list only includes
--     information about the DB clusters for these engines.
--
-- 'includeShared', 'describeDBClusters_includeShared' - Optional Boolean parameter that specifies whether the output includes
-- information about clusters shared from other Amazon Web Services
-- accounts.
--
-- 'marker', 'describeDBClusters_marker' - An optional pagination token provided by a previous @DescribeDBClusters@
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBClusters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
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
      includeShared = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The user-supplied DB cluster identifier or the Amazon Resource Name
-- (ARN) of the DB cluster. If this parameter is specified, information
-- from only the specific DB cluster is returned. This parameter isn\'t
-- case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match an existing DBClusterIdentifier.
describeDBClusters_dbClusterIdentifier :: Lens.Lens' DescribeDBClusters (Prelude.Maybe Prelude.Text)
describeDBClusters_dbClusterIdentifier = Lens.lens (\DescribeDBClusters' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DescribeDBClusters' {} a -> s {dbClusterIdentifier = a} :: DescribeDBClusters)

-- | A filter that specifies one or more DB clusters to describe.
--
-- Supported filters:
--
-- -   @clone-group-id@ - Accepts clone group identifiers. The results list
--     only includes information about the DB clusters associated with
--     these clone groups.
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs). The results list only includes
--     information about the DB clusters identified by these ARNs.
--
-- -   @domain@ - Accepts Active Directory directory IDs. The results list
--     only includes information about the DB clusters associated with
--     these domains.
--
-- -   @engine@ - Accepts engine names. The results list only includes
--     information about the DB clusters for these engines.
describeDBClusters_filters :: Lens.Lens' DescribeDBClusters (Prelude.Maybe [Filter])
describeDBClusters_filters = Lens.lens (\DescribeDBClusters' {filters} -> filters) (\s@DescribeDBClusters' {} a -> s {filters = a} :: DescribeDBClusters) Prelude.. Lens.mapping Lens.coerced

-- | Optional Boolean parameter that specifies whether the output includes
-- information about clusters shared from other Amazon Web Services
-- accounts.
describeDBClusters_includeShared :: Lens.Lens' DescribeDBClusters (Prelude.Maybe Prelude.Bool)
describeDBClusters_includeShared = Lens.lens (\DescribeDBClusters' {includeShared} -> includeShared) (\s@DescribeDBClusters' {} a -> s {includeShared = a} :: DescribeDBClusters)

-- | An optional pagination token provided by a previous @DescribeDBClusters@
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
describeDBClusters_marker :: Lens.Lens' DescribeDBClusters (Prelude.Maybe Prelude.Text)
describeDBClusters_marker = Lens.lens (\DescribeDBClusters' {marker} -> marker) (\s@DescribeDBClusters' {} a -> s {marker = a} :: DescribeDBClusters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
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
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> ( x
                            Data..@? "DBClusters"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "DBCluster")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBClusters where
  hashWithSalt _salt DescribeDBClusters' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` includeShared
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeDBClusters where
  rnf DescribeDBClusters' {..} =
    Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf includeShared
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
        "IncludeShared" Data.=: includeShared,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBClusters@ action.
--
-- /See:/ 'newDescribeDBClustersResponse' smart constructor.
data DescribeDBClustersResponse = DescribeDBClustersResponse'
  { -- | Contains a list of DB clusters for the user.
    dbClusters :: Prelude.Maybe [DBCluster],
    -- | A pagination token that can be used in a later DescribeDBClusters
    -- request.
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
-- 'dbClusters', 'describeDBClustersResponse_dbClusters' - Contains a list of DB clusters for the user.
--
-- 'marker', 'describeDBClustersResponse_marker' - A pagination token that can be used in a later DescribeDBClusters
-- request.
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

-- | Contains a list of DB clusters for the user.
describeDBClustersResponse_dbClusters :: Lens.Lens' DescribeDBClustersResponse (Prelude.Maybe [DBCluster])
describeDBClustersResponse_dbClusters = Lens.lens (\DescribeDBClustersResponse' {dbClusters} -> dbClusters) (\s@DescribeDBClustersResponse' {} a -> s {dbClusters = a} :: DescribeDBClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that can be used in a later DescribeDBClusters
-- request.
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
