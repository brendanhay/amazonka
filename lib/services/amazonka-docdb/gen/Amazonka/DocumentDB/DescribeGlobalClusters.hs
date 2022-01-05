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
-- Module      : Amazonka.DocumentDB.DescribeGlobalClusters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Amazon DocumentDB global clusters. This API
-- supports pagination.
--
-- This action only applies to Amazon DocumentDB clusters.
--
-- This operation returns paginated results.
module Amazonka.DocumentDB.DescribeGlobalClusters
  ( -- * Creating a Request
    DescribeGlobalClusters (..),
    newDescribeGlobalClusters,

    -- * Request Lenses
    describeGlobalClusters_globalClusterIdentifier,
    describeGlobalClusters_filters,
    describeGlobalClusters_marker,
    describeGlobalClusters_maxRecords,

    -- * Destructuring the Response
    DescribeGlobalClustersResponse (..),
    newDescribeGlobalClustersResponse,

    -- * Response Lenses
    describeGlobalClustersResponse_globalClusters,
    describeGlobalClustersResponse_marker,
    describeGlobalClustersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DocumentDB.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGlobalClusters' smart constructor.
data DescribeGlobalClusters = DescribeGlobalClusters'
  { -- | The user-supplied cluster identifier. If this parameter is specified,
    -- information from only the specific cluster is returned. This parameter
    -- isn\'t case-sensitive.
    globalClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A filter that specifies one or more global DB clusters to describe.
    --
    -- Supported filters: @db-cluster-id@ accepts cluster identifiers and
    -- cluster Amazon Resource Names (ARNs). The results list will only include
    -- information about the clusters identified by these ARNs.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeGlobalClusters@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGlobalClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalClusterIdentifier', 'describeGlobalClusters_globalClusterIdentifier' - The user-supplied cluster identifier. If this parameter is specified,
-- information from only the specific cluster is returned. This parameter
-- isn\'t case-sensitive.
--
-- 'filters', 'describeGlobalClusters_filters' - A filter that specifies one or more global DB clusters to describe.
--
-- Supported filters: @db-cluster-id@ accepts cluster identifiers and
-- cluster Amazon Resource Names (ARNs). The results list will only include
-- information about the clusters identified by these ARNs.
--
-- 'marker', 'describeGlobalClusters_marker' - An optional pagination token provided by a previous
-- @DescribeGlobalClusters@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeGlobalClusters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
newDescribeGlobalClusters ::
  DescribeGlobalClusters
newDescribeGlobalClusters =
  DescribeGlobalClusters'
    { globalClusterIdentifier =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The user-supplied cluster identifier. If this parameter is specified,
-- information from only the specific cluster is returned. This parameter
-- isn\'t case-sensitive.
describeGlobalClusters_globalClusterIdentifier :: Lens.Lens' DescribeGlobalClusters (Prelude.Maybe Prelude.Text)
describeGlobalClusters_globalClusterIdentifier = Lens.lens (\DescribeGlobalClusters' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@DescribeGlobalClusters' {} a -> s {globalClusterIdentifier = a} :: DescribeGlobalClusters)

-- | A filter that specifies one or more global DB clusters to describe.
--
-- Supported filters: @db-cluster-id@ accepts cluster identifiers and
-- cluster Amazon Resource Names (ARNs). The results list will only include
-- information about the clusters identified by these ARNs.
describeGlobalClusters_filters :: Lens.Lens' DescribeGlobalClusters (Prelude.Maybe [Filter])
describeGlobalClusters_filters = Lens.lens (\DescribeGlobalClusters' {filters} -> filters) (\s@DescribeGlobalClusters' {} a -> s {filters = a} :: DescribeGlobalClusters) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- @DescribeGlobalClusters@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeGlobalClusters_marker :: Lens.Lens' DescribeGlobalClusters (Prelude.Maybe Prelude.Text)
describeGlobalClusters_marker = Lens.lens (\DescribeGlobalClusters' {marker} -> marker) (\s@DescribeGlobalClusters' {} a -> s {marker = a} :: DescribeGlobalClusters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
describeGlobalClusters_maxRecords :: Lens.Lens' DescribeGlobalClusters (Prelude.Maybe Prelude.Int)
describeGlobalClusters_maxRecords = Lens.lens (\DescribeGlobalClusters' {maxRecords} -> maxRecords) (\s@DescribeGlobalClusters' {} a -> s {maxRecords = a} :: DescribeGlobalClusters)

instance Core.AWSPager DescribeGlobalClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeGlobalClustersResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeGlobalClustersResponse_globalClusters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeGlobalClusters_marker
          Lens..~ rs
          Lens.^? describeGlobalClustersResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeGlobalClusters where
  type
    AWSResponse DescribeGlobalClusters =
      DescribeGlobalClustersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeGlobalClustersResult"
      ( \s h x ->
          DescribeGlobalClustersResponse'
            Prelude.<$> ( x Core..@? "GlobalClusters" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "GlobalClusterMember")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGlobalClusters where
  hashWithSalt _salt DescribeGlobalClusters' {..} =
    _salt
      `Prelude.hashWithSalt` globalClusterIdentifier
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeGlobalClusters where
  rnf DescribeGlobalClusters' {..} =
    Prelude.rnf globalClusterIdentifier
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Core.ToHeaders DescribeGlobalClusters where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeGlobalClusters where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeGlobalClusters where
  toQuery DescribeGlobalClusters' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeGlobalClusters" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "GlobalClusterIdentifier"
          Core.=: globalClusterIdentifier,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeGlobalClustersResponse' smart constructor.
data DescribeGlobalClustersResponse = DescribeGlobalClustersResponse'
  { globalClusters :: Prelude.Maybe [GlobalCluster],
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGlobalClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalClusters', 'describeGlobalClustersResponse_globalClusters' -
--
-- 'marker', 'describeGlobalClustersResponse_marker' -
--
-- 'httpStatus', 'describeGlobalClustersResponse_httpStatus' - The response's http status code.
newDescribeGlobalClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGlobalClustersResponse
newDescribeGlobalClustersResponse pHttpStatus_ =
  DescribeGlobalClustersResponse'
    { globalClusters =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
describeGlobalClustersResponse_globalClusters :: Lens.Lens' DescribeGlobalClustersResponse (Prelude.Maybe [GlobalCluster])
describeGlobalClustersResponse_globalClusters = Lens.lens (\DescribeGlobalClustersResponse' {globalClusters} -> globalClusters) (\s@DescribeGlobalClustersResponse' {} a -> s {globalClusters = a} :: DescribeGlobalClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- |
describeGlobalClustersResponse_marker :: Lens.Lens' DescribeGlobalClustersResponse (Prelude.Maybe Prelude.Text)
describeGlobalClustersResponse_marker = Lens.lens (\DescribeGlobalClustersResponse' {marker} -> marker) (\s@DescribeGlobalClustersResponse' {} a -> s {marker = a} :: DescribeGlobalClustersResponse)

-- | The response's http status code.
describeGlobalClustersResponse_httpStatus :: Lens.Lens' DescribeGlobalClustersResponse Prelude.Int
describeGlobalClustersResponse_httpStatus = Lens.lens (\DescribeGlobalClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeGlobalClustersResponse' {} a -> s {httpStatus = a} :: DescribeGlobalClustersResponse)

instance
  Prelude.NFData
    DescribeGlobalClustersResponse
  where
  rnf DescribeGlobalClustersResponse' {..} =
    Prelude.rnf globalClusters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
