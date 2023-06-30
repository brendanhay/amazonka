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
-- Module      : Amazonka.Neptune.DescribeGlobalClusters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Neptune global database clusters. This API
-- supports pagination.
--
-- This operation returns paginated results.
module Amazonka.Neptune.DescribeGlobalClusters
  ( -- * Creating a Request
    DescribeGlobalClusters (..),
    newDescribeGlobalClusters,

    -- * Request Lenses
    describeGlobalClusters_globalClusterIdentifier,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGlobalClusters' smart constructor.
data DescribeGlobalClusters = DescribeGlobalClusters'
  { -- | The user-supplied DB cluster identifier. If this parameter is specified,
    -- only information about the specified DB cluster is returned. This
    -- parameter is not case-sensitive.
    --
    -- Constraints: If supplied, must match an existing DB cluster identifier.
    globalClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | (/Optional/) A pagination token returned by a previous call to
    -- @DescribeGlobalClusters@. If this parameter is specified, the response
    -- will only include records beyond the marker, up to the number specified
    -- by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination marker
    -- token is included in the response that you can use to retrieve the
    -- remaining results.
    --
    -- Default: @100@
    --
    -- Constraints: Minimum 20, maximum 100.
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
-- 'globalClusterIdentifier', 'describeGlobalClusters_globalClusterIdentifier' - The user-supplied DB cluster identifier. If this parameter is specified,
-- only information about the specified DB cluster is returned. This
-- parameter is not case-sensitive.
--
-- Constraints: If supplied, must match an existing DB cluster identifier.
--
-- 'marker', 'describeGlobalClusters_marker' - (/Optional/) A pagination token returned by a previous call to
-- @DescribeGlobalClusters@. If this parameter is specified, the response
-- will only include records beyond the marker, up to the number specified
-- by @MaxRecords@.
--
-- 'maxRecords', 'describeGlobalClusters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination marker
-- token is included in the response that you can use to retrieve the
-- remaining results.
--
-- Default: @100@
--
-- Constraints: Minimum 20, maximum 100.
newDescribeGlobalClusters ::
  DescribeGlobalClusters
newDescribeGlobalClusters =
  DescribeGlobalClusters'
    { globalClusterIdentifier =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The user-supplied DB cluster identifier. If this parameter is specified,
-- only information about the specified DB cluster is returned. This
-- parameter is not case-sensitive.
--
-- Constraints: If supplied, must match an existing DB cluster identifier.
describeGlobalClusters_globalClusterIdentifier :: Lens.Lens' DescribeGlobalClusters (Prelude.Maybe Prelude.Text)
describeGlobalClusters_globalClusterIdentifier = Lens.lens (\DescribeGlobalClusters' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@DescribeGlobalClusters' {} a -> s {globalClusterIdentifier = a} :: DescribeGlobalClusters)

-- | (/Optional/) A pagination token returned by a previous call to
-- @DescribeGlobalClusters@. If this parameter is specified, the response
-- will only include records beyond the marker, up to the number specified
-- by @MaxRecords@.
describeGlobalClusters_marker :: Lens.Lens' DescribeGlobalClusters (Prelude.Maybe Prelude.Text)
describeGlobalClusters_marker = Lens.lens (\DescribeGlobalClusters' {marker} -> marker) (\s@DescribeGlobalClusters' {} a -> s {marker = a} :: DescribeGlobalClusters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination marker
-- token is included in the response that you can use to retrieve the
-- remaining results.
--
-- Default: @100@
--
-- Constraints: Minimum 20, maximum 100.
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
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeGlobalClusters_marker
          Lens..~ rs
          Lens.^? describeGlobalClustersResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeGlobalClusters where
  type
    AWSResponse DescribeGlobalClusters =
      DescribeGlobalClustersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeGlobalClustersResult"
      ( \s h x ->
          DescribeGlobalClustersResponse'
            Prelude.<$> ( x
                            Data..@? "GlobalClusters"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "GlobalClusterMember")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGlobalClusters where
  hashWithSalt _salt DescribeGlobalClusters' {..} =
    _salt
      `Prelude.hashWithSalt` globalClusterIdentifier
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeGlobalClusters where
  rnf DescribeGlobalClusters' {..} =
    Prelude.rnf globalClusterIdentifier
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeGlobalClusters where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeGlobalClusters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeGlobalClusters where
  toQuery DescribeGlobalClusters' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeGlobalClusters" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "GlobalClusterIdentifier"
          Data.=: globalClusterIdentifier,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
      ]

-- | /See:/ 'newDescribeGlobalClustersResponse' smart constructor.
data DescribeGlobalClustersResponse = DescribeGlobalClustersResponse'
  { -- | The list of global clusters and instances returned by this request.
    globalClusters :: Prelude.Maybe [GlobalCluster],
    -- | A pagination token. If this parameter is returned in the response, more
    -- records are available, which can be retrieved by one or more additional
    -- calls to @DescribeGlobalClusters@.
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
-- 'globalClusters', 'describeGlobalClustersResponse_globalClusters' - The list of global clusters and instances returned by this request.
--
-- 'marker', 'describeGlobalClustersResponse_marker' - A pagination token. If this parameter is returned in the response, more
-- records are available, which can be retrieved by one or more additional
-- calls to @DescribeGlobalClusters@.
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

-- | The list of global clusters and instances returned by this request.
describeGlobalClustersResponse_globalClusters :: Lens.Lens' DescribeGlobalClustersResponse (Prelude.Maybe [GlobalCluster])
describeGlobalClustersResponse_globalClusters = Lens.lens (\DescribeGlobalClustersResponse' {globalClusters} -> globalClusters) (\s@DescribeGlobalClustersResponse' {} a -> s {globalClusters = a} :: DescribeGlobalClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token. If this parameter is returned in the response, more
-- records are available, which can be retrieved by one or more additional
-- calls to @DescribeGlobalClusters@.
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
