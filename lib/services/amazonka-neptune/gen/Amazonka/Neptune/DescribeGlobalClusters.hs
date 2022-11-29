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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    describeGlobalClusters_marker,
    describeGlobalClusters_maxRecords,
    describeGlobalClusters_globalClusterIdentifier,

    -- * Destructuring the Response
    DescribeGlobalClustersResponse (..),
    newDescribeGlobalClustersResponse,

    -- * Response Lenses
    describeGlobalClustersResponse_marker,
    describeGlobalClustersResponse_globalClusters,
    describeGlobalClustersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGlobalClusters' smart constructor.
data DescribeGlobalClusters = DescribeGlobalClusters'
  { -- | (/Optional/) A pagination token returned by a previous call to
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
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The user-supplied DB cluster identifier. If this parameter is specified,
    -- only information about the specified DB cluster is returned. This
    -- parameter is not case-sensitive.
    --
    -- Constraints: If supplied, must match an existing DB cluster identifier.
    globalClusterIdentifier :: Prelude.Maybe Prelude.Text
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
--
-- 'globalClusterIdentifier', 'describeGlobalClusters_globalClusterIdentifier' - The user-supplied DB cluster identifier. If this parameter is specified,
-- only information about the specified DB cluster is returned. This
-- parameter is not case-sensitive.
--
-- Constraints: If supplied, must match an existing DB cluster identifier.
newDescribeGlobalClusters ::
  DescribeGlobalClusters
newDescribeGlobalClusters =
  DescribeGlobalClusters'
    { marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      globalClusterIdentifier = Prelude.Nothing
    }

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

-- | The user-supplied DB cluster identifier. If this parameter is specified,
-- only information about the specified DB cluster is returned. This
-- parameter is not case-sensitive.
--
-- Constraints: If supplied, must match an existing DB cluster identifier.
describeGlobalClusters_globalClusterIdentifier :: Lens.Lens' DescribeGlobalClusters (Prelude.Maybe Prelude.Text)
describeGlobalClusters_globalClusterIdentifier = Lens.lens (\DescribeGlobalClusters' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@DescribeGlobalClusters' {} a -> s {globalClusterIdentifier = a} :: DescribeGlobalClusters)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeGlobalClustersResult"
      ( \s h x ->
          DescribeGlobalClustersResponse'
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> ( x Core..@? "GlobalClusters" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "GlobalClusterMember")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGlobalClusters where
  hashWithSalt _salt DescribeGlobalClusters' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` globalClusterIdentifier

instance Prelude.NFData DescribeGlobalClusters where
  rnf DescribeGlobalClusters' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf globalClusterIdentifier

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
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "GlobalClusterIdentifier"
          Core.=: globalClusterIdentifier
      ]

-- | /See:/ 'newDescribeGlobalClustersResponse' smart constructor.
data DescribeGlobalClustersResponse = DescribeGlobalClustersResponse'
  { -- | A pagination token. If this parameter is returned in the response, more
    -- records are available, which can be retrieved by one or more additional
    -- calls to @DescribeGlobalClusters@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The list of global clusters and instances returned by this request.
    globalClusters :: Prelude.Maybe [GlobalCluster],
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
-- 'marker', 'describeGlobalClustersResponse_marker' - A pagination token. If this parameter is returned in the response, more
-- records are available, which can be retrieved by one or more additional
-- calls to @DescribeGlobalClusters@.
--
-- 'globalClusters', 'describeGlobalClustersResponse_globalClusters' - The list of global clusters and instances returned by this request.
--
-- 'httpStatus', 'describeGlobalClustersResponse_httpStatus' - The response's http status code.
newDescribeGlobalClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGlobalClustersResponse
newDescribeGlobalClustersResponse pHttpStatus_ =
  DescribeGlobalClustersResponse'
    { marker =
        Prelude.Nothing,
      globalClusters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token. If this parameter is returned in the response, more
-- records are available, which can be retrieved by one or more additional
-- calls to @DescribeGlobalClusters@.
describeGlobalClustersResponse_marker :: Lens.Lens' DescribeGlobalClustersResponse (Prelude.Maybe Prelude.Text)
describeGlobalClustersResponse_marker = Lens.lens (\DescribeGlobalClustersResponse' {marker} -> marker) (\s@DescribeGlobalClustersResponse' {} a -> s {marker = a} :: DescribeGlobalClustersResponse)

-- | The list of global clusters and instances returned by this request.
describeGlobalClustersResponse_globalClusters :: Lens.Lens' DescribeGlobalClustersResponse (Prelude.Maybe [GlobalCluster])
describeGlobalClustersResponse_globalClusters = Lens.lens (\DescribeGlobalClustersResponse' {globalClusters} -> globalClusters) (\s@DescribeGlobalClustersResponse' {} a -> s {globalClusters = a} :: DescribeGlobalClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeGlobalClustersResponse_httpStatus :: Lens.Lens' DescribeGlobalClustersResponse Prelude.Int
describeGlobalClustersResponse_httpStatus = Lens.lens (\DescribeGlobalClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeGlobalClustersResponse' {} a -> s {httpStatus = a} :: DescribeGlobalClustersResponse)

instance
  Prelude.NFData
    DescribeGlobalClustersResponse
  where
  rnf DescribeGlobalClustersResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf globalClusters
      `Prelude.seq` Prelude.rnf httpStatus
