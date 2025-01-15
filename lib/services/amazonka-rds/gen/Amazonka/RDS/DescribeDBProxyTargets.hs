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
-- Module      : Amazonka.RDS.DescribeDBProxyTargets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about @DBProxyTarget@ objects. This API supports
-- pagination.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribeDBProxyTargets
  ( -- * Creating a Request
    DescribeDBProxyTargets (..),
    newDescribeDBProxyTargets,

    -- * Request Lenses
    describeDBProxyTargets_filters,
    describeDBProxyTargets_marker,
    describeDBProxyTargets_maxRecords,
    describeDBProxyTargets_targetGroupName,
    describeDBProxyTargets_dbProxyName,

    -- * Destructuring the Response
    DescribeDBProxyTargetsResponse (..),
    newDescribeDBProxyTargetsResponse,

    -- * Response Lenses
    describeDBProxyTargetsResponse_marker,
    describeDBProxyTargetsResponse_targets,
    describeDBProxyTargetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDBProxyTargets' smart constructor.
data DescribeDBProxyTargets = DescribeDBProxyTargets'
  { -- | This parameter is not currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the @DBProxyTargetGroup@ to describe.
    targetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the @DBProxyTarget@ to describe.
    dbProxyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBProxyTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeDBProxyTargets_filters' - This parameter is not currently supported.
--
-- 'marker', 'describeDBProxyTargets_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBProxyTargets_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'targetGroupName', 'describeDBProxyTargets_targetGroupName' - The identifier of the @DBProxyTargetGroup@ to describe.
--
-- 'dbProxyName', 'describeDBProxyTargets_dbProxyName' - The identifier of the @DBProxyTarget@ to describe.
newDescribeDBProxyTargets ::
  -- | 'dbProxyName'
  Prelude.Text ->
  DescribeDBProxyTargets
newDescribeDBProxyTargets pDBProxyName_ =
  DescribeDBProxyTargets'
    { filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      targetGroupName = Prelude.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | This parameter is not currently supported.
describeDBProxyTargets_filters :: Lens.Lens' DescribeDBProxyTargets (Prelude.Maybe [Filter])
describeDBProxyTargets_filters = Lens.lens (\DescribeDBProxyTargets' {filters} -> filters) (\s@DescribeDBProxyTargets' {} a -> s {filters = a} :: DescribeDBProxyTargets) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxyTargets_marker :: Lens.Lens' DescribeDBProxyTargets (Prelude.Maybe Prelude.Text)
describeDBProxyTargets_marker = Lens.lens (\DescribeDBProxyTargets' {marker} -> marker) (\s@DescribeDBProxyTargets' {} a -> s {marker = a} :: DescribeDBProxyTargets)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBProxyTargets_maxRecords :: Lens.Lens' DescribeDBProxyTargets (Prelude.Maybe Prelude.Natural)
describeDBProxyTargets_maxRecords = Lens.lens (\DescribeDBProxyTargets' {maxRecords} -> maxRecords) (\s@DescribeDBProxyTargets' {} a -> s {maxRecords = a} :: DescribeDBProxyTargets)

-- | The identifier of the @DBProxyTargetGroup@ to describe.
describeDBProxyTargets_targetGroupName :: Lens.Lens' DescribeDBProxyTargets (Prelude.Maybe Prelude.Text)
describeDBProxyTargets_targetGroupName = Lens.lens (\DescribeDBProxyTargets' {targetGroupName} -> targetGroupName) (\s@DescribeDBProxyTargets' {} a -> s {targetGroupName = a} :: DescribeDBProxyTargets)

-- | The identifier of the @DBProxyTarget@ to describe.
describeDBProxyTargets_dbProxyName :: Lens.Lens' DescribeDBProxyTargets Prelude.Text
describeDBProxyTargets_dbProxyName = Lens.lens (\DescribeDBProxyTargets' {dbProxyName} -> dbProxyName) (\s@DescribeDBProxyTargets' {} a -> s {dbProxyName = a} :: DescribeDBProxyTargets)

instance Core.AWSPager DescribeDBProxyTargets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBProxyTargetsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBProxyTargetsResponse_targets
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeDBProxyTargets_marker
              Lens..~ rs
              Lens.^? describeDBProxyTargetsResponse_marker
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBProxyTargets where
  type
    AWSResponse DescribeDBProxyTargets =
      DescribeDBProxyTargetsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBProxyTargetsResult"
      ( \s h x ->
          DescribeDBProxyTargetsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "Targets" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBProxyTargets where
  hashWithSalt _salt DescribeDBProxyTargets' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` targetGroupName
      `Prelude.hashWithSalt` dbProxyName

instance Prelude.NFData DescribeDBProxyTargets where
  rnf DescribeDBProxyTargets' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf marker `Prelude.seq`
        Prelude.rnf maxRecords `Prelude.seq`
          Prelude.rnf targetGroupName `Prelude.seq`
            Prelude.rnf dbProxyName

instance Data.ToHeaders DescribeDBProxyTargets where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDBProxyTargets where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDBProxyTargets where
  toQuery DescribeDBProxyTargets' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeDBProxyTargets" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "TargetGroupName" Data.=: targetGroupName,
        "DBProxyName" Data.=: dbProxyName
      ]

-- | /See:/ 'newDescribeDBProxyTargetsResponse' smart constructor.
data DescribeDBProxyTargetsResponse = DescribeDBProxyTargetsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | An arbitrary number of @DBProxyTarget@ objects, containing details of
    -- the corresponding targets.
    targets :: Prelude.Maybe [DBProxyTarget],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBProxyTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDBProxyTargetsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'targets', 'describeDBProxyTargetsResponse_targets' - An arbitrary number of @DBProxyTarget@ objects, containing details of
-- the corresponding targets.
--
-- 'httpStatus', 'describeDBProxyTargetsResponse_httpStatus' - The response's http status code.
newDescribeDBProxyTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBProxyTargetsResponse
newDescribeDBProxyTargetsResponse pHttpStatus_ =
  DescribeDBProxyTargetsResponse'
    { marker =
        Prelude.Nothing,
      targets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxyTargetsResponse_marker :: Lens.Lens' DescribeDBProxyTargetsResponse (Prelude.Maybe Prelude.Text)
describeDBProxyTargetsResponse_marker = Lens.lens (\DescribeDBProxyTargetsResponse' {marker} -> marker) (\s@DescribeDBProxyTargetsResponse' {} a -> s {marker = a} :: DescribeDBProxyTargetsResponse)

-- | An arbitrary number of @DBProxyTarget@ objects, containing details of
-- the corresponding targets.
describeDBProxyTargetsResponse_targets :: Lens.Lens' DescribeDBProxyTargetsResponse (Prelude.Maybe [DBProxyTarget])
describeDBProxyTargetsResponse_targets = Lens.lens (\DescribeDBProxyTargetsResponse' {targets} -> targets) (\s@DescribeDBProxyTargetsResponse' {} a -> s {targets = a} :: DescribeDBProxyTargetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDBProxyTargetsResponse_httpStatus :: Lens.Lens' DescribeDBProxyTargetsResponse Prelude.Int
describeDBProxyTargetsResponse_httpStatus = Lens.lens (\DescribeDBProxyTargetsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBProxyTargetsResponse' {} a -> s {httpStatus = a} :: DescribeDBProxyTargetsResponse)

instance
  Prelude.NFData
    DescribeDBProxyTargetsResponse
  where
  rnf DescribeDBProxyTargetsResponse' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf targets `Prelude.seq`
        Prelude.rnf httpStatus
