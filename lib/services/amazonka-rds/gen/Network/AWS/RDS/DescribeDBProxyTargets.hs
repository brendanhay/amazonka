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
-- Module      : Network.AWS.RDS.DescribeDBProxyTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about @DBProxyTarget@ objects. This API supports
-- pagination.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBProxyTargets
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
    describeDBProxyTargetsResponse_targets,
    describeDBProxyTargetsResponse_marker,
    describeDBProxyTargetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBProxyTargetsResult"
      ( \s h x ->
          DescribeDBProxyTargetsResponse'
            Prelude.<$> ( x Core..@? "Targets" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBProxyTargets

instance Prelude.NFData DescribeDBProxyTargets

instance Core.ToHeaders DescribeDBProxyTargets where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeDBProxyTargets where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDBProxyTargets where
  toQuery DescribeDBProxyTargets' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeDBProxyTargets" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "TargetGroupName" Core.=: targetGroupName,
        "DBProxyName" Core.=: dbProxyName
      ]

-- | /See:/ 'newDescribeDBProxyTargetsResponse' smart constructor.
data DescribeDBProxyTargetsResponse = DescribeDBProxyTargetsResponse'
  { -- | An arbitrary number of @DBProxyTarget@ objects, containing details of
    -- the corresponding targets.
    targets :: Prelude.Maybe [DBProxyTarget],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
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
-- 'targets', 'describeDBProxyTargetsResponse_targets' - An arbitrary number of @DBProxyTarget@ objects, containing details of
-- the corresponding targets.
--
-- 'marker', 'describeDBProxyTargetsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBProxyTargetsResponse_httpStatus' - The response's http status code.
newDescribeDBProxyTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBProxyTargetsResponse
newDescribeDBProxyTargetsResponse pHttpStatus_ =
  DescribeDBProxyTargetsResponse'
    { targets =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An arbitrary number of @DBProxyTarget@ objects, containing details of
-- the corresponding targets.
describeDBProxyTargetsResponse_targets :: Lens.Lens' DescribeDBProxyTargetsResponse (Prelude.Maybe [DBProxyTarget])
describeDBProxyTargetsResponse_targets = Lens.lens (\DescribeDBProxyTargetsResponse' {targets} -> targets) (\s@DescribeDBProxyTargetsResponse' {} a -> s {targets = a} :: DescribeDBProxyTargetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxyTargetsResponse_marker :: Lens.Lens' DescribeDBProxyTargetsResponse (Prelude.Maybe Prelude.Text)
describeDBProxyTargetsResponse_marker = Lens.lens (\DescribeDBProxyTargetsResponse' {marker} -> marker) (\s@DescribeDBProxyTargetsResponse' {} a -> s {marker = a} :: DescribeDBProxyTargetsResponse)

-- | The response's http status code.
describeDBProxyTargetsResponse_httpStatus :: Lens.Lens' DescribeDBProxyTargetsResponse Prelude.Int
describeDBProxyTargetsResponse_httpStatus = Lens.lens (\DescribeDBProxyTargetsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBProxyTargetsResponse' {} a -> s {httpStatus = a} :: DescribeDBProxyTargetsResponse)

instance
  Prelude.NFData
    DescribeDBProxyTargetsResponse
