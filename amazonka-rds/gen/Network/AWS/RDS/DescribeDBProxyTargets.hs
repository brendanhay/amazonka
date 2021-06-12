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
    describeDBProxyTargets_targetGroupName,
    describeDBProxyTargets_filters,
    describeDBProxyTargets_marker,
    describeDBProxyTargets_maxRecords,
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
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDBProxyTargets' smart constructor.
data DescribeDBProxyTargets = DescribeDBProxyTargets'
  { -- | The identifier of the @DBProxyTargetGroup@ to describe.
    targetGroupName :: Core.Maybe Core.Text,
    -- | This parameter is not currently supported.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Natural,
    -- | The identifier of the @DBProxyTarget@ to describe.
    dbProxyName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBProxyTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroupName', 'describeDBProxyTargets_targetGroupName' - The identifier of the @DBProxyTargetGroup@ to describe.
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
-- 'dbProxyName', 'describeDBProxyTargets_dbProxyName' - The identifier of the @DBProxyTarget@ to describe.
newDescribeDBProxyTargets ::
  -- | 'dbProxyName'
  Core.Text ->
  DescribeDBProxyTargets
newDescribeDBProxyTargets pDBProxyName_ =
  DescribeDBProxyTargets'
    { targetGroupName =
        Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | The identifier of the @DBProxyTargetGroup@ to describe.
describeDBProxyTargets_targetGroupName :: Lens.Lens' DescribeDBProxyTargets (Core.Maybe Core.Text)
describeDBProxyTargets_targetGroupName = Lens.lens (\DescribeDBProxyTargets' {targetGroupName} -> targetGroupName) (\s@DescribeDBProxyTargets' {} a -> s {targetGroupName = a} :: DescribeDBProxyTargets)

-- | This parameter is not currently supported.
describeDBProxyTargets_filters :: Lens.Lens' DescribeDBProxyTargets (Core.Maybe [Filter])
describeDBProxyTargets_filters = Lens.lens (\DescribeDBProxyTargets' {filters} -> filters) (\s@DescribeDBProxyTargets' {} a -> s {filters = a} :: DescribeDBProxyTargets) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxyTargets_marker :: Lens.Lens' DescribeDBProxyTargets (Core.Maybe Core.Text)
describeDBProxyTargets_marker = Lens.lens (\DescribeDBProxyTargets' {marker} -> marker) (\s@DescribeDBProxyTargets' {} a -> s {marker = a} :: DescribeDBProxyTargets)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBProxyTargets_maxRecords :: Lens.Lens' DescribeDBProxyTargets (Core.Maybe Core.Natural)
describeDBProxyTargets_maxRecords = Lens.lens (\DescribeDBProxyTargets' {maxRecords} -> maxRecords) (\s@DescribeDBProxyTargets' {} a -> s {maxRecords = a} :: DescribeDBProxyTargets)

-- | The identifier of the @DBProxyTarget@ to describe.
describeDBProxyTargets_dbProxyName :: Lens.Lens' DescribeDBProxyTargets Core.Text
describeDBProxyTargets_dbProxyName = Lens.lens (\DescribeDBProxyTargets' {dbProxyName} -> dbProxyName) (\s@DescribeDBProxyTargets' {} a -> s {dbProxyName = a} :: DescribeDBProxyTargets)

instance Core.AWSPager DescribeDBProxyTargets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBProxyTargetsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBProxyTargetsResponse_targets
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBProxyTargets_marker
          Lens..~ rs
          Lens.^? describeDBProxyTargetsResponse_marker
            Core.. Lens._Just

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
            Core.<$> ( x Core..@? "Targets" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBProxyTargets

instance Core.NFData DescribeDBProxyTargets

instance Core.ToHeaders DescribeDBProxyTargets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBProxyTargets where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBProxyTargets where
  toQuery DescribeDBProxyTargets' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBProxyTargets" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "TargetGroupName" Core.=: targetGroupName,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "DBProxyName" Core.=: dbProxyName
      ]

-- | /See:/ 'newDescribeDBProxyTargetsResponse' smart constructor.
data DescribeDBProxyTargetsResponse = DescribeDBProxyTargetsResponse'
  { -- | An arbitrary number of @DBProxyTarget@ objects, containing details of
    -- the corresponding targets.
    targets :: Core.Maybe [DBProxyTarget],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeDBProxyTargetsResponse
newDescribeDBProxyTargetsResponse pHttpStatus_ =
  DescribeDBProxyTargetsResponse'
    { targets =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An arbitrary number of @DBProxyTarget@ objects, containing details of
-- the corresponding targets.
describeDBProxyTargetsResponse_targets :: Lens.Lens' DescribeDBProxyTargetsResponse (Core.Maybe [DBProxyTarget])
describeDBProxyTargetsResponse_targets = Lens.lens (\DescribeDBProxyTargetsResponse' {targets} -> targets) (\s@DescribeDBProxyTargetsResponse' {} a -> s {targets = a} :: DescribeDBProxyTargetsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxyTargetsResponse_marker :: Lens.Lens' DescribeDBProxyTargetsResponse (Core.Maybe Core.Text)
describeDBProxyTargetsResponse_marker = Lens.lens (\DescribeDBProxyTargetsResponse' {marker} -> marker) (\s@DescribeDBProxyTargetsResponse' {} a -> s {marker = a} :: DescribeDBProxyTargetsResponse)

-- | The response's http status code.
describeDBProxyTargetsResponse_httpStatus :: Lens.Lens' DescribeDBProxyTargetsResponse Core.Int
describeDBProxyTargetsResponse_httpStatus = Lens.lens (\DescribeDBProxyTargetsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBProxyTargetsResponse' {} a -> s {httpStatus = a} :: DescribeDBProxyTargetsResponse)

instance Core.NFData DescribeDBProxyTargetsResponse
