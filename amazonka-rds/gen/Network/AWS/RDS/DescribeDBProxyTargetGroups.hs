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
-- Module      : Network.AWS.RDS.DescribeDBProxyTargetGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB proxy target groups, represented by
-- @DBProxyTargetGroup@ data structures.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBProxyTargetGroups
  ( -- * Creating a Request
    DescribeDBProxyTargetGroups (..),
    newDescribeDBProxyTargetGroups,

    -- * Request Lenses
    describeDBProxyTargetGroups_targetGroupName,
    describeDBProxyTargetGroups_filters,
    describeDBProxyTargetGroups_marker,
    describeDBProxyTargetGroups_maxRecords,
    describeDBProxyTargetGroups_dbProxyName,

    -- * Destructuring the Response
    DescribeDBProxyTargetGroupsResponse (..),
    newDescribeDBProxyTargetGroupsResponse,

    -- * Response Lenses
    describeDBProxyTargetGroupsResponse_targetGroups,
    describeDBProxyTargetGroupsResponse_marker,
    describeDBProxyTargetGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDBProxyTargetGroups' smart constructor.
data DescribeDBProxyTargetGroups = DescribeDBProxyTargetGroups'
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
    -- | The identifier of the @DBProxy@ associated with the target group.
    dbProxyName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBProxyTargetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroupName', 'describeDBProxyTargetGroups_targetGroupName' - The identifier of the @DBProxyTargetGroup@ to describe.
--
-- 'filters', 'describeDBProxyTargetGroups_filters' - This parameter is not currently supported.
--
-- 'marker', 'describeDBProxyTargetGroups_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBProxyTargetGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'dbProxyName', 'describeDBProxyTargetGroups_dbProxyName' - The identifier of the @DBProxy@ associated with the target group.
newDescribeDBProxyTargetGroups ::
  -- | 'dbProxyName'
  Core.Text ->
  DescribeDBProxyTargetGroups
newDescribeDBProxyTargetGroups pDBProxyName_ =
  DescribeDBProxyTargetGroups'
    { targetGroupName =
        Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | The identifier of the @DBProxyTargetGroup@ to describe.
describeDBProxyTargetGroups_targetGroupName :: Lens.Lens' DescribeDBProxyTargetGroups (Core.Maybe Core.Text)
describeDBProxyTargetGroups_targetGroupName = Lens.lens (\DescribeDBProxyTargetGroups' {targetGroupName} -> targetGroupName) (\s@DescribeDBProxyTargetGroups' {} a -> s {targetGroupName = a} :: DescribeDBProxyTargetGroups)

-- | This parameter is not currently supported.
describeDBProxyTargetGroups_filters :: Lens.Lens' DescribeDBProxyTargetGroups (Core.Maybe [Filter])
describeDBProxyTargetGroups_filters = Lens.lens (\DescribeDBProxyTargetGroups' {filters} -> filters) (\s@DescribeDBProxyTargetGroups' {} a -> s {filters = a} :: DescribeDBProxyTargetGroups) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxyTargetGroups_marker :: Lens.Lens' DescribeDBProxyTargetGroups (Core.Maybe Core.Text)
describeDBProxyTargetGroups_marker = Lens.lens (\DescribeDBProxyTargetGroups' {marker} -> marker) (\s@DescribeDBProxyTargetGroups' {} a -> s {marker = a} :: DescribeDBProxyTargetGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBProxyTargetGroups_maxRecords :: Lens.Lens' DescribeDBProxyTargetGroups (Core.Maybe Core.Natural)
describeDBProxyTargetGroups_maxRecords = Lens.lens (\DescribeDBProxyTargetGroups' {maxRecords} -> maxRecords) (\s@DescribeDBProxyTargetGroups' {} a -> s {maxRecords = a} :: DescribeDBProxyTargetGroups)

-- | The identifier of the @DBProxy@ associated with the target group.
describeDBProxyTargetGroups_dbProxyName :: Lens.Lens' DescribeDBProxyTargetGroups Core.Text
describeDBProxyTargetGroups_dbProxyName = Lens.lens (\DescribeDBProxyTargetGroups' {dbProxyName} -> dbProxyName) (\s@DescribeDBProxyTargetGroups' {} a -> s {dbProxyName = a} :: DescribeDBProxyTargetGroups)

instance Core.AWSPager DescribeDBProxyTargetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBProxyTargetGroupsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBProxyTargetGroupsResponse_targetGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBProxyTargetGroups_marker
          Lens..~ rs
          Lens.^? describeDBProxyTargetGroupsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeDBProxyTargetGroups where
  type
    AWSResponse DescribeDBProxyTargetGroups =
      DescribeDBProxyTargetGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBProxyTargetGroupsResult"
      ( \s h x ->
          DescribeDBProxyTargetGroupsResponse'
            Core.<$> ( x Core..@? "TargetGroups" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBProxyTargetGroups

instance Core.NFData DescribeDBProxyTargetGroups

instance Core.ToHeaders DescribeDBProxyTargetGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBProxyTargetGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBProxyTargetGroups where
  toQuery DescribeDBProxyTargetGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBProxyTargetGroups" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "TargetGroupName" Core.=: targetGroupName,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "DBProxyName" Core.=: dbProxyName
      ]

-- | /See:/ 'newDescribeDBProxyTargetGroupsResponse' smart constructor.
data DescribeDBProxyTargetGroupsResponse = DescribeDBProxyTargetGroupsResponse'
  { -- | An arbitrary number of @DBProxyTargetGroup@ objects, containing details
    -- of the corresponding target groups.
    targetGroups :: Core.Maybe [DBProxyTargetGroup],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBProxyTargetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroups', 'describeDBProxyTargetGroupsResponse_targetGroups' - An arbitrary number of @DBProxyTargetGroup@ objects, containing details
-- of the corresponding target groups.
--
-- 'marker', 'describeDBProxyTargetGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBProxyTargetGroupsResponse_httpStatus' - The response's http status code.
newDescribeDBProxyTargetGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBProxyTargetGroupsResponse
newDescribeDBProxyTargetGroupsResponse pHttpStatus_ =
  DescribeDBProxyTargetGroupsResponse'
    { targetGroups =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An arbitrary number of @DBProxyTargetGroup@ objects, containing details
-- of the corresponding target groups.
describeDBProxyTargetGroupsResponse_targetGroups :: Lens.Lens' DescribeDBProxyTargetGroupsResponse (Core.Maybe [DBProxyTargetGroup])
describeDBProxyTargetGroupsResponse_targetGroups = Lens.lens (\DescribeDBProxyTargetGroupsResponse' {targetGroups} -> targetGroups) (\s@DescribeDBProxyTargetGroupsResponse' {} a -> s {targetGroups = a} :: DescribeDBProxyTargetGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxyTargetGroupsResponse_marker :: Lens.Lens' DescribeDBProxyTargetGroupsResponse (Core.Maybe Core.Text)
describeDBProxyTargetGroupsResponse_marker = Lens.lens (\DescribeDBProxyTargetGroupsResponse' {marker} -> marker) (\s@DescribeDBProxyTargetGroupsResponse' {} a -> s {marker = a} :: DescribeDBProxyTargetGroupsResponse)

-- | The response's http status code.
describeDBProxyTargetGroupsResponse_httpStatus :: Lens.Lens' DescribeDBProxyTargetGroupsResponse Core.Int
describeDBProxyTargetGroupsResponse_httpStatus = Lens.lens (\DescribeDBProxyTargetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBProxyTargetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeDBProxyTargetGroupsResponse)

instance
  Core.NFData
    DescribeDBProxyTargetGroupsResponse
