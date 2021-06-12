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
-- Module      : Network.AWS.RDS.DescribeDBParameterGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBParameterGroup@ descriptions. If a
-- @DBParameterGroupName@ is specified, the list will contain only the
-- description of the specified DB parameter group.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBParameterGroups
  ( -- * Creating a Request
    DescribeDBParameterGroups (..),
    newDescribeDBParameterGroups,

    -- * Request Lenses
    describeDBParameterGroups_dbParameterGroupName,
    describeDBParameterGroups_filters,
    describeDBParameterGroups_marker,
    describeDBParameterGroups_maxRecords,

    -- * Destructuring the Response
    DescribeDBParameterGroupsResponse (..),
    newDescribeDBParameterGroupsResponse,

    -- * Response Lenses
    describeDBParameterGroupsResponse_dbParameterGroups,
    describeDBParameterGroupsResponse_marker,
    describeDBParameterGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBParameterGroups' smart constructor.
data DescribeDBParameterGroups = DescribeDBParameterGroups'
  { -- | The name of a specific DB parameter group to return details for.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing
    --     DBClusterParameterGroup.
    dbParameterGroupName :: Core.Maybe Core.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBParameterGroups@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBParameterGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbParameterGroupName', 'describeDBParameterGroups_dbParameterGroupName' - The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
--
-- 'filters', 'describeDBParameterGroups_filters' - This parameter isn\'t currently supported.
--
-- 'marker', 'describeDBParameterGroups_marker' - An optional pagination token provided by a previous
-- @DescribeDBParameterGroups@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBParameterGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBParameterGroups ::
  DescribeDBParameterGroups
newDescribeDBParameterGroups =
  DescribeDBParameterGroups'
    { dbParameterGroupName =
        Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
describeDBParameterGroups_dbParameterGroupName :: Lens.Lens' DescribeDBParameterGroups (Core.Maybe Core.Text)
describeDBParameterGroups_dbParameterGroupName = Lens.lens (\DescribeDBParameterGroups' {dbParameterGroupName} -> dbParameterGroupName) (\s@DescribeDBParameterGroups' {} a -> s {dbParameterGroupName = a} :: DescribeDBParameterGroups)

-- | This parameter isn\'t currently supported.
describeDBParameterGroups_filters :: Lens.Lens' DescribeDBParameterGroups (Core.Maybe [Filter])
describeDBParameterGroups_filters = Lens.lens (\DescribeDBParameterGroups' {filters} -> filters) (\s@DescribeDBParameterGroups' {} a -> s {filters = a} :: DescribeDBParameterGroups) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeDBParameterGroups@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBParameterGroups_marker :: Lens.Lens' DescribeDBParameterGroups (Core.Maybe Core.Text)
describeDBParameterGroups_marker = Lens.lens (\DescribeDBParameterGroups' {marker} -> marker) (\s@DescribeDBParameterGroups' {} a -> s {marker = a} :: DescribeDBParameterGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBParameterGroups_maxRecords :: Lens.Lens' DescribeDBParameterGroups (Core.Maybe Core.Int)
describeDBParameterGroups_maxRecords = Lens.lens (\DescribeDBParameterGroups' {maxRecords} -> maxRecords) (\s@DescribeDBParameterGroups' {} a -> s {maxRecords = a} :: DescribeDBParameterGroups)

instance Core.AWSPager DescribeDBParameterGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBParameterGroupsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBParameterGroupsResponse_dbParameterGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBParameterGroups_marker
          Lens..~ rs
          Lens.^? describeDBParameterGroupsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeDBParameterGroups where
  type
    AWSResponse DescribeDBParameterGroups =
      DescribeDBParameterGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBParameterGroupsResult"
      ( \s h x ->
          DescribeDBParameterGroupsResponse'
            Core.<$> ( x Core..@? "DBParameterGroups" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "DBParameterGroup")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBParameterGroups

instance Core.NFData DescribeDBParameterGroups

instance Core.ToHeaders DescribeDBParameterGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBParameterGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBParameterGroups where
  toQuery DescribeDBParameterGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBParameterGroups" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBParameterGroupName" Core.=: dbParameterGroupName,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBParameterGroups@ action.
--
-- /See:/ 'newDescribeDBParameterGroupsResponse' smart constructor.
data DescribeDBParameterGroupsResponse = DescribeDBParameterGroupsResponse'
  { -- | A list of @DBParameterGroup@ instances.
    dbParameterGroups :: Core.Maybe [DBParameterGroup],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBParameterGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbParameterGroups', 'describeDBParameterGroupsResponse_dbParameterGroups' - A list of @DBParameterGroup@ instances.
--
-- 'marker', 'describeDBParameterGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBParameterGroupsResponse_httpStatus' - The response's http status code.
newDescribeDBParameterGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBParameterGroupsResponse
newDescribeDBParameterGroupsResponse pHttpStatus_ =
  DescribeDBParameterGroupsResponse'
    { dbParameterGroups =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @DBParameterGroup@ instances.
describeDBParameterGroupsResponse_dbParameterGroups :: Lens.Lens' DescribeDBParameterGroupsResponse (Core.Maybe [DBParameterGroup])
describeDBParameterGroupsResponse_dbParameterGroups = Lens.lens (\DescribeDBParameterGroupsResponse' {dbParameterGroups} -> dbParameterGroups) (\s@DescribeDBParameterGroupsResponse' {} a -> s {dbParameterGroups = a} :: DescribeDBParameterGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBParameterGroupsResponse_marker :: Lens.Lens' DescribeDBParameterGroupsResponse (Core.Maybe Core.Text)
describeDBParameterGroupsResponse_marker = Lens.lens (\DescribeDBParameterGroupsResponse' {marker} -> marker) (\s@DescribeDBParameterGroupsResponse' {} a -> s {marker = a} :: DescribeDBParameterGroupsResponse)

-- | The response's http status code.
describeDBParameterGroupsResponse_httpStatus :: Lens.Lens' DescribeDBParameterGroupsResponse Core.Int
describeDBParameterGroupsResponse_httpStatus = Lens.lens (\DescribeDBParameterGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBParameterGroupsResponse' {} a -> s {httpStatus = a} :: DescribeDBParameterGroupsResponse)

instance
  Core.NFData
    DescribeDBParameterGroupsResponse
