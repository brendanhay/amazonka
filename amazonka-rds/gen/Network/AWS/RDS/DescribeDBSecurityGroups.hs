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
-- Module      : Network.AWS.RDS.DescribeDBSecurityGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBSecurityGroup@ descriptions. If a
-- @DBSecurityGroupName@ is specified, the list will contain only the
-- descriptions of the specified DB security group.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBSecurityGroups
  ( -- * Creating a Request
    DescribeDBSecurityGroups (..),
    newDescribeDBSecurityGroups,

    -- * Request Lenses
    describeDBSecurityGroups_dbSecurityGroupName,
    describeDBSecurityGroups_filters,
    describeDBSecurityGroups_marker,
    describeDBSecurityGroups_maxRecords,

    -- * Destructuring the Response
    DescribeDBSecurityGroupsResponse (..),
    newDescribeDBSecurityGroupsResponse,

    -- * Response Lenses
    describeDBSecurityGroupsResponse_dbSecurityGroups,
    describeDBSecurityGroupsResponse_marker,
    describeDBSecurityGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBSecurityGroups' smart constructor.
data DescribeDBSecurityGroups = DescribeDBSecurityGroups'
  { -- | The name of the DB security group to return details for.
    dbSecurityGroupName :: Core.Maybe Core.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBSecurityGroups@ request. If this parameter is specified, the
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
-- Create a value of 'DescribeDBSecurityGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSecurityGroupName', 'describeDBSecurityGroups_dbSecurityGroupName' - The name of the DB security group to return details for.
--
-- 'filters', 'describeDBSecurityGroups_filters' - This parameter isn\'t currently supported.
--
-- 'marker', 'describeDBSecurityGroups_marker' - An optional pagination token provided by a previous
-- @DescribeDBSecurityGroups@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBSecurityGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBSecurityGroups ::
  DescribeDBSecurityGroups
newDescribeDBSecurityGroups =
  DescribeDBSecurityGroups'
    { dbSecurityGroupName =
        Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of the DB security group to return details for.
describeDBSecurityGroups_dbSecurityGroupName :: Lens.Lens' DescribeDBSecurityGroups (Core.Maybe Core.Text)
describeDBSecurityGroups_dbSecurityGroupName = Lens.lens (\DescribeDBSecurityGroups' {dbSecurityGroupName} -> dbSecurityGroupName) (\s@DescribeDBSecurityGroups' {} a -> s {dbSecurityGroupName = a} :: DescribeDBSecurityGroups)

-- | This parameter isn\'t currently supported.
describeDBSecurityGroups_filters :: Lens.Lens' DescribeDBSecurityGroups (Core.Maybe [Filter])
describeDBSecurityGroups_filters = Lens.lens (\DescribeDBSecurityGroups' {filters} -> filters) (\s@DescribeDBSecurityGroups' {} a -> s {filters = a} :: DescribeDBSecurityGroups) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeDBSecurityGroups@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBSecurityGroups_marker :: Lens.Lens' DescribeDBSecurityGroups (Core.Maybe Core.Text)
describeDBSecurityGroups_marker = Lens.lens (\DescribeDBSecurityGroups' {marker} -> marker) (\s@DescribeDBSecurityGroups' {} a -> s {marker = a} :: DescribeDBSecurityGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBSecurityGroups_maxRecords :: Lens.Lens' DescribeDBSecurityGroups (Core.Maybe Core.Int)
describeDBSecurityGroups_maxRecords = Lens.lens (\DescribeDBSecurityGroups' {maxRecords} -> maxRecords) (\s@DescribeDBSecurityGroups' {} a -> s {maxRecords = a} :: DescribeDBSecurityGroups)

instance Core.AWSPager DescribeDBSecurityGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBSecurityGroupsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBSecurityGroupsResponse_dbSecurityGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBSecurityGroups_marker
          Lens..~ rs
          Lens.^? describeDBSecurityGroupsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeDBSecurityGroups where
  type
    AWSResponse DescribeDBSecurityGroups =
      DescribeDBSecurityGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBSecurityGroupsResult"
      ( \s h x ->
          DescribeDBSecurityGroupsResponse'
            Core.<$> ( x Core..@? "DBSecurityGroups" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "DBSecurityGroup")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBSecurityGroups

instance Core.NFData DescribeDBSecurityGroups

instance Core.ToHeaders DescribeDBSecurityGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBSecurityGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBSecurityGroups where
  toQuery DescribeDBSecurityGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBSecurityGroups" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBSecurityGroupName" Core.=: dbSecurityGroupName,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBSecurityGroups@ action.
--
-- /See:/ 'newDescribeDBSecurityGroupsResponse' smart constructor.
data DescribeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse'
  { -- | A list of @DBSecurityGroup@ instances.
    dbSecurityGroups :: Core.Maybe [DBSecurityGroup],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBSecurityGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSecurityGroups', 'describeDBSecurityGroupsResponse_dbSecurityGroups' - A list of @DBSecurityGroup@ instances.
--
-- 'marker', 'describeDBSecurityGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBSecurityGroupsResponse_httpStatus' - The response's http status code.
newDescribeDBSecurityGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBSecurityGroupsResponse
newDescribeDBSecurityGroupsResponse pHttpStatus_ =
  DescribeDBSecurityGroupsResponse'
    { dbSecurityGroups =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @DBSecurityGroup@ instances.
describeDBSecurityGroupsResponse_dbSecurityGroups :: Lens.Lens' DescribeDBSecurityGroupsResponse (Core.Maybe [DBSecurityGroup])
describeDBSecurityGroupsResponse_dbSecurityGroups = Lens.lens (\DescribeDBSecurityGroupsResponse' {dbSecurityGroups} -> dbSecurityGroups) (\s@DescribeDBSecurityGroupsResponse' {} a -> s {dbSecurityGroups = a} :: DescribeDBSecurityGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBSecurityGroupsResponse_marker :: Lens.Lens' DescribeDBSecurityGroupsResponse (Core.Maybe Core.Text)
describeDBSecurityGroupsResponse_marker = Lens.lens (\DescribeDBSecurityGroupsResponse' {marker} -> marker) (\s@DescribeDBSecurityGroupsResponse' {} a -> s {marker = a} :: DescribeDBSecurityGroupsResponse)

-- | The response's http status code.
describeDBSecurityGroupsResponse_httpStatus :: Lens.Lens' DescribeDBSecurityGroupsResponse Core.Int
describeDBSecurityGroupsResponse_httpStatus = Lens.lens (\DescribeDBSecurityGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBSecurityGroupsResponse' {} a -> s {httpStatus = a} :: DescribeDBSecurityGroupsResponse)

instance Core.NFData DescribeDBSecurityGroupsResponse
