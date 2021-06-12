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
-- Module      : Network.AWS.RDS.DescribeDBSubnetGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DBSubnetGroup descriptions. If a DBSubnetGroupName is
-- specified, the list will contain only the descriptions of the specified
-- DBSubnetGroup.
--
-- For an overview of CIDR ranges, go to the
-- <http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Wikipedia Tutorial>.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBSubnetGroups
  ( -- * Creating a Request
    DescribeDBSubnetGroups (..),
    newDescribeDBSubnetGroups,

    -- * Request Lenses
    describeDBSubnetGroups_dbSubnetGroupName,
    describeDBSubnetGroups_filters,
    describeDBSubnetGroups_marker,
    describeDBSubnetGroups_maxRecords,

    -- * Destructuring the Response
    DescribeDBSubnetGroupsResponse (..),
    newDescribeDBSubnetGroupsResponse,

    -- * Response Lenses
    describeDBSubnetGroupsResponse_dbSubnetGroups,
    describeDBSubnetGroupsResponse_marker,
    describeDBSubnetGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBSubnetGroups' smart constructor.
data DescribeDBSubnetGroups = DescribeDBSubnetGroups'
  { -- | The name of the DB subnet group to return details for.
    dbSubnetGroupName :: Core.Maybe Core.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- DescribeDBSubnetGroups request. If this parameter is specified, the
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
-- Create a value of 'DescribeDBSubnetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSubnetGroupName', 'describeDBSubnetGroups_dbSubnetGroupName' - The name of the DB subnet group to return details for.
--
-- 'filters', 'describeDBSubnetGroups_filters' - This parameter isn\'t currently supported.
--
-- 'marker', 'describeDBSubnetGroups_marker' - An optional pagination token provided by a previous
-- DescribeDBSubnetGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBSubnetGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBSubnetGroups ::
  DescribeDBSubnetGroups
newDescribeDBSubnetGroups =
  DescribeDBSubnetGroups'
    { dbSubnetGroupName =
        Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of the DB subnet group to return details for.
describeDBSubnetGroups_dbSubnetGroupName :: Lens.Lens' DescribeDBSubnetGroups (Core.Maybe Core.Text)
describeDBSubnetGroups_dbSubnetGroupName = Lens.lens (\DescribeDBSubnetGroups' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@DescribeDBSubnetGroups' {} a -> s {dbSubnetGroupName = a} :: DescribeDBSubnetGroups)

-- | This parameter isn\'t currently supported.
describeDBSubnetGroups_filters :: Lens.Lens' DescribeDBSubnetGroups (Core.Maybe [Filter])
describeDBSubnetGroups_filters = Lens.lens (\DescribeDBSubnetGroups' {filters} -> filters) (\s@DescribeDBSubnetGroups' {} a -> s {filters = a} :: DescribeDBSubnetGroups) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- DescribeDBSubnetGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBSubnetGroups_marker :: Lens.Lens' DescribeDBSubnetGroups (Core.Maybe Core.Text)
describeDBSubnetGroups_marker = Lens.lens (\DescribeDBSubnetGroups' {marker} -> marker) (\s@DescribeDBSubnetGroups' {} a -> s {marker = a} :: DescribeDBSubnetGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBSubnetGroups_maxRecords :: Lens.Lens' DescribeDBSubnetGroups (Core.Maybe Core.Int)
describeDBSubnetGroups_maxRecords = Lens.lens (\DescribeDBSubnetGroups' {maxRecords} -> maxRecords) (\s@DescribeDBSubnetGroups' {} a -> s {maxRecords = a} :: DescribeDBSubnetGroups)

instance Core.AWSPager DescribeDBSubnetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBSubnetGroupsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBSubnetGroupsResponse_dbSubnetGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBSubnetGroups_marker
          Lens..~ rs
          Lens.^? describeDBSubnetGroupsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeDBSubnetGroups where
  type
    AWSResponse DescribeDBSubnetGroups =
      DescribeDBSubnetGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBSubnetGroupsResult"
      ( \s h x ->
          DescribeDBSubnetGroupsResponse'
            Core.<$> ( x Core..@? "DBSubnetGroups" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "DBSubnetGroup")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBSubnetGroups

instance Core.NFData DescribeDBSubnetGroups

instance Core.ToHeaders DescribeDBSubnetGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBSubnetGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBSubnetGroups where
  toQuery DescribeDBSubnetGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBSubnetGroups" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBSubnetGroups@ action.
--
-- /See:/ 'newDescribeDBSubnetGroupsResponse' smart constructor.
data DescribeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse'
  { -- | A list of @DBSubnetGroup@ instances.
    dbSubnetGroups :: Core.Maybe [DBSubnetGroup],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBSubnetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSubnetGroups', 'describeDBSubnetGroupsResponse_dbSubnetGroups' - A list of @DBSubnetGroup@ instances.
--
-- 'marker', 'describeDBSubnetGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBSubnetGroupsResponse_httpStatus' - The response's http status code.
newDescribeDBSubnetGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBSubnetGroupsResponse
newDescribeDBSubnetGroupsResponse pHttpStatus_ =
  DescribeDBSubnetGroupsResponse'
    { dbSubnetGroups =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @DBSubnetGroup@ instances.
describeDBSubnetGroupsResponse_dbSubnetGroups :: Lens.Lens' DescribeDBSubnetGroupsResponse (Core.Maybe [DBSubnetGroup])
describeDBSubnetGroupsResponse_dbSubnetGroups = Lens.lens (\DescribeDBSubnetGroupsResponse' {dbSubnetGroups} -> dbSubnetGroups) (\s@DescribeDBSubnetGroupsResponse' {} a -> s {dbSubnetGroups = a} :: DescribeDBSubnetGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBSubnetGroupsResponse_marker :: Lens.Lens' DescribeDBSubnetGroupsResponse (Core.Maybe Core.Text)
describeDBSubnetGroupsResponse_marker = Lens.lens (\DescribeDBSubnetGroupsResponse' {marker} -> marker) (\s@DescribeDBSubnetGroupsResponse' {} a -> s {marker = a} :: DescribeDBSubnetGroupsResponse)

-- | The response's http status code.
describeDBSubnetGroupsResponse_httpStatus :: Lens.Lens' DescribeDBSubnetGroupsResponse Core.Int
describeDBSubnetGroupsResponse_httpStatus = Lens.lens (\DescribeDBSubnetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBSubnetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeDBSubnetGroupsResponse)

instance Core.NFData DescribeDBSubnetGroupsResponse
