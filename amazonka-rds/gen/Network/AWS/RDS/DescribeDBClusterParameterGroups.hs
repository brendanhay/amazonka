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
-- Module      : Network.AWS.RDS.DescribeDBClusterParameterGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBClusterParameterGroup@ descriptions. If a
-- @DBClusterParameterGroupName@ parameter is specified, the list will
-- contain only the description of the specified DB cluster parameter
-- group.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterParameterGroups
  ( -- * Creating a Request
    DescribeDBClusterParameterGroups (..),
    newDescribeDBClusterParameterGroups,

    -- * Request Lenses
    describeDBClusterParameterGroups_filters,
    describeDBClusterParameterGroups_dbClusterParameterGroupName,
    describeDBClusterParameterGroups_marker,
    describeDBClusterParameterGroups_maxRecords,

    -- * Destructuring the Response
    DescribeDBClusterParameterGroupsResponse (..),
    newDescribeDBClusterParameterGroupsResponse,

    -- * Response Lenses
    describeDBClusterParameterGroupsResponse_dbClusterParameterGroups,
    describeDBClusterParameterGroupsResponse_marker,
    describeDBClusterParameterGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBClusterParameterGroups' smart constructor.
data DescribeDBClusterParameterGroups = DescribeDBClusterParameterGroups'
  { -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The name of a specific DB cluster parameter group to return details for.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing
    --     DBClusterParameterGroup.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterParameterGroups@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBClusterParameterGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeDBClusterParameterGroups_filters' - This parameter isn\'t currently supported.
--
-- 'dbClusterParameterGroupName', 'describeDBClusterParameterGroups_dbClusterParameterGroupName' - The name of a specific DB cluster parameter group to return details for.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
--
-- 'marker', 'describeDBClusterParameterGroups_marker' - An optional pagination token provided by a previous
-- @DescribeDBClusterParameterGroups@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBClusterParameterGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBClusterParameterGroups ::
  DescribeDBClusterParameterGroups
newDescribeDBClusterParameterGroups =
  DescribeDBClusterParameterGroups'
    { filters =
        Prelude.Nothing,
      dbClusterParameterGroupName =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | This parameter isn\'t currently supported.
describeDBClusterParameterGroups_filters :: Lens.Lens' DescribeDBClusterParameterGroups (Prelude.Maybe [Filter])
describeDBClusterParameterGroups_filters = Lens.lens (\DescribeDBClusterParameterGroups' {filters} -> filters) (\s@DescribeDBClusterParameterGroups' {} a -> s {filters = a} :: DescribeDBClusterParameterGroups) Prelude.. Lens.mapping Lens._Coerce

-- | The name of a specific DB cluster parameter group to return details for.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
describeDBClusterParameterGroups_dbClusterParameterGroupName :: Lens.Lens' DescribeDBClusterParameterGroups (Prelude.Maybe Prelude.Text)
describeDBClusterParameterGroups_dbClusterParameterGroupName = Lens.lens (\DescribeDBClusterParameterGroups' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@DescribeDBClusterParameterGroups' {} a -> s {dbClusterParameterGroupName = a} :: DescribeDBClusterParameterGroups)

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterParameterGroups@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeDBClusterParameterGroups_marker :: Lens.Lens' DescribeDBClusterParameterGroups (Prelude.Maybe Prelude.Text)
describeDBClusterParameterGroups_marker = Lens.lens (\DescribeDBClusterParameterGroups' {marker} -> marker) (\s@DescribeDBClusterParameterGroups' {} a -> s {marker = a} :: DescribeDBClusterParameterGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusterParameterGroups_maxRecords :: Lens.Lens' DescribeDBClusterParameterGroups (Prelude.Maybe Prelude.Int)
describeDBClusterParameterGroups_maxRecords = Lens.lens (\DescribeDBClusterParameterGroups' {maxRecords} -> maxRecords) (\s@DescribeDBClusterParameterGroups' {} a -> s {maxRecords = a} :: DescribeDBClusterParameterGroups)

instance
  Core.AWSPager
    DescribeDBClusterParameterGroups
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBClusterParameterGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBClusterParameterGroupsResponse_dbClusterParameterGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDBClusterParameterGroups_marker
          Lens..~ rs
          Lens.^? describeDBClusterParameterGroupsResponse_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeDBClusterParameterGroups
  where
  type
    AWSResponse DescribeDBClusterParameterGroups =
      DescribeDBClusterParameterGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterParameterGroupsResult"
      ( \s h x ->
          DescribeDBClusterParameterGroupsResponse'
            Prelude.<$> ( x Core..@? "DBClusterParameterGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may
                              (Core.parseXMLList "DBClusterParameterGroup")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDBClusterParameterGroups

instance
  Prelude.NFData
    DescribeDBClusterParameterGroups

instance
  Core.ToHeaders
    DescribeDBClusterParameterGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeDBClusterParameterGroups where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeDBClusterParameterGroups
  where
  toQuery DescribeDBClusterParameterGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeDBClusterParameterGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "DBClusterParameterGroupName"
          Core.=: dbClusterParameterGroupName,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- |
--
-- /See:/ 'newDescribeDBClusterParameterGroupsResponse' smart constructor.
data DescribeDBClusterParameterGroupsResponse = DescribeDBClusterParameterGroupsResponse'
  { -- | A list of DB cluster parameter groups.
    dbClusterParameterGroups :: Prelude.Maybe [DBClusterParameterGroup],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterParameterGroups@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBClusterParameterGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterParameterGroups', 'describeDBClusterParameterGroupsResponse_dbClusterParameterGroups' - A list of DB cluster parameter groups.
--
-- 'marker', 'describeDBClusterParameterGroupsResponse_marker' - An optional pagination token provided by a previous
-- @DescribeDBClusterParameterGroups@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBClusterParameterGroupsResponse_httpStatus' - The response's http status code.
newDescribeDBClusterParameterGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBClusterParameterGroupsResponse
newDescribeDBClusterParameterGroupsResponse
  pHttpStatus_ =
    DescribeDBClusterParameterGroupsResponse'
      { dbClusterParameterGroups =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of DB cluster parameter groups.
describeDBClusterParameterGroupsResponse_dbClusterParameterGroups :: Lens.Lens' DescribeDBClusterParameterGroupsResponse (Prelude.Maybe [DBClusterParameterGroup])
describeDBClusterParameterGroupsResponse_dbClusterParameterGroups = Lens.lens (\DescribeDBClusterParameterGroupsResponse' {dbClusterParameterGroups} -> dbClusterParameterGroups) (\s@DescribeDBClusterParameterGroupsResponse' {} a -> s {dbClusterParameterGroups = a} :: DescribeDBClusterParameterGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterParameterGroups@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeDBClusterParameterGroupsResponse_marker :: Lens.Lens' DescribeDBClusterParameterGroupsResponse (Prelude.Maybe Prelude.Text)
describeDBClusterParameterGroupsResponse_marker = Lens.lens (\DescribeDBClusterParameterGroupsResponse' {marker} -> marker) (\s@DescribeDBClusterParameterGroupsResponse' {} a -> s {marker = a} :: DescribeDBClusterParameterGroupsResponse)

-- | The response's http status code.
describeDBClusterParameterGroupsResponse_httpStatus :: Lens.Lens' DescribeDBClusterParameterGroupsResponse Prelude.Int
describeDBClusterParameterGroupsResponse_httpStatus = Lens.lens (\DescribeDBClusterParameterGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterParameterGroupsResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterParameterGroupsResponse)

instance
  Prelude.NFData
    DescribeDBClusterParameterGroupsResponse
