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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDBProxyTargetGroups' smart constructor.
data DescribeDBProxyTargetGroups = DescribeDBProxyTargetGroups'
  { -- | The identifier of the @DBProxyTargetGroup@ to describe.
    targetGroupName :: Prelude.Maybe Prelude.Text,
    -- | This parameter is not currently supported.
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
    -- | The identifier of the @DBProxy@ associated with the target group.
    dbProxyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeDBProxyTargetGroups
newDescribeDBProxyTargetGroups pDBProxyName_ =
  DescribeDBProxyTargetGroups'
    { targetGroupName =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | The identifier of the @DBProxyTargetGroup@ to describe.
describeDBProxyTargetGroups_targetGroupName :: Lens.Lens' DescribeDBProxyTargetGroups (Prelude.Maybe Prelude.Text)
describeDBProxyTargetGroups_targetGroupName = Lens.lens (\DescribeDBProxyTargetGroups' {targetGroupName} -> targetGroupName) (\s@DescribeDBProxyTargetGroups' {} a -> s {targetGroupName = a} :: DescribeDBProxyTargetGroups)

-- | This parameter is not currently supported.
describeDBProxyTargetGroups_filters :: Lens.Lens' DescribeDBProxyTargetGroups (Prelude.Maybe [Filter])
describeDBProxyTargetGroups_filters = Lens.lens (\DescribeDBProxyTargetGroups' {filters} -> filters) (\s@DescribeDBProxyTargetGroups' {} a -> s {filters = a} :: DescribeDBProxyTargetGroups) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxyTargetGroups_marker :: Lens.Lens' DescribeDBProxyTargetGroups (Prelude.Maybe Prelude.Text)
describeDBProxyTargetGroups_marker = Lens.lens (\DescribeDBProxyTargetGroups' {marker} -> marker) (\s@DescribeDBProxyTargetGroups' {} a -> s {marker = a} :: DescribeDBProxyTargetGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBProxyTargetGroups_maxRecords :: Lens.Lens' DescribeDBProxyTargetGroups (Prelude.Maybe Prelude.Natural)
describeDBProxyTargetGroups_maxRecords = Lens.lens (\DescribeDBProxyTargetGroups' {maxRecords} -> maxRecords) (\s@DescribeDBProxyTargetGroups' {} a -> s {maxRecords = a} :: DescribeDBProxyTargetGroups)

-- | The identifier of the @DBProxy@ associated with the target group.
describeDBProxyTargetGroups_dbProxyName :: Lens.Lens' DescribeDBProxyTargetGroups Prelude.Text
describeDBProxyTargetGroups_dbProxyName = Lens.lens (\DescribeDBProxyTargetGroups' {dbProxyName} -> dbProxyName) (\s@DescribeDBProxyTargetGroups' {} a -> s {dbProxyName = a} :: DescribeDBProxyTargetGroups)

instance Core.AWSPager DescribeDBProxyTargetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBProxyTargetGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBProxyTargetGroupsResponse_targetGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDBProxyTargetGroups_marker
          Lens..~ rs
          Lens.^? describeDBProxyTargetGroupsResponse_marker
            Prelude.. Lens._Just

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
            Prelude.<$> ( x Core..@? "TargetGroups" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBProxyTargetGroups

instance Prelude.NFData DescribeDBProxyTargetGroups

instance Core.ToHeaders DescribeDBProxyTargetGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeDBProxyTargetGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDBProxyTargetGroups where
  toQuery DescribeDBProxyTargetGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeDBProxyTargetGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "TargetGroupName" Core.=: targetGroupName,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "DBProxyName" Core.=: dbProxyName
      ]

-- | /See:/ 'newDescribeDBProxyTargetGroupsResponse' smart constructor.
data DescribeDBProxyTargetGroupsResponse = DescribeDBProxyTargetGroupsResponse'
  { -- | An arbitrary number of @DBProxyTargetGroup@ objects, containing details
    -- of the corresponding target groups.
    targetGroups :: Prelude.Maybe [DBProxyTargetGroup],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDBProxyTargetGroupsResponse
newDescribeDBProxyTargetGroupsResponse pHttpStatus_ =
  DescribeDBProxyTargetGroupsResponse'
    { targetGroups =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An arbitrary number of @DBProxyTargetGroup@ objects, containing details
-- of the corresponding target groups.
describeDBProxyTargetGroupsResponse_targetGroups :: Lens.Lens' DescribeDBProxyTargetGroupsResponse (Prelude.Maybe [DBProxyTargetGroup])
describeDBProxyTargetGroupsResponse_targetGroups = Lens.lens (\DescribeDBProxyTargetGroupsResponse' {targetGroups} -> targetGroups) (\s@DescribeDBProxyTargetGroupsResponse' {} a -> s {targetGroups = a} :: DescribeDBProxyTargetGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxyTargetGroupsResponse_marker :: Lens.Lens' DescribeDBProxyTargetGroupsResponse (Prelude.Maybe Prelude.Text)
describeDBProxyTargetGroupsResponse_marker = Lens.lens (\DescribeDBProxyTargetGroupsResponse' {marker} -> marker) (\s@DescribeDBProxyTargetGroupsResponse' {} a -> s {marker = a} :: DescribeDBProxyTargetGroupsResponse)

-- | The response's http status code.
describeDBProxyTargetGroupsResponse_httpStatus :: Lens.Lens' DescribeDBProxyTargetGroupsResponse Prelude.Int
describeDBProxyTargetGroupsResponse_httpStatus = Lens.lens (\DescribeDBProxyTargetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBProxyTargetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeDBProxyTargetGroupsResponse)

instance
  Prelude.NFData
    DescribeDBProxyTargetGroupsResponse
