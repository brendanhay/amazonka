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
-- Module      : Amazonka.RDS.DescribeDBProxyTargetGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB proxy target groups, represented by
-- @DBProxyTargetGroup@ data structures.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribeDBProxyTargetGroups
  ( -- * Creating a Request
    DescribeDBProxyTargetGroups (..),
    newDescribeDBProxyTargetGroups,

    -- * Request Lenses
    describeDBProxyTargetGroups_filters,
    describeDBProxyTargetGroups_marker,
    describeDBProxyTargetGroups_maxRecords,
    describeDBProxyTargetGroups_targetGroupName,
    describeDBProxyTargetGroups_dbProxyName,

    -- * Destructuring the Response
    DescribeDBProxyTargetGroupsResponse (..),
    newDescribeDBProxyTargetGroupsResponse,

    -- * Response Lenses
    describeDBProxyTargetGroupsResponse_marker,
    describeDBProxyTargetGroupsResponse_targetGroups,
    describeDBProxyTargetGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDBProxyTargetGroups' smart constructor.
data DescribeDBProxyTargetGroups = DescribeDBProxyTargetGroups'
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
-- 'targetGroupName', 'describeDBProxyTargetGroups_targetGroupName' - The identifier of the @DBProxyTargetGroup@ to describe.
--
-- 'dbProxyName', 'describeDBProxyTargetGroups_dbProxyName' - The identifier of the @DBProxy@ associated with the target group.
newDescribeDBProxyTargetGroups ::
  -- | 'dbProxyName'
  Prelude.Text ->
  DescribeDBProxyTargetGroups
newDescribeDBProxyTargetGroups pDBProxyName_ =
  DescribeDBProxyTargetGroups'
    { filters =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      targetGroupName = Prelude.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | This parameter is not currently supported.
describeDBProxyTargetGroups_filters :: Lens.Lens' DescribeDBProxyTargetGroups (Prelude.Maybe [Filter])
describeDBProxyTargetGroups_filters = Lens.lens (\DescribeDBProxyTargetGroups' {filters} -> filters) (\s@DescribeDBProxyTargetGroups' {} a -> s {filters = a} :: DescribeDBProxyTargetGroups) Prelude.. Lens.mapping Lens.coerced

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

-- | The identifier of the @DBProxyTargetGroup@ to describe.
describeDBProxyTargetGroups_targetGroupName :: Lens.Lens' DescribeDBProxyTargetGroups (Prelude.Maybe Prelude.Text)
describeDBProxyTargetGroups_targetGroupName = Lens.lens (\DescribeDBProxyTargetGroups' {targetGroupName} -> targetGroupName) (\s@DescribeDBProxyTargetGroups' {} a -> s {targetGroupName = a} :: DescribeDBProxyTargetGroups)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeDBProxyTargetGroups_marker
          Lens..~ rs
          Lens.^? describeDBProxyTargetGroupsResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBProxyTargetGroups where
  type
    AWSResponse DescribeDBProxyTargetGroups =
      DescribeDBProxyTargetGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBProxyTargetGroupsResult"
      ( \s h x ->
          DescribeDBProxyTargetGroupsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x
                            Data..@? "TargetGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBProxyTargetGroups where
  hashWithSalt _salt DescribeDBProxyTargetGroups' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` targetGroupName
      `Prelude.hashWithSalt` dbProxyName

instance Prelude.NFData DescribeDBProxyTargetGroups where
  rnf DescribeDBProxyTargetGroups' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf targetGroupName
      `Prelude.seq` Prelude.rnf dbProxyName

instance Data.ToHeaders DescribeDBProxyTargetGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDBProxyTargetGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDBProxyTargetGroups where
  toQuery DescribeDBProxyTargetGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeDBProxyTargetGroups" ::
                      Prelude.ByteString
                  ),
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

-- | /See:/ 'newDescribeDBProxyTargetGroupsResponse' smart constructor.
data DescribeDBProxyTargetGroupsResponse = DescribeDBProxyTargetGroupsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | An arbitrary number of @DBProxyTargetGroup@ objects, containing details
    -- of the corresponding target groups.
    targetGroups :: Prelude.Maybe [DBProxyTargetGroup],
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
-- 'marker', 'describeDBProxyTargetGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'targetGroups', 'describeDBProxyTargetGroupsResponse_targetGroups' - An arbitrary number of @DBProxyTargetGroup@ objects, containing details
-- of the corresponding target groups.
--
-- 'httpStatus', 'describeDBProxyTargetGroupsResponse_httpStatus' - The response's http status code.
newDescribeDBProxyTargetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBProxyTargetGroupsResponse
newDescribeDBProxyTargetGroupsResponse pHttpStatus_ =
  DescribeDBProxyTargetGroupsResponse'
    { marker =
        Prelude.Nothing,
      targetGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxyTargetGroupsResponse_marker :: Lens.Lens' DescribeDBProxyTargetGroupsResponse (Prelude.Maybe Prelude.Text)
describeDBProxyTargetGroupsResponse_marker = Lens.lens (\DescribeDBProxyTargetGroupsResponse' {marker} -> marker) (\s@DescribeDBProxyTargetGroupsResponse' {} a -> s {marker = a} :: DescribeDBProxyTargetGroupsResponse)

-- | An arbitrary number of @DBProxyTargetGroup@ objects, containing details
-- of the corresponding target groups.
describeDBProxyTargetGroupsResponse_targetGroups :: Lens.Lens' DescribeDBProxyTargetGroupsResponse (Prelude.Maybe [DBProxyTargetGroup])
describeDBProxyTargetGroupsResponse_targetGroups = Lens.lens (\DescribeDBProxyTargetGroupsResponse' {targetGroups} -> targetGroups) (\s@DescribeDBProxyTargetGroupsResponse' {} a -> s {targetGroups = a} :: DescribeDBProxyTargetGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDBProxyTargetGroupsResponse_httpStatus :: Lens.Lens' DescribeDBProxyTargetGroupsResponse Prelude.Int
describeDBProxyTargetGroupsResponse_httpStatus = Lens.lens (\DescribeDBProxyTargetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBProxyTargetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeDBProxyTargetGroupsResponse)

instance
  Prelude.NFData
    DescribeDBProxyTargetGroupsResponse
  where
  rnf DescribeDBProxyTargetGroupsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf targetGroups
      `Prelude.seq` Prelude.rnf httpStatus
