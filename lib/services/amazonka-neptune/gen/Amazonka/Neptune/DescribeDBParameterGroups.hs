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
-- Module      : Amazonka.Neptune.DescribeDBParameterGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBParameterGroup@ descriptions. If a
-- @DBParameterGroupName@ is specified, the list will contain only the
-- description of the specified DB parameter group.
--
-- This operation returns paginated results.
module Amazonka.Neptune.DescribeDBParameterGroups
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDBParameterGroups' smart constructor.
data DescribeDBParameterGroups = DescribeDBParameterGroups'
  { -- | The name of a specific DB parameter group to return details for.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing
    --     DBClusterParameterGroup.
    dbParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | This parameter is not currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBParameterGroups@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'filters', 'describeDBParameterGroups_filters' - This parameter is not currently supported.
--
-- 'marker', 'describeDBParameterGroups_marker' - An optional pagination token provided by a previous
-- @DescribeDBParameterGroups@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBParameterGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBParameterGroups ::
  DescribeDBParameterGroups
newDescribeDBParameterGroups =
  DescribeDBParameterGroups'
    { dbParameterGroupName =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
describeDBParameterGroups_dbParameterGroupName :: Lens.Lens' DescribeDBParameterGroups (Prelude.Maybe Prelude.Text)
describeDBParameterGroups_dbParameterGroupName = Lens.lens (\DescribeDBParameterGroups' {dbParameterGroupName} -> dbParameterGroupName) (\s@DescribeDBParameterGroups' {} a -> s {dbParameterGroupName = a} :: DescribeDBParameterGroups)

-- | This parameter is not currently supported.
describeDBParameterGroups_filters :: Lens.Lens' DescribeDBParameterGroups (Prelude.Maybe [Filter])
describeDBParameterGroups_filters = Lens.lens (\DescribeDBParameterGroups' {filters} -> filters) (\s@DescribeDBParameterGroups' {} a -> s {filters = a} :: DescribeDBParameterGroups) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- @DescribeDBParameterGroups@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBParameterGroups_marker :: Lens.Lens' DescribeDBParameterGroups (Prelude.Maybe Prelude.Text)
describeDBParameterGroups_marker = Lens.lens (\DescribeDBParameterGroups' {marker} -> marker) (\s@DescribeDBParameterGroups' {} a -> s {marker = a} :: DescribeDBParameterGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBParameterGroups_maxRecords :: Lens.Lens' DescribeDBParameterGroups (Prelude.Maybe Prelude.Int)
describeDBParameterGroups_maxRecords = Lens.lens (\DescribeDBParameterGroups' {maxRecords} -> maxRecords) (\s@DescribeDBParameterGroups' {} a -> s {maxRecords = a} :: DescribeDBParameterGroups)

instance Core.AWSPager DescribeDBParameterGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBParameterGroupsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBParameterGroupsResponse_dbParameterGroups
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeDBParameterGroups_marker
          Lens..~ rs
          Lens.^? describeDBParameterGroupsResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBParameterGroups where
  type
    AWSResponse DescribeDBParameterGroups =
      DescribeDBParameterGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBParameterGroupsResult"
      ( \s h x ->
          DescribeDBParameterGroupsResponse'
            Prelude.<$> ( x
                            Data..@? "DBParameterGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "DBParameterGroup")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBParameterGroups where
  hashWithSalt _salt DescribeDBParameterGroups' {..} =
    _salt
      `Prelude.hashWithSalt` dbParameterGroupName
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeDBParameterGroups where
  rnf DescribeDBParameterGroups' {..} =
    Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeDBParameterGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDBParameterGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDBParameterGroups where
  toQuery DescribeDBParameterGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeDBParameterGroups" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBParameterGroupName" Data.=: dbParameterGroupName,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
      ]

-- | /See:/ 'newDescribeDBParameterGroupsResponse' smart constructor.
data DescribeDBParameterGroupsResponse = DescribeDBParameterGroupsResponse'
  { -- | A list of DBParameterGroup instances.
    dbParameterGroups :: Prelude.Maybe [DBParameterGroup],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBParameterGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbParameterGroups', 'describeDBParameterGroupsResponse_dbParameterGroups' - A list of DBParameterGroup instances.
--
-- 'marker', 'describeDBParameterGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBParameterGroupsResponse_httpStatus' - The response's http status code.
newDescribeDBParameterGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBParameterGroupsResponse
newDescribeDBParameterGroupsResponse pHttpStatus_ =
  DescribeDBParameterGroupsResponse'
    { dbParameterGroups =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of DBParameterGroup instances.
describeDBParameterGroupsResponse_dbParameterGroups :: Lens.Lens' DescribeDBParameterGroupsResponse (Prelude.Maybe [DBParameterGroup])
describeDBParameterGroupsResponse_dbParameterGroups = Lens.lens (\DescribeDBParameterGroupsResponse' {dbParameterGroups} -> dbParameterGroups) (\s@DescribeDBParameterGroupsResponse' {} a -> s {dbParameterGroups = a} :: DescribeDBParameterGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBParameterGroupsResponse_marker :: Lens.Lens' DescribeDBParameterGroupsResponse (Prelude.Maybe Prelude.Text)
describeDBParameterGroupsResponse_marker = Lens.lens (\DescribeDBParameterGroupsResponse' {marker} -> marker) (\s@DescribeDBParameterGroupsResponse' {} a -> s {marker = a} :: DescribeDBParameterGroupsResponse)

-- | The response's http status code.
describeDBParameterGroupsResponse_httpStatus :: Lens.Lens' DescribeDBParameterGroupsResponse Prelude.Int
describeDBParameterGroupsResponse_httpStatus = Lens.lens (\DescribeDBParameterGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBParameterGroupsResponse' {} a -> s {httpStatus = a} :: DescribeDBParameterGroupsResponse)

instance
  Prelude.NFData
    DescribeDBParameterGroupsResponse
  where
  rnf DescribeDBParameterGroupsResponse' {..} =
    Prelude.rnf dbParameterGroups
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
