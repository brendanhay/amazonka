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
-- Module      : Amazonka.DocumentDB.DescribeDBClusterParameterGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBClusterParameterGroup@ descriptions. If a
-- @DBClusterParameterGroupName@ parameter is specified, the list contains
-- only the description of the specified cluster parameter group.
--
-- This operation returns paginated results.
module Amazonka.DocumentDB.DescribeDBClusterParameterGroups
  ( -- * Creating a Request
    DescribeDBClusterParameterGroups (..),
    newDescribeDBClusterParameterGroups,

    -- * Request Lenses
    describeDBClusterParameterGroups_marker,
    describeDBClusterParameterGroups_filters,
    describeDBClusterParameterGroups_maxRecords,
    describeDBClusterParameterGroups_dbClusterParameterGroupName,

    -- * Destructuring the Response
    DescribeDBClusterParameterGroupsResponse (..),
    newDescribeDBClusterParameterGroupsResponse,

    -- * Response Lenses
    describeDBClusterParameterGroupsResponse_marker,
    describeDBClusterParameterGroupsResponse_dbClusterParameterGroups,
    describeDBClusterParameterGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to DescribeDBClusterParameterGroups.
--
-- /See:/ 'newDescribeDBClusterParameterGroups' smart constructor.
data DescribeDBClusterParameterGroups = DescribeDBClusterParameterGroups'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | This parameter is not currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- (marker) is included in the response so that the remaining results can
    -- be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The name of a specific cluster parameter group to return details for.
    --
    -- Constraints:
    --
    -- -   If provided, must match the name of an existing
    --     @DBClusterParameterGroup@.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text
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
-- 'marker', 'describeDBClusterParameterGroups_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'filters', 'describeDBClusterParameterGroups_filters' - This parameter is not currently supported.
--
-- 'maxRecords', 'describeDBClusterParameterGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- (marker) is included in the response so that the remaining results can
-- be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'dbClusterParameterGroupName', 'describeDBClusterParameterGroups_dbClusterParameterGroupName' - The name of a specific cluster parameter group to return details for.
--
-- Constraints:
--
-- -   If provided, must match the name of an existing
--     @DBClusterParameterGroup@.
newDescribeDBClusterParameterGroups ::
  DescribeDBClusterParameterGroups
newDescribeDBClusterParameterGroups =
  DescribeDBClusterParameterGroups'
    { marker =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      dbClusterParameterGroupName =
        Prelude.Nothing
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBClusterParameterGroups_marker :: Lens.Lens' DescribeDBClusterParameterGroups (Prelude.Maybe Prelude.Text)
describeDBClusterParameterGroups_marker = Lens.lens (\DescribeDBClusterParameterGroups' {marker} -> marker) (\s@DescribeDBClusterParameterGroups' {} a -> s {marker = a} :: DescribeDBClusterParameterGroups)

-- | This parameter is not currently supported.
describeDBClusterParameterGroups_filters :: Lens.Lens' DescribeDBClusterParameterGroups (Prelude.Maybe [Filter])
describeDBClusterParameterGroups_filters = Lens.lens (\DescribeDBClusterParameterGroups' {filters} -> filters) (\s@DescribeDBClusterParameterGroups' {} a -> s {filters = a} :: DescribeDBClusterParameterGroups) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- (marker) is included in the response so that the remaining results can
-- be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusterParameterGroups_maxRecords :: Lens.Lens' DescribeDBClusterParameterGroups (Prelude.Maybe Prelude.Int)
describeDBClusterParameterGroups_maxRecords = Lens.lens (\DescribeDBClusterParameterGroups' {maxRecords} -> maxRecords) (\s@DescribeDBClusterParameterGroups' {} a -> s {maxRecords = a} :: DescribeDBClusterParameterGroups)

-- | The name of a specific cluster parameter group to return details for.
--
-- Constraints:
--
-- -   If provided, must match the name of an existing
--     @DBClusterParameterGroup@.
describeDBClusterParameterGroups_dbClusterParameterGroupName :: Lens.Lens' DescribeDBClusterParameterGroups (Prelude.Maybe Prelude.Text)
describeDBClusterParameterGroups_dbClusterParameterGroupName = Lens.lens (\DescribeDBClusterParameterGroups' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@DescribeDBClusterParameterGroups' {} a -> s {dbClusterParameterGroupName = a} :: DescribeDBClusterParameterGroups)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterParameterGroupsResult"
      ( \s h x ->
          DescribeDBClusterParameterGroupsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "DBClusterParameterGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may
                              (Data.parseXMLList "DBClusterParameterGroup")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDBClusterParameterGroups
  where
  hashWithSalt
    _salt
    DescribeDBClusterParameterGroups' {..} =
      _salt `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` dbClusterParameterGroupName

instance
  Prelude.NFData
    DescribeDBClusterParameterGroups
  where
  rnf DescribeDBClusterParameterGroups' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName

instance
  Data.ToHeaders
    DescribeDBClusterParameterGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDBClusterParameterGroups where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeDBClusterParameterGroups
  where
  toQuery DescribeDBClusterParameterGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeDBClusterParameterGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxRecords" Data.=: maxRecords,
        "DBClusterParameterGroupName"
          Data.=: dbClusterParameterGroupName
      ]

-- | Represents the output of DBClusterParameterGroups.
--
-- /See:/ 'newDescribeDBClusterParameterGroupsResponse' smart constructor.
data DescribeDBClusterParameterGroupsResponse = DescribeDBClusterParameterGroupsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of cluster parameter groups.
    dbClusterParameterGroups :: Prelude.Maybe [DBClusterParameterGroup],
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
-- 'marker', 'describeDBClusterParameterGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'dbClusterParameterGroups', 'describeDBClusterParameterGroupsResponse_dbClusterParameterGroups' - A list of cluster parameter groups.
--
-- 'httpStatus', 'describeDBClusterParameterGroupsResponse_httpStatus' - The response's http status code.
newDescribeDBClusterParameterGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBClusterParameterGroupsResponse
newDescribeDBClusterParameterGroupsResponse
  pHttpStatus_ =
    DescribeDBClusterParameterGroupsResponse'
      { marker =
          Prelude.Nothing,
        dbClusterParameterGroups =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBClusterParameterGroupsResponse_marker :: Lens.Lens' DescribeDBClusterParameterGroupsResponse (Prelude.Maybe Prelude.Text)
describeDBClusterParameterGroupsResponse_marker = Lens.lens (\DescribeDBClusterParameterGroupsResponse' {marker} -> marker) (\s@DescribeDBClusterParameterGroupsResponse' {} a -> s {marker = a} :: DescribeDBClusterParameterGroupsResponse)

-- | A list of cluster parameter groups.
describeDBClusterParameterGroupsResponse_dbClusterParameterGroups :: Lens.Lens' DescribeDBClusterParameterGroupsResponse (Prelude.Maybe [DBClusterParameterGroup])
describeDBClusterParameterGroupsResponse_dbClusterParameterGroups = Lens.lens (\DescribeDBClusterParameterGroupsResponse' {dbClusterParameterGroups} -> dbClusterParameterGroups) (\s@DescribeDBClusterParameterGroupsResponse' {} a -> s {dbClusterParameterGroups = a} :: DescribeDBClusterParameterGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDBClusterParameterGroupsResponse_httpStatus :: Lens.Lens' DescribeDBClusterParameterGroupsResponse Prelude.Int
describeDBClusterParameterGroupsResponse_httpStatus = Lens.lens (\DescribeDBClusterParameterGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterParameterGroupsResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterParameterGroupsResponse)

instance
  Prelude.NFData
    DescribeDBClusterParameterGroupsResponse
  where
  rnf DescribeDBClusterParameterGroupsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf dbClusterParameterGroups
      `Prelude.seq` Prelude.rnf httpStatus
