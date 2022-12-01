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
-- Module      : Amazonka.Neptune.DescribeDBSubnetGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Neptune.DescribeDBSubnetGroups
  ( -- * Creating a Request
    DescribeDBSubnetGroups (..),
    newDescribeDBSubnetGroups,

    -- * Request Lenses
    describeDBSubnetGroups_marker,
    describeDBSubnetGroups_dbSubnetGroupName,
    describeDBSubnetGroups_filters,
    describeDBSubnetGroups_maxRecords,

    -- * Destructuring the Response
    DescribeDBSubnetGroupsResponse (..),
    newDescribeDBSubnetGroupsResponse,

    -- * Response Lenses
    describeDBSubnetGroupsResponse_marker,
    describeDBSubnetGroupsResponse_dbSubnetGroups,
    describeDBSubnetGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDBSubnetGroups' smart constructor.
data DescribeDBSubnetGroups = DescribeDBSubnetGroups'
  { -- | An optional pagination token provided by a previous
    -- DescribeDBSubnetGroups request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB subnet group to return details for.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | This parameter is not currently supported.
    filters :: Prelude.Maybe [Filter],
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
-- Create a value of 'DescribeDBSubnetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDBSubnetGroups_marker' - An optional pagination token provided by a previous
-- DescribeDBSubnetGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'dbSubnetGroupName', 'describeDBSubnetGroups_dbSubnetGroupName' - The name of the DB subnet group to return details for.
--
-- 'filters', 'describeDBSubnetGroups_filters' - This parameter is not currently supported.
--
-- 'maxRecords', 'describeDBSubnetGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBSubnetGroups ::
  DescribeDBSubnetGroups
newDescribeDBSubnetGroups =
  DescribeDBSubnetGroups'
    { marker = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | An optional pagination token provided by a previous
-- DescribeDBSubnetGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBSubnetGroups_marker :: Lens.Lens' DescribeDBSubnetGroups (Prelude.Maybe Prelude.Text)
describeDBSubnetGroups_marker = Lens.lens (\DescribeDBSubnetGroups' {marker} -> marker) (\s@DescribeDBSubnetGroups' {} a -> s {marker = a} :: DescribeDBSubnetGroups)

-- | The name of the DB subnet group to return details for.
describeDBSubnetGroups_dbSubnetGroupName :: Lens.Lens' DescribeDBSubnetGroups (Prelude.Maybe Prelude.Text)
describeDBSubnetGroups_dbSubnetGroupName = Lens.lens (\DescribeDBSubnetGroups' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@DescribeDBSubnetGroups' {} a -> s {dbSubnetGroupName = a} :: DescribeDBSubnetGroups)

-- | This parameter is not currently supported.
describeDBSubnetGroups_filters :: Lens.Lens' DescribeDBSubnetGroups (Prelude.Maybe [Filter])
describeDBSubnetGroups_filters = Lens.lens (\DescribeDBSubnetGroups' {filters} -> filters) (\s@DescribeDBSubnetGroups' {} a -> s {filters = a} :: DescribeDBSubnetGroups) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBSubnetGroups_maxRecords :: Lens.Lens' DescribeDBSubnetGroups (Prelude.Maybe Prelude.Int)
describeDBSubnetGroups_maxRecords = Lens.lens (\DescribeDBSubnetGroups' {maxRecords} -> maxRecords) (\s@DescribeDBSubnetGroups' {} a -> s {maxRecords = a} :: DescribeDBSubnetGroups)

instance Core.AWSPager DescribeDBSubnetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBSubnetGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBSubnetGroupsResponse_dbSubnetGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDBSubnetGroups_marker
          Lens..~ rs
          Lens.^? describeDBSubnetGroupsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBSubnetGroups where
  type
    AWSResponse DescribeDBSubnetGroups =
      DescribeDBSubnetGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBSubnetGroupsResult"
      ( \s h x ->
          DescribeDBSubnetGroupsResponse'
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> ( x Core..@? "DBSubnetGroups" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "DBSubnetGroup")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBSubnetGroups where
  hashWithSalt _salt DescribeDBSubnetGroups' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeDBSubnetGroups where
  rnf DescribeDBSubnetGroups' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords

instance Core.ToHeaders DescribeDBSubnetGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeDBSubnetGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDBSubnetGroups where
  toQuery DescribeDBSubnetGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeDBSubnetGroups" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Marker" Core.=: marker,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeDBSubnetGroupsResponse' smart constructor.
data DescribeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of DBSubnetGroup instances.
    dbSubnetGroups :: Prelude.Maybe [DBSubnetGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBSubnetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDBSubnetGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'dbSubnetGroups', 'describeDBSubnetGroupsResponse_dbSubnetGroups' - A list of DBSubnetGroup instances.
--
-- 'httpStatus', 'describeDBSubnetGroupsResponse_httpStatus' - The response's http status code.
newDescribeDBSubnetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBSubnetGroupsResponse
newDescribeDBSubnetGroupsResponse pHttpStatus_ =
  DescribeDBSubnetGroupsResponse'
    { marker =
        Prelude.Nothing,
      dbSubnetGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBSubnetGroupsResponse_marker :: Lens.Lens' DescribeDBSubnetGroupsResponse (Prelude.Maybe Prelude.Text)
describeDBSubnetGroupsResponse_marker = Lens.lens (\DescribeDBSubnetGroupsResponse' {marker} -> marker) (\s@DescribeDBSubnetGroupsResponse' {} a -> s {marker = a} :: DescribeDBSubnetGroupsResponse)

-- | A list of DBSubnetGroup instances.
describeDBSubnetGroupsResponse_dbSubnetGroups :: Lens.Lens' DescribeDBSubnetGroupsResponse (Prelude.Maybe [DBSubnetGroup])
describeDBSubnetGroupsResponse_dbSubnetGroups = Lens.lens (\DescribeDBSubnetGroupsResponse' {dbSubnetGroups} -> dbSubnetGroups) (\s@DescribeDBSubnetGroupsResponse' {} a -> s {dbSubnetGroups = a} :: DescribeDBSubnetGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDBSubnetGroupsResponse_httpStatus :: Lens.Lens' DescribeDBSubnetGroupsResponse Prelude.Int
describeDBSubnetGroupsResponse_httpStatus = Lens.lens (\DescribeDBSubnetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBSubnetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeDBSubnetGroupsResponse)

instance
  Prelude.NFData
    DescribeDBSubnetGroupsResponse
  where
  rnf DescribeDBSubnetGroupsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf dbSubnetGroups
      `Prelude.seq` Prelude.rnf httpStatus
