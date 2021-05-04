{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBSubnetGroups' smart constructor.
data DescribeDBSubnetGroups = DescribeDBSubnetGroups'
  { -- | The name of the DB subnet group to return details for.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- DescribeDBSubnetGroups request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The name of the DB subnet group to return details for.
describeDBSubnetGroups_dbSubnetGroupName :: Lens.Lens' DescribeDBSubnetGroups (Prelude.Maybe Prelude.Text)
describeDBSubnetGroups_dbSubnetGroupName = Lens.lens (\DescribeDBSubnetGroups' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@DescribeDBSubnetGroups' {} a -> s {dbSubnetGroupName = a} :: DescribeDBSubnetGroups)

-- | This parameter isn\'t currently supported.
describeDBSubnetGroups_filters :: Lens.Lens' DescribeDBSubnetGroups (Prelude.Maybe [Filter])
describeDBSubnetGroups_filters = Lens.lens (\DescribeDBSubnetGroups' {filters} -> filters) (\s@DescribeDBSubnetGroups' {} a -> s {filters = a} :: DescribeDBSubnetGroups) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional pagination token provided by a previous
-- DescribeDBSubnetGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBSubnetGroups_marker :: Lens.Lens' DescribeDBSubnetGroups (Prelude.Maybe Prelude.Text)
describeDBSubnetGroups_marker = Lens.lens (\DescribeDBSubnetGroups' {marker} -> marker) (\s@DescribeDBSubnetGroups' {} a -> s {marker = a} :: DescribeDBSubnetGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBSubnetGroups_maxRecords :: Lens.Lens' DescribeDBSubnetGroups (Prelude.Maybe Prelude.Int)
describeDBSubnetGroups_maxRecords = Lens.lens (\DescribeDBSubnetGroups' {maxRecords} -> maxRecords) (\s@DescribeDBSubnetGroups' {} a -> s {maxRecords = a} :: DescribeDBSubnetGroups)

instance Pager.AWSPager DescribeDBSubnetGroups where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeDBSubnetGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeDBSubnetGroupsResponse_dbSubnetGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeDBSubnetGroups_marker
          Lens..~ rs
          Lens.^? describeDBSubnetGroupsResponse_marker
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeDBSubnetGroups where
  type
    Rs DescribeDBSubnetGroups =
      DescribeDBSubnetGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBSubnetGroupsResult"
      ( \s h x ->
          DescribeDBSubnetGroupsResponse'
            Prelude.<$> ( x Prelude..@? "DBSubnetGroups"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "DBSubnetGroup")
                        )
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBSubnetGroups

instance Prelude.NFData DescribeDBSubnetGroups

instance Prelude.ToHeaders DescribeDBSubnetGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeDBSubnetGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeDBSubnetGroups where
  toQuery DescribeDBSubnetGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeDBSubnetGroups" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2014-10-31" :: Prelude.ByteString),
        "DBSubnetGroupName" Prelude.=: dbSubnetGroupName,
        "Filters"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Prelude.=: marker,
        "MaxRecords" Prelude.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBSubnetGroups@ action.
--
-- /See:/ 'newDescribeDBSubnetGroupsResponse' smart constructor.
data DescribeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse'
  { -- | A list of @DBSubnetGroup@ instances.
    dbSubnetGroups :: Prelude.Maybe [DBSubnetGroup],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDBSubnetGroupsResponse
newDescribeDBSubnetGroupsResponse pHttpStatus_ =
  DescribeDBSubnetGroupsResponse'
    { dbSubnetGroups =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @DBSubnetGroup@ instances.
describeDBSubnetGroupsResponse_dbSubnetGroups :: Lens.Lens' DescribeDBSubnetGroupsResponse (Prelude.Maybe [DBSubnetGroup])
describeDBSubnetGroupsResponse_dbSubnetGroups = Lens.lens (\DescribeDBSubnetGroupsResponse' {dbSubnetGroups} -> dbSubnetGroups) (\s@DescribeDBSubnetGroupsResponse' {} a -> s {dbSubnetGroups = a} :: DescribeDBSubnetGroupsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBSubnetGroupsResponse_marker :: Lens.Lens' DescribeDBSubnetGroupsResponse (Prelude.Maybe Prelude.Text)
describeDBSubnetGroupsResponse_marker = Lens.lens (\DescribeDBSubnetGroupsResponse' {marker} -> marker) (\s@DescribeDBSubnetGroupsResponse' {} a -> s {marker = a} :: DescribeDBSubnetGroupsResponse)

-- | The response's http status code.
describeDBSubnetGroupsResponse_httpStatus :: Lens.Lens' DescribeDBSubnetGroupsResponse Prelude.Int
describeDBSubnetGroupsResponse_httpStatus = Lens.lens (\DescribeDBSubnetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBSubnetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeDBSubnetGroupsResponse)

instance
  Prelude.NFData
    DescribeDBSubnetGroupsResponse
