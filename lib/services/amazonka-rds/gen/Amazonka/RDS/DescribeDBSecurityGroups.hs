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
-- Module      : Amazonka.RDS.DescribeDBSecurityGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBSecurityGroup@ descriptions. If a
-- @DBSecurityGroupName@ is specified, the list will contain only the
-- descriptions of the specified DB security group.
--
-- EC2-Classic was retired on August 15, 2022. If you haven\'t migrated
-- from EC2-Classic to a VPC, we recommend that you migrate as soon as
-- possible. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon EC2 User Guide/, the blog
-- <http://aws.amazon.com/blogs/aws/ec2-classic-is-retiring-heres-how-to-prepare/ EC2-Classic Networking is Retiring – Here’s How to Prepare>,
-- and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.Non-VPC2VPC.html Moving a DB instance not in a VPC into a VPC>
-- in the /Amazon RDS User Guide/.
--
-- This operation returns paginated results.
module Amazonka.RDS.DescribeDBSecurityGroups
  ( -- * Creating a Request
    DescribeDBSecurityGroups (..),
    newDescribeDBSecurityGroups,

    -- * Request Lenses
    describeDBSecurityGroups_marker,
    describeDBSecurityGroups_filters,
    describeDBSecurityGroups_maxRecords,
    describeDBSecurityGroups_dbSecurityGroupName,

    -- * Destructuring the Response
    DescribeDBSecurityGroupsResponse (..),
    newDescribeDBSecurityGroupsResponse,

    -- * Response Lenses
    describeDBSecurityGroupsResponse_marker,
    describeDBSecurityGroupsResponse_dbSecurityGroups,
    describeDBSecurityGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeDBSecurityGroups' smart constructor.
data DescribeDBSecurityGroups = DescribeDBSecurityGroups'
  { -- | An optional pagination token provided by a previous
    -- @DescribeDBSecurityGroups@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The name of the DB security group to return details for.
    dbSecurityGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBSecurityGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDBSecurityGroups_marker' - An optional pagination token provided by a previous
-- @DescribeDBSecurityGroups@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'filters', 'describeDBSecurityGroups_filters' - This parameter isn\'t currently supported.
--
-- 'maxRecords', 'describeDBSecurityGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'dbSecurityGroupName', 'describeDBSecurityGroups_dbSecurityGroupName' - The name of the DB security group to return details for.
newDescribeDBSecurityGroups ::
  DescribeDBSecurityGroups
newDescribeDBSecurityGroups =
  DescribeDBSecurityGroups'
    { marker = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      dbSecurityGroupName = Prelude.Nothing
    }

-- | An optional pagination token provided by a previous
-- @DescribeDBSecurityGroups@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBSecurityGroups_marker :: Lens.Lens' DescribeDBSecurityGroups (Prelude.Maybe Prelude.Text)
describeDBSecurityGroups_marker = Lens.lens (\DescribeDBSecurityGroups' {marker} -> marker) (\s@DescribeDBSecurityGroups' {} a -> s {marker = a} :: DescribeDBSecurityGroups)

-- | This parameter isn\'t currently supported.
describeDBSecurityGroups_filters :: Lens.Lens' DescribeDBSecurityGroups (Prelude.Maybe [Filter])
describeDBSecurityGroups_filters = Lens.lens (\DescribeDBSecurityGroups' {filters} -> filters) (\s@DescribeDBSecurityGroups' {} a -> s {filters = a} :: DescribeDBSecurityGroups) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBSecurityGroups_maxRecords :: Lens.Lens' DescribeDBSecurityGroups (Prelude.Maybe Prelude.Int)
describeDBSecurityGroups_maxRecords = Lens.lens (\DescribeDBSecurityGroups' {maxRecords} -> maxRecords) (\s@DescribeDBSecurityGroups' {} a -> s {maxRecords = a} :: DescribeDBSecurityGroups)

-- | The name of the DB security group to return details for.
describeDBSecurityGroups_dbSecurityGroupName :: Lens.Lens' DescribeDBSecurityGroups (Prelude.Maybe Prelude.Text)
describeDBSecurityGroups_dbSecurityGroupName = Lens.lens (\DescribeDBSecurityGroups' {dbSecurityGroupName} -> dbSecurityGroupName) (\s@DescribeDBSecurityGroups' {} a -> s {dbSecurityGroupName = a} :: DescribeDBSecurityGroups)

instance Core.AWSPager DescribeDBSecurityGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBSecurityGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBSecurityGroupsResponse_dbSecurityGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDBSecurityGroups_marker
          Lens..~ rs
          Lens.^? describeDBSecurityGroupsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBSecurityGroups where
  type
    AWSResponse DescribeDBSecurityGroups =
      DescribeDBSecurityGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBSecurityGroupsResult"
      ( \s h x ->
          DescribeDBSecurityGroupsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "DBSecurityGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "DBSecurityGroup")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBSecurityGroups where
  hashWithSalt _salt DescribeDBSecurityGroups' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` dbSecurityGroupName

instance Prelude.NFData DescribeDBSecurityGroups where
  rnf DescribeDBSecurityGroups' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf dbSecurityGroupName

instance Data.ToHeaders DescribeDBSecurityGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDBSecurityGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDBSecurityGroups where
  toQuery DescribeDBSecurityGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeDBSecurityGroups" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxRecords" Data.=: maxRecords,
        "DBSecurityGroupName" Data.=: dbSecurityGroupName
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBSecurityGroups@ action.
--
-- /See:/ 'newDescribeDBSecurityGroupsResponse' smart constructor.
data DescribeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of @DBSecurityGroup@ instances.
    dbSecurityGroups :: Prelude.Maybe [DBSecurityGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBSecurityGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDBSecurityGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'dbSecurityGroups', 'describeDBSecurityGroupsResponse_dbSecurityGroups' - A list of @DBSecurityGroup@ instances.
--
-- 'httpStatus', 'describeDBSecurityGroupsResponse_httpStatus' - The response's http status code.
newDescribeDBSecurityGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBSecurityGroupsResponse
newDescribeDBSecurityGroupsResponse pHttpStatus_ =
  DescribeDBSecurityGroupsResponse'
    { marker =
        Prelude.Nothing,
      dbSecurityGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBSecurityGroupsResponse_marker :: Lens.Lens' DescribeDBSecurityGroupsResponse (Prelude.Maybe Prelude.Text)
describeDBSecurityGroupsResponse_marker = Lens.lens (\DescribeDBSecurityGroupsResponse' {marker} -> marker) (\s@DescribeDBSecurityGroupsResponse' {} a -> s {marker = a} :: DescribeDBSecurityGroupsResponse)

-- | A list of @DBSecurityGroup@ instances.
describeDBSecurityGroupsResponse_dbSecurityGroups :: Lens.Lens' DescribeDBSecurityGroupsResponse (Prelude.Maybe [DBSecurityGroup])
describeDBSecurityGroupsResponse_dbSecurityGroups = Lens.lens (\DescribeDBSecurityGroupsResponse' {dbSecurityGroups} -> dbSecurityGroups) (\s@DescribeDBSecurityGroupsResponse' {} a -> s {dbSecurityGroups = a} :: DescribeDBSecurityGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDBSecurityGroupsResponse_httpStatus :: Lens.Lens' DescribeDBSecurityGroupsResponse Prelude.Int
describeDBSecurityGroupsResponse_httpStatus = Lens.lens (\DescribeDBSecurityGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBSecurityGroupsResponse' {} a -> s {httpStatus = a} :: DescribeDBSecurityGroupsResponse)

instance
  Prelude.NFData
    DescribeDBSecurityGroupsResponse
  where
  rnf DescribeDBSecurityGroupsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf dbSecurityGroups
      `Prelude.seq` Prelude.rnf httpStatus
