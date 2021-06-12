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
-- Module      : Network.AWS.RDS.DescribeDBInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about provisioned RDS instances. This API supports
-- pagination.
--
-- This operation can also return information for Amazon Neptune DB
-- instances and Amazon DocumentDB instances.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBInstances
  ( -- * Creating a Request
    DescribeDBInstances (..),
    newDescribeDBInstances,

    -- * Request Lenses
    describeDBInstances_dbInstanceIdentifier,
    describeDBInstances_filters,
    describeDBInstances_marker,
    describeDBInstances_maxRecords,

    -- * Destructuring the Response
    DescribeDBInstancesResponse (..),
    newDescribeDBInstancesResponse,

    -- * Response Lenses
    describeDBInstancesResponse_dbInstances,
    describeDBInstancesResponse_marker,
    describeDBInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBInstances' smart constructor.
data DescribeDBInstances = DescribeDBInstances'
  { -- | The user-supplied instance identifier. If this parameter is specified,
    -- information from only the specific DB instance is returned. This
    -- parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the identifier of an existing DBInstance.
    dbInstanceIdentifier :: Core.Maybe Core.Text,
    -- | A filter that specifies one or more DB instances to describe.
    --
    -- Supported filters:
    --
    -- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
    --     Amazon Resource Names (ARNs). The results list will only include
    --     information about the DB instances associated with the DB clusters
    --     identified by these ARNs.
    --
    -- -   @db-instance-id@ - Accepts DB instance identifiers and DB instance
    --     Amazon Resource Names (ARNs). The results list will only include
    --     information about the DB instances identified by these ARNs.
    --
    -- -   @dbi-resource-id@ - Accepts DB instance resource identifiers. The
    --     results list will only include information about the DB instances
    --     identified by these DB instance resource identifiers.
    --
    -- -   @domain@ - Accepts Active Directory directory IDs. The results list
    --     will only include information about the DB instances associated with
    --     these domains.
    --
    -- -   @engine@ - Accepts engine names. The results list will only include
    --     information about the DB instances for these engines.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBInstances@ request. If this parameter is specified, the
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
-- Create a value of 'DescribeDBInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceIdentifier', 'describeDBInstances_dbInstanceIdentifier' - The user-supplied instance identifier. If this parameter is specified,
-- information from only the specific DB instance is returned. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match the identifier of an existing DBInstance.
--
-- 'filters', 'describeDBInstances_filters' - A filter that specifies one or more DB instances to describe.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs). The results list will only include
--     information about the DB instances associated with the DB clusters
--     identified by these ARNs.
--
-- -   @db-instance-id@ - Accepts DB instance identifiers and DB instance
--     Amazon Resource Names (ARNs). The results list will only include
--     information about the DB instances identified by these ARNs.
--
-- -   @dbi-resource-id@ - Accepts DB instance resource identifiers. The
--     results list will only include information about the DB instances
--     identified by these DB instance resource identifiers.
--
-- -   @domain@ - Accepts Active Directory directory IDs. The results list
--     will only include information about the DB instances associated with
--     these domains.
--
-- -   @engine@ - Accepts engine names. The results list will only include
--     information about the DB instances for these engines.
--
-- 'marker', 'describeDBInstances_marker' - An optional pagination token provided by a previous
-- @DescribeDBInstances@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBInstances_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBInstances ::
  DescribeDBInstances
newDescribeDBInstances =
  DescribeDBInstances'
    { dbInstanceIdentifier =
        Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The user-supplied instance identifier. If this parameter is specified,
-- information from only the specific DB instance is returned. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match the identifier of an existing DBInstance.
describeDBInstances_dbInstanceIdentifier :: Lens.Lens' DescribeDBInstances (Core.Maybe Core.Text)
describeDBInstances_dbInstanceIdentifier = Lens.lens (\DescribeDBInstances' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DescribeDBInstances' {} a -> s {dbInstanceIdentifier = a} :: DescribeDBInstances)

-- | A filter that specifies one or more DB instances to describe.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs). The results list will only include
--     information about the DB instances associated with the DB clusters
--     identified by these ARNs.
--
-- -   @db-instance-id@ - Accepts DB instance identifiers and DB instance
--     Amazon Resource Names (ARNs). The results list will only include
--     information about the DB instances identified by these ARNs.
--
-- -   @dbi-resource-id@ - Accepts DB instance resource identifiers. The
--     results list will only include information about the DB instances
--     identified by these DB instance resource identifiers.
--
-- -   @domain@ - Accepts Active Directory directory IDs. The results list
--     will only include information about the DB instances associated with
--     these domains.
--
-- -   @engine@ - Accepts engine names. The results list will only include
--     information about the DB instances for these engines.
describeDBInstances_filters :: Lens.Lens' DescribeDBInstances (Core.Maybe [Filter])
describeDBInstances_filters = Lens.lens (\DescribeDBInstances' {filters} -> filters) (\s@DescribeDBInstances' {} a -> s {filters = a} :: DescribeDBInstances) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeDBInstances@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBInstances_marker :: Lens.Lens' DescribeDBInstances (Core.Maybe Core.Text)
describeDBInstances_marker = Lens.lens (\DescribeDBInstances' {marker} -> marker) (\s@DescribeDBInstances' {} a -> s {marker = a} :: DescribeDBInstances)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBInstances_maxRecords :: Lens.Lens' DescribeDBInstances (Core.Maybe Core.Int)
describeDBInstances_maxRecords = Lens.lens (\DescribeDBInstances' {maxRecords} -> maxRecords) (\s@DescribeDBInstances' {} a -> s {maxRecords = a} :: DescribeDBInstances)

instance Core.AWSPager DescribeDBInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBInstancesResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBInstancesResponse_dbInstances
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBInstances_marker
          Lens..~ rs
          Lens.^? describeDBInstancesResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeDBInstances where
  type
    AWSResponse DescribeDBInstances =
      DescribeDBInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBInstancesResult"
      ( \s h x ->
          DescribeDBInstancesResponse'
            Core.<$> ( x Core..@? "DBInstances" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "DBInstance")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBInstances

instance Core.NFData DescribeDBInstances

instance Core.ToHeaders DescribeDBInstances where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBInstances where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBInstances where
  toQuery DescribeDBInstances' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBInstances" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBInstances@ action.
--
-- /See:/ 'newDescribeDBInstancesResponse' smart constructor.
data DescribeDBInstancesResponse = DescribeDBInstancesResponse'
  { -- | A list of @DBInstance@ instances.
    dbInstances :: Core.Maybe [DBInstance],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstances', 'describeDBInstancesResponse_dbInstances' - A list of @DBInstance@ instances.
--
-- 'marker', 'describeDBInstancesResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@ .
--
-- 'httpStatus', 'describeDBInstancesResponse_httpStatus' - The response's http status code.
newDescribeDBInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBInstancesResponse
newDescribeDBInstancesResponse pHttpStatus_ =
  DescribeDBInstancesResponse'
    { dbInstances =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @DBInstance@ instances.
describeDBInstancesResponse_dbInstances :: Lens.Lens' DescribeDBInstancesResponse (Core.Maybe [DBInstance])
describeDBInstancesResponse_dbInstances = Lens.lens (\DescribeDBInstancesResponse' {dbInstances} -> dbInstances) (\s@DescribeDBInstancesResponse' {} a -> s {dbInstances = a} :: DescribeDBInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@ .
describeDBInstancesResponse_marker :: Lens.Lens' DescribeDBInstancesResponse (Core.Maybe Core.Text)
describeDBInstancesResponse_marker = Lens.lens (\DescribeDBInstancesResponse' {marker} -> marker) (\s@DescribeDBInstancesResponse' {} a -> s {marker = a} :: DescribeDBInstancesResponse)

-- | The response's http status code.
describeDBInstancesResponse_httpStatus :: Lens.Lens' DescribeDBInstancesResponse Core.Int
describeDBInstancesResponse_httpStatus = Lens.lens (\DescribeDBInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeDBInstancesResponse' {} a -> s {httpStatus = a} :: DescribeDBInstancesResponse)

instance Core.NFData DescribeDBInstancesResponse
