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
-- Module      : Network.AWS.RDS.DescribeDBInstanceAutomatedBackups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays backups for both current and deleted instances. For example,
-- use this operation to find details about automated backups for
-- previously deleted instances. Current instances with retention periods
-- greater than zero (0) are returned for both the
-- @DescribeDBInstanceAutomatedBackups@ and @DescribeDBInstances@
-- operations.
--
-- All parameters are optional.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBInstanceAutomatedBackups
  ( -- * Creating a Request
    DescribeDBInstanceAutomatedBackups (..),
    newDescribeDBInstanceAutomatedBackups,

    -- * Request Lenses
    describeDBInstanceAutomatedBackups_dbInstanceAutomatedBackupsArn,
    describeDBInstanceAutomatedBackups_dbiResourceId,
    describeDBInstanceAutomatedBackups_dbInstanceIdentifier,
    describeDBInstanceAutomatedBackups_filters,
    describeDBInstanceAutomatedBackups_marker,
    describeDBInstanceAutomatedBackups_maxRecords,

    -- * Destructuring the Response
    DescribeDBInstanceAutomatedBackupsResponse (..),
    newDescribeDBInstanceAutomatedBackupsResponse,

    -- * Response Lenses
    describeDBInstanceAutomatedBackupsResponse_dbInstanceAutomatedBackups,
    describeDBInstanceAutomatedBackupsResponse_marker,
    describeDBInstanceAutomatedBackupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Parameter input for DescribeDBInstanceAutomatedBackups.
--
-- /See:/ 'newDescribeDBInstanceAutomatedBackups' smart constructor.
data DescribeDBInstanceAutomatedBackups = DescribeDBInstanceAutomatedBackups'
  { -- | The Amazon Resource Name (ARN) of the replicated automated backups, for
    -- example,
    -- @arn:aws:rds:us-east-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
    dbInstanceAutomatedBackupsArn :: Core.Maybe Core.Text,
    -- | The resource ID of the DB instance that is the source of the automated
    -- backup. This parameter isn\'t case-sensitive.
    dbiResourceId :: Core.Maybe Core.Text,
    -- | (Optional) The user-supplied instance identifier. If this parameter is
    -- specified, it must match the identifier of an existing DB instance. It
    -- returns information from the specific DB instance\' automated backup.
    -- This parameter isn\'t case-sensitive.
    dbInstanceIdentifier :: Core.Maybe Core.Text,
    -- | A filter that specifies which resources to return based on status.
    --
    -- Supported filters are the following:
    --
    -- -   @status@
    --
    --     -   @active@ - automated backups for current instances
    --
    --     -   @retained@ - automated backups for deleted instances and after
    --         backup replication is stopped
    --
    --     -   @creating@ - automated backups that are waiting for the first
    --         automated snapshot to be available
    --
    -- -   @db-instance-id@ - Accepts DB instance identifiers and Amazon
    --     Resource Names (ARNs). The results list includes only information
    --     about the DB instance automated backups identified by these ARNs.
    --
    -- -   @dbi-resource-id@ - Accepts DB resource identifiers and Amazon
    --     Resource Names (ARNs). The results list includes only information
    --     about the DB instance resources identified by these ARNs.
    --
    -- Returns all resources by default. The status for each resource is
    -- specified in the response.
    filters :: Core.Maybe [Filter],
    -- | The pagination token provided in the previous request. If this parameter
    -- is specified the response includes only records beyond the marker, up to
    -- @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBInstanceAutomatedBackups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceAutomatedBackupsArn', 'describeDBInstanceAutomatedBackups_dbInstanceAutomatedBackupsArn' - The Amazon Resource Name (ARN) of the replicated automated backups, for
-- example,
-- @arn:aws:rds:us-east-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
--
-- 'dbiResourceId', 'describeDBInstanceAutomatedBackups_dbiResourceId' - The resource ID of the DB instance that is the source of the automated
-- backup. This parameter isn\'t case-sensitive.
--
-- 'dbInstanceIdentifier', 'describeDBInstanceAutomatedBackups_dbInstanceIdentifier' - (Optional) The user-supplied instance identifier. If this parameter is
-- specified, it must match the identifier of an existing DB instance. It
-- returns information from the specific DB instance\' automated backup.
-- This parameter isn\'t case-sensitive.
--
-- 'filters', 'describeDBInstanceAutomatedBackups_filters' - A filter that specifies which resources to return based on status.
--
-- Supported filters are the following:
--
-- -   @status@
--
--     -   @active@ - automated backups for current instances
--
--     -   @retained@ - automated backups for deleted instances and after
--         backup replication is stopped
--
--     -   @creating@ - automated backups that are waiting for the first
--         automated snapshot to be available
--
-- -   @db-instance-id@ - Accepts DB instance identifiers and Amazon
--     Resource Names (ARNs). The results list includes only information
--     about the DB instance automated backups identified by these ARNs.
--
-- -   @dbi-resource-id@ - Accepts DB resource identifiers and Amazon
--     Resource Names (ARNs). The results list includes only information
--     about the DB instance resources identified by these ARNs.
--
-- Returns all resources by default. The status for each resource is
-- specified in the response.
--
-- 'marker', 'describeDBInstanceAutomatedBackups_marker' - The pagination token provided in the previous request. If this parameter
-- is specified the response includes only records beyond the marker, up to
-- @MaxRecords@.
--
-- 'maxRecords', 'describeDBInstanceAutomatedBackups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
newDescribeDBInstanceAutomatedBackups ::
  DescribeDBInstanceAutomatedBackups
newDescribeDBInstanceAutomatedBackups =
  DescribeDBInstanceAutomatedBackups'
    { dbInstanceAutomatedBackupsArn =
        Core.Nothing,
      dbiResourceId = Core.Nothing,
      dbInstanceIdentifier = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the replicated automated backups, for
-- example,
-- @arn:aws:rds:us-east-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
describeDBInstanceAutomatedBackups_dbInstanceAutomatedBackupsArn :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Core.Maybe Core.Text)
describeDBInstanceAutomatedBackups_dbInstanceAutomatedBackupsArn = Lens.lens (\DescribeDBInstanceAutomatedBackups' {dbInstanceAutomatedBackupsArn} -> dbInstanceAutomatedBackupsArn) (\s@DescribeDBInstanceAutomatedBackups' {} a -> s {dbInstanceAutomatedBackupsArn = a} :: DescribeDBInstanceAutomatedBackups)

-- | The resource ID of the DB instance that is the source of the automated
-- backup. This parameter isn\'t case-sensitive.
describeDBInstanceAutomatedBackups_dbiResourceId :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Core.Maybe Core.Text)
describeDBInstanceAutomatedBackups_dbiResourceId = Lens.lens (\DescribeDBInstanceAutomatedBackups' {dbiResourceId} -> dbiResourceId) (\s@DescribeDBInstanceAutomatedBackups' {} a -> s {dbiResourceId = a} :: DescribeDBInstanceAutomatedBackups)

-- | (Optional) The user-supplied instance identifier. If this parameter is
-- specified, it must match the identifier of an existing DB instance. It
-- returns information from the specific DB instance\' automated backup.
-- This parameter isn\'t case-sensitive.
describeDBInstanceAutomatedBackups_dbInstanceIdentifier :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Core.Maybe Core.Text)
describeDBInstanceAutomatedBackups_dbInstanceIdentifier = Lens.lens (\DescribeDBInstanceAutomatedBackups' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DescribeDBInstanceAutomatedBackups' {} a -> s {dbInstanceIdentifier = a} :: DescribeDBInstanceAutomatedBackups)

-- | A filter that specifies which resources to return based on status.
--
-- Supported filters are the following:
--
-- -   @status@
--
--     -   @active@ - automated backups for current instances
--
--     -   @retained@ - automated backups for deleted instances and after
--         backup replication is stopped
--
--     -   @creating@ - automated backups that are waiting for the first
--         automated snapshot to be available
--
-- -   @db-instance-id@ - Accepts DB instance identifiers and Amazon
--     Resource Names (ARNs). The results list includes only information
--     about the DB instance automated backups identified by these ARNs.
--
-- -   @dbi-resource-id@ - Accepts DB resource identifiers and Amazon
--     Resource Names (ARNs). The results list includes only information
--     about the DB instance resources identified by these ARNs.
--
-- Returns all resources by default. The status for each resource is
-- specified in the response.
describeDBInstanceAutomatedBackups_filters :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Core.Maybe [Filter])
describeDBInstanceAutomatedBackups_filters = Lens.lens (\DescribeDBInstanceAutomatedBackups' {filters} -> filters) (\s@DescribeDBInstanceAutomatedBackups' {} a -> s {filters = a} :: DescribeDBInstanceAutomatedBackups) Core.. Lens.mapping Lens._Coerce

-- | The pagination token provided in the previous request. If this parameter
-- is specified the response includes only records beyond the marker, up to
-- @MaxRecords@.
describeDBInstanceAutomatedBackups_marker :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Core.Maybe Core.Text)
describeDBInstanceAutomatedBackups_marker = Lens.lens (\DescribeDBInstanceAutomatedBackups' {marker} -> marker) (\s@DescribeDBInstanceAutomatedBackups' {} a -> s {marker = a} :: DescribeDBInstanceAutomatedBackups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
describeDBInstanceAutomatedBackups_maxRecords :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Core.Maybe Core.Int)
describeDBInstanceAutomatedBackups_maxRecords = Lens.lens (\DescribeDBInstanceAutomatedBackups' {maxRecords} -> maxRecords) (\s@DescribeDBInstanceAutomatedBackups' {} a -> s {maxRecords = a} :: DescribeDBInstanceAutomatedBackups)

instance
  Core.AWSPager
    DescribeDBInstanceAutomatedBackups
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBInstanceAutomatedBackupsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBInstanceAutomatedBackupsResponse_dbInstanceAutomatedBackups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBInstanceAutomatedBackups_marker
          Lens..~ rs
          Lens.^? describeDBInstanceAutomatedBackupsResponse_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeDBInstanceAutomatedBackups
  where
  type
    AWSResponse DescribeDBInstanceAutomatedBackups =
      DescribeDBInstanceAutomatedBackupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBInstanceAutomatedBackupsResult"
      ( \s h x ->
          DescribeDBInstanceAutomatedBackupsResponse'
            Core.<$> ( x Core..@? "DBInstanceAutomatedBackups"
                         Core..!@ Core.mempty
                         Core.>>= Core.may
                           (Core.parseXMLList "DBInstanceAutomatedBackup")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeDBInstanceAutomatedBackups

instance
  Core.NFData
    DescribeDBInstanceAutomatedBackups

instance
  Core.ToHeaders
    DescribeDBInstanceAutomatedBackups
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeDBInstanceAutomatedBackups
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeDBInstanceAutomatedBackups
  where
  toQuery DescribeDBInstanceAutomatedBackups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeDBInstanceAutomatedBackups" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBInstanceAutomatedBackupsArn"
          Core.=: dbInstanceAutomatedBackupsArn,
        "DbiResourceId" Core.=: dbiResourceId,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBInstanceAutomatedBackups@ action.
--
-- /See:/ 'newDescribeDBInstanceAutomatedBackupsResponse' smart constructor.
data DescribeDBInstanceAutomatedBackupsResponse = DescribeDBInstanceAutomatedBackupsResponse'
  { -- | A list of @DBInstanceAutomatedBackup@ instances.
    dbInstanceAutomatedBackups :: Core.Maybe [DBInstanceAutomatedBackup],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@ .
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBInstanceAutomatedBackupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceAutomatedBackups', 'describeDBInstanceAutomatedBackupsResponse_dbInstanceAutomatedBackups' - A list of @DBInstanceAutomatedBackup@ instances.
--
-- 'marker', 'describeDBInstanceAutomatedBackupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@ .
--
-- 'httpStatus', 'describeDBInstanceAutomatedBackupsResponse_httpStatus' - The response's http status code.
newDescribeDBInstanceAutomatedBackupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBInstanceAutomatedBackupsResponse
newDescribeDBInstanceAutomatedBackupsResponse
  pHttpStatus_ =
    DescribeDBInstanceAutomatedBackupsResponse'
      { dbInstanceAutomatedBackups =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of @DBInstanceAutomatedBackup@ instances.
describeDBInstanceAutomatedBackupsResponse_dbInstanceAutomatedBackups :: Lens.Lens' DescribeDBInstanceAutomatedBackupsResponse (Core.Maybe [DBInstanceAutomatedBackup])
describeDBInstanceAutomatedBackupsResponse_dbInstanceAutomatedBackups = Lens.lens (\DescribeDBInstanceAutomatedBackupsResponse' {dbInstanceAutomatedBackups} -> dbInstanceAutomatedBackups) (\s@DescribeDBInstanceAutomatedBackupsResponse' {} a -> s {dbInstanceAutomatedBackups = a} :: DescribeDBInstanceAutomatedBackupsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@ .
describeDBInstanceAutomatedBackupsResponse_marker :: Lens.Lens' DescribeDBInstanceAutomatedBackupsResponse (Core.Maybe Core.Text)
describeDBInstanceAutomatedBackupsResponse_marker = Lens.lens (\DescribeDBInstanceAutomatedBackupsResponse' {marker} -> marker) (\s@DescribeDBInstanceAutomatedBackupsResponse' {} a -> s {marker = a} :: DescribeDBInstanceAutomatedBackupsResponse)

-- | The response's http status code.
describeDBInstanceAutomatedBackupsResponse_httpStatus :: Lens.Lens' DescribeDBInstanceAutomatedBackupsResponse Core.Int
describeDBInstanceAutomatedBackupsResponse_httpStatus = Lens.lens (\DescribeDBInstanceAutomatedBackupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBInstanceAutomatedBackupsResponse' {} a -> s {httpStatus = a} :: DescribeDBInstanceAutomatedBackupsResponse)

instance
  Core.NFData
    DescribeDBInstanceAutomatedBackupsResponse
