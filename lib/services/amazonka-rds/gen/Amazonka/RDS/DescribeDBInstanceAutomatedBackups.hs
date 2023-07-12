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
-- Module      : Amazonka.RDS.DescribeDBInstanceAutomatedBackups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.RDS.DescribeDBInstanceAutomatedBackups
  ( -- * Creating a Request
    DescribeDBInstanceAutomatedBackups (..),
    newDescribeDBInstanceAutomatedBackups,

    -- * Request Lenses
    describeDBInstanceAutomatedBackups_dbInstanceAutomatedBackupsArn,
    describeDBInstanceAutomatedBackups_dbInstanceIdentifier,
    describeDBInstanceAutomatedBackups_dbiResourceId,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Parameter input for DescribeDBInstanceAutomatedBackups.
--
-- /See:/ 'newDescribeDBInstanceAutomatedBackups' smart constructor.
data DescribeDBInstanceAutomatedBackups = DescribeDBInstanceAutomatedBackups'
  { -- | The Amazon Resource Name (ARN) of the replicated automated backups, for
    -- example,
    -- @arn:aws:rds:us-east-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
    --
    -- This setting doesn\'t apply to RDS Custom.
    dbInstanceAutomatedBackupsArn :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The user-supplied instance identifier. If this parameter is
    -- specified, it must match the identifier of an existing DB instance. It
    -- returns information from the specific DB instance\' automated backup.
    -- This parameter isn\'t case-sensitive.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the DB instance that is the source of the automated
    -- backup. This parameter isn\'t case-sensitive.
    dbiResourceId :: Prelude.Maybe Prelude.Text,
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
    filters :: Prelude.Maybe [Filter],
    -- | The pagination token provided in the previous request. If this parameter
    -- is specified the response includes only records beyond the marker, up to
    -- @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- This setting doesn\'t apply to RDS Custom.
--
-- 'dbInstanceIdentifier', 'describeDBInstanceAutomatedBackups_dbInstanceIdentifier' - (Optional) The user-supplied instance identifier. If this parameter is
-- specified, it must match the identifier of an existing DB instance. It
-- returns information from the specific DB instance\' automated backup.
-- This parameter isn\'t case-sensitive.
--
-- 'dbiResourceId', 'describeDBInstanceAutomatedBackups_dbiResourceId' - The resource ID of the DB instance that is the source of the automated
-- backup. This parameter isn\'t case-sensitive.
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
        Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      dbiResourceId = Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the replicated automated backups, for
-- example,
-- @arn:aws:rds:us-east-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
--
-- This setting doesn\'t apply to RDS Custom.
describeDBInstanceAutomatedBackups_dbInstanceAutomatedBackupsArn :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Prelude.Maybe Prelude.Text)
describeDBInstanceAutomatedBackups_dbInstanceAutomatedBackupsArn = Lens.lens (\DescribeDBInstanceAutomatedBackups' {dbInstanceAutomatedBackupsArn} -> dbInstanceAutomatedBackupsArn) (\s@DescribeDBInstanceAutomatedBackups' {} a -> s {dbInstanceAutomatedBackupsArn = a} :: DescribeDBInstanceAutomatedBackups)

-- | (Optional) The user-supplied instance identifier. If this parameter is
-- specified, it must match the identifier of an existing DB instance. It
-- returns information from the specific DB instance\' automated backup.
-- This parameter isn\'t case-sensitive.
describeDBInstanceAutomatedBackups_dbInstanceIdentifier :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Prelude.Maybe Prelude.Text)
describeDBInstanceAutomatedBackups_dbInstanceIdentifier = Lens.lens (\DescribeDBInstanceAutomatedBackups' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DescribeDBInstanceAutomatedBackups' {} a -> s {dbInstanceIdentifier = a} :: DescribeDBInstanceAutomatedBackups)

-- | The resource ID of the DB instance that is the source of the automated
-- backup. This parameter isn\'t case-sensitive.
describeDBInstanceAutomatedBackups_dbiResourceId :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Prelude.Maybe Prelude.Text)
describeDBInstanceAutomatedBackups_dbiResourceId = Lens.lens (\DescribeDBInstanceAutomatedBackups' {dbiResourceId} -> dbiResourceId) (\s@DescribeDBInstanceAutomatedBackups' {} a -> s {dbiResourceId = a} :: DescribeDBInstanceAutomatedBackups)

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
describeDBInstanceAutomatedBackups_filters :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Prelude.Maybe [Filter])
describeDBInstanceAutomatedBackups_filters = Lens.lens (\DescribeDBInstanceAutomatedBackups' {filters} -> filters) (\s@DescribeDBInstanceAutomatedBackups' {} a -> s {filters = a} :: DescribeDBInstanceAutomatedBackups) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token provided in the previous request. If this parameter
-- is specified the response includes only records beyond the marker, up to
-- @MaxRecords@.
describeDBInstanceAutomatedBackups_marker :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Prelude.Maybe Prelude.Text)
describeDBInstanceAutomatedBackups_marker = Lens.lens (\DescribeDBInstanceAutomatedBackups' {marker} -> marker) (\s@DescribeDBInstanceAutomatedBackups' {} a -> s {marker = a} :: DescribeDBInstanceAutomatedBackups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
describeDBInstanceAutomatedBackups_maxRecords :: Lens.Lens' DescribeDBInstanceAutomatedBackups (Prelude.Maybe Prelude.Int)
describeDBInstanceAutomatedBackups_maxRecords = Lens.lens (\DescribeDBInstanceAutomatedBackups' {maxRecords} -> maxRecords) (\s@DescribeDBInstanceAutomatedBackups' {} a -> s {maxRecords = a} :: DescribeDBInstanceAutomatedBackups)

instance
  Core.AWSPager
    DescribeDBInstanceAutomatedBackups
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBInstanceAutomatedBackupsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBInstanceAutomatedBackupsResponse_dbInstanceAutomatedBackups
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeDBInstanceAutomatedBackups_marker
          Lens..~ rs
          Lens.^? describeDBInstanceAutomatedBackupsResponse_marker
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeDBInstanceAutomatedBackups
  where
  type
    AWSResponse DescribeDBInstanceAutomatedBackups =
      DescribeDBInstanceAutomatedBackupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBInstanceAutomatedBackupsResult"
      ( \s h x ->
          DescribeDBInstanceAutomatedBackupsResponse'
            Prelude.<$> ( x
                            Data..@? "DBInstanceAutomatedBackups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may
                              (Data.parseXMLList "DBInstanceAutomatedBackup")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDBInstanceAutomatedBackups
  where
  hashWithSalt
    _salt
    DescribeDBInstanceAutomatedBackups' {..} =
      _salt
        `Prelude.hashWithSalt` dbInstanceAutomatedBackupsArn
        `Prelude.hashWithSalt` dbInstanceIdentifier
        `Prelude.hashWithSalt` dbiResourceId
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords

instance
  Prelude.NFData
    DescribeDBInstanceAutomatedBackups
  where
  rnf DescribeDBInstanceAutomatedBackups' {..} =
    Prelude.rnf dbInstanceAutomatedBackupsArn
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf dbiResourceId
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance
  Data.ToHeaders
    DescribeDBInstanceAutomatedBackups
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeDBInstanceAutomatedBackups
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeDBInstanceAutomatedBackups
  where
  toQuery DescribeDBInstanceAutomatedBackups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeDBInstanceAutomatedBackups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBInstanceAutomatedBackupsArn"
          Data.=: dbInstanceAutomatedBackupsArn,
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier,
        "DbiResourceId" Data.=: dbiResourceId,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBInstanceAutomatedBackups@ action.
--
-- /See:/ 'newDescribeDBInstanceAutomatedBackupsResponse' smart constructor.
data DescribeDBInstanceAutomatedBackupsResponse = DescribeDBInstanceAutomatedBackupsResponse'
  { -- | A list of @DBInstanceAutomatedBackup@ instances.
    dbInstanceAutomatedBackups :: Prelude.Maybe [DBInstanceAutomatedBackup],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBInstanceAutomatedBackupsResponse_httpStatus' - The response's http status code.
newDescribeDBInstanceAutomatedBackupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBInstanceAutomatedBackupsResponse
newDescribeDBInstanceAutomatedBackupsResponse
  pHttpStatus_ =
    DescribeDBInstanceAutomatedBackupsResponse'
      { dbInstanceAutomatedBackups =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of @DBInstanceAutomatedBackup@ instances.
describeDBInstanceAutomatedBackupsResponse_dbInstanceAutomatedBackups :: Lens.Lens' DescribeDBInstanceAutomatedBackupsResponse (Prelude.Maybe [DBInstanceAutomatedBackup])
describeDBInstanceAutomatedBackupsResponse_dbInstanceAutomatedBackups = Lens.lens (\DescribeDBInstanceAutomatedBackupsResponse' {dbInstanceAutomatedBackups} -> dbInstanceAutomatedBackups) (\s@DescribeDBInstanceAutomatedBackupsResponse' {} a -> s {dbInstanceAutomatedBackups = a} :: DescribeDBInstanceAutomatedBackupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBInstanceAutomatedBackupsResponse_marker :: Lens.Lens' DescribeDBInstanceAutomatedBackupsResponse (Prelude.Maybe Prelude.Text)
describeDBInstanceAutomatedBackupsResponse_marker = Lens.lens (\DescribeDBInstanceAutomatedBackupsResponse' {marker} -> marker) (\s@DescribeDBInstanceAutomatedBackupsResponse' {} a -> s {marker = a} :: DescribeDBInstanceAutomatedBackupsResponse)

-- | The response's http status code.
describeDBInstanceAutomatedBackupsResponse_httpStatus :: Lens.Lens' DescribeDBInstanceAutomatedBackupsResponse Prelude.Int
describeDBInstanceAutomatedBackupsResponse_httpStatus = Lens.lens (\DescribeDBInstanceAutomatedBackupsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBInstanceAutomatedBackupsResponse' {} a -> s {httpStatus = a} :: DescribeDBInstanceAutomatedBackupsResponse)

instance
  Prelude.NFData
    DescribeDBInstanceAutomatedBackupsResponse
  where
  rnf DescribeDBInstanceAutomatedBackupsResponse' {..} =
    Prelude.rnf dbInstanceAutomatedBackups
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
