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
-- Module      : Network.AWS.RDS.DescribeOrderableDBInstanceOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of orderable DB instance options for the specified
-- engine.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeOrderableDBInstanceOptions
  ( -- * Creating a Request
    DescribeOrderableDBInstanceOptions (..),
    newDescribeOrderableDBInstanceOptions,

    -- * Request Lenses
    describeOrderableDBInstanceOptions_engineVersion,
    describeOrderableDBInstanceOptions_licenseModel,
    describeOrderableDBInstanceOptions_dbInstanceClass,
    describeOrderableDBInstanceOptions_filters,
    describeOrderableDBInstanceOptions_availabilityZoneGroup,
    describeOrderableDBInstanceOptions_vpc,
    describeOrderableDBInstanceOptions_marker,
    describeOrderableDBInstanceOptions_maxRecords,
    describeOrderableDBInstanceOptions_engine,

    -- * Destructuring the Response
    DescribeOrderableDBInstanceOptionsResponse (..),
    newDescribeOrderableDBInstanceOptionsResponse,

    -- * Response Lenses
    describeOrderableDBInstanceOptionsResponse_orderableDBInstanceOptions,
    describeOrderableDBInstanceOptionsResponse_marker,
    describeOrderableDBInstanceOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeOrderableDBInstanceOptions' smart constructor.
data DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptions'
  { -- | The engine version filter value. Specify this parameter to show only the
    -- available offerings matching the specified engine version.
    engineVersion :: Core.Maybe Core.Text,
    -- | The license model filter value. Specify this parameter to show only the
    -- available offerings matching the specified license model.
    licenseModel :: Core.Maybe Core.Text,
    -- | The DB instance class filter value. Specify this parameter to show only
    -- the available offerings matching the specified DB instance class.
    dbInstanceClass :: Core.Maybe Core.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Core.Maybe [Filter],
    -- | The Availability Zone group associated with a Local Zone. Specify this
    -- parameter to retrieve available offerings for the Local Zones in the
    -- group.
    --
    -- Omit this parameter to show the available offerings in the specified AWS
    -- Region.
    availabilityZoneGroup :: Core.Maybe Core.Text,
    -- | A value that indicates whether to show only VPC or non-VPC offerings.
    vpc :: Core.Maybe Core.Bool,
    -- | An optional pagination token provided by a previous
    -- DescribeOrderableDBInstanceOptions request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@ .
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The name of the engine to retrieve DB instance options for.
    --
    -- Valid Values:
    --
    -- -   @aurora@ (for MySQL 5.6-compatible Aurora)
    --
    -- -   @aurora-mysql@ (for MySQL 5.7-compatible Aurora)
    --
    -- -   @aurora-postgresql@
    --
    -- -   @mariadb@
    --
    -- -   @mysql@
    --
    -- -   @oracle-ee@
    --
    -- -   @oracle-se2@
    --
    -- -   @oracle-se1@
    --
    -- -   @oracle-se@
    --
    -- -   @postgres@
    --
    -- -   @sqlserver-ee@
    --
    -- -   @sqlserver-se@
    --
    -- -   @sqlserver-ex@
    --
    -- -   @sqlserver-web@
    engine :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeOrderableDBInstanceOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'describeOrderableDBInstanceOptions_engineVersion' - The engine version filter value. Specify this parameter to show only the
-- available offerings matching the specified engine version.
--
-- 'licenseModel', 'describeOrderableDBInstanceOptions_licenseModel' - The license model filter value. Specify this parameter to show only the
-- available offerings matching the specified license model.
--
-- 'dbInstanceClass', 'describeOrderableDBInstanceOptions_dbInstanceClass' - The DB instance class filter value. Specify this parameter to show only
-- the available offerings matching the specified DB instance class.
--
-- 'filters', 'describeOrderableDBInstanceOptions_filters' - This parameter isn\'t currently supported.
--
-- 'availabilityZoneGroup', 'describeOrderableDBInstanceOptions_availabilityZoneGroup' - The Availability Zone group associated with a Local Zone. Specify this
-- parameter to retrieve available offerings for the Local Zones in the
-- group.
--
-- Omit this parameter to show the available offerings in the specified AWS
-- Region.
--
-- 'vpc', 'describeOrderableDBInstanceOptions_vpc' - A value that indicates whether to show only VPC or non-VPC offerings.
--
-- 'marker', 'describeOrderableDBInstanceOptions_marker' - An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@ .
--
-- 'maxRecords', 'describeOrderableDBInstanceOptions_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'engine', 'describeOrderableDBInstanceOptions_engine' - The name of the engine to retrieve DB instance options for.
--
-- Valid Values:
--
-- -   @aurora@ (for MySQL 5.6-compatible Aurora)
--
-- -   @aurora-mysql@ (for MySQL 5.7-compatible Aurora)
--
-- -   @aurora-postgresql@
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-se2@
--
-- -   @oracle-se1@
--
-- -   @oracle-se@
--
-- -   @postgres@
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-web@
newDescribeOrderableDBInstanceOptions ::
  -- | 'engine'
  Core.Text ->
  DescribeOrderableDBInstanceOptions
newDescribeOrderableDBInstanceOptions pEngine_ =
  DescribeOrderableDBInstanceOptions'
    { engineVersion =
        Core.Nothing,
      licenseModel = Core.Nothing,
      dbInstanceClass = Core.Nothing,
      filters = Core.Nothing,
      availabilityZoneGroup = Core.Nothing,
      vpc = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      engine = pEngine_
    }

-- | The engine version filter value. Specify this parameter to show only the
-- available offerings matching the specified engine version.
describeOrderableDBInstanceOptions_engineVersion :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Text)
describeOrderableDBInstanceOptions_engineVersion = Lens.lens (\DescribeOrderableDBInstanceOptions' {engineVersion} -> engineVersion) (\s@DescribeOrderableDBInstanceOptions' {} a -> s {engineVersion = a} :: DescribeOrderableDBInstanceOptions)

-- | The license model filter value. Specify this parameter to show only the
-- available offerings matching the specified license model.
describeOrderableDBInstanceOptions_licenseModel :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Text)
describeOrderableDBInstanceOptions_licenseModel = Lens.lens (\DescribeOrderableDBInstanceOptions' {licenseModel} -> licenseModel) (\s@DescribeOrderableDBInstanceOptions' {} a -> s {licenseModel = a} :: DescribeOrderableDBInstanceOptions)

-- | The DB instance class filter value. Specify this parameter to show only
-- the available offerings matching the specified DB instance class.
describeOrderableDBInstanceOptions_dbInstanceClass :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Text)
describeOrderableDBInstanceOptions_dbInstanceClass = Lens.lens (\DescribeOrderableDBInstanceOptions' {dbInstanceClass} -> dbInstanceClass) (\s@DescribeOrderableDBInstanceOptions' {} a -> s {dbInstanceClass = a} :: DescribeOrderableDBInstanceOptions)

-- | This parameter isn\'t currently supported.
describeOrderableDBInstanceOptions_filters :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe [Filter])
describeOrderableDBInstanceOptions_filters = Lens.lens (\DescribeOrderableDBInstanceOptions' {filters} -> filters) (\s@DescribeOrderableDBInstanceOptions' {} a -> s {filters = a} :: DescribeOrderableDBInstanceOptions) Core.. Lens.mapping Lens._Coerce

-- | The Availability Zone group associated with a Local Zone. Specify this
-- parameter to retrieve available offerings for the Local Zones in the
-- group.
--
-- Omit this parameter to show the available offerings in the specified AWS
-- Region.
describeOrderableDBInstanceOptions_availabilityZoneGroup :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Text)
describeOrderableDBInstanceOptions_availabilityZoneGroup = Lens.lens (\DescribeOrderableDBInstanceOptions' {availabilityZoneGroup} -> availabilityZoneGroup) (\s@DescribeOrderableDBInstanceOptions' {} a -> s {availabilityZoneGroup = a} :: DescribeOrderableDBInstanceOptions)

-- | A value that indicates whether to show only VPC or non-VPC offerings.
describeOrderableDBInstanceOptions_vpc :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Bool)
describeOrderableDBInstanceOptions_vpc = Lens.lens (\DescribeOrderableDBInstanceOptions' {vpc} -> vpc) (\s@DescribeOrderableDBInstanceOptions' {} a -> s {vpc = a} :: DescribeOrderableDBInstanceOptions)

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@ .
describeOrderableDBInstanceOptions_marker :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Text)
describeOrderableDBInstanceOptions_marker = Lens.lens (\DescribeOrderableDBInstanceOptions' {marker} -> marker) (\s@DescribeOrderableDBInstanceOptions' {} a -> s {marker = a} :: DescribeOrderableDBInstanceOptions)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeOrderableDBInstanceOptions_maxRecords :: Lens.Lens' DescribeOrderableDBInstanceOptions (Core.Maybe Core.Int)
describeOrderableDBInstanceOptions_maxRecords = Lens.lens (\DescribeOrderableDBInstanceOptions' {maxRecords} -> maxRecords) (\s@DescribeOrderableDBInstanceOptions' {} a -> s {maxRecords = a} :: DescribeOrderableDBInstanceOptions)

-- | The name of the engine to retrieve DB instance options for.
--
-- Valid Values:
--
-- -   @aurora@ (for MySQL 5.6-compatible Aurora)
--
-- -   @aurora-mysql@ (for MySQL 5.7-compatible Aurora)
--
-- -   @aurora-postgresql@
--
-- -   @mariadb@
--
-- -   @mysql@
--
-- -   @oracle-ee@
--
-- -   @oracle-se2@
--
-- -   @oracle-se1@
--
-- -   @oracle-se@
--
-- -   @postgres@
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-web@
describeOrderableDBInstanceOptions_engine :: Lens.Lens' DescribeOrderableDBInstanceOptions Core.Text
describeOrderableDBInstanceOptions_engine = Lens.lens (\DescribeOrderableDBInstanceOptions' {engine} -> engine) (\s@DescribeOrderableDBInstanceOptions' {} a -> s {engine = a} :: DescribeOrderableDBInstanceOptions)

instance
  Core.AWSPager
    DescribeOrderableDBInstanceOptions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOrderableDBInstanceOptionsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOrderableDBInstanceOptionsResponse_orderableDBInstanceOptions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeOrderableDBInstanceOptions_marker
          Lens..~ rs
          Lens.^? describeOrderableDBInstanceOptionsResponse_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeOrderableDBInstanceOptions
  where
  type
    AWSResponse DescribeOrderableDBInstanceOptions =
      DescribeOrderableDBInstanceOptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeOrderableDBInstanceOptionsResult"
      ( \s h x ->
          DescribeOrderableDBInstanceOptionsResponse'
            Core.<$> ( x Core..@? "OrderableDBInstanceOptions"
                         Core..!@ Core.mempty
                         Core.>>= Core.may
                           (Core.parseXMLList "OrderableDBInstanceOption")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeOrderableDBInstanceOptions

instance
  Core.NFData
    DescribeOrderableDBInstanceOptions

instance
  Core.ToHeaders
    DescribeOrderableDBInstanceOptions
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeOrderableDBInstanceOptions
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeOrderableDBInstanceOptions
  where
  toQuery DescribeOrderableDBInstanceOptions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeOrderableDBInstanceOptions" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "EngineVersion" Core.=: engineVersion,
        "LicenseModel" Core.=: licenseModel,
        "DBInstanceClass" Core.=: dbInstanceClass,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "AvailabilityZoneGroup"
          Core.=: availabilityZoneGroup,
        "Vpc" Core.=: vpc,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "Engine" Core.=: engine
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeOrderableDBInstanceOptions@ action.
--
-- /See:/ 'newDescribeOrderableDBInstanceOptionsResponse' smart constructor.
data DescribeOrderableDBInstanceOptionsResponse = DescribeOrderableDBInstanceOptionsResponse'
  { -- | An @OrderableDBInstanceOption@ structure containing information about
    -- orderable options for the DB instance.
    orderableDBInstanceOptions :: Core.Maybe [OrderableDBInstanceOption],
    -- | An optional pagination token provided by a previous
    -- OrderableDBInstanceOptions request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@ .
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeOrderableDBInstanceOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'orderableDBInstanceOptions', 'describeOrderableDBInstanceOptionsResponse_orderableDBInstanceOptions' - An @OrderableDBInstanceOption@ structure containing information about
-- orderable options for the DB instance.
--
-- 'marker', 'describeOrderableDBInstanceOptionsResponse_marker' - An optional pagination token provided by a previous
-- OrderableDBInstanceOptions request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
--
-- 'httpStatus', 'describeOrderableDBInstanceOptionsResponse_httpStatus' - The response's http status code.
newDescribeOrderableDBInstanceOptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeOrderableDBInstanceOptionsResponse
newDescribeOrderableDBInstanceOptionsResponse
  pHttpStatus_ =
    DescribeOrderableDBInstanceOptionsResponse'
      { orderableDBInstanceOptions =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An @OrderableDBInstanceOption@ structure containing information about
-- orderable options for the DB instance.
describeOrderableDBInstanceOptionsResponse_orderableDBInstanceOptions :: Lens.Lens' DescribeOrderableDBInstanceOptionsResponse (Core.Maybe [OrderableDBInstanceOption])
describeOrderableDBInstanceOptionsResponse_orderableDBInstanceOptions = Lens.lens (\DescribeOrderableDBInstanceOptionsResponse' {orderableDBInstanceOptions} -> orderableDBInstanceOptions) (\s@DescribeOrderableDBInstanceOptionsResponse' {} a -> s {orderableDBInstanceOptions = a} :: DescribeOrderableDBInstanceOptionsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- OrderableDBInstanceOptions request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
describeOrderableDBInstanceOptionsResponse_marker :: Lens.Lens' DescribeOrderableDBInstanceOptionsResponse (Core.Maybe Core.Text)
describeOrderableDBInstanceOptionsResponse_marker = Lens.lens (\DescribeOrderableDBInstanceOptionsResponse' {marker} -> marker) (\s@DescribeOrderableDBInstanceOptionsResponse' {} a -> s {marker = a} :: DescribeOrderableDBInstanceOptionsResponse)

-- | The response's http status code.
describeOrderableDBInstanceOptionsResponse_httpStatus :: Lens.Lens' DescribeOrderableDBInstanceOptionsResponse Core.Int
describeOrderableDBInstanceOptionsResponse_httpStatus = Lens.lens (\DescribeOrderableDBInstanceOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeOrderableDBInstanceOptionsResponse' {} a -> s {httpStatus = a} :: DescribeOrderableDBInstanceOptionsResponse)

instance
  Core.NFData
    DescribeOrderableDBInstanceOptionsResponse
