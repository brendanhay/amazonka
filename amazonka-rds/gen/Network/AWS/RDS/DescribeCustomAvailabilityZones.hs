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
-- Module      : Network.AWS.RDS.DescribeCustomAvailabilityZones
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about custom Availability Zones (AZs).
--
-- A custom AZ is an on-premises AZ that is integrated with a VMware
-- vSphere cluster.
--
-- For more information about RDS on VMware, see the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html RDS on VMware User Guide.>
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeCustomAvailabilityZones
  ( -- * Creating a Request
    DescribeCustomAvailabilityZones (..),
    newDescribeCustomAvailabilityZones,

    -- * Request Lenses
    describeCustomAvailabilityZones_customAvailabilityZoneId,
    describeCustomAvailabilityZones_filters,
    describeCustomAvailabilityZones_marker,
    describeCustomAvailabilityZones_maxRecords,

    -- * Destructuring the Response
    DescribeCustomAvailabilityZonesResponse (..),
    newDescribeCustomAvailabilityZonesResponse,

    -- * Response Lenses
    describeCustomAvailabilityZonesResponse_customAvailabilityZones,
    describeCustomAvailabilityZonesResponse_marker,
    describeCustomAvailabilityZonesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCustomAvailabilityZones' smart constructor.
data DescribeCustomAvailabilityZones = DescribeCustomAvailabilityZones'
  { -- | The custom AZ identifier. If this parameter is specified, information
    -- from only the specific custom AZ is returned.
    customAvailabilityZoneId :: Core.Maybe Core.Text,
    -- | A filter that specifies one or more custom AZs to describe.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeCustomAvailabilityZones@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCustomAvailabilityZones' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customAvailabilityZoneId', 'describeCustomAvailabilityZones_customAvailabilityZoneId' - The custom AZ identifier. If this parameter is specified, information
-- from only the specific custom AZ is returned.
--
-- 'filters', 'describeCustomAvailabilityZones_filters' - A filter that specifies one or more custom AZs to describe.
--
-- 'marker', 'describeCustomAvailabilityZones_marker' - An optional pagination token provided by a previous
-- @DescribeCustomAvailabilityZones@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeCustomAvailabilityZones_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeCustomAvailabilityZones ::
  DescribeCustomAvailabilityZones
newDescribeCustomAvailabilityZones =
  DescribeCustomAvailabilityZones'
    { customAvailabilityZoneId =
        Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The custom AZ identifier. If this parameter is specified, information
-- from only the specific custom AZ is returned.
describeCustomAvailabilityZones_customAvailabilityZoneId :: Lens.Lens' DescribeCustomAvailabilityZones (Core.Maybe Core.Text)
describeCustomAvailabilityZones_customAvailabilityZoneId = Lens.lens (\DescribeCustomAvailabilityZones' {customAvailabilityZoneId} -> customAvailabilityZoneId) (\s@DescribeCustomAvailabilityZones' {} a -> s {customAvailabilityZoneId = a} :: DescribeCustomAvailabilityZones)

-- | A filter that specifies one or more custom AZs to describe.
describeCustomAvailabilityZones_filters :: Lens.Lens' DescribeCustomAvailabilityZones (Core.Maybe [Filter])
describeCustomAvailabilityZones_filters = Lens.lens (\DescribeCustomAvailabilityZones' {filters} -> filters) (\s@DescribeCustomAvailabilityZones' {} a -> s {filters = a} :: DescribeCustomAvailabilityZones) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeCustomAvailabilityZones@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeCustomAvailabilityZones_marker :: Lens.Lens' DescribeCustomAvailabilityZones (Core.Maybe Core.Text)
describeCustomAvailabilityZones_marker = Lens.lens (\DescribeCustomAvailabilityZones' {marker} -> marker) (\s@DescribeCustomAvailabilityZones' {} a -> s {marker = a} :: DescribeCustomAvailabilityZones)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeCustomAvailabilityZones_maxRecords :: Lens.Lens' DescribeCustomAvailabilityZones (Core.Maybe Core.Int)
describeCustomAvailabilityZones_maxRecords = Lens.lens (\DescribeCustomAvailabilityZones' {maxRecords} -> maxRecords) (\s@DescribeCustomAvailabilityZones' {} a -> s {maxRecords = a} :: DescribeCustomAvailabilityZones)

instance
  Core.AWSPager
    DescribeCustomAvailabilityZones
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCustomAvailabilityZonesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCustomAvailabilityZonesResponse_customAvailabilityZones
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeCustomAvailabilityZones_marker
          Lens..~ rs
          Lens.^? describeCustomAvailabilityZonesResponse_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeCustomAvailabilityZones
  where
  type
    AWSResponse DescribeCustomAvailabilityZones =
      DescribeCustomAvailabilityZonesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeCustomAvailabilityZonesResult"
      ( \s h x ->
          DescribeCustomAvailabilityZonesResponse'
            Core.<$> ( x Core..@? "CustomAvailabilityZones"
                         Core..!@ Core.mempty
                         Core.>>= Core.may
                           (Core.parseXMLList "CustomAvailabilityZone")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeCustomAvailabilityZones

instance Core.NFData DescribeCustomAvailabilityZones

instance
  Core.ToHeaders
    DescribeCustomAvailabilityZones
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeCustomAvailabilityZones where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCustomAvailabilityZones where
  toQuery DescribeCustomAvailabilityZones' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeCustomAvailabilityZones" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "CustomAvailabilityZoneId"
          Core.=: customAvailabilityZoneId,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeCustomAvailabilityZonesResponse' smart constructor.
data DescribeCustomAvailabilityZonesResponse = DescribeCustomAvailabilityZonesResponse'
  { -- | The list of CustomAvailabilityZone objects for the AWS account.
    customAvailabilityZones :: Core.Maybe [CustomAvailabilityZone],
    -- | An optional pagination token provided by a previous
    -- @DescribeCustomAvailabilityZones@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCustomAvailabilityZonesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customAvailabilityZones', 'describeCustomAvailabilityZonesResponse_customAvailabilityZones' - The list of CustomAvailabilityZone objects for the AWS account.
--
-- 'marker', 'describeCustomAvailabilityZonesResponse_marker' - An optional pagination token provided by a previous
-- @DescribeCustomAvailabilityZones@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeCustomAvailabilityZonesResponse_httpStatus' - The response's http status code.
newDescribeCustomAvailabilityZonesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCustomAvailabilityZonesResponse
newDescribeCustomAvailabilityZonesResponse
  pHttpStatus_ =
    DescribeCustomAvailabilityZonesResponse'
      { customAvailabilityZones =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of CustomAvailabilityZone objects for the AWS account.
describeCustomAvailabilityZonesResponse_customAvailabilityZones :: Lens.Lens' DescribeCustomAvailabilityZonesResponse (Core.Maybe [CustomAvailabilityZone])
describeCustomAvailabilityZonesResponse_customAvailabilityZones = Lens.lens (\DescribeCustomAvailabilityZonesResponse' {customAvailabilityZones} -> customAvailabilityZones) (\s@DescribeCustomAvailabilityZonesResponse' {} a -> s {customAvailabilityZones = a} :: DescribeCustomAvailabilityZonesResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeCustomAvailabilityZones@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeCustomAvailabilityZonesResponse_marker :: Lens.Lens' DescribeCustomAvailabilityZonesResponse (Core.Maybe Core.Text)
describeCustomAvailabilityZonesResponse_marker = Lens.lens (\DescribeCustomAvailabilityZonesResponse' {marker} -> marker) (\s@DescribeCustomAvailabilityZonesResponse' {} a -> s {marker = a} :: DescribeCustomAvailabilityZonesResponse)

-- | The response's http status code.
describeCustomAvailabilityZonesResponse_httpStatus :: Lens.Lens' DescribeCustomAvailabilityZonesResponse Core.Int
describeCustomAvailabilityZonesResponse_httpStatus = Lens.lens (\DescribeCustomAvailabilityZonesResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomAvailabilityZonesResponse' {} a -> s {httpStatus = a} :: DescribeCustomAvailabilityZonesResponse)

instance
  Core.NFData
    DescribeCustomAvailabilityZonesResponse
