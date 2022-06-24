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
-- Module      : Amazonka.RDS.DescribeCustomAvailabilityZones
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
module Amazonka.RDS.DescribeCustomAvailabilityZones
  ( -- * Creating a Request
    DescribeCustomAvailabilityZones (..),
    newDescribeCustomAvailabilityZones,

    -- * Request Lenses
    describeCustomAvailabilityZones_marker,
    describeCustomAvailabilityZones_filters,
    describeCustomAvailabilityZones_maxRecords,
    describeCustomAvailabilityZones_customAvailabilityZoneId,

    -- * Destructuring the Response
    DescribeCustomAvailabilityZonesResponse (..),
    newDescribeCustomAvailabilityZonesResponse,

    -- * Response Lenses
    describeCustomAvailabilityZonesResponse_marker,
    describeCustomAvailabilityZonesResponse_customAvailabilityZones,
    describeCustomAvailabilityZonesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCustomAvailabilityZones' smart constructor.
data DescribeCustomAvailabilityZones = DescribeCustomAvailabilityZones'
  { -- | An optional pagination token provided by a previous
    -- @DescribeCustomAvailabilityZones@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A filter that specifies one or more custom AZs to describe.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The custom AZ identifier. If this parameter is specified, information
    -- from only the specific custom AZ is returned.
    customAvailabilityZoneId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomAvailabilityZones' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeCustomAvailabilityZones_marker' - An optional pagination token provided by a previous
-- @DescribeCustomAvailabilityZones@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'filters', 'describeCustomAvailabilityZones_filters' - A filter that specifies one or more custom AZs to describe.
--
-- 'maxRecords', 'describeCustomAvailabilityZones_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'customAvailabilityZoneId', 'describeCustomAvailabilityZones_customAvailabilityZoneId' - The custom AZ identifier. If this parameter is specified, information
-- from only the specific custom AZ is returned.
newDescribeCustomAvailabilityZones ::
  DescribeCustomAvailabilityZones
newDescribeCustomAvailabilityZones =
  DescribeCustomAvailabilityZones'
    { marker =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      customAvailabilityZoneId = Prelude.Nothing
    }

-- | An optional pagination token provided by a previous
-- @DescribeCustomAvailabilityZones@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeCustomAvailabilityZones_marker :: Lens.Lens' DescribeCustomAvailabilityZones (Prelude.Maybe Prelude.Text)
describeCustomAvailabilityZones_marker = Lens.lens (\DescribeCustomAvailabilityZones' {marker} -> marker) (\s@DescribeCustomAvailabilityZones' {} a -> s {marker = a} :: DescribeCustomAvailabilityZones)

-- | A filter that specifies one or more custom AZs to describe.
describeCustomAvailabilityZones_filters :: Lens.Lens' DescribeCustomAvailabilityZones (Prelude.Maybe [Filter])
describeCustomAvailabilityZones_filters = Lens.lens (\DescribeCustomAvailabilityZones' {filters} -> filters) (\s@DescribeCustomAvailabilityZones' {} a -> s {filters = a} :: DescribeCustomAvailabilityZones) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeCustomAvailabilityZones_maxRecords :: Lens.Lens' DescribeCustomAvailabilityZones (Prelude.Maybe Prelude.Int)
describeCustomAvailabilityZones_maxRecords = Lens.lens (\DescribeCustomAvailabilityZones' {maxRecords} -> maxRecords) (\s@DescribeCustomAvailabilityZones' {} a -> s {maxRecords = a} :: DescribeCustomAvailabilityZones)

-- | The custom AZ identifier. If this parameter is specified, information
-- from only the specific custom AZ is returned.
describeCustomAvailabilityZones_customAvailabilityZoneId :: Lens.Lens' DescribeCustomAvailabilityZones (Prelude.Maybe Prelude.Text)
describeCustomAvailabilityZones_customAvailabilityZoneId = Lens.lens (\DescribeCustomAvailabilityZones' {customAvailabilityZoneId} -> customAvailabilityZoneId) (\s@DescribeCustomAvailabilityZones' {} a -> s {customAvailabilityZoneId = a} :: DescribeCustomAvailabilityZones)

instance
  Core.AWSPager
    DescribeCustomAvailabilityZones
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCustomAvailabilityZonesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCustomAvailabilityZonesResponse_customAvailabilityZones
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeCustomAvailabilityZones_marker
          Lens..~ rs
          Lens.^? describeCustomAvailabilityZonesResponse_marker
            Prelude.. Lens._Just

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
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> ( x Core..@? "CustomAvailabilityZones"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may
                              (Core.parseXMLList "CustomAvailabilityZone")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCustomAvailabilityZones
  where
  hashWithSalt
    _salt
    DescribeCustomAvailabilityZones' {..} =
      _salt `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` customAvailabilityZoneId

instance
  Prelude.NFData
    DescribeCustomAvailabilityZones
  where
  rnf DescribeCustomAvailabilityZones' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf customAvailabilityZoneId

instance
  Core.ToHeaders
    DescribeCustomAvailabilityZones
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeCustomAvailabilityZones where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCustomAvailabilityZones where
  toQuery DescribeCustomAvailabilityZones' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeCustomAvailabilityZones" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Marker" Core.=: marker,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "MaxRecords" Core.=: maxRecords,
        "CustomAvailabilityZoneId"
          Core.=: customAvailabilityZoneId
      ]

-- | /See:/ 'newDescribeCustomAvailabilityZonesResponse' smart constructor.
data DescribeCustomAvailabilityZonesResponse = DescribeCustomAvailabilityZonesResponse'
  { -- | An optional pagination token provided by a previous
    -- @DescribeCustomAvailabilityZones@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The list of CustomAvailabilityZone objects for the Amazon Web Services
    -- account.
    customAvailabilityZones :: Prelude.Maybe [CustomAvailabilityZone],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomAvailabilityZonesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeCustomAvailabilityZonesResponse_marker' - An optional pagination token provided by a previous
-- @DescribeCustomAvailabilityZones@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'customAvailabilityZones', 'describeCustomAvailabilityZonesResponse_customAvailabilityZones' - The list of CustomAvailabilityZone objects for the Amazon Web Services
-- account.
--
-- 'httpStatus', 'describeCustomAvailabilityZonesResponse_httpStatus' - The response's http status code.
newDescribeCustomAvailabilityZonesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCustomAvailabilityZonesResponse
newDescribeCustomAvailabilityZonesResponse
  pHttpStatus_ =
    DescribeCustomAvailabilityZonesResponse'
      { marker =
          Prelude.Nothing,
        customAvailabilityZones =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An optional pagination token provided by a previous
-- @DescribeCustomAvailabilityZones@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeCustomAvailabilityZonesResponse_marker :: Lens.Lens' DescribeCustomAvailabilityZonesResponse (Prelude.Maybe Prelude.Text)
describeCustomAvailabilityZonesResponse_marker = Lens.lens (\DescribeCustomAvailabilityZonesResponse' {marker} -> marker) (\s@DescribeCustomAvailabilityZonesResponse' {} a -> s {marker = a} :: DescribeCustomAvailabilityZonesResponse)

-- | The list of CustomAvailabilityZone objects for the Amazon Web Services
-- account.
describeCustomAvailabilityZonesResponse_customAvailabilityZones :: Lens.Lens' DescribeCustomAvailabilityZonesResponse (Prelude.Maybe [CustomAvailabilityZone])
describeCustomAvailabilityZonesResponse_customAvailabilityZones = Lens.lens (\DescribeCustomAvailabilityZonesResponse' {customAvailabilityZones} -> customAvailabilityZones) (\s@DescribeCustomAvailabilityZonesResponse' {} a -> s {customAvailabilityZones = a} :: DescribeCustomAvailabilityZonesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeCustomAvailabilityZonesResponse_httpStatus :: Lens.Lens' DescribeCustomAvailabilityZonesResponse Prelude.Int
describeCustomAvailabilityZonesResponse_httpStatus = Lens.lens (\DescribeCustomAvailabilityZonesResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomAvailabilityZonesResponse' {} a -> s {httpStatus = a} :: DescribeCustomAvailabilityZonesResponse)

instance
  Prelude.NFData
    DescribeCustomAvailabilityZonesResponse
  where
  rnf DescribeCustomAvailabilityZonesResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf customAvailabilityZones
      `Prelude.seq` Prelude.rnf httpStatus
