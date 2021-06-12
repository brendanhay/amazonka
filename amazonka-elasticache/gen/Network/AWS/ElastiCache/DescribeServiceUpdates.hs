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
-- Module      : Network.AWS.ElastiCache.DescribeServiceUpdates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details of the service updates
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeServiceUpdates
  ( -- * Creating a Request
    DescribeServiceUpdates (..),
    newDescribeServiceUpdates,

    -- * Request Lenses
    describeServiceUpdates_serviceUpdateStatus,
    describeServiceUpdates_serviceUpdateName,
    describeServiceUpdates_marker,
    describeServiceUpdates_maxRecords,

    -- * Destructuring the Response
    DescribeServiceUpdatesResponse (..),
    newDescribeServiceUpdatesResponse,

    -- * Response Lenses
    describeServiceUpdatesResponse_serviceUpdates,
    describeServiceUpdatesResponse_marker,
    describeServiceUpdatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeServiceUpdates' smart constructor.
data DescribeServiceUpdates = DescribeServiceUpdates'
  { -- | The status of the service update
    serviceUpdateStatus :: Core.Maybe [ServiceUpdateStatus],
    -- | The unique ID of the service update
    serviceUpdateName :: Core.Maybe Core.Text,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeServiceUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceUpdateStatus', 'describeServiceUpdates_serviceUpdateStatus' - The status of the service update
--
-- 'serviceUpdateName', 'describeServiceUpdates_serviceUpdateName' - The unique ID of the service update
--
-- 'marker', 'describeServiceUpdates_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeServiceUpdates_maxRecords' - The maximum number of records to include in the response
newDescribeServiceUpdates ::
  DescribeServiceUpdates
newDescribeServiceUpdates =
  DescribeServiceUpdates'
    { serviceUpdateStatus =
        Core.Nothing,
      serviceUpdateName = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The status of the service update
describeServiceUpdates_serviceUpdateStatus :: Lens.Lens' DescribeServiceUpdates (Core.Maybe [ServiceUpdateStatus])
describeServiceUpdates_serviceUpdateStatus = Lens.lens (\DescribeServiceUpdates' {serviceUpdateStatus} -> serviceUpdateStatus) (\s@DescribeServiceUpdates' {} a -> s {serviceUpdateStatus = a} :: DescribeServiceUpdates) Core.. Lens.mapping Lens._Coerce

-- | The unique ID of the service update
describeServiceUpdates_serviceUpdateName :: Lens.Lens' DescribeServiceUpdates (Core.Maybe Core.Text)
describeServiceUpdates_serviceUpdateName = Lens.lens (\DescribeServiceUpdates' {serviceUpdateName} -> serviceUpdateName) (\s@DescribeServiceUpdates' {} a -> s {serviceUpdateName = a} :: DescribeServiceUpdates)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeServiceUpdates_marker :: Lens.Lens' DescribeServiceUpdates (Core.Maybe Core.Text)
describeServiceUpdates_marker = Lens.lens (\DescribeServiceUpdates' {marker} -> marker) (\s@DescribeServiceUpdates' {} a -> s {marker = a} :: DescribeServiceUpdates)

-- | The maximum number of records to include in the response
describeServiceUpdates_maxRecords :: Lens.Lens' DescribeServiceUpdates (Core.Maybe Core.Int)
describeServiceUpdates_maxRecords = Lens.lens (\DescribeServiceUpdates' {maxRecords} -> maxRecords) (\s@DescribeServiceUpdates' {} a -> s {maxRecords = a} :: DescribeServiceUpdates)

instance Core.AWSPager DescribeServiceUpdates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeServiceUpdatesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeServiceUpdatesResponse_serviceUpdates
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeServiceUpdates_marker
          Lens..~ rs
          Lens.^? describeServiceUpdatesResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeServiceUpdates where
  type
    AWSResponse DescribeServiceUpdates =
      DescribeServiceUpdatesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeServiceUpdatesResult"
      ( \s h x ->
          DescribeServiceUpdatesResponse'
            Core.<$> ( x Core..@? "ServiceUpdates" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "ServiceUpdate")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeServiceUpdates

instance Core.NFData DescribeServiceUpdates

instance Core.ToHeaders DescribeServiceUpdates where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeServiceUpdates where
  toPath = Core.const "/"

instance Core.ToQuery DescribeServiceUpdates where
  toQuery DescribeServiceUpdates' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeServiceUpdates" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "ServiceUpdateStatus"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> serviceUpdateStatus
            ),
        "ServiceUpdateName" Core.=: serviceUpdateName,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeServiceUpdatesResponse' smart constructor.
data DescribeServiceUpdatesResponse = DescribeServiceUpdatesResponse'
  { -- | A list of service updates
    serviceUpdates :: Core.Maybe [ServiceUpdate],
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeServiceUpdatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceUpdates', 'describeServiceUpdatesResponse_serviceUpdates' - A list of service updates
--
-- 'marker', 'describeServiceUpdatesResponse_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeServiceUpdatesResponse_httpStatus' - The response's http status code.
newDescribeServiceUpdatesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeServiceUpdatesResponse
newDescribeServiceUpdatesResponse pHttpStatus_ =
  DescribeServiceUpdatesResponse'
    { serviceUpdates =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of service updates
describeServiceUpdatesResponse_serviceUpdates :: Lens.Lens' DescribeServiceUpdatesResponse (Core.Maybe [ServiceUpdate])
describeServiceUpdatesResponse_serviceUpdates = Lens.lens (\DescribeServiceUpdatesResponse' {serviceUpdates} -> serviceUpdates) (\s@DescribeServiceUpdatesResponse' {} a -> s {serviceUpdates = a} :: DescribeServiceUpdatesResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeServiceUpdatesResponse_marker :: Lens.Lens' DescribeServiceUpdatesResponse (Core.Maybe Core.Text)
describeServiceUpdatesResponse_marker = Lens.lens (\DescribeServiceUpdatesResponse' {marker} -> marker) (\s@DescribeServiceUpdatesResponse' {} a -> s {marker = a} :: DescribeServiceUpdatesResponse)

-- | The response's http status code.
describeServiceUpdatesResponse_httpStatus :: Lens.Lens' DescribeServiceUpdatesResponse Core.Int
describeServiceUpdatesResponse_httpStatus = Lens.lens (\DescribeServiceUpdatesResponse' {httpStatus} -> httpStatus) (\s@DescribeServiceUpdatesResponse' {} a -> s {httpStatus = a} :: DescribeServiceUpdatesResponse)

instance Core.NFData DescribeServiceUpdatesResponse
