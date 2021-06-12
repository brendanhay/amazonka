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
-- Module      : Network.AWS.DMS.DescribeOrderableReplicationInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the replication instance types that can be
-- created in the specified region.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeOrderableReplicationInstances
  ( -- * Creating a Request
    DescribeOrderableReplicationInstances (..),
    newDescribeOrderableReplicationInstances,

    -- * Request Lenses
    describeOrderableReplicationInstances_marker,
    describeOrderableReplicationInstances_maxRecords,

    -- * Destructuring the Response
    DescribeOrderableReplicationInstancesResponse (..),
    newDescribeOrderableReplicationInstancesResponse,

    -- * Response Lenses
    describeOrderableReplicationInstancesResponse_orderableReplicationInstances,
    describeOrderableReplicationInstancesResponse_marker,
    describeOrderableReplicationInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeOrderableReplicationInstances' smart constructor.
data DescribeOrderableReplicationInstances = DescribeOrderableReplicationInstances'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeOrderableReplicationInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeOrderableReplicationInstances_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeOrderableReplicationInstances_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeOrderableReplicationInstances ::
  DescribeOrderableReplicationInstances
newDescribeOrderableReplicationInstances =
  DescribeOrderableReplicationInstances'
    { marker =
        Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeOrderableReplicationInstances_marker :: Lens.Lens' DescribeOrderableReplicationInstances (Core.Maybe Core.Text)
describeOrderableReplicationInstances_marker = Lens.lens (\DescribeOrderableReplicationInstances' {marker} -> marker) (\s@DescribeOrderableReplicationInstances' {} a -> s {marker = a} :: DescribeOrderableReplicationInstances)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeOrderableReplicationInstances_maxRecords :: Lens.Lens' DescribeOrderableReplicationInstances (Core.Maybe Core.Int)
describeOrderableReplicationInstances_maxRecords = Lens.lens (\DescribeOrderableReplicationInstances' {maxRecords} -> maxRecords) (\s@DescribeOrderableReplicationInstances' {} a -> s {maxRecords = a} :: DescribeOrderableReplicationInstances)

instance
  Core.AWSPager
    DescribeOrderableReplicationInstances
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOrderableReplicationInstancesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOrderableReplicationInstancesResponse_orderableReplicationInstances
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeOrderableReplicationInstances_marker
          Lens..~ rs
          Lens.^? describeOrderableReplicationInstancesResponse_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeOrderableReplicationInstances
  where
  type
    AWSResponse
      DescribeOrderableReplicationInstances =
      DescribeOrderableReplicationInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrderableReplicationInstancesResponse'
            Core.<$> ( x Core..?> "OrderableReplicationInstances"
                         Core..!@ Core.mempty
                     )
              Core.<*> (x Core..?> "Marker")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeOrderableReplicationInstances

instance
  Core.NFData
    DescribeOrderableReplicationInstances

instance
  Core.ToHeaders
    DescribeOrderableReplicationInstances
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeOrderableReplicationInstances" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeOrderableReplicationInstances
  where
  toJSON DescribeOrderableReplicationInstances' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords
          ]
      )

instance
  Core.ToPath
    DescribeOrderableReplicationInstances
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeOrderableReplicationInstances
  where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeOrderableReplicationInstancesResponse' smart constructor.
data DescribeOrderableReplicationInstancesResponse = DescribeOrderableReplicationInstancesResponse'
  { -- | The order-able replication instances available.
    orderableReplicationInstances :: Core.Maybe [OrderableReplicationInstance],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeOrderableReplicationInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'orderableReplicationInstances', 'describeOrderableReplicationInstancesResponse_orderableReplicationInstances' - The order-able replication instances available.
--
-- 'marker', 'describeOrderableReplicationInstancesResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeOrderableReplicationInstancesResponse_httpStatus' - The response's http status code.
newDescribeOrderableReplicationInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeOrderableReplicationInstancesResponse
newDescribeOrderableReplicationInstancesResponse
  pHttpStatus_ =
    DescribeOrderableReplicationInstancesResponse'
      { orderableReplicationInstances =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The order-able replication instances available.
describeOrderableReplicationInstancesResponse_orderableReplicationInstances :: Lens.Lens' DescribeOrderableReplicationInstancesResponse (Core.Maybe [OrderableReplicationInstance])
describeOrderableReplicationInstancesResponse_orderableReplicationInstances = Lens.lens (\DescribeOrderableReplicationInstancesResponse' {orderableReplicationInstances} -> orderableReplicationInstances) (\s@DescribeOrderableReplicationInstancesResponse' {} a -> s {orderableReplicationInstances = a} :: DescribeOrderableReplicationInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeOrderableReplicationInstancesResponse_marker :: Lens.Lens' DescribeOrderableReplicationInstancesResponse (Core.Maybe Core.Text)
describeOrderableReplicationInstancesResponse_marker = Lens.lens (\DescribeOrderableReplicationInstancesResponse' {marker} -> marker) (\s@DescribeOrderableReplicationInstancesResponse' {} a -> s {marker = a} :: DescribeOrderableReplicationInstancesResponse)

-- | The response's http status code.
describeOrderableReplicationInstancesResponse_httpStatus :: Lens.Lens' DescribeOrderableReplicationInstancesResponse Core.Int
describeOrderableReplicationInstancesResponse_httpStatus = Lens.lens (\DescribeOrderableReplicationInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeOrderableReplicationInstancesResponse' {} a -> s {httpStatus = a} :: DescribeOrderableReplicationInstancesResponse)

instance
  Core.NFData
    DescribeOrderableReplicationInstancesResponse
