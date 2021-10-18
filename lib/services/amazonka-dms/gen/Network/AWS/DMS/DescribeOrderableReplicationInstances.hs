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
    describeOrderableReplicationInstances_maxRecords,
    describeOrderableReplicationInstances_marker,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeOrderableReplicationInstances' smart constructor.
data DescribeOrderableReplicationInstances = DescribeOrderableReplicationInstances'
  { -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrderableReplicationInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRecords', 'describeOrderableReplicationInstances_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'marker', 'describeOrderableReplicationInstances_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
newDescribeOrderableReplicationInstances ::
  DescribeOrderableReplicationInstances
newDescribeOrderableReplicationInstances =
  DescribeOrderableReplicationInstances'
    { maxRecords =
        Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeOrderableReplicationInstances_maxRecords :: Lens.Lens' DescribeOrderableReplicationInstances (Prelude.Maybe Prelude.Int)
describeOrderableReplicationInstances_maxRecords = Lens.lens (\DescribeOrderableReplicationInstances' {maxRecords} -> maxRecords) (\s@DescribeOrderableReplicationInstances' {} a -> s {maxRecords = a} :: DescribeOrderableReplicationInstances)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeOrderableReplicationInstances_marker :: Lens.Lens' DescribeOrderableReplicationInstances (Prelude.Maybe Prelude.Text)
describeOrderableReplicationInstances_marker = Lens.lens (\DescribeOrderableReplicationInstances' {marker} -> marker) (\s@DescribeOrderableReplicationInstances' {} a -> s {marker = a} :: DescribeOrderableReplicationInstances)

instance
  Core.AWSPager
    DescribeOrderableReplicationInstances
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOrderableReplicationInstancesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOrderableReplicationInstancesResponse_orderableReplicationInstances
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeOrderableReplicationInstances_marker
          Lens..~ rs
          Lens.^? describeOrderableReplicationInstancesResponse_marker
            Prelude.. Lens._Just

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
            Prelude.<$> ( x Core..?> "OrderableReplicationInstances"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "Marker")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeOrderableReplicationInstances

instance
  Prelude.NFData
    DescribeOrderableReplicationInstances

instance
  Core.ToHeaders
    DescribeOrderableReplicationInstances
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeOrderableReplicationInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeOrderableReplicationInstances
  where
  toJSON DescribeOrderableReplicationInstances' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxRecords" Core..=) Prelude.<$> maxRecords,
            ("Marker" Core..=) Prelude.<$> marker
          ]
      )

instance
  Core.ToPath
    DescribeOrderableReplicationInstances
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeOrderableReplicationInstances
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeOrderableReplicationInstancesResponse' smart constructor.
data DescribeOrderableReplicationInstancesResponse = DescribeOrderableReplicationInstancesResponse'
  { -- | The order-able replication instances available.
    orderableReplicationInstances :: Prelude.Maybe [OrderableReplicationInstance],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeOrderableReplicationInstancesResponse
newDescribeOrderableReplicationInstancesResponse
  pHttpStatus_ =
    DescribeOrderableReplicationInstancesResponse'
      { orderableReplicationInstances =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The order-able replication instances available.
describeOrderableReplicationInstancesResponse_orderableReplicationInstances :: Lens.Lens' DescribeOrderableReplicationInstancesResponse (Prelude.Maybe [OrderableReplicationInstance])
describeOrderableReplicationInstancesResponse_orderableReplicationInstances = Lens.lens (\DescribeOrderableReplicationInstancesResponse' {orderableReplicationInstances} -> orderableReplicationInstances) (\s@DescribeOrderableReplicationInstancesResponse' {} a -> s {orderableReplicationInstances = a} :: DescribeOrderableReplicationInstancesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeOrderableReplicationInstancesResponse_marker :: Lens.Lens' DescribeOrderableReplicationInstancesResponse (Prelude.Maybe Prelude.Text)
describeOrderableReplicationInstancesResponse_marker = Lens.lens (\DescribeOrderableReplicationInstancesResponse' {marker} -> marker) (\s@DescribeOrderableReplicationInstancesResponse' {} a -> s {marker = a} :: DescribeOrderableReplicationInstancesResponse)

-- | The response's http status code.
describeOrderableReplicationInstancesResponse_httpStatus :: Lens.Lens' DescribeOrderableReplicationInstancesResponse Prelude.Int
describeOrderableReplicationInstancesResponse_httpStatus = Lens.lens (\DescribeOrderableReplicationInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeOrderableReplicationInstancesResponse' {} a -> s {httpStatus = a} :: DescribeOrderableReplicationInstancesResponse)

instance
  Prelude.NFData
    DescribeOrderableReplicationInstancesResponse
