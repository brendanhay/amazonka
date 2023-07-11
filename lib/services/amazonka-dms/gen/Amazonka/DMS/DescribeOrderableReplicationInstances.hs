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
-- Module      : Amazonka.DMS.DescribeOrderableReplicationInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the replication instance types that can be
-- created in the specified region.
--
-- This operation returns paginated results.
module Amazonka.DMS.DescribeOrderableReplicationInstances
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
    describeOrderableReplicationInstancesResponse_marker,
    describeOrderableReplicationInstancesResponse_orderableReplicationInstances,
    describeOrderableReplicationInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeOrderableReplicationInstances' smart constructor.
data DescribeOrderableReplicationInstances = DescribeOrderableReplicationInstances'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
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
        Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeOrderableReplicationInstances_marker :: Lens.Lens' DescribeOrderableReplicationInstances (Prelude.Maybe Prelude.Text)
describeOrderableReplicationInstances_marker = Lens.lens (\DescribeOrderableReplicationInstances' {marker} -> marker) (\s@DescribeOrderableReplicationInstances' {} a -> s {marker = a} :: DescribeOrderableReplicationInstances)

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
        Prelude.Just
          Prelude.$ rq
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrderableReplicationInstancesResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> ( x
                            Data..?> "OrderableReplicationInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeOrderableReplicationInstances
  where
  hashWithSalt
    _salt
    DescribeOrderableReplicationInstances' {..} =
      _salt
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords

instance
  Prelude.NFData
    DescribeOrderableReplicationInstances
  where
  rnf DescribeOrderableReplicationInstances' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance
  Data.ToHeaders
    DescribeOrderableReplicationInstances
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeOrderableReplicationInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeOrderableReplicationInstances
  where
  toJSON DescribeOrderableReplicationInstances' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Marker" Data..=) Prelude.<$> marker,
            ("MaxRecords" Data..=) Prelude.<$> maxRecords
          ]
      )

instance
  Data.ToPath
    DescribeOrderableReplicationInstances
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeOrderableReplicationInstances
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeOrderableReplicationInstancesResponse' smart constructor.
data DescribeOrderableReplicationInstancesResponse = DescribeOrderableReplicationInstancesResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The order-able replication instances available.
    orderableReplicationInstances :: Prelude.Maybe [OrderableReplicationInstance],
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
-- 'marker', 'describeOrderableReplicationInstancesResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'orderableReplicationInstances', 'describeOrderableReplicationInstancesResponse_orderableReplicationInstances' - The order-able replication instances available.
--
-- 'httpStatus', 'describeOrderableReplicationInstancesResponse_httpStatus' - The response's http status code.
newDescribeOrderableReplicationInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOrderableReplicationInstancesResponse
newDescribeOrderableReplicationInstancesResponse
  pHttpStatus_ =
    DescribeOrderableReplicationInstancesResponse'
      { marker =
          Prelude.Nothing,
        orderableReplicationInstances =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeOrderableReplicationInstancesResponse_marker :: Lens.Lens' DescribeOrderableReplicationInstancesResponse (Prelude.Maybe Prelude.Text)
describeOrderableReplicationInstancesResponse_marker = Lens.lens (\DescribeOrderableReplicationInstancesResponse' {marker} -> marker) (\s@DescribeOrderableReplicationInstancesResponse' {} a -> s {marker = a} :: DescribeOrderableReplicationInstancesResponse)

-- | The order-able replication instances available.
describeOrderableReplicationInstancesResponse_orderableReplicationInstances :: Lens.Lens' DescribeOrderableReplicationInstancesResponse (Prelude.Maybe [OrderableReplicationInstance])
describeOrderableReplicationInstancesResponse_orderableReplicationInstances = Lens.lens (\DescribeOrderableReplicationInstancesResponse' {orderableReplicationInstances} -> orderableReplicationInstances) (\s@DescribeOrderableReplicationInstancesResponse' {} a -> s {orderableReplicationInstances = a} :: DescribeOrderableReplicationInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeOrderableReplicationInstancesResponse_httpStatus :: Lens.Lens' DescribeOrderableReplicationInstancesResponse Prelude.Int
describeOrderableReplicationInstancesResponse_httpStatus = Lens.lens (\DescribeOrderableReplicationInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeOrderableReplicationInstancesResponse' {} a -> s {httpStatus = a} :: DescribeOrderableReplicationInstancesResponse)

instance
  Prelude.NFData
    DescribeOrderableReplicationInstancesResponse
  where
  rnf
    DescribeOrderableReplicationInstancesResponse' {..} =
      Prelude.rnf marker
        `Prelude.seq` Prelude.rnf orderableReplicationInstances
        `Prelude.seq` Prelude.rnf httpStatus
