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
-- Module      : Amazonka.DMS.DescribeReplicationInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about replication instances for your account in the
-- current region.
--
-- This operation returns paginated results.
module Amazonka.DMS.DescribeReplicationInstances
  ( -- * Creating a Request
    DescribeReplicationInstances (..),
    newDescribeReplicationInstances,

    -- * Request Lenses
    describeReplicationInstances_filters,
    describeReplicationInstances_marker,
    describeReplicationInstances_maxRecords,

    -- * Destructuring the Response
    DescribeReplicationInstancesResponse (..),
    newDescribeReplicationInstancesResponse,

    -- * Response Lenses
    describeReplicationInstancesResponse_marker,
    describeReplicationInstancesResponse_replicationInstances,
    describeReplicationInstancesResponse_httpStatus,
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
-- /See:/ 'newDescribeReplicationInstances' smart constructor.
data DescribeReplicationInstances = DescribeReplicationInstances'
  { -- | Filters applied to replication instances.
    --
    -- Valid filter names: replication-instance-arn | replication-instance-id |
    -- replication-instance-class | engine-version
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
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
-- Create a value of 'DescribeReplicationInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeReplicationInstances_filters' - Filters applied to replication instances.
--
-- Valid filter names: replication-instance-arn | replication-instance-id |
-- replication-instance-class | engine-version
--
-- 'marker', 'describeReplicationInstances_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeReplicationInstances_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeReplicationInstances ::
  DescribeReplicationInstances
newDescribeReplicationInstances =
  DescribeReplicationInstances'
    { filters =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | Filters applied to replication instances.
--
-- Valid filter names: replication-instance-arn | replication-instance-id |
-- replication-instance-class | engine-version
describeReplicationInstances_filters :: Lens.Lens' DescribeReplicationInstances (Prelude.Maybe [Filter])
describeReplicationInstances_filters = Lens.lens (\DescribeReplicationInstances' {filters} -> filters) (\s@DescribeReplicationInstances' {} a -> s {filters = a} :: DescribeReplicationInstances) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationInstances_marker :: Lens.Lens' DescribeReplicationInstances (Prelude.Maybe Prelude.Text)
describeReplicationInstances_marker = Lens.lens (\DescribeReplicationInstances' {marker} -> marker) (\s@DescribeReplicationInstances' {} a -> s {marker = a} :: DescribeReplicationInstances)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeReplicationInstances_maxRecords :: Lens.Lens' DescribeReplicationInstances (Prelude.Maybe Prelude.Int)
describeReplicationInstances_maxRecords = Lens.lens (\DescribeReplicationInstances' {maxRecords} -> maxRecords) (\s@DescribeReplicationInstances' {} a -> s {maxRecords = a} :: DescribeReplicationInstances)

instance Core.AWSPager DescribeReplicationInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReplicationInstancesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReplicationInstancesResponse_replicationInstances
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeReplicationInstances_marker
          Lens..~ rs
          Lens.^? describeReplicationInstancesResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeReplicationInstances where
  type
    AWSResponse DescribeReplicationInstances =
      DescribeReplicationInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationInstancesResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> ( x Data..?> "ReplicationInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReplicationInstances
  where
  hashWithSalt _salt DescribeReplicationInstances' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeReplicationInstances where
  rnf DescribeReplicationInstances' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeReplicationInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeReplicationInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeReplicationInstances where
  toJSON DescribeReplicationInstances' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("Marker" Data..=) Prelude.<$> marker,
            ("MaxRecords" Data..=) Prelude.<$> maxRecords
          ]
      )

instance Data.ToPath DescribeReplicationInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeReplicationInstances where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeReplicationInstancesResponse' smart constructor.
data DescribeReplicationInstancesResponse = DescribeReplicationInstancesResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The replication instances described.
    replicationInstances :: Prelude.Maybe [ReplicationInstance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeReplicationInstancesResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'replicationInstances', 'describeReplicationInstancesResponse_replicationInstances' - The replication instances described.
--
-- 'httpStatus', 'describeReplicationInstancesResponse_httpStatus' - The response's http status code.
newDescribeReplicationInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplicationInstancesResponse
newDescribeReplicationInstancesResponse pHttpStatus_ =
  DescribeReplicationInstancesResponse'
    { marker =
        Prelude.Nothing,
      replicationInstances =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationInstancesResponse_marker :: Lens.Lens' DescribeReplicationInstancesResponse (Prelude.Maybe Prelude.Text)
describeReplicationInstancesResponse_marker = Lens.lens (\DescribeReplicationInstancesResponse' {marker} -> marker) (\s@DescribeReplicationInstancesResponse' {} a -> s {marker = a} :: DescribeReplicationInstancesResponse)

-- | The replication instances described.
describeReplicationInstancesResponse_replicationInstances :: Lens.Lens' DescribeReplicationInstancesResponse (Prelude.Maybe [ReplicationInstance])
describeReplicationInstancesResponse_replicationInstances = Lens.lens (\DescribeReplicationInstancesResponse' {replicationInstances} -> replicationInstances) (\s@DescribeReplicationInstancesResponse' {} a -> s {replicationInstances = a} :: DescribeReplicationInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReplicationInstancesResponse_httpStatus :: Lens.Lens' DescribeReplicationInstancesResponse Prelude.Int
describeReplicationInstancesResponse_httpStatus = Lens.lens (\DescribeReplicationInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationInstancesResponse' {} a -> s {httpStatus = a} :: DescribeReplicationInstancesResponse)

instance
  Prelude.NFData
    DescribeReplicationInstancesResponse
  where
  rnf DescribeReplicationInstancesResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf replicationInstances
      `Prelude.seq` Prelude.rnf httpStatus
