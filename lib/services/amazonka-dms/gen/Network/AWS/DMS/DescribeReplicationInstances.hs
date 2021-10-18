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
-- Module      : Network.AWS.DMS.DescribeReplicationInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about replication instances for your account in the
-- current region.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeReplicationInstances
  ( -- * Creating a Request
    DescribeReplicationInstances (..),
    newDescribeReplicationInstances,

    -- * Request Lenses
    describeReplicationInstances_filters,
    describeReplicationInstances_maxRecords,
    describeReplicationInstances_marker,

    -- * Destructuring the Response
    DescribeReplicationInstancesResponse (..),
    newDescribeReplicationInstancesResponse,

    -- * Response Lenses
    describeReplicationInstancesResponse_replicationInstances,
    describeReplicationInstancesResponse_marker,
    describeReplicationInstancesResponse_httpStatus,
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
-- /See:/ 'newDescribeReplicationInstances' smart constructor.
data DescribeReplicationInstances = DescribeReplicationInstances'
  { -- | Filters applied to replication instances.
    --
    -- Valid filter names: replication-instance-arn | replication-instance-id |
    -- replication-instance-class | engine-version
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of records to include in the response. If more
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
-- 'maxRecords', 'describeReplicationInstances_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'marker', 'describeReplicationInstances_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
newDescribeReplicationInstances ::
  DescribeReplicationInstances
newDescribeReplicationInstances =
  DescribeReplicationInstances'
    { filters =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | Filters applied to replication instances.
--
-- Valid filter names: replication-instance-arn | replication-instance-id |
-- replication-instance-class | engine-version
describeReplicationInstances_filters :: Lens.Lens' DescribeReplicationInstances (Prelude.Maybe [Filter])
describeReplicationInstances_filters = Lens.lens (\DescribeReplicationInstances' {filters} -> filters) (\s@DescribeReplicationInstances' {} a -> s {filters = a} :: DescribeReplicationInstances) Prelude.. Lens.mapping Lens._Coerce

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

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationInstances_marker :: Lens.Lens' DescribeReplicationInstances (Prelude.Maybe Prelude.Text)
describeReplicationInstances_marker = Lens.lens (\DescribeReplicationInstances' {marker} -> marker) (\s@DescribeReplicationInstances' {} a -> s {marker = a} :: DescribeReplicationInstances)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationInstancesResponse'
            Prelude.<$> ( x Core..?> "ReplicationInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReplicationInstances

instance Prelude.NFData DescribeReplicationInstances

instance Core.ToHeaders DescribeReplicationInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeReplicationInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeReplicationInstances where
  toJSON DescribeReplicationInstances' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Filters" Core..=) Prelude.<$> filters,
            ("MaxRecords" Core..=) Prelude.<$> maxRecords,
            ("Marker" Core..=) Prelude.<$> marker
          ]
      )

instance Core.ToPath DescribeReplicationInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeReplicationInstances where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeReplicationInstancesResponse' smart constructor.
data DescribeReplicationInstancesResponse = DescribeReplicationInstancesResponse'
  { -- | The replication instances described.
    replicationInstances :: Prelude.Maybe [ReplicationInstance],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
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
-- 'replicationInstances', 'describeReplicationInstancesResponse_replicationInstances' - The replication instances described.
--
-- 'marker', 'describeReplicationInstancesResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeReplicationInstancesResponse_httpStatus' - The response's http status code.
newDescribeReplicationInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplicationInstancesResponse
newDescribeReplicationInstancesResponse pHttpStatus_ =
  DescribeReplicationInstancesResponse'
    { replicationInstances =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication instances described.
describeReplicationInstancesResponse_replicationInstances :: Lens.Lens' DescribeReplicationInstancesResponse (Prelude.Maybe [ReplicationInstance])
describeReplicationInstancesResponse_replicationInstances = Lens.lens (\DescribeReplicationInstancesResponse' {replicationInstances} -> replicationInstances) (\s@DescribeReplicationInstancesResponse' {} a -> s {replicationInstances = a} :: DescribeReplicationInstancesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationInstancesResponse_marker :: Lens.Lens' DescribeReplicationInstancesResponse (Prelude.Maybe Prelude.Text)
describeReplicationInstancesResponse_marker = Lens.lens (\DescribeReplicationInstancesResponse' {marker} -> marker) (\s@DescribeReplicationInstancesResponse' {} a -> s {marker = a} :: DescribeReplicationInstancesResponse)

-- | The response's http status code.
describeReplicationInstancesResponse_httpStatus :: Lens.Lens' DescribeReplicationInstancesResponse Prelude.Int
describeReplicationInstancesResponse_httpStatus = Lens.lens (\DescribeReplicationInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationInstancesResponse' {} a -> s {httpStatus = a} :: DescribeReplicationInstancesResponse)

instance
  Prelude.NFData
    DescribeReplicationInstancesResponse
