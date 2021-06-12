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
    describeReplicationInstances_marker,
    describeReplicationInstances_maxRecords,

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
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
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
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | Filters applied to replication instances.
--
-- Valid filter names: replication-instance-arn | replication-instance-id |
-- replication-instance-class | engine-version
describeReplicationInstances_filters :: Lens.Lens' DescribeReplicationInstances (Core.Maybe [Filter])
describeReplicationInstances_filters = Lens.lens (\DescribeReplicationInstances' {filters} -> filters) (\s@DescribeReplicationInstances' {} a -> s {filters = a} :: DescribeReplicationInstances) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationInstances_marker :: Lens.Lens' DescribeReplicationInstances (Core.Maybe Core.Text)
describeReplicationInstances_marker = Lens.lens (\DescribeReplicationInstances' {marker} -> marker) (\s@DescribeReplicationInstances' {} a -> s {marker = a} :: DescribeReplicationInstances)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeReplicationInstances_maxRecords :: Lens.Lens' DescribeReplicationInstances (Core.Maybe Core.Int)
describeReplicationInstances_maxRecords = Lens.lens (\DescribeReplicationInstances' {maxRecords} -> maxRecords) (\s@DescribeReplicationInstances' {} a -> s {maxRecords = a} :: DescribeReplicationInstances)

instance Core.AWSPager DescribeReplicationInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReplicationInstancesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReplicationInstancesResponse_replicationInstances
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeReplicationInstances_marker
          Lens..~ rs
          Lens.^? describeReplicationInstancesResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeReplicationInstances where
  type
    AWSResponse DescribeReplicationInstances =
      DescribeReplicationInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationInstancesResponse'
            Core.<$> ( x Core..?> "ReplicationInstances"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeReplicationInstances

instance Core.NFData DescribeReplicationInstances

instance Core.ToHeaders DescribeReplicationInstances where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeReplicationInstances" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeReplicationInstances where
  toJSON DescribeReplicationInstances' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords
          ]
      )

instance Core.ToPath DescribeReplicationInstances where
  toPath = Core.const "/"

instance Core.ToQuery DescribeReplicationInstances where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeReplicationInstancesResponse' smart constructor.
data DescribeReplicationInstancesResponse = DescribeReplicationInstancesResponse'
  { -- | The replication instances described.
    replicationInstances :: Core.Maybe [ReplicationInstance],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeReplicationInstancesResponse
newDescribeReplicationInstancesResponse pHttpStatus_ =
  DescribeReplicationInstancesResponse'
    { replicationInstances =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication instances described.
describeReplicationInstancesResponse_replicationInstances :: Lens.Lens' DescribeReplicationInstancesResponse (Core.Maybe [ReplicationInstance])
describeReplicationInstancesResponse_replicationInstances = Lens.lens (\DescribeReplicationInstancesResponse' {replicationInstances} -> replicationInstances) (\s@DescribeReplicationInstancesResponse' {} a -> s {replicationInstances = a} :: DescribeReplicationInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationInstancesResponse_marker :: Lens.Lens' DescribeReplicationInstancesResponse (Core.Maybe Core.Text)
describeReplicationInstancesResponse_marker = Lens.lens (\DescribeReplicationInstancesResponse' {marker} -> marker) (\s@DescribeReplicationInstancesResponse' {} a -> s {marker = a} :: DescribeReplicationInstancesResponse)

-- | The response's http status code.
describeReplicationInstancesResponse_httpStatus :: Lens.Lens' DescribeReplicationInstancesResponse Core.Int
describeReplicationInstancesResponse_httpStatus = Lens.lens (\DescribeReplicationInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationInstancesResponse' {} a -> s {httpStatus = a} :: DescribeReplicationInstancesResponse)

instance
  Core.NFData
    DescribeReplicationInstancesResponse
