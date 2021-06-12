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
-- Module      : Network.AWS.DMS.DescribeReplicationSubnetGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the replication subnet groups.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeReplicationSubnetGroups
  ( -- * Creating a Request
    DescribeReplicationSubnetGroups (..),
    newDescribeReplicationSubnetGroups,

    -- * Request Lenses
    describeReplicationSubnetGroups_filters,
    describeReplicationSubnetGroups_marker,
    describeReplicationSubnetGroups_maxRecords,

    -- * Destructuring the Response
    DescribeReplicationSubnetGroupsResponse (..),
    newDescribeReplicationSubnetGroupsResponse,

    -- * Response Lenses
    describeReplicationSubnetGroupsResponse_replicationSubnetGroups,
    describeReplicationSubnetGroupsResponse_marker,
    describeReplicationSubnetGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeReplicationSubnetGroups' smart constructor.
data DescribeReplicationSubnetGroups = DescribeReplicationSubnetGroups'
  { -- | Filters applied to replication subnet groups.
    --
    -- Valid filter names: replication-subnet-group-id
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
-- Create a value of 'DescribeReplicationSubnetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeReplicationSubnetGroups_filters' - Filters applied to replication subnet groups.
--
-- Valid filter names: replication-subnet-group-id
--
-- 'marker', 'describeReplicationSubnetGroups_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeReplicationSubnetGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeReplicationSubnetGroups ::
  DescribeReplicationSubnetGroups
newDescribeReplicationSubnetGroups =
  DescribeReplicationSubnetGroups'
    { filters =
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | Filters applied to replication subnet groups.
--
-- Valid filter names: replication-subnet-group-id
describeReplicationSubnetGroups_filters :: Lens.Lens' DescribeReplicationSubnetGroups (Core.Maybe [Filter])
describeReplicationSubnetGroups_filters = Lens.lens (\DescribeReplicationSubnetGroups' {filters} -> filters) (\s@DescribeReplicationSubnetGroups' {} a -> s {filters = a} :: DescribeReplicationSubnetGroups) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationSubnetGroups_marker :: Lens.Lens' DescribeReplicationSubnetGroups (Core.Maybe Core.Text)
describeReplicationSubnetGroups_marker = Lens.lens (\DescribeReplicationSubnetGroups' {marker} -> marker) (\s@DescribeReplicationSubnetGroups' {} a -> s {marker = a} :: DescribeReplicationSubnetGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeReplicationSubnetGroups_maxRecords :: Lens.Lens' DescribeReplicationSubnetGroups (Core.Maybe Core.Int)
describeReplicationSubnetGroups_maxRecords = Lens.lens (\DescribeReplicationSubnetGroups' {maxRecords} -> maxRecords) (\s@DescribeReplicationSubnetGroups' {} a -> s {maxRecords = a} :: DescribeReplicationSubnetGroups)

instance
  Core.AWSPager
    DescribeReplicationSubnetGroups
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReplicationSubnetGroupsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReplicationSubnetGroupsResponse_replicationSubnetGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeReplicationSubnetGroups_marker
          Lens..~ rs
          Lens.^? describeReplicationSubnetGroupsResponse_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeReplicationSubnetGroups
  where
  type
    AWSResponse DescribeReplicationSubnetGroups =
      DescribeReplicationSubnetGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationSubnetGroupsResponse'
            Core.<$> ( x Core..?> "ReplicationSubnetGroups"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeReplicationSubnetGroups

instance Core.NFData DescribeReplicationSubnetGroups

instance
  Core.ToHeaders
    DescribeReplicationSubnetGroups
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeReplicationSubnetGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeReplicationSubnetGroups where
  toJSON DescribeReplicationSubnetGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords
          ]
      )

instance Core.ToPath DescribeReplicationSubnetGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeReplicationSubnetGroups where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeReplicationSubnetGroupsResponse' smart constructor.
data DescribeReplicationSubnetGroupsResponse = DescribeReplicationSubnetGroupsResponse'
  { -- | A description of the replication subnet groups.
    replicationSubnetGroups :: Core.Maybe [ReplicationSubnetGroup],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReplicationSubnetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationSubnetGroups', 'describeReplicationSubnetGroupsResponse_replicationSubnetGroups' - A description of the replication subnet groups.
--
-- 'marker', 'describeReplicationSubnetGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeReplicationSubnetGroupsResponse_httpStatus' - The response's http status code.
newDescribeReplicationSubnetGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeReplicationSubnetGroupsResponse
newDescribeReplicationSubnetGroupsResponse
  pHttpStatus_ =
    DescribeReplicationSubnetGroupsResponse'
      { replicationSubnetGroups =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A description of the replication subnet groups.
describeReplicationSubnetGroupsResponse_replicationSubnetGroups :: Lens.Lens' DescribeReplicationSubnetGroupsResponse (Core.Maybe [ReplicationSubnetGroup])
describeReplicationSubnetGroupsResponse_replicationSubnetGroups = Lens.lens (\DescribeReplicationSubnetGroupsResponse' {replicationSubnetGroups} -> replicationSubnetGroups) (\s@DescribeReplicationSubnetGroupsResponse' {} a -> s {replicationSubnetGroups = a} :: DescribeReplicationSubnetGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationSubnetGroupsResponse_marker :: Lens.Lens' DescribeReplicationSubnetGroupsResponse (Core.Maybe Core.Text)
describeReplicationSubnetGroupsResponse_marker = Lens.lens (\DescribeReplicationSubnetGroupsResponse' {marker} -> marker) (\s@DescribeReplicationSubnetGroupsResponse' {} a -> s {marker = a} :: DescribeReplicationSubnetGroupsResponse)

-- | The response's http status code.
describeReplicationSubnetGroupsResponse_httpStatus :: Lens.Lens' DescribeReplicationSubnetGroupsResponse Core.Int
describeReplicationSubnetGroupsResponse_httpStatus = Lens.lens (\DescribeReplicationSubnetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationSubnetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationSubnetGroupsResponse)

instance
  Core.NFData
    DescribeReplicationSubnetGroupsResponse
