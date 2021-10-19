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
    describeReplicationSubnetGroupsResponse_marker,
    describeReplicationSubnetGroupsResponse_replicationSubnetGroups,
    describeReplicationSubnetGroupsResponse_httpStatus,
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
-- /See:/ 'newDescribeReplicationSubnetGroups' smart constructor.
data DescribeReplicationSubnetGroups = DescribeReplicationSubnetGroups'
  { -- | Filters applied to replication subnet groups.
    --
    -- Valid filter names: replication-subnet-group-id
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
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | Filters applied to replication subnet groups.
--
-- Valid filter names: replication-subnet-group-id
describeReplicationSubnetGroups_filters :: Lens.Lens' DescribeReplicationSubnetGroups (Prelude.Maybe [Filter])
describeReplicationSubnetGroups_filters = Lens.lens (\DescribeReplicationSubnetGroups' {filters} -> filters) (\s@DescribeReplicationSubnetGroups' {} a -> s {filters = a} :: DescribeReplicationSubnetGroups) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationSubnetGroups_marker :: Lens.Lens' DescribeReplicationSubnetGroups (Prelude.Maybe Prelude.Text)
describeReplicationSubnetGroups_marker = Lens.lens (\DescribeReplicationSubnetGroups' {marker} -> marker) (\s@DescribeReplicationSubnetGroups' {} a -> s {marker = a} :: DescribeReplicationSubnetGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeReplicationSubnetGroups_maxRecords :: Lens.Lens' DescribeReplicationSubnetGroups (Prelude.Maybe Prelude.Int)
describeReplicationSubnetGroups_maxRecords = Lens.lens (\DescribeReplicationSubnetGroups' {maxRecords} -> maxRecords) (\s@DescribeReplicationSubnetGroups' {} a -> s {maxRecords = a} :: DescribeReplicationSubnetGroups)

instance
  Core.AWSPager
    DescribeReplicationSubnetGroups
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReplicationSubnetGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReplicationSubnetGroupsResponse_replicationSubnetGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeReplicationSubnetGroups_marker
          Lens..~ rs
          Lens.^? describeReplicationSubnetGroupsResponse_marker
            Prelude.. Lens._Just

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
            Prelude.<$> (x Core..?> "Marker")
            Prelude.<*> ( x Core..?> "ReplicationSubnetGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReplicationSubnetGroups

instance
  Prelude.NFData
    DescribeReplicationSubnetGroups

instance
  Core.ToHeaders
    DescribeReplicationSubnetGroups
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeReplicationSubnetGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeReplicationSubnetGroups where
  toJSON DescribeReplicationSubnetGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Filters" Core..=) Prelude.<$> filters,
            ("Marker" Core..=) Prelude.<$> marker,
            ("MaxRecords" Core..=) Prelude.<$> maxRecords
          ]
      )

instance Core.ToPath DescribeReplicationSubnetGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeReplicationSubnetGroups where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeReplicationSubnetGroupsResponse' smart constructor.
data DescribeReplicationSubnetGroupsResponse = DescribeReplicationSubnetGroupsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A description of the replication subnet groups.
    replicationSubnetGroups :: Prelude.Maybe [ReplicationSubnetGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationSubnetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeReplicationSubnetGroupsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'replicationSubnetGroups', 'describeReplicationSubnetGroupsResponse_replicationSubnetGroups' - A description of the replication subnet groups.
--
-- 'httpStatus', 'describeReplicationSubnetGroupsResponse_httpStatus' - The response's http status code.
newDescribeReplicationSubnetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplicationSubnetGroupsResponse
newDescribeReplicationSubnetGroupsResponse
  pHttpStatus_ =
    DescribeReplicationSubnetGroupsResponse'
      { marker =
          Prelude.Nothing,
        replicationSubnetGroups =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationSubnetGroupsResponse_marker :: Lens.Lens' DescribeReplicationSubnetGroupsResponse (Prelude.Maybe Prelude.Text)
describeReplicationSubnetGroupsResponse_marker = Lens.lens (\DescribeReplicationSubnetGroupsResponse' {marker} -> marker) (\s@DescribeReplicationSubnetGroupsResponse' {} a -> s {marker = a} :: DescribeReplicationSubnetGroupsResponse)

-- | A description of the replication subnet groups.
describeReplicationSubnetGroupsResponse_replicationSubnetGroups :: Lens.Lens' DescribeReplicationSubnetGroupsResponse (Prelude.Maybe [ReplicationSubnetGroup])
describeReplicationSubnetGroupsResponse_replicationSubnetGroups = Lens.lens (\DescribeReplicationSubnetGroupsResponse' {replicationSubnetGroups} -> replicationSubnetGroups) (\s@DescribeReplicationSubnetGroupsResponse' {} a -> s {replicationSubnetGroups = a} :: DescribeReplicationSubnetGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReplicationSubnetGroupsResponse_httpStatus :: Lens.Lens' DescribeReplicationSubnetGroupsResponse Prelude.Int
describeReplicationSubnetGroupsResponse_httpStatus = Lens.lens (\DescribeReplicationSubnetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationSubnetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationSubnetGroupsResponse)

instance
  Prelude.NFData
    DescribeReplicationSubnetGroupsResponse
