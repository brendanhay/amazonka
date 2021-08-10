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
-- Module      : Network.AWS.ElastiCache.DescribeReplicationGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a particular replication group. If no
-- identifier is specified, @DescribeReplicationGroups@ returns information
-- about all replication groups.
--
-- This operation is valid for Redis only.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeReplicationGroups
  ( -- * Creating a Request
    DescribeReplicationGroups (..),
    newDescribeReplicationGroups,

    -- * Request Lenses
    describeReplicationGroups_replicationGroupId,
    describeReplicationGroups_marker,
    describeReplicationGroups_maxRecords,

    -- * Destructuring the Response
    DescribeReplicationGroupsResponse (..),
    newDescribeReplicationGroupsResponse,

    -- * Response Lenses
    describeReplicationGroupsResponse_replicationGroups,
    describeReplicationGroupsResponse_marker,
    describeReplicationGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeReplicationGroups@ operation.
--
-- /See:/ 'newDescribeReplicationGroups' smart constructor.
data DescribeReplicationGroups = DescribeReplicationGroups'
  { -- | The identifier for the replication group to be described. This parameter
    -- is not case sensitive.
    --
    -- If you do not specify this parameter, information about all replication
    -- groups is returned.
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a marker is
    -- included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroupId', 'describeReplicationGroups_replicationGroupId' - The identifier for the replication group to be described. This parameter
-- is not case sensitive.
--
-- If you do not specify this parameter, information about all replication
-- groups is returned.
--
-- 'marker', 'describeReplicationGroups_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeReplicationGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
newDescribeReplicationGroups ::
  DescribeReplicationGroups
newDescribeReplicationGroups =
  DescribeReplicationGroups'
    { replicationGroupId =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The identifier for the replication group to be described. This parameter
-- is not case sensitive.
--
-- If you do not specify this parameter, information about all replication
-- groups is returned.
describeReplicationGroups_replicationGroupId :: Lens.Lens' DescribeReplicationGroups (Prelude.Maybe Prelude.Text)
describeReplicationGroups_replicationGroupId = Lens.lens (\DescribeReplicationGroups' {replicationGroupId} -> replicationGroupId) (\s@DescribeReplicationGroups' {} a -> s {replicationGroupId = a} :: DescribeReplicationGroups)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeReplicationGroups_marker :: Lens.Lens' DescribeReplicationGroups (Prelude.Maybe Prelude.Text)
describeReplicationGroups_marker = Lens.lens (\DescribeReplicationGroups' {marker} -> marker) (\s@DescribeReplicationGroups' {} a -> s {marker = a} :: DescribeReplicationGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
describeReplicationGroups_maxRecords :: Lens.Lens' DescribeReplicationGroups (Prelude.Maybe Prelude.Int)
describeReplicationGroups_maxRecords = Lens.lens (\DescribeReplicationGroups' {maxRecords} -> maxRecords) (\s@DescribeReplicationGroups' {} a -> s {maxRecords = a} :: DescribeReplicationGroups)

instance Core.AWSPager DescribeReplicationGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReplicationGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReplicationGroupsResponse_replicationGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeReplicationGroups_marker
          Lens..~ rs
          Lens.^? describeReplicationGroupsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeReplicationGroups where
  type
    AWSResponse DescribeReplicationGroups =
      DescribeReplicationGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeReplicationGroupsResult"
      ( \s h x ->
          DescribeReplicationGroupsResponse'
            Prelude.<$> ( x Core..@? "ReplicationGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "ReplicationGroup")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReplicationGroups

instance Prelude.NFData DescribeReplicationGroups

instance Core.ToHeaders DescribeReplicationGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeReplicationGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeReplicationGroups where
  toQuery DescribeReplicationGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeReplicationGroups" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "ReplicationGroupId" Core.=: replicationGroupId,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Represents the output of a @DescribeReplicationGroups@ operation.
--
-- /See:/ 'newDescribeReplicationGroupsResponse' smart constructor.
data DescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse'
  { -- | A list of replication groups. Each item in the list contains detailed
    -- information about one replication group.
    replicationGroups :: Prelude.Maybe [ReplicationGroup],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroups', 'describeReplicationGroupsResponse_replicationGroups' - A list of replication groups. Each item in the list contains detailed
-- information about one replication group.
--
-- 'marker', 'describeReplicationGroupsResponse_marker' - Provides an identifier to allow retrieval of paginated results.
--
-- 'httpStatus', 'describeReplicationGroupsResponse_httpStatus' - The response's http status code.
newDescribeReplicationGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplicationGroupsResponse
newDescribeReplicationGroupsResponse pHttpStatus_ =
  DescribeReplicationGroupsResponse'
    { replicationGroups =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of replication groups. Each item in the list contains detailed
-- information about one replication group.
describeReplicationGroupsResponse_replicationGroups :: Lens.Lens' DescribeReplicationGroupsResponse (Prelude.Maybe [ReplicationGroup])
describeReplicationGroupsResponse_replicationGroups = Lens.lens (\DescribeReplicationGroupsResponse' {replicationGroups} -> replicationGroups) (\s@DescribeReplicationGroupsResponse' {} a -> s {replicationGroups = a} :: DescribeReplicationGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Provides an identifier to allow retrieval of paginated results.
describeReplicationGroupsResponse_marker :: Lens.Lens' DescribeReplicationGroupsResponse (Prelude.Maybe Prelude.Text)
describeReplicationGroupsResponse_marker = Lens.lens (\DescribeReplicationGroupsResponse' {marker} -> marker) (\s@DescribeReplicationGroupsResponse' {} a -> s {marker = a} :: DescribeReplicationGroupsResponse)

-- | The response's http status code.
describeReplicationGroupsResponse_httpStatus :: Lens.Lens' DescribeReplicationGroupsResponse Prelude.Int
describeReplicationGroupsResponse_httpStatus = Lens.lens (\DescribeReplicationGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationGroupsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationGroupsResponse)

instance
  Prelude.NFData
    DescribeReplicationGroupsResponse
