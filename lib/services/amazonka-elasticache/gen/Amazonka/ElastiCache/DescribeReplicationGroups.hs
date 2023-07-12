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
-- Module      : Amazonka.ElastiCache.DescribeReplicationGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.ElastiCache.DescribeReplicationGroups
  ( -- * Creating a Request
    DescribeReplicationGroups (..),
    newDescribeReplicationGroups,

    -- * Request Lenses
    describeReplicationGroups_marker,
    describeReplicationGroups_maxRecords,
    describeReplicationGroups_replicationGroupId,

    -- * Destructuring the Response
    DescribeReplicationGroupsResponse (..),
    newDescribeReplicationGroupsResponse,

    -- * Response Lenses
    describeReplicationGroupsResponse_marker,
    describeReplicationGroupsResponse_replicationGroups,
    describeReplicationGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DescribeReplicationGroups@ operation.
--
-- /See:/ 'newDescribeReplicationGroups' smart constructor.
data DescribeReplicationGroups = DescribeReplicationGroups'
  { -- | An optional marker returned from a prior request. Use this marker for
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
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the replication group to be described. This parameter
    -- is not case sensitive.
    --
    -- If you do not specify this parameter, information about all replication
    -- groups is returned.
    replicationGroupId :: Prelude.Maybe Prelude.Text
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
--
-- 'replicationGroupId', 'describeReplicationGroups_replicationGroupId' - The identifier for the replication group to be described. This parameter
-- is not case sensitive.
--
-- If you do not specify this parameter, information about all replication
-- groups is returned.
newDescribeReplicationGroups ::
  DescribeReplicationGroups
newDescribeReplicationGroups =
  DescribeReplicationGroups'
    { marker =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      replicationGroupId = Prelude.Nothing
    }

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

-- | The identifier for the replication group to be described. This parameter
-- is not case sensitive.
--
-- If you do not specify this parameter, information about all replication
-- groups is returned.
describeReplicationGroups_replicationGroupId :: Lens.Lens' DescribeReplicationGroups (Prelude.Maybe Prelude.Text)
describeReplicationGroups_replicationGroupId = Lens.lens (\DescribeReplicationGroups' {replicationGroupId} -> replicationGroupId) (\s@DescribeReplicationGroups' {} a -> s {replicationGroupId = a} :: DescribeReplicationGroups)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeReplicationGroups_marker
          Lens..~ rs
          Lens.^? describeReplicationGroupsResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeReplicationGroups where
  type
    AWSResponse DescribeReplicationGroups =
      DescribeReplicationGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeReplicationGroupsResult"
      ( \s h x ->
          DescribeReplicationGroupsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x
                            Data..@? "ReplicationGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "ReplicationGroup")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReplicationGroups where
  hashWithSalt _salt DescribeReplicationGroups' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` replicationGroupId

instance Prelude.NFData DescribeReplicationGroups where
  rnf DescribeReplicationGroups' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf replicationGroupId

instance Data.ToHeaders DescribeReplicationGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeReplicationGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeReplicationGroups where
  toQuery DescribeReplicationGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeReplicationGroups" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "ReplicationGroupId" Data.=: replicationGroupId
      ]

-- | Represents the output of a @DescribeReplicationGroups@ operation.
--
-- /See:/ 'newDescribeReplicationGroupsResponse' smart constructor.
data DescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of replication groups. Each item in the list contains detailed
    -- information about one replication group.
    replicationGroups :: Prelude.Maybe [ReplicationGroup],
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
-- 'marker', 'describeReplicationGroupsResponse_marker' - Provides an identifier to allow retrieval of paginated results.
--
-- 'replicationGroups', 'describeReplicationGroupsResponse_replicationGroups' - A list of replication groups. Each item in the list contains detailed
-- information about one replication group.
--
-- 'httpStatus', 'describeReplicationGroupsResponse_httpStatus' - The response's http status code.
newDescribeReplicationGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplicationGroupsResponse
newDescribeReplicationGroupsResponse pHttpStatus_ =
  DescribeReplicationGroupsResponse'
    { marker =
        Prelude.Nothing,
      replicationGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
describeReplicationGroupsResponse_marker :: Lens.Lens' DescribeReplicationGroupsResponse (Prelude.Maybe Prelude.Text)
describeReplicationGroupsResponse_marker = Lens.lens (\DescribeReplicationGroupsResponse' {marker} -> marker) (\s@DescribeReplicationGroupsResponse' {} a -> s {marker = a} :: DescribeReplicationGroupsResponse)

-- | A list of replication groups. Each item in the list contains detailed
-- information about one replication group.
describeReplicationGroupsResponse_replicationGroups :: Lens.Lens' DescribeReplicationGroupsResponse (Prelude.Maybe [ReplicationGroup])
describeReplicationGroupsResponse_replicationGroups = Lens.lens (\DescribeReplicationGroupsResponse' {replicationGroups} -> replicationGroups) (\s@DescribeReplicationGroupsResponse' {} a -> s {replicationGroups = a} :: DescribeReplicationGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReplicationGroupsResponse_httpStatus :: Lens.Lens' DescribeReplicationGroupsResponse Prelude.Int
describeReplicationGroupsResponse_httpStatus = Lens.lens (\DescribeReplicationGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationGroupsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationGroupsResponse)

instance
  Prelude.NFData
    DescribeReplicationGroupsResponse
  where
  rnf DescribeReplicationGroupsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf replicationGroups
      `Prelude.seq` Prelude.rnf httpStatus
