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
-- Module      : Amazonka.ElastiCache.DescribeGlobalReplicationGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a particular global replication group. If no
-- identifier is specified, returns information about all Global
-- datastores.
--
-- This operation returns paginated results.
module Amazonka.ElastiCache.DescribeGlobalReplicationGroups
  ( -- * Creating a Request
    DescribeGlobalReplicationGroups (..),
    newDescribeGlobalReplicationGroups,

    -- * Request Lenses
    describeGlobalReplicationGroups_showMemberInfo,
    describeGlobalReplicationGroups_marker,
    describeGlobalReplicationGroups_maxRecords,
    describeGlobalReplicationGroups_globalReplicationGroupId,

    -- * Destructuring the Response
    DescribeGlobalReplicationGroupsResponse (..),
    newDescribeGlobalReplicationGroupsResponse,

    -- * Response Lenses
    describeGlobalReplicationGroupsResponse_marker,
    describeGlobalReplicationGroupsResponse_globalReplicationGroups,
    describeGlobalReplicationGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGlobalReplicationGroups' smart constructor.
data DescribeGlobalReplicationGroups = DescribeGlobalReplicationGroups'
  { -- | Returns the list of members that comprise the Global datastore.
    showMemberInfo :: Prelude.Maybe Prelude.Bool,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxRecords value, a marker is included
    -- in the response so that the remaining results can be retrieved.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The name of the Global datastore
    globalReplicationGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGlobalReplicationGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'showMemberInfo', 'describeGlobalReplicationGroups_showMemberInfo' - Returns the list of members that comprise the Global datastore.
--
-- 'marker', 'describeGlobalReplicationGroups_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeGlobalReplicationGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
--
-- 'globalReplicationGroupId', 'describeGlobalReplicationGroups_globalReplicationGroupId' - The name of the Global datastore
newDescribeGlobalReplicationGroups ::
  DescribeGlobalReplicationGroups
newDescribeGlobalReplicationGroups =
  DescribeGlobalReplicationGroups'
    { showMemberInfo =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      globalReplicationGroupId = Prelude.Nothing
    }

-- | Returns the list of members that comprise the Global datastore.
describeGlobalReplicationGroups_showMemberInfo :: Lens.Lens' DescribeGlobalReplicationGroups (Prelude.Maybe Prelude.Bool)
describeGlobalReplicationGroups_showMemberInfo = Lens.lens (\DescribeGlobalReplicationGroups' {showMemberInfo} -> showMemberInfo) (\s@DescribeGlobalReplicationGroups' {} a -> s {showMemberInfo = a} :: DescribeGlobalReplicationGroups)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeGlobalReplicationGroups_marker :: Lens.Lens' DescribeGlobalReplicationGroups (Prelude.Maybe Prelude.Text)
describeGlobalReplicationGroups_marker = Lens.lens (\DescribeGlobalReplicationGroups' {marker} -> marker) (\s@DescribeGlobalReplicationGroups' {} a -> s {marker = a} :: DescribeGlobalReplicationGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
describeGlobalReplicationGroups_maxRecords :: Lens.Lens' DescribeGlobalReplicationGroups (Prelude.Maybe Prelude.Int)
describeGlobalReplicationGroups_maxRecords = Lens.lens (\DescribeGlobalReplicationGroups' {maxRecords} -> maxRecords) (\s@DescribeGlobalReplicationGroups' {} a -> s {maxRecords = a} :: DescribeGlobalReplicationGroups)

-- | The name of the Global datastore
describeGlobalReplicationGroups_globalReplicationGroupId :: Lens.Lens' DescribeGlobalReplicationGroups (Prelude.Maybe Prelude.Text)
describeGlobalReplicationGroups_globalReplicationGroupId = Lens.lens (\DescribeGlobalReplicationGroups' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@DescribeGlobalReplicationGroups' {} a -> s {globalReplicationGroupId = a} :: DescribeGlobalReplicationGroups)

instance
  Core.AWSPager
    DescribeGlobalReplicationGroups
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeGlobalReplicationGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeGlobalReplicationGroupsResponse_globalReplicationGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeGlobalReplicationGroups_marker
          Lens..~ rs
          Lens.^? describeGlobalReplicationGroupsResponse_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeGlobalReplicationGroups
  where
  type
    AWSResponse DescribeGlobalReplicationGroups =
      DescribeGlobalReplicationGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeGlobalReplicationGroupsResult"
      ( \s h x ->
          DescribeGlobalReplicationGroupsResponse'
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> ( x Core..@? "GlobalReplicationGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may
                              (Core.parseXMLList "GlobalReplicationGroup")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeGlobalReplicationGroups
  where
  hashWithSalt
    salt'
    DescribeGlobalReplicationGroups' {..} =
      salt'
        `Prelude.hashWithSalt` globalReplicationGroupId
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` showMemberInfo

instance
  Prelude.NFData
    DescribeGlobalReplicationGroups
  where
  rnf DescribeGlobalReplicationGroups' {..} =
    Prelude.rnf showMemberInfo
      `Prelude.seq` Prelude.rnf globalReplicationGroupId
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf marker

instance
  Core.ToHeaders
    DescribeGlobalReplicationGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeGlobalReplicationGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeGlobalReplicationGroups where
  toQuery DescribeGlobalReplicationGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeGlobalReplicationGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "ShowMemberInfo" Core.=: showMemberInfo,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "GlobalReplicationGroupId"
          Core.=: globalReplicationGroupId
      ]

-- | /See:/ 'newDescribeGlobalReplicationGroupsResponse' smart constructor.
data DescribeGlobalReplicationGroupsResponse = DescribeGlobalReplicationGroupsResponse'
  { -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by MaxRecords. >
    marker :: Prelude.Maybe Prelude.Text,
    -- | Indicates the slot configuration and global identifier for each slice
    -- group.
    globalReplicationGroups :: Prelude.Maybe [GlobalReplicationGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGlobalReplicationGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeGlobalReplicationGroupsResponse_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
--
-- 'globalReplicationGroups', 'describeGlobalReplicationGroupsResponse_globalReplicationGroups' - Indicates the slot configuration and global identifier for each slice
-- group.
--
-- 'httpStatus', 'describeGlobalReplicationGroupsResponse_httpStatus' - The response's http status code.
newDescribeGlobalReplicationGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGlobalReplicationGroupsResponse
newDescribeGlobalReplicationGroupsResponse
  pHttpStatus_ =
    DescribeGlobalReplicationGroupsResponse'
      { marker =
          Prelude.Nothing,
        globalReplicationGroups =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
describeGlobalReplicationGroupsResponse_marker :: Lens.Lens' DescribeGlobalReplicationGroupsResponse (Prelude.Maybe Prelude.Text)
describeGlobalReplicationGroupsResponse_marker = Lens.lens (\DescribeGlobalReplicationGroupsResponse' {marker} -> marker) (\s@DescribeGlobalReplicationGroupsResponse' {} a -> s {marker = a} :: DescribeGlobalReplicationGroupsResponse)

-- | Indicates the slot configuration and global identifier for each slice
-- group.
describeGlobalReplicationGroupsResponse_globalReplicationGroups :: Lens.Lens' DescribeGlobalReplicationGroupsResponse (Prelude.Maybe [GlobalReplicationGroup])
describeGlobalReplicationGroupsResponse_globalReplicationGroups = Lens.lens (\DescribeGlobalReplicationGroupsResponse' {globalReplicationGroups} -> globalReplicationGroups) (\s@DescribeGlobalReplicationGroupsResponse' {} a -> s {globalReplicationGroups = a} :: DescribeGlobalReplicationGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeGlobalReplicationGroupsResponse_httpStatus :: Lens.Lens' DescribeGlobalReplicationGroupsResponse Prelude.Int
describeGlobalReplicationGroupsResponse_httpStatus = Lens.lens (\DescribeGlobalReplicationGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeGlobalReplicationGroupsResponse' {} a -> s {httpStatus = a} :: DescribeGlobalReplicationGroupsResponse)

instance
  Prelude.NFData
    DescribeGlobalReplicationGroupsResponse
  where
  rnf DescribeGlobalReplicationGroupsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf globalReplicationGroups
