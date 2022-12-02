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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    describeGlobalReplicationGroups_marker,
    describeGlobalReplicationGroups_globalReplicationGroupId,
    describeGlobalReplicationGroups_maxRecords,
    describeGlobalReplicationGroups_showMemberInfo,

    -- * Destructuring the Response
    DescribeGlobalReplicationGroupsResponse (..),
    newDescribeGlobalReplicationGroupsResponse,

    -- * Response Lenses
    describeGlobalReplicationGroupsResponse_globalReplicationGroups,
    describeGlobalReplicationGroupsResponse_marker,
    describeGlobalReplicationGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGlobalReplicationGroups' smart constructor.
data DescribeGlobalReplicationGroups = DescribeGlobalReplicationGroups'
  { -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The name of the Global datastore
    globalReplicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxRecords value, a marker is included
    -- in the response so that the remaining results can be retrieved.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | Returns the list of members that comprise the Global datastore.
    showMemberInfo :: Prelude.Maybe Prelude.Bool
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
-- 'marker', 'describeGlobalReplicationGroups_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'globalReplicationGroupId', 'describeGlobalReplicationGroups_globalReplicationGroupId' - The name of the Global datastore
--
-- 'maxRecords', 'describeGlobalReplicationGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
--
-- 'showMemberInfo', 'describeGlobalReplicationGroups_showMemberInfo' - Returns the list of members that comprise the Global datastore.
newDescribeGlobalReplicationGroups ::
  DescribeGlobalReplicationGroups
newDescribeGlobalReplicationGroups =
  DescribeGlobalReplicationGroups'
    { marker =
        Prelude.Nothing,
      globalReplicationGroupId = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      showMemberInfo = Prelude.Nothing
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeGlobalReplicationGroups_marker :: Lens.Lens' DescribeGlobalReplicationGroups (Prelude.Maybe Prelude.Text)
describeGlobalReplicationGroups_marker = Lens.lens (\DescribeGlobalReplicationGroups' {marker} -> marker) (\s@DescribeGlobalReplicationGroups' {} a -> s {marker = a} :: DescribeGlobalReplicationGroups)

-- | The name of the Global datastore
describeGlobalReplicationGroups_globalReplicationGroupId :: Lens.Lens' DescribeGlobalReplicationGroups (Prelude.Maybe Prelude.Text)
describeGlobalReplicationGroups_globalReplicationGroupId = Lens.lens (\DescribeGlobalReplicationGroups' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@DescribeGlobalReplicationGroups' {} a -> s {globalReplicationGroupId = a} :: DescribeGlobalReplicationGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
describeGlobalReplicationGroups_maxRecords :: Lens.Lens' DescribeGlobalReplicationGroups (Prelude.Maybe Prelude.Int)
describeGlobalReplicationGroups_maxRecords = Lens.lens (\DescribeGlobalReplicationGroups' {maxRecords} -> maxRecords) (\s@DescribeGlobalReplicationGroups' {} a -> s {maxRecords = a} :: DescribeGlobalReplicationGroups)

-- | Returns the list of members that comprise the Global datastore.
describeGlobalReplicationGroups_showMemberInfo :: Lens.Lens' DescribeGlobalReplicationGroups (Prelude.Maybe Prelude.Bool)
describeGlobalReplicationGroups_showMemberInfo = Lens.lens (\DescribeGlobalReplicationGroups' {showMemberInfo} -> showMemberInfo) (\s@DescribeGlobalReplicationGroups' {} a -> s {showMemberInfo = a} :: DescribeGlobalReplicationGroups)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeGlobalReplicationGroupsResult"
      ( \s h x ->
          DescribeGlobalReplicationGroupsResponse'
            Prelude.<$> ( x Data..@? "GlobalReplicationGroups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may
                              (Data.parseXMLList "GlobalReplicationGroup")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeGlobalReplicationGroups
  where
  hashWithSalt
    _salt
    DescribeGlobalReplicationGroups' {..} =
      _salt `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` globalReplicationGroupId
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` showMemberInfo

instance
  Prelude.NFData
    DescribeGlobalReplicationGroups
  where
  rnf DescribeGlobalReplicationGroups' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf globalReplicationGroupId
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf showMemberInfo

instance
  Data.ToHeaders
    DescribeGlobalReplicationGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeGlobalReplicationGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeGlobalReplicationGroups where
  toQuery DescribeGlobalReplicationGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeGlobalReplicationGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "GlobalReplicationGroupId"
          Data.=: globalReplicationGroupId,
        "MaxRecords" Data.=: maxRecords,
        "ShowMemberInfo" Data.=: showMemberInfo
      ]

-- | /See:/ 'newDescribeGlobalReplicationGroupsResponse' smart constructor.
data DescribeGlobalReplicationGroupsResponse = DescribeGlobalReplicationGroupsResponse'
  { -- | Indicates the slot configuration and global identifier for each slice
    -- group.
    globalReplicationGroups :: Prelude.Maybe [GlobalReplicationGroup],
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by MaxRecords. >
    marker :: Prelude.Maybe Prelude.Text,
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
-- 'globalReplicationGroups', 'describeGlobalReplicationGroupsResponse_globalReplicationGroups' - Indicates the slot configuration and global identifier for each slice
-- group.
--
-- 'marker', 'describeGlobalReplicationGroupsResponse_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
--
-- 'httpStatus', 'describeGlobalReplicationGroupsResponse_httpStatus' - The response's http status code.
newDescribeGlobalReplicationGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGlobalReplicationGroupsResponse
newDescribeGlobalReplicationGroupsResponse
  pHttpStatus_ =
    DescribeGlobalReplicationGroupsResponse'
      { globalReplicationGroups =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Indicates the slot configuration and global identifier for each slice
-- group.
describeGlobalReplicationGroupsResponse_globalReplicationGroups :: Lens.Lens' DescribeGlobalReplicationGroupsResponse (Prelude.Maybe [GlobalReplicationGroup])
describeGlobalReplicationGroupsResponse_globalReplicationGroups = Lens.lens (\DescribeGlobalReplicationGroupsResponse' {globalReplicationGroups} -> globalReplicationGroups) (\s@DescribeGlobalReplicationGroupsResponse' {} a -> s {globalReplicationGroups = a} :: DescribeGlobalReplicationGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
describeGlobalReplicationGroupsResponse_marker :: Lens.Lens' DescribeGlobalReplicationGroupsResponse (Prelude.Maybe Prelude.Text)
describeGlobalReplicationGroupsResponse_marker = Lens.lens (\DescribeGlobalReplicationGroupsResponse' {marker} -> marker) (\s@DescribeGlobalReplicationGroupsResponse' {} a -> s {marker = a} :: DescribeGlobalReplicationGroupsResponse)

-- | The response's http status code.
describeGlobalReplicationGroupsResponse_httpStatus :: Lens.Lens' DescribeGlobalReplicationGroupsResponse Prelude.Int
describeGlobalReplicationGroupsResponse_httpStatus = Lens.lens (\DescribeGlobalReplicationGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeGlobalReplicationGroupsResponse' {} a -> s {httpStatus = a} :: DescribeGlobalReplicationGroupsResponse)

instance
  Prelude.NFData
    DescribeGlobalReplicationGroupsResponse
  where
  rnf DescribeGlobalReplicationGroupsResponse' {..} =
    Prelude.rnf globalReplicationGroups
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
