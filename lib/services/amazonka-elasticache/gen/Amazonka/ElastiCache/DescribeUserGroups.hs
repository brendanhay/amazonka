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
-- Module      : Amazonka.ElastiCache.DescribeUserGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of user groups.
--
-- This operation returns paginated results.
module Amazonka.ElastiCache.DescribeUserGroups
  ( -- * Creating a Request
    DescribeUserGroups (..),
    newDescribeUserGroups,

    -- * Request Lenses
    describeUserGroups_marker,
    describeUserGroups_maxRecords,
    describeUserGroups_userGroupId,

    -- * Destructuring the Response
    DescribeUserGroupsResponse (..),
    newDescribeUserGroupsResponse,

    -- * Response Lenses
    describeUserGroupsResponse_marker,
    describeUserGroupsResponse_userGroups,
    describeUserGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeUserGroups' smart constructor.
data DescribeUserGroups = DescribeUserGroups'
  { -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by MaxRecords. >
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxRecords value, a marker is included
    -- in the response so that the remaining results can be retrieved.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The ID of the user group.
    userGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeUserGroups_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
--
-- 'maxRecords', 'describeUserGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
--
-- 'userGroupId', 'describeUserGroups_userGroupId' - The ID of the user group.
newDescribeUserGroups ::
  DescribeUserGroups
newDescribeUserGroups =
  DescribeUserGroups'
    { marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      userGroupId = Prelude.Nothing
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
describeUserGroups_marker :: Lens.Lens' DescribeUserGroups (Prelude.Maybe Prelude.Text)
describeUserGroups_marker = Lens.lens (\DescribeUserGroups' {marker} -> marker) (\s@DescribeUserGroups' {} a -> s {marker = a} :: DescribeUserGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
describeUserGroups_maxRecords :: Lens.Lens' DescribeUserGroups (Prelude.Maybe Prelude.Int)
describeUserGroups_maxRecords = Lens.lens (\DescribeUserGroups' {maxRecords} -> maxRecords) (\s@DescribeUserGroups' {} a -> s {maxRecords = a} :: DescribeUserGroups)

-- | The ID of the user group.
describeUserGroups_userGroupId :: Lens.Lens' DescribeUserGroups (Prelude.Maybe Prelude.Text)
describeUserGroups_userGroupId = Lens.lens (\DescribeUserGroups' {userGroupId} -> userGroupId) (\s@DescribeUserGroups' {} a -> s {userGroupId = a} :: DescribeUserGroups)

instance Core.AWSPager DescribeUserGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeUserGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeUserGroupsResponse_userGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeUserGroups_marker
          Lens..~ rs
          Lens.^? describeUserGroupsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeUserGroups where
  type
    AWSResponse DescribeUserGroups =
      DescribeUserGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeUserGroupsResult"
      ( \s h x ->
          DescribeUserGroupsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "UserGroups" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUserGroups where
  hashWithSalt _salt DescribeUserGroups' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` userGroupId

instance Prelude.NFData DescribeUserGroups where
  rnf DescribeUserGroups' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf userGroupId

instance Data.ToHeaders DescribeUserGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeUserGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeUserGroups where
  toQuery DescribeUserGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeUserGroups" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "UserGroupId" Data.=: userGroupId
      ]

-- | /See:/ 'newDescribeUserGroupsResponse' smart constructor.
data DescribeUserGroupsResponse = DescribeUserGroupsResponse'
  { -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by MaxRecords. >
    marker :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of user groups.
    userGroups :: Prelude.Maybe [UserGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeUserGroupsResponse_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
--
-- 'userGroups', 'describeUserGroupsResponse_userGroups' - Returns a list of user groups.
--
-- 'httpStatus', 'describeUserGroupsResponse_httpStatus' - The response's http status code.
newDescribeUserGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUserGroupsResponse
newDescribeUserGroupsResponse pHttpStatus_ =
  DescribeUserGroupsResponse'
    { marker =
        Prelude.Nothing,
      userGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
describeUserGroupsResponse_marker :: Lens.Lens' DescribeUserGroupsResponse (Prelude.Maybe Prelude.Text)
describeUserGroupsResponse_marker = Lens.lens (\DescribeUserGroupsResponse' {marker} -> marker) (\s@DescribeUserGroupsResponse' {} a -> s {marker = a} :: DescribeUserGroupsResponse)

-- | Returns a list of user groups.
describeUserGroupsResponse_userGroups :: Lens.Lens' DescribeUserGroupsResponse (Prelude.Maybe [UserGroup])
describeUserGroupsResponse_userGroups = Lens.lens (\DescribeUserGroupsResponse' {userGroups} -> userGroups) (\s@DescribeUserGroupsResponse' {} a -> s {userGroups = a} :: DescribeUserGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeUserGroupsResponse_httpStatus :: Lens.Lens' DescribeUserGroupsResponse Prelude.Int
describeUserGroupsResponse_httpStatus = Lens.lens (\DescribeUserGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeUserGroupsResponse' {} a -> s {httpStatus = a} :: DescribeUserGroupsResponse)

instance Prelude.NFData DescribeUserGroupsResponse where
  rnf DescribeUserGroupsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf userGroups
      `Prelude.seq` Prelude.rnf httpStatus
