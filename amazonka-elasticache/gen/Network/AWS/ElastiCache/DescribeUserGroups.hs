{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElastiCache.DescribeUserGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of user groups.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeUserGroups
  ( -- * Creating a Request
    DescribeUserGroups (..),
    newDescribeUserGroups,

    -- * Request Lenses
    describeUserGroups_userGroupId,
    describeUserGroups_marker,
    describeUserGroups_maxRecords,

    -- * Destructuring the Response
    DescribeUserGroupsResponse (..),
    newDescribeUserGroupsResponse,

    -- * Response Lenses
    describeUserGroupsResponse_userGroups,
    describeUserGroupsResponse_marker,
    describeUserGroupsResponse_httpStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUserGroups' smart constructor.
data DescribeUserGroups = DescribeUserGroups'
  { -- | The ID of the user group.
    userGroupId :: Prelude.Maybe Prelude.Text,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by MaxRecords. >
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxRecords value, a marker is included
    -- in the response so that the remaining results can be retrieved.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userGroupId', 'describeUserGroups_userGroupId' - The ID of the user group.
--
-- 'marker', 'describeUserGroups_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
--
-- 'maxRecords', 'describeUserGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
newDescribeUserGroups ::
  DescribeUserGroups
newDescribeUserGroups =
  DescribeUserGroups'
    { userGroupId = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The ID of the user group.
describeUserGroups_userGroupId :: Lens.Lens' DescribeUserGroups (Prelude.Maybe Prelude.Text)
describeUserGroups_userGroupId = Lens.lens (\DescribeUserGroups' {userGroupId} -> userGroupId) (\s@DescribeUserGroups' {} a -> s {userGroupId = a} :: DescribeUserGroups)

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

instance Pager.AWSPager DescribeUserGroups where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeUserGroupsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeUserGroupsResponse_userGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeUserGroups_marker
          Lens..~ rs
          Lens.^? describeUserGroupsResponse_marker
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeUserGroups where
  type
    Rs DescribeUserGroups =
      DescribeUserGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeUserGroupsResult"
      ( \s h x ->
          DescribeUserGroupsResponse'
            Prelude.<$> ( x Prelude..@? "UserGroups"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUserGroups

instance Prelude.NFData DescribeUserGroups

instance Prelude.ToHeaders DescribeUserGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeUserGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeUserGroups where
  toQuery DescribeUserGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeUserGroups" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "UserGroupId" Prelude.=: userGroupId,
        "Marker" Prelude.=: marker,
        "MaxRecords" Prelude.=: maxRecords
      ]

-- | /See:/ 'newDescribeUserGroupsResponse' smart constructor.
data DescribeUserGroupsResponse = DescribeUserGroupsResponse'
  { -- | Returns a list of user groups.
    userGroups :: Prelude.Maybe [UserGroup],
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by MaxRecords. >
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userGroups', 'describeUserGroupsResponse_userGroups' - Returns a list of user groups.
--
-- 'marker', 'describeUserGroupsResponse_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
--
-- 'httpStatus', 'describeUserGroupsResponse_httpStatus' - The response's http status code.
newDescribeUserGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUserGroupsResponse
newDescribeUserGroupsResponse pHttpStatus_ =
  DescribeUserGroupsResponse'
    { userGroups =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of user groups.
describeUserGroupsResponse_userGroups :: Lens.Lens' DescribeUserGroupsResponse (Prelude.Maybe [UserGroup])
describeUserGroupsResponse_userGroups = Lens.lens (\DescribeUserGroupsResponse' {userGroups} -> userGroups) (\s@DescribeUserGroupsResponse' {} a -> s {userGroups = a} :: DescribeUserGroupsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
describeUserGroupsResponse_marker :: Lens.Lens' DescribeUserGroupsResponse (Prelude.Maybe Prelude.Text)
describeUserGroupsResponse_marker = Lens.lens (\DescribeUserGroupsResponse' {marker} -> marker) (\s@DescribeUserGroupsResponse' {} a -> s {marker = a} :: DescribeUserGroupsResponse)

-- | The response's http status code.
describeUserGroupsResponse_httpStatus :: Lens.Lens' DescribeUserGroupsResponse Prelude.Int
describeUserGroupsResponse_httpStatus = Lens.lens (\DescribeUserGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeUserGroupsResponse' {} a -> s {httpStatus = a} :: DescribeUserGroupsResponse)

instance Prelude.NFData DescribeUserGroupsResponse
