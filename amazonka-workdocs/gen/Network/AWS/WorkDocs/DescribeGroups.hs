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
-- Module      : Network.AWS.WorkDocs.DescribeGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the groups specified by the query. Groups are defined by the
-- underlying Active Directory.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeGroups
  ( -- * Creating a Request
    DescribeGroups (..),
    newDescribeGroups,

    -- * Request Lenses
    describeGroups_organizationId,
    describeGroups_authenticationToken,
    describeGroups_limit,
    describeGroups_marker,
    describeGroups_searchQuery,

    -- * Destructuring the Response
    DescribeGroupsResponse (..),
    newDescribeGroupsResponse,

    -- * Response Lenses
    describeGroupsResponse_groups,
    describeGroupsResponse_marker,
    describeGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDescribeGroups' smart constructor.
data DescribeGroups = DescribeGroups'
  { -- | The ID of the organization.
    organizationId :: Core.Maybe Core.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The maximum number of items to return with this call.
    limit :: Core.Maybe Core.Natural,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Core.Maybe Core.Text,
    -- | A query to describe groups by group name.
    searchQuery :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'describeGroups_organizationId' - The ID of the organization.
--
-- 'authenticationToken', 'describeGroups_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'limit', 'describeGroups_limit' - The maximum number of items to return with this call.
--
-- 'marker', 'describeGroups_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
--
-- 'searchQuery', 'describeGroups_searchQuery' - A query to describe groups by group name.
newDescribeGroups ::
  -- | 'searchQuery'
  Core.Text ->
  DescribeGroups
newDescribeGroups pSearchQuery_ =
  DescribeGroups'
    { organizationId = Core.Nothing,
      authenticationToken = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing,
      searchQuery = Core._Sensitive Lens.# pSearchQuery_
    }

-- | The ID of the organization.
describeGroups_organizationId :: Lens.Lens' DescribeGroups (Core.Maybe Core.Text)
describeGroups_organizationId = Lens.lens (\DescribeGroups' {organizationId} -> organizationId) (\s@DescribeGroups' {} a -> s {organizationId = a} :: DescribeGroups)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
describeGroups_authenticationToken :: Lens.Lens' DescribeGroups (Core.Maybe Core.Text)
describeGroups_authenticationToken = Lens.lens (\DescribeGroups' {authenticationToken} -> authenticationToken) (\s@DescribeGroups' {} a -> s {authenticationToken = a} :: DescribeGroups) Core.. Lens.mapping Core._Sensitive

-- | The maximum number of items to return with this call.
describeGroups_limit :: Lens.Lens' DescribeGroups (Core.Maybe Core.Natural)
describeGroups_limit = Lens.lens (\DescribeGroups' {limit} -> limit) (\s@DescribeGroups' {} a -> s {limit = a} :: DescribeGroups)

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeGroups_marker :: Lens.Lens' DescribeGroups (Core.Maybe Core.Text)
describeGroups_marker = Lens.lens (\DescribeGroups' {marker} -> marker) (\s@DescribeGroups' {} a -> s {marker = a} :: DescribeGroups)

-- | A query to describe groups by group name.
describeGroups_searchQuery :: Lens.Lens' DescribeGroups Core.Text
describeGroups_searchQuery = Lens.lens (\DescribeGroups' {searchQuery} -> searchQuery) (\s@DescribeGroups' {} a -> s {searchQuery = a} :: DescribeGroups) Core.. Core._Sensitive

instance Core.AWSPager DescribeGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeGroupsResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeGroupsResponse_groups Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeGroups_marker
          Lens..~ rs
          Lens.^? describeGroupsResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeGroups where
  type
    AWSResponse DescribeGroups =
      DescribeGroupsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGroupsResponse'
            Core.<$> (x Core..?> "Groups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeGroups

instance Core.NFData DescribeGroups

instance Core.ToHeaders DescribeGroups where
  toHeaders DescribeGroups' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToPath DescribeGroups where
  toPath = Core.const "/api/v1/groups"

instance Core.ToQuery DescribeGroups where
  toQuery DescribeGroups' {..} =
    Core.mconcat
      [ "organizationId" Core.=: organizationId,
        "limit" Core.=: limit,
        "marker" Core.=: marker,
        "searchQuery" Core.=: searchQuery
      ]

-- | /See:/ 'newDescribeGroupsResponse' smart constructor.
data DescribeGroupsResponse = DescribeGroupsResponse'
  { -- | The list of groups.
    groups :: Core.Maybe [GroupMetadata],
    -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'describeGroupsResponse_groups' - The list of groups.
--
-- 'marker', 'describeGroupsResponse_marker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'httpStatus', 'describeGroupsResponse_httpStatus' - The response's http status code.
newDescribeGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeGroupsResponse
newDescribeGroupsResponse pHttpStatus_ =
  DescribeGroupsResponse'
    { groups = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of groups.
describeGroupsResponse_groups :: Lens.Lens' DescribeGroupsResponse (Core.Maybe [GroupMetadata])
describeGroupsResponse_groups = Lens.lens (\DescribeGroupsResponse' {groups} -> groups) (\s@DescribeGroupsResponse' {} a -> s {groups = a} :: DescribeGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeGroupsResponse_marker :: Lens.Lens' DescribeGroupsResponse (Core.Maybe Core.Text)
describeGroupsResponse_marker = Lens.lens (\DescribeGroupsResponse' {marker} -> marker) (\s@DescribeGroupsResponse' {} a -> s {marker = a} :: DescribeGroupsResponse)

-- | The response's http status code.
describeGroupsResponse_httpStatus :: Lens.Lens' DescribeGroupsResponse Core.Int
describeGroupsResponse_httpStatus = Lens.lens (\DescribeGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeGroupsResponse' {} a -> s {httpStatus = a} :: DescribeGroupsResponse)

instance Core.NFData DescribeGroupsResponse
