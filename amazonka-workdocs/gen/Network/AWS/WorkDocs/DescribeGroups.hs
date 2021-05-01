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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDescribeGroups' smart constructor.
data DescribeGroups = DescribeGroups'
  { -- | The ID of the organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The maximum number of items to return with this call.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | A query to describe groups by group name.
    searchQuery :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeGroups
newDescribeGroups pSearchQuery_ =
  DescribeGroups'
    { organizationId = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      searchQuery =
        Prelude._Sensitive Lens.# pSearchQuery_
    }

-- | The ID of the organization.
describeGroups_organizationId :: Lens.Lens' DescribeGroups (Prelude.Maybe Prelude.Text)
describeGroups_organizationId = Lens.lens (\DescribeGroups' {organizationId} -> organizationId) (\s@DescribeGroups' {} a -> s {organizationId = a} :: DescribeGroups)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
describeGroups_authenticationToken :: Lens.Lens' DescribeGroups (Prelude.Maybe Prelude.Text)
describeGroups_authenticationToken = Lens.lens (\DescribeGroups' {authenticationToken} -> authenticationToken) (\s@DescribeGroups' {} a -> s {authenticationToken = a} :: DescribeGroups) Prelude.. Lens.mapping Prelude._Sensitive

-- | The maximum number of items to return with this call.
describeGroups_limit :: Lens.Lens' DescribeGroups (Prelude.Maybe Prelude.Natural)
describeGroups_limit = Lens.lens (\DescribeGroups' {limit} -> limit) (\s@DescribeGroups' {} a -> s {limit = a} :: DescribeGroups)

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeGroups_marker :: Lens.Lens' DescribeGroups (Prelude.Maybe Prelude.Text)
describeGroups_marker = Lens.lens (\DescribeGroups' {marker} -> marker) (\s@DescribeGroups' {} a -> s {marker = a} :: DescribeGroups)

-- | A query to describe groups by group name.
describeGroups_searchQuery :: Lens.Lens' DescribeGroups Prelude.Text
describeGroups_searchQuery = Lens.lens (\DescribeGroups' {searchQuery} -> searchQuery) (\s@DescribeGroups' {} a -> s {searchQuery = a} :: DescribeGroups) Prelude.. Prelude._Sensitive

instance Pager.AWSPager DescribeGroups where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeGroupsResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeGroupsResponse_groups Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeGroups_marker
          Lens..~ rs
          Lens.^? describeGroupsResponse_marker Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeGroups where
  type Rs DescribeGroups = DescribeGroupsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGroupsResponse'
            Prelude.<$> (x Prelude..?> "Groups" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGroups

instance Prelude.NFData DescribeGroups

instance Prelude.ToHeaders DescribeGroups where
  toHeaders DescribeGroups' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath DescribeGroups where
  toPath = Prelude.const "/api/v1/groups"

instance Prelude.ToQuery DescribeGroups where
  toQuery DescribeGroups' {..} =
    Prelude.mconcat
      [ "organizationId" Prelude.=: organizationId,
        "limit" Prelude.=: limit,
        "marker" Prelude.=: marker,
        "searchQuery" Prelude.=: searchQuery
      ]

-- | /See:/ 'newDescribeGroupsResponse' smart constructor.
data DescribeGroupsResponse = DescribeGroupsResponse'
  { -- | The list of groups.
    groups :: Prelude.Maybe [GroupMetadata],
    -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeGroupsResponse
newDescribeGroupsResponse pHttpStatus_ =
  DescribeGroupsResponse'
    { groups = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of groups.
describeGroupsResponse_groups :: Lens.Lens' DescribeGroupsResponse (Prelude.Maybe [GroupMetadata])
describeGroupsResponse_groups = Lens.lens (\DescribeGroupsResponse' {groups} -> groups) (\s@DescribeGroupsResponse' {} a -> s {groups = a} :: DescribeGroupsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeGroupsResponse_marker :: Lens.Lens' DescribeGroupsResponse (Prelude.Maybe Prelude.Text)
describeGroupsResponse_marker = Lens.lens (\DescribeGroupsResponse' {marker} -> marker) (\s@DescribeGroupsResponse' {} a -> s {marker = a} :: DescribeGroupsResponse)

-- | The response's http status code.
describeGroupsResponse_httpStatus :: Lens.Lens' DescribeGroupsResponse Prelude.Int
describeGroupsResponse_httpStatus = Lens.lens (\DescribeGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeGroupsResponse' {} a -> s {httpStatus = a} :: DescribeGroupsResponse)

instance Prelude.NFData DescribeGroupsResponse
