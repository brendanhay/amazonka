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
-- Module      : Network.AWS.WorkMail.ListGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summaries of the organization\'s groups.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListGroups
  ( -- * Creating a Request
    ListGroups (..),
    newListGroups,

    -- * Request Lenses
    listGroups_nextToken,
    listGroups_maxResults,
    listGroups_organizationId,

    -- * Destructuring the Response
    ListGroupsResponse (..),
    newListGroupsResponse,

    -- * Response Lenses
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newListGroups' smart constructor.
data ListGroups = ListGroups'
  { -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier for the organization under which the groups exist.
    organizationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGroups_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
--
-- 'maxResults', 'listGroups_maxResults' - The maximum number of results to return in a single call.
--
-- 'organizationId', 'listGroups_organizationId' - The identifier for the organization under which the groups exist.
newListGroups ::
  -- | 'organizationId'
  Core.Text ->
  ListGroups
newListGroups pOrganizationId_ =
  ListGroups'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      organizationId = pOrganizationId_
    }

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listGroups_nextToken :: Lens.Lens' ListGroups (Core.Maybe Core.Text)
listGroups_nextToken = Lens.lens (\ListGroups' {nextToken} -> nextToken) (\s@ListGroups' {} a -> s {nextToken = a} :: ListGroups)

-- | The maximum number of results to return in a single call.
listGroups_maxResults :: Lens.Lens' ListGroups (Core.Maybe Core.Natural)
listGroups_maxResults = Lens.lens (\ListGroups' {maxResults} -> maxResults) (\s@ListGroups' {} a -> s {maxResults = a} :: ListGroups)

-- | The identifier for the organization under which the groups exist.
listGroups_organizationId :: Lens.Lens' ListGroups Core.Text
listGroups_organizationId = Lens.lens (\ListGroups' {organizationId} -> organizationId) (\s@ListGroups' {} a -> s {organizationId = a} :: ListGroups)

instance Core.AWSPager ListGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGroupsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listGroupsResponse_groups Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listGroups_nextToken
          Lens..~ rs
          Lens.^? listGroupsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListGroups where
  type AWSResponse ListGroups = ListGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupsResponse'
            Core.<$> (x Core..?> "Groups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListGroups

instance Core.NFData ListGroups

instance Core.ToHeaders ListGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("WorkMailService.ListGroups" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListGroups where
  toJSON ListGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("OrganizationId" Core..= organizationId)
          ]
      )

instance Core.ToPath ListGroups where
  toPath = Core.const "/"

instance Core.ToQuery ListGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { -- | The overview of groups for an organization.
    groups :: Core.Maybe [Group],
    -- | The token to use to retrieve the next page of results. The value is
    -- \"null\" when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'listGroupsResponse_groups' - The overview of groups for an organization.
--
-- 'nextToken', 'listGroupsResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- \"null\" when there are no more results to return.
--
-- 'httpStatus', 'listGroupsResponse_httpStatus' - The response's http status code.
newListGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListGroupsResponse
newListGroupsResponse pHttpStatus_ =
  ListGroupsResponse'
    { groups = Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The overview of groups for an organization.
listGroupsResponse_groups :: Lens.Lens' ListGroupsResponse (Core.Maybe [Group])
listGroupsResponse_groups = Lens.lens (\ListGroupsResponse' {groups} -> groups) (\s@ListGroupsResponse' {} a -> s {groups = a} :: ListGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to use to retrieve the next page of results. The value is
-- \"null\" when there are no more results to return.
listGroupsResponse_nextToken :: Lens.Lens' ListGroupsResponse (Core.Maybe Core.Text)
listGroupsResponse_nextToken = Lens.lens (\ListGroupsResponse' {nextToken} -> nextToken) (\s@ListGroupsResponse' {} a -> s {nextToken = a} :: ListGroupsResponse)

-- | The response's http status code.
listGroupsResponse_httpStatus :: Lens.Lens' ListGroupsResponse Core.Int
listGroupsResponse_httpStatus = Lens.lens (\ListGroupsResponse' {httpStatus} -> httpStatus) (\s@ListGroupsResponse' {} a -> s {httpStatus = a} :: ListGroupsResponse)

instance Core.NFData ListGroupsResponse
