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
-- Module      : Network.AWS.WorkMail.ListMailboxPermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the mailbox permissions associated with a user, group, or resource
-- mailbox.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListMailboxPermissions
  ( -- * Creating a Request
    ListMailboxPermissions (..),
    newListMailboxPermissions,

    -- * Request Lenses
    listMailboxPermissions_nextToken,
    listMailboxPermissions_maxResults,
    listMailboxPermissions_organizationId,
    listMailboxPermissions_entityId,

    -- * Destructuring the Response
    ListMailboxPermissionsResponse (..),
    newListMailboxPermissionsResponse,

    -- * Response Lenses
    listMailboxPermissionsResponse_nextToken,
    listMailboxPermissionsResponse_permissions,
    listMailboxPermissionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newListMailboxPermissions' smart constructor.
data ListMailboxPermissions = ListMailboxPermissions'
  { -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the organization under which the user, group, or
    -- resource exists.
    organizationId :: Core.Text,
    -- | The identifier of the user, group, or resource for which to list mailbox
    -- permissions.
    entityId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMailboxPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMailboxPermissions_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
--
-- 'maxResults', 'listMailboxPermissions_maxResults' - The maximum number of results to return in a single call.
--
-- 'organizationId', 'listMailboxPermissions_organizationId' - The identifier of the organization under which the user, group, or
-- resource exists.
--
-- 'entityId', 'listMailboxPermissions_entityId' - The identifier of the user, group, or resource for which to list mailbox
-- permissions.
newListMailboxPermissions ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'entityId'
  Core.Text ->
  ListMailboxPermissions
newListMailboxPermissions pOrganizationId_ pEntityId_ =
  ListMailboxPermissions'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      organizationId = pOrganizationId_,
      entityId = pEntityId_
    }

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listMailboxPermissions_nextToken :: Lens.Lens' ListMailboxPermissions (Core.Maybe Core.Text)
listMailboxPermissions_nextToken = Lens.lens (\ListMailboxPermissions' {nextToken} -> nextToken) (\s@ListMailboxPermissions' {} a -> s {nextToken = a} :: ListMailboxPermissions)

-- | The maximum number of results to return in a single call.
listMailboxPermissions_maxResults :: Lens.Lens' ListMailboxPermissions (Core.Maybe Core.Natural)
listMailboxPermissions_maxResults = Lens.lens (\ListMailboxPermissions' {maxResults} -> maxResults) (\s@ListMailboxPermissions' {} a -> s {maxResults = a} :: ListMailboxPermissions)

-- | The identifier of the organization under which the user, group, or
-- resource exists.
listMailboxPermissions_organizationId :: Lens.Lens' ListMailboxPermissions Core.Text
listMailboxPermissions_organizationId = Lens.lens (\ListMailboxPermissions' {organizationId} -> organizationId) (\s@ListMailboxPermissions' {} a -> s {organizationId = a} :: ListMailboxPermissions)

-- | The identifier of the user, group, or resource for which to list mailbox
-- permissions.
listMailboxPermissions_entityId :: Lens.Lens' ListMailboxPermissions Core.Text
listMailboxPermissions_entityId = Lens.lens (\ListMailboxPermissions' {entityId} -> entityId) (\s@ListMailboxPermissions' {} a -> s {entityId = a} :: ListMailboxPermissions)

instance Core.AWSPager ListMailboxPermissions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMailboxPermissionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listMailboxPermissionsResponse_permissions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listMailboxPermissions_nextToken
          Lens..~ rs
          Lens.^? listMailboxPermissionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListMailboxPermissions where
  type
    AWSResponse ListMailboxPermissions =
      ListMailboxPermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMailboxPermissionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Permissions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListMailboxPermissions

instance Core.NFData ListMailboxPermissions

instance Core.ToHeaders ListMailboxPermissions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.ListMailboxPermissions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListMailboxPermissions where
  toJSON ListMailboxPermissions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId)
          ]
      )

instance Core.ToPath ListMailboxPermissions where
  toPath = Core.const "/"

instance Core.ToQuery ListMailboxPermissions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListMailboxPermissionsResponse' smart constructor.
data ListMailboxPermissionsResponse = ListMailboxPermissionsResponse'
  { -- | The token to use to retrieve the next page of results. The value is
    -- \"null\" when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | One page of the user, group, or resource mailbox permissions.
    permissions :: Core.Maybe [Permission],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMailboxPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMailboxPermissionsResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- \"null\" when there are no more results to return.
--
-- 'permissions', 'listMailboxPermissionsResponse_permissions' - One page of the user, group, or resource mailbox permissions.
--
-- 'httpStatus', 'listMailboxPermissionsResponse_httpStatus' - The response's http status code.
newListMailboxPermissionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListMailboxPermissionsResponse
newListMailboxPermissionsResponse pHttpStatus_ =
  ListMailboxPermissionsResponse'
    { nextToken =
        Core.Nothing,
      permissions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. The value is
-- \"null\" when there are no more results to return.
listMailboxPermissionsResponse_nextToken :: Lens.Lens' ListMailboxPermissionsResponse (Core.Maybe Core.Text)
listMailboxPermissionsResponse_nextToken = Lens.lens (\ListMailboxPermissionsResponse' {nextToken} -> nextToken) (\s@ListMailboxPermissionsResponse' {} a -> s {nextToken = a} :: ListMailboxPermissionsResponse)

-- | One page of the user, group, or resource mailbox permissions.
listMailboxPermissionsResponse_permissions :: Lens.Lens' ListMailboxPermissionsResponse (Core.Maybe [Permission])
listMailboxPermissionsResponse_permissions = Lens.lens (\ListMailboxPermissionsResponse' {permissions} -> permissions) (\s@ListMailboxPermissionsResponse' {} a -> s {permissions = a} :: ListMailboxPermissionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listMailboxPermissionsResponse_httpStatus :: Lens.Lens' ListMailboxPermissionsResponse Core.Int
listMailboxPermissionsResponse_httpStatus = Lens.lens (\ListMailboxPermissionsResponse' {httpStatus} -> httpStatus) (\s@ListMailboxPermissionsResponse' {} a -> s {httpStatus = a} :: ListMailboxPermissionsResponse)

instance Core.NFData ListMailboxPermissionsResponse
