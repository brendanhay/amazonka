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
-- Module      : Amazonka.WorkMail.ListMailboxPermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the mailbox permissions associated with a user, group, or resource
-- mailbox.
--
-- This operation returns paginated results.
module Amazonka.WorkMail.ListMailboxPermissions
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newListMailboxPermissions' smart constructor.
data ListMailboxPermissions = ListMailboxPermissions'
  { -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the organization under which the user, group, or
    -- resource exists.
    organizationId :: Prelude.Text,
    -- | The identifier of the user, group, or resource for which to list mailbox
    -- permissions.
    entityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  ListMailboxPermissions
newListMailboxPermissions pOrganizationId_ pEntityId_ =
  ListMailboxPermissions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      organizationId = pOrganizationId_,
      entityId = pEntityId_
    }

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listMailboxPermissions_nextToken :: Lens.Lens' ListMailboxPermissions (Prelude.Maybe Prelude.Text)
listMailboxPermissions_nextToken = Lens.lens (\ListMailboxPermissions' {nextToken} -> nextToken) (\s@ListMailboxPermissions' {} a -> s {nextToken = a} :: ListMailboxPermissions)

-- | The maximum number of results to return in a single call.
listMailboxPermissions_maxResults :: Lens.Lens' ListMailboxPermissions (Prelude.Maybe Prelude.Natural)
listMailboxPermissions_maxResults = Lens.lens (\ListMailboxPermissions' {maxResults} -> maxResults) (\s@ListMailboxPermissions' {} a -> s {maxResults = a} :: ListMailboxPermissions)

-- | The identifier of the organization under which the user, group, or
-- resource exists.
listMailboxPermissions_organizationId :: Lens.Lens' ListMailboxPermissions Prelude.Text
listMailboxPermissions_organizationId = Lens.lens (\ListMailboxPermissions' {organizationId} -> organizationId) (\s@ListMailboxPermissions' {} a -> s {organizationId = a} :: ListMailboxPermissions)

-- | The identifier of the user, group, or resource for which to list mailbox
-- permissions.
listMailboxPermissions_entityId :: Lens.Lens' ListMailboxPermissions Prelude.Text
listMailboxPermissions_entityId = Lens.lens (\ListMailboxPermissions' {entityId} -> entityId) (\s@ListMailboxPermissions' {} a -> s {entityId = a} :: ListMailboxPermissions)

instance Core.AWSPager ListMailboxPermissions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMailboxPermissionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMailboxPermissionsResponse_permissions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMailboxPermissions_nextToken
          Lens..~ rs
          Lens.^? listMailboxPermissionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListMailboxPermissions where
  type
    AWSResponse ListMailboxPermissions =
      ListMailboxPermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMailboxPermissionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Permissions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMailboxPermissions where
  hashWithSalt _salt ListMailboxPermissions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` entityId

instance Prelude.NFData ListMailboxPermissions where
  rnf ListMailboxPermissions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf entityId

instance Data.ToHeaders ListMailboxPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.ListMailboxPermissions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMailboxPermissions where
  toJSON ListMailboxPermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("EntityId" Data..= entityId)
          ]
      )

instance Data.ToPath ListMailboxPermissions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMailboxPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMailboxPermissionsResponse' smart constructor.
data ListMailboxPermissionsResponse = ListMailboxPermissionsResponse'
  { -- | The token to use to retrieve the next page of results. The value is
    -- \"null\" when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One page of the user, group, or resource mailbox permissions.
    permissions :: Prelude.Maybe [Permission],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListMailboxPermissionsResponse
newListMailboxPermissionsResponse pHttpStatus_ =
  ListMailboxPermissionsResponse'
    { nextToken =
        Prelude.Nothing,
      permissions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. The value is
-- \"null\" when there are no more results to return.
listMailboxPermissionsResponse_nextToken :: Lens.Lens' ListMailboxPermissionsResponse (Prelude.Maybe Prelude.Text)
listMailboxPermissionsResponse_nextToken = Lens.lens (\ListMailboxPermissionsResponse' {nextToken} -> nextToken) (\s@ListMailboxPermissionsResponse' {} a -> s {nextToken = a} :: ListMailboxPermissionsResponse)

-- | One page of the user, group, or resource mailbox permissions.
listMailboxPermissionsResponse_permissions :: Lens.Lens' ListMailboxPermissionsResponse (Prelude.Maybe [Permission])
listMailboxPermissionsResponse_permissions = Lens.lens (\ListMailboxPermissionsResponse' {permissions} -> permissions) (\s@ListMailboxPermissionsResponse' {} a -> s {permissions = a} :: ListMailboxPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMailboxPermissionsResponse_httpStatus :: Lens.Lens' ListMailboxPermissionsResponse Prelude.Int
listMailboxPermissionsResponse_httpStatus = Lens.lens (\ListMailboxPermissionsResponse' {httpStatus} -> httpStatus) (\s@ListMailboxPermissionsResponse' {} a -> s {httpStatus = a} :: ListMailboxPermissionsResponse)

instance
  Prelude.NFData
    ListMailboxPermissionsResponse
  where
  rnf ListMailboxPermissionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf httpStatus
