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
-- Module      : Network.AWS.WorkMail.ListAliases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a paginated call to list the aliases associated with a given
-- entity.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListAliases
  ( -- * Creating a Request
    ListAliases (..),
    newListAliases,

    -- * Request Lenses
    listAliases_nextToken,
    listAliases_maxResults,
    listAliases_organizationId,
    listAliases_entityId,

    -- * Destructuring the Response
    ListAliasesResponse (..),
    newListAliasesResponse,

    -- * Response Lenses
    listAliasesResponse_nextToken,
    listAliasesResponse_aliases,
    listAliasesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newListAliases' smart constructor.
data ListAliases = ListAliases'
  { -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier for the organization under which the entity exists.
    organizationId :: Core.Text,
    -- | The identifier for the entity for which to list the aliases.
    entityId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAliases_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
--
-- 'maxResults', 'listAliases_maxResults' - The maximum number of results to return in a single call.
--
-- 'organizationId', 'listAliases_organizationId' - The identifier for the organization under which the entity exists.
--
-- 'entityId', 'listAliases_entityId' - The identifier for the entity for which to list the aliases.
newListAliases ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'entityId'
  Core.Text ->
  ListAliases
newListAliases pOrganizationId_ pEntityId_ =
  ListAliases'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      organizationId = pOrganizationId_,
      entityId = pEntityId_
    }

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listAliases_nextToken :: Lens.Lens' ListAliases (Core.Maybe Core.Text)
listAliases_nextToken = Lens.lens (\ListAliases' {nextToken} -> nextToken) (\s@ListAliases' {} a -> s {nextToken = a} :: ListAliases)

-- | The maximum number of results to return in a single call.
listAliases_maxResults :: Lens.Lens' ListAliases (Core.Maybe Core.Natural)
listAliases_maxResults = Lens.lens (\ListAliases' {maxResults} -> maxResults) (\s@ListAliases' {} a -> s {maxResults = a} :: ListAliases)

-- | The identifier for the organization under which the entity exists.
listAliases_organizationId :: Lens.Lens' ListAliases Core.Text
listAliases_organizationId = Lens.lens (\ListAliases' {organizationId} -> organizationId) (\s@ListAliases' {} a -> s {organizationId = a} :: ListAliases)

-- | The identifier for the entity for which to list the aliases.
listAliases_entityId :: Lens.Lens' ListAliases Core.Text
listAliases_entityId = Lens.lens (\ListAliases' {entityId} -> entityId) (\s@ListAliases' {} a -> s {entityId = a} :: ListAliases)

instance Core.AWSPager ListAliases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_aliases Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAliases_nextToken
          Lens..~ rs
          Lens.^? listAliasesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListAliases where
  type AWSResponse ListAliases = ListAliasesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Aliases" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAliases

instance Core.NFData ListAliases

instance Core.ToHeaders ListAliases where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("WorkMailService.ListAliases" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAliases where
  toJSON ListAliases' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId)
          ]
      )

instance Core.ToPath ListAliases where
  toPath = Core.const "/"

instance Core.ToQuery ListAliases where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { -- | The token to use to retrieve the next page of results. The value is
    -- \"null\" when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The entity\'s paginated aliases.
    aliases :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAliasesResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- \"null\" when there are no more results to return.
--
-- 'aliases', 'listAliasesResponse_aliases' - The entity\'s paginated aliases.
--
-- 'httpStatus', 'listAliasesResponse_httpStatus' - The response's http status code.
newListAliasesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAliasesResponse
newListAliasesResponse pHttpStatus_ =
  ListAliasesResponse'
    { nextToken = Core.Nothing,
      aliases = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. The value is
-- \"null\" when there are no more results to return.
listAliasesResponse_nextToken :: Lens.Lens' ListAliasesResponse (Core.Maybe Core.Text)
listAliasesResponse_nextToken = Lens.lens (\ListAliasesResponse' {nextToken} -> nextToken) (\s@ListAliasesResponse' {} a -> s {nextToken = a} :: ListAliasesResponse)

-- | The entity\'s paginated aliases.
listAliasesResponse_aliases :: Lens.Lens' ListAliasesResponse (Core.Maybe [Core.Text])
listAliasesResponse_aliases = Lens.lens (\ListAliasesResponse' {aliases} -> aliases) (\s@ListAliasesResponse' {} a -> s {aliases = a} :: ListAliasesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAliasesResponse_httpStatus :: Lens.Lens' ListAliasesResponse Core.Int
listAliasesResponse_httpStatus = Lens.lens (\ListAliasesResponse' {httpStatus} -> httpStatus) (\s@ListAliasesResponse' {} a -> s {httpStatus = a} :: ListAliasesResponse)

instance Core.NFData ListAliasesResponse
