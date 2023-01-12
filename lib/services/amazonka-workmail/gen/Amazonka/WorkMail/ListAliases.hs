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
-- Module      : Amazonka.WorkMail.ListAliases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a paginated call to list the aliases associated with a given
-- entity.
--
-- This operation returns paginated results.
module Amazonka.WorkMail.ListAliases
  ( -- * Creating a Request
    ListAliases (..),
    newListAliases,

    -- * Request Lenses
    listAliases_maxResults,
    listAliases_nextToken,
    listAliases_organizationId,
    listAliases_entityId,

    -- * Destructuring the Response
    ListAliasesResponse (..),
    newListAliasesResponse,

    -- * Response Lenses
    listAliasesResponse_aliases,
    listAliasesResponse_nextToken,
    listAliasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newListAliases' smart constructor.
data ListAliases = ListAliases'
  { -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use to retrieve the next page of results. The first call
    -- does not contain any tokens.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the organization under which the entity exists.
    organizationId :: Prelude.Text,
    -- | The identifier for the entity for which to list the aliases.
    entityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAliases_maxResults' - The maximum number of results to return in a single call.
--
-- 'nextToken', 'listAliases_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
--
-- 'organizationId', 'listAliases_organizationId' - The identifier for the organization under which the entity exists.
--
-- 'entityId', 'listAliases_entityId' - The identifier for the entity for which to list the aliases.
newListAliases ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  ListAliases
newListAliases pOrganizationId_ pEntityId_ =
  ListAliases'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      organizationId = pOrganizationId_,
      entityId = pEntityId_
    }

-- | The maximum number of results to return in a single call.
listAliases_maxResults :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Natural)
listAliases_maxResults = Lens.lens (\ListAliases' {maxResults} -> maxResults) (\s@ListAliases' {} a -> s {maxResults = a} :: ListAliases)

-- | The token to use to retrieve the next page of results. The first call
-- does not contain any tokens.
listAliases_nextToken :: Lens.Lens' ListAliases (Prelude.Maybe Prelude.Text)
listAliases_nextToken = Lens.lens (\ListAliases' {nextToken} -> nextToken) (\s@ListAliases' {} a -> s {nextToken = a} :: ListAliases)

-- | The identifier for the organization under which the entity exists.
listAliases_organizationId :: Lens.Lens' ListAliases Prelude.Text
listAliases_organizationId = Lens.lens (\ListAliases' {organizationId} -> organizationId) (\s@ListAliases' {} a -> s {organizationId = a} :: ListAliases)

-- | The identifier for the entity for which to list the aliases.
listAliases_entityId :: Lens.Lens' ListAliases Prelude.Text
listAliases_entityId = Lens.lens (\ListAliases' {entityId} -> entityId) (\s@ListAliases' {} a -> s {entityId = a} :: ListAliases)

instance Core.AWSPager ListAliases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAliasesResponse_aliases Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAliases_nextToken
          Lens..~ rs
          Lens.^? listAliasesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListAliases where
  type AWSResponse ListAliases = ListAliasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAliasesResponse'
            Prelude.<$> (x Data..?> "Aliases" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAliases where
  hashWithSalt _salt ListAliases' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` entityId

instance Prelude.NFData ListAliases where
  rnf ListAliases' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf entityId

instance Data.ToHeaders ListAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.ListAliases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAliases where
  toJSON ListAliases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("EntityId" Data..= entityId)
          ]
      )

instance Data.ToPath ListAliases where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAliases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { -- | The entity\'s paginated aliases.
    aliases :: Prelude.Maybe [Prelude.Text],
    -- | The token to use to retrieve the next page of results. The value is
    -- \"null\" when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliases', 'listAliasesResponse_aliases' - The entity\'s paginated aliases.
--
-- 'nextToken', 'listAliasesResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- \"null\" when there are no more results to return.
--
-- 'httpStatus', 'listAliasesResponse_httpStatus' - The response's http status code.
newListAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAliasesResponse
newListAliasesResponse pHttpStatus_ =
  ListAliasesResponse'
    { aliases = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The entity\'s paginated aliases.
listAliasesResponse_aliases :: Lens.Lens' ListAliasesResponse (Prelude.Maybe [Prelude.Text])
listAliasesResponse_aliases = Lens.lens (\ListAliasesResponse' {aliases} -> aliases) (\s@ListAliasesResponse' {} a -> s {aliases = a} :: ListAliasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. The value is
-- \"null\" when there are no more results to return.
listAliasesResponse_nextToken :: Lens.Lens' ListAliasesResponse (Prelude.Maybe Prelude.Text)
listAliasesResponse_nextToken = Lens.lens (\ListAliasesResponse' {nextToken} -> nextToken) (\s@ListAliasesResponse' {} a -> s {nextToken = a} :: ListAliasesResponse)

-- | The response's http status code.
listAliasesResponse_httpStatus :: Lens.Lens' ListAliasesResponse Prelude.Int
listAliasesResponse_httpStatus = Lens.lens (\ListAliasesResponse' {httpStatus} -> httpStatus) (\s@ListAliasesResponse' {} a -> s {httpStatus = a} :: ListAliasesResponse)

instance Prelude.NFData ListAliasesResponse where
  rnf ListAliasesResponse' {..} =
    Prelude.rnf aliases
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
