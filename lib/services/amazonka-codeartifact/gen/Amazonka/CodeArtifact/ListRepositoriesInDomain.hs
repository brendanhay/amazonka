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
-- Module      : Amazonka.CodeArtifact.ListRepositoriesInDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_RepositorySummary.html RepositorySummary>
-- objects. Each @RepositorySummary@ contains information about a
-- repository in the specified domain and that matches the input
-- parameters.
--
-- This operation returns paginated results.
module Amazonka.CodeArtifact.ListRepositoriesInDomain
  ( -- * Creating a Request
    ListRepositoriesInDomain (..),
    newListRepositoriesInDomain,

    -- * Request Lenses
    listRepositoriesInDomain_administratorAccount,
    listRepositoriesInDomain_domainOwner,
    listRepositoriesInDomain_maxResults,
    listRepositoriesInDomain_nextToken,
    listRepositoriesInDomain_repositoryPrefix,
    listRepositoriesInDomain_domain,

    -- * Destructuring the Response
    ListRepositoriesInDomainResponse (..),
    newListRepositoriesInDomainResponse,

    -- * Response Lenses
    listRepositoriesInDomainResponse_nextToken,
    listRepositoriesInDomainResponse_repositories,
    listRepositoriesInDomainResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRepositoriesInDomain' smart constructor.
data ListRepositoriesInDomain = ListRepositoriesInDomain'
  { -- | Filter the list of repositories to only include those that are managed
    -- by the Amazon Web Services account ID.
    administratorAccount :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A prefix used to filter returned repositories. Only repositories with
    -- names that start with @repositoryPrefix@ are returned.
    repositoryPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the returned list of repositories.
    domain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRepositoriesInDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'administratorAccount', 'listRepositoriesInDomain_administratorAccount' - Filter the list of repositories to only include those that are managed
-- by the Amazon Web Services account ID.
--
-- 'domainOwner', 'listRepositoriesInDomain_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'maxResults', 'listRepositoriesInDomain_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listRepositoriesInDomain_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'repositoryPrefix', 'listRepositoriesInDomain_repositoryPrefix' - A prefix used to filter returned repositories. Only repositories with
-- names that start with @repositoryPrefix@ are returned.
--
-- 'domain', 'listRepositoriesInDomain_domain' - The name of the domain that contains the returned list of repositories.
newListRepositoriesInDomain ::
  -- | 'domain'
  Prelude.Text ->
  ListRepositoriesInDomain
newListRepositoriesInDomain pDomain_ =
  ListRepositoriesInDomain'
    { administratorAccount =
        Prelude.Nothing,
      domainOwner = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      repositoryPrefix = Prelude.Nothing,
      domain = pDomain_
    }

-- | Filter the list of repositories to only include those that are managed
-- by the Amazon Web Services account ID.
listRepositoriesInDomain_administratorAccount :: Lens.Lens' ListRepositoriesInDomain (Prelude.Maybe Prelude.Text)
listRepositoriesInDomain_administratorAccount = Lens.lens (\ListRepositoriesInDomain' {administratorAccount} -> administratorAccount) (\s@ListRepositoriesInDomain' {} a -> s {administratorAccount = a} :: ListRepositoriesInDomain)

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
listRepositoriesInDomain_domainOwner :: Lens.Lens' ListRepositoriesInDomain (Prelude.Maybe Prelude.Text)
listRepositoriesInDomain_domainOwner = Lens.lens (\ListRepositoriesInDomain' {domainOwner} -> domainOwner) (\s@ListRepositoriesInDomain' {} a -> s {domainOwner = a} :: ListRepositoriesInDomain)

-- | The maximum number of results to return per page.
listRepositoriesInDomain_maxResults :: Lens.Lens' ListRepositoriesInDomain (Prelude.Maybe Prelude.Natural)
listRepositoriesInDomain_maxResults = Lens.lens (\ListRepositoriesInDomain' {maxResults} -> maxResults) (\s@ListRepositoriesInDomain' {} a -> s {maxResults = a} :: ListRepositoriesInDomain)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listRepositoriesInDomain_nextToken :: Lens.Lens' ListRepositoriesInDomain (Prelude.Maybe Prelude.Text)
listRepositoriesInDomain_nextToken = Lens.lens (\ListRepositoriesInDomain' {nextToken} -> nextToken) (\s@ListRepositoriesInDomain' {} a -> s {nextToken = a} :: ListRepositoriesInDomain)

-- | A prefix used to filter returned repositories. Only repositories with
-- names that start with @repositoryPrefix@ are returned.
listRepositoriesInDomain_repositoryPrefix :: Lens.Lens' ListRepositoriesInDomain (Prelude.Maybe Prelude.Text)
listRepositoriesInDomain_repositoryPrefix = Lens.lens (\ListRepositoriesInDomain' {repositoryPrefix} -> repositoryPrefix) (\s@ListRepositoriesInDomain' {} a -> s {repositoryPrefix = a} :: ListRepositoriesInDomain)

-- | The name of the domain that contains the returned list of repositories.
listRepositoriesInDomain_domain :: Lens.Lens' ListRepositoriesInDomain Prelude.Text
listRepositoriesInDomain_domain = Lens.lens (\ListRepositoriesInDomain' {domain} -> domain) (\s@ListRepositoriesInDomain' {} a -> s {domain = a} :: ListRepositoriesInDomain)

instance Core.AWSPager ListRepositoriesInDomain where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRepositoriesInDomainResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRepositoriesInDomainResponse_repositories
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRepositoriesInDomain_nextToken
          Lens..~ rs
          Lens.^? listRepositoriesInDomainResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRepositoriesInDomain where
  type
    AWSResponse ListRepositoriesInDomain =
      ListRepositoriesInDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRepositoriesInDomainResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "repositories" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRepositoriesInDomain where
  hashWithSalt _salt ListRepositoriesInDomain' {..} =
    _salt
      `Prelude.hashWithSalt` administratorAccount
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` repositoryPrefix
      `Prelude.hashWithSalt` domain

instance Prelude.NFData ListRepositoriesInDomain where
  rnf ListRepositoriesInDomain' {..} =
    Prelude.rnf administratorAccount
      `Prelude.seq` Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf repositoryPrefix
      `Prelude.seq` Prelude.rnf domain

instance Data.ToHeaders ListRepositoriesInDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRepositoriesInDomain where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListRepositoriesInDomain where
  toPath = Prelude.const "/v1/domain/repositories"

instance Data.ToQuery ListRepositoriesInDomain where
  toQuery ListRepositoriesInDomain' {..} =
    Prelude.mconcat
      [ "administrator-account"
          Data.=: administratorAccount,
        "domain-owner" Data.=: domainOwner,
        "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken,
        "repository-prefix" Data.=: repositoryPrefix,
        "domain" Data.=: domain
      ]

-- | /See:/ 'newListRepositoriesInDomainResponse' smart constructor.
data ListRepositoriesInDomainResponse = ListRepositoriesInDomainResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The returned list of repositories.
    repositories :: Prelude.Maybe [RepositorySummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRepositoriesInDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRepositoriesInDomainResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'repositories', 'listRepositoriesInDomainResponse_repositories' - The returned list of repositories.
--
-- 'httpStatus', 'listRepositoriesInDomainResponse_httpStatus' - The response's http status code.
newListRepositoriesInDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRepositoriesInDomainResponse
newListRepositoriesInDomainResponse pHttpStatus_ =
  ListRepositoriesInDomainResponse'
    { nextToken =
        Prelude.Nothing,
      repositories = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listRepositoriesInDomainResponse_nextToken :: Lens.Lens' ListRepositoriesInDomainResponse (Prelude.Maybe Prelude.Text)
listRepositoriesInDomainResponse_nextToken = Lens.lens (\ListRepositoriesInDomainResponse' {nextToken} -> nextToken) (\s@ListRepositoriesInDomainResponse' {} a -> s {nextToken = a} :: ListRepositoriesInDomainResponse)

-- | The returned list of repositories.
listRepositoriesInDomainResponse_repositories :: Lens.Lens' ListRepositoriesInDomainResponse (Prelude.Maybe [RepositorySummary])
listRepositoriesInDomainResponse_repositories = Lens.lens (\ListRepositoriesInDomainResponse' {repositories} -> repositories) (\s@ListRepositoriesInDomainResponse' {} a -> s {repositories = a} :: ListRepositoriesInDomainResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRepositoriesInDomainResponse_httpStatus :: Lens.Lens' ListRepositoriesInDomainResponse Prelude.Int
listRepositoriesInDomainResponse_httpStatus = Lens.lens (\ListRepositoriesInDomainResponse' {httpStatus} -> httpStatus) (\s@ListRepositoriesInDomainResponse' {} a -> s {httpStatus = a} :: ListRepositoriesInDomainResponse)

instance
  Prelude.NFData
    ListRepositoriesInDomainResponse
  where
  rnf ListRepositoriesInDomainResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf repositories
      `Prelude.seq` Prelude.rnf httpStatus
