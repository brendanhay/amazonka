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
-- Module      : Amazonka.ECRPublic.DescribeRepositories
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes repositories in a public registry.
--
-- This operation returns paginated results.
module Amazonka.ECRPublic.DescribeRepositories
  ( -- * Creating a Request
    DescribeRepositories (..),
    newDescribeRepositories,

    -- * Request Lenses
    describeRepositories_maxResults,
    describeRepositories_nextToken,
    describeRepositories_registryId,
    describeRepositories_repositoryNames,

    -- * Destructuring the Response
    DescribeRepositoriesResponse (..),
    newDescribeRepositoriesResponse,

    -- * Response Lenses
    describeRepositoriesResponse_nextToken,
    describeRepositoriesResponse_repositories,
    describeRepositoriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECRPublic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRepositories' smart constructor.
data DescribeRepositories = DescribeRepositories'
  { -- | The maximum number of repository results returned by
    -- @DescribeRepositories@ in paginated output. When this parameter is used,
    -- @DescribeRepositories@ only returns @maxResults@ results in a single
    -- page along with a @nextToken@ response element. The remaining results of
    -- the initial request can be seen by sending another
    -- @DescribeRepositories@ request with the returned @nextToken@ value. This
    -- value can be between 1 and 1000. If this parameter is not used, then
    -- @DescribeRepositories@ returns up to 100 results and a @nextToken@
    -- value, if applicable. This option cannot be used when you specify
    -- repositories with @repositoryNames@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeRepositories@ request where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    -- This value is @null@ when there are no more results to return. This
    -- option cannot be used when you specify repositories with
    -- @repositoryNames@.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID associated with the registry that contains the
    -- repositories to be described. If you do not specify a registry, the
    -- default public registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | A list of repositories to describe. If this parameter is omitted, then
    -- all repositories in a registry are described.
    repositoryNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRepositories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeRepositories_maxResults' - The maximum number of repository results returned by
-- @DescribeRepositories@ in paginated output. When this parameter is used,
-- @DescribeRepositories@ only returns @maxResults@ results in a single
-- page along with a @nextToken@ response element. The remaining results of
-- the initial request can be seen by sending another
-- @DescribeRepositories@ request with the returned @nextToken@ value. This
-- value can be between 1 and 1000. If this parameter is not used, then
-- @DescribeRepositories@ returns up to 100 results and a @nextToken@
-- value, if applicable. This option cannot be used when you specify
-- repositories with @repositoryNames@.
--
-- 'nextToken', 'describeRepositories_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeRepositories@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is @null@ when there are no more results to return. This
-- option cannot be used when you specify repositories with
-- @repositoryNames@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'registryId', 'describeRepositories_registryId' - The AWS account ID associated with the registry that contains the
-- repositories to be described. If you do not specify a registry, the
-- default public registry is assumed.
--
-- 'repositoryNames', 'describeRepositories_repositoryNames' - A list of repositories to describe. If this parameter is omitted, then
-- all repositories in a registry are described.
newDescribeRepositories ::
  DescribeRepositories
newDescribeRepositories =
  DescribeRepositories'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryNames = Prelude.Nothing
    }

-- | The maximum number of repository results returned by
-- @DescribeRepositories@ in paginated output. When this parameter is used,
-- @DescribeRepositories@ only returns @maxResults@ results in a single
-- page along with a @nextToken@ response element. The remaining results of
-- the initial request can be seen by sending another
-- @DescribeRepositories@ request with the returned @nextToken@ value. This
-- value can be between 1 and 1000. If this parameter is not used, then
-- @DescribeRepositories@ returns up to 100 results and a @nextToken@
-- value, if applicable. This option cannot be used when you specify
-- repositories with @repositoryNames@.
describeRepositories_maxResults :: Lens.Lens' DescribeRepositories (Prelude.Maybe Prelude.Natural)
describeRepositories_maxResults = Lens.lens (\DescribeRepositories' {maxResults} -> maxResults) (\s@DescribeRepositories' {} a -> s {maxResults = a} :: DescribeRepositories)

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeRepositories@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is @null@ when there are no more results to return. This
-- option cannot be used when you specify repositories with
-- @repositoryNames@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
describeRepositories_nextToken :: Lens.Lens' DescribeRepositories (Prelude.Maybe Prelude.Text)
describeRepositories_nextToken = Lens.lens (\DescribeRepositories' {nextToken} -> nextToken) (\s@DescribeRepositories' {} a -> s {nextToken = a} :: DescribeRepositories)

-- | The AWS account ID associated with the registry that contains the
-- repositories to be described. If you do not specify a registry, the
-- default public registry is assumed.
describeRepositories_registryId :: Lens.Lens' DescribeRepositories (Prelude.Maybe Prelude.Text)
describeRepositories_registryId = Lens.lens (\DescribeRepositories' {registryId} -> registryId) (\s@DescribeRepositories' {} a -> s {registryId = a} :: DescribeRepositories)

-- | A list of repositories to describe. If this parameter is omitted, then
-- all repositories in a registry are described.
describeRepositories_repositoryNames :: Lens.Lens' DescribeRepositories (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeRepositories_repositoryNames = Lens.lens (\DescribeRepositories' {repositoryNames} -> repositoryNames) (\s@DescribeRepositories' {} a -> s {repositoryNames = a} :: DescribeRepositories) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeRepositories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeRepositoriesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeRepositoriesResponse_repositories
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeRepositories_nextToken
              Lens..~ rs
              Lens.^? describeRepositoriesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeRepositories where
  type
    AWSResponse DescribeRepositories =
      DescribeRepositoriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRepositoriesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "repositories" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRepositories where
  hashWithSalt _salt DescribeRepositories' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryNames

instance Prelude.NFData DescribeRepositories where
  rnf DescribeRepositories' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf registryId `Prelude.seq`
          Prelude.rnf repositoryNames

instance Data.ToHeaders DescribeRepositories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SpencerFrontendService.DescribeRepositories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRepositories where
  toJSON DescribeRepositories' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("registryId" Data..=) Prelude.<$> registryId,
            ("repositoryNames" Data..=)
              Prelude.<$> repositoryNames
          ]
      )

instance Data.ToPath DescribeRepositories where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRepositories where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRepositoriesResponse' smart constructor.
data DescribeRepositoriesResponse = DescribeRepositoriesResponse'
  { -- | The @nextToken@ value to include in a future @DescribeRepositories@
    -- request. When the results of a @DescribeRepositories@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of repository objects corresponding to valid repositories.
    repositories :: Prelude.Maybe [Repository],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRepositoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRepositoriesResponse_nextToken' - The @nextToken@ value to include in a future @DescribeRepositories@
-- request. When the results of a @DescribeRepositories@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'repositories', 'describeRepositoriesResponse_repositories' - A list of repository objects corresponding to valid repositories.
--
-- 'httpStatus', 'describeRepositoriesResponse_httpStatus' - The response's http status code.
newDescribeRepositoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRepositoriesResponse
newDescribeRepositoriesResponse pHttpStatus_ =
  DescribeRepositoriesResponse'
    { nextToken =
        Prelude.Nothing,
      repositories = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @DescribeRepositories@
-- request. When the results of a @DescribeRepositories@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
describeRepositoriesResponse_nextToken :: Lens.Lens' DescribeRepositoriesResponse (Prelude.Maybe Prelude.Text)
describeRepositoriesResponse_nextToken = Lens.lens (\DescribeRepositoriesResponse' {nextToken} -> nextToken) (\s@DescribeRepositoriesResponse' {} a -> s {nextToken = a} :: DescribeRepositoriesResponse)

-- | A list of repository objects corresponding to valid repositories.
describeRepositoriesResponse_repositories :: Lens.Lens' DescribeRepositoriesResponse (Prelude.Maybe [Repository])
describeRepositoriesResponse_repositories = Lens.lens (\DescribeRepositoriesResponse' {repositories} -> repositories) (\s@DescribeRepositoriesResponse' {} a -> s {repositories = a} :: DescribeRepositoriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRepositoriesResponse_httpStatus :: Lens.Lens' DescribeRepositoriesResponse Prelude.Int
describeRepositoriesResponse_httpStatus = Lens.lens (\DescribeRepositoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeRepositoriesResponse' {} a -> s {httpStatus = a} :: DescribeRepositoriesResponse)

instance Prelude.NFData DescribeRepositoriesResponse where
  rnf DescribeRepositoriesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf repositories `Prelude.seq`
        Prelude.rnf httpStatus
