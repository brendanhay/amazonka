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
-- Module      : Network.AWS.ECR.DescribeRepositories
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes image repositories in a registry.
--
-- This operation returns paginated results.
module Network.AWS.ECR.DescribeRepositories
  ( -- * Creating a Request
    DescribeRepositories (..),
    newDescribeRepositories,

    -- * Request Lenses
    describeRepositories_nextToken,
    describeRepositories_maxResults,
    describeRepositories_repositoryNames,
    describeRepositories_registryId,

    -- * Destructuring the Response
    DescribeRepositoriesResponse (..),
    newDescribeRepositoriesResponse,

    -- * Response Lenses
    describeRepositoriesResponse_nextToken,
    describeRepositoriesResponse_repositories,
    describeRepositoriesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRepositories' smart constructor.
data DescribeRepositories = DescribeRepositories'
  { -- | The @nextToken@ value returned from a previous paginated
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
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A list of repositories to describe. If this parameter is omitted, then
    -- all repositories in a registry are described.
    repositoryNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The AWS account ID associated with the registry that contains the
    -- repositories to be described. If you do not specify a registry, the
    -- default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text
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
-- 'repositoryNames', 'describeRepositories_repositoryNames' - A list of repositories to describe. If this parameter is omitted, then
-- all repositories in a registry are described.
--
-- 'registryId', 'describeRepositories_registryId' - The AWS account ID associated with the registry that contains the
-- repositories to be described. If you do not specify a registry, the
-- default registry is assumed.
newDescribeRepositories ::
  DescribeRepositories
newDescribeRepositories =
  DescribeRepositories'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      repositoryNames = Prelude.Nothing,
      registryId = Prelude.Nothing
    }

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

-- | A list of repositories to describe. If this parameter is omitted, then
-- all repositories in a registry are described.
describeRepositories_repositoryNames :: Lens.Lens' DescribeRepositories (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeRepositories_repositoryNames = Lens.lens (\DescribeRepositories' {repositoryNames} -> repositoryNames) (\s@DescribeRepositories' {} a -> s {repositoryNames = a} :: DescribeRepositories) Prelude.. Lens.mapping Lens._Coerce

-- | The AWS account ID associated with the registry that contains the
-- repositories to be described. If you do not specify a registry, the
-- default registry is assumed.
describeRepositories_registryId :: Lens.Lens' DescribeRepositories (Prelude.Maybe Prelude.Text)
describeRepositories_registryId = Lens.lens (\DescribeRepositories' {registryId} -> registryId) (\s@DescribeRepositories' {} a -> s {registryId = a} :: DescribeRepositories)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRepositoriesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "repositories" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRepositories

instance Prelude.NFData DescribeRepositories

instance Core.ToHeaders DescribeRepositories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.DescribeRepositories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeRepositories where
  toJSON DescribeRepositories' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("repositoryNames" Core..=)
              Prelude.<$> repositoryNames,
            ("registryId" Core..=) Prelude.<$> registryId
          ]
      )

instance Core.ToPath DescribeRepositories where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeRepositories where
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
describeRepositoriesResponse_repositories = Lens.lens (\DescribeRepositoriesResponse' {repositories} -> repositories) (\s@DescribeRepositoriesResponse' {} a -> s {repositories = a} :: DescribeRepositoriesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeRepositoriesResponse_httpStatus :: Lens.Lens' DescribeRepositoriesResponse Prelude.Int
describeRepositoriesResponse_httpStatus = Lens.lens (\DescribeRepositoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeRepositoriesResponse' {} a -> s {httpStatus = a} :: DescribeRepositoriesResponse)

instance Prelude.NFData DescribeRepositoriesResponse
