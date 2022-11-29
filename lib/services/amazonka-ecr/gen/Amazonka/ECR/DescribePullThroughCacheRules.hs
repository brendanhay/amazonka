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
-- Module      : Amazonka.ECR.DescribePullThroughCacheRules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the pull through cache rules for a registry.
--
-- This operation returns paginated results.
module Amazonka.ECR.DescribePullThroughCacheRules
  ( -- * Creating a Request
    DescribePullThroughCacheRules (..),
    newDescribePullThroughCacheRules,

    -- * Request Lenses
    describePullThroughCacheRules_nextToken,
    describePullThroughCacheRules_ecrRepositoryPrefixes,
    describePullThroughCacheRules_maxResults,
    describePullThroughCacheRules_registryId,

    -- * Destructuring the Response
    DescribePullThroughCacheRulesResponse (..),
    newDescribePullThroughCacheRulesResponse,

    -- * Response Lenses
    describePullThroughCacheRulesResponse_nextToken,
    describePullThroughCacheRulesResponse_pullThroughCacheRules,
    describePullThroughCacheRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePullThroughCacheRules' smart constructor.
data DescribePullThroughCacheRules = DescribePullThroughCacheRules'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @DescribePullThroughCacheRulesRequest@ request where @maxResults@ was
    -- used and the results exceeded the value of that parameter. Pagination
    -- continues from the end of the previous results that returned the
    -- @nextToken@ value. This value is null when there are no more results to
    -- return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon ECR repository prefixes associated with the pull through
    -- cache rules to return. If no repository prefix value is specified, all
    -- pull through cache rules are returned.
    ecrRepositoryPrefixes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of pull through cache rules returned by
    -- @DescribePullThroughCacheRulesRequest@ in paginated output. When this
    -- parameter is used, @DescribePullThroughCacheRulesRequest@ only returns
    -- @maxResults@ results in a single page along with a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @DescribePullThroughCacheRulesRequest@ request with the
    -- returned @nextToken@ value. This value can be between 1 and 1000. If
    -- this parameter is not used, then @DescribePullThroughCacheRulesRequest@
    -- returns up to 100 results and a @nextToken@ value, if applicable.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Web Services account ID associated with the registry to
    -- return the pull through cache rules for. If you do not specify a
    -- registry, the default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePullThroughCacheRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePullThroughCacheRules_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribePullThroughCacheRulesRequest@ request where @maxResults@ was
-- used and the results exceeded the value of that parameter. Pagination
-- continues from the end of the previous results that returned the
-- @nextToken@ value. This value is null when there are no more results to
-- return.
--
-- 'ecrRepositoryPrefixes', 'describePullThroughCacheRules_ecrRepositoryPrefixes' - The Amazon ECR repository prefixes associated with the pull through
-- cache rules to return. If no repository prefix value is specified, all
-- pull through cache rules are returned.
--
-- 'maxResults', 'describePullThroughCacheRules_maxResults' - The maximum number of pull through cache rules returned by
-- @DescribePullThroughCacheRulesRequest@ in paginated output. When this
-- parameter is used, @DescribePullThroughCacheRulesRequest@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @DescribePullThroughCacheRulesRequest@ request with the
-- returned @nextToken@ value. This value can be between 1 and 1000. If
-- this parameter is not used, then @DescribePullThroughCacheRulesRequest@
-- returns up to 100 results and a @nextToken@ value, if applicable.
--
-- 'registryId', 'describePullThroughCacheRules_registryId' - The Amazon Web Services account ID associated with the registry to
-- return the pull through cache rules for. If you do not specify a
-- registry, the default registry is assumed.
newDescribePullThroughCacheRules ::
  DescribePullThroughCacheRules
newDescribePullThroughCacheRules =
  DescribePullThroughCacheRules'
    { nextToken =
        Prelude.Nothing,
      ecrRepositoryPrefixes = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      registryId = Prelude.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribePullThroughCacheRulesRequest@ request where @maxResults@ was
-- used and the results exceeded the value of that parameter. Pagination
-- continues from the end of the previous results that returned the
-- @nextToken@ value. This value is null when there are no more results to
-- return.
describePullThroughCacheRules_nextToken :: Lens.Lens' DescribePullThroughCacheRules (Prelude.Maybe Prelude.Text)
describePullThroughCacheRules_nextToken = Lens.lens (\DescribePullThroughCacheRules' {nextToken} -> nextToken) (\s@DescribePullThroughCacheRules' {} a -> s {nextToken = a} :: DescribePullThroughCacheRules)

-- | The Amazon ECR repository prefixes associated with the pull through
-- cache rules to return. If no repository prefix value is specified, all
-- pull through cache rules are returned.
describePullThroughCacheRules_ecrRepositoryPrefixes :: Lens.Lens' DescribePullThroughCacheRules (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describePullThroughCacheRules_ecrRepositoryPrefixes = Lens.lens (\DescribePullThroughCacheRules' {ecrRepositoryPrefixes} -> ecrRepositoryPrefixes) (\s@DescribePullThroughCacheRules' {} a -> s {ecrRepositoryPrefixes = a} :: DescribePullThroughCacheRules) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of pull through cache rules returned by
-- @DescribePullThroughCacheRulesRequest@ in paginated output. When this
-- parameter is used, @DescribePullThroughCacheRulesRequest@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @DescribePullThroughCacheRulesRequest@ request with the
-- returned @nextToken@ value. This value can be between 1 and 1000. If
-- this parameter is not used, then @DescribePullThroughCacheRulesRequest@
-- returns up to 100 results and a @nextToken@ value, if applicable.
describePullThroughCacheRules_maxResults :: Lens.Lens' DescribePullThroughCacheRules (Prelude.Maybe Prelude.Natural)
describePullThroughCacheRules_maxResults = Lens.lens (\DescribePullThroughCacheRules' {maxResults} -> maxResults) (\s@DescribePullThroughCacheRules' {} a -> s {maxResults = a} :: DescribePullThroughCacheRules)

-- | The Amazon Web Services account ID associated with the registry to
-- return the pull through cache rules for. If you do not specify a
-- registry, the default registry is assumed.
describePullThroughCacheRules_registryId :: Lens.Lens' DescribePullThroughCacheRules (Prelude.Maybe Prelude.Text)
describePullThroughCacheRules_registryId = Lens.lens (\DescribePullThroughCacheRules' {registryId} -> registryId) (\s@DescribePullThroughCacheRules' {} a -> s {registryId = a} :: DescribePullThroughCacheRules)

instance Core.AWSPager DescribePullThroughCacheRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePullThroughCacheRulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describePullThroughCacheRulesResponse_pullThroughCacheRules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describePullThroughCacheRules_nextToken
          Lens..~ rs
          Lens.^? describePullThroughCacheRulesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribePullThroughCacheRules
  where
  type
    AWSResponse DescribePullThroughCacheRules =
      DescribePullThroughCacheRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePullThroughCacheRulesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "pullThroughCacheRules"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribePullThroughCacheRules
  where
  hashWithSalt _salt DescribePullThroughCacheRules' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` ecrRepositoryPrefixes
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` registryId

instance Prelude.NFData DescribePullThroughCacheRules where
  rnf DescribePullThroughCacheRules' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ecrRepositoryPrefixes
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf registryId

instance Core.ToHeaders DescribePullThroughCacheRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.DescribePullThroughCacheRules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribePullThroughCacheRules where
  toJSON DescribePullThroughCacheRules' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("ecrRepositoryPrefixes" Core..=)
              Prelude.<$> ecrRepositoryPrefixes,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("registryId" Core..=) Prelude.<$> registryId
          ]
      )

instance Core.ToPath DescribePullThroughCacheRules where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribePullThroughCacheRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePullThroughCacheRulesResponse' smart constructor.
data DescribePullThroughCacheRulesResponse = DescribePullThroughCacheRulesResponse'
  { -- | The @nextToken@ value to include in a future
    -- @DescribePullThroughCacheRulesRequest@ request. When the results of a
    -- @DescribePullThroughCacheRulesRequest@ request exceed @maxResults@, this
    -- value can be used to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The details of the pull through cache rules.
    pullThroughCacheRules :: Prelude.Maybe [PullThroughCacheRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePullThroughCacheRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePullThroughCacheRulesResponse_nextToken' - The @nextToken@ value to include in a future
-- @DescribePullThroughCacheRulesRequest@ request. When the results of a
-- @DescribePullThroughCacheRulesRequest@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'pullThroughCacheRules', 'describePullThroughCacheRulesResponse_pullThroughCacheRules' - The details of the pull through cache rules.
--
-- 'httpStatus', 'describePullThroughCacheRulesResponse_httpStatus' - The response's http status code.
newDescribePullThroughCacheRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePullThroughCacheRulesResponse
newDescribePullThroughCacheRulesResponse pHttpStatus_ =
  DescribePullThroughCacheRulesResponse'
    { nextToken =
        Prelude.Nothing,
      pullThroughCacheRules =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future
-- @DescribePullThroughCacheRulesRequest@ request. When the results of a
-- @DescribePullThroughCacheRulesRequest@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- null when there are no more results to return.
describePullThroughCacheRulesResponse_nextToken :: Lens.Lens' DescribePullThroughCacheRulesResponse (Prelude.Maybe Prelude.Text)
describePullThroughCacheRulesResponse_nextToken = Lens.lens (\DescribePullThroughCacheRulesResponse' {nextToken} -> nextToken) (\s@DescribePullThroughCacheRulesResponse' {} a -> s {nextToken = a} :: DescribePullThroughCacheRulesResponse)

-- | The details of the pull through cache rules.
describePullThroughCacheRulesResponse_pullThroughCacheRules :: Lens.Lens' DescribePullThroughCacheRulesResponse (Prelude.Maybe [PullThroughCacheRule])
describePullThroughCacheRulesResponse_pullThroughCacheRules = Lens.lens (\DescribePullThroughCacheRulesResponse' {pullThroughCacheRules} -> pullThroughCacheRules) (\s@DescribePullThroughCacheRulesResponse' {} a -> s {pullThroughCacheRules = a} :: DescribePullThroughCacheRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePullThroughCacheRulesResponse_httpStatus :: Lens.Lens' DescribePullThroughCacheRulesResponse Prelude.Int
describePullThroughCacheRulesResponse_httpStatus = Lens.lens (\DescribePullThroughCacheRulesResponse' {httpStatus} -> httpStatus) (\s@DescribePullThroughCacheRulesResponse' {} a -> s {httpStatus = a} :: DescribePullThroughCacheRulesResponse)

instance
  Prelude.NFData
    DescribePullThroughCacheRulesResponse
  where
  rnf DescribePullThroughCacheRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pullThroughCacheRules
      `Prelude.seq` Prelude.rnf httpStatus
