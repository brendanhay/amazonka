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
-- Module      : Amazonka.Route53Resolver.ListResolverConfigs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Resolver configurations that you have defined. Route 53
-- Resolver uses the configurations to manage DNS resolution behavior for
-- your VPCs.
--
-- This operation returns paginated results.
module Amazonka.Route53Resolver.ListResolverConfigs
  ( -- * Creating a Request
    ListResolverConfigs (..),
    newListResolverConfigs,

    -- * Request Lenses
    listResolverConfigs_nextToken,
    listResolverConfigs_maxResults,

    -- * Destructuring the Response
    ListResolverConfigsResponse (..),
    newListResolverConfigsResponse,

    -- * Response Lenses
    listResolverConfigsResponse_nextToken,
    listResolverConfigsResponse_resolverConfigs,
    listResolverConfigsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newListResolverConfigs' smart constructor.
data ListResolverConfigs = ListResolverConfigs'
  { -- | (Optional) If the current Amazon Web Services account has more than
    -- @MaxResults@ Resolver configurations, use @NextToken@ to get the second
    -- and subsequent pages of results.
    --
    -- For the first @ListResolverConfigs@ request, omit this value.
    --
    -- For the second and subsequent requests, get the value of @NextToken@
    -- from the previous response and specify that value for @NextToken@ in the
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of Resolver configurations that you want to return in
    -- the response to a @ListResolverConfigs@ request. If you don\'t specify a
    -- value for @MaxResults@, up to 100 Resolver configurations are returned.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolverConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResolverConfigs_nextToken' - (Optional) If the current Amazon Web Services account has more than
-- @MaxResults@ Resolver configurations, use @NextToken@ to get the second
-- and subsequent pages of results.
--
-- For the first @ListResolverConfigs@ request, omit this value.
--
-- For the second and subsequent requests, get the value of @NextToken@
-- from the previous response and specify that value for @NextToken@ in the
-- request.
--
-- 'maxResults', 'listResolverConfigs_maxResults' - The maximum number of Resolver configurations that you want to return in
-- the response to a @ListResolverConfigs@ request. If you don\'t specify a
-- value for @MaxResults@, up to 100 Resolver configurations are returned.
newListResolverConfigs ::
  ListResolverConfigs
newListResolverConfigs =
  ListResolverConfigs'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | (Optional) If the current Amazon Web Services account has more than
-- @MaxResults@ Resolver configurations, use @NextToken@ to get the second
-- and subsequent pages of results.
--
-- For the first @ListResolverConfigs@ request, omit this value.
--
-- For the second and subsequent requests, get the value of @NextToken@
-- from the previous response and specify that value for @NextToken@ in the
-- request.
listResolverConfigs_nextToken :: Lens.Lens' ListResolverConfigs (Prelude.Maybe Prelude.Text)
listResolverConfigs_nextToken = Lens.lens (\ListResolverConfigs' {nextToken} -> nextToken) (\s@ListResolverConfigs' {} a -> s {nextToken = a} :: ListResolverConfigs)

-- | The maximum number of Resolver configurations that you want to return in
-- the response to a @ListResolverConfigs@ request. If you don\'t specify a
-- value for @MaxResults@, up to 100 Resolver configurations are returned.
listResolverConfigs_maxResults :: Lens.Lens' ListResolverConfigs (Prelude.Maybe Prelude.Natural)
listResolverConfigs_maxResults = Lens.lens (\ListResolverConfigs' {maxResults} -> maxResults) (\s@ListResolverConfigs' {} a -> s {maxResults = a} :: ListResolverConfigs)

instance Core.AWSPager ListResolverConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResolverConfigsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResolverConfigsResponse_resolverConfigs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResolverConfigs_nextToken
          Lens..~ rs
          Lens.^? listResolverConfigsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListResolverConfigs where
  type
    AWSResponse ListResolverConfigs =
      ListResolverConfigsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResolverConfigsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ResolverConfigs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResolverConfigs where
  hashWithSalt _salt ListResolverConfigs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListResolverConfigs where
  rnf ListResolverConfigs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListResolverConfigs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Resolver.ListResolverConfigs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResolverConfigs where
  toJSON ListResolverConfigs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListResolverConfigs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListResolverConfigs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResolverConfigsResponse' smart constructor.
data ListResolverConfigsResponse = ListResolverConfigsResponse'
  { -- | If a response includes the last of the Resolver configurations that are
    -- associated with the current Amazon Web Services account, @NextToken@
    -- doesn\'t appear in the response.
    --
    -- If a response doesn\'t include the last of the configurations, you can
    -- get more configurations by submitting another @ListResolverConfigs@
    -- request. Get the value of @NextToken@ that Amazon Route 53 returned in
    -- the previous response and include it in @NextToken@ in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array that contains one @ResolverConfigs@ element for each Resolver
    -- configuration that is associated with the current Amazon Web Services
    -- account.
    resolverConfigs :: Prelude.Maybe [ResolverConfig],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolverConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResolverConfigsResponse_nextToken' - If a response includes the last of the Resolver configurations that are
-- associated with the current Amazon Web Services account, @NextToken@
-- doesn\'t appear in the response.
--
-- If a response doesn\'t include the last of the configurations, you can
-- get more configurations by submitting another @ListResolverConfigs@
-- request. Get the value of @NextToken@ that Amazon Route 53 returned in
-- the previous response and include it in @NextToken@ in the next request.
--
-- 'resolverConfigs', 'listResolverConfigsResponse_resolverConfigs' - An array that contains one @ResolverConfigs@ element for each Resolver
-- configuration that is associated with the current Amazon Web Services
-- account.
--
-- 'httpStatus', 'listResolverConfigsResponse_httpStatus' - The response's http status code.
newListResolverConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResolverConfigsResponse
newListResolverConfigsResponse pHttpStatus_ =
  ListResolverConfigsResponse'
    { nextToken =
        Prelude.Nothing,
      resolverConfigs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a response includes the last of the Resolver configurations that are
-- associated with the current Amazon Web Services account, @NextToken@
-- doesn\'t appear in the response.
--
-- If a response doesn\'t include the last of the configurations, you can
-- get more configurations by submitting another @ListResolverConfigs@
-- request. Get the value of @NextToken@ that Amazon Route 53 returned in
-- the previous response and include it in @NextToken@ in the next request.
listResolverConfigsResponse_nextToken :: Lens.Lens' ListResolverConfigsResponse (Prelude.Maybe Prelude.Text)
listResolverConfigsResponse_nextToken = Lens.lens (\ListResolverConfigsResponse' {nextToken} -> nextToken) (\s@ListResolverConfigsResponse' {} a -> s {nextToken = a} :: ListResolverConfigsResponse)

-- | An array that contains one @ResolverConfigs@ element for each Resolver
-- configuration that is associated with the current Amazon Web Services
-- account.
listResolverConfigsResponse_resolverConfigs :: Lens.Lens' ListResolverConfigsResponse (Prelude.Maybe [ResolverConfig])
listResolverConfigsResponse_resolverConfigs = Lens.lens (\ListResolverConfigsResponse' {resolverConfigs} -> resolverConfigs) (\s@ListResolverConfigsResponse' {} a -> s {resolverConfigs = a} :: ListResolverConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResolverConfigsResponse_httpStatus :: Lens.Lens' ListResolverConfigsResponse Prelude.Int
listResolverConfigsResponse_httpStatus = Lens.lens (\ListResolverConfigsResponse' {httpStatus} -> httpStatus) (\s@ListResolverConfigsResponse' {} a -> s {httpStatus = a} :: ListResolverConfigsResponse)

instance Prelude.NFData ListResolverConfigsResponse where
  rnf ListResolverConfigsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resolverConfigs
      `Prelude.seq` Prelude.rnf httpStatus
