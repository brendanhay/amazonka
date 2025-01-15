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
-- Module      : Amazonka.Route53Resolver.ListResolverDnssecConfigs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the configurations for DNSSEC validation that are associated with
-- the current Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.Route53Resolver.ListResolverDnssecConfigs
  ( -- * Creating a Request
    ListResolverDnssecConfigs (..),
    newListResolverDnssecConfigs,

    -- * Request Lenses
    listResolverDnssecConfigs_filters,
    listResolverDnssecConfigs_maxResults,
    listResolverDnssecConfigs_nextToken,

    -- * Destructuring the Response
    ListResolverDnssecConfigsResponse (..),
    newListResolverDnssecConfigsResponse,

    -- * Response Lenses
    listResolverDnssecConfigsResponse_nextToken,
    listResolverDnssecConfigsResponse_resolverDnssecConfigs,
    listResolverDnssecConfigsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newListResolverDnssecConfigs' smart constructor.
data ListResolverDnssecConfigs = ListResolverDnssecConfigs'
  { -- | An optional specification to return a subset of objects.
    filters :: Prelude.Maybe [Filter],
    -- | /Optional/: An integer that specifies the maximum number of DNSSEC
    -- configuration results that you want Amazon Route 53 to return. If you
    -- don\'t specify a value for @MaxResults@, Route 53 returns up to 100
    -- configuration per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | (Optional) If the current Amazon Web Services account has more than
    -- @MaxResults@ DNSSEC configurations, use @NextToken@ to get the second
    -- and subsequent pages of results.
    --
    -- For the first @ListResolverDnssecConfigs@ request, omit this value.
    --
    -- For the second and subsequent requests, get the value of @NextToken@
    -- from the previous response and specify that value for @NextToken@ in the
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolverDnssecConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listResolverDnssecConfigs_filters' - An optional specification to return a subset of objects.
--
-- 'maxResults', 'listResolverDnssecConfigs_maxResults' - /Optional/: An integer that specifies the maximum number of DNSSEC
-- configuration results that you want Amazon Route 53 to return. If you
-- don\'t specify a value for @MaxResults@, Route 53 returns up to 100
-- configuration per page.
--
-- 'nextToken', 'listResolverDnssecConfigs_nextToken' - (Optional) If the current Amazon Web Services account has more than
-- @MaxResults@ DNSSEC configurations, use @NextToken@ to get the second
-- and subsequent pages of results.
--
-- For the first @ListResolverDnssecConfigs@ request, omit this value.
--
-- For the second and subsequent requests, get the value of @NextToken@
-- from the previous response and specify that value for @NextToken@ in the
-- request.
newListResolverDnssecConfigs ::
  ListResolverDnssecConfigs
newListResolverDnssecConfigs =
  ListResolverDnssecConfigs'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An optional specification to return a subset of objects.
listResolverDnssecConfigs_filters :: Lens.Lens' ListResolverDnssecConfigs (Prelude.Maybe [Filter])
listResolverDnssecConfigs_filters = Lens.lens (\ListResolverDnssecConfigs' {filters} -> filters) (\s@ListResolverDnssecConfigs' {} a -> s {filters = a} :: ListResolverDnssecConfigs) Prelude.. Lens.mapping Lens.coerced

-- | /Optional/: An integer that specifies the maximum number of DNSSEC
-- configuration results that you want Amazon Route 53 to return. If you
-- don\'t specify a value for @MaxResults@, Route 53 returns up to 100
-- configuration per page.
listResolverDnssecConfigs_maxResults :: Lens.Lens' ListResolverDnssecConfigs (Prelude.Maybe Prelude.Natural)
listResolverDnssecConfigs_maxResults = Lens.lens (\ListResolverDnssecConfigs' {maxResults} -> maxResults) (\s@ListResolverDnssecConfigs' {} a -> s {maxResults = a} :: ListResolverDnssecConfigs)

-- | (Optional) If the current Amazon Web Services account has more than
-- @MaxResults@ DNSSEC configurations, use @NextToken@ to get the second
-- and subsequent pages of results.
--
-- For the first @ListResolverDnssecConfigs@ request, omit this value.
--
-- For the second and subsequent requests, get the value of @NextToken@
-- from the previous response and specify that value for @NextToken@ in the
-- request.
listResolverDnssecConfigs_nextToken :: Lens.Lens' ListResolverDnssecConfigs (Prelude.Maybe Prelude.Text)
listResolverDnssecConfigs_nextToken = Lens.lens (\ListResolverDnssecConfigs' {nextToken} -> nextToken) (\s@ListResolverDnssecConfigs' {} a -> s {nextToken = a} :: ListResolverDnssecConfigs)

instance Core.AWSPager ListResolverDnssecConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResolverDnssecConfigsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResolverDnssecConfigsResponse_resolverDnssecConfigs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listResolverDnssecConfigs_nextToken
              Lens..~ rs
              Lens.^? listResolverDnssecConfigsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListResolverDnssecConfigs where
  type
    AWSResponse ListResolverDnssecConfigs =
      ListResolverDnssecConfigsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResolverDnssecConfigsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ResolverDnssecConfigs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResolverDnssecConfigs where
  hashWithSalt _salt ListResolverDnssecConfigs' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListResolverDnssecConfigs where
  rnf ListResolverDnssecConfigs' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListResolverDnssecConfigs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.ListResolverDnssecConfigs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResolverDnssecConfigs where
  toJSON ListResolverDnssecConfigs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListResolverDnssecConfigs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListResolverDnssecConfigs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResolverDnssecConfigsResponse' smart constructor.
data ListResolverDnssecConfigsResponse = ListResolverDnssecConfigsResponse'
  { -- | If a response includes the last of the DNSSEC configurations that are
    -- associated with the current Amazon Web Services account, @NextToken@
    -- doesn\'t appear in the response.
    --
    -- If a response doesn\'t include the last of the configurations, you can
    -- get more configurations by submitting another
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListResolverDnssecConfigs.html ListResolverDnssecConfigs>
    -- request. Get the value of @NextToken@ that Amazon Route 53 returned in
    -- the previous response and include it in @NextToken@ in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array that contains one
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ResolverDnssecConfig.html ResolverDnssecConfig>
    -- element for each configuration for DNSSEC validation that is associated
    -- with the current Amazon Web Services account.
    resolverDnssecConfigs :: Prelude.Maybe [ResolverDnssecConfig],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolverDnssecConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResolverDnssecConfigsResponse_nextToken' - If a response includes the last of the DNSSEC configurations that are
-- associated with the current Amazon Web Services account, @NextToken@
-- doesn\'t appear in the response.
--
-- If a response doesn\'t include the last of the configurations, you can
-- get more configurations by submitting another
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListResolverDnssecConfigs.html ListResolverDnssecConfigs>
-- request. Get the value of @NextToken@ that Amazon Route 53 returned in
-- the previous response and include it in @NextToken@ in the next request.
--
-- 'resolverDnssecConfigs', 'listResolverDnssecConfigsResponse_resolverDnssecConfigs' - An array that contains one
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ResolverDnssecConfig.html ResolverDnssecConfig>
-- element for each configuration for DNSSEC validation that is associated
-- with the current Amazon Web Services account.
--
-- 'httpStatus', 'listResolverDnssecConfigsResponse_httpStatus' - The response's http status code.
newListResolverDnssecConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResolverDnssecConfigsResponse
newListResolverDnssecConfigsResponse pHttpStatus_ =
  ListResolverDnssecConfigsResponse'
    { nextToken =
        Prelude.Nothing,
      resolverDnssecConfigs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a response includes the last of the DNSSEC configurations that are
-- associated with the current Amazon Web Services account, @NextToken@
-- doesn\'t appear in the response.
--
-- If a response doesn\'t include the last of the configurations, you can
-- get more configurations by submitting another
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListResolverDnssecConfigs.html ListResolverDnssecConfigs>
-- request. Get the value of @NextToken@ that Amazon Route 53 returned in
-- the previous response and include it in @NextToken@ in the next request.
listResolverDnssecConfigsResponse_nextToken :: Lens.Lens' ListResolverDnssecConfigsResponse (Prelude.Maybe Prelude.Text)
listResolverDnssecConfigsResponse_nextToken = Lens.lens (\ListResolverDnssecConfigsResponse' {nextToken} -> nextToken) (\s@ListResolverDnssecConfigsResponse' {} a -> s {nextToken = a} :: ListResolverDnssecConfigsResponse)

-- | An array that contains one
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ResolverDnssecConfig.html ResolverDnssecConfig>
-- element for each configuration for DNSSEC validation that is associated
-- with the current Amazon Web Services account.
listResolverDnssecConfigsResponse_resolverDnssecConfigs :: Lens.Lens' ListResolverDnssecConfigsResponse (Prelude.Maybe [ResolverDnssecConfig])
listResolverDnssecConfigsResponse_resolverDnssecConfigs = Lens.lens (\ListResolverDnssecConfigsResponse' {resolverDnssecConfigs} -> resolverDnssecConfigs) (\s@ListResolverDnssecConfigsResponse' {} a -> s {resolverDnssecConfigs = a} :: ListResolverDnssecConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResolverDnssecConfigsResponse_httpStatus :: Lens.Lens' ListResolverDnssecConfigsResponse Prelude.Int
listResolverDnssecConfigsResponse_httpStatus = Lens.lens (\ListResolverDnssecConfigsResponse' {httpStatus} -> httpStatus) (\s@ListResolverDnssecConfigsResponse' {} a -> s {httpStatus = a} :: ListResolverDnssecConfigsResponse)

instance
  Prelude.NFData
    ListResolverDnssecConfigsResponse
  where
  rnf ListResolverDnssecConfigsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf resolverDnssecConfigs `Prelude.seq`
        Prelude.rnf httpStatus
