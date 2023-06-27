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
-- Module      : Amazonka.Route53Resolver.ListResolverRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Resolver rules that were created using the current Amazon Web
-- Services account.
--
-- This operation returns paginated results.
module Amazonka.Route53Resolver.ListResolverRules
  ( -- * Creating a Request
    ListResolverRules (..),
    newListResolverRules,

    -- * Request Lenses
    listResolverRules_filters,
    listResolverRules_maxResults,
    listResolverRules_nextToken,

    -- * Destructuring the Response
    ListResolverRulesResponse (..),
    newListResolverRulesResponse,

    -- * Response Lenses
    listResolverRulesResponse_maxResults,
    listResolverRulesResponse_nextToken,
    listResolverRulesResponse_resolverRules,
    listResolverRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newListResolverRules' smart constructor.
data ListResolverRules = ListResolverRules'
  { -- | An optional specification to return a subset of Resolver rules, such as
    -- all Resolver rules that are associated with the same Resolver endpoint.
    --
    -- If you submit a second or subsequent @ListResolverRules@ request and
    -- specify the @NextToken@ parameter, you must use the same values for
    -- @Filters@, if any, as in the previous request.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of Resolver rules that you want to return in the
    -- response to a @ListResolverRules@ request. If you don\'t specify a value
    -- for @MaxResults@, Resolver returns up to 100 Resolver rules.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For the first @ListResolverRules@ request, omit this value.
    --
    -- If you have more than @MaxResults@ Resolver rules, you can submit
    -- another @ListResolverRules@ request to get the next group of Resolver
    -- rules. In the next request, specify the value of @NextToken@ from the
    -- previous response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolverRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listResolverRules_filters' - An optional specification to return a subset of Resolver rules, such as
-- all Resolver rules that are associated with the same Resolver endpoint.
--
-- If you submit a second or subsequent @ListResolverRules@ request and
-- specify the @NextToken@ parameter, you must use the same values for
-- @Filters@, if any, as in the previous request.
--
-- 'maxResults', 'listResolverRules_maxResults' - The maximum number of Resolver rules that you want to return in the
-- response to a @ListResolverRules@ request. If you don\'t specify a value
-- for @MaxResults@, Resolver returns up to 100 Resolver rules.
--
-- 'nextToken', 'listResolverRules_nextToken' - For the first @ListResolverRules@ request, omit this value.
--
-- If you have more than @MaxResults@ Resolver rules, you can submit
-- another @ListResolverRules@ request to get the next group of Resolver
-- rules. In the next request, specify the value of @NextToken@ from the
-- previous response.
newListResolverRules ::
  ListResolverRules
newListResolverRules =
  ListResolverRules'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An optional specification to return a subset of Resolver rules, such as
-- all Resolver rules that are associated with the same Resolver endpoint.
--
-- If you submit a second or subsequent @ListResolverRules@ request and
-- specify the @NextToken@ parameter, you must use the same values for
-- @Filters@, if any, as in the previous request.
listResolverRules_filters :: Lens.Lens' ListResolverRules (Prelude.Maybe [Filter])
listResolverRules_filters = Lens.lens (\ListResolverRules' {filters} -> filters) (\s@ListResolverRules' {} a -> s {filters = a} :: ListResolverRules) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of Resolver rules that you want to return in the
-- response to a @ListResolverRules@ request. If you don\'t specify a value
-- for @MaxResults@, Resolver returns up to 100 Resolver rules.
listResolverRules_maxResults :: Lens.Lens' ListResolverRules (Prelude.Maybe Prelude.Natural)
listResolverRules_maxResults = Lens.lens (\ListResolverRules' {maxResults} -> maxResults) (\s@ListResolverRules' {} a -> s {maxResults = a} :: ListResolverRules)

-- | For the first @ListResolverRules@ request, omit this value.
--
-- If you have more than @MaxResults@ Resolver rules, you can submit
-- another @ListResolverRules@ request to get the next group of Resolver
-- rules. In the next request, specify the value of @NextToken@ from the
-- previous response.
listResolverRules_nextToken :: Lens.Lens' ListResolverRules (Prelude.Maybe Prelude.Text)
listResolverRules_nextToken = Lens.lens (\ListResolverRules' {nextToken} -> nextToken) (\s@ListResolverRules' {} a -> s {nextToken = a} :: ListResolverRules)

instance Core.AWSPager ListResolverRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResolverRulesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResolverRulesResponse_resolverRules
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listResolverRules_nextToken
          Lens..~ rs
          Lens.^? listResolverRulesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListResolverRules where
  type
    AWSResponse ListResolverRules =
      ListResolverRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResolverRulesResponse'
            Prelude.<$> (x Data..?> "MaxResults")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ResolverRules" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResolverRules where
  hashWithSalt _salt ListResolverRules' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListResolverRules where
  rnf ListResolverRules' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListResolverRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.ListResolverRules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResolverRules where
  toJSON ListResolverRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListResolverRules where
  toPath = Prelude.const "/"

instance Data.ToQuery ListResolverRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResolverRulesResponse' smart constructor.
data ListResolverRulesResponse = ListResolverRulesResponse'
  { -- | The value that you specified for @MaxResults@ in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If more than @MaxResults@ Resolver rules match the specified criteria,
    -- you can submit another @ListResolverRules@ request to get the next group
    -- of results. In the next request, specify the value of @NextToken@ from
    -- the previous response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Resolver rules that were created using the current Amazon Web
    -- Services account and that match the specified filters, if any.
    resolverRules :: Prelude.Maybe [ResolverRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolverRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listResolverRulesResponse_maxResults' - The value that you specified for @MaxResults@ in the request.
--
-- 'nextToken', 'listResolverRulesResponse_nextToken' - If more than @MaxResults@ Resolver rules match the specified criteria,
-- you can submit another @ListResolverRules@ request to get the next group
-- of results. In the next request, specify the value of @NextToken@ from
-- the previous response.
--
-- 'resolverRules', 'listResolverRulesResponse_resolverRules' - The Resolver rules that were created using the current Amazon Web
-- Services account and that match the specified filters, if any.
--
-- 'httpStatus', 'listResolverRulesResponse_httpStatus' - The response's http status code.
newListResolverRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResolverRulesResponse
newListResolverRulesResponse pHttpStatus_ =
  ListResolverRulesResponse'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resolverRules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The value that you specified for @MaxResults@ in the request.
listResolverRulesResponse_maxResults :: Lens.Lens' ListResolverRulesResponse (Prelude.Maybe Prelude.Natural)
listResolverRulesResponse_maxResults = Lens.lens (\ListResolverRulesResponse' {maxResults} -> maxResults) (\s@ListResolverRulesResponse' {} a -> s {maxResults = a} :: ListResolverRulesResponse)

-- | If more than @MaxResults@ Resolver rules match the specified criteria,
-- you can submit another @ListResolverRules@ request to get the next group
-- of results. In the next request, specify the value of @NextToken@ from
-- the previous response.
listResolverRulesResponse_nextToken :: Lens.Lens' ListResolverRulesResponse (Prelude.Maybe Prelude.Text)
listResolverRulesResponse_nextToken = Lens.lens (\ListResolverRulesResponse' {nextToken} -> nextToken) (\s@ListResolverRulesResponse' {} a -> s {nextToken = a} :: ListResolverRulesResponse)

-- | The Resolver rules that were created using the current Amazon Web
-- Services account and that match the specified filters, if any.
listResolverRulesResponse_resolverRules :: Lens.Lens' ListResolverRulesResponse (Prelude.Maybe [ResolverRule])
listResolverRulesResponse_resolverRules = Lens.lens (\ListResolverRulesResponse' {resolverRules} -> resolverRules) (\s@ListResolverRulesResponse' {} a -> s {resolverRules = a} :: ListResolverRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResolverRulesResponse_httpStatus :: Lens.Lens' ListResolverRulesResponse Prelude.Int
listResolverRulesResponse_httpStatus = Lens.lens (\ListResolverRulesResponse' {httpStatus} -> httpStatus) (\s@ListResolverRulesResponse' {} a -> s {httpStatus = a} :: ListResolverRulesResponse)

instance Prelude.NFData ListResolverRulesResponse where
  rnf ListResolverRulesResponse' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resolverRules
      `Prelude.seq` Prelude.rnf httpStatus
