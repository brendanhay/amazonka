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
-- Module      : Amazonka.Route53Resolver.ListResolverEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the Resolver endpoints that were created using the current
-- Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.Route53Resolver.ListResolverEndpoints
  ( -- * Creating a Request
    ListResolverEndpoints (..),
    newListResolverEndpoints,

    -- * Request Lenses
    listResolverEndpoints_filters,
    listResolverEndpoints_maxResults,
    listResolverEndpoints_nextToken,

    -- * Destructuring the Response
    ListResolverEndpointsResponse (..),
    newListResolverEndpointsResponse,

    -- * Response Lenses
    listResolverEndpointsResponse_maxResults,
    listResolverEndpointsResponse_nextToken,
    listResolverEndpointsResponse_resolverEndpoints,
    listResolverEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newListResolverEndpoints' smart constructor.
data ListResolverEndpoints = ListResolverEndpoints'
  { -- | An optional specification to return a subset of Resolver endpoints, such
    -- as all inbound Resolver endpoints.
    --
    -- If you submit a second or subsequent @ListResolverEndpoints@ request and
    -- specify the @NextToken@ parameter, you must use the same values for
    -- @Filters@, if any, as in the previous request.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of Resolver endpoints that you want to return in the
    -- response to a @ListResolverEndpoints@ request. If you don\'t specify a
    -- value for @MaxResults@, Resolver returns up to 100 Resolver endpoints.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For the first @ListResolverEndpoints@ request, omit this value.
    --
    -- If you have more than @MaxResults@ Resolver endpoints, you can submit
    -- another @ListResolverEndpoints@ request to get the next group of
    -- Resolver endpoints. In the next request, specify the value of
    -- @NextToken@ from the previous response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolverEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listResolverEndpoints_filters' - An optional specification to return a subset of Resolver endpoints, such
-- as all inbound Resolver endpoints.
--
-- If you submit a second or subsequent @ListResolverEndpoints@ request and
-- specify the @NextToken@ parameter, you must use the same values for
-- @Filters@, if any, as in the previous request.
--
-- 'maxResults', 'listResolverEndpoints_maxResults' - The maximum number of Resolver endpoints that you want to return in the
-- response to a @ListResolverEndpoints@ request. If you don\'t specify a
-- value for @MaxResults@, Resolver returns up to 100 Resolver endpoints.
--
-- 'nextToken', 'listResolverEndpoints_nextToken' - For the first @ListResolverEndpoints@ request, omit this value.
--
-- If you have more than @MaxResults@ Resolver endpoints, you can submit
-- another @ListResolverEndpoints@ request to get the next group of
-- Resolver endpoints. In the next request, specify the value of
-- @NextToken@ from the previous response.
newListResolverEndpoints ::
  ListResolverEndpoints
newListResolverEndpoints =
  ListResolverEndpoints'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An optional specification to return a subset of Resolver endpoints, such
-- as all inbound Resolver endpoints.
--
-- If you submit a second or subsequent @ListResolverEndpoints@ request and
-- specify the @NextToken@ parameter, you must use the same values for
-- @Filters@, if any, as in the previous request.
listResolverEndpoints_filters :: Lens.Lens' ListResolverEndpoints (Prelude.Maybe [Filter])
listResolverEndpoints_filters = Lens.lens (\ListResolverEndpoints' {filters} -> filters) (\s@ListResolverEndpoints' {} a -> s {filters = a} :: ListResolverEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of Resolver endpoints that you want to return in the
-- response to a @ListResolverEndpoints@ request. If you don\'t specify a
-- value for @MaxResults@, Resolver returns up to 100 Resolver endpoints.
listResolverEndpoints_maxResults :: Lens.Lens' ListResolverEndpoints (Prelude.Maybe Prelude.Natural)
listResolverEndpoints_maxResults = Lens.lens (\ListResolverEndpoints' {maxResults} -> maxResults) (\s@ListResolverEndpoints' {} a -> s {maxResults = a} :: ListResolverEndpoints)

-- | For the first @ListResolverEndpoints@ request, omit this value.
--
-- If you have more than @MaxResults@ Resolver endpoints, you can submit
-- another @ListResolverEndpoints@ request to get the next group of
-- Resolver endpoints. In the next request, specify the value of
-- @NextToken@ from the previous response.
listResolverEndpoints_nextToken :: Lens.Lens' ListResolverEndpoints (Prelude.Maybe Prelude.Text)
listResolverEndpoints_nextToken = Lens.lens (\ListResolverEndpoints' {nextToken} -> nextToken) (\s@ListResolverEndpoints' {} a -> s {nextToken = a} :: ListResolverEndpoints)

instance Core.AWSPager ListResolverEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResolverEndpointsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResolverEndpointsResponse_resolverEndpoints
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listResolverEndpoints_nextToken
          Lens..~ rs
          Lens.^? listResolverEndpointsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListResolverEndpoints where
  type
    AWSResponse ListResolverEndpoints =
      ListResolverEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResolverEndpointsResponse'
            Prelude.<$> (x Data..?> "MaxResults")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ResolverEndpoints"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResolverEndpoints where
  hashWithSalt _salt ListResolverEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListResolverEndpoints where
  rnf ListResolverEndpoints' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListResolverEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.ListResolverEndpoints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResolverEndpoints where
  toJSON ListResolverEndpoints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListResolverEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery ListResolverEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResolverEndpointsResponse' smart constructor.
data ListResolverEndpointsResponse = ListResolverEndpointsResponse'
  { -- | The value that you specified for @MaxResults@ in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If more than @MaxResults@ IP addresses match the specified criteria, you
    -- can submit another @ListResolverEndpoint@ request to get the next group
    -- of results. In the next request, specify the value of @NextToken@ from
    -- the previous response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Resolver endpoints that were created by using the current Amazon Web
    -- Services account, and that match the specified filters, if any.
    resolverEndpoints :: Prelude.Maybe [ResolverEndpoint],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolverEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listResolverEndpointsResponse_maxResults' - The value that you specified for @MaxResults@ in the request.
--
-- 'nextToken', 'listResolverEndpointsResponse_nextToken' - If more than @MaxResults@ IP addresses match the specified criteria, you
-- can submit another @ListResolverEndpoint@ request to get the next group
-- of results. In the next request, specify the value of @NextToken@ from
-- the previous response.
--
-- 'resolverEndpoints', 'listResolverEndpointsResponse_resolverEndpoints' - The Resolver endpoints that were created by using the current Amazon Web
-- Services account, and that match the specified filters, if any.
--
-- 'httpStatus', 'listResolverEndpointsResponse_httpStatus' - The response's http status code.
newListResolverEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResolverEndpointsResponse
newListResolverEndpointsResponse pHttpStatus_ =
  ListResolverEndpointsResponse'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resolverEndpoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The value that you specified for @MaxResults@ in the request.
listResolverEndpointsResponse_maxResults :: Lens.Lens' ListResolverEndpointsResponse (Prelude.Maybe Prelude.Natural)
listResolverEndpointsResponse_maxResults = Lens.lens (\ListResolverEndpointsResponse' {maxResults} -> maxResults) (\s@ListResolverEndpointsResponse' {} a -> s {maxResults = a} :: ListResolverEndpointsResponse)

-- | If more than @MaxResults@ IP addresses match the specified criteria, you
-- can submit another @ListResolverEndpoint@ request to get the next group
-- of results. In the next request, specify the value of @NextToken@ from
-- the previous response.
listResolverEndpointsResponse_nextToken :: Lens.Lens' ListResolverEndpointsResponse (Prelude.Maybe Prelude.Text)
listResolverEndpointsResponse_nextToken = Lens.lens (\ListResolverEndpointsResponse' {nextToken} -> nextToken) (\s@ListResolverEndpointsResponse' {} a -> s {nextToken = a} :: ListResolverEndpointsResponse)

-- | The Resolver endpoints that were created by using the current Amazon Web
-- Services account, and that match the specified filters, if any.
listResolverEndpointsResponse_resolverEndpoints :: Lens.Lens' ListResolverEndpointsResponse (Prelude.Maybe [ResolverEndpoint])
listResolverEndpointsResponse_resolverEndpoints = Lens.lens (\ListResolverEndpointsResponse' {resolverEndpoints} -> resolverEndpoints) (\s@ListResolverEndpointsResponse' {} a -> s {resolverEndpoints = a} :: ListResolverEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResolverEndpointsResponse_httpStatus :: Lens.Lens' ListResolverEndpointsResponse Prelude.Int
listResolverEndpointsResponse_httpStatus = Lens.lens (\ListResolverEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListResolverEndpointsResponse' {} a -> s {httpStatus = a} :: ListResolverEndpointsResponse)

instance Prelude.NFData ListResolverEndpointsResponse where
  rnf ListResolverEndpointsResponse' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resolverEndpoints
      `Prelude.seq` Prelude.rnf httpStatus
