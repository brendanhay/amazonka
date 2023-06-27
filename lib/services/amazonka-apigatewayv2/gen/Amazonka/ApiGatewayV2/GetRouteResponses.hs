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
-- Module      : Amazonka.ApiGatewayV2.GetRouteResponses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the RouteResponses for a Route.
--
-- This operation returns paginated results.
module Amazonka.ApiGatewayV2.GetRouteResponses
  ( -- * Creating a Request
    GetRouteResponses (..),
    newGetRouteResponses,

    -- * Request Lenses
    getRouteResponses_maxResults,
    getRouteResponses_nextToken,
    getRouteResponses_routeId,
    getRouteResponses_apiId,

    -- * Destructuring the Response
    GetRouteResponsesResponse (..),
    newGetRouteResponsesResponse,

    -- * Response Lenses
    getRouteResponsesResponse_items,
    getRouteResponsesResponse_nextToken,
    getRouteResponsesResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRouteResponses' smart constructor.
data GetRouteResponses = GetRouteResponses'
  { -- | The maximum number of elements to be returned for this resource.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The route ID.
    routeId :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRouteResponses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getRouteResponses_maxResults' - The maximum number of elements to be returned for this resource.
--
-- 'nextToken', 'getRouteResponses_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'routeId', 'getRouteResponses_routeId' - The route ID.
--
-- 'apiId', 'getRouteResponses_apiId' - The API identifier.
newGetRouteResponses ::
  -- | 'routeId'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  GetRouteResponses
newGetRouteResponses pRouteId_ pApiId_ =
  GetRouteResponses'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      routeId = pRouteId_,
      apiId = pApiId_
    }

-- | The maximum number of elements to be returned for this resource.
getRouteResponses_maxResults :: Lens.Lens' GetRouteResponses (Prelude.Maybe Prelude.Text)
getRouteResponses_maxResults = Lens.lens (\GetRouteResponses' {maxResults} -> maxResults) (\s@GetRouteResponses' {} a -> s {maxResults = a} :: GetRouteResponses)

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getRouteResponses_nextToken :: Lens.Lens' GetRouteResponses (Prelude.Maybe Prelude.Text)
getRouteResponses_nextToken = Lens.lens (\GetRouteResponses' {nextToken} -> nextToken) (\s@GetRouteResponses' {} a -> s {nextToken = a} :: GetRouteResponses)

-- | The route ID.
getRouteResponses_routeId :: Lens.Lens' GetRouteResponses Prelude.Text
getRouteResponses_routeId = Lens.lens (\GetRouteResponses' {routeId} -> routeId) (\s@GetRouteResponses' {} a -> s {routeId = a} :: GetRouteResponses)

-- | The API identifier.
getRouteResponses_apiId :: Lens.Lens' GetRouteResponses Prelude.Text
getRouteResponses_apiId = Lens.lens (\GetRouteResponses' {apiId} -> apiId) (\s@GetRouteResponses' {} a -> s {apiId = a} :: GetRouteResponses)

instance Core.AWSPager GetRouteResponses where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRouteResponsesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getRouteResponsesResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getRouteResponses_nextToken
          Lens..~ rs
          Lens.^? getRouteResponsesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetRouteResponses where
  type
    AWSResponse GetRouteResponses =
      GetRouteResponsesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRouteResponsesResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRouteResponses where
  hashWithSalt _salt GetRouteResponses' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` routeId
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData GetRouteResponses where
  rnf GetRouteResponses' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf routeId
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders GetRouteResponses where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRouteResponses where
  toPath GetRouteResponses' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/routes/",
        Data.toBS routeId,
        "/routeresponses"
      ]

instance Data.ToQuery GetRouteResponses where
  toQuery GetRouteResponses' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetRouteResponsesResponse' smart constructor.
data GetRouteResponsesResponse = GetRouteResponsesResponse'
  { -- | The elements from this collection.
    items :: Prelude.Maybe [RouteResponse],
    -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRouteResponsesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getRouteResponsesResponse_items' - The elements from this collection.
--
-- 'nextToken', 'getRouteResponsesResponse_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'httpStatus', 'getRouteResponsesResponse_httpStatus' - The response's http status code.
newGetRouteResponsesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRouteResponsesResponse
newGetRouteResponsesResponse pHttpStatus_ =
  GetRouteResponsesResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The elements from this collection.
getRouteResponsesResponse_items :: Lens.Lens' GetRouteResponsesResponse (Prelude.Maybe [RouteResponse])
getRouteResponsesResponse_items = Lens.lens (\GetRouteResponsesResponse' {items} -> items) (\s@GetRouteResponsesResponse' {} a -> s {items = a} :: GetRouteResponsesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getRouteResponsesResponse_nextToken :: Lens.Lens' GetRouteResponsesResponse (Prelude.Maybe Prelude.Text)
getRouteResponsesResponse_nextToken = Lens.lens (\GetRouteResponsesResponse' {nextToken} -> nextToken) (\s@GetRouteResponsesResponse' {} a -> s {nextToken = a} :: GetRouteResponsesResponse)

-- | The response's http status code.
getRouteResponsesResponse_httpStatus :: Lens.Lens' GetRouteResponsesResponse Prelude.Int
getRouteResponsesResponse_httpStatus = Lens.lens (\GetRouteResponsesResponse' {httpStatus} -> httpStatus) (\s@GetRouteResponsesResponse' {} a -> s {httpStatus = a} :: GetRouteResponsesResponse)

instance Prelude.NFData GetRouteResponsesResponse where
  rnf GetRouteResponsesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
