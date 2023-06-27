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
-- Module      : Amazonka.ApiGatewayV2.GetRoutes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Routes for an API.
--
-- This operation returns paginated results.
module Amazonka.ApiGatewayV2.GetRoutes
  ( -- * Creating a Request
    GetRoutes (..),
    newGetRoutes,

    -- * Request Lenses
    getRoutes_maxResults,
    getRoutes_nextToken,
    getRoutes_apiId,

    -- * Destructuring the Response
    GetRoutesResponse (..),
    newGetRoutesResponse,

    -- * Response Lenses
    getRoutesResponse_items,
    getRoutesResponse_nextToken,
    getRoutesResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRoutes' smart constructor.
data GetRoutes = GetRoutes'
  { -- | The maximum number of elements to be returned for this resource.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getRoutes_maxResults' - The maximum number of elements to be returned for this resource.
--
-- 'nextToken', 'getRoutes_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'apiId', 'getRoutes_apiId' - The API identifier.
newGetRoutes ::
  -- | 'apiId'
  Prelude.Text ->
  GetRoutes
newGetRoutes pApiId_ =
  GetRoutes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      apiId = pApiId_
    }

-- | The maximum number of elements to be returned for this resource.
getRoutes_maxResults :: Lens.Lens' GetRoutes (Prelude.Maybe Prelude.Text)
getRoutes_maxResults = Lens.lens (\GetRoutes' {maxResults} -> maxResults) (\s@GetRoutes' {} a -> s {maxResults = a} :: GetRoutes)

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getRoutes_nextToken :: Lens.Lens' GetRoutes (Prelude.Maybe Prelude.Text)
getRoutes_nextToken = Lens.lens (\GetRoutes' {nextToken} -> nextToken) (\s@GetRoutes' {} a -> s {nextToken = a} :: GetRoutes)

-- | The API identifier.
getRoutes_apiId :: Lens.Lens' GetRoutes Prelude.Text
getRoutes_apiId = Lens.lens (\GetRoutes' {apiId} -> apiId) (\s@GetRoutes' {} a -> s {apiId = a} :: GetRoutes)

instance Core.AWSPager GetRoutes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRoutesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getRoutesResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getRoutes_nextToken
          Lens..~ rs
          Lens.^? getRoutesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetRoutes where
  type AWSResponse GetRoutes = GetRoutesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRoutesResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRoutes where
  hashWithSalt _salt GetRoutes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData GetRoutes where
  rnf GetRoutes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders GetRoutes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRoutes where
  toPath GetRoutes' {..} =
    Prelude.mconcat
      ["/v2/apis/", Data.toBS apiId, "/routes"]

instance Data.ToQuery GetRoutes where
  toQuery GetRoutes' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetRoutesResponse' smart constructor.
data GetRoutesResponse = GetRoutesResponse'
  { -- | The elements from this collection.
    items :: Prelude.Maybe [Route],
    -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getRoutesResponse_items' - The elements from this collection.
--
-- 'nextToken', 'getRoutesResponse_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'httpStatus', 'getRoutesResponse_httpStatus' - The response's http status code.
newGetRoutesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRoutesResponse
newGetRoutesResponse pHttpStatus_ =
  GetRoutesResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The elements from this collection.
getRoutesResponse_items :: Lens.Lens' GetRoutesResponse (Prelude.Maybe [Route])
getRoutesResponse_items = Lens.lens (\GetRoutesResponse' {items} -> items) (\s@GetRoutesResponse' {} a -> s {items = a} :: GetRoutesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getRoutesResponse_nextToken :: Lens.Lens' GetRoutesResponse (Prelude.Maybe Prelude.Text)
getRoutesResponse_nextToken = Lens.lens (\GetRoutesResponse' {nextToken} -> nextToken) (\s@GetRoutesResponse' {} a -> s {nextToken = a} :: GetRoutesResponse)

-- | The response's http status code.
getRoutesResponse_httpStatus :: Lens.Lens' GetRoutesResponse Prelude.Int
getRoutesResponse_httpStatus = Lens.lens (\GetRoutesResponse' {httpStatus} -> httpStatus) (\s@GetRoutesResponse' {} a -> s {httpStatus = a} :: GetRoutesResponse)

instance Prelude.NFData GetRoutesResponse where
  rnf GetRoutesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
