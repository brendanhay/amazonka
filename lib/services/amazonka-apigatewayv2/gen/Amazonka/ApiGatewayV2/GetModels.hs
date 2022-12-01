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
-- Module      : Amazonka.ApiGatewayV2.GetModels
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Models for an API.
--
-- This operation returns paginated results.
module Amazonka.ApiGatewayV2.GetModels
  ( -- * Creating a Request
    GetModels (..),
    newGetModels,

    -- * Request Lenses
    getModels_nextToken,
    getModels_maxResults,
    getModels_apiId,

    -- * Destructuring the Response
    GetModelsResponse (..),
    newGetModelsResponse,

    -- * Response Lenses
    getModelsResponse_items,
    getModelsResponse_nextToken,
    getModelsResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetModels' smart constructor.
data GetModels = GetModels'
  { -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of elements to be returned for this resource.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetModels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getModels_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'maxResults', 'getModels_maxResults' - The maximum number of elements to be returned for this resource.
--
-- 'apiId', 'getModels_apiId' - The API identifier.
newGetModels ::
  -- | 'apiId'
  Prelude.Text ->
  GetModels
newGetModels pApiId_ =
  GetModels'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      apiId = pApiId_
    }

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getModels_nextToken :: Lens.Lens' GetModels (Prelude.Maybe Prelude.Text)
getModels_nextToken = Lens.lens (\GetModels' {nextToken} -> nextToken) (\s@GetModels' {} a -> s {nextToken = a} :: GetModels)

-- | The maximum number of elements to be returned for this resource.
getModels_maxResults :: Lens.Lens' GetModels (Prelude.Maybe Prelude.Text)
getModels_maxResults = Lens.lens (\GetModels' {maxResults} -> maxResults) (\s@GetModels' {} a -> s {maxResults = a} :: GetModels)

-- | The API identifier.
getModels_apiId :: Lens.Lens' GetModels Prelude.Text
getModels_apiId = Lens.lens (\GetModels' {apiId} -> apiId) (\s@GetModels' {} a -> s {apiId = a} :: GetModels)

instance Core.AWSPager GetModels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getModelsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getModelsResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getModels_nextToken
          Lens..~ rs
          Lens.^? getModelsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetModels where
  type AWSResponse GetModels = GetModelsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetModelsResponse'
            Prelude.<$> (x Core..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetModels where
  hashWithSalt _salt GetModels' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData GetModels where
  rnf GetModels' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf apiId

instance Core.ToHeaders GetModels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetModels where
  toPath GetModels' {..} =
    Prelude.mconcat
      ["/v2/apis/", Core.toBS apiId, "/models"]

instance Core.ToQuery GetModels where
  toQuery GetModels' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetModelsResponse' smart constructor.
data GetModelsResponse = GetModelsResponse'
  { -- | The elements from this collection.
    items :: Prelude.Maybe [Model],
    -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetModelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getModelsResponse_items' - The elements from this collection.
--
-- 'nextToken', 'getModelsResponse_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'httpStatus', 'getModelsResponse_httpStatus' - The response's http status code.
newGetModelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetModelsResponse
newGetModelsResponse pHttpStatus_ =
  GetModelsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The elements from this collection.
getModelsResponse_items :: Lens.Lens' GetModelsResponse (Prelude.Maybe [Model])
getModelsResponse_items = Lens.lens (\GetModelsResponse' {items} -> items) (\s@GetModelsResponse' {} a -> s {items = a} :: GetModelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getModelsResponse_nextToken :: Lens.Lens' GetModelsResponse (Prelude.Maybe Prelude.Text)
getModelsResponse_nextToken = Lens.lens (\GetModelsResponse' {nextToken} -> nextToken) (\s@GetModelsResponse' {} a -> s {nextToken = a} :: GetModelsResponse)

-- | The response's http status code.
getModelsResponse_httpStatus :: Lens.Lens' GetModelsResponse Prelude.Int
getModelsResponse_httpStatus = Lens.lens (\GetModelsResponse' {httpStatus} -> httpStatus) (\s@GetModelsResponse' {} a -> s {httpStatus = a} :: GetModelsResponse)

instance Prelude.NFData GetModelsResponse where
  rnf GetModelsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
