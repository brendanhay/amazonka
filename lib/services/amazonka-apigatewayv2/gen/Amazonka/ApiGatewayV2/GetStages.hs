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
-- Module      : Amazonka.ApiGatewayV2.GetStages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Stages for an API.
--
-- This operation returns paginated results.
module Amazonka.ApiGatewayV2.GetStages
  ( -- * Creating a Request
    GetStages (..),
    newGetStages,

    -- * Request Lenses
    getStages_maxResults,
    getStages_nextToken,
    getStages_apiId,

    -- * Destructuring the Response
    GetStagesResponse (..),
    newGetStagesResponse,

    -- * Response Lenses
    getStagesResponse_items,
    getStagesResponse_nextToken,
    getStagesResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStages' smart constructor.
data GetStages = GetStages'
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
-- Create a value of 'GetStages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getStages_maxResults' - The maximum number of elements to be returned for this resource.
--
-- 'nextToken', 'getStages_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'apiId', 'getStages_apiId' - The API identifier.
newGetStages ::
  -- | 'apiId'
  Prelude.Text ->
  GetStages
newGetStages pApiId_ =
  GetStages'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      apiId = pApiId_
    }

-- | The maximum number of elements to be returned for this resource.
getStages_maxResults :: Lens.Lens' GetStages (Prelude.Maybe Prelude.Text)
getStages_maxResults = Lens.lens (\GetStages' {maxResults} -> maxResults) (\s@GetStages' {} a -> s {maxResults = a} :: GetStages)

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getStages_nextToken :: Lens.Lens' GetStages (Prelude.Maybe Prelude.Text)
getStages_nextToken = Lens.lens (\GetStages' {nextToken} -> nextToken) (\s@GetStages' {} a -> s {nextToken = a} :: GetStages)

-- | The API identifier.
getStages_apiId :: Lens.Lens' GetStages Prelude.Text
getStages_apiId = Lens.lens (\GetStages' {apiId} -> apiId) (\s@GetStages' {} a -> s {apiId = a} :: GetStages)

instance Core.AWSPager GetStages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getStagesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getStagesResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getStages_nextToken
          Lens..~ rs
          Lens.^? getStagesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetStages where
  type AWSResponse GetStages = GetStagesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStagesResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStages where
  hashWithSalt _salt GetStages' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData GetStages where
  rnf GetStages' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders GetStages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetStages where
  toPath GetStages' {..} =
    Prelude.mconcat
      ["/v2/apis/", Data.toBS apiId, "/stages"]

instance Data.ToQuery GetStages where
  toQuery GetStages' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetStagesResponse' smart constructor.
data GetStagesResponse = GetStagesResponse'
  { -- | The elements from this collection.
    items :: Prelude.Maybe [Stage],
    -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getStagesResponse_items' - The elements from this collection.
--
-- 'nextToken', 'getStagesResponse_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'httpStatus', 'getStagesResponse_httpStatus' - The response's http status code.
newGetStagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStagesResponse
newGetStagesResponse pHttpStatus_ =
  GetStagesResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The elements from this collection.
getStagesResponse_items :: Lens.Lens' GetStagesResponse (Prelude.Maybe [Stage])
getStagesResponse_items = Lens.lens (\GetStagesResponse' {items} -> items) (\s@GetStagesResponse' {} a -> s {items = a} :: GetStagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getStagesResponse_nextToken :: Lens.Lens' GetStagesResponse (Prelude.Maybe Prelude.Text)
getStagesResponse_nextToken = Lens.lens (\GetStagesResponse' {nextToken} -> nextToken) (\s@GetStagesResponse' {} a -> s {nextToken = a} :: GetStagesResponse)

-- | The response's http status code.
getStagesResponse_httpStatus :: Lens.Lens' GetStagesResponse Prelude.Int
getStagesResponse_httpStatus = Lens.lens (\GetStagesResponse' {httpStatus} -> httpStatus) (\s@GetStagesResponse' {} a -> s {httpStatus = a} :: GetStagesResponse)

instance Prelude.NFData GetStagesResponse where
  rnf GetStagesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
