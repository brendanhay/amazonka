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
-- Module      : Amazonka.ApiGatewayV2.GetApis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a collection of Api resources.
--
-- This operation returns paginated results.
module Amazonka.ApiGatewayV2.GetApis
  ( -- * Creating a Request
    GetApis (..),
    newGetApis,

    -- * Request Lenses
    getApis_maxResults,
    getApis_nextToken,

    -- * Destructuring the Response
    GetApisResponse (..),
    newGetApisResponse,

    -- * Response Lenses
    getApisResponse_items,
    getApisResponse_nextToken,
    getApisResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetApis' smart constructor.
data GetApis = GetApis'
  { -- | The maximum number of elements to be returned for this resource.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getApis_maxResults' - The maximum number of elements to be returned for this resource.
--
-- 'nextToken', 'getApis_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
newGetApis ::
  GetApis
newGetApis =
  GetApis'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of elements to be returned for this resource.
getApis_maxResults :: Lens.Lens' GetApis (Prelude.Maybe Prelude.Text)
getApis_maxResults = Lens.lens (\GetApis' {maxResults} -> maxResults) (\s@GetApis' {} a -> s {maxResults = a} :: GetApis)

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getApis_nextToken :: Lens.Lens' GetApis (Prelude.Maybe Prelude.Text)
getApis_nextToken = Lens.lens (\GetApis' {nextToken} -> nextToken) (\s@GetApis' {} a -> s {nextToken = a} :: GetApis)

instance Core.AWSPager GetApis where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getApisResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getApisResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getApis_nextToken
              Lens..~ rs
              Lens.^? getApisResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetApis where
  type AWSResponse GetApis = GetApisResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApisResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApis where
  hashWithSalt _salt GetApis' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetApis where
  rnf GetApis' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders GetApis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetApis where
  toPath = Prelude.const "/v2/apis"

instance Data.ToQuery GetApis where
  toQuery GetApis' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetApisResponse' smart constructor.
data GetApisResponse = GetApisResponse'
  { -- | The elements from this collection.
    items :: Prelude.Maybe [Api],
    -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getApisResponse_items' - The elements from this collection.
--
-- 'nextToken', 'getApisResponse_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'httpStatus', 'getApisResponse_httpStatus' - The response's http status code.
newGetApisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApisResponse
newGetApisResponse pHttpStatus_ =
  GetApisResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The elements from this collection.
getApisResponse_items :: Lens.Lens' GetApisResponse (Prelude.Maybe [Api])
getApisResponse_items = Lens.lens (\GetApisResponse' {items} -> items) (\s@GetApisResponse' {} a -> s {items = a} :: GetApisResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getApisResponse_nextToken :: Lens.Lens' GetApisResponse (Prelude.Maybe Prelude.Text)
getApisResponse_nextToken = Lens.lens (\GetApisResponse' {nextToken} -> nextToken) (\s@GetApisResponse' {} a -> s {nextToken = a} :: GetApisResponse)

-- | The response's http status code.
getApisResponse_httpStatus :: Lens.Lens' GetApisResponse Prelude.Int
getApisResponse_httpStatus = Lens.lens (\GetApisResponse' {httpStatus} -> httpStatus) (\s@GetApisResponse' {} a -> s {httpStatus = a} :: GetApisResponse)

instance Prelude.NFData GetApisResponse where
  rnf GetApisResponse' {..} =
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
