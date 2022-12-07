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
-- Module      : Amazonka.ApiGatewayV2.GetDomainNames
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the domain names for an AWS account.
--
-- This operation returns paginated results.
module Amazonka.ApiGatewayV2.GetDomainNames
  ( -- * Creating a Request
    GetDomainNames (..),
    newGetDomainNames,

    -- * Request Lenses
    getDomainNames_nextToken,
    getDomainNames_maxResults,

    -- * Destructuring the Response
    GetDomainNamesResponse (..),
    newGetDomainNamesResponse,

    -- * Response Lenses
    getDomainNamesResponse_items,
    getDomainNamesResponse_nextToken,
    getDomainNamesResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDomainNames' smart constructor.
data GetDomainNames = GetDomainNames'
  { -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of elements to be returned for this resource.
    maxResults :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDomainNames_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'maxResults', 'getDomainNames_maxResults' - The maximum number of elements to be returned for this resource.
newGetDomainNames ::
  GetDomainNames
newGetDomainNames =
  GetDomainNames'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getDomainNames_nextToken :: Lens.Lens' GetDomainNames (Prelude.Maybe Prelude.Text)
getDomainNames_nextToken = Lens.lens (\GetDomainNames' {nextToken} -> nextToken) (\s@GetDomainNames' {} a -> s {nextToken = a} :: GetDomainNames)

-- | The maximum number of elements to be returned for this resource.
getDomainNames_maxResults :: Lens.Lens' GetDomainNames (Prelude.Maybe Prelude.Text)
getDomainNames_maxResults = Lens.lens (\GetDomainNames' {maxResults} -> maxResults) (\s@GetDomainNames' {} a -> s {maxResults = a} :: GetDomainNames)

instance Core.AWSPager GetDomainNames where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDomainNamesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getDomainNamesResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getDomainNames_nextToken
          Lens..~ rs
          Lens.^? getDomainNamesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetDomainNames where
  type
    AWSResponse GetDomainNames =
      GetDomainNamesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainNamesResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDomainNames where
  hashWithSalt _salt GetDomainNames' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData GetDomainNames where
  rnf GetDomainNames' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders GetDomainNames where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDomainNames where
  toPath = Prelude.const "/v2/domainnames"

instance Data.ToQuery GetDomainNames where
  toQuery GetDomainNames' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newGetDomainNamesResponse' smart constructor.
data GetDomainNamesResponse = GetDomainNamesResponse'
  { -- | The elements from this collection.
    items :: Prelude.Maybe [DomainName],
    -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainNamesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getDomainNamesResponse_items' - The elements from this collection.
--
-- 'nextToken', 'getDomainNamesResponse_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'httpStatus', 'getDomainNamesResponse_httpStatus' - The response's http status code.
newGetDomainNamesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDomainNamesResponse
newGetDomainNamesResponse pHttpStatus_ =
  GetDomainNamesResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The elements from this collection.
getDomainNamesResponse_items :: Lens.Lens' GetDomainNamesResponse (Prelude.Maybe [DomainName])
getDomainNamesResponse_items = Lens.lens (\GetDomainNamesResponse' {items} -> items) (\s@GetDomainNamesResponse' {} a -> s {items = a} :: GetDomainNamesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getDomainNamesResponse_nextToken :: Lens.Lens' GetDomainNamesResponse (Prelude.Maybe Prelude.Text)
getDomainNamesResponse_nextToken = Lens.lens (\GetDomainNamesResponse' {nextToken} -> nextToken) (\s@GetDomainNamesResponse' {} a -> s {nextToken = a} :: GetDomainNamesResponse)

-- | The response's http status code.
getDomainNamesResponse_httpStatus :: Lens.Lens' GetDomainNamesResponse Prelude.Int
getDomainNamesResponse_httpStatus = Lens.lens (\GetDomainNamesResponse' {httpStatus} -> httpStatus) (\s@GetDomainNamesResponse' {} a -> s {httpStatus = a} :: GetDomainNamesResponse)

instance Prelude.NFData GetDomainNamesResponse where
  rnf GetDomainNamesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
