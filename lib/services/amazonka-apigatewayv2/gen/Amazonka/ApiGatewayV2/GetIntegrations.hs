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
-- Module      : Amazonka.ApiGatewayV2.GetIntegrations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Integrations for an API.
--
-- This operation returns paginated results.
module Amazonka.ApiGatewayV2.GetIntegrations
  ( -- * Creating a Request
    GetIntegrations (..),
    newGetIntegrations,

    -- * Request Lenses
    getIntegrations_maxResults,
    getIntegrations_nextToken,
    getIntegrations_apiId,

    -- * Destructuring the Response
    GetIntegrationsResponse (..),
    newGetIntegrationsResponse,

    -- * Response Lenses
    getIntegrationsResponse_items,
    getIntegrationsResponse_nextToken,
    getIntegrationsResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIntegrations' smart constructor.
data GetIntegrations = GetIntegrations'
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
-- Create a value of 'GetIntegrations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getIntegrations_maxResults' - The maximum number of elements to be returned for this resource.
--
-- 'nextToken', 'getIntegrations_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'apiId', 'getIntegrations_apiId' - The API identifier.
newGetIntegrations ::
  -- | 'apiId'
  Prelude.Text ->
  GetIntegrations
newGetIntegrations pApiId_ =
  GetIntegrations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      apiId = pApiId_
    }

-- | The maximum number of elements to be returned for this resource.
getIntegrations_maxResults :: Lens.Lens' GetIntegrations (Prelude.Maybe Prelude.Text)
getIntegrations_maxResults = Lens.lens (\GetIntegrations' {maxResults} -> maxResults) (\s@GetIntegrations' {} a -> s {maxResults = a} :: GetIntegrations)

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getIntegrations_nextToken :: Lens.Lens' GetIntegrations (Prelude.Maybe Prelude.Text)
getIntegrations_nextToken = Lens.lens (\GetIntegrations' {nextToken} -> nextToken) (\s@GetIntegrations' {} a -> s {nextToken = a} :: GetIntegrations)

-- | The API identifier.
getIntegrations_apiId :: Lens.Lens' GetIntegrations Prelude.Text
getIntegrations_apiId = Lens.lens (\GetIntegrations' {apiId} -> apiId) (\s@GetIntegrations' {} a -> s {apiId = a} :: GetIntegrations)

instance Core.AWSPager GetIntegrations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getIntegrationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getIntegrationsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getIntegrations_nextToken
              Lens..~ rs
              Lens.^? getIntegrationsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetIntegrations where
  type
    AWSResponse GetIntegrations =
      GetIntegrationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIntegrationsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIntegrations where
  hashWithSalt _salt GetIntegrations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData GetIntegrations where
  rnf GetIntegrations' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf apiId

instance Data.ToHeaders GetIntegrations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetIntegrations where
  toPath GetIntegrations' {..} =
    Prelude.mconcat
      ["/v2/apis/", Data.toBS apiId, "/integrations"]

instance Data.ToQuery GetIntegrations where
  toQuery GetIntegrations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetIntegrationsResponse' smart constructor.
data GetIntegrationsResponse = GetIntegrationsResponse'
  { -- | The elements from this collection.
    items :: Prelude.Maybe [Integration],
    -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIntegrationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getIntegrationsResponse_items' - The elements from this collection.
--
-- 'nextToken', 'getIntegrationsResponse_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'httpStatus', 'getIntegrationsResponse_httpStatus' - The response's http status code.
newGetIntegrationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIntegrationsResponse
newGetIntegrationsResponse pHttpStatus_ =
  GetIntegrationsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The elements from this collection.
getIntegrationsResponse_items :: Lens.Lens' GetIntegrationsResponse (Prelude.Maybe [Integration])
getIntegrationsResponse_items = Lens.lens (\GetIntegrationsResponse' {items} -> items) (\s@GetIntegrationsResponse' {} a -> s {items = a} :: GetIntegrationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getIntegrationsResponse_nextToken :: Lens.Lens' GetIntegrationsResponse (Prelude.Maybe Prelude.Text)
getIntegrationsResponse_nextToken = Lens.lens (\GetIntegrationsResponse' {nextToken} -> nextToken) (\s@GetIntegrationsResponse' {} a -> s {nextToken = a} :: GetIntegrationsResponse)

-- | The response's http status code.
getIntegrationsResponse_httpStatus :: Lens.Lens' GetIntegrationsResponse Prelude.Int
getIntegrationsResponse_httpStatus = Lens.lens (\GetIntegrationsResponse' {httpStatus} -> httpStatus) (\s@GetIntegrationsResponse' {} a -> s {httpStatus = a} :: GetIntegrationsResponse)

instance Prelude.NFData GetIntegrationsResponse where
  rnf GetIntegrationsResponse' {..} =
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
