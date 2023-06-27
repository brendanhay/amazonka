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
-- Module      : Amazonka.ApiGatewayV2.GetDeployments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Deployments for an API.
--
-- This operation returns paginated results.
module Amazonka.ApiGatewayV2.GetDeployments
  ( -- * Creating a Request
    GetDeployments (..),
    newGetDeployments,

    -- * Request Lenses
    getDeployments_maxResults,
    getDeployments_nextToken,
    getDeployments_apiId,

    -- * Destructuring the Response
    GetDeploymentsResponse (..),
    newGetDeploymentsResponse,

    -- * Response Lenses
    getDeploymentsResponse_items,
    getDeploymentsResponse_nextToken,
    getDeploymentsResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDeployments' smart constructor.
data GetDeployments = GetDeployments'
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
-- Create a value of 'GetDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getDeployments_maxResults' - The maximum number of elements to be returned for this resource.
--
-- 'nextToken', 'getDeployments_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'apiId', 'getDeployments_apiId' - The API identifier.
newGetDeployments ::
  -- | 'apiId'
  Prelude.Text ->
  GetDeployments
newGetDeployments pApiId_ =
  GetDeployments'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      apiId = pApiId_
    }

-- | The maximum number of elements to be returned for this resource.
getDeployments_maxResults :: Lens.Lens' GetDeployments (Prelude.Maybe Prelude.Text)
getDeployments_maxResults = Lens.lens (\GetDeployments' {maxResults} -> maxResults) (\s@GetDeployments' {} a -> s {maxResults = a} :: GetDeployments)

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getDeployments_nextToken :: Lens.Lens' GetDeployments (Prelude.Maybe Prelude.Text)
getDeployments_nextToken = Lens.lens (\GetDeployments' {nextToken} -> nextToken) (\s@GetDeployments' {} a -> s {nextToken = a} :: GetDeployments)

-- | The API identifier.
getDeployments_apiId :: Lens.Lens' GetDeployments Prelude.Text
getDeployments_apiId = Lens.lens (\GetDeployments' {apiId} -> apiId) (\s@GetDeployments' {} a -> s {apiId = a} :: GetDeployments)

instance Core.AWSPager GetDeployments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDeploymentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getDeploymentsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getDeployments_nextToken
          Lens..~ rs
          Lens.^? getDeploymentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetDeployments where
  type
    AWSResponse GetDeployments =
      GetDeploymentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeployments where
  hashWithSalt _salt GetDeployments' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData GetDeployments where
  rnf GetDeployments' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders GetDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDeployments where
  toPath GetDeployments' {..} =
    Prelude.mconcat
      ["/v2/apis/", Data.toBS apiId, "/deployments"]

instance Data.ToQuery GetDeployments where
  toQuery GetDeployments' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetDeploymentsResponse' smart constructor.
data GetDeploymentsResponse = GetDeploymentsResponse'
  { -- | The elements from this collection.
    items :: Prelude.Maybe [Deployment],
    -- | The next page of elements from this collection. Not valid for the last
    -- element of the collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getDeploymentsResponse_items' - The elements from this collection.
--
-- 'nextToken', 'getDeploymentsResponse_nextToken' - The next page of elements from this collection. Not valid for the last
-- element of the collection.
--
-- 'httpStatus', 'getDeploymentsResponse_httpStatus' - The response's http status code.
newGetDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeploymentsResponse
newGetDeploymentsResponse pHttpStatus_ =
  GetDeploymentsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The elements from this collection.
getDeploymentsResponse_items :: Lens.Lens' GetDeploymentsResponse (Prelude.Maybe [Deployment])
getDeploymentsResponse_items = Lens.lens (\GetDeploymentsResponse' {items} -> items) (\s@GetDeploymentsResponse' {} a -> s {items = a} :: GetDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page of elements from this collection. Not valid for the last
-- element of the collection.
getDeploymentsResponse_nextToken :: Lens.Lens' GetDeploymentsResponse (Prelude.Maybe Prelude.Text)
getDeploymentsResponse_nextToken = Lens.lens (\GetDeploymentsResponse' {nextToken} -> nextToken) (\s@GetDeploymentsResponse' {} a -> s {nextToken = a} :: GetDeploymentsResponse)

-- | The response's http status code.
getDeploymentsResponse_httpStatus :: Lens.Lens' GetDeploymentsResponse Prelude.Int
getDeploymentsResponse_httpStatus = Lens.lens (\GetDeploymentsResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentsResponse' {} a -> s {httpStatus = a} :: GetDeploymentsResponse)

instance Prelude.NFData GetDeploymentsResponse where
  rnf GetDeploymentsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
