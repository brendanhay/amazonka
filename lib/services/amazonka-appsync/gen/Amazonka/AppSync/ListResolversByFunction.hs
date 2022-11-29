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
-- Module      : Amazonka.AppSync.ListResolversByFunction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the resolvers that are associated with a specific function.
--
-- This operation returns paginated results.
module Amazonka.AppSync.ListResolversByFunction
  ( -- * Creating a Request
    ListResolversByFunction (..),
    newListResolversByFunction,

    -- * Request Lenses
    listResolversByFunction_nextToken,
    listResolversByFunction_maxResults,
    listResolversByFunction_apiId,
    listResolversByFunction_functionId,

    -- * Destructuring the Response
    ListResolversByFunctionResponse (..),
    newListResolversByFunctionResponse,

    -- * Response Lenses
    listResolversByFunctionResponse_nextToken,
    listResolversByFunctionResponse_resolvers,
    listResolversByFunctionResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResolversByFunction' smart constructor.
data ListResolversByFunction = ListResolversByFunction'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which you can use to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that you want the request to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The API ID.
    apiId :: Prelude.Text,
    -- | The function ID.
    functionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolversByFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResolversByFunction_nextToken' - An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
--
-- 'maxResults', 'listResolversByFunction_maxResults' - The maximum number of results that you want the request to return.
--
-- 'apiId', 'listResolversByFunction_apiId' - The API ID.
--
-- 'functionId', 'listResolversByFunction_functionId' - The function ID.
newListResolversByFunction ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'functionId'
  Prelude.Text ->
  ListResolversByFunction
newListResolversByFunction pApiId_ pFunctionId_ =
  ListResolversByFunction'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      apiId = pApiId_,
      functionId = pFunctionId_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
listResolversByFunction_nextToken :: Lens.Lens' ListResolversByFunction (Prelude.Maybe Prelude.Text)
listResolversByFunction_nextToken = Lens.lens (\ListResolversByFunction' {nextToken} -> nextToken) (\s@ListResolversByFunction' {} a -> s {nextToken = a} :: ListResolversByFunction)

-- | The maximum number of results that you want the request to return.
listResolversByFunction_maxResults :: Lens.Lens' ListResolversByFunction (Prelude.Maybe Prelude.Natural)
listResolversByFunction_maxResults = Lens.lens (\ListResolversByFunction' {maxResults} -> maxResults) (\s@ListResolversByFunction' {} a -> s {maxResults = a} :: ListResolversByFunction)

-- | The API ID.
listResolversByFunction_apiId :: Lens.Lens' ListResolversByFunction Prelude.Text
listResolversByFunction_apiId = Lens.lens (\ListResolversByFunction' {apiId} -> apiId) (\s@ListResolversByFunction' {} a -> s {apiId = a} :: ListResolversByFunction)

-- | The function ID.
listResolversByFunction_functionId :: Lens.Lens' ListResolversByFunction Prelude.Text
listResolversByFunction_functionId = Lens.lens (\ListResolversByFunction' {functionId} -> functionId) (\s@ListResolversByFunction' {} a -> s {functionId = a} :: ListResolversByFunction)

instance Core.AWSPager ListResolversByFunction where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResolversByFunctionResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResolversByFunctionResponse_resolvers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResolversByFunction_nextToken
          Lens..~ rs
          Lens.^? listResolversByFunctionResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListResolversByFunction where
  type
    AWSResponse ListResolversByFunction =
      ListResolversByFunctionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResolversByFunctionResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "resolvers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListResolversByFunction where
  hashWithSalt _salt ListResolversByFunction' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` functionId

instance Prelude.NFData ListResolversByFunction where
  rnf ListResolversByFunction' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf functionId

instance Core.ToHeaders ListResolversByFunction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListResolversByFunction where
  toPath ListResolversByFunction' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/functions/",
        Core.toBS functionId,
        "/resolvers"
      ]

instance Core.ToQuery ListResolversByFunction where
  toQuery ListResolversByFunction' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListResolversByFunctionResponse' smart constructor.
data ListResolversByFunctionResponse = ListResolversByFunctionResponse'
  { -- | An identifier that you can use to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of resolvers.
    resolvers :: Prelude.Maybe [Resolver],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolversByFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResolversByFunctionResponse_nextToken' - An identifier that you can use to return the next set of items in the
-- list.
--
-- 'resolvers', 'listResolversByFunctionResponse_resolvers' - The list of resolvers.
--
-- 'httpStatus', 'listResolversByFunctionResponse_httpStatus' - The response's http status code.
newListResolversByFunctionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResolversByFunctionResponse
newListResolversByFunctionResponse pHttpStatus_ =
  ListResolversByFunctionResponse'
    { nextToken =
        Prelude.Nothing,
      resolvers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that you can use to return the next set of items in the
-- list.
listResolversByFunctionResponse_nextToken :: Lens.Lens' ListResolversByFunctionResponse (Prelude.Maybe Prelude.Text)
listResolversByFunctionResponse_nextToken = Lens.lens (\ListResolversByFunctionResponse' {nextToken} -> nextToken) (\s@ListResolversByFunctionResponse' {} a -> s {nextToken = a} :: ListResolversByFunctionResponse)

-- | The list of resolvers.
listResolversByFunctionResponse_resolvers :: Lens.Lens' ListResolversByFunctionResponse (Prelude.Maybe [Resolver])
listResolversByFunctionResponse_resolvers = Lens.lens (\ListResolversByFunctionResponse' {resolvers} -> resolvers) (\s@ListResolversByFunctionResponse' {} a -> s {resolvers = a} :: ListResolversByFunctionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResolversByFunctionResponse_httpStatus :: Lens.Lens' ListResolversByFunctionResponse Prelude.Int
listResolversByFunctionResponse_httpStatus = Lens.lens (\ListResolversByFunctionResponse' {httpStatus} -> httpStatus) (\s@ListResolversByFunctionResponse' {} a -> s {httpStatus = a} :: ListResolversByFunctionResponse)

instance
  Prelude.NFData
    ListResolversByFunctionResponse
  where
  rnf ListResolversByFunctionResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resolvers
      `Prelude.seq` Prelude.rnf httpStatus
