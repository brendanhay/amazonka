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
-- Module      : Network.AWS.AppSync.ListResolversByFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the resolvers that are associated with a specific function.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListResolversByFunction
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

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListResolversByFunction' smart constructor.
data ListResolversByFunction = ListResolversByFunction'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which you can use to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results you want the request to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The API ID.
    apiId :: Core.Text,
    -- | The Function ID.
    functionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'maxResults', 'listResolversByFunction_maxResults' - The maximum number of results you want the request to return.
--
-- 'apiId', 'listResolversByFunction_apiId' - The API ID.
--
-- 'functionId', 'listResolversByFunction_functionId' - The Function ID.
newListResolversByFunction ::
  -- | 'apiId'
  Core.Text ->
  -- | 'functionId'
  Core.Text ->
  ListResolversByFunction
newListResolversByFunction pApiId_ pFunctionId_ =
  ListResolversByFunction'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      apiId = pApiId_,
      functionId = pFunctionId_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which you can use to return the next set of items in the
-- list.
listResolversByFunction_nextToken :: Lens.Lens' ListResolversByFunction (Core.Maybe Core.Text)
listResolversByFunction_nextToken = Lens.lens (\ListResolversByFunction' {nextToken} -> nextToken) (\s@ListResolversByFunction' {} a -> s {nextToken = a} :: ListResolversByFunction)

-- | The maximum number of results you want the request to return.
listResolversByFunction_maxResults :: Lens.Lens' ListResolversByFunction (Core.Maybe Core.Natural)
listResolversByFunction_maxResults = Lens.lens (\ListResolversByFunction' {maxResults} -> maxResults) (\s@ListResolversByFunction' {} a -> s {maxResults = a} :: ListResolversByFunction)

-- | The API ID.
listResolversByFunction_apiId :: Lens.Lens' ListResolversByFunction Core.Text
listResolversByFunction_apiId = Lens.lens (\ListResolversByFunction' {apiId} -> apiId) (\s@ListResolversByFunction' {} a -> s {apiId = a} :: ListResolversByFunction)

-- | The Function ID.
listResolversByFunction_functionId :: Lens.Lens' ListResolversByFunction Core.Text
listResolversByFunction_functionId = Lens.lens (\ListResolversByFunction' {functionId} -> functionId) (\s@ListResolversByFunction' {} a -> s {functionId = a} :: ListResolversByFunction)

instance Core.AWSPager ListResolversByFunction where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResolversByFunctionResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listResolversByFunctionResponse_resolvers
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listResolversByFunction_nextToken
          Lens..~ rs
          Lens.^? listResolversByFunctionResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListResolversByFunction where
  type
    AWSResponse ListResolversByFunction =
      ListResolversByFunctionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResolversByFunctionResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "resolvers" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListResolversByFunction

instance Core.NFData ListResolversByFunction

instance Core.ToHeaders ListResolversByFunction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListResolversByFunction where
  toPath ListResolversByFunction' {..} =
    Core.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/functions/",
        Core.toBS functionId,
        "/resolvers"
      ]

instance Core.ToQuery ListResolversByFunction where
  toQuery ListResolversByFunction' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListResolversByFunctionResponse' smart constructor.
data ListResolversByFunctionResponse = ListResolversByFunctionResponse'
  { -- | An identifier that can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of resolvers.
    resolvers :: Core.Maybe [Resolver],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResolversByFunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResolversByFunctionResponse_nextToken' - An identifier that can be used to return the next set of items in the
-- list.
--
-- 'resolvers', 'listResolversByFunctionResponse_resolvers' - The list of resolvers.
--
-- 'httpStatus', 'listResolversByFunctionResponse_httpStatus' - The response's http status code.
newListResolversByFunctionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListResolversByFunctionResponse
newListResolversByFunctionResponse pHttpStatus_ =
  ListResolversByFunctionResponse'
    { nextToken =
        Core.Nothing,
      resolvers = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that can be used to return the next set of items in the
-- list.
listResolversByFunctionResponse_nextToken :: Lens.Lens' ListResolversByFunctionResponse (Core.Maybe Core.Text)
listResolversByFunctionResponse_nextToken = Lens.lens (\ListResolversByFunctionResponse' {nextToken} -> nextToken) (\s@ListResolversByFunctionResponse' {} a -> s {nextToken = a} :: ListResolversByFunctionResponse)

-- | The list of resolvers.
listResolversByFunctionResponse_resolvers :: Lens.Lens' ListResolversByFunctionResponse (Core.Maybe [Resolver])
listResolversByFunctionResponse_resolvers = Lens.lens (\ListResolversByFunctionResponse' {resolvers} -> resolvers) (\s@ListResolversByFunctionResponse' {} a -> s {resolvers = a} :: ListResolversByFunctionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listResolversByFunctionResponse_httpStatus :: Lens.Lens' ListResolversByFunctionResponse Core.Int
listResolversByFunctionResponse_httpStatus = Lens.lens (\ListResolversByFunctionResponse' {httpStatus} -> httpStatus) (\s@ListResolversByFunctionResponse' {} a -> s {httpStatus = a} :: ListResolversByFunctionResponse)

instance Core.NFData ListResolversByFunctionResponse
