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
-- Module      : Network.AWS.AppSync.ListGraphqlApis
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your GraphQL APIs.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListGraphqlApis
  ( -- * Creating a Request
    ListGraphqlApis (..),
    newListGraphqlApis,

    -- * Request Lenses
    listGraphqlApis_nextToken,
    listGraphqlApis_maxResults,

    -- * Destructuring the Response
    ListGraphqlApisResponse (..),
    newListGraphqlApisResponse,

    -- * Response Lenses
    listGraphqlApisResponse_nextToken,
    listGraphqlApisResponse_graphqlApis,
    listGraphqlApisResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGraphqlApis' smart constructor.
data ListGraphqlApis = ListGraphqlApis'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results you want the request to return.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGraphqlApis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGraphqlApis_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'maxResults', 'listGraphqlApis_maxResults' - The maximum number of results you want the request to return.
newListGraphqlApis ::
  ListGraphqlApis
newListGraphqlApis =
  ListGraphqlApis'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listGraphqlApis_nextToken :: Lens.Lens' ListGraphqlApis (Core.Maybe Core.Text)
listGraphqlApis_nextToken = Lens.lens (\ListGraphqlApis' {nextToken} -> nextToken) (\s@ListGraphqlApis' {} a -> s {nextToken = a} :: ListGraphqlApis)

-- | The maximum number of results you want the request to return.
listGraphqlApis_maxResults :: Lens.Lens' ListGraphqlApis (Core.Maybe Core.Natural)
listGraphqlApis_maxResults = Lens.lens (\ListGraphqlApis' {maxResults} -> maxResults) (\s@ListGraphqlApis' {} a -> s {maxResults = a} :: ListGraphqlApis)

instance Core.AWSPager ListGraphqlApis where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGraphqlApisResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listGraphqlApisResponse_graphqlApis
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listGraphqlApis_nextToken
          Lens..~ rs
          Lens.^? listGraphqlApisResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListGraphqlApis where
  type
    AWSResponse ListGraphqlApis =
      ListGraphqlApisResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGraphqlApisResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "graphqlApis" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListGraphqlApis

instance Core.NFData ListGraphqlApis

instance Core.ToHeaders ListGraphqlApis where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListGraphqlApis where
  toPath = Core.const "/v1/apis"

instance Core.ToQuery ListGraphqlApis where
  toQuery ListGraphqlApis' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListGraphqlApisResponse' smart constructor.
data ListGraphqlApisResponse = ListGraphqlApisResponse'
  { -- | An identifier to be passed in the next request to this operation to
    -- return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | The @GraphqlApi@ objects.
    graphqlApis :: Core.Maybe [GraphqlApi],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGraphqlApisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGraphqlApisResponse_nextToken' - An identifier to be passed in the next request to this operation to
-- return the next set of items in the list.
--
-- 'graphqlApis', 'listGraphqlApisResponse_graphqlApis' - The @GraphqlApi@ objects.
--
-- 'httpStatus', 'listGraphqlApisResponse_httpStatus' - The response's http status code.
newListGraphqlApisResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListGraphqlApisResponse
newListGraphqlApisResponse pHttpStatus_ =
  ListGraphqlApisResponse'
    { nextToken = Core.Nothing,
      graphqlApis = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier to be passed in the next request to this operation to
-- return the next set of items in the list.
listGraphqlApisResponse_nextToken :: Lens.Lens' ListGraphqlApisResponse (Core.Maybe Core.Text)
listGraphqlApisResponse_nextToken = Lens.lens (\ListGraphqlApisResponse' {nextToken} -> nextToken) (\s@ListGraphqlApisResponse' {} a -> s {nextToken = a} :: ListGraphqlApisResponse)

-- | The @GraphqlApi@ objects.
listGraphqlApisResponse_graphqlApis :: Lens.Lens' ListGraphqlApisResponse (Core.Maybe [GraphqlApi])
listGraphqlApisResponse_graphqlApis = Lens.lens (\ListGraphqlApisResponse' {graphqlApis} -> graphqlApis) (\s@ListGraphqlApisResponse' {} a -> s {graphqlApis = a} :: ListGraphqlApisResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listGraphqlApisResponse_httpStatus :: Lens.Lens' ListGraphqlApisResponse Core.Int
listGraphqlApisResponse_httpStatus = Lens.lens (\ListGraphqlApisResponse' {httpStatus} -> httpStatus) (\s@ListGraphqlApisResponse' {} a -> s {httpStatus = a} :: ListGraphqlApisResponse)

instance Core.NFData ListGraphqlApisResponse
