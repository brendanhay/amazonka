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
-- Module      : Network.AWS.AppSync.ListResolvers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resolvers for a given API and type.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListResolvers
  ( -- * Creating a Request
    ListResolvers (..),
    newListResolvers,

    -- * Request Lenses
    listResolvers_nextToken,
    listResolvers_maxResults,
    listResolvers_apiId,
    listResolvers_typeName,

    -- * Destructuring the Response
    ListResolversResponse (..),
    newListResolversResponse,

    -- * Response Lenses
    listResolversResponse_nextToken,
    listResolversResponse_resolvers,
    listResolversResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListResolvers' smart constructor.
data ListResolvers = ListResolvers'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results you want the request to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The API ID.
    apiId :: Core.Text,
    -- | The type name.
    typeName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResolvers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResolvers_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'maxResults', 'listResolvers_maxResults' - The maximum number of results you want the request to return.
--
-- 'apiId', 'listResolvers_apiId' - The API ID.
--
-- 'typeName', 'listResolvers_typeName' - The type name.
newListResolvers ::
  -- | 'apiId'
  Core.Text ->
  -- | 'typeName'
  Core.Text ->
  ListResolvers
newListResolvers pApiId_ pTypeName_ =
  ListResolvers'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      apiId = pApiId_,
      typeName = pTypeName_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listResolvers_nextToken :: Lens.Lens' ListResolvers (Core.Maybe Core.Text)
listResolvers_nextToken = Lens.lens (\ListResolvers' {nextToken} -> nextToken) (\s@ListResolvers' {} a -> s {nextToken = a} :: ListResolvers)

-- | The maximum number of results you want the request to return.
listResolvers_maxResults :: Lens.Lens' ListResolvers (Core.Maybe Core.Natural)
listResolvers_maxResults = Lens.lens (\ListResolvers' {maxResults} -> maxResults) (\s@ListResolvers' {} a -> s {maxResults = a} :: ListResolvers)

-- | The API ID.
listResolvers_apiId :: Lens.Lens' ListResolvers Core.Text
listResolvers_apiId = Lens.lens (\ListResolvers' {apiId} -> apiId) (\s@ListResolvers' {} a -> s {apiId = a} :: ListResolvers)

-- | The type name.
listResolvers_typeName :: Lens.Lens' ListResolvers Core.Text
listResolvers_typeName = Lens.lens (\ListResolvers' {typeName} -> typeName) (\s@ListResolvers' {} a -> s {typeName = a} :: ListResolvers)

instance Core.AWSPager ListResolvers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResolversResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listResolversResponse_resolvers Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listResolvers_nextToken
          Lens..~ rs
          Lens.^? listResolversResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListResolvers where
  type
    AWSResponse ListResolvers =
      ListResolversResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResolversResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "resolvers" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListResolvers

instance Core.NFData ListResolvers

instance Core.ToHeaders ListResolvers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListResolvers where
  toPath ListResolvers' {..} =
    Core.mconcat
      [ "/v1/apis/",
        Core.toBS apiId,
        "/types/",
        Core.toBS typeName,
        "/resolvers"
      ]

instance Core.ToQuery ListResolvers where
  toQuery ListResolvers' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListResolversResponse' smart constructor.
data ListResolversResponse = ListResolversResponse'
  { -- | An identifier to be passed in the next request to this operation to
    -- return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | The @Resolver@ objects.
    resolvers :: Core.Maybe [Resolver],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListResolversResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResolversResponse_nextToken' - An identifier to be passed in the next request to this operation to
-- return the next set of items in the list.
--
-- 'resolvers', 'listResolversResponse_resolvers' - The @Resolver@ objects.
--
-- 'httpStatus', 'listResolversResponse_httpStatus' - The response's http status code.
newListResolversResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListResolversResponse
newListResolversResponse pHttpStatus_ =
  ListResolversResponse'
    { nextToken = Core.Nothing,
      resolvers = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier to be passed in the next request to this operation to
-- return the next set of items in the list.
listResolversResponse_nextToken :: Lens.Lens' ListResolversResponse (Core.Maybe Core.Text)
listResolversResponse_nextToken = Lens.lens (\ListResolversResponse' {nextToken} -> nextToken) (\s@ListResolversResponse' {} a -> s {nextToken = a} :: ListResolversResponse)

-- | The @Resolver@ objects.
listResolversResponse_resolvers :: Lens.Lens' ListResolversResponse (Core.Maybe [Resolver])
listResolversResponse_resolvers = Lens.lens (\ListResolversResponse' {resolvers} -> resolvers) (\s@ListResolversResponse' {} a -> s {resolvers = a} :: ListResolversResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listResolversResponse_httpStatus :: Lens.Lens' ListResolversResponse Core.Int
listResolversResponse_httpStatus = Lens.lens (\ListResolversResponse' {httpStatus} -> httpStatus) (\s@ListResolversResponse' {} a -> s {httpStatus = a} :: ListResolversResponse)

instance Core.NFData ListResolversResponse
