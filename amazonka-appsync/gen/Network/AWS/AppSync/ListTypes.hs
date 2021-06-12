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
-- Module      : Network.AWS.AppSync.ListTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the types for a given API.
--
-- This operation returns paginated results.
module Network.AWS.AppSync.ListTypes
  ( -- * Creating a Request
    ListTypes (..),
    newListTypes,

    -- * Request Lenses
    listTypes_nextToken,
    listTypes_maxResults,
    listTypes_apiId,
    listTypes_format,

    -- * Destructuring the Response
    ListTypesResponse (..),
    newListTypesResponse,

    -- * Response Lenses
    listTypesResponse_nextToken,
    listTypesResponse_types,
    listTypesResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTypes' smart constructor.
data ListTypes = ListTypes'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results you want the request to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The API ID.
    apiId :: Core.Text,
    -- | The type format: SDL or JSON.
    format :: TypeDefinitionFormat
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTypes_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'maxResults', 'listTypes_maxResults' - The maximum number of results you want the request to return.
--
-- 'apiId', 'listTypes_apiId' - The API ID.
--
-- 'format', 'listTypes_format' - The type format: SDL or JSON.
newListTypes ::
  -- | 'apiId'
  Core.Text ->
  -- | 'format'
  TypeDefinitionFormat ->
  ListTypes
newListTypes pApiId_ pFormat_ =
  ListTypes'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      apiId = pApiId_,
      format = pFormat_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listTypes_nextToken :: Lens.Lens' ListTypes (Core.Maybe Core.Text)
listTypes_nextToken = Lens.lens (\ListTypes' {nextToken} -> nextToken) (\s@ListTypes' {} a -> s {nextToken = a} :: ListTypes)

-- | The maximum number of results you want the request to return.
listTypes_maxResults :: Lens.Lens' ListTypes (Core.Maybe Core.Natural)
listTypes_maxResults = Lens.lens (\ListTypes' {maxResults} -> maxResults) (\s@ListTypes' {} a -> s {maxResults = a} :: ListTypes)

-- | The API ID.
listTypes_apiId :: Lens.Lens' ListTypes Core.Text
listTypes_apiId = Lens.lens (\ListTypes' {apiId} -> apiId) (\s@ListTypes' {} a -> s {apiId = a} :: ListTypes)

-- | The type format: SDL or JSON.
listTypes_format :: Lens.Lens' ListTypes TypeDefinitionFormat
listTypes_format = Lens.lens (\ListTypes' {format} -> format) (\s@ListTypes' {} a -> s {format = a} :: ListTypes)

instance Core.AWSPager ListTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTypesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTypesResponse_types Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTypes_nextToken
          Lens..~ rs
          Lens.^? listTypesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListTypes where
  type AWSResponse ListTypes = ListTypesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTypesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "types" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTypes

instance Core.NFData ListTypes

instance Core.ToHeaders ListTypes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListTypes where
  toPath ListTypes' {..} =
    Core.mconcat
      ["/v1/apis/", Core.toBS apiId, "/types"]

instance Core.ToQuery ListTypes where
  toQuery ListTypes' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "format" Core.=: format
      ]

-- | /See:/ 'newListTypesResponse' smart constructor.
data ListTypesResponse = ListTypesResponse'
  { -- | An identifier to be passed in the next request to this operation to
    -- return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | The @Type@ objects.
    types :: Core.Maybe [Type],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTypesResponse_nextToken' - An identifier to be passed in the next request to this operation to
-- return the next set of items in the list.
--
-- 'types', 'listTypesResponse_types' - The @Type@ objects.
--
-- 'httpStatus', 'listTypesResponse_httpStatus' - The response's http status code.
newListTypesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTypesResponse
newListTypesResponse pHttpStatus_ =
  ListTypesResponse'
    { nextToken = Core.Nothing,
      types = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier to be passed in the next request to this operation to
-- return the next set of items in the list.
listTypesResponse_nextToken :: Lens.Lens' ListTypesResponse (Core.Maybe Core.Text)
listTypesResponse_nextToken = Lens.lens (\ListTypesResponse' {nextToken} -> nextToken) (\s@ListTypesResponse' {} a -> s {nextToken = a} :: ListTypesResponse)

-- | The @Type@ objects.
listTypesResponse_types :: Lens.Lens' ListTypesResponse (Core.Maybe [Type])
listTypesResponse_types = Lens.lens (\ListTypesResponse' {types} -> types) (\s@ListTypesResponse' {} a -> s {types = a} :: ListTypesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTypesResponse_httpStatus :: Lens.Lens' ListTypesResponse Core.Int
listTypesResponse_httpStatus = Lens.lens (\ListTypesResponse' {httpStatus} -> httpStatus) (\s@ListTypesResponse' {} a -> s {httpStatus = a} :: ListTypesResponse)

instance Core.NFData ListTypesResponse
