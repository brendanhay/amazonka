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
-- Module      : Network.AWS.APIGateway.GetDocumentationParts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetDocumentationParts
  ( -- * Creating a Request
    GetDocumentationParts (..),
    newGetDocumentationParts,

    -- * Request Lenses
    getDocumentationParts_locationStatus,
    getDocumentationParts_position,
    getDocumentationParts_type,
    getDocumentationParts_limit,
    getDocumentationParts_path,
    getDocumentationParts_nameQuery,
    getDocumentationParts_restApiId,

    -- * Destructuring the Response
    GetDocumentationPartsResponse (..),
    newGetDocumentationPartsResponse,

    -- * Response Lenses
    getDocumentationPartsResponse_items,
    getDocumentationPartsResponse_position,
    getDocumentationPartsResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets the documentation parts of an API. The result may be filtered by
-- the type, name, or path of API entities (targets).
--
-- /See:/ 'newGetDocumentationParts' smart constructor.
data GetDocumentationParts = GetDocumentationParts'
  { -- | The status of the API documentation parts to retrieve. Valid values are
    -- @DOCUMENTED@ for retrieving DocumentationPart resources with content and
    -- @UNDOCUMENTED@ for DocumentationPart resources without content.
    locationStatus :: Core.Maybe LocationStatusType,
    -- | The current pagination position in the paged result set.
    position :: Core.Maybe Core.Text,
    -- | The type of API entities of the to-be-retrieved documentation parts.
    type' :: Core.Maybe DocumentationPartType,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Core.Maybe Core.Int,
    -- | The path of API entities of the to-be-retrieved documentation parts.
    path :: Core.Maybe Core.Text,
    -- | The name of API entities of the to-be-retrieved documentation parts.
    nameQuery :: Core.Maybe Core.Text,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDocumentationParts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationStatus', 'getDocumentationParts_locationStatus' - The status of the API documentation parts to retrieve. Valid values are
-- @DOCUMENTED@ for retrieving DocumentationPart resources with content and
-- @UNDOCUMENTED@ for DocumentationPart resources without content.
--
-- 'position', 'getDocumentationParts_position' - The current pagination position in the paged result set.
--
-- 'type'', 'getDocumentationParts_type' - The type of API entities of the to-be-retrieved documentation parts.
--
-- 'limit', 'getDocumentationParts_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'path', 'getDocumentationParts_path' - The path of API entities of the to-be-retrieved documentation parts.
--
-- 'nameQuery', 'getDocumentationParts_nameQuery' - The name of API entities of the to-be-retrieved documentation parts.
--
-- 'restApiId', 'getDocumentationParts_restApiId' - [Required] The string identifier of the associated RestApi.
newGetDocumentationParts ::
  -- | 'restApiId'
  Core.Text ->
  GetDocumentationParts
newGetDocumentationParts pRestApiId_ =
  GetDocumentationParts'
    { locationStatus =
        Core.Nothing,
      position = Core.Nothing,
      type' = Core.Nothing,
      limit = Core.Nothing,
      path = Core.Nothing,
      nameQuery = Core.Nothing,
      restApiId = pRestApiId_
    }

-- | The status of the API documentation parts to retrieve. Valid values are
-- @DOCUMENTED@ for retrieving DocumentationPart resources with content and
-- @UNDOCUMENTED@ for DocumentationPart resources without content.
getDocumentationParts_locationStatus :: Lens.Lens' GetDocumentationParts (Core.Maybe LocationStatusType)
getDocumentationParts_locationStatus = Lens.lens (\GetDocumentationParts' {locationStatus} -> locationStatus) (\s@GetDocumentationParts' {} a -> s {locationStatus = a} :: GetDocumentationParts)

-- | The current pagination position in the paged result set.
getDocumentationParts_position :: Lens.Lens' GetDocumentationParts (Core.Maybe Core.Text)
getDocumentationParts_position = Lens.lens (\GetDocumentationParts' {position} -> position) (\s@GetDocumentationParts' {} a -> s {position = a} :: GetDocumentationParts)

-- | The type of API entities of the to-be-retrieved documentation parts.
getDocumentationParts_type :: Lens.Lens' GetDocumentationParts (Core.Maybe DocumentationPartType)
getDocumentationParts_type = Lens.lens (\GetDocumentationParts' {type'} -> type') (\s@GetDocumentationParts' {} a -> s {type' = a} :: GetDocumentationParts)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getDocumentationParts_limit :: Lens.Lens' GetDocumentationParts (Core.Maybe Core.Int)
getDocumentationParts_limit = Lens.lens (\GetDocumentationParts' {limit} -> limit) (\s@GetDocumentationParts' {} a -> s {limit = a} :: GetDocumentationParts)

-- | The path of API entities of the to-be-retrieved documentation parts.
getDocumentationParts_path :: Lens.Lens' GetDocumentationParts (Core.Maybe Core.Text)
getDocumentationParts_path = Lens.lens (\GetDocumentationParts' {path} -> path) (\s@GetDocumentationParts' {} a -> s {path = a} :: GetDocumentationParts)

-- | The name of API entities of the to-be-retrieved documentation parts.
getDocumentationParts_nameQuery :: Lens.Lens' GetDocumentationParts (Core.Maybe Core.Text)
getDocumentationParts_nameQuery = Lens.lens (\GetDocumentationParts' {nameQuery} -> nameQuery) (\s@GetDocumentationParts' {} a -> s {nameQuery = a} :: GetDocumentationParts)

-- | [Required] The string identifier of the associated RestApi.
getDocumentationParts_restApiId :: Lens.Lens' GetDocumentationParts Core.Text
getDocumentationParts_restApiId = Lens.lens (\GetDocumentationParts' {restApiId} -> restApiId) (\s@GetDocumentationParts' {} a -> s {restApiId = a} :: GetDocumentationParts)

instance Core.AWSPager GetDocumentationParts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDocumentationPartsResponse_position
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getDocumentationPartsResponse_items
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getDocumentationParts_position
          Lens..~ rs
          Lens.^? getDocumentationPartsResponse_position
            Core.. Lens._Just

instance Core.AWSRequest GetDocumentationParts where
  type
    AWSResponse GetDocumentationParts =
      GetDocumentationPartsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentationPartsResponse'
            Core.<$> (x Core..?> "item" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "position")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDocumentationParts

instance Core.NFData GetDocumentationParts

instance Core.ToHeaders GetDocumentationParts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetDocumentationParts where
  toPath GetDocumentationParts' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/documentation/parts"
      ]

instance Core.ToQuery GetDocumentationParts where
  toQuery GetDocumentationParts' {..} =
    Core.mconcat
      [ "locationStatus" Core.=: locationStatus,
        "position" Core.=: position,
        "type" Core.=: type',
        "limit" Core.=: limit,
        "path" Core.=: path,
        "name" Core.=: nameQuery
      ]

-- | The collection of documentation parts of an API.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API>,
-- DocumentationPart
--
-- /See:/ 'newGetDocumentationPartsResponse' smart constructor.
data GetDocumentationPartsResponse = GetDocumentationPartsResponse'
  { -- | The current page of elements from this collection.
    items :: Core.Maybe [DocumentationPart],
    position :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDocumentationPartsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getDocumentationPartsResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getDocumentationPartsResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getDocumentationPartsResponse_httpStatus' - The response's http status code.
newGetDocumentationPartsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDocumentationPartsResponse
newGetDocumentationPartsResponse pHttpStatus_ =
  GetDocumentationPartsResponse'
    { items =
        Core.Nothing,
      position = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getDocumentationPartsResponse_items :: Lens.Lens' GetDocumentationPartsResponse (Core.Maybe [DocumentationPart])
getDocumentationPartsResponse_items = Lens.lens (\GetDocumentationPartsResponse' {items} -> items) (\s@GetDocumentationPartsResponse' {} a -> s {items = a} :: GetDocumentationPartsResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getDocumentationPartsResponse_position :: Lens.Lens' GetDocumentationPartsResponse (Core.Maybe Core.Text)
getDocumentationPartsResponse_position = Lens.lens (\GetDocumentationPartsResponse' {position} -> position) (\s@GetDocumentationPartsResponse' {} a -> s {position = a} :: GetDocumentationPartsResponse)

-- | The response's http status code.
getDocumentationPartsResponse_httpStatus :: Lens.Lens' GetDocumentationPartsResponse Core.Int
getDocumentationPartsResponse_httpStatus = Lens.lens (\GetDocumentationPartsResponse' {httpStatus} -> httpStatus) (\s@GetDocumentationPartsResponse' {} a -> s {httpStatus = a} :: GetDocumentationPartsResponse)

instance Core.NFData GetDocumentationPartsResponse
