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
-- Module      : Network.AWS.APIGateway.GetResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about a collection of Resource resources.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetResources
  ( -- * Creating a Request
    GetResources (..),
    newGetResources,

    -- * Request Lenses
    getResources_position,
    getResources_embed,
    getResources_limit,
    getResources_restApiId,

    -- * Destructuring the Response
    GetResourcesResponse (..),
    newGetResourcesResponse,

    -- * Response Lenses
    getResourcesResponse_items,
    getResourcesResponse_position,
    getResourcesResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to list information about a collection of resources.
--
-- /See:/ 'newGetResources' smart constructor.
data GetResources = GetResources'
  { -- | The current pagination position in the paged result set.
    position :: Core.Maybe Core.Text,
    -- | A query parameter used to retrieve the specified resources embedded in
    -- the returned Resources resource in the response. This @embed@ parameter
    -- value is a list of comma-separated strings. Currently, the request
    -- supports only retrieval of the embedded Method resources this way. The
    -- query parameter value must be a single-valued list and contain the
    -- @\"methods\"@ string. For example,
    -- @GET \/restapis\/{restapi_id}\/resources?embed=methods@.
    embed :: Core.Maybe [Core.Text],
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Core.Maybe Core.Int,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getResources_position' - The current pagination position in the paged result set.
--
-- 'embed', 'getResources_embed' - A query parameter used to retrieve the specified resources embedded in
-- the returned Resources resource in the response. This @embed@ parameter
-- value is a list of comma-separated strings. Currently, the request
-- supports only retrieval of the embedded Method resources this way. The
-- query parameter value must be a single-valued list and contain the
-- @\"methods\"@ string. For example,
-- @GET \/restapis\/{restapi_id}\/resources?embed=methods@.
--
-- 'limit', 'getResources_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'restApiId', 'getResources_restApiId' - [Required] The string identifier of the associated RestApi.
newGetResources ::
  -- | 'restApiId'
  Core.Text ->
  GetResources
newGetResources pRestApiId_ =
  GetResources'
    { position = Core.Nothing,
      embed = Core.Nothing,
      limit = Core.Nothing,
      restApiId = pRestApiId_
    }

-- | The current pagination position in the paged result set.
getResources_position :: Lens.Lens' GetResources (Core.Maybe Core.Text)
getResources_position = Lens.lens (\GetResources' {position} -> position) (\s@GetResources' {} a -> s {position = a} :: GetResources)

-- | A query parameter used to retrieve the specified resources embedded in
-- the returned Resources resource in the response. This @embed@ parameter
-- value is a list of comma-separated strings. Currently, the request
-- supports only retrieval of the embedded Method resources this way. The
-- query parameter value must be a single-valued list and contain the
-- @\"methods\"@ string. For example,
-- @GET \/restapis\/{restapi_id}\/resources?embed=methods@.
getResources_embed :: Lens.Lens' GetResources (Core.Maybe [Core.Text])
getResources_embed = Lens.lens (\GetResources' {embed} -> embed) (\s@GetResources' {} a -> s {embed = a} :: GetResources) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getResources_limit :: Lens.Lens' GetResources (Core.Maybe Core.Int)
getResources_limit = Lens.lens (\GetResources' {limit} -> limit) (\s@GetResources' {} a -> s {limit = a} :: GetResources)

-- | [Required] The string identifier of the associated RestApi.
getResources_restApiId :: Lens.Lens' GetResources Core.Text
getResources_restApiId = Lens.lens (\GetResources' {restApiId} -> restApiId) (\s@GetResources' {} a -> s {restApiId = a} :: GetResources)

instance Core.AWSPager GetResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getResourcesResponse_position Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getResourcesResponse_items Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getResources_position
          Lens..~ rs
          Lens.^? getResourcesResponse_position Core.. Lens._Just

instance Core.AWSRequest GetResources where
  type AWSResponse GetResources = GetResourcesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcesResponse'
            Core.<$> (x Core..?> "item" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "position")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetResources

instance Core.NFData GetResources

instance Core.ToHeaders GetResources where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetResources where
  toPath GetResources' {..} =
    Core.mconcat
      ["/restapis/", Core.toBS restApiId, "/resources"]

instance Core.ToQuery GetResources where
  toQuery GetResources' {..} =
    Core.mconcat
      [ "position" Core.=: position,
        "embed"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> embed),
        "limit" Core.=: limit
      ]

-- | Represents a collection of Resource resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Create an API>
--
-- /See:/ 'newGetResourcesResponse' smart constructor.
data GetResourcesResponse = GetResourcesResponse'
  { -- | The current page of elements from this collection.
    items :: Core.Maybe [Resource],
    position :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getResourcesResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getResourcesResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getResourcesResponse_httpStatus' - The response's http status code.
newGetResourcesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetResourcesResponse
newGetResourcesResponse pHttpStatus_ =
  GetResourcesResponse'
    { items = Core.Nothing,
      position = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getResourcesResponse_items :: Lens.Lens' GetResourcesResponse (Core.Maybe [Resource])
getResourcesResponse_items = Lens.lens (\GetResourcesResponse' {items} -> items) (\s@GetResourcesResponse' {} a -> s {items = a} :: GetResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getResourcesResponse_position :: Lens.Lens' GetResourcesResponse (Core.Maybe Core.Text)
getResourcesResponse_position = Lens.lens (\GetResourcesResponse' {position} -> position) (\s@GetResourcesResponse' {} a -> s {position = a} :: GetResourcesResponse)

-- | The response's http status code.
getResourcesResponse_httpStatus :: Lens.Lens' GetResourcesResponse Core.Int
getResourcesResponse_httpStatus = Lens.lens (\GetResourcesResponse' {httpStatus} -> httpStatus) (\s@GetResourcesResponse' {} a -> s {httpStatus = a} :: GetResourcesResponse)

instance Core.NFData GetResourcesResponse
