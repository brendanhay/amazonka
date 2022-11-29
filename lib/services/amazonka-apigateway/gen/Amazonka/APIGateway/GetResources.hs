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
-- Module      : Amazonka.APIGateway.GetResources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about a collection of Resource resources.
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetResources
  ( -- * Creating a Request
    GetResources (..),
    newGetResources,

    -- * Request Lenses
    getResources_limit,
    getResources_position,
    getResources_embed,
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to list information about a collection of resources.
--
-- /See:/ 'newGetResources' smart constructor.
data GetResources = GetResources'
  { -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | A query parameter used to retrieve the specified resources embedded in
    -- the returned Resources resource in the response. This @embed@ parameter
    -- value is a list of comma-separated strings. Currently, the request
    -- supports only retrieval of the embedded Method resources this way. The
    -- query parameter value must be a single-valued list and contain the
    -- @\"methods\"@ string. For example,
    -- @GET \/restapis\/{restapi_id}\/resources?embed=methods@.
    embed :: Prelude.Maybe [Prelude.Text],
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getResources_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
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
-- 'restApiId', 'getResources_restApiId' - The string identifier of the associated RestApi.
newGetResources ::
  -- | 'restApiId'
  Prelude.Text ->
  GetResources
newGetResources pRestApiId_ =
  GetResources'
    { limit = Prelude.Nothing,
      position = Prelude.Nothing,
      embed = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getResources_limit :: Lens.Lens' GetResources (Prelude.Maybe Prelude.Int)
getResources_limit = Lens.lens (\GetResources' {limit} -> limit) (\s@GetResources' {} a -> s {limit = a} :: GetResources)

-- | The current pagination position in the paged result set.
getResources_position :: Lens.Lens' GetResources (Prelude.Maybe Prelude.Text)
getResources_position = Lens.lens (\GetResources' {position} -> position) (\s@GetResources' {} a -> s {position = a} :: GetResources)

-- | A query parameter used to retrieve the specified resources embedded in
-- the returned Resources resource in the response. This @embed@ parameter
-- value is a list of comma-separated strings. Currently, the request
-- supports only retrieval of the embedded Method resources this way. The
-- query parameter value must be a single-valued list and contain the
-- @\"methods\"@ string. For example,
-- @GET \/restapis\/{restapi_id}\/resources?embed=methods@.
getResources_embed :: Lens.Lens' GetResources (Prelude.Maybe [Prelude.Text])
getResources_embed = Lens.lens (\GetResources' {embed} -> embed) (\s@GetResources' {} a -> s {embed = a} :: GetResources) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
getResources_restApiId :: Lens.Lens' GetResources Prelude.Text
getResources_restApiId = Lens.lens (\GetResources' {restApiId} -> restApiId) (\s@GetResources' {} a -> s {restApiId = a} :: GetResources)

instance Core.AWSPager GetResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getResourcesResponse_position Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getResourcesResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getResources_position
          Lens..~ rs
          Lens.^? getResourcesResponse_position Prelude.. Lens._Just

instance Core.AWSRequest GetResources where
  type AWSResponse GetResources = GetResourcesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcesResponse'
            Prelude.<$> (x Core..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResources where
  hashWithSalt _salt GetResources' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` embed
      `Prelude.hashWithSalt` restApiId

instance Prelude.NFData GetResources where
  rnf GetResources' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf embed
      `Prelude.seq` Prelude.rnf restApiId

instance Core.ToHeaders GetResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetResources where
  toPath GetResources' {..} =
    Prelude.mconcat
      ["/restapis/", Core.toBS restApiId, "/resources"]

instance Core.ToQuery GetResources where
  toQuery GetResources' {..} =
    Prelude.mconcat
      [ "limit" Core.=: limit,
        "position" Core.=: position,
        "embed"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> embed)
      ]

-- | Represents a collection of Resource resources.
--
-- /See:/ 'newGetResourcesResponse' smart constructor.
data GetResourcesResponse = GetResourcesResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [Resource],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetResourcesResponse
newGetResourcesResponse pHttpStatus_ =
  GetResourcesResponse'
    { items = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getResourcesResponse_items :: Lens.Lens' GetResourcesResponse (Prelude.Maybe [Resource])
getResourcesResponse_items = Lens.lens (\GetResourcesResponse' {items} -> items) (\s@GetResourcesResponse' {} a -> s {items = a} :: GetResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getResourcesResponse_position :: Lens.Lens' GetResourcesResponse (Prelude.Maybe Prelude.Text)
getResourcesResponse_position = Lens.lens (\GetResourcesResponse' {position} -> position) (\s@GetResourcesResponse' {} a -> s {position = a} :: GetResourcesResponse)

-- | The response's http status code.
getResourcesResponse_httpStatus :: Lens.Lens' GetResourcesResponse Prelude.Int
getResourcesResponse_httpStatus = Lens.lens (\GetResourcesResponse' {httpStatus} -> httpStatus) (\s@GetResourcesResponse' {} a -> s {httpStatus = a} :: GetResourcesResponse)

instance Prelude.NFData GetResourcesResponse where
  rnf GetResourcesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf httpStatus
