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
-- Module      : Amazonka.APIGateway.GetDocumentationParts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets documentation parts.
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetDocumentationParts
  ( -- * Creating a Request
    GetDocumentationParts (..),
    newGetDocumentationParts,

    -- * Request Lenses
    getDocumentationParts_limit,
    getDocumentationParts_locationStatus,
    getDocumentationParts_nameQuery,
    getDocumentationParts_path,
    getDocumentationParts_position,
    getDocumentationParts_type,
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Gets the documentation parts of an API. The result may be filtered by
-- the type, name, or path of API entities (targets).
--
-- /See:/ 'newGetDocumentationParts' smart constructor.
data GetDocumentationParts = GetDocumentationParts'
  { -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The status of the API documentation parts to retrieve. Valid values are
    -- @DOCUMENTED@ for retrieving DocumentationPart resources with content and
    -- @UNDOCUMENTED@ for DocumentationPart resources without content.
    locationStatus :: Prelude.Maybe LocationStatusType,
    -- | The name of API entities of the to-be-retrieved documentation parts.
    nameQuery :: Prelude.Maybe Prelude.Text,
    -- | The path of API entities of the to-be-retrieved documentation parts.
    path :: Prelude.Maybe Prelude.Text,
    -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | The type of API entities of the to-be-retrieved documentation parts.
    type' :: Prelude.Maybe DocumentationPartType,
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentationParts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getDocumentationParts_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'locationStatus', 'getDocumentationParts_locationStatus' - The status of the API documentation parts to retrieve. Valid values are
-- @DOCUMENTED@ for retrieving DocumentationPart resources with content and
-- @UNDOCUMENTED@ for DocumentationPart resources without content.
--
-- 'nameQuery', 'getDocumentationParts_nameQuery' - The name of API entities of the to-be-retrieved documentation parts.
--
-- 'path', 'getDocumentationParts_path' - The path of API entities of the to-be-retrieved documentation parts.
--
-- 'position', 'getDocumentationParts_position' - The current pagination position in the paged result set.
--
-- 'type'', 'getDocumentationParts_type' - The type of API entities of the to-be-retrieved documentation parts.
--
-- 'restApiId', 'getDocumentationParts_restApiId' - The string identifier of the associated RestApi.
newGetDocumentationParts ::
  -- | 'restApiId'
  Prelude.Text ->
  GetDocumentationParts
newGetDocumentationParts pRestApiId_ =
  GetDocumentationParts'
    { limit = Prelude.Nothing,
      locationStatus = Prelude.Nothing,
      nameQuery = Prelude.Nothing,
      path = Prelude.Nothing,
      position = Prelude.Nothing,
      type' = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getDocumentationParts_limit :: Lens.Lens' GetDocumentationParts (Prelude.Maybe Prelude.Int)
getDocumentationParts_limit = Lens.lens (\GetDocumentationParts' {limit} -> limit) (\s@GetDocumentationParts' {} a -> s {limit = a} :: GetDocumentationParts)

-- | The status of the API documentation parts to retrieve. Valid values are
-- @DOCUMENTED@ for retrieving DocumentationPart resources with content and
-- @UNDOCUMENTED@ for DocumentationPart resources without content.
getDocumentationParts_locationStatus :: Lens.Lens' GetDocumentationParts (Prelude.Maybe LocationStatusType)
getDocumentationParts_locationStatus = Lens.lens (\GetDocumentationParts' {locationStatus} -> locationStatus) (\s@GetDocumentationParts' {} a -> s {locationStatus = a} :: GetDocumentationParts)

-- | The name of API entities of the to-be-retrieved documentation parts.
getDocumentationParts_nameQuery :: Lens.Lens' GetDocumentationParts (Prelude.Maybe Prelude.Text)
getDocumentationParts_nameQuery = Lens.lens (\GetDocumentationParts' {nameQuery} -> nameQuery) (\s@GetDocumentationParts' {} a -> s {nameQuery = a} :: GetDocumentationParts)

-- | The path of API entities of the to-be-retrieved documentation parts.
getDocumentationParts_path :: Lens.Lens' GetDocumentationParts (Prelude.Maybe Prelude.Text)
getDocumentationParts_path = Lens.lens (\GetDocumentationParts' {path} -> path) (\s@GetDocumentationParts' {} a -> s {path = a} :: GetDocumentationParts)

-- | The current pagination position in the paged result set.
getDocumentationParts_position :: Lens.Lens' GetDocumentationParts (Prelude.Maybe Prelude.Text)
getDocumentationParts_position = Lens.lens (\GetDocumentationParts' {position} -> position) (\s@GetDocumentationParts' {} a -> s {position = a} :: GetDocumentationParts)

-- | The type of API entities of the to-be-retrieved documentation parts.
getDocumentationParts_type :: Lens.Lens' GetDocumentationParts (Prelude.Maybe DocumentationPartType)
getDocumentationParts_type = Lens.lens (\GetDocumentationParts' {type'} -> type') (\s@GetDocumentationParts' {} a -> s {type' = a} :: GetDocumentationParts)

-- | The string identifier of the associated RestApi.
getDocumentationParts_restApiId :: Lens.Lens' GetDocumentationParts Prelude.Text
getDocumentationParts_restApiId = Lens.lens (\GetDocumentationParts' {restApiId} -> restApiId) (\s@GetDocumentationParts' {} a -> s {restApiId = a} :: GetDocumentationParts)

instance Core.AWSPager GetDocumentationParts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDocumentationPartsResponse_position
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getDocumentationPartsResponse_items
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getDocumentationParts_position
          Lens..~ rs
          Lens.^? getDocumentationPartsResponse_position
            Prelude.. Lens._Just

instance Core.AWSRequest GetDocumentationParts where
  type
    AWSResponse GetDocumentationParts =
      GetDocumentationPartsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentationPartsResponse'
            Prelude.<$> (x Data..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDocumentationParts where
  hashWithSalt _salt GetDocumentationParts' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` locationStatus
      `Prelude.hashWithSalt` nameQuery
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` restApiId

instance Prelude.NFData GetDocumentationParts where
  rnf GetDocumentationParts' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf locationStatus
      `Prelude.seq` Prelude.rnf nameQuery
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf restApiId

instance Data.ToHeaders GetDocumentationParts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetDocumentationParts where
  toPath GetDocumentationParts' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/documentation/parts"
      ]

instance Data.ToQuery GetDocumentationParts where
  toQuery GetDocumentationParts' {..} =
    Prelude.mconcat
      [ "limit" Data.=: limit,
        "locationStatus" Data.=: locationStatus,
        "name" Data.=: nameQuery,
        "path" Data.=: path,
        "position" Data.=: position,
        "type" Data.=: type'
      ]

-- | The collection of documentation parts of an API.
--
-- /See:/ 'newGetDocumentationPartsResponse' smart constructor.
data GetDocumentationPartsResponse = GetDocumentationPartsResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [DocumentationPart],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetDocumentationPartsResponse
newGetDocumentationPartsResponse pHttpStatus_ =
  GetDocumentationPartsResponse'
    { items =
        Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getDocumentationPartsResponse_items :: Lens.Lens' GetDocumentationPartsResponse (Prelude.Maybe [DocumentationPart])
getDocumentationPartsResponse_items = Lens.lens (\GetDocumentationPartsResponse' {items} -> items) (\s@GetDocumentationPartsResponse' {} a -> s {items = a} :: GetDocumentationPartsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getDocumentationPartsResponse_position :: Lens.Lens' GetDocumentationPartsResponse (Prelude.Maybe Prelude.Text)
getDocumentationPartsResponse_position = Lens.lens (\GetDocumentationPartsResponse' {position} -> position) (\s@GetDocumentationPartsResponse' {} a -> s {position = a} :: GetDocumentationPartsResponse)

-- | The response's http status code.
getDocumentationPartsResponse_httpStatus :: Lens.Lens' GetDocumentationPartsResponse Prelude.Int
getDocumentationPartsResponse_httpStatus = Lens.lens (\GetDocumentationPartsResponse' {httpStatus} -> httpStatus) (\s@GetDocumentationPartsResponse' {} a -> s {httpStatus = a} :: GetDocumentationPartsResponse)

instance Prelude.NFData GetDocumentationPartsResponse where
  rnf GetDocumentationPartsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf httpStatus
