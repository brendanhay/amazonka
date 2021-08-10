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
-- Module      : Network.AWS.APIGateway.GetDocumentationVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetDocumentationVersions
  ( -- * Creating a Request
    GetDocumentationVersions (..),
    newGetDocumentationVersions,

    -- * Request Lenses
    getDocumentationVersions_position,
    getDocumentationVersions_limit,
    getDocumentationVersions_restApiId,

    -- * Destructuring the Response
    GetDocumentationVersionsResponse (..),
    newGetDocumentationVersionsResponse,

    -- * Response Lenses
    getDocumentationVersionsResponse_items,
    getDocumentationVersionsResponse_position,
    getDocumentationVersionsResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets the documentation versions of an API.
--
-- /See:/ 'newGetDocumentationVersions' smart constructor.
data GetDocumentationVersions = GetDocumentationVersions'
  { -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentationVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getDocumentationVersions_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getDocumentationVersions_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'restApiId', 'getDocumentationVersions_restApiId' - [Required] The string identifier of the associated RestApi.
newGetDocumentationVersions ::
  -- | 'restApiId'
  Prelude.Text ->
  GetDocumentationVersions
newGetDocumentationVersions pRestApiId_ =
  GetDocumentationVersions'
    { position =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | The current pagination position in the paged result set.
getDocumentationVersions_position :: Lens.Lens' GetDocumentationVersions (Prelude.Maybe Prelude.Text)
getDocumentationVersions_position = Lens.lens (\GetDocumentationVersions' {position} -> position) (\s@GetDocumentationVersions' {} a -> s {position = a} :: GetDocumentationVersions)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getDocumentationVersions_limit :: Lens.Lens' GetDocumentationVersions (Prelude.Maybe Prelude.Int)
getDocumentationVersions_limit = Lens.lens (\GetDocumentationVersions' {limit} -> limit) (\s@GetDocumentationVersions' {} a -> s {limit = a} :: GetDocumentationVersions)

-- | [Required] The string identifier of the associated RestApi.
getDocumentationVersions_restApiId :: Lens.Lens' GetDocumentationVersions Prelude.Text
getDocumentationVersions_restApiId = Lens.lens (\GetDocumentationVersions' {restApiId} -> restApiId) (\s@GetDocumentationVersions' {} a -> s {restApiId = a} :: GetDocumentationVersions)

instance Core.AWSPager GetDocumentationVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDocumentationVersionsResponse_position
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getDocumentationVersionsResponse_items
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getDocumentationVersions_position
          Lens..~ rs
          Lens.^? getDocumentationVersionsResponse_position
            Prelude.. Lens._Just

instance Core.AWSRequest GetDocumentationVersions where
  type
    AWSResponse GetDocumentationVersions =
      GetDocumentationVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentationVersionsResponse'
            Prelude.<$> (x Core..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDocumentationVersions

instance Prelude.NFData GetDocumentationVersions

instance Core.ToHeaders GetDocumentationVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetDocumentationVersions where
  toPath GetDocumentationVersions' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/documentation/versions"
      ]

instance Core.ToQuery GetDocumentationVersions where
  toQuery GetDocumentationVersions' {..} =
    Prelude.mconcat
      ["position" Core.=: position, "limit" Core.=: limit]

-- | The collection of documentation snapshots of an API.
--
-- Use the DocumentationVersions to manage documentation snapshots
-- associated with various API stages.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API>,
-- DocumentationPart, DocumentationVersion
--
-- /See:/ 'newGetDocumentationVersionsResponse' smart constructor.
data GetDocumentationVersionsResponse = GetDocumentationVersionsResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [DocumentationVersion],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentationVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getDocumentationVersionsResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getDocumentationVersionsResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getDocumentationVersionsResponse_httpStatus' - The response's http status code.
newGetDocumentationVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDocumentationVersionsResponse
newGetDocumentationVersionsResponse pHttpStatus_ =
  GetDocumentationVersionsResponse'
    { items =
        Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getDocumentationVersionsResponse_items :: Lens.Lens' GetDocumentationVersionsResponse (Prelude.Maybe [DocumentationVersion])
getDocumentationVersionsResponse_items = Lens.lens (\GetDocumentationVersionsResponse' {items} -> items) (\s@GetDocumentationVersionsResponse' {} a -> s {items = a} :: GetDocumentationVersionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getDocumentationVersionsResponse_position :: Lens.Lens' GetDocumentationVersionsResponse (Prelude.Maybe Prelude.Text)
getDocumentationVersionsResponse_position = Lens.lens (\GetDocumentationVersionsResponse' {position} -> position) (\s@GetDocumentationVersionsResponse' {} a -> s {position = a} :: GetDocumentationVersionsResponse)

-- | The response's http status code.
getDocumentationVersionsResponse_httpStatus :: Lens.Lens' GetDocumentationVersionsResponse Prelude.Int
getDocumentationVersionsResponse_httpStatus = Lens.lens (\GetDocumentationVersionsResponse' {httpStatus} -> httpStatus) (\s@GetDocumentationVersionsResponse' {} a -> s {httpStatus = a} :: GetDocumentationVersionsResponse)

instance
  Prelude.NFData
    GetDocumentationVersionsResponse
