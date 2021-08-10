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
-- Module      : Network.AWS.APIGateway.GetModels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes existing Models defined for a RestApi resource.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetModels
  ( -- * Creating a Request
    GetModels (..),
    newGetModels,

    -- * Request Lenses
    getModels_position,
    getModels_limit,
    getModels_restApiId,

    -- * Destructuring the Response
    GetModelsResponse (..),
    newGetModelsResponse,

    -- * Response Lenses
    getModelsResponse_items,
    getModelsResponse_position,
    getModelsResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to list existing Models defined for a RestApi resource.
--
-- /See:/ 'newGetModels' smart constructor.
data GetModels = GetModels'
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
-- Create a value of 'GetModels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getModels_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getModels_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'restApiId', 'getModels_restApiId' - [Required] The string identifier of the associated RestApi.
newGetModels ::
  -- | 'restApiId'
  Prelude.Text ->
  GetModels
newGetModels pRestApiId_ =
  GetModels'
    { position = Prelude.Nothing,
      limit = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | The current pagination position in the paged result set.
getModels_position :: Lens.Lens' GetModels (Prelude.Maybe Prelude.Text)
getModels_position = Lens.lens (\GetModels' {position} -> position) (\s@GetModels' {} a -> s {position = a} :: GetModels)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getModels_limit :: Lens.Lens' GetModels (Prelude.Maybe Prelude.Int)
getModels_limit = Lens.lens (\GetModels' {limit} -> limit) (\s@GetModels' {} a -> s {limit = a} :: GetModels)

-- | [Required] The string identifier of the associated RestApi.
getModels_restApiId :: Lens.Lens' GetModels Prelude.Text
getModels_restApiId = Lens.lens (\GetModels' {restApiId} -> restApiId) (\s@GetModels' {} a -> s {restApiId = a} :: GetModels)

instance Core.AWSPager GetModels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getModelsResponse_position Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getModelsResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getModels_position
          Lens..~ rs
          Lens.^? getModelsResponse_position Prelude.. Lens._Just

instance Core.AWSRequest GetModels where
  type AWSResponse GetModels = GetModelsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetModelsResponse'
            Prelude.<$> (x Core..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetModels

instance Prelude.NFData GetModels

instance Core.ToHeaders GetModels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetModels where
  toPath GetModels' {..} =
    Prelude.mconcat
      ["/restapis/", Core.toBS restApiId, "/models"]

instance Core.ToQuery GetModels where
  toQuery GetModels' {..} =
    Prelude.mconcat
      ["position" Core.=: position, "limit" Core.=: limit]

-- | Represents a collection of Model resources.
--
-- Method, MethodResponse,
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html Models and Mappings>
--
-- /See:/ 'newGetModelsResponse' smart constructor.
data GetModelsResponse = GetModelsResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [Model],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetModelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getModelsResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getModelsResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getModelsResponse_httpStatus' - The response's http status code.
newGetModelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetModelsResponse
newGetModelsResponse pHttpStatus_ =
  GetModelsResponse'
    { items = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getModelsResponse_items :: Lens.Lens' GetModelsResponse (Prelude.Maybe [Model])
getModelsResponse_items = Lens.lens (\GetModelsResponse' {items} -> items) (\s@GetModelsResponse' {} a -> s {items = a} :: GetModelsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getModelsResponse_position :: Lens.Lens' GetModelsResponse (Prelude.Maybe Prelude.Text)
getModelsResponse_position = Lens.lens (\GetModelsResponse' {position} -> position) (\s@GetModelsResponse' {} a -> s {position = a} :: GetModelsResponse)

-- | The response's http status code.
getModelsResponse_httpStatus :: Lens.Lens' GetModelsResponse Prelude.Int
getModelsResponse_httpStatus = Lens.lens (\GetModelsResponse' {httpStatus} -> httpStatus) (\s@GetModelsResponse' {} a -> s {httpStatus = a} :: GetModelsResponse)

instance Prelude.NFData GetModelsResponse
