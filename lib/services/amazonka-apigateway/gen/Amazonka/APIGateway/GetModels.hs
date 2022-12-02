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
-- Module      : Amazonka.APIGateway.GetModels
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes existing Models defined for a RestApi resource.
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetModels
  ( -- * Creating a Request
    GetModels (..),
    newGetModels,

    -- * Request Lenses
    getModels_limit,
    getModels_position,
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to list existing Models defined for a RestApi resource.
--
-- /See:/ 'newGetModels' smart constructor.
data GetModels = GetModels'
  { -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | The string identifier of the associated RestApi.
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
-- 'limit', 'getModels_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'position', 'getModels_position' - The current pagination position in the paged result set.
--
-- 'restApiId', 'getModels_restApiId' - The string identifier of the associated RestApi.
newGetModels ::
  -- | 'restApiId'
  Prelude.Text ->
  GetModels
newGetModels pRestApiId_ =
  GetModels'
    { limit = Prelude.Nothing,
      position = Prelude.Nothing,
      restApiId = pRestApiId_
    }

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getModels_limit :: Lens.Lens' GetModels (Prelude.Maybe Prelude.Int)
getModels_limit = Lens.lens (\GetModels' {limit} -> limit) (\s@GetModels' {} a -> s {limit = a} :: GetModels)

-- | The current pagination position in the paged result set.
getModels_position :: Lens.Lens' GetModels (Prelude.Maybe Prelude.Text)
getModels_position = Lens.lens (\GetModels' {position} -> position) (\s@GetModels' {} a -> s {position = a} :: GetModels)

-- | The string identifier of the associated RestApi.
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetModelsResponse'
            Prelude.<$> (x Data..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetModels where
  hashWithSalt _salt GetModels' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` restApiId

instance Prelude.NFData GetModels where
  rnf GetModels' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf restApiId

instance Data.ToHeaders GetModels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetModels where
  toPath GetModels' {..} =
    Prelude.mconcat
      ["/restapis/", Data.toBS restApiId, "/models"]

instance Data.ToQuery GetModels where
  toQuery GetModels' {..} =
    Prelude.mconcat
      ["limit" Data.=: limit, "position" Data.=: position]

-- | Represents a collection of Model resources.
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
getModelsResponse_items = Lens.lens (\GetModelsResponse' {items} -> items) (\s@GetModelsResponse' {} a -> s {items = a} :: GetModelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getModelsResponse_position :: Lens.Lens' GetModelsResponse (Prelude.Maybe Prelude.Text)
getModelsResponse_position = Lens.lens (\GetModelsResponse' {position} -> position) (\s@GetModelsResponse' {} a -> s {position = a} :: GetModelsResponse)

-- | The response's http status code.
getModelsResponse_httpStatus :: Lens.Lens' GetModelsResponse Prelude.Int
getModelsResponse_httpStatus = Lens.lens (\GetModelsResponse' {httpStatus} -> httpStatus) (\s@GetModelsResponse' {} a -> s {httpStatus = a} :: GetModelsResponse)

instance Prelude.NFData GetModelsResponse where
  rnf GetModelsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf httpStatus
