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
-- Module      : Network.AWS.APIGateway.GetSdkTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetSdkTypes
  ( -- * Creating a Request
    GetSdkTypes (..),
    newGetSdkTypes,

    -- * Request Lenses
    getSdkTypes_position,
    getSdkTypes_limit,

    -- * Destructuring the Response
    GetSdkTypesResponse (..),
    newGetSdkTypesResponse,

    -- * Response Lenses
    getSdkTypesResponse_items,
    getSdkTypesResponse_position,
    getSdkTypesResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Get the SdkTypes collection.
--
-- /See:/ 'newGetSdkTypes' smart constructor.
data GetSdkTypes = GetSdkTypes'
  { -- | The current pagination position in the paged result set.
    position :: Core.Maybe Core.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSdkTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getSdkTypes_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getSdkTypes_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
newGetSdkTypes ::
  GetSdkTypes
newGetSdkTypes =
  GetSdkTypes'
    { position = Core.Nothing,
      limit = Core.Nothing
    }

-- | The current pagination position in the paged result set.
getSdkTypes_position :: Lens.Lens' GetSdkTypes (Core.Maybe Core.Text)
getSdkTypes_position = Lens.lens (\GetSdkTypes' {position} -> position) (\s@GetSdkTypes' {} a -> s {position = a} :: GetSdkTypes)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getSdkTypes_limit :: Lens.Lens' GetSdkTypes (Core.Maybe Core.Int)
getSdkTypes_limit = Lens.lens (\GetSdkTypes' {limit} -> limit) (\s@GetSdkTypes' {} a -> s {limit = a} :: GetSdkTypes)

instance Core.AWSPager GetSdkTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSdkTypesResponse_position Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getSdkTypesResponse_items Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getSdkTypes_position
          Lens..~ rs
          Lens.^? getSdkTypesResponse_position Core.. Lens._Just

instance Core.AWSRequest GetSdkTypes where
  type AWSResponse GetSdkTypes = GetSdkTypesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSdkTypesResponse'
            Core.<$> (x Core..?> "item" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "position")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSdkTypes

instance Core.NFData GetSdkTypes

instance Core.ToHeaders GetSdkTypes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetSdkTypes where
  toPath = Core.const "/sdktypes"

instance Core.ToQuery GetSdkTypes where
  toQuery GetSdkTypes' {..} =
    Core.mconcat
      ["position" Core.=: position, "limit" Core.=: limit]

-- | The collection of SdkType instances.
--
-- /See:/ 'newGetSdkTypesResponse' smart constructor.
data GetSdkTypesResponse = GetSdkTypesResponse'
  { -- | The current page of elements from this collection.
    items :: Core.Maybe [SdkType],
    position :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSdkTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getSdkTypesResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getSdkTypesResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getSdkTypesResponse_httpStatus' - The response's http status code.
newGetSdkTypesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSdkTypesResponse
newGetSdkTypesResponse pHttpStatus_ =
  GetSdkTypesResponse'
    { items = Core.Nothing,
      position = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getSdkTypesResponse_items :: Lens.Lens' GetSdkTypesResponse (Core.Maybe [SdkType])
getSdkTypesResponse_items = Lens.lens (\GetSdkTypesResponse' {items} -> items) (\s@GetSdkTypesResponse' {} a -> s {items = a} :: GetSdkTypesResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getSdkTypesResponse_position :: Lens.Lens' GetSdkTypesResponse (Core.Maybe Core.Text)
getSdkTypesResponse_position = Lens.lens (\GetSdkTypesResponse' {position} -> position) (\s@GetSdkTypesResponse' {} a -> s {position = a} :: GetSdkTypesResponse)

-- | The response's http status code.
getSdkTypesResponse_httpStatus :: Lens.Lens' GetSdkTypesResponse Core.Int
getSdkTypesResponse_httpStatus = Lens.lens (\GetSdkTypesResponse' {httpStatus} -> httpStatus) (\s@GetSdkTypesResponse' {} a -> s {httpStatus = a} :: GetSdkTypesResponse)

instance Core.NFData GetSdkTypesResponse
