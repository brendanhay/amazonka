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
-- Module      : Network.AWS.APIGateway.GetRestApis
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the RestApis resources for your collection.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetRestApis
  ( -- * Creating a Request
    GetRestApis (..),
    newGetRestApis,

    -- * Request Lenses
    getRestApis_position,
    getRestApis_limit,

    -- * Destructuring the Response
    GetRestApisResponse (..),
    newGetRestApisResponse,

    -- * Response Lenses
    getRestApisResponse_items,
    getRestApisResponse_position,
    getRestApisResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to list existing RestApis defined for your collection.
--
-- /See:/ 'newGetRestApis' smart constructor.
data GetRestApis = GetRestApis'
  { -- | The current pagination position in the paged result set.
    position :: Core.Maybe Core.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRestApis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getRestApis_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getRestApis_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
newGetRestApis ::
  GetRestApis
newGetRestApis =
  GetRestApis'
    { position = Core.Nothing,
      limit = Core.Nothing
    }

-- | The current pagination position in the paged result set.
getRestApis_position :: Lens.Lens' GetRestApis (Core.Maybe Core.Text)
getRestApis_position = Lens.lens (\GetRestApis' {position} -> position) (\s@GetRestApis' {} a -> s {position = a} :: GetRestApis)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getRestApis_limit :: Lens.Lens' GetRestApis (Core.Maybe Core.Int)
getRestApis_limit = Lens.lens (\GetRestApis' {limit} -> limit) (\s@GetRestApis' {} a -> s {limit = a} :: GetRestApis)

instance Core.AWSPager GetRestApis where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRestApisResponse_position Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getRestApisResponse_items Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getRestApis_position
          Lens..~ rs
          Lens.^? getRestApisResponse_position Core.. Lens._Just

instance Core.AWSRequest GetRestApis where
  type AWSResponse GetRestApis = GetRestApisResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRestApisResponse'
            Core.<$> (x Core..?> "item" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "position")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRestApis

instance Core.NFData GetRestApis

instance Core.ToHeaders GetRestApis where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetRestApis where
  toPath = Core.const "/restapis"

instance Core.ToQuery GetRestApis where
  toQuery GetRestApis' {..} =
    Core.mconcat
      ["position" Core.=: position, "limit" Core.=: limit]

-- | Contains references to your APIs and links that guide you in how to
-- interact with your collection. A collection offers a paginated view of
-- your APIs.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Create an API>
--
-- /See:/ 'newGetRestApisResponse' smart constructor.
data GetRestApisResponse = GetRestApisResponse'
  { -- | The current page of elements from this collection.
    items :: Core.Maybe [RestApi],
    position :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRestApisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getRestApisResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getRestApisResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getRestApisResponse_httpStatus' - The response's http status code.
newGetRestApisResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRestApisResponse
newGetRestApisResponse pHttpStatus_ =
  GetRestApisResponse'
    { items = Core.Nothing,
      position = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getRestApisResponse_items :: Lens.Lens' GetRestApisResponse (Core.Maybe [RestApi])
getRestApisResponse_items = Lens.lens (\GetRestApisResponse' {items} -> items) (\s@GetRestApisResponse' {} a -> s {items = a} :: GetRestApisResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getRestApisResponse_position :: Lens.Lens' GetRestApisResponse (Core.Maybe Core.Text)
getRestApisResponse_position = Lens.lens (\GetRestApisResponse' {position} -> position) (\s@GetRestApisResponse' {} a -> s {position = a} :: GetRestApisResponse)

-- | The response's http status code.
getRestApisResponse_httpStatus :: Lens.Lens' GetRestApisResponse Core.Int
getRestApisResponse_httpStatus = Lens.lens (\GetRestApisResponse' {httpStatus} -> httpStatus) (\s@GetRestApisResponse' {} a -> s {httpStatus = a} :: GetRestApisResponse)

instance Core.NFData GetRestApisResponse
