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
-- Module      : Network.AWS.APIGateway.GetUsagePlans
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the usage plans of the caller\'s account.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetUsagePlans
  ( -- * Creating a Request
    GetUsagePlans (..),
    newGetUsagePlans,

    -- * Request Lenses
    getUsagePlans_position,
    getUsagePlans_limit,
    getUsagePlans_keyId,

    -- * Destructuring the Response
    GetUsagePlansResponse (..),
    newGetUsagePlansResponse,

    -- * Response Lenses
    getUsagePlansResponse_items,
    getUsagePlansResponse_position,
    getUsagePlansResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to get all the usage plans of the caller\'s account.
--
-- /See:/ 'newGetUsagePlans' smart constructor.
data GetUsagePlans = GetUsagePlans'
  { -- | The current pagination position in the paged result set.
    position :: Core.Maybe Core.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Core.Maybe Core.Int,
    -- | The identifier of the API key associated with the usage plans.
    keyId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUsagePlans' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getUsagePlans_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getUsagePlans_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'keyId', 'getUsagePlans_keyId' - The identifier of the API key associated with the usage plans.
newGetUsagePlans ::
  GetUsagePlans
newGetUsagePlans =
  GetUsagePlans'
    { position = Core.Nothing,
      limit = Core.Nothing,
      keyId = Core.Nothing
    }

-- | The current pagination position in the paged result set.
getUsagePlans_position :: Lens.Lens' GetUsagePlans (Core.Maybe Core.Text)
getUsagePlans_position = Lens.lens (\GetUsagePlans' {position} -> position) (\s@GetUsagePlans' {} a -> s {position = a} :: GetUsagePlans)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getUsagePlans_limit :: Lens.Lens' GetUsagePlans (Core.Maybe Core.Int)
getUsagePlans_limit = Lens.lens (\GetUsagePlans' {limit} -> limit) (\s@GetUsagePlans' {} a -> s {limit = a} :: GetUsagePlans)

-- | The identifier of the API key associated with the usage plans.
getUsagePlans_keyId :: Lens.Lens' GetUsagePlans (Core.Maybe Core.Text)
getUsagePlans_keyId = Lens.lens (\GetUsagePlans' {keyId} -> keyId) (\s@GetUsagePlans' {} a -> s {keyId = a} :: GetUsagePlans)

instance Core.AWSPager GetUsagePlans where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getUsagePlansResponse_position Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getUsagePlansResponse_items Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getUsagePlans_position
          Lens..~ rs
          Lens.^? getUsagePlansResponse_position Core.. Lens._Just

instance Core.AWSRequest GetUsagePlans where
  type
    AWSResponse GetUsagePlans =
      GetUsagePlansResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUsagePlansResponse'
            Core.<$> (x Core..?> "item" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "position")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetUsagePlans

instance Core.NFData GetUsagePlans

instance Core.ToHeaders GetUsagePlans where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetUsagePlans where
  toPath = Core.const "/usageplans"

instance Core.ToQuery GetUsagePlans where
  toQuery GetUsagePlans' {..} =
    Core.mconcat
      [ "position" Core.=: position,
        "limit" Core.=: limit,
        "keyId" Core.=: keyId
      ]

-- | Represents a collection of usage plans for an AWS account.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>
--
-- /See:/ 'newGetUsagePlansResponse' smart constructor.
data GetUsagePlansResponse = GetUsagePlansResponse'
  { -- | The current page of elements from this collection.
    items :: Core.Maybe [UsagePlan],
    position :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUsagePlansResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getUsagePlansResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getUsagePlansResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getUsagePlansResponse_httpStatus' - The response's http status code.
newGetUsagePlansResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetUsagePlansResponse
newGetUsagePlansResponse pHttpStatus_ =
  GetUsagePlansResponse'
    { items = Core.Nothing,
      position = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getUsagePlansResponse_items :: Lens.Lens' GetUsagePlansResponse (Core.Maybe [UsagePlan])
getUsagePlansResponse_items = Lens.lens (\GetUsagePlansResponse' {items} -> items) (\s@GetUsagePlansResponse' {} a -> s {items = a} :: GetUsagePlansResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getUsagePlansResponse_position :: Lens.Lens' GetUsagePlansResponse (Core.Maybe Core.Text)
getUsagePlansResponse_position = Lens.lens (\GetUsagePlansResponse' {position} -> position) (\s@GetUsagePlansResponse' {} a -> s {position = a} :: GetUsagePlansResponse)

-- | The response's http status code.
getUsagePlansResponse_httpStatus :: Lens.Lens' GetUsagePlansResponse Core.Int
getUsagePlansResponse_httpStatus = Lens.lens (\GetUsagePlansResponse' {httpStatus} -> httpStatus) (\s@GetUsagePlansResponse' {} a -> s {httpStatus = a} :: GetUsagePlansResponse)

instance Core.NFData GetUsagePlansResponse
