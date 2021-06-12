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
-- Module      : Network.AWS.APIGateway.GetUsagePlanKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the usage plan keys representing the API keys added to a
-- specified usage plan.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetUsagePlanKeys
  ( -- * Creating a Request
    GetUsagePlanKeys (..),
    newGetUsagePlanKeys,

    -- * Request Lenses
    getUsagePlanKeys_position,
    getUsagePlanKeys_limit,
    getUsagePlanKeys_nameQuery,
    getUsagePlanKeys_usagePlanId,

    -- * Destructuring the Response
    GetUsagePlanKeysResponse (..),
    newGetUsagePlanKeysResponse,

    -- * Response Lenses
    getUsagePlanKeysResponse_items,
    getUsagePlanKeysResponse_position,
    getUsagePlanKeysResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to get all the usage plan keys representing the API keys
-- added to a specified usage plan.
--
-- /See:/ 'newGetUsagePlanKeys' smart constructor.
data GetUsagePlanKeys = GetUsagePlanKeys'
  { -- | The current pagination position in the paged result set.
    position :: Core.Maybe Core.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Core.Maybe Core.Int,
    -- | A query parameter specifying the name of the to-be-returned usage plan
    -- keys.
    nameQuery :: Core.Maybe Core.Text,
    -- | [Required] The Id of the UsagePlan resource representing the usage plan
    -- containing the to-be-retrieved UsagePlanKey resource representing a plan
    -- customer.
    usagePlanId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUsagePlanKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getUsagePlanKeys_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getUsagePlanKeys_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'nameQuery', 'getUsagePlanKeys_nameQuery' - A query parameter specifying the name of the to-be-returned usage plan
-- keys.
--
-- 'usagePlanId', 'getUsagePlanKeys_usagePlanId' - [Required] The Id of the UsagePlan resource representing the usage plan
-- containing the to-be-retrieved UsagePlanKey resource representing a plan
-- customer.
newGetUsagePlanKeys ::
  -- | 'usagePlanId'
  Core.Text ->
  GetUsagePlanKeys
newGetUsagePlanKeys pUsagePlanId_ =
  GetUsagePlanKeys'
    { position = Core.Nothing,
      limit = Core.Nothing,
      nameQuery = Core.Nothing,
      usagePlanId = pUsagePlanId_
    }

-- | The current pagination position in the paged result set.
getUsagePlanKeys_position :: Lens.Lens' GetUsagePlanKeys (Core.Maybe Core.Text)
getUsagePlanKeys_position = Lens.lens (\GetUsagePlanKeys' {position} -> position) (\s@GetUsagePlanKeys' {} a -> s {position = a} :: GetUsagePlanKeys)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getUsagePlanKeys_limit :: Lens.Lens' GetUsagePlanKeys (Core.Maybe Core.Int)
getUsagePlanKeys_limit = Lens.lens (\GetUsagePlanKeys' {limit} -> limit) (\s@GetUsagePlanKeys' {} a -> s {limit = a} :: GetUsagePlanKeys)

-- | A query parameter specifying the name of the to-be-returned usage plan
-- keys.
getUsagePlanKeys_nameQuery :: Lens.Lens' GetUsagePlanKeys (Core.Maybe Core.Text)
getUsagePlanKeys_nameQuery = Lens.lens (\GetUsagePlanKeys' {nameQuery} -> nameQuery) (\s@GetUsagePlanKeys' {} a -> s {nameQuery = a} :: GetUsagePlanKeys)

-- | [Required] The Id of the UsagePlan resource representing the usage plan
-- containing the to-be-retrieved UsagePlanKey resource representing a plan
-- customer.
getUsagePlanKeys_usagePlanId :: Lens.Lens' GetUsagePlanKeys Core.Text
getUsagePlanKeys_usagePlanId = Lens.lens (\GetUsagePlanKeys' {usagePlanId} -> usagePlanId) (\s@GetUsagePlanKeys' {} a -> s {usagePlanId = a} :: GetUsagePlanKeys)

instance Core.AWSPager GetUsagePlanKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getUsagePlanKeysResponse_position Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getUsagePlanKeysResponse_items Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getUsagePlanKeys_position
          Lens..~ rs
          Lens.^? getUsagePlanKeysResponse_position Core.. Lens._Just

instance Core.AWSRequest GetUsagePlanKeys where
  type
    AWSResponse GetUsagePlanKeys =
      GetUsagePlanKeysResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUsagePlanKeysResponse'
            Core.<$> (x Core..?> "item" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "position")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetUsagePlanKeys

instance Core.NFData GetUsagePlanKeys

instance Core.ToHeaders GetUsagePlanKeys where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetUsagePlanKeys where
  toPath GetUsagePlanKeys' {..} =
    Core.mconcat
      ["/usageplans/", Core.toBS usagePlanId, "/keys"]

instance Core.ToQuery GetUsagePlanKeys where
  toQuery GetUsagePlanKeys' {..} =
    Core.mconcat
      [ "position" Core.=: position,
        "limit" Core.=: limit,
        "name" Core.=: nameQuery
      ]

-- | Represents the collection of usage plan keys added to usage plans for
-- the associated API keys and, possibly, other types of keys.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>
--
-- /See:/ 'newGetUsagePlanKeysResponse' smart constructor.
data GetUsagePlanKeysResponse = GetUsagePlanKeysResponse'
  { -- | The current page of elements from this collection.
    items :: Core.Maybe [UsagePlanKey],
    position :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUsagePlanKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getUsagePlanKeysResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getUsagePlanKeysResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getUsagePlanKeysResponse_httpStatus' - The response's http status code.
newGetUsagePlanKeysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetUsagePlanKeysResponse
newGetUsagePlanKeysResponse pHttpStatus_ =
  GetUsagePlanKeysResponse'
    { items = Core.Nothing,
      position = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getUsagePlanKeysResponse_items :: Lens.Lens' GetUsagePlanKeysResponse (Core.Maybe [UsagePlanKey])
getUsagePlanKeysResponse_items = Lens.lens (\GetUsagePlanKeysResponse' {items} -> items) (\s@GetUsagePlanKeysResponse' {} a -> s {items = a} :: GetUsagePlanKeysResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getUsagePlanKeysResponse_position :: Lens.Lens' GetUsagePlanKeysResponse (Core.Maybe Core.Text)
getUsagePlanKeysResponse_position = Lens.lens (\GetUsagePlanKeysResponse' {position} -> position) (\s@GetUsagePlanKeysResponse' {} a -> s {position = a} :: GetUsagePlanKeysResponse)

-- | The response's http status code.
getUsagePlanKeysResponse_httpStatus :: Lens.Lens' GetUsagePlanKeysResponse Core.Int
getUsagePlanKeysResponse_httpStatus = Lens.lens (\GetUsagePlanKeysResponse' {httpStatus} -> httpStatus) (\s@GetUsagePlanKeysResponse' {} a -> s {httpStatus = a} :: GetUsagePlanKeysResponse)

instance Core.NFData GetUsagePlanKeysResponse
