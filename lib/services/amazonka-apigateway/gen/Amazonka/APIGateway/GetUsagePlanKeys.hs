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
-- Module      : Amazonka.APIGateway.GetUsagePlanKeys
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the usage plan keys representing the API keys added to a
-- specified usage plan.
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetUsagePlanKeys
  ( -- * Creating a Request
    GetUsagePlanKeys (..),
    newGetUsagePlanKeys,

    -- * Request Lenses
    getUsagePlanKeys_limit,
    getUsagePlanKeys_nameQuery,
    getUsagePlanKeys_position,
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The GET request to get all the usage plan keys representing the API keys
-- added to a specified usage plan.
--
-- /See:/ 'newGetUsagePlanKeys' smart constructor.
data GetUsagePlanKeys = GetUsagePlanKeys'
  { -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | A query parameter specifying the name of the to-be-returned usage plan
    -- keys.
    nameQuery :: Prelude.Maybe Prelude.Text,
    -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | The Id of the UsagePlan resource representing the usage plan containing
    -- the to-be-retrieved UsagePlanKey resource representing a plan customer.
    usagePlanId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUsagePlanKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getUsagePlanKeys_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'nameQuery', 'getUsagePlanKeys_nameQuery' - A query parameter specifying the name of the to-be-returned usage plan
-- keys.
--
-- 'position', 'getUsagePlanKeys_position' - The current pagination position in the paged result set.
--
-- 'usagePlanId', 'getUsagePlanKeys_usagePlanId' - The Id of the UsagePlan resource representing the usage plan containing
-- the to-be-retrieved UsagePlanKey resource representing a plan customer.
newGetUsagePlanKeys ::
  -- | 'usagePlanId'
  Prelude.Text ->
  GetUsagePlanKeys
newGetUsagePlanKeys pUsagePlanId_ =
  GetUsagePlanKeys'
    { limit = Prelude.Nothing,
      nameQuery = Prelude.Nothing,
      position = Prelude.Nothing,
      usagePlanId = pUsagePlanId_
    }

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getUsagePlanKeys_limit :: Lens.Lens' GetUsagePlanKeys (Prelude.Maybe Prelude.Int)
getUsagePlanKeys_limit = Lens.lens (\GetUsagePlanKeys' {limit} -> limit) (\s@GetUsagePlanKeys' {} a -> s {limit = a} :: GetUsagePlanKeys)

-- | A query parameter specifying the name of the to-be-returned usage plan
-- keys.
getUsagePlanKeys_nameQuery :: Lens.Lens' GetUsagePlanKeys (Prelude.Maybe Prelude.Text)
getUsagePlanKeys_nameQuery = Lens.lens (\GetUsagePlanKeys' {nameQuery} -> nameQuery) (\s@GetUsagePlanKeys' {} a -> s {nameQuery = a} :: GetUsagePlanKeys)

-- | The current pagination position in the paged result set.
getUsagePlanKeys_position :: Lens.Lens' GetUsagePlanKeys (Prelude.Maybe Prelude.Text)
getUsagePlanKeys_position = Lens.lens (\GetUsagePlanKeys' {position} -> position) (\s@GetUsagePlanKeys' {} a -> s {position = a} :: GetUsagePlanKeys)

-- | The Id of the UsagePlan resource representing the usage plan containing
-- the to-be-retrieved UsagePlanKey resource representing a plan customer.
getUsagePlanKeys_usagePlanId :: Lens.Lens' GetUsagePlanKeys Prelude.Text
getUsagePlanKeys_usagePlanId = Lens.lens (\GetUsagePlanKeys' {usagePlanId} -> usagePlanId) (\s@GetUsagePlanKeys' {} a -> s {usagePlanId = a} :: GetUsagePlanKeys)

instance Core.AWSPager GetUsagePlanKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getUsagePlanKeysResponse_position
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getUsagePlanKeysResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getUsagePlanKeys_position
          Lens..~ rs
          Lens.^? getUsagePlanKeysResponse_position
            Prelude.. Lens._Just

instance Core.AWSRequest GetUsagePlanKeys where
  type
    AWSResponse GetUsagePlanKeys =
      GetUsagePlanKeysResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUsagePlanKeysResponse'
            Prelude.<$> (x Core..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUsagePlanKeys where
  hashWithSalt _salt GetUsagePlanKeys' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nameQuery
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` usagePlanId

instance Prelude.NFData GetUsagePlanKeys where
  rnf GetUsagePlanKeys' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nameQuery
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf usagePlanId

instance Core.ToHeaders GetUsagePlanKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetUsagePlanKeys where
  toPath GetUsagePlanKeys' {..} =
    Prelude.mconcat
      ["/usageplans/", Core.toBS usagePlanId, "/keys"]

instance Core.ToQuery GetUsagePlanKeys where
  toQuery GetUsagePlanKeys' {..} =
    Prelude.mconcat
      [ "limit" Core.=: limit,
        "name" Core.=: nameQuery,
        "position" Core.=: position
      ]

-- | Represents the collection of usage plan keys added to usage plans for
-- the associated API keys and, possibly, other types of keys.
--
-- /See:/ 'newGetUsagePlanKeysResponse' smart constructor.
data GetUsagePlanKeysResponse = GetUsagePlanKeysResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [UsagePlanKey],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetUsagePlanKeysResponse
newGetUsagePlanKeysResponse pHttpStatus_ =
  GetUsagePlanKeysResponse'
    { items = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getUsagePlanKeysResponse_items :: Lens.Lens' GetUsagePlanKeysResponse (Prelude.Maybe [UsagePlanKey])
getUsagePlanKeysResponse_items = Lens.lens (\GetUsagePlanKeysResponse' {items} -> items) (\s@GetUsagePlanKeysResponse' {} a -> s {items = a} :: GetUsagePlanKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getUsagePlanKeysResponse_position :: Lens.Lens' GetUsagePlanKeysResponse (Prelude.Maybe Prelude.Text)
getUsagePlanKeysResponse_position = Lens.lens (\GetUsagePlanKeysResponse' {position} -> position) (\s@GetUsagePlanKeysResponse' {} a -> s {position = a} :: GetUsagePlanKeysResponse)

-- | The response's http status code.
getUsagePlanKeysResponse_httpStatus :: Lens.Lens' GetUsagePlanKeysResponse Prelude.Int
getUsagePlanKeysResponse_httpStatus = Lens.lens (\GetUsagePlanKeysResponse' {httpStatus} -> httpStatus) (\s@GetUsagePlanKeysResponse' {} a -> s {httpStatus = a} :: GetUsagePlanKeysResponse)

instance Prelude.NFData GetUsagePlanKeysResponse where
  rnf GetUsagePlanKeysResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf httpStatus
