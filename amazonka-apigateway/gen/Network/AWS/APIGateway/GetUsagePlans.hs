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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to get all the usage plans of the caller\'s account.
--
-- /See:/ 'newGetUsagePlans' smart constructor.
data GetUsagePlans = GetUsagePlans'
  { -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the API key associated with the usage plans.
    keyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { position = Prelude.Nothing,
      limit = Prelude.Nothing,
      keyId = Prelude.Nothing
    }

-- | The current pagination position in the paged result set.
getUsagePlans_position :: Lens.Lens' GetUsagePlans (Prelude.Maybe Prelude.Text)
getUsagePlans_position = Lens.lens (\GetUsagePlans' {position} -> position) (\s@GetUsagePlans' {} a -> s {position = a} :: GetUsagePlans)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getUsagePlans_limit :: Lens.Lens' GetUsagePlans (Prelude.Maybe Prelude.Int)
getUsagePlans_limit = Lens.lens (\GetUsagePlans' {limit} -> limit) (\s@GetUsagePlans' {} a -> s {limit = a} :: GetUsagePlans)

-- | The identifier of the API key associated with the usage plans.
getUsagePlans_keyId :: Lens.Lens' GetUsagePlans (Prelude.Maybe Prelude.Text)
getUsagePlans_keyId = Lens.lens (\GetUsagePlans' {keyId} -> keyId) (\s@GetUsagePlans' {} a -> s {keyId = a} :: GetUsagePlans)

instance Core.AWSPager GetUsagePlans where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getUsagePlansResponse_position Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getUsagePlansResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getUsagePlans_position
          Lens..~ rs
          Lens.^? getUsagePlansResponse_position Prelude.. Lens._Just

instance Core.AWSRequest GetUsagePlans where
  type
    AWSResponse GetUsagePlans =
      GetUsagePlansResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUsagePlansResponse'
            Prelude.<$> (x Core..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUsagePlans

instance Prelude.NFData GetUsagePlans

instance Core.ToHeaders GetUsagePlans where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetUsagePlans where
  toPath = Prelude.const "/usageplans"

instance Core.ToQuery GetUsagePlans where
  toQuery GetUsagePlans' {..} =
    Prelude.mconcat
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
    items :: Prelude.Maybe [UsagePlan],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetUsagePlansResponse
newGetUsagePlansResponse pHttpStatus_ =
  GetUsagePlansResponse'
    { items = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getUsagePlansResponse_items :: Lens.Lens' GetUsagePlansResponse (Prelude.Maybe [UsagePlan])
getUsagePlansResponse_items = Lens.lens (\GetUsagePlansResponse' {items} -> items) (\s@GetUsagePlansResponse' {} a -> s {items = a} :: GetUsagePlansResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getUsagePlansResponse_position :: Lens.Lens' GetUsagePlansResponse (Prelude.Maybe Prelude.Text)
getUsagePlansResponse_position = Lens.lens (\GetUsagePlansResponse' {position} -> position) (\s@GetUsagePlansResponse' {} a -> s {position = a} :: GetUsagePlansResponse)

-- | The response's http status code.
getUsagePlansResponse_httpStatus :: Lens.Lens' GetUsagePlansResponse Prelude.Int
getUsagePlansResponse_httpStatus = Lens.lens (\GetUsagePlansResponse' {httpStatus} -> httpStatus) (\s@GetUsagePlansResponse' {} a -> s {httpStatus = a} :: GetUsagePlansResponse)

instance Prelude.NFData GetUsagePlansResponse
