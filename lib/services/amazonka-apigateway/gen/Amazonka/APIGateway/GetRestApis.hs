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
-- Module      : Amazonka.APIGateway.GetRestApis
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the RestApis resources for your collection.
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetRestApis
  ( -- * Creating a Request
    GetRestApis (..),
    newGetRestApis,

    -- * Request Lenses
    getRestApis_limit,
    getRestApis_position,

    -- * Destructuring the Response
    GetRestApisResponse (..),
    newGetRestApisResponse,

    -- * Response Lenses
    getRestApisResponse_items,
    getRestApisResponse_position,
    getRestApisResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The GET request to list existing RestApis defined for your collection.
--
-- /See:/ 'newGetRestApis' smart constructor.
data GetRestApis = GetRestApis'
  { -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRestApis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getRestApis_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'position', 'getRestApis_position' - The current pagination position in the paged result set.
newGetRestApis ::
  GetRestApis
newGetRestApis =
  GetRestApis'
    { limit = Prelude.Nothing,
      position = Prelude.Nothing
    }

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getRestApis_limit :: Lens.Lens' GetRestApis (Prelude.Maybe Prelude.Int)
getRestApis_limit = Lens.lens (\GetRestApis' {limit} -> limit) (\s@GetRestApis' {} a -> s {limit = a} :: GetRestApis)

-- | The current pagination position in the paged result set.
getRestApis_position :: Lens.Lens' GetRestApis (Prelude.Maybe Prelude.Text)
getRestApis_position = Lens.lens (\GetRestApis' {position} -> position) (\s@GetRestApis' {} a -> s {position = a} :: GetRestApis)

instance Core.AWSPager GetRestApis where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getRestApisResponse_position Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getRestApisResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getRestApis_position
          Lens..~ rs
          Lens.^? getRestApisResponse_position Prelude.. Lens._Just

instance Core.AWSRequest GetRestApis where
  type AWSResponse GetRestApis = GetRestApisResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRestApisResponse'
            Prelude.<$> (x Core..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRestApis where
  hashWithSalt _salt GetRestApis' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` position

instance Prelude.NFData GetRestApis where
  rnf GetRestApis' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf position

instance Core.ToHeaders GetRestApis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetRestApis where
  toPath = Prelude.const "/restapis"

instance Core.ToQuery GetRestApis where
  toQuery GetRestApis' {..} =
    Prelude.mconcat
      ["limit" Core.=: limit, "position" Core.=: position]

-- | Contains references to your APIs and links that guide you in how to
-- interact with your collection. A collection offers a paginated view of
-- your APIs.
--
-- /See:/ 'newGetRestApisResponse' smart constructor.
data GetRestApisResponse = GetRestApisResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [RestApi],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetRestApisResponse
newGetRestApisResponse pHttpStatus_ =
  GetRestApisResponse'
    { items = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getRestApisResponse_items :: Lens.Lens' GetRestApisResponse (Prelude.Maybe [RestApi])
getRestApisResponse_items = Lens.lens (\GetRestApisResponse' {items} -> items) (\s@GetRestApisResponse' {} a -> s {items = a} :: GetRestApisResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getRestApisResponse_position :: Lens.Lens' GetRestApisResponse (Prelude.Maybe Prelude.Text)
getRestApisResponse_position = Lens.lens (\GetRestApisResponse' {position} -> position) (\s@GetRestApisResponse' {} a -> s {position = a} :: GetRestApisResponse)

-- | The response's http status code.
getRestApisResponse_httpStatus :: Lens.Lens' GetRestApisResponse Prelude.Int
getRestApisResponse_httpStatus = Lens.lens (\GetRestApisResponse' {httpStatus} -> httpStatus) (\s@GetRestApisResponse' {} a -> s {httpStatus = a} :: GetRestApisResponse)

instance Prelude.NFData GetRestApisResponse where
  rnf GetRestApisResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf httpStatus
