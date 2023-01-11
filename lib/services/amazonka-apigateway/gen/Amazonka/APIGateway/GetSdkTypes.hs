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
-- Module      : Amazonka.APIGateway.GetSdkTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets SDK types
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetSdkTypes
  ( -- * Creating a Request
    GetSdkTypes (..),
    newGetSdkTypes,

    -- * Request Lenses
    getSdkTypes_limit,
    getSdkTypes_position,

    -- * Destructuring the Response
    GetSdkTypesResponse (..),
    newGetSdkTypesResponse,

    -- * Response Lenses
    getSdkTypesResponse_items,
    getSdkTypesResponse_position,
    getSdkTypesResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Get the SdkTypes collection.
--
-- /See:/ 'newGetSdkTypes' smart constructor.
data GetSdkTypes = GetSdkTypes'
  { -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSdkTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getSdkTypes_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'position', 'getSdkTypes_position' - The current pagination position in the paged result set.
newGetSdkTypes ::
  GetSdkTypes
newGetSdkTypes =
  GetSdkTypes'
    { limit = Prelude.Nothing,
      position = Prelude.Nothing
    }

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getSdkTypes_limit :: Lens.Lens' GetSdkTypes (Prelude.Maybe Prelude.Int)
getSdkTypes_limit = Lens.lens (\GetSdkTypes' {limit} -> limit) (\s@GetSdkTypes' {} a -> s {limit = a} :: GetSdkTypes)

-- | The current pagination position in the paged result set.
getSdkTypes_position :: Lens.Lens' GetSdkTypes (Prelude.Maybe Prelude.Text)
getSdkTypes_position = Lens.lens (\GetSdkTypes' {position} -> position) (\s@GetSdkTypes' {} a -> s {position = a} :: GetSdkTypes)

instance Core.AWSPager GetSdkTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSdkTypesResponse_position Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getSdkTypesResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getSdkTypes_position
          Lens..~ rs
          Lens.^? getSdkTypesResponse_position Prelude.. Lens._Just

instance Core.AWSRequest GetSdkTypes where
  type AWSResponse GetSdkTypes = GetSdkTypesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSdkTypesResponse'
            Prelude.<$> (x Data..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSdkTypes where
  hashWithSalt _salt GetSdkTypes' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` position

instance Prelude.NFData GetSdkTypes where
  rnf GetSdkTypes' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf position

instance Data.ToHeaders GetSdkTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetSdkTypes where
  toPath = Prelude.const "/sdktypes"

instance Data.ToQuery GetSdkTypes where
  toQuery GetSdkTypes' {..} =
    Prelude.mconcat
      ["limit" Data.=: limit, "position" Data.=: position]

-- | The collection of SdkType instances.
--
-- /See:/ 'newGetSdkTypesResponse' smart constructor.
data GetSdkTypesResponse = GetSdkTypesResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [SdkType],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetSdkTypesResponse
newGetSdkTypesResponse pHttpStatus_ =
  GetSdkTypesResponse'
    { items = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getSdkTypesResponse_items :: Lens.Lens' GetSdkTypesResponse (Prelude.Maybe [SdkType])
getSdkTypesResponse_items = Lens.lens (\GetSdkTypesResponse' {items} -> items) (\s@GetSdkTypesResponse' {} a -> s {items = a} :: GetSdkTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getSdkTypesResponse_position :: Lens.Lens' GetSdkTypesResponse (Prelude.Maybe Prelude.Text)
getSdkTypesResponse_position = Lens.lens (\GetSdkTypesResponse' {position} -> position) (\s@GetSdkTypesResponse' {} a -> s {position = a} :: GetSdkTypesResponse)

-- | The response's http status code.
getSdkTypesResponse_httpStatus :: Lens.Lens' GetSdkTypesResponse Prelude.Int
getSdkTypesResponse_httpStatus = Lens.lens (\GetSdkTypesResponse' {httpStatus} -> httpStatus) (\s@GetSdkTypesResponse' {} a -> s {httpStatus = a} :: GetSdkTypesResponse)

instance Prelude.NFData GetSdkTypesResponse where
  rnf GetSdkTypesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf httpStatus
