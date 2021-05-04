{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Get the SdkTypes collection.
--
-- /See:/ 'newGetSdkTypes' smart constructor.
data GetSdkTypes = GetSdkTypes'
  { -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { position = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The current pagination position in the paged result set.
getSdkTypes_position :: Lens.Lens' GetSdkTypes (Prelude.Maybe Prelude.Text)
getSdkTypes_position = Lens.lens (\GetSdkTypes' {position} -> position) (\s@GetSdkTypes' {} a -> s {position = a} :: GetSdkTypes)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getSdkTypes_limit :: Lens.Lens' GetSdkTypes (Prelude.Maybe Prelude.Int)
getSdkTypes_limit = Lens.lens (\GetSdkTypes' {limit} -> limit) (\s@GetSdkTypes' {} a -> s {limit = a} :: GetSdkTypes)

instance Pager.AWSPager GetSdkTypes where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getSdkTypesResponse_position Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getSdkTypesResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getSdkTypes_position
          Lens..~ rs
          Lens.^? getSdkTypesResponse_position Prelude.. Lens._Just

instance Prelude.AWSRequest GetSdkTypes where
  type Rs GetSdkTypes = GetSdkTypesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSdkTypesResponse'
            Prelude.<$> (x Prelude..?> "item" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSdkTypes

instance Prelude.NFData GetSdkTypes

instance Prelude.ToHeaders GetSdkTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath GetSdkTypes where
  toPath = Prelude.const "/sdktypes"

instance Prelude.ToQuery GetSdkTypes where
  toQuery GetSdkTypes' {..} =
    Prelude.mconcat
      [ "position" Prelude.=: position,
        "limit" Prelude.=: limit
      ]

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
getSdkTypesResponse_items = Lens.lens (\GetSdkTypesResponse' {items} -> items) (\s@GetSdkTypesResponse' {} a -> s {items = a} :: GetSdkTypesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
getSdkTypesResponse_position :: Lens.Lens' GetSdkTypesResponse (Prelude.Maybe Prelude.Text)
getSdkTypesResponse_position = Lens.lens (\GetSdkTypesResponse' {position} -> position) (\s@GetSdkTypesResponse' {} a -> s {position = a} :: GetSdkTypesResponse)

-- | The response's http status code.
getSdkTypesResponse_httpStatus :: Lens.Lens' GetSdkTypesResponse Prelude.Int
getSdkTypesResponse_httpStatus = Lens.lens (\GetSdkTypesResponse' {httpStatus} -> httpStatus) (\s@GetSdkTypesResponse' {} a -> s {httpStatus = a} :: GetSdkTypesResponse)

instance Prelude.NFData GetSdkTypesResponse
