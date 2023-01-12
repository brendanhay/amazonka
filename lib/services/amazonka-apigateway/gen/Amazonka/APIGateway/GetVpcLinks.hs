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
-- Module      : Amazonka.APIGateway.GetVpcLinks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the VpcLinks collection under the caller\'s account in a selected
-- region.
--
-- This operation returns paginated results.
module Amazonka.APIGateway.GetVpcLinks
  ( -- * Creating a Request
    GetVpcLinks (..),
    newGetVpcLinks,

    -- * Request Lenses
    getVpcLinks_limit,
    getVpcLinks_position,

    -- * Destructuring the Response
    GetVpcLinksResponse (..),
    newGetVpcLinksResponse,

    -- * Response Lenses
    getVpcLinksResponse_items,
    getVpcLinksResponse_position,
    getVpcLinksResponse_httpStatus,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Gets the VpcLinks collection under the caller\'s account in a selected
-- region.
--
-- /See:/ 'newGetVpcLinks' smart constructor.
data GetVpcLinks = GetVpcLinks'
  { -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpcLinks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getVpcLinks_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'position', 'getVpcLinks_position' - The current pagination position in the paged result set.
newGetVpcLinks ::
  GetVpcLinks
newGetVpcLinks =
  GetVpcLinks'
    { limit = Prelude.Nothing,
      position = Prelude.Nothing
    }

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getVpcLinks_limit :: Lens.Lens' GetVpcLinks (Prelude.Maybe Prelude.Int)
getVpcLinks_limit = Lens.lens (\GetVpcLinks' {limit} -> limit) (\s@GetVpcLinks' {} a -> s {limit = a} :: GetVpcLinks)

-- | The current pagination position in the paged result set.
getVpcLinks_position :: Lens.Lens' GetVpcLinks (Prelude.Maybe Prelude.Text)
getVpcLinks_position = Lens.lens (\GetVpcLinks' {position} -> position) (\s@GetVpcLinks' {} a -> s {position = a} :: GetVpcLinks)

instance Core.AWSPager GetVpcLinks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getVpcLinksResponse_position Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getVpcLinksResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getVpcLinks_position
          Lens..~ rs
          Lens.^? getVpcLinksResponse_position Prelude.. Lens._Just

instance Core.AWSRequest GetVpcLinks where
  type AWSResponse GetVpcLinks = GetVpcLinksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVpcLinksResponse'
            Prelude.<$> (x Data..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVpcLinks where
  hashWithSalt _salt GetVpcLinks' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` position

instance Prelude.NFData GetVpcLinks where
  rnf GetVpcLinks' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf position

instance Data.ToHeaders GetVpcLinks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetVpcLinks where
  toPath = Prelude.const "/vpclinks"

instance Data.ToQuery GetVpcLinks where
  toQuery GetVpcLinks' {..} =
    Prelude.mconcat
      ["limit" Data.=: limit, "position" Data.=: position]

-- | The collection of VPC links under the caller\'s account in a region.
--
-- /See:/ 'newGetVpcLinksResponse' smart constructor.
data GetVpcLinksResponse = GetVpcLinksResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [VpcLink],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpcLinksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getVpcLinksResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getVpcLinksResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getVpcLinksResponse_httpStatus' - The response's http status code.
newGetVpcLinksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVpcLinksResponse
newGetVpcLinksResponse pHttpStatus_ =
  GetVpcLinksResponse'
    { items = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getVpcLinksResponse_items :: Lens.Lens' GetVpcLinksResponse (Prelude.Maybe [VpcLink])
getVpcLinksResponse_items = Lens.lens (\GetVpcLinksResponse' {items} -> items) (\s@GetVpcLinksResponse' {} a -> s {items = a} :: GetVpcLinksResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getVpcLinksResponse_position :: Lens.Lens' GetVpcLinksResponse (Prelude.Maybe Prelude.Text)
getVpcLinksResponse_position = Lens.lens (\GetVpcLinksResponse' {position} -> position) (\s@GetVpcLinksResponse' {} a -> s {position = a} :: GetVpcLinksResponse)

-- | The response's http status code.
getVpcLinksResponse_httpStatus :: Lens.Lens' GetVpcLinksResponse Prelude.Int
getVpcLinksResponse_httpStatus = Lens.lens (\GetVpcLinksResponse' {httpStatus} -> httpStatus) (\s@GetVpcLinksResponse' {} a -> s {httpStatus = a} :: GetVpcLinksResponse)

instance Prelude.NFData GetVpcLinksResponse where
  rnf GetVpcLinksResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf httpStatus
