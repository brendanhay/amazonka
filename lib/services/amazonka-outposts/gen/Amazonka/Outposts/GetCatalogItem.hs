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
-- Module      : Amazonka.Outposts.GetCatalogItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified catalog item.
module Amazonka.Outposts.GetCatalogItem
  ( -- * Creating a Request
    GetCatalogItem (..),
    newGetCatalogItem,

    -- * Request Lenses
    getCatalogItem_catalogItemId,

    -- * Destructuring the Response
    GetCatalogItemResponse (..),
    newGetCatalogItemResponse,

    -- * Response Lenses
    getCatalogItemResponse_catalogItem,
    getCatalogItemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCatalogItem' smart constructor.
data GetCatalogItem = GetCatalogItem'
  { -- | The ID of the catalog item.
    catalogItemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCatalogItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogItemId', 'getCatalogItem_catalogItemId' - The ID of the catalog item.
newGetCatalogItem ::
  -- | 'catalogItemId'
  Prelude.Text ->
  GetCatalogItem
newGetCatalogItem pCatalogItemId_ =
  GetCatalogItem' {catalogItemId = pCatalogItemId_}

-- | The ID of the catalog item.
getCatalogItem_catalogItemId :: Lens.Lens' GetCatalogItem Prelude.Text
getCatalogItem_catalogItemId = Lens.lens (\GetCatalogItem' {catalogItemId} -> catalogItemId) (\s@GetCatalogItem' {} a -> s {catalogItemId = a} :: GetCatalogItem)

instance Core.AWSRequest GetCatalogItem where
  type
    AWSResponse GetCatalogItem =
      GetCatalogItemResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCatalogItemResponse'
            Prelude.<$> (x Data..?> "CatalogItem")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCatalogItem where
  hashWithSalt _salt GetCatalogItem' {..} =
    _salt `Prelude.hashWithSalt` catalogItemId

instance Prelude.NFData GetCatalogItem where
  rnf GetCatalogItem' {..} = Prelude.rnf catalogItemId

instance Data.ToHeaders GetCatalogItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCatalogItem where
  toPath GetCatalogItem' {..} =
    Prelude.mconcat
      ["/catalog/item/", Data.toBS catalogItemId]

instance Data.ToQuery GetCatalogItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCatalogItemResponse' smart constructor.
data GetCatalogItemResponse = GetCatalogItemResponse'
  { -- | Information about this catalog item.
    catalogItem :: Prelude.Maybe CatalogItem,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCatalogItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogItem', 'getCatalogItemResponse_catalogItem' - Information about this catalog item.
--
-- 'httpStatus', 'getCatalogItemResponse_httpStatus' - The response's http status code.
newGetCatalogItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCatalogItemResponse
newGetCatalogItemResponse pHttpStatus_ =
  GetCatalogItemResponse'
    { catalogItem =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about this catalog item.
getCatalogItemResponse_catalogItem :: Lens.Lens' GetCatalogItemResponse (Prelude.Maybe CatalogItem)
getCatalogItemResponse_catalogItem = Lens.lens (\GetCatalogItemResponse' {catalogItem} -> catalogItem) (\s@GetCatalogItemResponse' {} a -> s {catalogItem = a} :: GetCatalogItemResponse)

-- | The response's http status code.
getCatalogItemResponse_httpStatus :: Lens.Lens' GetCatalogItemResponse Prelude.Int
getCatalogItemResponse_httpStatus = Lens.lens (\GetCatalogItemResponse' {httpStatus} -> httpStatus) (\s@GetCatalogItemResponse' {} a -> s {httpStatus = a} :: GetCatalogItemResponse)

instance Prelude.NFData GetCatalogItemResponse where
  rnf GetCatalogItemResponse' {..} =
    Prelude.rnf catalogItem
      `Prelude.seq` Prelude.rnf httpStatus
