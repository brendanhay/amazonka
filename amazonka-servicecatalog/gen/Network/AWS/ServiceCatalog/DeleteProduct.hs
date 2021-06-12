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
-- Module      : Network.AWS.ServiceCatalog.DeleteProduct
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified product.
--
-- You cannot delete a product if it was shared with you or is associated
-- with a portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.DeleteProduct
  ( -- * Creating a Request
    DeleteProduct (..),
    newDeleteProduct,

    -- * Request Lenses
    deleteProduct_acceptLanguage,
    deleteProduct_id,

    -- * Destructuring the Response
    DeleteProductResponse (..),
    newDeleteProductResponse,

    -- * Response Lenses
    deleteProductResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDeleteProduct' smart constructor.
data DeleteProduct = DeleteProduct'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The product identifier.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'deleteProduct_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'deleteProduct_id' - The product identifier.
newDeleteProduct ::
  -- | 'id'
  Core.Text ->
  DeleteProduct
newDeleteProduct pId_ =
  DeleteProduct'
    { acceptLanguage = Core.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
deleteProduct_acceptLanguage :: Lens.Lens' DeleteProduct (Core.Maybe Core.Text)
deleteProduct_acceptLanguage = Lens.lens (\DeleteProduct' {acceptLanguage} -> acceptLanguage) (\s@DeleteProduct' {} a -> s {acceptLanguage = a} :: DeleteProduct)

-- | The product identifier.
deleteProduct_id :: Lens.Lens' DeleteProduct Core.Text
deleteProduct_id = Lens.lens (\DeleteProduct' {id} -> id) (\s@DeleteProduct' {} a -> s {id = a} :: DeleteProduct)

instance Core.AWSRequest DeleteProduct where
  type
    AWSResponse DeleteProduct =
      DeleteProductResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProductResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteProduct

instance Core.NFData DeleteProduct

instance Core.ToHeaders DeleteProduct where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DeleteProduct" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteProduct where
  toJSON DeleteProduct' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath DeleteProduct where
  toPath = Core.const "/"

instance Core.ToQuery DeleteProduct where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteProductResponse' smart constructor.
data DeleteProductResponse = DeleteProductResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProductResponse_httpStatus' - The response's http status code.
newDeleteProductResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteProductResponse
newDeleteProductResponse pHttpStatus_ =
  DeleteProductResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteProductResponse_httpStatus :: Lens.Lens' DeleteProductResponse Core.Int
deleteProductResponse_httpStatus = Lens.lens (\DeleteProductResponse' {httpStatus} -> httpStatus) (\s@DeleteProductResponse' {} a -> s {httpStatus = a} :: DeleteProductResponse)

instance Core.NFData DeleteProductResponse
