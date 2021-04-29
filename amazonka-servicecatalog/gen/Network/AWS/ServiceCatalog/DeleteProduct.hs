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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteProduct
newDeleteProduct pId_ =
  DeleteProduct'
    { acceptLanguage = Prelude.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
deleteProduct_acceptLanguage :: Lens.Lens' DeleteProduct (Prelude.Maybe Prelude.Text)
deleteProduct_acceptLanguage = Lens.lens (\DeleteProduct' {acceptLanguage} -> acceptLanguage) (\s@DeleteProduct' {} a -> s {acceptLanguage = a} :: DeleteProduct)

-- | The product identifier.
deleteProduct_id :: Lens.Lens' DeleteProduct Prelude.Text
deleteProduct_id = Lens.lens (\DeleteProduct' {id} -> id) (\s@DeleteProduct' {} a -> s {id = a} :: DeleteProduct)

instance Prelude.AWSRequest DeleteProduct where
  type Rs DeleteProduct = DeleteProductResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProductResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProduct

instance Prelude.NFData DeleteProduct

instance Prelude.ToHeaders DeleteProduct where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.DeleteProduct" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteProduct where
  toJSON DeleteProduct' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Prelude..= id)
          ]
      )

instance Prelude.ToPath DeleteProduct where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteProduct where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProductResponse' smart constructor.
data DeleteProductResponse = DeleteProductResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteProductResponse
newDeleteProductResponse pHttpStatus_ =
  DeleteProductResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteProductResponse_httpStatus :: Lens.Lens' DeleteProductResponse Prelude.Int
deleteProductResponse_httpStatus = Lens.lens (\DeleteProductResponse' {httpStatus} -> httpStatus) (\s@DeleteProductResponse' {} a -> s {httpStatus = a} :: DeleteProductResponse)

instance Prelude.NFData DeleteProductResponse
