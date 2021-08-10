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
-- Module      : Network.AWS.ServiceCatalog.UpdateProduct
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified product.
module Network.AWS.ServiceCatalog.UpdateProduct
  ( -- * Creating a Request
    UpdateProduct (..),
    newUpdateProduct,

    -- * Request Lenses
    updateProduct_distributor,
    updateProduct_removeTags,
    updateProduct_addTags,
    updateProduct_name,
    updateProduct_supportUrl,
    updateProduct_supportDescription,
    updateProduct_owner,
    updateProduct_description,
    updateProduct_supportEmail,
    updateProduct_acceptLanguage,
    updateProduct_id,

    -- * Destructuring the Response
    UpdateProductResponse (..),
    newUpdateProductResponse,

    -- * Response Lenses
    updateProductResponse_productViewDetail,
    updateProductResponse_tags,
    updateProductResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newUpdateProduct' smart constructor.
data UpdateProduct = UpdateProduct'
  { -- | The updated distributor of the product.
    distributor :: Prelude.Maybe Prelude.Text,
    -- | The tags to remove from the product.
    removeTags :: Prelude.Maybe [Prelude.Text],
    -- | The tags to add to the product.
    addTags :: Prelude.Maybe [Tag],
    -- | The updated product name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The updated support URL for the product.
    supportUrl :: Prelude.Maybe Prelude.Text,
    -- | The updated support description for the product.
    supportDescription :: Prelude.Maybe Prelude.Text,
    -- | The updated owner of the product.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The updated description of the product.
    description :: Prelude.Maybe Prelude.Text,
    -- | The updated support email for the product.
    supportEmail :: Prelude.Maybe Prelude.Text,
    -- | The language code.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributor', 'updateProduct_distributor' - The updated distributor of the product.
--
-- 'removeTags', 'updateProduct_removeTags' - The tags to remove from the product.
--
-- 'addTags', 'updateProduct_addTags' - The tags to add to the product.
--
-- 'name', 'updateProduct_name' - The updated product name.
--
-- 'supportUrl', 'updateProduct_supportUrl' - The updated support URL for the product.
--
-- 'supportDescription', 'updateProduct_supportDescription' - The updated support description for the product.
--
-- 'owner', 'updateProduct_owner' - The updated owner of the product.
--
-- 'description', 'updateProduct_description' - The updated description of the product.
--
-- 'supportEmail', 'updateProduct_supportEmail' - The updated support email for the product.
--
-- 'acceptLanguage', 'updateProduct_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'updateProduct_id' - The product identifier.
newUpdateProduct ::
  -- | 'id'
  Prelude.Text ->
  UpdateProduct
newUpdateProduct pId_ =
  UpdateProduct'
    { distributor = Prelude.Nothing,
      removeTags = Prelude.Nothing,
      addTags = Prelude.Nothing,
      name = Prelude.Nothing,
      supportUrl = Prelude.Nothing,
      supportDescription = Prelude.Nothing,
      owner = Prelude.Nothing,
      description = Prelude.Nothing,
      supportEmail = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      id = pId_
    }

-- | The updated distributor of the product.
updateProduct_distributor :: Lens.Lens' UpdateProduct (Prelude.Maybe Prelude.Text)
updateProduct_distributor = Lens.lens (\UpdateProduct' {distributor} -> distributor) (\s@UpdateProduct' {} a -> s {distributor = a} :: UpdateProduct)

-- | The tags to remove from the product.
updateProduct_removeTags :: Lens.Lens' UpdateProduct (Prelude.Maybe [Prelude.Text])
updateProduct_removeTags = Lens.lens (\UpdateProduct' {removeTags} -> removeTags) (\s@UpdateProduct' {} a -> s {removeTags = a} :: UpdateProduct) Prelude.. Lens.mapping Lens._Coerce

-- | The tags to add to the product.
updateProduct_addTags :: Lens.Lens' UpdateProduct (Prelude.Maybe [Tag])
updateProduct_addTags = Lens.lens (\UpdateProduct' {addTags} -> addTags) (\s@UpdateProduct' {} a -> s {addTags = a} :: UpdateProduct) Prelude.. Lens.mapping Lens._Coerce

-- | The updated product name.
updateProduct_name :: Lens.Lens' UpdateProduct (Prelude.Maybe Prelude.Text)
updateProduct_name = Lens.lens (\UpdateProduct' {name} -> name) (\s@UpdateProduct' {} a -> s {name = a} :: UpdateProduct)

-- | The updated support URL for the product.
updateProduct_supportUrl :: Lens.Lens' UpdateProduct (Prelude.Maybe Prelude.Text)
updateProduct_supportUrl = Lens.lens (\UpdateProduct' {supportUrl} -> supportUrl) (\s@UpdateProduct' {} a -> s {supportUrl = a} :: UpdateProduct)

-- | The updated support description for the product.
updateProduct_supportDescription :: Lens.Lens' UpdateProduct (Prelude.Maybe Prelude.Text)
updateProduct_supportDescription = Lens.lens (\UpdateProduct' {supportDescription} -> supportDescription) (\s@UpdateProduct' {} a -> s {supportDescription = a} :: UpdateProduct)

-- | The updated owner of the product.
updateProduct_owner :: Lens.Lens' UpdateProduct (Prelude.Maybe Prelude.Text)
updateProduct_owner = Lens.lens (\UpdateProduct' {owner} -> owner) (\s@UpdateProduct' {} a -> s {owner = a} :: UpdateProduct)

-- | The updated description of the product.
updateProduct_description :: Lens.Lens' UpdateProduct (Prelude.Maybe Prelude.Text)
updateProduct_description = Lens.lens (\UpdateProduct' {description} -> description) (\s@UpdateProduct' {} a -> s {description = a} :: UpdateProduct)

-- | The updated support email for the product.
updateProduct_supportEmail :: Lens.Lens' UpdateProduct (Prelude.Maybe Prelude.Text)
updateProduct_supportEmail = Lens.lens (\UpdateProduct' {supportEmail} -> supportEmail) (\s@UpdateProduct' {} a -> s {supportEmail = a} :: UpdateProduct)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updateProduct_acceptLanguage :: Lens.Lens' UpdateProduct (Prelude.Maybe Prelude.Text)
updateProduct_acceptLanguage = Lens.lens (\UpdateProduct' {acceptLanguage} -> acceptLanguage) (\s@UpdateProduct' {} a -> s {acceptLanguage = a} :: UpdateProduct)

-- | The product identifier.
updateProduct_id :: Lens.Lens' UpdateProduct Prelude.Text
updateProduct_id = Lens.lens (\UpdateProduct' {id} -> id) (\s@UpdateProduct' {} a -> s {id = a} :: UpdateProduct)

instance Core.AWSRequest UpdateProduct where
  type
    AWSResponse UpdateProduct =
      UpdateProductResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProductResponse'
            Prelude.<$> (x Core..?> "ProductViewDetail")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProduct

instance Prelude.NFData UpdateProduct

instance Core.ToHeaders UpdateProduct where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.UpdateProduct" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateProduct where
  toJSON UpdateProduct' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Distributor" Core..=) Prelude.<$> distributor,
            ("RemoveTags" Core..=) Prelude.<$> removeTags,
            ("AddTags" Core..=) Prelude.<$> addTags,
            ("Name" Core..=) Prelude.<$> name,
            ("SupportUrl" Core..=) Prelude.<$> supportUrl,
            ("SupportDescription" Core..=)
              Prelude.<$> supportDescription,
            ("Owner" Core..=) Prelude.<$> owner,
            ("Description" Core..=) Prelude.<$> description,
            ("SupportEmail" Core..=) Prelude.<$> supportEmail,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath UpdateProduct where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateProduct where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProductResponse' smart constructor.
data UpdateProductResponse = UpdateProductResponse'
  { -- | Information about the product view.
    productViewDetail :: Prelude.Maybe ProductViewDetail,
    -- | Information about the tags associated with the product.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productViewDetail', 'updateProductResponse_productViewDetail' - Information about the product view.
--
-- 'tags', 'updateProductResponse_tags' - Information about the tags associated with the product.
--
-- 'httpStatus', 'updateProductResponse_httpStatus' - The response's http status code.
newUpdateProductResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProductResponse
newUpdateProductResponse pHttpStatus_ =
  UpdateProductResponse'
    { productViewDetail =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the product view.
updateProductResponse_productViewDetail :: Lens.Lens' UpdateProductResponse (Prelude.Maybe ProductViewDetail)
updateProductResponse_productViewDetail = Lens.lens (\UpdateProductResponse' {productViewDetail} -> productViewDetail) (\s@UpdateProductResponse' {} a -> s {productViewDetail = a} :: UpdateProductResponse)

-- | Information about the tags associated with the product.
updateProductResponse_tags :: Lens.Lens' UpdateProductResponse (Prelude.Maybe [Tag])
updateProductResponse_tags = Lens.lens (\UpdateProductResponse' {tags} -> tags) (\s@UpdateProductResponse' {} a -> s {tags = a} :: UpdateProductResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
updateProductResponse_httpStatus :: Lens.Lens' UpdateProductResponse Prelude.Int
updateProductResponse_httpStatus = Lens.lens (\UpdateProductResponse' {httpStatus} -> httpStatus) (\s@UpdateProductResponse' {} a -> s {httpStatus = a} :: UpdateProductResponse)

instance Prelude.NFData UpdateProductResponse
