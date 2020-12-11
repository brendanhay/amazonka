{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified product.
module Network.AWS.ServiceCatalog.UpdateProduct
  ( -- * Creating a request
    UpdateProduct (..),
    mkUpdateProduct,

    -- ** Request lenses
    upRemoveTags,
    upOwner,
    upSupportURL,
    upDistributor,
    upName,
    upAcceptLanguage,
    upAddTags,
    upSupportEmail,
    upDescription,
    upSupportDescription,
    upId,

    -- * Destructuring the response
    UpdateProductResponse (..),
    mkUpdateProductResponse,

    -- ** Response lenses
    ursProductViewDetail,
    ursTags,
    ursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkUpdateProduct' smart constructor.
data UpdateProduct = UpdateProduct'
  { removeTags ::
      Lude.Maybe [Lude.Text],
    owner :: Lude.Maybe Lude.Text,
    supportURL :: Lude.Maybe Lude.Text,
    distributor :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    acceptLanguage :: Lude.Maybe Lude.Text,
    addTags :: Lude.Maybe [Tag],
    supportEmail :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    supportDescription :: Lude.Maybe Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProduct' with the minimum fields required to make a request.
--
-- * 'acceptLanguage' - The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
-- * 'addTags' - The tags to add to the product.
-- * 'description' - The updated description of the product.
-- * 'distributor' - The updated distributor of the product.
-- * 'id' - The product identifier.
-- * 'name' - The updated product name.
-- * 'owner' - The updated owner of the product.
-- * 'removeTags' - The tags to remove from the product.
-- * 'supportDescription' - The updated support description for the product.
-- * 'supportEmail' - The updated support email for the product.
-- * 'supportURL' - The updated support URL for the product.
mkUpdateProduct ::
  -- | 'id'
  Lude.Text ->
  UpdateProduct
mkUpdateProduct pId_ =
  UpdateProduct'
    { removeTags = Lude.Nothing,
      owner = Lude.Nothing,
      supportURL = Lude.Nothing,
      distributor = Lude.Nothing,
      name = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      addTags = Lude.Nothing,
      supportEmail = Lude.Nothing,
      description = Lude.Nothing,
      supportDescription = Lude.Nothing,
      id = pId_
    }

-- | The tags to remove from the product.
--
-- /Note:/ Consider using 'removeTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upRemoveTags :: Lens.Lens' UpdateProduct (Lude.Maybe [Lude.Text])
upRemoveTags = Lens.lens (removeTags :: UpdateProduct -> Lude.Maybe [Lude.Text]) (\s a -> s {removeTags = a} :: UpdateProduct)
{-# DEPRECATED upRemoveTags "Use generic-lens or generic-optics with 'removeTags' instead." #-}

-- | The updated owner of the product.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upOwner :: Lens.Lens' UpdateProduct (Lude.Maybe Lude.Text)
upOwner = Lens.lens (owner :: UpdateProduct -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: UpdateProduct)
{-# DEPRECATED upOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The updated support URL for the product.
--
-- /Note:/ Consider using 'supportURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSupportURL :: Lens.Lens' UpdateProduct (Lude.Maybe Lude.Text)
upSupportURL = Lens.lens (supportURL :: UpdateProduct -> Lude.Maybe Lude.Text) (\s a -> s {supportURL = a} :: UpdateProduct)
{-# DEPRECATED upSupportURL "Use generic-lens or generic-optics with 'supportURL' instead." #-}

-- | The updated distributor of the product.
--
-- /Note:/ Consider using 'distributor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDistributor :: Lens.Lens' UpdateProduct (Lude.Maybe Lude.Text)
upDistributor = Lens.lens (distributor :: UpdateProduct -> Lude.Maybe Lude.Text) (\s a -> s {distributor = a} :: UpdateProduct)
{-# DEPRECATED upDistributor "Use generic-lens or generic-optics with 'distributor' instead." #-}

-- | The updated product name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdateProduct (Lude.Maybe Lude.Text)
upName = Lens.lens (name :: UpdateProduct -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateProduct)
{-# DEPRECATED upName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upAcceptLanguage :: Lens.Lens' UpdateProduct (Lude.Maybe Lude.Text)
upAcceptLanguage = Lens.lens (acceptLanguage :: UpdateProduct -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: UpdateProduct)
{-# DEPRECATED upAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The tags to add to the product.
--
-- /Note:/ Consider using 'addTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upAddTags :: Lens.Lens' UpdateProduct (Lude.Maybe [Tag])
upAddTags = Lens.lens (addTags :: UpdateProduct -> Lude.Maybe [Tag]) (\s a -> s {addTags = a} :: UpdateProduct)
{-# DEPRECATED upAddTags "Use generic-lens or generic-optics with 'addTags' instead." #-}

-- | The updated support email for the product.
--
-- /Note:/ Consider using 'supportEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSupportEmail :: Lens.Lens' UpdateProduct (Lude.Maybe Lude.Text)
upSupportEmail = Lens.lens (supportEmail :: UpdateProduct -> Lude.Maybe Lude.Text) (\s a -> s {supportEmail = a} :: UpdateProduct)
{-# DEPRECATED upSupportEmail "Use generic-lens or generic-optics with 'supportEmail' instead." #-}

-- | The updated description of the product.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UpdateProduct (Lude.Maybe Lude.Text)
upDescription = Lens.lens (description :: UpdateProduct -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateProduct)
{-# DEPRECATED upDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The updated support description for the product.
--
-- /Note:/ Consider using 'supportDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSupportDescription :: Lens.Lens' UpdateProduct (Lude.Maybe Lude.Text)
upSupportDescription = Lens.lens (supportDescription :: UpdateProduct -> Lude.Maybe Lude.Text) (\s a -> s {supportDescription = a} :: UpdateProduct)
{-# DEPRECATED upSupportDescription "Use generic-lens or generic-optics with 'supportDescription' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upId :: Lens.Lens' UpdateProduct Lude.Text
upId = Lens.lens (id :: UpdateProduct -> Lude.Text) (\s a -> s {id = a} :: UpdateProduct)
{-# DEPRECATED upId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdateProduct where
  type Rs UpdateProduct = UpdateProductResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateProductResponse'
            Lude.<$> (x Lude..?> "ProductViewDetail")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateProduct where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.UpdateProduct" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateProduct where
  toJSON UpdateProduct' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RemoveTags" Lude..=) Lude.<$> removeTags,
            ("Owner" Lude..=) Lude.<$> owner,
            ("SupportUrl" Lude..=) Lude.<$> supportURL,
            ("Distributor" Lude..=) Lude.<$> distributor,
            ("Name" Lude..=) Lude.<$> name,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("AddTags" Lude..=) Lude.<$> addTags,
            ("SupportEmail" Lude..=) Lude.<$> supportEmail,
            ("Description" Lude..=) Lude.<$> description,
            ("SupportDescription" Lude..=) Lude.<$> supportDescription,
            Lude.Just ("Id" Lude..= id)
          ]
      )

instance Lude.ToPath UpdateProduct where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateProduct where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateProductResponse' smart constructor.
data UpdateProductResponse = UpdateProductResponse'
  { productViewDetail ::
      Lude.Maybe ProductViewDetail,
    tags :: Lude.Maybe [Tag],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProductResponse' with the minimum fields required to make a request.
--
-- * 'productViewDetail' - Information about the product view.
-- * 'responseStatus' - The response status code.
-- * 'tags' - Information about the tags associated with the product.
mkUpdateProductResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateProductResponse
mkUpdateProductResponse pResponseStatus_ =
  UpdateProductResponse'
    { productViewDetail = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the product view.
--
-- /Note:/ Consider using 'productViewDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursProductViewDetail :: Lens.Lens' UpdateProductResponse (Lude.Maybe ProductViewDetail)
ursProductViewDetail = Lens.lens (productViewDetail :: UpdateProductResponse -> Lude.Maybe ProductViewDetail) (\s a -> s {productViewDetail = a} :: UpdateProductResponse)
{-# DEPRECATED ursProductViewDetail "Use generic-lens or generic-optics with 'productViewDetail' instead." #-}

-- | Information about the tags associated with the product.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursTags :: Lens.Lens' UpdateProductResponse (Lude.Maybe [Tag])
ursTags = Lens.lens (tags :: UpdateProductResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: UpdateProductResponse)
{-# DEPRECATED ursTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateProductResponse Lude.Int
ursResponseStatus = Lens.lens (responseStatus :: UpdateProductResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateProductResponse)
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
