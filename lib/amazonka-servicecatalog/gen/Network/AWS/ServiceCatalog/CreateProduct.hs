{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreateProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a product.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.CreateProduct
  ( -- * Creating a request
    CreateProduct (..),
    mkCreateProduct,

    -- ** Request lenses
    cpSupportURL,
    cpDistributor,
    cpAcceptLanguage,
    cpSupportEmail,
    cpDescription,
    cpTags,
    cpSupportDescription,
    cpName,
    cpOwner,
    cpProductType,
    cpProvisioningArtifactParameters,
    cpIdempotencyToken,

    -- * Destructuring the response
    CreateProductResponse (..),
    mkCreateProductResponse,

    -- ** Response lenses
    cprsProductViewDetail,
    cprsProvisioningArtifactDetail,
    cprsTags,
    cprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkCreateProduct' smart constructor.
data CreateProduct = CreateProduct'
  { supportURL ::
      Lude.Maybe Lude.Text,
    distributor :: Lude.Maybe Lude.Text,
    acceptLanguage :: Lude.Maybe Lude.Text,
    supportEmail :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    supportDescription :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    owner :: Lude.Text,
    productType :: ProductType,
    provisioningArtifactParameters ::
      ProvisioningArtifactProperties,
    idempotencyToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProduct' with the minimum fields required to make a request.
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
-- * 'description' - The description of the product.
-- * 'distributor' - The distributor of the product.
-- * 'idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
-- * 'name' - The name of the product.
-- * 'owner' - The owner of the product.
-- * 'productType' - The type of product.
-- * 'provisioningArtifactParameters' - The configuration of the provisioning artifact. The @info@ field accepts @ImportFromPhysicalID@ .
-- * 'supportDescription' - The support information about the product.
-- * 'supportEmail' - The contact email for product support.
-- * 'supportURL' - The contact URL for product support.
-- * 'tags' - One or more tags.
mkCreateProduct ::
  -- | 'name'
  Lude.Text ->
  -- | 'owner'
  Lude.Text ->
  -- | 'productType'
  ProductType ->
  -- | 'provisioningArtifactParameters'
  ProvisioningArtifactProperties ->
  -- | 'idempotencyToken'
  Lude.Text ->
  CreateProduct
mkCreateProduct
  pName_
  pOwner_
  pProductType_
  pProvisioningArtifactParameters_
  pIdempotencyToken_ =
    CreateProduct'
      { supportURL = Lude.Nothing,
        distributor = Lude.Nothing,
        acceptLanguage = Lude.Nothing,
        supportEmail = Lude.Nothing,
        description = Lude.Nothing,
        tags = Lude.Nothing,
        supportDescription = Lude.Nothing,
        name = pName_,
        owner = pOwner_,
        productType = pProductType_,
        provisioningArtifactParameters = pProvisioningArtifactParameters_,
        idempotencyToken = pIdempotencyToken_
      }

-- | The contact URL for product support.
--
-- /Note:/ Consider using 'supportURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSupportURL :: Lens.Lens' CreateProduct (Lude.Maybe Lude.Text)
cpSupportURL = Lens.lens (supportURL :: CreateProduct -> Lude.Maybe Lude.Text) (\s a -> s {supportURL = a} :: CreateProduct)
{-# DEPRECATED cpSupportURL "Use generic-lens or generic-optics with 'supportURL' instead." #-}

-- | The distributor of the product.
--
-- /Note:/ Consider using 'distributor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDistributor :: Lens.Lens' CreateProduct (Lude.Maybe Lude.Text)
cpDistributor = Lens.lens (distributor :: CreateProduct -> Lude.Maybe Lude.Text) (\s a -> s {distributor = a} :: CreateProduct)
{-# DEPRECATED cpDistributor "Use generic-lens or generic-optics with 'distributor' instead." #-}

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
cpAcceptLanguage :: Lens.Lens' CreateProduct (Lude.Maybe Lude.Text)
cpAcceptLanguage = Lens.lens (acceptLanguage :: CreateProduct -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: CreateProduct)
{-# DEPRECATED cpAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The contact email for product support.
--
-- /Note:/ Consider using 'supportEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSupportEmail :: Lens.Lens' CreateProduct (Lude.Maybe Lude.Text)
cpSupportEmail = Lens.lens (supportEmail :: CreateProduct -> Lude.Maybe Lude.Text) (\s a -> s {supportEmail = a} :: CreateProduct)
{-# DEPRECATED cpSupportEmail "Use generic-lens or generic-optics with 'supportEmail' instead." #-}

-- | The description of the product.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreateProduct (Lude.Maybe Lude.Text)
cpDescription = Lens.lens (description :: CreateProduct -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateProduct)
{-# DEPRECATED cpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreateProduct (Lude.Maybe [Tag])
cpTags = Lens.lens (tags :: CreateProduct -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateProduct)
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The support information about the product.
--
-- /Note:/ Consider using 'supportDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSupportDescription :: Lens.Lens' CreateProduct (Lude.Maybe Lude.Text)
cpSupportDescription = Lens.lens (supportDescription :: CreateProduct -> Lude.Maybe Lude.Text) (\s a -> s {supportDescription = a} :: CreateProduct)
{-# DEPRECATED cpSupportDescription "Use generic-lens or generic-optics with 'supportDescription' instead." #-}

-- | The name of the product.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreateProduct Lude.Text
cpName = Lens.lens (name :: CreateProduct -> Lude.Text) (\s a -> s {name = a} :: CreateProduct)
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The owner of the product.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpOwner :: Lens.Lens' CreateProduct Lude.Text
cpOwner = Lens.lens (owner :: CreateProduct -> Lude.Text) (\s a -> s {owner = a} :: CreateProduct)
{-# DEPRECATED cpOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The type of product.
--
-- /Note:/ Consider using 'productType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpProductType :: Lens.Lens' CreateProduct ProductType
cpProductType = Lens.lens (productType :: CreateProduct -> ProductType) (\s a -> s {productType = a} :: CreateProduct)
{-# DEPRECATED cpProductType "Use generic-lens or generic-optics with 'productType' instead." #-}

-- | The configuration of the provisioning artifact. The @info@ field accepts @ImportFromPhysicalID@ .
--
-- /Note:/ Consider using 'provisioningArtifactParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpProvisioningArtifactParameters :: Lens.Lens' CreateProduct ProvisioningArtifactProperties
cpProvisioningArtifactParameters = Lens.lens (provisioningArtifactParameters :: CreateProduct -> ProvisioningArtifactProperties) (\s a -> s {provisioningArtifactParameters = a} :: CreateProduct)
{-# DEPRECATED cpProvisioningArtifactParameters "Use generic-lens or generic-optics with 'provisioningArtifactParameters' instead." #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpIdempotencyToken :: Lens.Lens' CreateProduct Lude.Text
cpIdempotencyToken = Lens.lens (idempotencyToken :: CreateProduct -> Lude.Text) (\s a -> s {idempotencyToken = a} :: CreateProduct)
{-# DEPRECATED cpIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

instance Lude.AWSRequest CreateProduct where
  type Rs CreateProduct = CreateProductResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProductResponse'
            Lude.<$> (x Lude..?> "ProductViewDetail")
            Lude.<*> (x Lude..?> "ProvisioningArtifactDetail")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateProduct where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.CreateProduct" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateProduct where
  toJSON CreateProduct' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SupportUrl" Lude..=) Lude.<$> supportURL,
            ("Distributor" Lude..=) Lude.<$> distributor,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("SupportEmail" Lude..=) Lude.<$> supportEmail,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            ("SupportDescription" Lude..=) Lude.<$> supportDescription,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Owner" Lude..= owner),
            Lude.Just ("ProductType" Lude..= productType),
            Lude.Just
              ( "ProvisioningArtifactParameters"
                  Lude..= provisioningArtifactParameters
              ),
            Lude.Just ("IdempotencyToken" Lude..= idempotencyToken)
          ]
      )

instance Lude.ToPath CreateProduct where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateProduct where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateProductResponse' smart constructor.
data CreateProductResponse = CreateProductResponse'
  { productViewDetail ::
      Lude.Maybe ProductViewDetail,
    provisioningArtifactDetail ::
      Lude.Maybe ProvisioningArtifactDetail,
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

-- | Creates a value of 'CreateProductResponse' with the minimum fields required to make a request.
--
-- * 'productViewDetail' - Information about the product view.
-- * 'provisioningArtifactDetail' - Information about the provisioning artifact.
-- * 'responseStatus' - The response status code.
-- * 'tags' - Information about the tags associated with the product.
mkCreateProductResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProductResponse
mkCreateProductResponse pResponseStatus_ =
  CreateProductResponse'
    { productViewDetail = Lude.Nothing,
      provisioningArtifactDetail = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the product view.
--
-- /Note:/ Consider using 'productViewDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsProductViewDetail :: Lens.Lens' CreateProductResponse (Lude.Maybe ProductViewDetail)
cprsProductViewDetail = Lens.lens (productViewDetail :: CreateProductResponse -> Lude.Maybe ProductViewDetail) (\s a -> s {productViewDetail = a} :: CreateProductResponse)
{-# DEPRECATED cprsProductViewDetail "Use generic-lens or generic-optics with 'productViewDetail' instead." #-}

-- | Information about the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsProvisioningArtifactDetail :: Lens.Lens' CreateProductResponse (Lude.Maybe ProvisioningArtifactDetail)
cprsProvisioningArtifactDetail = Lens.lens (provisioningArtifactDetail :: CreateProductResponse -> Lude.Maybe ProvisioningArtifactDetail) (\s a -> s {provisioningArtifactDetail = a} :: CreateProductResponse)
{-# DEPRECATED cprsProvisioningArtifactDetail "Use generic-lens or generic-optics with 'provisioningArtifactDetail' instead." #-}

-- | Information about the tags associated with the product.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsTags :: Lens.Lens' CreateProductResponse (Lude.Maybe [Tag])
cprsTags = Lens.lens (tags :: CreateProductResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateProductResponse)
{-# DEPRECATED cprsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreateProductResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreateProductResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProductResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
