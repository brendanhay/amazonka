{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.UpdateProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests updates to the configuration of the specified provisioned product.
--
-- If there are tags associated with the object, they cannot be updated or added. Depending on the specific updates requested, this operation can update with no interruption, with some interruption, or replace the provisioned product entirely.
-- You can check the status of this request using 'DescribeRecord' .
module Network.AWS.ServiceCatalog.UpdateProvisionedProduct
  ( -- * Creating a request
    UpdateProvisionedProduct (..),
    mkUpdateProvisionedProduct,

    -- ** Request lenses
    uppProductName,
    uppProvisionedProductName,
    uppProvisioningArtifactId,
    uppProvisioningArtifactName,
    uppPathName,
    uppAcceptLanguage,
    uppPathId,
    uppProvisioningParameters,
    uppProvisionedProductId,
    uppProductId,
    uppTags,
    uppProvisioningPreferences,
    uppUpdateToken,

    -- * Destructuring the response
    UpdateProvisionedProductResponse (..),
    mkUpdateProvisionedProductResponse,

    -- ** Response lenses
    upprsRecordDetail,
    upprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkUpdateProvisionedProduct' smart constructor.
data UpdateProvisionedProduct = UpdateProvisionedProduct'
  { productName ::
      Lude.Maybe Lude.Text,
    provisionedProductName ::
      Lude.Maybe Lude.Text,
    provisioningArtifactId ::
      Lude.Maybe Lude.Text,
    provisioningArtifactName ::
      Lude.Maybe Lude.Text,
    pathName :: Lude.Maybe Lude.Text,
    acceptLanguage :: Lude.Maybe Lude.Text,
    pathId :: Lude.Maybe Lude.Text,
    provisioningParameters ::
      Lude.Maybe [UpdateProvisioningParameter],
    provisionedProductId ::
      Lude.Maybe Lude.Text,
    productId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    provisioningPreferences ::
      Lude.Maybe UpdateProvisioningPreferences,
    updateToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProvisionedProduct' with the minimum fields required to make a request.
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
-- * 'pathId' - The path identifier. This value is optional if the product has a default path, and required if the product has more than one path. You must provide the name or ID, but not both.
-- * 'pathName' - The name of the path. You must provide the name or ID, but not both.
-- * 'productId' - The identifier of the product. You must provide the name or ID, but not both.
-- * 'productName' - The name of the product. You must provide the name or ID, but not both.
-- * 'provisionedProductId' - The identifier of the provisioned product. You must provide the name or ID, but not both.
-- * 'provisionedProductName' - The name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact.
-- * 'provisioningArtifactName' - The name of the provisioning artifact. You must provide the name or ID, but not both.
-- * 'provisioningParameters' - The new parameters.
-- * 'provisioningPreferences' - An object that contains information about the provisioning preferences for a stack set.
-- * 'tags' - One or more tags. Requires the product to have @RESOURCE_UPDATE@ constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
-- * 'updateToken' - The idempotency token that uniquely identifies the provisioning update request.
mkUpdateProvisionedProduct ::
  -- | 'updateToken'
  Lude.Text ->
  UpdateProvisionedProduct
mkUpdateProvisionedProduct pUpdateToken_ =
  UpdateProvisionedProduct'
    { productName = Lude.Nothing,
      provisionedProductName = Lude.Nothing,
      provisioningArtifactId = Lude.Nothing,
      provisioningArtifactName = Lude.Nothing,
      pathName = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      pathId = Lude.Nothing,
      provisioningParameters = Lude.Nothing,
      provisionedProductId = Lude.Nothing,
      productId = Lude.Nothing,
      tags = Lude.Nothing,
      provisioningPreferences = Lude.Nothing,
      updateToken = pUpdateToken_
    }

-- | The name of the product. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'productName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProductName :: Lens.Lens' UpdateProvisionedProduct (Lude.Maybe Lude.Text)
uppProductName = Lens.lens (productName :: UpdateProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {productName = a} :: UpdateProvisionedProduct)
{-# DEPRECATED uppProductName "Use generic-lens or generic-optics with 'productName' instead." #-}

-- | The name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProvisionedProductName :: Lens.Lens' UpdateProvisionedProduct (Lude.Maybe Lude.Text)
uppProvisionedProductName = Lens.lens (provisionedProductName :: UpdateProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {provisionedProductName = a} :: UpdateProvisionedProduct)
{-# DEPRECATED uppProvisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProvisioningArtifactId :: Lens.Lens' UpdateProvisionedProduct (Lude.Maybe Lude.Text)
uppProvisioningArtifactId = Lens.lens (provisioningArtifactId :: UpdateProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactId = a} :: UpdateProvisionedProduct)
{-# DEPRECATED uppProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The name of the provisioning artifact. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'provisioningArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProvisioningArtifactName :: Lens.Lens' UpdateProvisionedProduct (Lude.Maybe Lude.Text)
uppProvisioningArtifactName = Lens.lens (provisioningArtifactName :: UpdateProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactName = a} :: UpdateProvisionedProduct)
{-# DEPRECATED uppProvisioningArtifactName "Use generic-lens or generic-optics with 'provisioningArtifactName' instead." #-}

-- | The name of the path. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'pathName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppPathName :: Lens.Lens' UpdateProvisionedProduct (Lude.Maybe Lude.Text)
uppPathName = Lens.lens (pathName :: UpdateProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {pathName = a} :: UpdateProvisionedProduct)
{-# DEPRECATED uppPathName "Use generic-lens or generic-optics with 'pathName' instead." #-}

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
uppAcceptLanguage :: Lens.Lens' UpdateProvisionedProduct (Lude.Maybe Lude.Text)
uppAcceptLanguage = Lens.lens (acceptLanguage :: UpdateProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: UpdateProvisionedProduct)
{-# DEPRECATED uppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The path identifier. This value is optional if the product has a default path, and required if the product has more than one path. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'pathId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppPathId :: Lens.Lens' UpdateProvisionedProduct (Lude.Maybe Lude.Text)
uppPathId = Lens.lens (pathId :: UpdateProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {pathId = a} :: UpdateProvisionedProduct)
{-# DEPRECATED uppPathId "Use generic-lens or generic-optics with 'pathId' instead." #-}

-- | The new parameters.
--
-- /Note:/ Consider using 'provisioningParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProvisioningParameters :: Lens.Lens' UpdateProvisionedProduct (Lude.Maybe [UpdateProvisioningParameter])
uppProvisioningParameters = Lens.lens (provisioningParameters :: UpdateProvisionedProduct -> Lude.Maybe [UpdateProvisioningParameter]) (\s a -> s {provisioningParameters = a} :: UpdateProvisionedProduct)
{-# DEPRECATED uppProvisioningParameters "Use generic-lens or generic-optics with 'provisioningParameters' instead." #-}

-- | The identifier of the provisioned product. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProvisionedProductId :: Lens.Lens' UpdateProvisionedProduct (Lude.Maybe Lude.Text)
uppProvisionedProductId = Lens.lens (provisionedProductId :: UpdateProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {provisionedProductId = a} :: UpdateProvisionedProduct)
{-# DEPRECATED uppProvisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead." #-}

-- | The identifier of the product. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProductId :: Lens.Lens' UpdateProvisionedProduct (Lude.Maybe Lude.Text)
uppProductId = Lens.lens (productId :: UpdateProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: UpdateProvisionedProduct)
{-# DEPRECATED uppProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | One or more tags. Requires the product to have @RESOURCE_UPDATE@ constraint with @TagUpdatesOnProvisionedProduct@ set to @ALLOWED@ to allow tag updates.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppTags :: Lens.Lens' UpdateProvisionedProduct (Lude.Maybe [Tag])
uppTags = Lens.lens (tags :: UpdateProvisionedProduct -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: UpdateProvisionedProduct)
{-# DEPRECATED uppTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | An object that contains information about the provisioning preferences for a stack set.
--
-- /Note:/ Consider using 'provisioningPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppProvisioningPreferences :: Lens.Lens' UpdateProvisionedProduct (Lude.Maybe UpdateProvisioningPreferences)
uppProvisioningPreferences = Lens.lens (provisioningPreferences :: UpdateProvisionedProduct -> Lude.Maybe UpdateProvisioningPreferences) (\s a -> s {provisioningPreferences = a} :: UpdateProvisionedProduct)
{-# DEPRECATED uppProvisioningPreferences "Use generic-lens or generic-optics with 'provisioningPreferences' instead." #-}

-- | The idempotency token that uniquely identifies the provisioning update request.
--
-- /Note:/ Consider using 'updateToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uppUpdateToken :: Lens.Lens' UpdateProvisionedProduct Lude.Text
uppUpdateToken = Lens.lens (updateToken :: UpdateProvisionedProduct -> Lude.Text) (\s a -> s {updateToken = a} :: UpdateProvisionedProduct)
{-# DEPRECATED uppUpdateToken "Use generic-lens or generic-optics with 'updateToken' instead." #-}

instance Lude.AWSRequest UpdateProvisionedProduct where
  type Rs UpdateProvisionedProduct = UpdateProvisionedProductResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateProvisionedProductResponse'
            Lude.<$> (x Lude..?> "RecordDetail") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateProvisionedProduct where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.UpdateProvisionedProduct" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateProvisionedProduct where
  toJSON UpdateProvisionedProduct' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProductName" Lude..=) Lude.<$> productName,
            ("ProvisionedProductName" Lude..=) Lude.<$> provisionedProductName,
            ("ProvisioningArtifactId" Lude..=) Lude.<$> provisioningArtifactId,
            ("ProvisioningArtifactName" Lude..=)
              Lude.<$> provisioningArtifactName,
            ("PathName" Lude..=) Lude.<$> pathName,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PathId" Lude..=) Lude.<$> pathId,
            ("ProvisioningParameters" Lude..=) Lude.<$> provisioningParameters,
            ("ProvisionedProductId" Lude..=) Lude.<$> provisionedProductId,
            ("ProductId" Lude..=) Lude.<$> productId,
            ("Tags" Lude..=) Lude.<$> tags,
            ("ProvisioningPreferences" Lude..=)
              Lude.<$> provisioningPreferences,
            Lude.Just ("UpdateToken" Lude..= updateToken)
          ]
      )

instance Lude.ToPath UpdateProvisionedProduct where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateProvisionedProduct where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateProvisionedProductResponse' smart constructor.
data UpdateProvisionedProductResponse = UpdateProvisionedProductResponse'
  { recordDetail ::
      Lude.Maybe RecordDetail,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProvisionedProductResponse' with the minimum fields required to make a request.
--
-- * 'recordDetail' - Information about the result of the request.
-- * 'responseStatus' - The response status code.
mkUpdateProvisionedProductResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateProvisionedProductResponse
mkUpdateProvisionedProductResponse pResponseStatus_ =
  UpdateProvisionedProductResponse'
    { recordDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the result of the request.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upprsRecordDetail :: Lens.Lens' UpdateProvisionedProductResponse (Lude.Maybe RecordDetail)
upprsRecordDetail = Lens.lens (recordDetail :: UpdateProvisionedProductResponse -> Lude.Maybe RecordDetail) (\s a -> s {recordDetail = a} :: UpdateProvisionedProductResponse)
{-# DEPRECATED upprsRecordDetail "Use generic-lens or generic-optics with 'recordDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upprsResponseStatus :: Lens.Lens' UpdateProvisionedProductResponse Lude.Int
upprsResponseStatus = Lens.lens (responseStatus :: UpdateProvisionedProductResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateProvisionedProductResponse)
{-# DEPRECATED upprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
