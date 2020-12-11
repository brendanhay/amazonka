{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ProvisionProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions the specified product.
--
-- A provisioned product is a resourced instance of a product. For example, provisioning a product based on a CloudFormation template launches a CloudFormation stack and its underlying resources. You can check the status of this request using 'DescribeRecord' .
-- If the request contains a tag key with an empty list of values, there is a tag conflict for that key. Do not include conflicted keys as tags, or this causes the error "Parameter validation failed: Missing required parameter in Tags[/N/ ]:/Value/ ".
module Network.AWS.ServiceCatalog.ProvisionProduct
  ( -- * Creating a request
    ProvisionProduct (..),
    mkProvisionProduct,

    -- ** Request lenses
    ppProductName,
    ppProvisioningArtifactId,
    ppProvisioningArtifactName,
    ppNotificationARNs,
    ppPathName,
    ppAcceptLanguage,
    ppPathId,
    ppProvisioningParameters,
    ppProductId,
    ppTags,
    ppProvisioningPreferences,
    ppProvisionedProductName,
    ppProvisionToken,

    -- * Destructuring the response
    ProvisionProductResponse (..),
    mkProvisionProductResponse,

    -- ** Response lenses
    pprsRecordDetail,
    pprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkProvisionProduct' smart constructor.
data ProvisionProduct = ProvisionProduct'
  { productName ::
      Lude.Maybe Lude.Text,
    provisioningArtifactId :: Lude.Maybe Lude.Text,
    provisioningArtifactName :: Lude.Maybe Lude.Text,
    notificationARNs :: Lude.Maybe [Lude.Text],
    pathName :: Lude.Maybe Lude.Text,
    acceptLanguage :: Lude.Maybe Lude.Text,
    pathId :: Lude.Maybe Lude.Text,
    provisioningParameters ::
      Lude.Maybe [ProvisioningParameter],
    productId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    provisioningPreferences ::
      Lude.Maybe ProvisioningPreferences,
    provisionedProductName :: Lude.Text,
    provisionToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisionProduct' with the minimum fields required to make a request.
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
-- * 'notificationARNs' - Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
-- * 'pathId' - The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' . You must provide the name or ID, but not both.
-- * 'pathName' - The name of the path. You must provide the name or ID, but not both.
-- * 'productId' - The product identifier. You must provide the name or ID, but not both.
-- * 'productName' - The name of the product. You must provide the name or ID, but not both.
-- * 'provisionToken' - An idempotency token that uniquely identifies the provisioning request.
-- * 'provisionedProductName' - A user-friendly name for the provisioned product. This value must be unique for the AWS account and cannot be updated after the product is provisioned.
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact. You must provide the name or ID, but not both.
-- * 'provisioningArtifactName' - The name of the provisioning artifact. You must provide the name or ID, but not both.
-- * 'provisioningParameters' - Parameters specified by the administrator that are required for provisioning the product.
-- * 'provisioningPreferences' - An object that contains information about the provisioning preferences for a stack set.
-- * 'tags' - One or more tags.
mkProvisionProduct ::
  -- | 'provisionedProductName'
  Lude.Text ->
  -- | 'provisionToken'
  Lude.Text ->
  ProvisionProduct
mkProvisionProduct pProvisionedProductName_ pProvisionToken_ =
  ProvisionProduct'
    { productName = Lude.Nothing,
      provisioningArtifactId = Lude.Nothing,
      provisioningArtifactName = Lude.Nothing,
      notificationARNs = Lude.Nothing,
      pathName = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      pathId = Lude.Nothing,
      provisioningParameters = Lude.Nothing,
      productId = Lude.Nothing,
      tags = Lude.Nothing,
      provisioningPreferences = Lude.Nothing,
      provisionedProductName = pProvisionedProductName_,
      provisionToken = pProvisionToken_
    }

-- | The name of the product. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'productName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProductName :: Lens.Lens' ProvisionProduct (Lude.Maybe Lude.Text)
ppProductName = Lens.lens (productName :: ProvisionProduct -> Lude.Maybe Lude.Text) (\s a -> s {productName = a} :: ProvisionProduct)
{-# DEPRECATED ppProductName "Use generic-lens or generic-optics with 'productName' instead." #-}

-- | The identifier of the provisioning artifact. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProvisioningArtifactId :: Lens.Lens' ProvisionProduct (Lude.Maybe Lude.Text)
ppProvisioningArtifactId = Lens.lens (provisioningArtifactId :: ProvisionProduct -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactId = a} :: ProvisionProduct)
{-# DEPRECATED ppProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The name of the provisioning artifact. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'provisioningArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProvisioningArtifactName :: Lens.Lens' ProvisionProduct (Lude.Maybe Lude.Text)
ppProvisioningArtifactName = Lens.lens (provisioningArtifactName :: ProvisionProduct -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactName = a} :: ProvisionProduct)
{-# DEPRECATED ppProvisioningArtifactName "Use generic-lens or generic-optics with 'provisioningArtifactName' instead." #-}

-- | Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppNotificationARNs :: Lens.Lens' ProvisionProduct (Lude.Maybe [Lude.Text])
ppNotificationARNs = Lens.lens (notificationARNs :: ProvisionProduct -> Lude.Maybe [Lude.Text]) (\s a -> s {notificationARNs = a} :: ProvisionProduct)
{-# DEPRECATED ppNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | The name of the path. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'pathName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPathName :: Lens.Lens' ProvisionProduct (Lude.Maybe Lude.Text)
ppPathName = Lens.lens (pathName :: ProvisionProduct -> Lude.Maybe Lude.Text) (\s a -> s {pathName = a} :: ProvisionProduct)
{-# DEPRECATED ppPathName "Use generic-lens or generic-optics with 'pathName' instead." #-}

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
ppAcceptLanguage :: Lens.Lens' ProvisionProduct (Lude.Maybe Lude.Text)
ppAcceptLanguage = Lens.lens (acceptLanguage :: ProvisionProduct -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ProvisionProduct)
{-# DEPRECATED ppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' . You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'pathId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPathId :: Lens.Lens' ProvisionProduct (Lude.Maybe Lude.Text)
ppPathId = Lens.lens (pathId :: ProvisionProduct -> Lude.Maybe Lude.Text) (\s a -> s {pathId = a} :: ProvisionProduct)
{-# DEPRECATED ppPathId "Use generic-lens or generic-optics with 'pathId' instead." #-}

-- | Parameters specified by the administrator that are required for provisioning the product.
--
-- /Note:/ Consider using 'provisioningParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProvisioningParameters :: Lens.Lens' ProvisionProduct (Lude.Maybe [ProvisioningParameter])
ppProvisioningParameters = Lens.lens (provisioningParameters :: ProvisionProduct -> Lude.Maybe [ProvisioningParameter]) (\s a -> s {provisioningParameters = a} :: ProvisionProduct)
{-# DEPRECATED ppProvisioningParameters "Use generic-lens or generic-optics with 'provisioningParameters' instead." #-}

-- | The product identifier. You must provide the name or ID, but not both.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProductId :: Lens.Lens' ProvisionProduct (Lude.Maybe Lude.Text)
ppProductId = Lens.lens (productId :: ProvisionProduct -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: ProvisionProduct)
{-# DEPRECATED ppProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppTags :: Lens.Lens' ProvisionProduct (Lude.Maybe [Tag])
ppTags = Lens.lens (tags :: ProvisionProduct -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ProvisionProduct)
{-# DEPRECATED ppTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | An object that contains information about the provisioning preferences for a stack set.
--
-- /Note:/ Consider using 'provisioningPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProvisioningPreferences :: Lens.Lens' ProvisionProduct (Lude.Maybe ProvisioningPreferences)
ppProvisioningPreferences = Lens.lens (provisioningPreferences :: ProvisionProduct -> Lude.Maybe ProvisioningPreferences) (\s a -> s {provisioningPreferences = a} :: ProvisionProduct)
{-# DEPRECATED ppProvisioningPreferences "Use generic-lens or generic-optics with 'provisioningPreferences' instead." #-}

-- | A user-friendly name for the provisioned product. This value must be unique for the AWS account and cannot be updated after the product is provisioned.
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProvisionedProductName :: Lens.Lens' ProvisionProduct Lude.Text
ppProvisionedProductName = Lens.lens (provisionedProductName :: ProvisionProduct -> Lude.Text) (\s a -> s {provisionedProductName = a} :: ProvisionProduct)
{-# DEPRECATED ppProvisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead." #-}

-- | An idempotency token that uniquely identifies the provisioning request.
--
-- /Note:/ Consider using 'provisionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppProvisionToken :: Lens.Lens' ProvisionProduct Lude.Text
ppProvisionToken = Lens.lens (provisionToken :: ProvisionProduct -> Lude.Text) (\s a -> s {provisionToken = a} :: ProvisionProduct)
{-# DEPRECATED ppProvisionToken "Use generic-lens or generic-optics with 'provisionToken' instead." #-}

instance Lude.AWSRequest ProvisionProduct where
  type Rs ProvisionProduct = ProvisionProductResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ProvisionProductResponse'
            Lude.<$> (x Lude..?> "RecordDetail") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ProvisionProduct where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ProvisionProduct" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ProvisionProduct where
  toJSON ProvisionProduct' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProductName" Lude..=) Lude.<$> productName,
            ("ProvisioningArtifactId" Lude..=) Lude.<$> provisioningArtifactId,
            ("ProvisioningArtifactName" Lude..=)
              Lude.<$> provisioningArtifactName,
            ("NotificationArns" Lude..=) Lude.<$> notificationARNs,
            ("PathName" Lude..=) Lude.<$> pathName,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("PathId" Lude..=) Lude.<$> pathId,
            ("ProvisioningParameters" Lude..=) Lude.<$> provisioningParameters,
            ("ProductId" Lude..=) Lude.<$> productId,
            ("Tags" Lude..=) Lude.<$> tags,
            ("ProvisioningPreferences" Lude..=)
              Lude.<$> provisioningPreferences,
            Lude.Just
              ("ProvisionedProductName" Lude..= provisionedProductName),
            Lude.Just ("ProvisionToken" Lude..= provisionToken)
          ]
      )

instance Lude.ToPath ProvisionProduct where
  toPath = Lude.const "/"

instance Lude.ToQuery ProvisionProduct where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkProvisionProductResponse' smart constructor.
data ProvisionProductResponse = ProvisionProductResponse'
  { recordDetail ::
      Lude.Maybe RecordDetail,
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

-- | Creates a value of 'ProvisionProductResponse' with the minimum fields required to make a request.
--
-- * 'recordDetail' - Information about the result of provisioning the product.
-- * 'responseStatus' - The response status code.
mkProvisionProductResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ProvisionProductResponse
mkProvisionProductResponse pResponseStatus_ =
  ProvisionProductResponse'
    { recordDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the result of provisioning the product.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprsRecordDetail :: Lens.Lens' ProvisionProductResponse (Lude.Maybe RecordDetail)
pprsRecordDetail = Lens.lens (recordDetail :: ProvisionProductResponse -> Lude.Maybe RecordDetail) (\s a -> s {recordDetail = a} :: ProvisionProductResponse)
{-# DEPRECATED pprsRecordDetail "Use generic-lens or generic-optics with 'recordDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprsResponseStatus :: Lens.Lens' ProvisionProductResponse Lude.Int
pprsResponseStatus = Lens.lens (responseStatus :: ProvisionProductResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ProvisionProductResponse)
{-# DEPRECATED pprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
