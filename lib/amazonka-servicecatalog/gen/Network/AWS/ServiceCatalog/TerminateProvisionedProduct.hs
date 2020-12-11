{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.TerminateProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified provisioned product.
--
-- This operation does not delete any records associated with the provisioned product.
-- You can check the status of this request using 'DescribeRecord' .
module Network.AWS.ServiceCatalog.TerminateProvisionedProduct
  ( -- * Creating a request
    TerminateProvisionedProduct (..),
    mkTerminateProvisionedProduct,

    -- ** Request lenses
    tppProvisionedProductName,
    tppRetainPhysicalResources,
    tppAcceptLanguage,
    tppIgnoreErrors,
    tppProvisionedProductId,
    tppTerminateToken,

    -- * Destructuring the response
    TerminateProvisionedProductResponse (..),
    mkTerminateProvisionedProductResponse,

    -- ** Response lenses
    tpprsRecordDetail,
    tpprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkTerminateProvisionedProduct' smart constructor.
data TerminateProvisionedProduct = TerminateProvisionedProduct'
  { provisionedProductName ::
      Lude.Maybe Lude.Text,
    retainPhysicalResources ::
      Lude.Maybe Lude.Bool,
    acceptLanguage ::
      Lude.Maybe Lude.Text,
    ignoreErrors ::
      Lude.Maybe Lude.Bool,
    provisionedProductId ::
      Lude.Maybe Lude.Text,
    terminateToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateProvisionedProduct' with the minimum fields required to make a request.
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
-- * 'ignoreErrors' - If set to true, AWS Service Catalog stops managing the specified provisioned product even if it cannot delete the underlying resources.
-- * 'provisionedProductId' - The identifier of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
-- * 'provisionedProductName' - The name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
-- * 'retainPhysicalResources' - When this boolean parameter is set to true, the TerminateProvisionedProduct API deletes the Service Catalog provisioned product. However, it does not remove the CloudFormation stack, stack set, or the underlying resources of the deleted provisioned product. The default value is false.
-- * 'terminateToken' - An idempotency token that uniquely identifies the termination request. This token is only valid during the termination process. After the provisioned product is terminated, subsequent requests to terminate the same provisioned product always return __ResourceNotFound__ .
mkTerminateProvisionedProduct ::
  -- | 'terminateToken'
  Lude.Text ->
  TerminateProvisionedProduct
mkTerminateProvisionedProduct pTerminateToken_ =
  TerminateProvisionedProduct'
    { provisionedProductName =
        Lude.Nothing,
      retainPhysicalResources = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      ignoreErrors = Lude.Nothing,
      provisionedProductId = Lude.Nothing,
      terminateToken = pTerminateToken_
    }

-- | The name of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppProvisionedProductName :: Lens.Lens' TerminateProvisionedProduct (Lude.Maybe Lude.Text)
tppProvisionedProductName = Lens.lens (provisionedProductName :: TerminateProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {provisionedProductName = a} :: TerminateProvisionedProduct)
{-# DEPRECATED tppProvisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead." #-}

-- | When this boolean parameter is set to true, the TerminateProvisionedProduct API deletes the Service Catalog provisioned product. However, it does not remove the CloudFormation stack, stack set, or the underlying resources of the deleted provisioned product. The default value is false.
--
-- /Note:/ Consider using 'retainPhysicalResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppRetainPhysicalResources :: Lens.Lens' TerminateProvisionedProduct (Lude.Maybe Lude.Bool)
tppRetainPhysicalResources = Lens.lens (retainPhysicalResources :: TerminateProvisionedProduct -> Lude.Maybe Lude.Bool) (\s a -> s {retainPhysicalResources = a} :: TerminateProvisionedProduct)
{-# DEPRECATED tppRetainPhysicalResources "Use generic-lens or generic-optics with 'retainPhysicalResources' instead." #-}

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
tppAcceptLanguage :: Lens.Lens' TerminateProvisionedProduct (Lude.Maybe Lude.Text)
tppAcceptLanguage = Lens.lens (acceptLanguage :: TerminateProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: TerminateProvisionedProduct)
{-# DEPRECATED tppAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | If set to true, AWS Service Catalog stops managing the specified provisioned product even if it cannot delete the underlying resources.
--
-- /Note:/ Consider using 'ignoreErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppIgnoreErrors :: Lens.Lens' TerminateProvisionedProduct (Lude.Maybe Lude.Bool)
tppIgnoreErrors = Lens.lens (ignoreErrors :: TerminateProvisionedProduct -> Lude.Maybe Lude.Bool) (\s a -> s {ignoreErrors = a} :: TerminateProvisionedProduct)
{-# DEPRECATED tppIgnoreErrors "Use generic-lens or generic-optics with 'ignoreErrors' instead." #-}

-- | The identifier of the provisioned product. You cannot specify both @ProvisionedProductName@ and @ProvisionedProductId@ .
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppProvisionedProductId :: Lens.Lens' TerminateProvisionedProduct (Lude.Maybe Lude.Text)
tppProvisionedProductId = Lens.lens (provisionedProductId :: TerminateProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {provisionedProductId = a} :: TerminateProvisionedProduct)
{-# DEPRECATED tppProvisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead." #-}

-- | An idempotency token that uniquely identifies the termination request. This token is only valid during the termination process. After the provisioned product is terminated, subsequent requests to terminate the same provisioned product always return __ResourceNotFound__ .
--
-- /Note:/ Consider using 'terminateToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tppTerminateToken :: Lens.Lens' TerminateProvisionedProduct Lude.Text
tppTerminateToken = Lens.lens (terminateToken :: TerminateProvisionedProduct -> Lude.Text) (\s a -> s {terminateToken = a} :: TerminateProvisionedProduct)
{-# DEPRECATED tppTerminateToken "Use generic-lens or generic-optics with 'terminateToken' instead." #-}

instance Lude.AWSRequest TerminateProvisionedProduct where
  type
    Rs TerminateProvisionedProduct =
      TerminateProvisionedProductResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          TerminateProvisionedProductResponse'
            Lude.<$> (x Lude..?> "RecordDetail") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TerminateProvisionedProduct where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.TerminateProvisionedProduct" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TerminateProvisionedProduct where
  toJSON TerminateProvisionedProduct' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProvisionedProductName" Lude..=)
              Lude.<$> provisionedProductName,
            ("RetainPhysicalResources" Lude..=)
              Lude.<$> retainPhysicalResources,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            ("IgnoreErrors" Lude..=) Lude.<$> ignoreErrors,
            ("ProvisionedProductId" Lude..=) Lude.<$> provisionedProductId,
            Lude.Just ("TerminateToken" Lude..= terminateToken)
          ]
      )

instance Lude.ToPath TerminateProvisionedProduct where
  toPath = Lude.const "/"

instance Lude.ToQuery TerminateProvisionedProduct where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTerminateProvisionedProductResponse' smart constructor.
data TerminateProvisionedProductResponse = TerminateProvisionedProductResponse'
  { recordDetail ::
      Lude.Maybe
        RecordDetail,
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

-- | Creates a value of 'TerminateProvisionedProductResponse' with the minimum fields required to make a request.
--
-- * 'recordDetail' - Information about the result of this request.
-- * 'responseStatus' - The response status code.
mkTerminateProvisionedProductResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TerminateProvisionedProductResponse
mkTerminateProvisionedProductResponse pResponseStatus_ =
  TerminateProvisionedProductResponse'
    { recordDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the result of this request.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpprsRecordDetail :: Lens.Lens' TerminateProvisionedProductResponse (Lude.Maybe RecordDetail)
tpprsRecordDetail = Lens.lens (recordDetail :: TerminateProvisionedProductResponse -> Lude.Maybe RecordDetail) (\s a -> s {recordDetail = a} :: TerminateProvisionedProductResponse)
{-# DEPRECATED tpprsRecordDetail "Use generic-lens or generic-optics with 'recordDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpprsResponseStatus :: Lens.Lens' TerminateProvisionedProductResponse Lude.Int
tpprsResponseStatus = Lens.lens (responseStatus :: TerminateProvisionedProductResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TerminateProvisionedProductResponse)
{-# DEPRECATED tpprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
