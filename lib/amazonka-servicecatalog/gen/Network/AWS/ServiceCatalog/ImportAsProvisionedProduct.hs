{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ImportAsProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests the import of a resource as a Service Catalog provisioned product that is associated to a Service Catalog product and provisioning artifact. Once imported all supported Service Catalog governance actions are supported on the provisioned product.
--
-- Resource import only supports CloudFormation stack ARNs. CloudFormation StackSets and non-root nested stacks are not supported.
-- The CloudFormation stack must have one of the following statuses to be imported: CREATE_COMPLETE, UPDATE_COMPLETE, UPDATE_ROLLBACK_COMPLETE, IMPORT_COMPLETE, IMPORT_ROLLBACK_COMPLETE.
-- Import of the resource requires that the CloudFormation stack template matches the associated Service Catalog product provisioning artifact.
module Network.AWS.ServiceCatalog.ImportAsProvisionedProduct
  ( -- * Creating a request
    ImportAsProvisionedProduct (..),
    mkImportAsProvisionedProduct,

    -- ** Request lenses
    iappAcceptLanguage,
    iappProductId,
    iappProvisioningArtifactId,
    iappProvisionedProductName,
    iappPhysicalId,
    iappIdempotencyToken,

    -- * Destructuring the response
    ImportAsProvisionedProductResponse (..),
    mkImportAsProvisionedProductResponse,

    -- ** Response lenses
    iapprsRecordDetail,
    iapprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkImportAsProvisionedProduct' smart constructor.
data ImportAsProvisionedProduct = ImportAsProvisionedProduct'
  { acceptLanguage ::
      Lude.Maybe Lude.Text,
    productId :: Lude.Text,
    provisioningArtifactId :: Lude.Text,
    provisionedProductName :: Lude.Text,
    physicalId :: Lude.Text,
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

-- | Creates a value of 'ImportAsProvisionedProduct' with the minimum fields required to make a request.
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
-- * 'idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
-- * 'physicalId' - The unique identifier of the resource to be imported. It only currently supports CloudFormation stack IDs.
-- * 'productId' - The product identifier.
-- * 'provisionedProductName' - The user-friendly name of the provisioned product. The value must be unique for the AWS account. The name cannot be updated after the product is provisioned.
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact.
mkImportAsProvisionedProduct ::
  -- | 'productId'
  Lude.Text ->
  -- | 'provisioningArtifactId'
  Lude.Text ->
  -- | 'provisionedProductName'
  Lude.Text ->
  -- | 'physicalId'
  Lude.Text ->
  -- | 'idempotencyToken'
  Lude.Text ->
  ImportAsProvisionedProduct
mkImportAsProvisionedProduct
  pProductId_
  pProvisioningArtifactId_
  pProvisionedProductName_
  pPhysicalId_
  pIdempotencyToken_ =
    ImportAsProvisionedProduct'
      { acceptLanguage = Lude.Nothing,
        productId = pProductId_,
        provisioningArtifactId = pProvisioningArtifactId_,
        provisionedProductName = pProvisionedProductName_,
        physicalId = pPhysicalId_,
        idempotencyToken = pIdempotencyToken_
      }

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
iappAcceptLanguage :: Lens.Lens' ImportAsProvisionedProduct (Lude.Maybe Lude.Text)
iappAcceptLanguage = Lens.lens (acceptLanguage :: ImportAsProvisionedProduct -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: ImportAsProvisionedProduct)
{-# DEPRECATED iappAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iappProductId :: Lens.Lens' ImportAsProvisionedProduct Lude.Text
iappProductId = Lens.lens (productId :: ImportAsProvisionedProduct -> Lude.Text) (\s a -> s {productId = a} :: ImportAsProvisionedProduct)
{-# DEPRECATED iappProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iappProvisioningArtifactId :: Lens.Lens' ImportAsProvisionedProduct Lude.Text
iappProvisioningArtifactId = Lens.lens (provisioningArtifactId :: ImportAsProvisionedProduct -> Lude.Text) (\s a -> s {provisioningArtifactId = a} :: ImportAsProvisionedProduct)
{-# DEPRECATED iappProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The user-friendly name of the provisioned product. The value must be unique for the AWS account. The name cannot be updated after the product is provisioned.
--
-- /Note:/ Consider using 'provisionedProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iappProvisionedProductName :: Lens.Lens' ImportAsProvisionedProduct Lude.Text
iappProvisionedProductName = Lens.lens (provisionedProductName :: ImportAsProvisionedProduct -> Lude.Text) (\s a -> s {provisionedProductName = a} :: ImportAsProvisionedProduct)
{-# DEPRECATED iappProvisionedProductName "Use generic-lens or generic-optics with 'provisionedProductName' instead." #-}

-- | The unique identifier of the resource to be imported. It only currently supports CloudFormation stack IDs.
--
-- /Note:/ Consider using 'physicalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iappPhysicalId :: Lens.Lens' ImportAsProvisionedProduct Lude.Text
iappPhysicalId = Lens.lens (physicalId :: ImportAsProvisionedProduct -> Lude.Text) (\s a -> s {physicalId = a} :: ImportAsProvisionedProduct)
{-# DEPRECATED iappPhysicalId "Use generic-lens or generic-optics with 'physicalId' instead." #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iappIdempotencyToken :: Lens.Lens' ImportAsProvisionedProduct Lude.Text
iappIdempotencyToken = Lens.lens (idempotencyToken :: ImportAsProvisionedProduct -> Lude.Text) (\s a -> s {idempotencyToken = a} :: ImportAsProvisionedProduct)
{-# DEPRECATED iappIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

instance Lude.AWSRequest ImportAsProvisionedProduct where
  type
    Rs ImportAsProvisionedProduct =
      ImportAsProvisionedProductResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          ImportAsProvisionedProductResponse'
            Lude.<$> (x Lude..?> "RecordDetail") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ImportAsProvisionedProduct where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.ImportAsProvisionedProduct" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ImportAsProvisionedProduct where
  toJSON ImportAsProvisionedProduct' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("ProductId" Lude..= productId),
            Lude.Just
              ("ProvisioningArtifactId" Lude..= provisioningArtifactId),
            Lude.Just
              ("ProvisionedProductName" Lude..= provisionedProductName),
            Lude.Just ("PhysicalId" Lude..= physicalId),
            Lude.Just ("IdempotencyToken" Lude..= idempotencyToken)
          ]
      )

instance Lude.ToPath ImportAsProvisionedProduct where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportAsProvisionedProduct where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkImportAsProvisionedProductResponse' smart constructor.
data ImportAsProvisionedProductResponse = ImportAsProvisionedProductResponse'
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

-- | Creates a value of 'ImportAsProvisionedProductResponse' with the minimum fields required to make a request.
--
-- * 'recordDetail' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkImportAsProvisionedProductResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportAsProvisionedProductResponse
mkImportAsProvisionedProductResponse pResponseStatus_ =
  ImportAsProvisionedProductResponse'
    { recordDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'recordDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapprsRecordDetail :: Lens.Lens' ImportAsProvisionedProductResponse (Lude.Maybe RecordDetail)
iapprsRecordDetail = Lens.lens (recordDetail :: ImportAsProvisionedProductResponse -> Lude.Maybe RecordDetail) (\s a -> s {recordDetail = a} :: ImportAsProvisionedProductResponse)
{-# DEPRECATED iapprsRecordDetail "Use generic-lens or generic-optics with 'recordDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iapprsResponseStatus :: Lens.Lens' ImportAsProvisionedProductResponse Lude.Int
iapprsResponseStatus = Lens.lens (responseStatus :: ImportAsProvisionedProductResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportAsProvisionedProductResponse)
{-# DEPRECATED iapprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
