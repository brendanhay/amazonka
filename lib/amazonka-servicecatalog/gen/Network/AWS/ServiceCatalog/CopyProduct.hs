{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CopyProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified source product to the specified target product or a new product.
--
-- You can copy a product to the same account or another account. You can copy a product to the same region or another region.
-- This operation is performed asynchronously. To track the progress of the operation, use 'DescribeCopyProductStatus' .
module Network.AWS.ServiceCatalog.CopyProduct
  ( -- * Creating a request
    CopyProduct (..),
    mkCopyProduct,

    -- ** Request lenses
    cIdempotencyToken,
    cTargetProductId,
    cSourceProvisioningArtifactIdentifiers,
    cTargetProductName,
    cCopyOptions,
    cAcceptLanguage,
    cSourceProductARN,

    -- * Destructuring the response
    CopyProductResponse (..),
    mkCopyProductResponse,

    -- ** Response lenses
    cpfrsCopyProductToken,
    cpfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkCopyProduct' smart constructor.
data CopyProduct = CopyProduct'
  { -- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
    idempotencyToken :: Lude.Text,
    -- | The identifier of the target product. By default, a new product is created.
    targetProductId :: Lude.Maybe Lude.Text,
    -- | The identifiers of the provisioning artifacts (also known as versions) of the product to copy. By default, all provisioning artifacts are copied.
    sourceProvisioningArtifactIdentifiers :: Lude.Maybe [Lude.HashMap ProvisioningArtifactPropertyName (Lude.Text)],
    -- | A name for the target product. The default is the name of the source product.
    targetProductName :: Lude.Maybe Lude.Text,
    -- | The copy options. If the value is @CopyTags@ , the tags from the source product are copied to the target product.
    copyOptions :: Lude.Maybe [CopyOption],
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
    acceptLanguage :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the source product.
    sourceProductARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyProduct' with the minimum fields required to make a request.
--
-- * 'idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
-- * 'targetProductId' - The identifier of the target product. By default, a new product is created.
-- * 'sourceProvisioningArtifactIdentifiers' - The identifiers of the provisioning artifacts (also known as versions) of the product to copy. By default, all provisioning artifacts are copied.
-- * 'targetProductName' - A name for the target product. The default is the name of the source product.
-- * 'copyOptions' - The copy options. If the value is @CopyTags@ , the tags from the source product are copied to the target product.
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
-- * 'sourceProductARN' - The Amazon Resource Name (ARN) of the source product.
mkCopyProduct ::
  -- | 'idempotencyToken'
  Lude.Text ->
  -- | 'sourceProductARN'
  Lude.Text ->
  CopyProduct
mkCopyProduct pIdempotencyToken_ pSourceProductARN_ =
  CopyProduct'
    { idempotencyToken = pIdempotencyToken_,
      targetProductId = Lude.Nothing,
      sourceProvisioningArtifactIdentifiers = Lude.Nothing,
      targetProductName = Lude.Nothing,
      copyOptions = Lude.Nothing,
      acceptLanguage = Lude.Nothing,
      sourceProductARN = pSourceProductARN_
    }

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIdempotencyToken :: Lens.Lens' CopyProduct Lude.Text
cIdempotencyToken = Lens.lens (idempotencyToken :: CopyProduct -> Lude.Text) (\s a -> s {idempotencyToken = a} :: CopyProduct)
{-# DEPRECATED cIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | The identifier of the target product. By default, a new product is created.
--
-- /Note:/ Consider using 'targetProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTargetProductId :: Lens.Lens' CopyProduct (Lude.Maybe Lude.Text)
cTargetProductId = Lens.lens (targetProductId :: CopyProduct -> Lude.Maybe Lude.Text) (\s a -> s {targetProductId = a} :: CopyProduct)
{-# DEPRECATED cTargetProductId "Use generic-lens or generic-optics with 'targetProductId' instead." #-}

-- | The identifiers of the provisioning artifacts (also known as versions) of the product to copy. By default, all provisioning artifacts are copied.
--
-- /Note:/ Consider using 'sourceProvisioningArtifactIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSourceProvisioningArtifactIdentifiers :: Lens.Lens' CopyProduct (Lude.Maybe [Lude.HashMap ProvisioningArtifactPropertyName (Lude.Text)])
cSourceProvisioningArtifactIdentifiers = Lens.lens (sourceProvisioningArtifactIdentifiers :: CopyProduct -> Lude.Maybe [Lude.HashMap ProvisioningArtifactPropertyName (Lude.Text)]) (\s a -> s {sourceProvisioningArtifactIdentifiers = a} :: CopyProduct)
{-# DEPRECATED cSourceProvisioningArtifactIdentifiers "Use generic-lens or generic-optics with 'sourceProvisioningArtifactIdentifiers' instead." #-}

-- | A name for the target product. The default is the name of the source product.
--
-- /Note:/ Consider using 'targetProductName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTargetProductName :: Lens.Lens' CopyProduct (Lude.Maybe Lude.Text)
cTargetProductName = Lens.lens (targetProductName :: CopyProduct -> Lude.Maybe Lude.Text) (\s a -> s {targetProductName = a} :: CopyProduct)
{-# DEPRECATED cTargetProductName "Use generic-lens or generic-optics with 'targetProductName' instead." #-}

-- | The copy options. If the value is @CopyTags@ , the tags from the source product are copied to the target product.
--
-- /Note:/ Consider using 'copyOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCopyOptions :: Lens.Lens' CopyProduct (Lude.Maybe [CopyOption])
cCopyOptions = Lens.lens (copyOptions :: CopyProduct -> Lude.Maybe [CopyOption]) (\s a -> s {copyOptions = a} :: CopyProduct)
{-# DEPRECATED cCopyOptions "Use generic-lens or generic-optics with 'copyOptions' instead." #-}

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
cAcceptLanguage :: Lens.Lens' CopyProduct (Lude.Maybe Lude.Text)
cAcceptLanguage = Lens.lens (acceptLanguage :: CopyProduct -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: CopyProduct)
{-# DEPRECATED cAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The Amazon Resource Name (ARN) of the source product.
--
-- /Note:/ Consider using 'sourceProductARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSourceProductARN :: Lens.Lens' CopyProduct Lude.Text
cSourceProductARN = Lens.lens (sourceProductARN :: CopyProduct -> Lude.Text) (\s a -> s {sourceProductARN = a} :: CopyProduct)
{-# DEPRECATED cSourceProductARN "Use generic-lens or generic-optics with 'sourceProductARN' instead." #-}

instance Lude.AWSRequest CopyProduct where
  type Rs CopyProduct = CopyProductResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          CopyProductResponse'
            Lude.<$> (x Lude..?> "CopyProductToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopyProduct where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWS242ServiceCatalogService.CopyProduct" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CopyProduct where
  toJSON CopyProduct' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("IdempotencyToken" Lude..= idempotencyToken),
            ("TargetProductId" Lude..=) Lude.<$> targetProductId,
            ("SourceProvisioningArtifactIdentifiers" Lude..=)
              Lude.<$> sourceProvisioningArtifactIdentifiers,
            ("TargetProductName" Lude..=) Lude.<$> targetProductName,
            ("CopyOptions" Lude..=) Lude.<$> copyOptions,
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("SourceProductArn" Lude..= sourceProductARN)
          ]
      )

instance Lude.ToPath CopyProduct where
  toPath = Lude.const "/"

instance Lude.ToQuery CopyProduct where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCopyProductResponse' smart constructor.
data CopyProductResponse = CopyProductResponse'
  { -- | The token to use to track the progress of the operation.
    copyProductToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyProductResponse' with the minimum fields required to make a request.
--
-- * 'copyProductToken' - The token to use to track the progress of the operation.
-- * 'responseStatus' - The response status code.
mkCopyProductResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopyProductResponse
mkCopyProductResponse pResponseStatus_ =
  CopyProductResponse'
    { copyProductToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to track the progress of the operation.
--
-- /Note:/ Consider using 'copyProductToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfrsCopyProductToken :: Lens.Lens' CopyProductResponse (Lude.Maybe Lude.Text)
cpfrsCopyProductToken = Lens.lens (copyProductToken :: CopyProductResponse -> Lude.Maybe Lude.Text) (\s a -> s {copyProductToken = a} :: CopyProductResponse)
{-# DEPRECATED cpfrsCopyProductToken "Use generic-lens or generic-optics with 'copyProductToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfrsResponseStatus :: Lens.Lens' CopyProductResponse Lude.Int
cpfrsResponseStatus = Lens.lens (responseStatus :: CopyProductResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopyProductResponse)
{-# DEPRECATED cpfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
