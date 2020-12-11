{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreateProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a provisioning artifact (also known as a version) for the specified product.
--
-- You cannot create a provisioning artifact for a product that was shared with you.
module Network.AWS.ServiceCatalog.CreateProvisioningArtifact
  ( -- * Creating a request
    CreateProvisioningArtifact (..),
    mkCreateProvisioningArtifact,

    -- ** Request lenses
    cpaAcceptLanguage,
    cpaProductId,
    cpaParameters,
    cpaIdempotencyToken,

    -- * Destructuring the response
    CreateProvisioningArtifactResponse (..),
    mkCreateProvisioningArtifactResponse,

    -- ** Response lenses
    cparsStatus,
    cparsInfo,
    cparsProvisioningArtifactDetail,
    cparsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkCreateProvisioningArtifact' smart constructor.
data CreateProvisioningArtifact = CreateProvisioningArtifact'
  { acceptLanguage ::
      Lude.Maybe Lude.Text,
    productId :: Lude.Text,
    parameters ::
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

-- | Creates a value of 'CreateProvisioningArtifact' with the minimum fields required to make a request.
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
-- * 'parameters' - The configuration for the provisioning artifact. The @info@ field accepts @ImportFromPhysicalID@ .
-- * 'productId' - The product identifier.
mkCreateProvisioningArtifact ::
  -- | 'productId'
  Lude.Text ->
  -- | 'parameters'
  ProvisioningArtifactProperties ->
  -- | 'idempotencyToken'
  Lude.Text ->
  CreateProvisioningArtifact
mkCreateProvisioningArtifact
  pProductId_
  pParameters_
  pIdempotencyToken_ =
    CreateProvisioningArtifact'
      { acceptLanguage = Lude.Nothing,
        productId = pProductId_,
        parameters = pParameters_,
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
cpaAcceptLanguage :: Lens.Lens' CreateProvisioningArtifact (Lude.Maybe Lude.Text)
cpaAcceptLanguage = Lens.lens (acceptLanguage :: CreateProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: CreateProvisioningArtifact)
{-# DEPRECATED cpaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpaProductId :: Lens.Lens' CreateProvisioningArtifact Lude.Text
cpaProductId = Lens.lens (productId :: CreateProvisioningArtifact -> Lude.Text) (\s a -> s {productId = a} :: CreateProvisioningArtifact)
{-# DEPRECATED cpaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The configuration for the provisioning artifact. The @info@ field accepts @ImportFromPhysicalID@ .
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpaParameters :: Lens.Lens' CreateProvisioningArtifact ProvisioningArtifactProperties
cpaParameters = Lens.lens (parameters :: CreateProvisioningArtifact -> ProvisioningArtifactProperties) (\s a -> s {parameters = a} :: CreateProvisioningArtifact)
{-# DEPRECATED cpaParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpaIdempotencyToken :: Lens.Lens' CreateProvisioningArtifact Lude.Text
cpaIdempotencyToken = Lens.lens (idempotencyToken :: CreateProvisioningArtifact -> Lude.Text) (\s a -> s {idempotencyToken = a} :: CreateProvisioningArtifact)
{-# DEPRECATED cpaIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

instance Lude.AWSRequest CreateProvisioningArtifact where
  type
    Rs CreateProvisioningArtifact =
      CreateProvisioningArtifactResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProvisioningArtifactResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "Info" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ProvisioningArtifactDetail")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateProvisioningArtifact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.CreateProvisioningArtifact" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateProvisioningArtifact where
  toJSON CreateProvisioningArtifact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("ProductId" Lude..= productId),
            Lude.Just ("Parameters" Lude..= parameters),
            Lude.Just ("IdempotencyToken" Lude..= idempotencyToken)
          ]
      )

instance Lude.ToPath CreateProvisioningArtifact where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateProvisioningArtifact where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateProvisioningArtifactResponse' smart constructor.
data CreateProvisioningArtifactResponse = CreateProvisioningArtifactResponse'
  { status ::
      Lude.Maybe
        RequestStatus,
    info ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    provisioningArtifactDetail ::
      Lude.Maybe
        ProvisioningArtifactDetail,
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

-- | Creates a value of 'CreateProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- * 'info' - The URL of the CloudFormation template in Amazon S3, in JSON format.
-- * 'provisioningArtifactDetail' - Information about the provisioning artifact.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the current request.
mkCreateProvisioningArtifactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProvisioningArtifactResponse
mkCreateProvisioningArtifactResponse pResponseStatus_ =
  CreateProvisioningArtifactResponse'
    { status = Lude.Nothing,
      info = Lude.Nothing,
      provisioningArtifactDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the current request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cparsStatus :: Lens.Lens' CreateProvisioningArtifactResponse (Lude.Maybe RequestStatus)
cparsStatus = Lens.lens (status :: CreateProvisioningArtifactResponse -> Lude.Maybe RequestStatus) (\s a -> s {status = a} :: CreateProvisioningArtifactResponse)
{-# DEPRECATED cparsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The URL of the CloudFormation template in Amazon S3, in JSON format.
--
-- /Note:/ Consider using 'info' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cparsInfo :: Lens.Lens' CreateProvisioningArtifactResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cparsInfo = Lens.lens (info :: CreateProvisioningArtifactResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {info = a} :: CreateProvisioningArtifactResponse)
{-# DEPRECATED cparsInfo "Use generic-lens or generic-optics with 'info' instead." #-}

-- | Information about the provisioning artifact.
--
-- /Note:/ Consider using 'provisioningArtifactDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cparsProvisioningArtifactDetail :: Lens.Lens' CreateProvisioningArtifactResponse (Lude.Maybe ProvisioningArtifactDetail)
cparsProvisioningArtifactDetail = Lens.lens (provisioningArtifactDetail :: CreateProvisioningArtifactResponse -> Lude.Maybe ProvisioningArtifactDetail) (\s a -> s {provisioningArtifactDetail = a} :: CreateProvisioningArtifactResponse)
{-# DEPRECATED cparsProvisioningArtifactDetail "Use generic-lens or generic-optics with 'provisioningArtifactDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cparsResponseStatus :: Lens.Lens' CreateProvisioningArtifactResponse Lude.Int
cparsResponseStatus = Lens.lens (responseStatus :: CreateProvisioningArtifactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProvisioningArtifactResponse)
{-# DEPRECATED cparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
