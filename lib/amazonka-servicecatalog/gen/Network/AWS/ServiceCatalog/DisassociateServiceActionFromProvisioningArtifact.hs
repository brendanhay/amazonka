{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DisassociateServiceActionFromProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified self-service action association from the specified provisioning artifact.
module Network.AWS.ServiceCatalog.DisassociateServiceActionFromProvisioningArtifact
  ( -- * Creating a request
    DisassociateServiceActionFromProvisioningArtifact (..),
    mkDisassociateServiceActionFromProvisioningArtifact,

    -- ** Request lenses
    dsafpaProvisioningArtifactId,
    dsafpaAcceptLanguage,
    dsafpaServiceActionId,
    dsafpaProductId,

    -- * Destructuring the response
    DisassociateServiceActionFromProvisioningArtifactResponse (..),
    mkDisassociateServiceActionFromProvisioningArtifactResponse,

    -- ** Response lenses
    dsafparsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDisassociateServiceActionFromProvisioningArtifact' smart constructor.
data DisassociateServiceActionFromProvisioningArtifact = DisassociateServiceActionFromProvisioningArtifact'
  { -- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
    provisioningArtifactId :: Lude.Text,
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
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
    serviceActionId :: Lude.Text,
    -- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
    productId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateServiceActionFromProvisioningArtifact' with the minimum fields required to make a request.
--
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
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
-- * 'serviceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
-- * 'productId' - The product identifier. For example, @prod-abcdzk7xy33qa@ .
mkDisassociateServiceActionFromProvisioningArtifact ::
  -- | 'provisioningArtifactId'
  Lude.Text ->
  -- | 'serviceActionId'
  Lude.Text ->
  -- | 'productId'
  Lude.Text ->
  DisassociateServiceActionFromProvisioningArtifact
mkDisassociateServiceActionFromProvisioningArtifact
  pProvisioningArtifactId_
  pServiceActionId_
  pProductId_ =
    DisassociateServiceActionFromProvisioningArtifact'
      { provisioningArtifactId =
          pProvisioningArtifactId_,
        acceptLanguage = Lude.Nothing,
        serviceActionId = pServiceActionId_,
        productId = pProductId_
      }

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafpaProvisioningArtifactId :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact Lude.Text
dsafpaProvisioningArtifactId = Lens.lens (provisioningArtifactId :: DisassociateServiceActionFromProvisioningArtifact -> Lude.Text) (\s a -> s {provisioningArtifactId = a} :: DisassociateServiceActionFromProvisioningArtifact)
{-# DEPRECATED dsafpaProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

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
dsafpaAcceptLanguage :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact (Lude.Maybe Lude.Text)
dsafpaAcceptLanguage = Lens.lens (acceptLanguage :: DisassociateServiceActionFromProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DisassociateServiceActionFromProvisioningArtifact)
{-# DEPRECATED dsafpaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafpaServiceActionId :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact Lude.Text
dsafpaServiceActionId = Lens.lens (serviceActionId :: DisassociateServiceActionFromProvisioningArtifact -> Lude.Text) (\s a -> s {serviceActionId = a} :: DisassociateServiceActionFromProvisioningArtifact)
{-# DEPRECATED dsafpaServiceActionId "Use generic-lens or generic-optics with 'serviceActionId' instead." #-}

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafpaProductId :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifact Lude.Text
dsafpaProductId = Lens.lens (productId :: DisassociateServiceActionFromProvisioningArtifact -> Lude.Text) (\s a -> s {productId = a} :: DisassociateServiceActionFromProvisioningArtifact)
{-# DEPRECATED dsafpaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance
  Lude.AWSRequest
    DisassociateServiceActionFromProvisioningArtifact
  where
  type
    Rs DisassociateServiceActionFromProvisioningArtifact =
      DisassociateServiceActionFromProvisioningArtifactResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateServiceActionFromProvisioningArtifactResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DisassociateServiceActionFromProvisioningArtifact
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DisassociateServiceActionFromProvisioningArtifact" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance
  Lude.ToJSON
    DisassociateServiceActionFromProvisioningArtifact
  where
  toJSON DisassociateServiceActionFromProvisioningArtifact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ProvisioningArtifactId" Lude..= provisioningArtifactId),
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("ServiceActionId" Lude..= serviceActionId),
            Lude.Just ("ProductId" Lude..= productId)
          ]
      )

instance
  Lude.ToPath
    DisassociateServiceActionFromProvisioningArtifact
  where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    DisassociateServiceActionFromProvisioningArtifact
  where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateServiceActionFromProvisioningArtifactResponse' smart constructor.
newtype DisassociateServiceActionFromProvisioningArtifactResponse = DisassociateServiceActionFromProvisioningArtifactResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateServiceActionFromProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateServiceActionFromProvisioningArtifactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateServiceActionFromProvisioningArtifactResponse
mkDisassociateServiceActionFromProvisioningArtifactResponse
  pResponseStatus_ =
    DisassociateServiceActionFromProvisioningArtifactResponse'
      { responseStatus =
          pResponseStatus_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafparsResponseStatus :: Lens.Lens' DisassociateServiceActionFromProvisioningArtifactResponse Lude.Int
dsafparsResponseStatus = Lens.lens (responseStatus :: DisassociateServiceActionFromProvisioningArtifactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateServiceActionFromProvisioningArtifactResponse)
{-# DEPRECATED dsafparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
