{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AssociateServiceActionWithProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a self-service action with a provisioning artifact.
module Network.AWS.ServiceCatalog.AssociateServiceActionWithProvisioningArtifact
  ( -- * Creating a request
    AssociateServiceActionWithProvisioningArtifact (..),
    mkAssociateServiceActionWithProvisioningArtifact,

    -- ** Request lenses
    asawpaAcceptLanguage,
    asawpaProductId,
    asawpaProvisioningArtifactId,
    asawpaServiceActionId,

    -- * Destructuring the response
    AssociateServiceActionWithProvisioningArtifactResponse (..),
    mkAssociateServiceActionWithProvisioningArtifactResponse,

    -- ** Response lenses
    asawparsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkAssociateServiceActionWithProvisioningArtifact' smart constructor.
data AssociateServiceActionWithProvisioningArtifact = AssociateServiceActionWithProvisioningArtifact'
  { acceptLanguage ::
      Lude.Maybe
        Lude.Text,
    productId ::
      Lude.Text,
    provisioningArtifactId ::
      Lude.Text,
    serviceActionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'AssociateServiceActionWithProvisioningArtifact' with the minimum fields required to make a request.
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
-- * 'productId' - The product identifier. For example, @prod-abcdzk7xy33qa@ .
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
-- * 'serviceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
mkAssociateServiceActionWithProvisioningArtifact ::
  -- | 'productId'
  Lude.Text ->
  -- | 'provisioningArtifactId'
  Lude.Text ->
  -- | 'serviceActionId'
  Lude.Text ->
  AssociateServiceActionWithProvisioningArtifact
mkAssociateServiceActionWithProvisioningArtifact
  pProductId_
  pProvisioningArtifactId_
  pServiceActionId_ =
    AssociateServiceActionWithProvisioningArtifact'
      { acceptLanguage =
          Lude.Nothing,
        productId = pProductId_,
        provisioningArtifactId =
          pProvisioningArtifactId_,
        serviceActionId = pServiceActionId_
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
asawpaAcceptLanguage :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact (Lude.Maybe Lude.Text)
asawpaAcceptLanguage = Lens.lens (acceptLanguage :: AssociateServiceActionWithProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: AssociateServiceActionWithProvisioningArtifact)
{-# DEPRECATED asawpaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asawpaProductId :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact Lude.Text
asawpaProductId = Lens.lens (productId :: AssociateServiceActionWithProvisioningArtifact -> Lude.Text) (\s a -> s {productId = a} :: AssociateServiceActionWithProvisioningArtifact)
{-# DEPRECATED asawpaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asawpaProvisioningArtifactId :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact Lude.Text
asawpaProvisioningArtifactId = Lens.lens (provisioningArtifactId :: AssociateServiceActionWithProvisioningArtifact -> Lude.Text) (\s a -> s {provisioningArtifactId = a} :: AssociateServiceActionWithProvisioningArtifact)
{-# DEPRECATED asawpaProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asawpaServiceActionId :: Lens.Lens' AssociateServiceActionWithProvisioningArtifact Lude.Text
asawpaServiceActionId = Lens.lens (serviceActionId :: AssociateServiceActionWithProvisioningArtifact -> Lude.Text) (\s a -> s {serviceActionId = a} :: AssociateServiceActionWithProvisioningArtifact)
{-# DEPRECATED asawpaServiceActionId "Use generic-lens or generic-optics with 'serviceActionId' instead." #-}

instance
  Lude.AWSRequest
    AssociateServiceActionWithProvisioningArtifact
  where
  type
    Rs AssociateServiceActionWithProvisioningArtifact =
      AssociateServiceActionWithProvisioningArtifactResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateServiceActionWithProvisioningArtifactResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    AssociateServiceActionWithProvisioningArtifact
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.AssociateServiceActionWithProvisioningArtifact" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateServiceActionWithProvisioningArtifact where
  toJSON AssociateServiceActionWithProvisioningArtifact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("ProductId" Lude..= productId),
            Lude.Just
              ("ProvisioningArtifactId" Lude..= provisioningArtifactId),
            Lude.Just ("ServiceActionId" Lude..= serviceActionId)
          ]
      )

instance Lude.ToPath AssociateServiceActionWithProvisioningArtifact where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    AssociateServiceActionWithProvisioningArtifact
  where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateServiceActionWithProvisioningArtifactResponse' smart constructor.
newtype AssociateServiceActionWithProvisioningArtifactResponse = AssociateServiceActionWithProvisioningArtifactResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'AssociateServiceActionWithProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateServiceActionWithProvisioningArtifactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateServiceActionWithProvisioningArtifactResponse
mkAssociateServiceActionWithProvisioningArtifactResponse
  pResponseStatus_ =
    AssociateServiceActionWithProvisioningArtifactResponse'
      { responseStatus =
          pResponseStatus_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asawparsResponseStatus :: Lens.Lens' AssociateServiceActionWithProvisioningArtifactResponse Lude.Int
asawparsResponseStatus = Lens.lens (responseStatus :: AssociateServiceActionWithProvisioningArtifactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateServiceActionWithProvisioningArtifactResponse)
{-# DEPRECATED asawparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
