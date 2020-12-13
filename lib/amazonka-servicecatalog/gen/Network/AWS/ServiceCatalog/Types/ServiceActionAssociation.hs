{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionAssociation
  ( ServiceActionAssociation (..),

    -- * Smart constructor
    mkServiceActionAssociation,

    -- * Lenses
    saaProvisioningArtifactId,
    saaServiceActionId,
    saaProductId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A self-service action association consisting of the Action ID, the Product ID, and the Provisioning Artifact ID.
--
-- /See:/ 'mkServiceActionAssociation' smart constructor.
data ServiceActionAssociation = ServiceActionAssociation'
  { -- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
    provisioningArtifactId :: Lude.Text,
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
    serviceActionId :: Lude.Text,
    -- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
    productId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceActionAssociation' with the minimum fields required to make a request.
--
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
-- * 'serviceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
-- * 'productId' - The product identifier. For example, @prod-abcdzk7xy33qa@ .
mkServiceActionAssociation ::
  -- | 'provisioningArtifactId'
  Lude.Text ->
  -- | 'serviceActionId'
  Lude.Text ->
  -- | 'productId'
  Lude.Text ->
  ServiceActionAssociation
mkServiceActionAssociation
  pProvisioningArtifactId_
  pServiceActionId_
  pProductId_ =
    ServiceActionAssociation'
      { provisioningArtifactId =
          pProvisioningArtifactId_,
        serviceActionId = pServiceActionId_,
        productId = pProductId_
      }

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saaProvisioningArtifactId :: Lens.Lens' ServiceActionAssociation Lude.Text
saaProvisioningArtifactId = Lens.lens (provisioningArtifactId :: ServiceActionAssociation -> Lude.Text) (\s a -> s {provisioningArtifactId = a} :: ServiceActionAssociation)
{-# DEPRECATED saaProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saaServiceActionId :: Lens.Lens' ServiceActionAssociation Lude.Text
saaServiceActionId = Lens.lens (serviceActionId :: ServiceActionAssociation -> Lude.Text) (\s a -> s {serviceActionId = a} :: ServiceActionAssociation)
{-# DEPRECATED saaServiceActionId "Use generic-lens or generic-optics with 'serviceActionId' instead." #-}

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saaProductId :: Lens.Lens' ServiceActionAssociation Lude.Text
saaProductId = Lens.lens (productId :: ServiceActionAssociation -> Lude.Text) (\s a -> s {productId = a} :: ServiceActionAssociation)
{-# DEPRECATED saaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance Lude.ToJSON ServiceActionAssociation where
  toJSON ServiceActionAssociation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ProvisioningArtifactId" Lude..= provisioningArtifactId),
            Lude.Just ("ServiceActionId" Lude..= serviceActionId),
            Lude.Just ("ProductId" Lude..= productId)
          ]
      )
