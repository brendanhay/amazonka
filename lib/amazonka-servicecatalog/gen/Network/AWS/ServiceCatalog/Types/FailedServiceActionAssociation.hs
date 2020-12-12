{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.FailedServiceActionAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.FailedServiceActionAssociation
  ( FailedServiceActionAssociation (..),

    -- * Smart constructor
    mkFailedServiceActionAssociation,

    -- * Lenses
    fsaaProvisioningArtifactId,
    fsaaErrorCode,
    fsaaErrorMessage,
    fsaaServiceActionId,
    fsaaProductId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorCode

-- | An object containing information about the error, along with identifying information about the self-service action and its associations.
--
-- /See:/ 'mkFailedServiceActionAssociation' smart constructor.
data FailedServiceActionAssociation = FailedServiceActionAssociation'
  { provisioningArtifactId ::
      Lude.Maybe Lude.Text,
    errorCode ::
      Lude.Maybe
        ServiceActionAssociationErrorCode,
    errorMessage ::
      Lude.Maybe Lude.Text,
    serviceActionId ::
      Lude.Maybe Lude.Text,
    productId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailedServiceActionAssociation' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code. Valid values are listed below.
-- * 'errorMessage' - A text description of the error.
-- * 'productId' - The product identifier. For example, @prod-abcdzk7xy33qa@ .
-- * 'provisioningArtifactId' - The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
-- * 'serviceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
mkFailedServiceActionAssociation ::
  FailedServiceActionAssociation
mkFailedServiceActionAssociation =
  FailedServiceActionAssociation'
    { provisioningArtifactId =
        Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing,
      serviceActionId = Lude.Nothing,
      productId = Lude.Nothing
    }

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsaaProvisioningArtifactId :: Lens.Lens' FailedServiceActionAssociation (Lude.Maybe Lude.Text)
fsaaProvisioningArtifactId = Lens.lens (provisioningArtifactId :: FailedServiceActionAssociation -> Lude.Maybe Lude.Text) (\s a -> s {provisioningArtifactId = a} :: FailedServiceActionAssociation)
{-# DEPRECATED fsaaProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The error code. Valid values are listed below.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsaaErrorCode :: Lens.Lens' FailedServiceActionAssociation (Lude.Maybe ServiceActionAssociationErrorCode)
fsaaErrorCode = Lens.lens (errorCode :: FailedServiceActionAssociation -> Lude.Maybe ServiceActionAssociationErrorCode) (\s a -> s {errorCode = a} :: FailedServiceActionAssociation)
{-# DEPRECATED fsaaErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | A text description of the error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsaaErrorMessage :: Lens.Lens' FailedServiceActionAssociation (Lude.Maybe Lude.Text)
fsaaErrorMessage = Lens.lens (errorMessage :: FailedServiceActionAssociation -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: FailedServiceActionAssociation)
{-# DEPRECATED fsaaErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsaaServiceActionId :: Lens.Lens' FailedServiceActionAssociation (Lude.Maybe Lude.Text)
fsaaServiceActionId = Lens.lens (serviceActionId :: FailedServiceActionAssociation -> Lude.Maybe Lude.Text) (\s a -> s {serviceActionId = a} :: FailedServiceActionAssociation)
{-# DEPRECATED fsaaServiceActionId "Use generic-lens or generic-optics with 'serviceActionId' instead." #-}

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsaaProductId :: Lens.Lens' FailedServiceActionAssociation (Lude.Maybe Lude.Text)
fsaaProductId = Lens.lens (productId :: FailedServiceActionAssociation -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: FailedServiceActionAssociation)
{-# DEPRECATED fsaaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

instance Lude.FromJSON FailedServiceActionAssociation where
  parseJSON =
    Lude.withObject
      "FailedServiceActionAssociation"
      ( \x ->
          FailedServiceActionAssociation'
            Lude.<$> (x Lude..:? "ProvisioningArtifactId")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "ServiceActionId")
            Lude.<*> (x Lude..:? "ProductId")
      )
