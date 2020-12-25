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
    fsaaErrorCode,
    fsaaErrorMessage,
    fsaaProductId,
    fsaaProvisioningArtifactId,
    fsaaServiceActionId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ProductId as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactId as Types
import qualified Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorCode as Types
import qualified Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorMessage as Types
import qualified Network.AWS.ServiceCatalog.Types.ServiceActionId as Types

-- | An object containing information about the error, along with identifying information about the self-service action and its associations.
--
-- /See:/ 'mkFailedServiceActionAssociation' smart constructor.
data FailedServiceActionAssociation = FailedServiceActionAssociation'
  { -- | The error code. Valid values are listed below.
    errorCode :: Core.Maybe Types.ServiceActionAssociationErrorCode,
    -- | A text description of the error.
    errorMessage :: Core.Maybe Types.ServiceActionAssociationErrorMessage,
    -- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
    productId :: Core.Maybe Types.ProductId,
    -- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
    provisioningArtifactId :: Core.Maybe Types.ProvisioningArtifactId,
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
    serviceActionId :: Core.Maybe Types.ServiceActionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailedServiceActionAssociation' value with any optional fields omitted.
mkFailedServiceActionAssociation ::
  FailedServiceActionAssociation
mkFailedServiceActionAssociation =
  FailedServiceActionAssociation'
    { errorCode = Core.Nothing,
      errorMessage = Core.Nothing,
      productId = Core.Nothing,
      provisioningArtifactId = Core.Nothing,
      serviceActionId = Core.Nothing
    }

-- | The error code. Valid values are listed below.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsaaErrorCode :: Lens.Lens' FailedServiceActionAssociation (Core.Maybe Types.ServiceActionAssociationErrorCode)
fsaaErrorCode = Lens.field @"errorCode"
{-# DEPRECATED fsaaErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | A text description of the error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsaaErrorMessage :: Lens.Lens' FailedServiceActionAssociation (Core.Maybe Types.ServiceActionAssociationErrorMessage)
fsaaErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED fsaaErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsaaProductId :: Lens.Lens' FailedServiceActionAssociation (Core.Maybe Types.ProductId)
fsaaProductId = Lens.field @"productId"
{-# DEPRECATED fsaaProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsaaProvisioningArtifactId :: Lens.Lens' FailedServiceActionAssociation (Core.Maybe Types.ProvisioningArtifactId)
fsaaProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# DEPRECATED fsaaProvisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead." #-}

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsaaServiceActionId :: Lens.Lens' FailedServiceActionAssociation (Core.Maybe Types.ServiceActionId)
fsaaServiceActionId = Lens.field @"serviceActionId"
{-# DEPRECATED fsaaServiceActionId "Use generic-lens or generic-optics with 'serviceActionId' instead." #-}

instance Core.FromJSON FailedServiceActionAssociation where
  parseJSON =
    Core.withObject "FailedServiceActionAssociation" Core.$
      \x ->
        FailedServiceActionAssociation'
          Core.<$> (x Core..:? "ErrorCode")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "ProductId")
          Core.<*> (x Core..:? "ProvisioningArtifactId")
          Core.<*> (x Core..:? "ServiceActionId")
