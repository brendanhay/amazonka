{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ServiceActionAssociation
  ( ServiceActionAssociation (..)
  -- * Smart constructor
  , mkServiceActionAssociation
  -- * Lenses
  , saaServiceActionId
  , saaProductId
  , saaProvisioningArtifactId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ProductId as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactId as Types
import qualified Network.AWS.ServiceCatalog.Types.ServiceActionId as Types

-- | A self-service action association consisting of the Action ID, the Product ID, and the Provisioning Artifact ID.
--
-- /See:/ 'mkServiceActionAssociation' smart constructor.
data ServiceActionAssociation = ServiceActionAssociation'
  { serviceActionId :: Types.ServiceActionId
    -- ^ The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
  , productId :: Types.ProductId
    -- ^ The product identifier. For example, @prod-abcdzk7xy33qa@ .
  , provisioningArtifactId :: Types.ProvisioningArtifactId
    -- ^ The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceActionAssociation' value with any optional fields omitted.
mkServiceActionAssociation
    :: Types.ServiceActionId -- ^ 'serviceActionId'
    -> Types.ProductId -- ^ 'productId'
    -> Types.ProvisioningArtifactId -- ^ 'provisioningArtifactId'
    -> ServiceActionAssociation
mkServiceActionAssociation serviceActionId productId
  provisioningArtifactId
  = ServiceActionAssociation'{serviceActionId, productId,
                              provisioningArtifactId}

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saaServiceActionId :: Lens.Lens' ServiceActionAssociation Types.ServiceActionId
saaServiceActionId = Lens.field @"serviceActionId"
{-# INLINEABLE saaServiceActionId #-}
{-# DEPRECATED serviceActionId "Use generic-lens or generic-optics with 'serviceActionId' instead"  #-}

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saaProductId :: Lens.Lens' ServiceActionAssociation Types.ProductId
saaProductId = Lens.field @"productId"
{-# INLINEABLE saaProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saaProvisioningArtifactId :: Lens.Lens' ServiceActionAssociation Types.ProvisioningArtifactId
saaProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# INLINEABLE saaProvisioningArtifactId #-}
{-# DEPRECATED provisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead"  #-}

instance Core.FromJSON ServiceActionAssociation where
        toJSON ServiceActionAssociation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ServiceActionId" Core..= serviceActionId),
                  Core.Just ("ProductId" Core..= productId),
                  Core.Just
                    ("ProvisioningArtifactId" Core..= provisioningArtifactId)])
