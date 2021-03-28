{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactSummary
  ( ProvisioningArtifactSummary (..)
  -- * Smart constructor
  , mkProvisioningArtifactSummary
  -- * Lenses
  , pasCreatedTime
  , pasDescription
  , pasId
  , pasName
  , pasProvisioningArtifactMetadata
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Description as Types
import qualified Network.AWS.ServiceCatalog.Types.Id as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactInfoKey as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactInfoValue as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactName as Types

-- | Summary information about a provisioning artifact (also known as a version) for a product.
--
-- /See:/ 'mkProvisioningArtifactSummary' smart constructor.
data ProvisioningArtifactSummary = ProvisioningArtifactSummary'
  { createdTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The UTC time stamp of the creation time.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the provisioning artifact.
  , id :: Core.Maybe Types.Id
    -- ^ The identifier of the provisioning artifact.
  , name :: Core.Maybe Types.ProvisioningArtifactName
    -- ^ The name of the provisioning artifact.
  , provisioningArtifactMetadata :: Core.Maybe (Core.HashMap Types.ProvisioningArtifactInfoKey Types.ProvisioningArtifactInfoValue)
    -- ^ The metadata for the provisioning artifact. This is used with AWS Marketplace products.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ProvisioningArtifactSummary' value with any optional fields omitted.
mkProvisioningArtifactSummary
    :: ProvisioningArtifactSummary
mkProvisioningArtifactSummary
  = ProvisioningArtifactSummary'{createdTime = Core.Nothing,
                                 description = Core.Nothing, id = Core.Nothing, name = Core.Nothing,
                                 provisioningArtifactMetadata = Core.Nothing}

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasCreatedTime :: Lens.Lens' ProvisioningArtifactSummary (Core.Maybe Core.NominalDiffTime)
pasCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE pasCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The description of the provisioning artifact.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasDescription :: Lens.Lens' ProvisioningArtifactSummary (Core.Maybe Types.Description)
pasDescription = Lens.field @"description"
{-# INLINEABLE pasDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasId :: Lens.Lens' ProvisioningArtifactSummary (Core.Maybe Types.Id)
pasId = Lens.field @"id"
{-# INLINEABLE pasId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the provisioning artifact.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasName :: Lens.Lens' ProvisioningArtifactSummary (Core.Maybe Types.ProvisioningArtifactName)
pasName = Lens.field @"name"
{-# INLINEABLE pasName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The metadata for the provisioning artifact. This is used with AWS Marketplace products.
--
-- /Note:/ Consider using 'provisioningArtifactMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasProvisioningArtifactMetadata :: Lens.Lens' ProvisioningArtifactSummary (Core.Maybe (Core.HashMap Types.ProvisioningArtifactInfoKey Types.ProvisioningArtifactInfoValue))
pasProvisioningArtifactMetadata = Lens.field @"provisioningArtifactMetadata"
{-# INLINEABLE pasProvisioningArtifactMetadata #-}
{-# DEPRECATED provisioningArtifactMetadata "Use generic-lens or generic-optics with 'provisioningArtifactMetadata' instead"  #-}

instance Core.FromJSON ProvisioningArtifactSummary where
        parseJSON
          = Core.withObject "ProvisioningArtifactSummary" Core.$
              \ x ->
                ProvisioningArtifactSummary' Core.<$>
                  (x Core..:? "CreatedTime") Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "ProvisioningArtifactMetadata"
