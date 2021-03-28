{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDetail
  ( ProvisioningArtifactDetail (..)
  -- * Smart constructor
  , mkProvisioningArtifactDetail
  -- * Lenses
  , padActive
  , padCreatedTime
  , padDescription
  , padGuidance
  , padId
  , padName
  , padType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Id as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactName as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType as Types

-- | Information about a provisioning artifact (also known as a version) for a product.
--
-- /See:/ 'mkProvisioningArtifactDetail' smart constructor.
data ProvisioningArtifactDetail = ProvisioningArtifactDetail'
  { active :: Core.Maybe Core.Bool
    -- ^ Indicates whether the product version is active.
  , createdTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The UTC time stamp of the creation time.
  , description :: Core.Maybe Types.ProvisioningArtifactName
    -- ^ The description of the provisioning artifact.
  , guidance :: Core.Maybe Types.ProvisioningArtifactGuidance
    -- ^ Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
  , id :: Core.Maybe Types.Id
    -- ^ The identifier of the provisioning artifact.
  , name :: Core.Maybe Types.ProvisioningArtifactName
    -- ^ The name of the provisioning artifact.
  , type' :: Core.Maybe Types.ProvisioningArtifactType
    -- ^ The type of provisioning artifact.
--
--
--     * @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template
--
--
--     * @MARKETPLACE_AMI@ - AWS Marketplace AMI
--
--
--     * @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ProvisioningArtifactDetail' value with any optional fields omitted.
mkProvisioningArtifactDetail
    :: ProvisioningArtifactDetail
mkProvisioningArtifactDetail
  = ProvisioningArtifactDetail'{active = Core.Nothing,
                                createdTime = Core.Nothing, description = Core.Nothing,
                                guidance = Core.Nothing, id = Core.Nothing, name = Core.Nothing,
                                type' = Core.Nothing}

-- | Indicates whether the product version is active.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padActive :: Lens.Lens' ProvisioningArtifactDetail (Core.Maybe Core.Bool)
padActive = Lens.field @"active"
{-# INLINEABLE padActive #-}
{-# DEPRECATED active "Use generic-lens or generic-optics with 'active' instead"  #-}

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padCreatedTime :: Lens.Lens' ProvisioningArtifactDetail (Core.Maybe Core.NominalDiffTime)
padCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE padCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The description of the provisioning artifact.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padDescription :: Lens.Lens' ProvisioningArtifactDetail (Core.Maybe Types.ProvisioningArtifactName)
padDescription = Lens.field @"description"
{-# INLINEABLE padDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
--
-- /Note:/ Consider using 'guidance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padGuidance :: Lens.Lens' ProvisioningArtifactDetail (Core.Maybe Types.ProvisioningArtifactGuidance)
padGuidance = Lens.field @"guidance"
{-# INLINEABLE padGuidance #-}
{-# DEPRECATED guidance "Use generic-lens or generic-optics with 'guidance' instead"  #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padId :: Lens.Lens' ProvisioningArtifactDetail (Core.Maybe Types.Id)
padId = Lens.field @"id"
{-# INLINEABLE padId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the provisioning artifact.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padName :: Lens.Lens' ProvisioningArtifactDetail (Core.Maybe Types.ProvisioningArtifactName)
padName = Lens.field @"name"
{-# INLINEABLE padName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of provisioning artifact.
--
--
--     * @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template
--
--
--     * @MARKETPLACE_AMI@ - AWS Marketplace AMI
--
--
--     * @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padType :: Lens.Lens' ProvisioningArtifactDetail (Core.Maybe Types.ProvisioningArtifactType)
padType = Lens.field @"type'"
{-# INLINEABLE padType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ProvisioningArtifactDetail where
        parseJSON
          = Core.withObject "ProvisioningArtifactDetail" Core.$
              \ x ->
                ProvisioningArtifactDetail' Core.<$>
                  (x Core..:? "Active") Core.<*> x Core..:? "CreatedTime" Core.<*>
                    x Core..:? "Description"
                    Core.<*> x Core..:? "Guidance"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Type"
