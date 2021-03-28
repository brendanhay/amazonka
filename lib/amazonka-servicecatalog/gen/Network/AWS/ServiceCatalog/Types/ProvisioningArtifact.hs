{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifact
  ( ProvisioningArtifact (..)
  -- * Smart constructor
  , mkProvisioningArtifact
  -- * Lenses
  , paCreatedTime
  , paDescription
  , paGuidance
  , paId
  , paName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Id as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDescription as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactName as Types

-- | Information about a provisioning artifact. A provisioning artifact is also known as a product version.
--
-- /See:/ 'mkProvisioningArtifact' smart constructor.
data ProvisioningArtifact = ProvisioningArtifact'
  { createdTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The UTC time stamp of the creation time.
  , description :: Core.Maybe Types.ProvisioningArtifactDescription
    -- ^ The description of the provisioning artifact.
  , guidance :: Core.Maybe Types.ProvisioningArtifactGuidance
    -- ^ Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
  , id :: Core.Maybe Types.Id
    -- ^ The identifier of the provisioning artifact.
  , name :: Core.Maybe Types.ProvisioningArtifactName
    -- ^ The name of the provisioning artifact.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ProvisioningArtifact' value with any optional fields omitted.
mkProvisioningArtifact
    :: ProvisioningArtifact
mkProvisioningArtifact
  = ProvisioningArtifact'{createdTime = Core.Nothing,
                          description = Core.Nothing, guidance = Core.Nothing,
                          id = Core.Nothing, name = Core.Nothing}

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paCreatedTime :: Lens.Lens' ProvisioningArtifact (Core.Maybe Core.NominalDiffTime)
paCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE paCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The description of the provisioning artifact.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paDescription :: Lens.Lens' ProvisioningArtifact (Core.Maybe Types.ProvisioningArtifactDescription)
paDescription = Lens.field @"description"
{-# INLINEABLE paDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
--
-- /Note:/ Consider using 'guidance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paGuidance :: Lens.Lens' ProvisioningArtifact (Core.Maybe Types.ProvisioningArtifactGuidance)
paGuidance = Lens.field @"guidance"
{-# INLINEABLE paGuidance #-}
{-# DEPRECATED guidance "Use generic-lens or generic-optics with 'guidance' instead"  #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paId :: Lens.Lens' ProvisioningArtifact (Core.Maybe Types.Id)
paId = Lens.field @"id"
{-# INLINEABLE paId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the provisioning artifact.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paName :: Lens.Lens' ProvisioningArtifact (Core.Maybe Types.ProvisioningArtifactName)
paName = Lens.field @"name"
{-# INLINEABLE paName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON ProvisioningArtifact where
        parseJSON
          = Core.withObject "ProvisioningArtifact" Core.$
              \ x ->
                ProvisioningArtifact' Core.<$>
                  (x Core..:? "CreatedTime") Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "Guidance"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Name"
