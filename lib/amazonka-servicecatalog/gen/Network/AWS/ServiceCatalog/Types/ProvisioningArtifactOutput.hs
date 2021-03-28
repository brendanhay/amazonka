{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactOutput
  ( ProvisioningArtifactOutput (..)
  -- * Smart constructor
  , mkProvisioningArtifactOutput
  -- * Lenses
  , paoDescription
  , paoKey
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Description as Types
import qualified Network.AWS.ServiceCatalog.Types.Key as Types

-- | Provisioning artifact output.
--
-- /See:/ 'mkProvisioningArtifactOutput' smart constructor.
data ProvisioningArtifactOutput = ProvisioningArtifactOutput'
  { description :: Core.Maybe Types.Description
    -- ^ Description of the provisioning artifact output key.
  , key :: Core.Maybe Types.Key
    -- ^ The provisioning artifact output key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisioningArtifactOutput' value with any optional fields omitted.
mkProvisioningArtifactOutput
    :: ProvisioningArtifactOutput
mkProvisioningArtifactOutput
  = ProvisioningArtifactOutput'{description = Core.Nothing,
                                key = Core.Nothing}

-- | Description of the provisioning artifact output key.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paoDescription :: Lens.Lens' ProvisioningArtifactOutput (Core.Maybe Types.Description)
paoDescription = Lens.field @"description"
{-# INLINEABLE paoDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The provisioning artifact output key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paoKey :: Lens.Lens' ProvisioningArtifactOutput (Core.Maybe Types.Key)
paoKey = Lens.field @"key"
{-# INLINEABLE paoKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

instance Core.FromJSON ProvisioningArtifactOutput where
        parseJSON
          = Core.withObject "ProvisioningArtifactOutput" Core.$
              \ x ->
                ProvisioningArtifactOutput' Core.<$>
                  (x Core..:? "Description") Core.<*> x Core..:? "Key"
