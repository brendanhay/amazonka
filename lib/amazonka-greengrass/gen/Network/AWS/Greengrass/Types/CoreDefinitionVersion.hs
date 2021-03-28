{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.CoreDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.CoreDefinitionVersion
  ( CoreDefinitionVersion (..)
  -- * Smart constructor
  , mkCoreDefinitionVersion
  -- * Lenses
  , cdvCores
  ) where

import qualified Network.AWS.Greengrass.Types.Core as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a core definition version.
--
-- /See:/ 'mkCoreDefinitionVersion' smart constructor.
newtype CoreDefinitionVersion = CoreDefinitionVersion'
  { cores :: Core.Maybe [Types.Core]
    -- ^ A list of cores in the core definition version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CoreDefinitionVersion' value with any optional fields omitted.
mkCoreDefinitionVersion
    :: CoreDefinitionVersion
mkCoreDefinitionVersion
  = CoreDefinitionVersion'{cores = Core.Nothing}

-- | A list of cores in the core definition version.
--
-- /Note:/ Consider using 'cores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvCores :: Lens.Lens' CoreDefinitionVersion (Core.Maybe [Types.Core])
cdvCores = Lens.field @"cores"
{-# INLINEABLE cdvCores #-}
{-# DEPRECATED cores "Use generic-lens or generic-optics with 'cores' instead"  #-}

instance Core.FromJSON CoreDefinitionVersion where
        toJSON CoreDefinitionVersion{..}
          = Core.object (Core.catMaybes [("Cores" Core..=) Core.<$> cores])

instance Core.FromJSON CoreDefinitionVersion where
        parseJSON
          = Core.withObject "CoreDefinitionVersion" Core.$
              \ x -> CoreDefinitionVersion' Core.<$> (x Core..:? "Cores")
