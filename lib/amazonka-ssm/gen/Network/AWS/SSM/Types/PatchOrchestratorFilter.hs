{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchOrchestratorFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchOrchestratorFilter
  ( PatchOrchestratorFilter (..)
  -- * Smart constructor
  , mkPatchOrchestratorFilter
  -- * Lenses
  , pofKey
  , pofValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.Key as Types
import qualified Network.AWS.SSM.Types.PatchOrchestratorFilterValue as Types

-- | Defines a filter used in Patch Manager APIs.
--
-- /See:/ 'mkPatchOrchestratorFilter' smart constructor.
data PatchOrchestratorFilter = PatchOrchestratorFilter'
  { key :: Core.Maybe Types.Key
    -- ^ The key for the filter.
  , values :: Core.Maybe [Types.PatchOrchestratorFilterValue]
    -- ^ The value for the filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PatchOrchestratorFilter' value with any optional fields omitted.
mkPatchOrchestratorFilter
    :: PatchOrchestratorFilter
mkPatchOrchestratorFilter
  = PatchOrchestratorFilter'{key = Core.Nothing,
                             values = Core.Nothing}

-- | The key for the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pofKey :: Lens.Lens' PatchOrchestratorFilter (Core.Maybe Types.Key)
pofKey = Lens.field @"key"
{-# INLINEABLE pofKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The value for the filter.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pofValues :: Lens.Lens' PatchOrchestratorFilter (Core.Maybe [Types.PatchOrchestratorFilterValue])
pofValues = Lens.field @"values"
{-# INLINEABLE pofValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON PatchOrchestratorFilter where
        toJSON PatchOrchestratorFilter{..}
          = Core.object
              (Core.catMaybes
                 [("Key" Core..=) Core.<$> key, ("Values" Core..=) Core.<$> values])
