{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchGroupPatchBaselineMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchGroupPatchBaselineMapping
  ( PatchGroupPatchBaselineMapping (..)
  -- * Smart constructor
  , mkPatchGroupPatchBaselineMapping
  -- * Lenses
  , pgpbmBaselineIdentity
  , pgpbmPatchGroup
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.PatchBaselineIdentity as Types
import qualified Network.AWS.SSM.Types.PatchGroup as Types

-- | The mapping between a patch group and the patch baseline the patch group is registered with.
--
-- /See:/ 'mkPatchGroupPatchBaselineMapping' smart constructor.
data PatchGroupPatchBaselineMapping = PatchGroupPatchBaselineMapping'
  { baselineIdentity :: Core.Maybe Types.PatchBaselineIdentity
    -- ^ The patch baseline the patch group is registered with.
  , patchGroup :: Core.Maybe Types.PatchGroup
    -- ^ The name of the patch group registered with the patch baseline.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PatchGroupPatchBaselineMapping' value with any optional fields omitted.
mkPatchGroupPatchBaselineMapping
    :: PatchGroupPatchBaselineMapping
mkPatchGroupPatchBaselineMapping
  = PatchGroupPatchBaselineMapping'{baselineIdentity = Core.Nothing,
                                    patchGroup = Core.Nothing}

-- | The patch baseline the patch group is registered with.
--
-- /Note:/ Consider using 'baselineIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgpbmBaselineIdentity :: Lens.Lens' PatchGroupPatchBaselineMapping (Core.Maybe Types.PatchBaselineIdentity)
pgpbmBaselineIdentity = Lens.field @"baselineIdentity"
{-# INLINEABLE pgpbmBaselineIdentity #-}
{-# DEPRECATED baselineIdentity "Use generic-lens or generic-optics with 'baselineIdentity' instead"  #-}

-- | The name of the patch group registered with the patch baseline.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgpbmPatchGroup :: Lens.Lens' PatchGroupPatchBaselineMapping (Core.Maybe Types.PatchGroup)
pgpbmPatchGroup = Lens.field @"patchGroup"
{-# INLINEABLE pgpbmPatchGroup #-}
{-# DEPRECATED patchGroup "Use generic-lens or generic-optics with 'patchGroup' instead"  #-}

instance Core.FromJSON PatchGroupPatchBaselineMapping where
        parseJSON
          = Core.withObject "PatchGroupPatchBaselineMapping" Core.$
              \ x ->
                PatchGroupPatchBaselineMapping' Core.<$>
                  (x Core..:? "BaselineIdentity") Core.<*> x Core..:? "PatchGroup"
