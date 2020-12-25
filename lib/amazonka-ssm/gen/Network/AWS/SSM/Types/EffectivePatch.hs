{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.EffectivePatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.EffectivePatch
  ( EffectivePatch (..),

    -- * Smart constructor
    mkEffectivePatch,

    -- * Lenses
    epPatch,
    epPatchStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.Patch as Types
import qualified Network.AWS.SSM.Types.PatchStatus as Types

-- | The EffectivePatch structure defines metadata about a patch along with the approval state of the patch in a particular patch baseline. The approval state includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.
--
-- /See:/ 'mkEffectivePatch' smart constructor.
data EffectivePatch = EffectivePatch'
  { -- | Provides metadata for a patch, including information such as the KB ID, severity, classification and a URL for where more information can be obtained about the patch.
    patch :: Core.Maybe Types.Patch,
    -- | The status of the patch in a patch baseline. This includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.
    patchStatus :: Core.Maybe Types.PatchStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EffectivePatch' value with any optional fields omitted.
mkEffectivePatch ::
  EffectivePatch
mkEffectivePatch =
  EffectivePatch' {patch = Core.Nothing, patchStatus = Core.Nothing}

-- | Provides metadata for a patch, including information such as the KB ID, severity, classification and a URL for where more information can be obtained about the patch.
--
-- /Note:/ Consider using 'patch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPatch :: Lens.Lens' EffectivePatch (Core.Maybe Types.Patch)
epPatch = Lens.field @"patch"
{-# DEPRECATED epPatch "Use generic-lens or generic-optics with 'patch' instead." #-}

-- | The status of the patch in a patch baseline. This includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.
--
-- /Note:/ Consider using 'patchStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPatchStatus :: Lens.Lens' EffectivePatch (Core.Maybe Types.PatchStatus)
epPatchStatus = Lens.field @"patchStatus"
{-# DEPRECATED epPatchStatus "Use generic-lens or generic-optics with 'patchStatus' instead." #-}

instance Core.FromJSON EffectivePatch where
  parseJSON =
    Core.withObject "EffectivePatch" Core.$
      \x ->
        EffectivePatch'
          Core.<$> (x Core..:? "Patch") Core.<*> (x Core..:? "PatchStatus")
