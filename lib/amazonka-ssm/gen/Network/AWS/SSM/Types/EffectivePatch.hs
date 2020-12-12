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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.Patch
import Network.AWS.SSM.Types.PatchStatus

-- | The EffectivePatch structure defines metadata about a patch along with the approval state of the patch in a particular patch baseline. The approval state includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.
--
-- /See:/ 'mkEffectivePatch' smart constructor.
data EffectivePatch = EffectivePatch'
  { patch :: Lude.Maybe Patch,
    patchStatus :: Lude.Maybe PatchStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EffectivePatch' with the minimum fields required to make a request.
--
-- * 'patch' - Provides metadata for a patch, including information such as the KB ID, severity, classification and a URL for where more information can be obtained about the patch.
-- * 'patchStatus' - The status of the patch in a patch baseline. This includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.
mkEffectivePatch ::
  EffectivePatch
mkEffectivePatch =
  EffectivePatch' {patch = Lude.Nothing, patchStatus = Lude.Nothing}

-- | Provides metadata for a patch, including information such as the KB ID, severity, classification and a URL for where more information can be obtained about the patch.
--
-- /Note:/ Consider using 'patch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPatch :: Lens.Lens' EffectivePatch (Lude.Maybe Patch)
epPatch = Lens.lens (patch :: EffectivePatch -> Lude.Maybe Patch) (\s a -> s {patch = a} :: EffectivePatch)
{-# DEPRECATED epPatch "Use generic-lens or generic-optics with 'patch' instead." #-}

-- | The status of the patch in a patch baseline. This includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.
--
-- /Note:/ Consider using 'patchStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPatchStatus :: Lens.Lens' EffectivePatch (Lude.Maybe PatchStatus)
epPatchStatus = Lens.lens (patchStatus :: EffectivePatch -> Lude.Maybe PatchStatus) (\s a -> s {patchStatus = a} :: EffectivePatch)
{-# DEPRECATED epPatchStatus "Use generic-lens or generic-optics with 'patchStatus' instead." #-}

instance Lude.FromJSON EffectivePatch where
  parseJSON =
    Lude.withObject
      "EffectivePatch"
      ( \x ->
          EffectivePatch'
            Lude.<$> (x Lude..:? "Patch") Lude.<*> (x Lude..:? "PatchStatus")
      )
