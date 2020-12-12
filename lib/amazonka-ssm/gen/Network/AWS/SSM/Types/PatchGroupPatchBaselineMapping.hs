{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchGroupPatchBaselineMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchGroupPatchBaselineMapping
  ( PatchGroupPatchBaselineMapping (..),

    -- * Smart constructor
    mkPatchGroupPatchBaselineMapping,

    -- * Lenses
    pgpbmBaselineIdentity,
    pgpbmPatchGroup,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.PatchBaselineIdentity

-- | The mapping between a patch group and the patch baseline the patch group is registered with.
--
-- /See:/ 'mkPatchGroupPatchBaselineMapping' smart constructor.
data PatchGroupPatchBaselineMapping = PatchGroupPatchBaselineMapping'
  { baselineIdentity ::
      Lude.Maybe
        PatchBaselineIdentity,
    patchGroup ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PatchGroupPatchBaselineMapping' with the minimum fields required to make a request.
--
-- * 'baselineIdentity' - The patch baseline the patch group is registered with.
-- * 'patchGroup' - The name of the patch group registered with the patch baseline.
mkPatchGroupPatchBaselineMapping ::
  PatchGroupPatchBaselineMapping
mkPatchGroupPatchBaselineMapping =
  PatchGroupPatchBaselineMapping'
    { baselineIdentity = Lude.Nothing,
      patchGroup = Lude.Nothing
    }

-- | The patch baseline the patch group is registered with.
--
-- /Note:/ Consider using 'baselineIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgpbmBaselineIdentity :: Lens.Lens' PatchGroupPatchBaselineMapping (Lude.Maybe PatchBaselineIdentity)
pgpbmBaselineIdentity = Lens.lens (baselineIdentity :: PatchGroupPatchBaselineMapping -> Lude.Maybe PatchBaselineIdentity) (\s a -> s {baselineIdentity = a} :: PatchGroupPatchBaselineMapping)
{-# DEPRECATED pgpbmBaselineIdentity "Use generic-lens or generic-optics with 'baselineIdentity' instead." #-}

-- | The name of the patch group registered with the patch baseline.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgpbmPatchGroup :: Lens.Lens' PatchGroupPatchBaselineMapping (Lude.Maybe Lude.Text)
pgpbmPatchGroup = Lens.lens (patchGroup :: PatchGroupPatchBaselineMapping -> Lude.Maybe Lude.Text) (\s a -> s {patchGroup = a} :: PatchGroupPatchBaselineMapping)
{-# DEPRECATED pgpbmPatchGroup "Use generic-lens or generic-optics with 'patchGroup' instead." #-}

instance Lude.FromJSON PatchGroupPatchBaselineMapping where
  parseJSON =
    Lude.withObject
      "PatchGroupPatchBaselineMapping"
      ( \x ->
          PatchGroupPatchBaselineMapping'
            Lude.<$> (x Lude..:? "BaselineIdentity") Lude.<*> (x Lude..:? "PatchGroup")
      )
