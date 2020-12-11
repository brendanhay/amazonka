-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchFilterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchFilterGroup
  ( PatchFilterGroup (..),

    -- * Smart constructor
    mkPatchFilterGroup,

    -- * Lenses
    pfgPatchFilters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.PatchFilter

-- | A set of patch filters, typically used for approval rules.
--
-- /See:/ 'mkPatchFilterGroup' smart constructor.
newtype PatchFilterGroup = PatchFilterGroup'
  { patchFilters ::
      [PatchFilter]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PatchFilterGroup' with the minimum fields required to make a request.
--
-- * 'patchFilters' - The set of patch filters that make up the group.
mkPatchFilterGroup ::
  PatchFilterGroup
mkPatchFilterGroup = PatchFilterGroup' {patchFilters = Lude.mempty}

-- | The set of patch filters that make up the group.
--
-- /Note:/ Consider using 'patchFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfgPatchFilters :: Lens.Lens' PatchFilterGroup [PatchFilter]
pfgPatchFilters = Lens.lens (patchFilters :: PatchFilterGroup -> [PatchFilter]) (\s a -> s {patchFilters = a} :: PatchFilterGroup)
{-# DEPRECATED pfgPatchFilters "Use generic-lens or generic-optics with 'patchFilters' instead." #-}

instance Lude.FromJSON PatchFilterGroup where
  parseJSON =
    Lude.withObject
      "PatchFilterGroup"
      ( \x ->
          PatchFilterGroup'
            Lude.<$> (x Lude..:? "PatchFilters" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON PatchFilterGroup where
  toJSON PatchFilterGroup' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("PatchFilters" Lude..= patchFilters)])
