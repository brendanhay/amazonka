{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.PatchFilter as Types

-- | A set of patch filters, typically used for approval rules.
--
-- /See:/ 'mkPatchFilterGroup' smart constructor.
newtype PatchFilterGroup = PatchFilterGroup'
  { -- | The set of patch filters that make up the group.
    patchFilters :: [Types.PatchFilter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PatchFilterGroup' value with any optional fields omitted.
mkPatchFilterGroup ::
  PatchFilterGroup
mkPatchFilterGroup = PatchFilterGroup' {patchFilters = Core.mempty}

-- | The set of patch filters that make up the group.
--
-- /Note:/ Consider using 'patchFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfgPatchFilters :: Lens.Lens' PatchFilterGroup [Types.PatchFilter]
pfgPatchFilters = Lens.field @"patchFilters"
{-# DEPRECATED pfgPatchFilters "Use generic-lens or generic-optics with 'patchFilters' instead." #-}

instance Core.FromJSON PatchFilterGroup where
  toJSON PatchFilterGroup {..} =
    Core.object
      (Core.catMaybes [Core.Just ("PatchFilters" Core..= patchFilters)])

instance Core.FromJSON PatchFilterGroup where
  parseJSON =
    Core.withObject "PatchFilterGroup" Core.$
      \x ->
        PatchFilterGroup'
          Core.<$> (x Core..:? "PatchFilters" Core..!= Core.mempty)
