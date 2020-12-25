{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchRuleGroup
  ( PatchRuleGroup (..),

    -- * Smart constructor
    mkPatchRuleGroup,

    -- * Lenses
    prgPatchRules,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.PatchRule as Types

-- | A set of rules defining the approval rules for a patch baseline.
--
-- /See:/ 'mkPatchRuleGroup' smart constructor.
newtype PatchRuleGroup = PatchRuleGroup'
  { -- | The rules that make up the rule group.
    patchRules :: [Types.PatchRule]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PatchRuleGroup' value with any optional fields omitted.
mkPatchRuleGroup ::
  PatchRuleGroup
mkPatchRuleGroup = PatchRuleGroup' {patchRules = Core.mempty}

-- | The rules that make up the rule group.
--
-- /Note:/ Consider using 'patchRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prgPatchRules :: Lens.Lens' PatchRuleGroup [Types.PatchRule]
prgPatchRules = Lens.field @"patchRules"
{-# DEPRECATED prgPatchRules "Use generic-lens or generic-optics with 'patchRules' instead." #-}

instance Core.FromJSON PatchRuleGroup where
  toJSON PatchRuleGroup {..} =
    Core.object
      (Core.catMaybes [Core.Just ("PatchRules" Core..= patchRules)])

instance Core.FromJSON PatchRuleGroup where
  parseJSON =
    Core.withObject "PatchRuleGroup" Core.$
      \x ->
        PatchRuleGroup'
          Core.<$> (x Core..:? "PatchRules" Core..!= Core.mempty)
