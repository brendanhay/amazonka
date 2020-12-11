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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.PatchRule

-- | A set of rules defining the approval rules for a patch baseline.
--
-- /See:/ 'mkPatchRuleGroup' smart constructor.
newtype PatchRuleGroup = PatchRuleGroup' {patchRules :: [PatchRule]}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PatchRuleGroup' with the minimum fields required to make a request.
--
-- * 'patchRules' - The rules that make up the rule group.
mkPatchRuleGroup ::
  PatchRuleGroup
mkPatchRuleGroup = PatchRuleGroup' {patchRules = Lude.mempty}

-- | The rules that make up the rule group.
--
-- /Note:/ Consider using 'patchRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prgPatchRules :: Lens.Lens' PatchRuleGroup [PatchRule]
prgPatchRules = Lens.lens (patchRules :: PatchRuleGroup -> [PatchRule]) (\s a -> s {patchRules = a} :: PatchRuleGroup)
{-# DEPRECATED prgPatchRules "Use generic-lens or generic-optics with 'patchRules' instead." #-}

instance Lude.FromJSON PatchRuleGroup where
  parseJSON =
    Lude.withObject
      "PatchRuleGroup"
      ( \x ->
          PatchRuleGroup'
            Lude.<$> (x Lude..:? "PatchRules" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON PatchRuleGroup where
  toJSON PatchRuleGroup' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("PatchRules" Lude..= patchRules)])
