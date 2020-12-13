{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ExcludedRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ExcludedRule
  ( ExcludedRule (..),

    -- * Smart constructor
    mkExcludedRule,

    -- * Lenses
    erRuleId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The rule to exclude from a rule group. This is applicable only when the @ActivatedRule@ refers to a @RuleGroup@ . The rule must belong to the @RuleGroup@ that is specified by the @ActivatedRule@ .
--
-- /See:/ 'mkExcludedRule' smart constructor.
newtype ExcludedRule = ExcludedRule'
  { -- | The unique identifier for the rule to exclude from the rule group.
    ruleId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExcludedRule' with the minimum fields required to make a request.
--
-- * 'ruleId' - The unique identifier for the rule to exclude from the rule group.
mkExcludedRule ::
  -- | 'ruleId'
  Lude.Text ->
  ExcludedRule
mkExcludedRule pRuleId_ = ExcludedRule' {ruleId = pRuleId_}

-- | The unique identifier for the rule to exclude from the rule group.
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erRuleId :: Lens.Lens' ExcludedRule Lude.Text
erRuleId = Lens.lens (ruleId :: ExcludedRule -> Lude.Text) (\s a -> s {ruleId = a} :: ExcludedRule)
{-# DEPRECATED erRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

instance Lude.FromJSON ExcludedRule where
  parseJSON =
    Lude.withObject
      "ExcludedRule"
      (\x -> ExcludedRule' Lude.<$> (x Lude..: "RuleId"))

instance Lude.ToJSON ExcludedRule where
  toJSON ExcludedRule' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RuleId" Lude..= ruleId)])
