{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RuleSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RuleSummary
  ( RuleSummary (..),

    -- * Smart constructor
    mkRuleSummary,

    -- * Lenses
    rsRuleId,
    rsName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the identifier and the friendly name or description of the @Rule@ .
--
-- /See:/ 'mkRuleSummary' smart constructor.
data RuleSummary = RuleSummary'
  { -- | A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ).
    --
    -- @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
    ruleId :: Lude.Text,
    -- | A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RuleSummary' with the minimum fields required to make a request.
--
-- * 'ruleId' - A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ).
--
-- @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
-- * 'name' - A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
mkRuleSummary ::
  -- | 'ruleId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  RuleSummary
mkRuleSummary pRuleId_ pName_ =
  RuleSummary' {ruleId = pRuleId_, name = pName_}

-- | A unique identifier for a @Rule@ . You use @RuleId@ to get more information about a @Rule@ (see 'GetRule' ), update a @Rule@ (see 'UpdateRule' ), insert a @Rule@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @Rule@ from AWS WAF (see 'DeleteRule' ).
--
-- @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- /Note:/ Consider using 'ruleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRuleId :: Lens.Lens' RuleSummary Lude.Text
rsRuleId = Lens.lens (ruleId :: RuleSummary -> Lude.Text) (\s a -> s {ruleId = a} :: RuleSummary)
{-# DEPRECATED rsRuleId "Use generic-lens or generic-optics with 'ruleId' instead." #-}

-- | A friendly name or description of the 'Rule' . You can't change the name of a @Rule@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsName :: Lens.Lens' RuleSummary Lude.Text
rsName = Lens.lens (name :: RuleSummary -> Lude.Text) (\s a -> s {name = a} :: RuleSummary)
{-# DEPRECATED rsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON RuleSummary where
  parseJSON =
    Lude.withObject
      "RuleSummary"
      ( \x ->
          RuleSummary'
            Lude.<$> (x Lude..: "RuleId") Lude.<*> (x Lude..: "Name")
      )
