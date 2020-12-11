-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RuleGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RuleGroupSummary
  ( RuleGroupSummary (..),

    -- * Smart constructor
    mkRuleGroupSummary,

    -- * Lenses
    rgsRuleGroupId,
    rgsName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the identifier and the friendly name or description of the @RuleGroup@ .
--
-- /See:/ 'mkRuleGroupSummary' smart constructor.
data RuleGroupSummary = RuleGroupSummary'
  { ruleGroupId :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RuleGroupSummary' with the minimum fields required to make a request.
--
-- * 'name' - A friendly name or description of the 'RuleGroup' . You can't change the name of a @RuleGroup@ after you create it.
-- * 'ruleGroupId' - A unique identifier for a @RuleGroup@ . You use @RuleGroupId@ to get more information about a @RuleGroup@ (see 'GetRuleGroup' ), update a @RuleGroup@ (see 'UpdateRuleGroup' ), insert a @RuleGroup@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RuleGroup@ from AWS WAF (see 'DeleteRuleGroup' ).
--
-- @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
mkRuleGroupSummary ::
  -- | 'ruleGroupId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  RuleGroupSummary
mkRuleGroupSummary pRuleGroupId_ pName_ =
  RuleGroupSummary' {ruleGroupId = pRuleGroupId_, name = pName_}

-- | A unique identifier for a @RuleGroup@ . You use @RuleGroupId@ to get more information about a @RuleGroup@ (see 'GetRuleGroup' ), update a @RuleGroup@ (see 'UpdateRuleGroup' ), insert a @RuleGroup@ into a @WebACL@ or delete one from a @WebACL@ (see 'UpdateWebACL' ), or delete a @RuleGroup@ from AWS WAF (see 'DeleteRuleGroup' ).
--
-- @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- /Note:/ Consider using 'ruleGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsRuleGroupId :: Lens.Lens' RuleGroupSummary Lude.Text
rgsRuleGroupId = Lens.lens (ruleGroupId :: RuleGroupSummary -> Lude.Text) (\s a -> s {ruleGroupId = a} :: RuleGroupSummary)
{-# DEPRECATED rgsRuleGroupId "Use generic-lens or generic-optics with 'ruleGroupId' instead." #-}

-- | A friendly name or description of the 'RuleGroup' . You can't change the name of a @RuleGroup@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsName :: Lens.Lens' RuleGroupSummary Lude.Text
rgsName = Lens.lens (name :: RuleGroupSummary -> Lude.Text) (\s a -> s {name = a} :: RuleGroupSummary)
{-# DEPRECATED rgsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON RuleGroupSummary where
  parseJSON =
    Lude.withObject
      "RuleGroupSummary"
      ( \x ->
          RuleGroupSummary'
            Lude.<$> (x Lude..: "RuleGroupId") Lude.<*> (x Lude..: "Name")
      )
