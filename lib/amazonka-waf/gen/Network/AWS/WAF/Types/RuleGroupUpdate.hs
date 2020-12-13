{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RuleGroupUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RuleGroupUpdate
  ( RuleGroupUpdate (..),

    -- * Smart constructor
    mkRuleGroupUpdate,

    -- * Lenses
    rguAction,
    rguActivatedRule,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.ActivatedRule
import Network.AWS.WAF.Types.ChangeAction

-- | Specifies an @ActivatedRule@ and indicates whether you want to add it to a @RuleGroup@ or delete it from a @RuleGroup@ .
--
-- /See:/ 'mkRuleGroupUpdate' smart constructor.
data RuleGroupUpdate = RuleGroupUpdate'
  { -- | Specify @INSERT@ to add an @ActivatedRule@ to a @RuleGroup@ . Use @DELETE@ to remove an @ActivatedRule@ from a @RuleGroup@ .
    action :: ChangeAction,
    -- | The @ActivatedRule@ object specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
    activatedRule :: ActivatedRule
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RuleGroupUpdate' with the minimum fields required to make a request.
--
-- * 'action' - Specify @INSERT@ to add an @ActivatedRule@ to a @RuleGroup@ . Use @DELETE@ to remove an @ActivatedRule@ from a @RuleGroup@ .
-- * 'activatedRule' - The @ActivatedRule@ object specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
mkRuleGroupUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'activatedRule'
  ActivatedRule ->
  RuleGroupUpdate
mkRuleGroupUpdate pAction_ pActivatedRule_ =
  RuleGroupUpdate'
    { action = pAction_,
      activatedRule = pActivatedRule_
    }

-- | Specify @INSERT@ to add an @ActivatedRule@ to a @RuleGroup@ . Use @DELETE@ to remove an @ActivatedRule@ from a @RuleGroup@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rguAction :: Lens.Lens' RuleGroupUpdate ChangeAction
rguAction = Lens.lens (action :: RuleGroupUpdate -> ChangeAction) (\s a -> s {action = a} :: RuleGroupUpdate)
{-# DEPRECATED rguAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The @ActivatedRule@ object specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
--
-- /Note:/ Consider using 'activatedRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rguActivatedRule :: Lens.Lens' RuleGroupUpdate ActivatedRule
rguActivatedRule = Lens.lens (activatedRule :: RuleGroupUpdate -> ActivatedRule) (\s a -> s {activatedRule = a} :: RuleGroupUpdate)
{-# DEPRECATED rguActivatedRule "Use generic-lens or generic-optics with 'activatedRule' instead." #-}

instance Lude.ToJSON RuleGroupUpdate where
  toJSON RuleGroupUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Action" Lude..= action),
            Lude.Just ("ActivatedRule" Lude..= activatedRule)
          ]
      )
