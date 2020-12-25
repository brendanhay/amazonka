{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RuleGroupUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RuleGroupUpdate
  ( RuleGroupUpdate (..),

    -- * Smart constructor
    mkRuleGroupUpdate,

    -- * Lenses
    rguAction,
    rguActivatedRule,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ActivatedRule as Types
import qualified Network.AWS.WAFRegional.Types.ChangeAction as Types

-- | Specifies an @ActivatedRule@ and indicates whether you want to add it to a @RuleGroup@ or delete it from a @RuleGroup@ .
--
-- /See:/ 'mkRuleGroupUpdate' smart constructor.
data RuleGroupUpdate = RuleGroupUpdate'
  { -- | Specify @INSERT@ to add an @ActivatedRule@ to a @RuleGroup@ . Use @DELETE@ to remove an @ActivatedRule@ from a @RuleGroup@ .
    action :: Types.ChangeAction,
    -- | The @ActivatedRule@ object specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
    activatedRule :: Types.ActivatedRule
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RuleGroupUpdate' value with any optional fields omitted.
mkRuleGroupUpdate ::
  -- | 'action'
  Types.ChangeAction ->
  -- | 'activatedRule'
  Types.ActivatedRule ->
  RuleGroupUpdate
mkRuleGroupUpdate action activatedRule =
  RuleGroupUpdate' {action, activatedRule}

-- | Specify @INSERT@ to add an @ActivatedRule@ to a @RuleGroup@ . Use @DELETE@ to remove an @ActivatedRule@ from a @RuleGroup@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rguAction :: Lens.Lens' RuleGroupUpdate Types.ChangeAction
rguAction = Lens.field @"action"
{-# DEPRECATED rguAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The @ActivatedRule@ object specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
--
-- /Note:/ Consider using 'activatedRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rguActivatedRule :: Lens.Lens' RuleGroupUpdate Types.ActivatedRule
rguActivatedRule = Lens.field @"activatedRule"
{-# DEPRECATED rguActivatedRule "Use generic-lens or generic-optics with 'activatedRule' instead." #-}

instance Core.FromJSON RuleGroupUpdate where
  toJSON RuleGroupUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Action" Core..= action),
            Core.Just ("ActivatedRule" Core..= activatedRule)
          ]
      )
