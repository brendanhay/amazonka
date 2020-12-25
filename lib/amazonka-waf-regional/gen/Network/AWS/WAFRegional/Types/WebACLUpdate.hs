{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.WebACLUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.WebACLUpdate
  ( WebACLUpdate (..),

    -- * Smart constructor
    mkWebACLUpdate,

    -- * Lenses
    wacluAction,
    wacluActivatedRule,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ActivatedRule as Types
import qualified Network.AWS.WAFRegional.Types.ChangeAction as Types

-- | Specifies whether to insert a @Rule@ into or delete a @Rule@ from a @WebACL@ .
--
-- /See:/ 'mkWebACLUpdate' smart constructor.
data WebACLUpdate = WebACLUpdate'
  { -- | Specifies whether to insert a @Rule@ into or delete a @Rule@ from a @WebACL@ .
    action :: Types.ChangeAction,
    -- | The @ActivatedRule@ object in an 'UpdateWebACL' request specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
    activatedRule :: Types.ActivatedRule
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WebACLUpdate' value with any optional fields omitted.
mkWebACLUpdate ::
  -- | 'action'
  Types.ChangeAction ->
  -- | 'activatedRule'
  Types.ActivatedRule ->
  WebACLUpdate
mkWebACLUpdate action activatedRule =
  WebACLUpdate' {action, activatedRule}

-- | Specifies whether to insert a @Rule@ into or delete a @Rule@ from a @WebACL@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wacluAction :: Lens.Lens' WebACLUpdate Types.ChangeAction
wacluAction = Lens.field @"action"
{-# DEPRECATED wacluAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The @ActivatedRule@ object in an 'UpdateWebACL' request specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
--
-- /Note:/ Consider using 'activatedRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wacluActivatedRule :: Lens.Lens' WebACLUpdate Types.ActivatedRule
wacluActivatedRule = Lens.field @"activatedRule"
{-# DEPRECATED wacluActivatedRule "Use generic-lens or generic-optics with 'activatedRule' instead." #-}

instance Core.FromJSON WebACLUpdate where
  toJSON WebACLUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Action" Core..= action),
            Core.Just ("ActivatedRule" Core..= activatedRule)
          ]
      )
