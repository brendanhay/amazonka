{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RuleUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RuleUpdate
  ( RuleUpdate (..),

    -- * Smart constructor
    mkRuleUpdate,

    -- * Lenses
    ruAction,
    ruPredicate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ChangeAction as Types
import qualified Network.AWS.WAFRegional.Types.Predicate as Types

-- | Specifies a @Predicate@ (such as an @IPSet@ ) and indicates whether you want to add it to a @Rule@ or delete it from a @Rule@ .
--
-- /See:/ 'mkRuleUpdate' smart constructor.
data RuleUpdate = RuleUpdate'
  { -- | Specify @INSERT@ to add a @Predicate@ to a @Rule@ . Use @DELETE@ to remove a @Predicate@ from a @Rule@ .
    action :: Types.ChangeAction,
    -- | The ID of the @Predicate@ (such as an @IPSet@ ) that you want to add to a @Rule@ .
    predicate :: Types.Predicate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RuleUpdate' value with any optional fields omitted.
mkRuleUpdate ::
  -- | 'action'
  Types.ChangeAction ->
  -- | 'predicate'
  Types.Predicate ->
  RuleUpdate
mkRuleUpdate action predicate = RuleUpdate' {action, predicate}

-- | Specify @INSERT@ to add a @Predicate@ to a @Rule@ . Use @DELETE@ to remove a @Predicate@ from a @Rule@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruAction :: Lens.Lens' RuleUpdate Types.ChangeAction
ruAction = Lens.field @"action"
{-# DEPRECATED ruAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The ID of the @Predicate@ (such as an @IPSet@ ) that you want to add to a @Rule@ .
--
-- /Note:/ Consider using 'predicate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruPredicate :: Lens.Lens' RuleUpdate Types.Predicate
ruPredicate = Lens.field @"predicate"
{-# DEPRECATED ruPredicate "Use generic-lens or generic-optics with 'predicate' instead." #-}

instance Core.FromJSON RuleUpdate where
  toJSON RuleUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Action" Core..= action),
            Core.Just ("Predicate" Core..= predicate)
          ]
      )
