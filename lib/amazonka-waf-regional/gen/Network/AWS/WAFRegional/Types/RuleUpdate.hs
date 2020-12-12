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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.ChangeAction
import Network.AWS.WAFRegional.Types.Predicate

-- | Specifies a @Predicate@ (such as an @IPSet@ ) and indicates whether you want to add it to a @Rule@ or delete it from a @Rule@ .
--
-- /See:/ 'mkRuleUpdate' smart constructor.
data RuleUpdate = RuleUpdate'
  { action :: ChangeAction,
    predicate :: Predicate
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RuleUpdate' with the minimum fields required to make a request.
--
-- * 'action' - Specify @INSERT@ to add a @Predicate@ to a @Rule@ . Use @DELETE@ to remove a @Predicate@ from a @Rule@ .
-- * 'predicate' - The ID of the @Predicate@ (such as an @IPSet@ ) that you want to add to a @Rule@ .
mkRuleUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'predicate'
  Predicate ->
  RuleUpdate
mkRuleUpdate pAction_ pPredicate_ =
  RuleUpdate' {action = pAction_, predicate = pPredicate_}

-- | Specify @INSERT@ to add a @Predicate@ to a @Rule@ . Use @DELETE@ to remove a @Predicate@ from a @Rule@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruAction :: Lens.Lens' RuleUpdate ChangeAction
ruAction = Lens.lens (action :: RuleUpdate -> ChangeAction) (\s a -> s {action = a} :: RuleUpdate)
{-# DEPRECATED ruAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The ID of the @Predicate@ (such as an @IPSet@ ) that you want to add to a @Rule@ .
--
-- /Note:/ Consider using 'predicate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruPredicate :: Lens.Lens' RuleUpdate Predicate
ruPredicate = Lens.lens (predicate :: RuleUpdate -> Predicate) (\s a -> s {predicate = a} :: RuleUpdate)
{-# DEPRECATED ruPredicate "Use generic-lens or generic-optics with 'predicate' instead." #-}

instance Lude.ToJSON RuleUpdate where
  toJSON RuleUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Action" Lude..= action),
            Lude.Just ("Predicate" Lude..= predicate)
          ]
      )
