-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SizeConstraintSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SizeConstraintSetUpdate
  ( SizeConstraintSetUpdate (..),

    -- * Smart constructor
    mkSizeConstraintSetUpdate,

    -- * Lenses
    scsuAction,
    scsuSizeConstraint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.ChangeAction
import Network.AWS.WAF.Types.SizeConstraint

-- | Specifies the part of a web request that you want to inspect the size of and indicates whether you want to add the specification to a 'SizeConstraintSet' or delete it from a @SizeConstraintSet@ .
--
-- /See:/ 'mkSizeConstraintSetUpdate' smart constructor.
data SizeConstraintSetUpdate = SizeConstraintSetUpdate'
  { action ::
      ChangeAction,
    sizeConstraint :: SizeConstraint
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SizeConstraintSetUpdate' with the minimum fields required to make a request.
--
-- * 'action' - Specify @INSERT@ to add a 'SizeConstraintSetUpdate' to a 'SizeConstraintSet' . Use @DELETE@ to remove a @SizeConstraintSetUpdate@ from a @SizeConstraintSet@ .
-- * 'sizeConstraint' - Specifies a constraint on the size of a part of the web request. AWS WAF uses the @Size@ , @ComparisonOperator@ , and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
mkSizeConstraintSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'sizeConstraint'
  SizeConstraint ->
  SizeConstraintSetUpdate
mkSizeConstraintSetUpdate pAction_ pSizeConstraint_ =
  SizeConstraintSetUpdate'
    { action = pAction_,
      sizeConstraint = pSizeConstraint_
    }

-- | Specify @INSERT@ to add a 'SizeConstraintSetUpdate' to a 'SizeConstraintSet' . Use @DELETE@ to remove a @SizeConstraintSetUpdate@ from a @SizeConstraintSet@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsuAction :: Lens.Lens' SizeConstraintSetUpdate ChangeAction
scsuAction = Lens.lens (action :: SizeConstraintSetUpdate -> ChangeAction) (\s a -> s {action = a} :: SizeConstraintSetUpdate)
{-# DEPRECATED scsuAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Specifies a constraint on the size of a part of the web request. AWS WAF uses the @Size@ , @ComparisonOperator@ , and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
-- /Note:/ Consider using 'sizeConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsuSizeConstraint :: Lens.Lens' SizeConstraintSetUpdate SizeConstraint
scsuSizeConstraint = Lens.lens (sizeConstraint :: SizeConstraintSetUpdate -> SizeConstraint) (\s a -> s {sizeConstraint = a} :: SizeConstraintSetUpdate)
{-# DEPRECATED scsuSizeConstraint "Use generic-lens or generic-optics with 'sizeConstraint' instead." #-}

instance Lude.ToJSON SizeConstraintSetUpdate where
  toJSON SizeConstraintSetUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Action" Lude..= action),
            Lude.Just ("SizeConstraint" Lude..= sizeConstraint)
          ]
      )
