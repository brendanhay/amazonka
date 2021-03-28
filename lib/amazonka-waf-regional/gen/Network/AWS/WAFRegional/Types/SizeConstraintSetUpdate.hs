{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SizeConstraintSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.SizeConstraintSetUpdate
  ( SizeConstraintSetUpdate (..)
  -- * Smart constructor
  , mkSizeConstraintSetUpdate
  -- * Lenses
  , scsuAction
  , scsuSizeConstraint
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ChangeAction as Types
import qualified Network.AWS.WAFRegional.Types.SizeConstraint as Types

-- | Specifies the part of a web request that you want to inspect the size of and indicates whether you want to add the specification to a 'SizeConstraintSet' or delete it from a @SizeConstraintSet@ .
--
-- /See:/ 'mkSizeConstraintSetUpdate' smart constructor.
data SizeConstraintSetUpdate = SizeConstraintSetUpdate'
  { action :: Types.ChangeAction
    -- ^ Specify @INSERT@ to add a 'SizeConstraintSetUpdate' to a 'SizeConstraintSet' . Use @DELETE@ to remove a @SizeConstraintSetUpdate@ from a @SizeConstraintSet@ .
  , sizeConstraint :: Types.SizeConstraint
    -- ^ Specifies a constraint on the size of a part of the web request. AWS WAF uses the @Size@ , @ComparisonOperator@ , and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SizeConstraintSetUpdate' value with any optional fields omitted.
mkSizeConstraintSetUpdate
    :: Types.ChangeAction -- ^ 'action'
    -> Types.SizeConstraint -- ^ 'sizeConstraint'
    -> SizeConstraintSetUpdate
mkSizeConstraintSetUpdate action sizeConstraint
  = SizeConstraintSetUpdate'{action, sizeConstraint}

-- | Specify @INSERT@ to add a 'SizeConstraintSetUpdate' to a 'SizeConstraintSet' . Use @DELETE@ to remove a @SizeConstraintSetUpdate@ from a @SizeConstraintSet@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsuAction :: Lens.Lens' SizeConstraintSetUpdate Types.ChangeAction
scsuAction = Lens.field @"action"
{-# INLINEABLE scsuAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | Specifies a constraint on the size of a part of the web request. AWS WAF uses the @Size@ , @ComparisonOperator@ , and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
--
-- /Note:/ Consider using 'sizeConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsuSizeConstraint :: Lens.Lens' SizeConstraintSetUpdate Types.SizeConstraint
scsuSizeConstraint = Lens.field @"sizeConstraint"
{-# INLINEABLE scsuSizeConstraint #-}
{-# DEPRECATED sizeConstraint "Use generic-lens or generic-optics with 'sizeConstraint' instead"  #-}

instance Core.FromJSON SizeConstraintSetUpdate where
        toJSON SizeConstraintSetUpdate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Action" Core..= action),
                  Core.Just ("SizeConstraint" Core..= sizeConstraint)])
