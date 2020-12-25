{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SizeConstraintSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SizeConstraintSet
  ( SizeConstraintSet (..),

    -- * Smart constructor
    mkSizeConstraintSet,

    -- * Lenses
    scsSizeConstraintSetId,
    scsSizeConstraints,
    scsName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.ResourceId as Types
import qualified Network.AWS.WAF.Types.ResourceName as Types
import qualified Network.AWS.WAF.Types.SizeConstraint as Types

-- | A complex type that contains @SizeConstraint@ objects, which specify the parts of web requests that you want AWS WAF to inspect the size of. If a @SizeConstraintSet@ contains more than one @SizeConstraint@ object, a request only needs to match one constraint to be considered a match.
--
-- /See:/ 'mkSizeConstraintSet' smart constructor.
data SizeConstraintSet = SizeConstraintSet'
  { -- | A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ).
    --
    -- @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
    sizeConstraintSetId :: Types.ResourceId,
    -- | Specifies the parts of web requests that you want to inspect the size of.
    sizeConstraints :: [Types.SizeConstraint],
    -- | The name, if any, of the @SizeConstraintSet@ .
    name :: Core.Maybe Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SizeConstraintSet' value with any optional fields omitted.
mkSizeConstraintSet ::
  -- | 'sizeConstraintSetId'
  Types.ResourceId ->
  SizeConstraintSet
mkSizeConstraintSet sizeConstraintSetId =
  SizeConstraintSet'
    { sizeConstraintSetId,
      sizeConstraints = Core.mempty,
      name = Core.Nothing
    }

-- | A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ).
--
-- @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
--
-- /Note:/ Consider using 'sizeConstraintSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsSizeConstraintSetId :: Lens.Lens' SizeConstraintSet Types.ResourceId
scsSizeConstraintSetId = Lens.field @"sizeConstraintSetId"
{-# DEPRECATED scsSizeConstraintSetId "Use generic-lens or generic-optics with 'sizeConstraintSetId' instead." #-}

-- | Specifies the parts of web requests that you want to inspect the size of.
--
-- /Note:/ Consider using 'sizeConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsSizeConstraints :: Lens.Lens' SizeConstraintSet [Types.SizeConstraint]
scsSizeConstraints = Lens.field @"sizeConstraints"
{-# DEPRECATED scsSizeConstraints "Use generic-lens or generic-optics with 'sizeConstraints' instead." #-}

-- | The name, if any, of the @SizeConstraintSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsName :: Lens.Lens' SizeConstraintSet (Core.Maybe Types.ResourceName)
scsName = Lens.field @"name"
{-# DEPRECATED scsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON SizeConstraintSet where
  parseJSON =
    Core.withObject "SizeConstraintSet" Core.$
      \x ->
        SizeConstraintSet'
          Core.<$> (x Core..: "SizeConstraintSetId")
          Core.<*> (x Core..:? "SizeConstraints" Core..!= Core.mempty)
          Core.<*> (x Core..:? "Name")
