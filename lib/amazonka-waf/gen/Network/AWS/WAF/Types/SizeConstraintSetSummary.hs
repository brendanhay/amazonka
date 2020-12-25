{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SizeConstraintSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SizeConstraintSetSummary
  ( SizeConstraintSetSummary (..),

    -- * Smart constructor
    mkSizeConstraintSetSummary,

    -- * Lenses
    scssSizeConstraintSetId,
    scssName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.ResourceId as Types
import qualified Network.AWS.WAF.Types.ResourceName as Types

-- | The @Id@ and @Name@ of a @SizeConstraintSet@ .
--
-- /See:/ 'mkSizeConstraintSetSummary' smart constructor.
data SizeConstraintSetSummary = SizeConstraintSetSummary'
  { -- | A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ).
    --
    -- @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
    sizeConstraintSetId :: Types.ResourceId,
    -- | The name of the @SizeConstraintSet@ , if any.
    name :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SizeConstraintSetSummary' value with any optional fields omitted.
mkSizeConstraintSetSummary ::
  -- | 'sizeConstraintSetId'
  Types.ResourceId ->
  -- | 'name'
  Types.ResourceName ->
  SizeConstraintSetSummary
mkSizeConstraintSetSummary sizeConstraintSetId name =
  SizeConstraintSetSummary' {sizeConstraintSetId, name}

-- | A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ).
--
-- @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
--
-- /Note:/ Consider using 'sizeConstraintSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scssSizeConstraintSetId :: Lens.Lens' SizeConstraintSetSummary Types.ResourceId
scssSizeConstraintSetId = Lens.field @"sizeConstraintSetId"
{-# DEPRECATED scssSizeConstraintSetId "Use generic-lens or generic-optics with 'sizeConstraintSetId' instead." #-}

-- | The name of the @SizeConstraintSet@ , if any.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scssName :: Lens.Lens' SizeConstraintSetSummary Types.ResourceName
scssName = Lens.field @"name"
{-# DEPRECATED scssName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON SizeConstraintSetSummary where
  parseJSON =
    Core.withObject "SizeConstraintSetSummary" Core.$
      \x ->
        SizeConstraintSetSummary'
          Core.<$> (x Core..: "SizeConstraintSetId") Core.<*> (x Core..: "Name")
