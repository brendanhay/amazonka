{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SizeConstraintSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SizeConstraintSetSummary
  ( SizeConstraintSetSummary (..),

    -- * Smart constructor
    mkSizeConstraintSetSummary,

    -- * Lenses
    scssSizeConstraintSetId,
    scssName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @Id@ and @Name@ of a @SizeConstraintSet@ .
--
-- /See:/ 'mkSizeConstraintSetSummary' smart constructor.
data SizeConstraintSetSummary = SizeConstraintSetSummary'
  { sizeConstraintSetId ::
      Lude.Text,
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

-- | Creates a value of 'SizeConstraintSetSummary' with the minimum fields required to make a request.
--
-- * 'name' - The name of the @SizeConstraintSet@ , if any.
-- * 'sizeConstraintSetId' - A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ).
--
-- @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
mkSizeConstraintSetSummary ::
  -- | 'sizeConstraintSetId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  SizeConstraintSetSummary
mkSizeConstraintSetSummary pSizeConstraintSetId_ pName_ =
  SizeConstraintSetSummary'
    { sizeConstraintSetId =
        pSizeConstraintSetId_,
      name = pName_
    }

-- | A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ).
--
-- @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
--
-- /Note:/ Consider using 'sizeConstraintSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scssSizeConstraintSetId :: Lens.Lens' SizeConstraintSetSummary Lude.Text
scssSizeConstraintSetId = Lens.lens (sizeConstraintSetId :: SizeConstraintSetSummary -> Lude.Text) (\s a -> s {sizeConstraintSetId = a} :: SizeConstraintSetSummary)
{-# DEPRECATED scssSizeConstraintSetId "Use generic-lens or generic-optics with 'sizeConstraintSetId' instead." #-}

-- | The name of the @SizeConstraintSet@ , if any.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scssName :: Lens.Lens' SizeConstraintSetSummary Lude.Text
scssName = Lens.lens (name :: SizeConstraintSetSummary -> Lude.Text) (\s a -> s {name = a} :: SizeConstraintSetSummary)
{-# DEPRECATED scssName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON SizeConstraintSetSummary where
  parseJSON =
    Lude.withObject
      "SizeConstraintSetSummary"
      ( \x ->
          SizeConstraintSetSummary'
            Lude.<$> (x Lude..: "SizeConstraintSetId") Lude.<*> (x Lude..: "Name")
      )
