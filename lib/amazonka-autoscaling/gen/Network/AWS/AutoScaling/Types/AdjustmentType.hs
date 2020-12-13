{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.AdjustmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.AdjustmentType
  ( AdjustmentType (..),

    -- * Smart constructor
    mkAdjustmentType,

    -- * Lenses
    atAdjustmentType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a policy adjustment type.
--
-- /See:/ 'mkAdjustmentType' smart constructor.
newtype AdjustmentType = AdjustmentType'
  { -- | The policy adjustment type. The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
    adjustmentType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdjustmentType' with the minimum fields required to make a request.
--
-- * 'adjustmentType' - The policy adjustment type. The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
mkAdjustmentType ::
  AdjustmentType
mkAdjustmentType = AdjustmentType' {adjustmentType = Lude.Nothing}

-- | The policy adjustment type. The valid values are @ChangeInCapacity@ , @ExactCapacity@ , and @PercentChangeInCapacity@ .
--
-- /Note:/ Consider using 'adjustmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atAdjustmentType :: Lens.Lens' AdjustmentType (Lude.Maybe Lude.Text)
atAdjustmentType = Lens.lens (adjustmentType :: AdjustmentType -> Lude.Maybe Lude.Text) (\s a -> s {adjustmentType = a} :: AdjustmentType)
{-# DEPRECATED atAdjustmentType "Use generic-lens or generic-optics with 'adjustmentType' instead." #-}

instance Lude.FromXML AdjustmentType where
  parseXML x = AdjustmentType' Lude.<$> (x Lude..@? "AdjustmentType")
