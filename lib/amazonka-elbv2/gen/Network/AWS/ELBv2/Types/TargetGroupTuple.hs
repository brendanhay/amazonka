{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetGroupTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetGroupTuple
  ( TargetGroupTuple (..),

    -- * Smart constructor
    mkTargetGroupTuple,

    -- * Lenses
    tgtWeight,
    tgtTargetGroupARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about how traffic will be distributed between multiple target groups in a forward rule.
--
-- /See:/ 'mkTargetGroupTuple' smart constructor.
data TargetGroupTuple = TargetGroupTuple'
  { weight ::
      Lude.Maybe Lude.Int,
    targetGroupARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetGroupTuple' with the minimum fields required to make a request.
--
-- * 'targetGroupARN' - The Amazon Resource Name (ARN) of the target group.
-- * 'weight' - The weight. The range is 0 to 999.
mkTargetGroupTuple ::
  TargetGroupTuple
mkTargetGroupTuple =
  TargetGroupTuple'
    { weight = Lude.Nothing,
      targetGroupARN = Lude.Nothing
    }

-- | The weight. The range is 0 to 999.
--
-- /Note:/ Consider using 'weight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgtWeight :: Lens.Lens' TargetGroupTuple (Lude.Maybe Lude.Int)
tgtWeight = Lens.lens (weight :: TargetGroupTuple -> Lude.Maybe Lude.Int) (\s a -> s {weight = a} :: TargetGroupTuple)
{-# DEPRECATED tgtWeight "Use generic-lens or generic-optics with 'weight' instead." #-}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgtTargetGroupARN :: Lens.Lens' TargetGroupTuple (Lude.Maybe Lude.Text)
tgtTargetGroupARN = Lens.lens (targetGroupARN :: TargetGroupTuple -> Lude.Maybe Lude.Text) (\s a -> s {targetGroupARN = a} :: TargetGroupTuple)
{-# DEPRECATED tgtTargetGroupARN "Use generic-lens or generic-optics with 'targetGroupARN' instead." #-}

instance Lude.FromXML TargetGroupTuple where
  parseXML x =
    TargetGroupTuple'
      Lude.<$> (x Lude..@? "Weight") Lude.<*> (x Lude..@? "TargetGroupArn")

instance Lude.ToQuery TargetGroupTuple where
  toQuery TargetGroupTuple' {..} =
    Lude.mconcat
      ["Weight" Lude.=: weight, "TargetGroupArn" Lude.=: targetGroupARN]
