{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetGroup
  ( TargetGroup (..),

    -- * Smart constructor
    mkTargetGroup,

    -- * Lenses
    tgARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a load balancer target group.
--
-- /See:/ 'mkTargetGroup' smart constructor.
newtype TargetGroup = TargetGroup' {arn :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetGroup' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the target group.
mkTargetGroup ::
  TargetGroup
mkTargetGroup = TargetGroup' {arn = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgARN :: Lens.Lens' TargetGroup (Lude.Maybe Lude.Text)
tgARN = Lens.lens (arn :: TargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: TargetGroup)
{-# DEPRECATED tgARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromXML TargetGroup where
  parseXML x = TargetGroup' Lude.<$> (x Lude..@? "arn")

instance Lude.ToQuery TargetGroup where
  toQuery TargetGroup' {..} = Lude.mconcat ["Arn" Lude.=: arn]
