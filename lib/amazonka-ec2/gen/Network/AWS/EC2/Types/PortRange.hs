{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PortRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PortRange
  ( PortRange (..),

    -- * Smart constructor
    mkPortRange,

    -- * Lenses
    prTo,
    prFrom,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a range of ports.
--
-- /See:/ 'mkPortRange' smart constructor.
data PortRange = PortRange'
  { to :: Lude.Maybe Lude.Int,
    from :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PortRange' with the minimum fields required to make a request.
--
-- * 'from' - The first port in the range.
-- * 'to' - The last port in the range.
mkPortRange ::
  PortRange
mkPortRange = PortRange' {to = Lude.Nothing, from = Lude.Nothing}

-- | The last port in the range.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prTo :: Lens.Lens' PortRange (Lude.Maybe Lude.Int)
prTo = Lens.lens (to :: PortRange -> Lude.Maybe Lude.Int) (\s a -> s {to = a} :: PortRange)
{-# DEPRECATED prTo "Use generic-lens or generic-optics with 'to' instead." #-}

-- | The first port in the range.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prFrom :: Lens.Lens' PortRange (Lude.Maybe Lude.Int)
prFrom = Lens.lens (from :: PortRange -> Lude.Maybe Lude.Int) (\s a -> s {from = a} :: PortRange)
{-# DEPRECATED prFrom "Use generic-lens or generic-optics with 'from' instead." #-}

instance Lude.FromXML PortRange where
  parseXML x =
    PortRange'
      Lude.<$> (x Lude..@? "to") Lude.<*> (x Lude..@? "from")

instance Lude.ToQuery PortRange where
  toQuery PortRange' {..} =
    Lude.mconcat ["To" Lude.=: to, "From" Lude.=: from]
