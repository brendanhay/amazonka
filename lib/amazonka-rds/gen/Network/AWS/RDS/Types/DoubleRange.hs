{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DoubleRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DoubleRange
  ( DoubleRange (..),

    -- * Smart constructor
    mkDoubleRange,

    -- * Lenses
    drTo,
    drFrom,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A range of double values.
--
-- /See:/ 'mkDoubleRange' smart constructor.
data DoubleRange = DoubleRange'
  { to :: Lude.Maybe Lude.Double,
    from :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DoubleRange' with the minimum fields required to make a request.
--
-- * 'from' - The minimum value in the range.
-- * 'to' - The maximum value in the range.
mkDoubleRange ::
  DoubleRange
mkDoubleRange =
  DoubleRange' {to = Lude.Nothing, from = Lude.Nothing}

-- | The maximum value in the range.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drTo :: Lens.Lens' DoubleRange (Lude.Maybe Lude.Double)
drTo = Lens.lens (to :: DoubleRange -> Lude.Maybe Lude.Double) (\s a -> s {to = a} :: DoubleRange)
{-# DEPRECATED drTo "Use generic-lens or generic-optics with 'to' instead." #-}

-- | The minimum value in the range.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drFrom :: Lens.Lens' DoubleRange (Lude.Maybe Lude.Double)
drFrom = Lens.lens (from :: DoubleRange -> Lude.Maybe Lude.Double) (\s a -> s {from = a} :: DoubleRange)
{-# DEPRECATED drFrom "Use generic-lens or generic-optics with 'from' instead." #-}

instance Lude.FromXML DoubleRange where
  parseXML x =
    DoubleRange'
      Lude.<$> (x Lude..@? "To") Lude.<*> (x Lude..@? "From")
