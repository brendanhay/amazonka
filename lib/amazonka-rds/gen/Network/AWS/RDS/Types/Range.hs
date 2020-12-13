{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Range
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Range
  ( Range (..),

    -- * Smart constructor
    mkRange,

    -- * Lenses
    rTo,
    rFrom,
    rStep,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A range of integer values.
--
-- /See:/ 'mkRange' smart constructor.
data Range = Range'
  { -- | The maximum value in the range.
    to :: Lude.Maybe Lude.Int,
    -- | The minimum value in the range.
    from :: Lude.Maybe Lude.Int,
    -- | The step value for the range. For example, if you have a range of 5,000 to 10,000, with a step value of 1,000, the valid values start at 5,000 and step up by 1,000. Even though 7,500 is within the range, it isn't a valid value for the range. The valid values are 5,000, 6,000, 7,000, 8,000...
    step :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Range' with the minimum fields required to make a request.
--
-- * 'to' - The maximum value in the range.
-- * 'from' - The minimum value in the range.
-- * 'step' - The step value for the range. For example, if you have a range of 5,000 to 10,000, with a step value of 1,000, the valid values start at 5,000 and step up by 1,000. Even though 7,500 is within the range, it isn't a valid value for the range. The valid values are 5,000, 6,000, 7,000, 8,000...
mkRange ::
  Range
mkRange =
  Range'
    { to = Lude.Nothing,
      from = Lude.Nothing,
      step = Lude.Nothing
    }

-- | The maximum value in the range.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTo :: Lens.Lens' Range (Lude.Maybe Lude.Int)
rTo = Lens.lens (to :: Range -> Lude.Maybe Lude.Int) (\s a -> s {to = a} :: Range)
{-# DEPRECATED rTo "Use generic-lens or generic-optics with 'to' instead." #-}

-- | The minimum value in the range.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rFrom :: Lens.Lens' Range (Lude.Maybe Lude.Int)
rFrom = Lens.lens (from :: Range -> Lude.Maybe Lude.Int) (\s a -> s {from = a} :: Range)
{-# DEPRECATED rFrom "Use generic-lens or generic-optics with 'from' instead." #-}

-- | The step value for the range. For example, if you have a range of 5,000 to 10,000, with a step value of 1,000, the valid values start at 5,000 and step up by 1,000. Even though 7,500 is within the range, it isn't a valid value for the range. The valid values are 5,000, 6,000, 7,000, 8,000...
--
-- /Note:/ Consider using 'step' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStep :: Lens.Lens' Range (Lude.Maybe Lude.Int)
rStep = Lens.lens (step :: Range -> Lude.Maybe Lude.Int) (\s a -> s {step = a} :: Range)
{-# DEPRECATED rStep "Use generic-lens or generic-optics with 'step' instead." #-}

instance Lude.FromXML Range where
  parseXML x =
    Range'
      Lude.<$> (x Lude..@? "To")
      Lude.<*> (x Lude..@? "From")
      Lude.<*> (x Lude..@? "Step")
