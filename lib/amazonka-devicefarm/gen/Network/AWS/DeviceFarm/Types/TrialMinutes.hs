-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TrialMinutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TrialMinutes
  ( TrialMinutes (..),

    -- * Smart constructor
    mkTrialMinutes,

    -- * Lenses
    tmRemaining,
    tmTotal,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about free trial device minutes for an AWS account.
--
-- /See:/ 'mkTrialMinutes' smart constructor.
data TrialMinutes = TrialMinutes'
  { remaining ::
      Lude.Maybe Lude.Double,
    total :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrialMinutes' with the minimum fields required to make a request.
--
-- * 'remaining' - The number of free trial minutes remaining in the account.
-- * 'total' - The total number of free trial minutes that the account started with.
mkTrialMinutes ::
  TrialMinutes
mkTrialMinutes =
  TrialMinutes' {remaining = Lude.Nothing, total = Lude.Nothing}

-- | The number of free trial minutes remaining in the account.
--
-- /Note:/ Consider using 'remaining' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmRemaining :: Lens.Lens' TrialMinutes (Lude.Maybe Lude.Double)
tmRemaining = Lens.lens (remaining :: TrialMinutes -> Lude.Maybe Lude.Double) (\s a -> s {remaining = a} :: TrialMinutes)
{-# DEPRECATED tmRemaining "Use generic-lens or generic-optics with 'remaining' instead." #-}

-- | The total number of free trial minutes that the account started with.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmTotal :: Lens.Lens' TrialMinutes (Lude.Maybe Lude.Double)
tmTotal = Lens.lens (total :: TrialMinutes -> Lude.Maybe Lude.Double) (\s a -> s {total = a} :: TrialMinutes)
{-# DEPRECATED tmTotal "Use generic-lens or generic-optics with 'total' instead." #-}

instance Lude.FromJSON TrialMinutes where
  parseJSON =
    Lude.withObject
      "TrialMinutes"
      ( \x ->
          TrialMinutes'
            Lude.<$> (x Lude..:? "remaining") Lude.<*> (x Lude..:? "total")
      )
