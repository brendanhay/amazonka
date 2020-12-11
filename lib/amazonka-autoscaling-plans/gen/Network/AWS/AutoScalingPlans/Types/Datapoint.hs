-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.Datapoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.Datapoint
  ( Datapoint (..),

    -- * Smart constructor
    mkDatapoint,

    -- * Lenses
    dValue,
    dTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a single value in the forecast data used for predictive scaling.
--
-- /See:/ 'mkDatapoint' smart constructor.
data Datapoint = Datapoint'
  { value :: Lude.Maybe Lude.Double,
    timestamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Datapoint' with the minimum fields required to make a request.
--
-- * 'timestamp' - The time stamp for the data point in UTC format.
-- * 'value' - The value of the data point.
mkDatapoint ::
  Datapoint
mkDatapoint =
  Datapoint' {value = Lude.Nothing, timestamp = Lude.Nothing}

-- | The value of the data point.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dValue :: Lens.Lens' Datapoint (Lude.Maybe Lude.Double)
dValue = Lens.lens (value :: Datapoint -> Lude.Maybe Lude.Double) (\s a -> s {value = a} :: Datapoint)
{-# DEPRECATED dValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The time stamp for the data point in UTC format.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTimestamp :: Lens.Lens' Datapoint (Lude.Maybe Lude.Timestamp)
dTimestamp = Lens.lens (timestamp :: Datapoint -> Lude.Maybe Lude.Timestamp) (\s a -> s {timestamp = a} :: Datapoint)
{-# DEPRECATED dTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON Datapoint where
  parseJSON =
    Lude.withObject
      "Datapoint"
      ( \x ->
          Datapoint'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Timestamp")
      )
