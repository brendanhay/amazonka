{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dTimestamp,
    dValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a single value in the forecast data used for predictive scaling.
--
-- /See:/ 'mkDatapoint' smart constructor.
data Datapoint = Datapoint'
  { -- | The time stamp for the data point in UTC format.
    timestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The value of the data point.
    value :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Datapoint' value with any optional fields omitted.
mkDatapoint ::
  Datapoint
mkDatapoint =
  Datapoint' {timestamp = Core.Nothing, value = Core.Nothing}

-- | The time stamp for the data point in UTC format.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTimestamp :: Lens.Lens' Datapoint (Core.Maybe Core.NominalDiffTime)
dTimestamp = Lens.field @"timestamp"
{-# DEPRECATED dTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The value of the data point.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dValue :: Lens.Lens' Datapoint (Core.Maybe Core.Double)
dValue = Lens.field @"value"
{-# DEPRECATED dValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Datapoint where
  parseJSON =
    Core.withObject "Datapoint" Core.$
      \x ->
        Datapoint'
          Core.<$> (x Core..:? "Timestamp") Core.<*> (x Core..:? "Value")
