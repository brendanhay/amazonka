{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ForecastStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ForecastStatistics
  ( ForecastStatistics (..),

    -- * Smart constructor
    mkForecastStatistics,

    -- * Lenses
    fsFaultCountHigh,
    fsFaultCountLow,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The predicted high and low fault count. This is used to determine if a service has become anomalous and if an insight should be created.
--
-- /See:/ 'mkForecastStatistics' smart constructor.
data ForecastStatistics = ForecastStatistics'
  { -- | The upper limit of fault counts for a service.
    faultCountHigh :: Core.Maybe Core.Integer,
    -- | The lower limit of fault counts for a service.
    faultCountLow :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ForecastStatistics' value with any optional fields omitted.
mkForecastStatistics ::
  ForecastStatistics
mkForecastStatistics =
  ForecastStatistics'
    { faultCountHigh = Core.Nothing,
      faultCountLow = Core.Nothing
    }

-- | The upper limit of fault counts for a service.
--
-- /Note:/ Consider using 'faultCountHigh' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsFaultCountHigh :: Lens.Lens' ForecastStatistics (Core.Maybe Core.Integer)
fsFaultCountHigh = Lens.field @"faultCountHigh"
{-# DEPRECATED fsFaultCountHigh "Use generic-lens or generic-optics with 'faultCountHigh' instead." #-}

-- | The lower limit of fault counts for a service.
--
-- /Note:/ Consider using 'faultCountLow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsFaultCountLow :: Lens.Lens' ForecastStatistics (Core.Maybe Core.Integer)
fsFaultCountLow = Lens.field @"faultCountLow"
{-# DEPRECATED fsFaultCountLow "Use generic-lens or generic-optics with 'faultCountLow' instead." #-}

instance Core.FromJSON ForecastStatistics where
  parseJSON =
    Core.withObject "ForecastStatistics" Core.$
      \x ->
        ForecastStatistics'
          Core.<$> (x Core..:? "FaultCountHigh") Core.<*> (x Core..:? "FaultCountLow")
