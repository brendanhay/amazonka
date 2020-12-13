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
    fsFaultCountLow,
    fsFaultCountHigh,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The predicted high and low fault count. This is used to determine if a service has become anomalous and if an insight should be created.
--
-- /See:/ 'mkForecastStatistics' smart constructor.
data ForecastStatistics = ForecastStatistics'
  { -- | The lower limit of fault counts for a service.
    faultCountLow :: Lude.Maybe Lude.Integer,
    -- | The upper limit of fault counts for a service.
    faultCountHigh :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ForecastStatistics' with the minimum fields required to make a request.
--
-- * 'faultCountLow' - The lower limit of fault counts for a service.
-- * 'faultCountHigh' - The upper limit of fault counts for a service.
mkForecastStatistics ::
  ForecastStatistics
mkForecastStatistics =
  ForecastStatistics'
    { faultCountLow = Lude.Nothing,
      faultCountHigh = Lude.Nothing
    }

-- | The lower limit of fault counts for a service.
--
-- /Note:/ Consider using 'faultCountLow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsFaultCountLow :: Lens.Lens' ForecastStatistics (Lude.Maybe Lude.Integer)
fsFaultCountLow = Lens.lens (faultCountLow :: ForecastStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {faultCountLow = a} :: ForecastStatistics)
{-# DEPRECATED fsFaultCountLow "Use generic-lens or generic-optics with 'faultCountLow' instead." #-}

-- | The upper limit of fault counts for a service.
--
-- /Note:/ Consider using 'faultCountHigh' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsFaultCountHigh :: Lens.Lens' ForecastStatistics (Lude.Maybe Lude.Integer)
fsFaultCountHigh = Lens.lens (faultCountHigh :: ForecastStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {faultCountHigh = a} :: ForecastStatistics)
{-# DEPRECATED fsFaultCountHigh "Use generic-lens or generic-optics with 'faultCountHigh' instead." #-}

instance Lude.FromJSON ForecastStatistics where
  parseJSON =
    Lude.withObject
      "ForecastStatistics"
      ( \x ->
          ForecastStatistics'
            Lude.<$> (x Lude..:? "FaultCountLow")
            Lude.<*> (x Lude..:? "FaultCountHigh")
      )
