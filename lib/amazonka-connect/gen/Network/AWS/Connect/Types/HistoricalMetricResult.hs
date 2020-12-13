{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HistoricalMetricResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HistoricalMetricResult
  ( HistoricalMetricResult (..),

    -- * Smart constructor
    mkHistoricalMetricResult,

    -- * Lenses
    hmrCollections,
    hmrDimensions,
  )
where

import Network.AWS.Connect.Types.Dimensions
import Network.AWS.Connect.Types.HistoricalMetricData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the historical metrics retrieved.
--
-- /See:/ 'mkHistoricalMetricResult' smart constructor.
data HistoricalMetricResult = HistoricalMetricResult'
  { -- | The set of metrics.
    collections :: Lude.Maybe [HistoricalMetricData],
    -- | The dimension for the metrics.
    dimensions :: Lude.Maybe Dimensions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HistoricalMetricResult' with the minimum fields required to make a request.
--
-- * 'collections' - The set of metrics.
-- * 'dimensions' - The dimension for the metrics.
mkHistoricalMetricResult ::
  HistoricalMetricResult
mkHistoricalMetricResult =
  HistoricalMetricResult'
    { collections = Lude.Nothing,
      dimensions = Lude.Nothing
    }

-- | The set of metrics.
--
-- /Note:/ Consider using 'collections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmrCollections :: Lens.Lens' HistoricalMetricResult (Lude.Maybe [HistoricalMetricData])
hmrCollections = Lens.lens (collections :: HistoricalMetricResult -> Lude.Maybe [HistoricalMetricData]) (\s a -> s {collections = a} :: HistoricalMetricResult)
{-# DEPRECATED hmrCollections "Use generic-lens or generic-optics with 'collections' instead." #-}

-- | The dimension for the metrics.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmrDimensions :: Lens.Lens' HistoricalMetricResult (Lude.Maybe Dimensions)
hmrDimensions = Lens.lens (dimensions :: HistoricalMetricResult -> Lude.Maybe Dimensions) (\s a -> s {dimensions = a} :: HistoricalMetricResult)
{-# DEPRECATED hmrDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Lude.FromJSON HistoricalMetricResult where
  parseJSON =
    Lude.withObject
      "HistoricalMetricResult"
      ( \x ->
          HistoricalMetricResult'
            Lude.<$> (x Lude..:? "Collections" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Dimensions")
      )
