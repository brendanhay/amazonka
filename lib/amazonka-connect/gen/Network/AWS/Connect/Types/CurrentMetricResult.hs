{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.CurrentMetricResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.CurrentMetricResult
  ( CurrentMetricResult (..),

    -- * Smart constructor
    mkCurrentMetricResult,

    -- * Lenses
    cmrCollections,
    cmrDimensions,
  )
where

import Network.AWS.Connect.Types.CurrentMetricData
import Network.AWS.Connect.Types.Dimensions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a set of real-time metrics.
--
-- /See:/ 'mkCurrentMetricResult' smart constructor.
data CurrentMetricResult = CurrentMetricResult'
  { -- | The set of metrics.
    collections :: Lude.Maybe [CurrentMetricData],
    -- | The dimensions for the metrics.
    dimensions :: Lude.Maybe Dimensions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CurrentMetricResult' with the minimum fields required to make a request.
--
-- * 'collections' - The set of metrics.
-- * 'dimensions' - The dimensions for the metrics.
mkCurrentMetricResult ::
  CurrentMetricResult
mkCurrentMetricResult =
  CurrentMetricResult'
    { collections = Lude.Nothing,
      dimensions = Lude.Nothing
    }

-- | The set of metrics.
--
-- /Note:/ Consider using 'collections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrCollections :: Lens.Lens' CurrentMetricResult (Lude.Maybe [CurrentMetricData])
cmrCollections = Lens.lens (collections :: CurrentMetricResult -> Lude.Maybe [CurrentMetricData]) (\s a -> s {collections = a} :: CurrentMetricResult)
{-# DEPRECATED cmrCollections "Use generic-lens or generic-optics with 'collections' instead." #-}

-- | The dimensions for the metrics.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrDimensions :: Lens.Lens' CurrentMetricResult (Lude.Maybe Dimensions)
cmrDimensions = Lens.lens (dimensions :: CurrentMetricResult -> Lude.Maybe Dimensions) (\s a -> s {dimensions = a} :: CurrentMetricResult)
{-# DEPRECATED cmrDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Lude.FromJSON CurrentMetricResult where
  parseJSON =
    Lude.withObject
      "CurrentMetricResult"
      ( \x ->
          CurrentMetricResult'
            Lude.<$> (x Lude..:? "Collections" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Dimensions")
      )
