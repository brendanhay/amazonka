{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalyScore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.AnomalyScore
  ( AnomalyScore (..)
  -- * Smart constructor
  , mkAnomalyScore
  -- * Lenses
  , asMaxScore
  , asCurrentScore
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Quantifies the anomaly. The higher score means that it is more anomalous. 
--
-- /See:/ 'mkAnomalyScore' smart constructor.
data AnomalyScore = AnomalyScore'
  { maxScore :: Core.Double
    -- ^ The maximum score observed during the @AnomalyDateInterval@ . 
  , currentScore :: Core.Double
    -- ^ The last observed score. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AnomalyScore' value with any optional fields omitted.
mkAnomalyScore
    :: Core.Double -- ^ 'maxScore'
    -> Core.Double -- ^ 'currentScore'
    -> AnomalyScore
mkAnomalyScore maxScore currentScore
  = AnomalyScore'{maxScore, currentScore}

-- | The maximum score observed during the @AnomalyDateInterval@ . 
--
-- /Note:/ Consider using 'maxScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMaxScore :: Lens.Lens' AnomalyScore Core.Double
asMaxScore = Lens.field @"maxScore"
{-# INLINEABLE asMaxScore #-}
{-# DEPRECATED maxScore "Use generic-lens or generic-optics with 'maxScore' instead"  #-}

-- | The last observed score. 
--
-- /Note:/ Consider using 'currentScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCurrentScore :: Lens.Lens' AnomalyScore Core.Double
asCurrentScore = Lens.field @"currentScore"
{-# INLINEABLE asCurrentScore #-}
{-# DEPRECATED currentScore "Use generic-lens or generic-optics with 'currentScore' instead"  #-}

instance Core.FromJSON AnomalyScore where
        parseJSON
          = Core.withObject "AnomalyScore" Core.$
              \ x ->
                AnomalyScore' Core.<$>
                  (x Core..: "MaxScore") Core.<*> x Core..: "CurrentScore"
