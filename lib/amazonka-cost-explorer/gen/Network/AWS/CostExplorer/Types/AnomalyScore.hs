{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalyScore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalyScore
  ( AnomalyScore (..),

    -- * Smart constructor
    mkAnomalyScore,

    -- * Lenses
    asMaxScore,
    asCurrentScore,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Quantifies the anomaly. The higher score means that it is more anomalous.
--
-- /See:/ 'mkAnomalyScore' smart constructor.
data AnomalyScore = AnomalyScore'
  { maxScore :: Lude.Double,
    currentScore :: Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnomalyScore' with the minimum fields required to make a request.
--
-- * 'currentScore' - The last observed score.
-- * 'maxScore' - The maximum score observed during the @AnomalyDateInterval@ .
mkAnomalyScore ::
  -- | 'maxScore'
  Lude.Double ->
  -- | 'currentScore'
  Lude.Double ->
  AnomalyScore
mkAnomalyScore pMaxScore_ pCurrentScore_ =
  AnomalyScore'
    { maxScore = pMaxScore_,
      currentScore = pCurrentScore_
    }

-- | The maximum score observed during the @AnomalyDateInterval@ .
--
-- /Note:/ Consider using 'maxScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMaxScore :: Lens.Lens' AnomalyScore Lude.Double
asMaxScore = Lens.lens (maxScore :: AnomalyScore -> Lude.Double) (\s a -> s {maxScore = a} :: AnomalyScore)
{-# DEPRECATED asMaxScore "Use generic-lens or generic-optics with 'maxScore' instead." #-}

-- | The last observed score.
--
-- /Note:/ Consider using 'currentScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCurrentScore :: Lens.Lens' AnomalyScore Lude.Double
asCurrentScore = Lens.lens (currentScore :: AnomalyScore -> Lude.Double) (\s a -> s {currentScore = a} :: AnomalyScore)
{-# DEPRECATED asCurrentScore "Use generic-lens or generic-optics with 'currentScore' instead." #-}

instance Lude.FromJSON AnomalyScore where
  parseJSON =
    Lude.withObject
      "AnomalyScore"
      ( \x ->
          AnomalyScore'
            Lude.<$> (x Lude..: "MaxScore") Lude.<*> (x Lude..: "CurrentScore")
      )
