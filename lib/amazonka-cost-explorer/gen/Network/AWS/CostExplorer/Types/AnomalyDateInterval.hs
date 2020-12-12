{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalyDateInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalyDateInterval
  ( AnomalyDateInterval (..),

    -- * Smart constructor
    mkAnomalyDateInterval,

    -- * Lenses
    adiEndDate,
    adiStartDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The time period for an anomaly.
--
-- /See:/ 'mkAnomalyDateInterval' smart constructor.
data AnomalyDateInterval = AnomalyDateInterval'
  { endDate ::
      Lude.Maybe Lude.Text,
    startDate :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnomalyDateInterval' with the minimum fields required to make a request.
--
-- * 'endDate' - The last date an anomaly was observed.
-- * 'startDate' - The first date an anomaly was observed.
mkAnomalyDateInterval ::
  -- | 'startDate'
  Lude.Text ->
  AnomalyDateInterval
mkAnomalyDateInterval pStartDate_ =
  AnomalyDateInterval'
    { endDate = Lude.Nothing,
      startDate = pStartDate_
    }

-- | The last date an anomaly was observed.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adiEndDate :: Lens.Lens' AnomalyDateInterval (Lude.Maybe Lude.Text)
adiEndDate = Lens.lens (endDate :: AnomalyDateInterval -> Lude.Maybe Lude.Text) (\s a -> s {endDate = a} :: AnomalyDateInterval)
{-# DEPRECATED adiEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The first date an anomaly was observed.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adiStartDate :: Lens.Lens' AnomalyDateInterval Lude.Text
adiStartDate = Lens.lens (startDate :: AnomalyDateInterval -> Lude.Text) (\s a -> s {startDate = a} :: AnomalyDateInterval)
{-# DEPRECATED adiStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

instance Lude.ToJSON AnomalyDateInterval where
  toJSON AnomalyDateInterval' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EndDate" Lude..=) Lude.<$> endDate,
            Lude.Just ("StartDate" Lude..= startDate)
          ]
      )
