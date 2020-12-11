-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansCoverage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansCoverage
  ( SavingsPlansCoverage (..),

    -- * Smart constructor
    mkSavingsPlansCoverage,

    -- * Lenses
    spcTimePeriod,
    spcCoverage,
    spcAttributes,
  )
where

import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The amount of Savings Plans eligible usage that is covered by Savings Plans. All calculations consider the On-Demand equivalent of your Savings Plans usage.
--
-- /See:/ 'mkSavingsPlansCoverage' smart constructor.
data SavingsPlansCoverage = SavingsPlansCoverage'
  { timePeriod ::
      Lude.Maybe DateInterval,
    coverage :: Lude.Maybe SavingsPlansCoverageData,
    attributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SavingsPlansCoverage' with the minimum fields required to make a request.
--
-- * 'attributes' - The attribute that applies to a specific @Dimension@ .
-- * 'coverage' - The amount of Savings Plans eligible usage that the Savings Plans covered.
-- * 'timePeriod' - Undocumented field.
mkSavingsPlansCoverage ::
  SavingsPlansCoverage
mkSavingsPlansCoverage =
  SavingsPlansCoverage'
    { timePeriod = Lude.Nothing,
      coverage = Lude.Nothing,
      attributes = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcTimePeriod :: Lens.Lens' SavingsPlansCoverage (Lude.Maybe DateInterval)
spcTimePeriod = Lens.lens (timePeriod :: SavingsPlansCoverage -> Lude.Maybe DateInterval) (\s a -> s {timePeriod = a} :: SavingsPlansCoverage)
{-# DEPRECATED spcTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The amount of Savings Plans eligible usage that the Savings Plans covered.
--
-- /Note:/ Consider using 'coverage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcCoverage :: Lens.Lens' SavingsPlansCoverage (Lude.Maybe SavingsPlansCoverageData)
spcCoverage = Lens.lens (coverage :: SavingsPlansCoverage -> Lude.Maybe SavingsPlansCoverageData) (\s a -> s {coverage = a} :: SavingsPlansCoverage)
{-# DEPRECATED spcCoverage "Use generic-lens or generic-optics with 'coverage' instead." #-}

-- | The attribute that applies to a specific @Dimension@ .
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcAttributes :: Lens.Lens' SavingsPlansCoverage (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
spcAttributes = Lens.lens (attributes :: SavingsPlansCoverage -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: SavingsPlansCoverage)
{-# DEPRECATED spcAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromJSON SavingsPlansCoverage where
  parseJSON =
    Lude.withObject
      "SavingsPlansCoverage"
      ( \x ->
          SavingsPlansCoverage'
            Lude.<$> (x Lude..:? "TimePeriod")
            Lude.<*> (x Lude..:? "Coverage")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
      )
