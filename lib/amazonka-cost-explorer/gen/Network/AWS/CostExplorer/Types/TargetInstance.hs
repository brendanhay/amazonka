-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.TargetInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TargetInstance
  ( TargetInstance (..),

    -- * Smart constructor
    mkTargetInstance,

    -- * Lenses
    tiCurrencyCode,
    tiResourceDetails,
    tiDefaultTargetInstance,
    tiEstimatedMonthlyCost,
    tiEstimatedMonthlySavings,
    tiExpectedResourceUtilization,
  )
where

import Network.AWS.CostExplorer.Types.ResourceDetails
import Network.AWS.CostExplorer.Types.ResourceUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details on recommended instance.
--
-- /See:/ 'mkTargetInstance' smart constructor.
data TargetInstance = TargetInstance'
  { currencyCode ::
      Lude.Maybe Lude.Text,
    resourceDetails :: Lude.Maybe ResourceDetails,
    defaultTargetInstance :: Lude.Maybe Lude.Bool,
    estimatedMonthlyCost :: Lude.Maybe Lude.Text,
    estimatedMonthlySavings :: Lude.Maybe Lude.Text,
    expectedResourceUtilization :: Lude.Maybe ResourceUtilization
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetInstance' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency code that AWS used to calculate the costs for this instance.
-- * 'defaultTargetInstance' - Indicates whether this recommendation is the defaulted AWS recommendation.
-- * 'estimatedMonthlyCost' - Expected cost to operate this instance type on a monthly basis.
-- * 'estimatedMonthlySavings' - Estimated savings resulting from modification, on a monthly basis.
-- * 'expectedResourceUtilization' - Expected utilization metrics for target instance type.
-- * 'resourceDetails' - Details on the target instance type.
mkTargetInstance ::
  TargetInstance
mkTargetInstance =
  TargetInstance'
    { currencyCode = Lude.Nothing,
      resourceDetails = Lude.Nothing,
      defaultTargetInstance = Lude.Nothing,
      estimatedMonthlyCost = Lude.Nothing,
      estimatedMonthlySavings = Lude.Nothing,
      expectedResourceUtilization = Lude.Nothing
    }

-- | The currency code that AWS used to calculate the costs for this instance.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiCurrencyCode :: Lens.Lens' TargetInstance (Lude.Maybe Lude.Text)
tiCurrencyCode = Lens.lens (currencyCode :: TargetInstance -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: TargetInstance)
{-# DEPRECATED tiCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Details on the target instance type.
--
-- /Note:/ Consider using 'resourceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiResourceDetails :: Lens.Lens' TargetInstance (Lude.Maybe ResourceDetails)
tiResourceDetails = Lens.lens (resourceDetails :: TargetInstance -> Lude.Maybe ResourceDetails) (\s a -> s {resourceDetails = a} :: TargetInstance)
{-# DEPRECATED tiResourceDetails "Use generic-lens or generic-optics with 'resourceDetails' instead." #-}

-- | Indicates whether this recommendation is the defaulted AWS recommendation.
--
-- /Note:/ Consider using 'defaultTargetInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiDefaultTargetInstance :: Lens.Lens' TargetInstance (Lude.Maybe Lude.Bool)
tiDefaultTargetInstance = Lens.lens (defaultTargetInstance :: TargetInstance -> Lude.Maybe Lude.Bool) (\s a -> s {defaultTargetInstance = a} :: TargetInstance)
{-# DEPRECATED tiDefaultTargetInstance "Use generic-lens or generic-optics with 'defaultTargetInstance' instead." #-}

-- | Expected cost to operate this instance type on a monthly basis.
--
-- /Note:/ Consider using 'estimatedMonthlyCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiEstimatedMonthlyCost :: Lens.Lens' TargetInstance (Lude.Maybe Lude.Text)
tiEstimatedMonthlyCost = Lens.lens (estimatedMonthlyCost :: TargetInstance -> Lude.Maybe Lude.Text) (\s a -> s {estimatedMonthlyCost = a} :: TargetInstance)
{-# DEPRECATED tiEstimatedMonthlyCost "Use generic-lens or generic-optics with 'estimatedMonthlyCost' instead." #-}

-- | Estimated savings resulting from modification, on a monthly basis.
--
-- /Note:/ Consider using 'estimatedMonthlySavings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiEstimatedMonthlySavings :: Lens.Lens' TargetInstance (Lude.Maybe Lude.Text)
tiEstimatedMonthlySavings = Lens.lens (estimatedMonthlySavings :: TargetInstance -> Lude.Maybe Lude.Text) (\s a -> s {estimatedMonthlySavings = a} :: TargetInstance)
{-# DEPRECATED tiEstimatedMonthlySavings "Use generic-lens or generic-optics with 'estimatedMonthlySavings' instead." #-}

-- | Expected utilization metrics for target instance type.
--
-- /Note:/ Consider using 'expectedResourceUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiExpectedResourceUtilization :: Lens.Lens' TargetInstance (Lude.Maybe ResourceUtilization)
tiExpectedResourceUtilization = Lens.lens (expectedResourceUtilization :: TargetInstance -> Lude.Maybe ResourceUtilization) (\s a -> s {expectedResourceUtilization = a} :: TargetInstance)
{-# DEPRECATED tiExpectedResourceUtilization "Use generic-lens or generic-optics with 'expectedResourceUtilization' instead." #-}

instance Lude.FromJSON TargetInstance where
  parseJSON =
    Lude.withObject
      "TargetInstance"
      ( \x ->
          TargetInstance'
            Lude.<$> (x Lude..:? "CurrencyCode")
            Lude.<*> (x Lude..:? "ResourceDetails")
            Lude.<*> (x Lude..:? "DefaultTargetInstance")
            Lude.<*> (x Lude..:? "EstimatedMonthlyCost")
            Lude.<*> (x Lude..:? "EstimatedMonthlySavings")
            Lude.<*> (x Lude..:? "ExpectedResourceUtilization")
      )
