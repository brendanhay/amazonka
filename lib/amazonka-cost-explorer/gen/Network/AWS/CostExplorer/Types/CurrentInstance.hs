{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CurrentInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CurrentInstance
  ( CurrentInstance (..),

    -- * Smart constructor
    mkCurrentInstance,

    -- * Lenses
    ciResourceId,
    ciCurrencyCode,
    ciResourceUtilization,
    ciResourceDetails,
    ciTotalRunningHoursInLookbackPeriod,
    ciReservationCoveredHoursInLookbackPeriod,
    ciOnDemandHoursInLookbackPeriod,
    ciMonthlyCost,
    ciInstanceName,
    ciSavingsPlansCoveredHoursInLookbackPeriod,
    ciTags,
  )
where

import Network.AWS.CostExplorer.Types.ResourceDetails
import Network.AWS.CostExplorer.Types.ResourceUtilization
import Network.AWS.CostExplorer.Types.TagValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Context about the current instance.
--
-- /See:/ 'mkCurrentInstance' smart constructor.
data CurrentInstance = CurrentInstance'
  { -- | Resource ID of the current instance.
    resourceId :: Lude.Maybe Lude.Text,
    -- | The currency code that AWS used to calculate the costs for this instance.
    currencyCode :: Lude.Maybe Lude.Text,
    -- | Utilization information of the current instance during the lookback period.
    resourceUtilization :: Lude.Maybe ResourceUtilization,
    -- | Details about the resource and utilization.
    resourceDetails :: Lude.Maybe ResourceDetails,
    -- | The total number of hours the instance ran during the lookback period.
    totalRunningHoursInLookbackPeriod :: Lude.Maybe Lude.Text,
    -- | Number of hours during the lookback period covered by reservations.
    reservationCoveredHoursInLookbackPeriod :: Lude.Maybe Lude.Text,
    -- | Number of hours during the lookback period billed at On-Demand rates.
    onDemandHoursInLookbackPeriod :: Lude.Maybe Lude.Text,
    -- | Current On-Demand cost of operating this instance on a monthly basis.
    monthlyCost :: Lude.Maybe Lude.Text,
    -- | The name you've given an instance. This field will show as blank if you haven't given the instance a name.
    instanceName :: Lude.Maybe Lude.Text,
    -- | Number of hours during the lookback period covered by Savings Plans.
    savingsPlansCoveredHoursInLookbackPeriod :: Lude.Maybe Lude.Text,
    -- | Cost allocation resource tags applied to the instance.
    tags :: Lude.Maybe [TagValues]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CurrentInstance' with the minimum fields required to make a request.
--
-- * 'resourceId' - Resource ID of the current instance.
-- * 'currencyCode' - The currency code that AWS used to calculate the costs for this instance.
-- * 'resourceUtilization' - Utilization information of the current instance during the lookback period.
-- * 'resourceDetails' - Details about the resource and utilization.
-- * 'totalRunningHoursInLookbackPeriod' - The total number of hours the instance ran during the lookback period.
-- * 'reservationCoveredHoursInLookbackPeriod' - Number of hours during the lookback period covered by reservations.
-- * 'onDemandHoursInLookbackPeriod' - Number of hours during the lookback period billed at On-Demand rates.
-- * 'monthlyCost' - Current On-Demand cost of operating this instance on a monthly basis.
-- * 'instanceName' - The name you've given an instance. This field will show as blank if you haven't given the instance a name.
-- * 'savingsPlansCoveredHoursInLookbackPeriod' - Number of hours during the lookback period covered by Savings Plans.
-- * 'tags' - Cost allocation resource tags applied to the instance.
mkCurrentInstance ::
  CurrentInstance
mkCurrentInstance =
  CurrentInstance'
    { resourceId = Lude.Nothing,
      currencyCode = Lude.Nothing,
      resourceUtilization = Lude.Nothing,
      resourceDetails = Lude.Nothing,
      totalRunningHoursInLookbackPeriod = Lude.Nothing,
      reservationCoveredHoursInLookbackPeriod = Lude.Nothing,
      onDemandHoursInLookbackPeriod = Lude.Nothing,
      monthlyCost = Lude.Nothing,
      instanceName = Lude.Nothing,
      savingsPlansCoveredHoursInLookbackPeriod = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Resource ID of the current instance.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceId :: Lens.Lens' CurrentInstance (Lude.Maybe Lude.Text)
ciResourceId = Lens.lens (resourceId :: CurrentInstance -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: CurrentInstance)
{-# DEPRECATED ciResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The currency code that AWS used to calculate the costs for this instance.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCurrencyCode :: Lens.Lens' CurrentInstance (Lude.Maybe Lude.Text)
ciCurrencyCode = Lens.lens (currencyCode :: CurrentInstance -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: CurrentInstance)
{-# DEPRECATED ciCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Utilization information of the current instance during the lookback period.
--
-- /Note:/ Consider using 'resourceUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceUtilization :: Lens.Lens' CurrentInstance (Lude.Maybe ResourceUtilization)
ciResourceUtilization = Lens.lens (resourceUtilization :: CurrentInstance -> Lude.Maybe ResourceUtilization) (\s a -> s {resourceUtilization = a} :: CurrentInstance)
{-# DEPRECATED ciResourceUtilization "Use generic-lens or generic-optics with 'resourceUtilization' instead." #-}

-- | Details about the resource and utilization.
--
-- /Note:/ Consider using 'resourceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceDetails :: Lens.Lens' CurrentInstance (Lude.Maybe ResourceDetails)
ciResourceDetails = Lens.lens (resourceDetails :: CurrentInstance -> Lude.Maybe ResourceDetails) (\s a -> s {resourceDetails = a} :: CurrentInstance)
{-# DEPRECATED ciResourceDetails "Use generic-lens or generic-optics with 'resourceDetails' instead." #-}

-- | The total number of hours the instance ran during the lookback period.
--
-- /Note:/ Consider using 'totalRunningHoursInLookbackPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTotalRunningHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Lude.Maybe Lude.Text)
ciTotalRunningHoursInLookbackPeriod = Lens.lens (totalRunningHoursInLookbackPeriod :: CurrentInstance -> Lude.Maybe Lude.Text) (\s a -> s {totalRunningHoursInLookbackPeriod = a} :: CurrentInstance)
{-# DEPRECATED ciTotalRunningHoursInLookbackPeriod "Use generic-lens or generic-optics with 'totalRunningHoursInLookbackPeriod' instead." #-}

-- | Number of hours during the lookback period covered by reservations.
--
-- /Note:/ Consider using 'reservationCoveredHoursInLookbackPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciReservationCoveredHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Lude.Maybe Lude.Text)
ciReservationCoveredHoursInLookbackPeriod = Lens.lens (reservationCoveredHoursInLookbackPeriod :: CurrentInstance -> Lude.Maybe Lude.Text) (\s a -> s {reservationCoveredHoursInLookbackPeriod = a} :: CurrentInstance)
{-# DEPRECATED ciReservationCoveredHoursInLookbackPeriod "Use generic-lens or generic-optics with 'reservationCoveredHoursInLookbackPeriod' instead." #-}

-- | Number of hours during the lookback period billed at On-Demand rates.
--
-- /Note:/ Consider using 'onDemandHoursInLookbackPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciOnDemandHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Lude.Maybe Lude.Text)
ciOnDemandHoursInLookbackPeriod = Lens.lens (onDemandHoursInLookbackPeriod :: CurrentInstance -> Lude.Maybe Lude.Text) (\s a -> s {onDemandHoursInLookbackPeriod = a} :: CurrentInstance)
{-# DEPRECATED ciOnDemandHoursInLookbackPeriod "Use generic-lens or generic-optics with 'onDemandHoursInLookbackPeriod' instead." #-}

-- | Current On-Demand cost of operating this instance on a monthly basis.
--
-- /Note:/ Consider using 'monthlyCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciMonthlyCost :: Lens.Lens' CurrentInstance (Lude.Maybe Lude.Text)
ciMonthlyCost = Lens.lens (monthlyCost :: CurrentInstance -> Lude.Maybe Lude.Text) (\s a -> s {monthlyCost = a} :: CurrentInstance)
{-# DEPRECATED ciMonthlyCost "Use generic-lens or generic-optics with 'monthlyCost' instead." #-}

-- | The name you've given an instance. This field will show as blank if you haven't given the instance a name.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInstanceName :: Lens.Lens' CurrentInstance (Lude.Maybe Lude.Text)
ciInstanceName = Lens.lens (instanceName :: CurrentInstance -> Lude.Maybe Lude.Text) (\s a -> s {instanceName = a} :: CurrentInstance)
{-# DEPRECATED ciInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

-- | Number of hours during the lookback period covered by Savings Plans.
--
-- /Note:/ Consider using 'savingsPlansCoveredHoursInLookbackPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSavingsPlansCoveredHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Lude.Maybe Lude.Text)
ciSavingsPlansCoveredHoursInLookbackPeriod = Lens.lens (savingsPlansCoveredHoursInLookbackPeriod :: CurrentInstance -> Lude.Maybe Lude.Text) (\s a -> s {savingsPlansCoveredHoursInLookbackPeriod = a} :: CurrentInstance)
{-# DEPRECATED ciSavingsPlansCoveredHoursInLookbackPeriod "Use generic-lens or generic-optics with 'savingsPlansCoveredHoursInLookbackPeriod' instead." #-}

-- | Cost allocation resource tags applied to the instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTags :: Lens.Lens' CurrentInstance (Lude.Maybe [TagValues])
ciTags = Lens.lens (tags :: CurrentInstance -> Lude.Maybe [TagValues]) (\s a -> s {tags = a} :: CurrentInstance)
{-# DEPRECATED ciTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON CurrentInstance where
  parseJSON =
    Lude.withObject
      "CurrentInstance"
      ( \x ->
          CurrentInstance'
            Lude.<$> (x Lude..:? "ResourceId")
            Lude.<*> (x Lude..:? "CurrencyCode")
            Lude.<*> (x Lude..:? "ResourceUtilization")
            Lude.<*> (x Lude..:? "ResourceDetails")
            Lude.<*> (x Lude..:? "TotalRunningHoursInLookbackPeriod")
            Lude.<*> (x Lude..:? "ReservationCoveredHoursInLookbackPeriod")
            Lude.<*> (x Lude..:? "OnDemandHoursInLookbackPeriod")
            Lude.<*> (x Lude..:? "MonthlyCost")
            Lude.<*> (x Lude..:? "InstanceName")
            Lude.<*> (x Lude..:? "SavingsPlansCoveredHoursInLookbackPeriod")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
