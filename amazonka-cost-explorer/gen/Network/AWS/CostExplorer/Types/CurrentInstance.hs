{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CurrentInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CurrentInstance where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.ResourceDetails
import Network.AWS.CostExplorer.Types.ResourceUtilization
import Network.AWS.CostExplorer.Types.TagValues
import qualified Network.AWS.Lens as Lens

-- | Context about the current instance.
--
-- /See:/ 'newCurrentInstance' smart constructor.
data CurrentInstance = CurrentInstance'
  { -- | Resource ID of the current instance.
    resourceId :: Core.Maybe Core.Text,
    -- | The name you\'ve given an instance. This field will show as blank if you
    -- haven\'t given the instance a name.
    instanceName :: Core.Maybe Core.Text,
    -- | Number of hours during the lookback period covered by Savings Plans.
    savingsPlansCoveredHoursInLookbackPeriod :: Core.Maybe Core.Text,
    -- | Number of hours during the lookback period billed at On-Demand rates.
    onDemandHoursInLookbackPeriod :: Core.Maybe Core.Text,
    -- | The currency code that AWS used to calculate the costs for this
    -- instance.
    currencyCode :: Core.Maybe Core.Text,
    -- | Cost allocation resource tags applied to the instance.
    tags :: Core.Maybe [TagValues],
    -- | Number of hours during the lookback period covered by reservations.
    reservationCoveredHoursInLookbackPeriod :: Core.Maybe Core.Text,
    -- | Current On-Demand cost of operating this instance on a monthly basis.
    monthlyCost :: Core.Maybe Core.Text,
    -- | Utilization information of the current instance during the lookback
    -- period.
    resourceUtilization :: Core.Maybe ResourceUtilization,
    -- | Details about the resource and utilization.
    resourceDetails :: Core.Maybe ResourceDetails,
    -- | The total number of hours the instance ran during the lookback period.
    totalRunningHoursInLookbackPeriod :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CurrentInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'currentInstance_resourceId' - Resource ID of the current instance.
--
-- 'instanceName', 'currentInstance_instanceName' - The name you\'ve given an instance. This field will show as blank if you
-- haven\'t given the instance a name.
--
-- 'savingsPlansCoveredHoursInLookbackPeriod', 'currentInstance_savingsPlansCoveredHoursInLookbackPeriod' - Number of hours during the lookback period covered by Savings Plans.
--
-- 'onDemandHoursInLookbackPeriod', 'currentInstance_onDemandHoursInLookbackPeriod' - Number of hours during the lookback period billed at On-Demand rates.
--
-- 'currencyCode', 'currentInstance_currencyCode' - The currency code that AWS used to calculate the costs for this
-- instance.
--
-- 'tags', 'currentInstance_tags' - Cost allocation resource tags applied to the instance.
--
-- 'reservationCoveredHoursInLookbackPeriod', 'currentInstance_reservationCoveredHoursInLookbackPeriod' - Number of hours during the lookback period covered by reservations.
--
-- 'monthlyCost', 'currentInstance_monthlyCost' - Current On-Demand cost of operating this instance on a monthly basis.
--
-- 'resourceUtilization', 'currentInstance_resourceUtilization' - Utilization information of the current instance during the lookback
-- period.
--
-- 'resourceDetails', 'currentInstance_resourceDetails' - Details about the resource and utilization.
--
-- 'totalRunningHoursInLookbackPeriod', 'currentInstance_totalRunningHoursInLookbackPeriod' - The total number of hours the instance ran during the lookback period.
newCurrentInstance ::
  CurrentInstance
newCurrentInstance =
  CurrentInstance'
    { resourceId = Core.Nothing,
      instanceName = Core.Nothing,
      savingsPlansCoveredHoursInLookbackPeriod =
        Core.Nothing,
      onDemandHoursInLookbackPeriod = Core.Nothing,
      currencyCode = Core.Nothing,
      tags = Core.Nothing,
      reservationCoveredHoursInLookbackPeriod =
        Core.Nothing,
      monthlyCost = Core.Nothing,
      resourceUtilization = Core.Nothing,
      resourceDetails = Core.Nothing,
      totalRunningHoursInLookbackPeriod = Core.Nothing
    }

-- | Resource ID of the current instance.
currentInstance_resourceId :: Lens.Lens' CurrentInstance (Core.Maybe Core.Text)
currentInstance_resourceId = Lens.lens (\CurrentInstance' {resourceId} -> resourceId) (\s@CurrentInstance' {} a -> s {resourceId = a} :: CurrentInstance)

-- | The name you\'ve given an instance. This field will show as blank if you
-- haven\'t given the instance a name.
currentInstance_instanceName :: Lens.Lens' CurrentInstance (Core.Maybe Core.Text)
currentInstance_instanceName = Lens.lens (\CurrentInstance' {instanceName} -> instanceName) (\s@CurrentInstance' {} a -> s {instanceName = a} :: CurrentInstance)

-- | Number of hours during the lookback period covered by Savings Plans.
currentInstance_savingsPlansCoveredHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Core.Maybe Core.Text)
currentInstance_savingsPlansCoveredHoursInLookbackPeriod = Lens.lens (\CurrentInstance' {savingsPlansCoveredHoursInLookbackPeriod} -> savingsPlansCoveredHoursInLookbackPeriod) (\s@CurrentInstance' {} a -> s {savingsPlansCoveredHoursInLookbackPeriod = a} :: CurrentInstance)

-- | Number of hours during the lookback period billed at On-Demand rates.
currentInstance_onDemandHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Core.Maybe Core.Text)
currentInstance_onDemandHoursInLookbackPeriod = Lens.lens (\CurrentInstance' {onDemandHoursInLookbackPeriod} -> onDemandHoursInLookbackPeriod) (\s@CurrentInstance' {} a -> s {onDemandHoursInLookbackPeriod = a} :: CurrentInstance)

-- | The currency code that AWS used to calculate the costs for this
-- instance.
currentInstance_currencyCode :: Lens.Lens' CurrentInstance (Core.Maybe Core.Text)
currentInstance_currencyCode = Lens.lens (\CurrentInstance' {currencyCode} -> currencyCode) (\s@CurrentInstance' {} a -> s {currencyCode = a} :: CurrentInstance)

-- | Cost allocation resource tags applied to the instance.
currentInstance_tags :: Lens.Lens' CurrentInstance (Core.Maybe [TagValues])
currentInstance_tags = Lens.lens (\CurrentInstance' {tags} -> tags) (\s@CurrentInstance' {} a -> s {tags = a} :: CurrentInstance) Core.. Lens.mapping Lens._Coerce

-- | Number of hours during the lookback period covered by reservations.
currentInstance_reservationCoveredHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Core.Maybe Core.Text)
currentInstance_reservationCoveredHoursInLookbackPeriod = Lens.lens (\CurrentInstance' {reservationCoveredHoursInLookbackPeriod} -> reservationCoveredHoursInLookbackPeriod) (\s@CurrentInstance' {} a -> s {reservationCoveredHoursInLookbackPeriod = a} :: CurrentInstance)

-- | Current On-Demand cost of operating this instance on a monthly basis.
currentInstance_monthlyCost :: Lens.Lens' CurrentInstance (Core.Maybe Core.Text)
currentInstance_monthlyCost = Lens.lens (\CurrentInstance' {monthlyCost} -> monthlyCost) (\s@CurrentInstance' {} a -> s {monthlyCost = a} :: CurrentInstance)

-- | Utilization information of the current instance during the lookback
-- period.
currentInstance_resourceUtilization :: Lens.Lens' CurrentInstance (Core.Maybe ResourceUtilization)
currentInstance_resourceUtilization = Lens.lens (\CurrentInstance' {resourceUtilization} -> resourceUtilization) (\s@CurrentInstance' {} a -> s {resourceUtilization = a} :: CurrentInstance)

-- | Details about the resource and utilization.
currentInstance_resourceDetails :: Lens.Lens' CurrentInstance (Core.Maybe ResourceDetails)
currentInstance_resourceDetails = Lens.lens (\CurrentInstance' {resourceDetails} -> resourceDetails) (\s@CurrentInstance' {} a -> s {resourceDetails = a} :: CurrentInstance)

-- | The total number of hours the instance ran during the lookback period.
currentInstance_totalRunningHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Core.Maybe Core.Text)
currentInstance_totalRunningHoursInLookbackPeriod = Lens.lens (\CurrentInstance' {totalRunningHoursInLookbackPeriod} -> totalRunningHoursInLookbackPeriod) (\s@CurrentInstance' {} a -> s {totalRunningHoursInLookbackPeriod = a} :: CurrentInstance)

instance Core.FromJSON CurrentInstance where
  parseJSON =
    Core.withObject
      "CurrentInstance"
      ( \x ->
          CurrentInstance'
            Core.<$> (x Core..:? "ResourceId")
            Core.<*> (x Core..:? "InstanceName")
            Core.<*> ( x
                         Core..:? "SavingsPlansCoveredHoursInLookbackPeriod"
                     )
            Core.<*> (x Core..:? "OnDemandHoursInLookbackPeriod")
            Core.<*> (x Core..:? "CurrencyCode")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> ( x
                         Core..:? "ReservationCoveredHoursInLookbackPeriod"
                     )
            Core.<*> (x Core..:? "MonthlyCost")
            Core.<*> (x Core..:? "ResourceUtilization")
            Core.<*> (x Core..:? "ResourceDetails")
            Core.<*> (x Core..:? "TotalRunningHoursInLookbackPeriod")
      )

instance Core.Hashable CurrentInstance

instance Core.NFData CurrentInstance
