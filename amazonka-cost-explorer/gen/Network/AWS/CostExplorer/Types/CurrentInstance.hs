{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.CostExplorer.Types.ResourceDetails
import Network.AWS.CostExplorer.Types.ResourceUtilization
import Network.AWS.CostExplorer.Types.TagValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Context about the current instance.
--
-- /See:/ 'newCurrentInstance' smart constructor.
data CurrentInstance = CurrentInstance'
  { -- | Resource ID of the current instance.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The name you\'ve given an instance. This field will show as blank if you
    -- haven\'t given the instance a name.
    instanceName :: Prelude.Maybe Prelude.Text,
    -- | Number of hours during the lookback period covered by Savings Plans.
    savingsPlansCoveredHoursInLookbackPeriod :: Prelude.Maybe Prelude.Text,
    -- | Number of hours during the lookback period billed at On-Demand rates.
    onDemandHoursInLookbackPeriod :: Prelude.Maybe Prelude.Text,
    -- | The currency code that AWS used to calculate the costs for this
    -- instance.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | Cost allocation resource tags applied to the instance.
    tags :: Prelude.Maybe [TagValues],
    -- | Number of hours during the lookback period covered by reservations.
    reservationCoveredHoursInLookbackPeriod :: Prelude.Maybe Prelude.Text,
    -- | Current On-Demand cost of operating this instance on a monthly basis.
    monthlyCost :: Prelude.Maybe Prelude.Text,
    -- | Utilization information of the current instance during the lookback
    -- period.
    resourceUtilization :: Prelude.Maybe ResourceUtilization,
    -- | Details about the resource and utilization.
    resourceDetails :: Prelude.Maybe ResourceDetails,
    -- | The total number of hours the instance ran during the lookback period.
    totalRunningHoursInLookbackPeriod :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { resourceId = Prelude.Nothing,
      instanceName = Prelude.Nothing,
      savingsPlansCoveredHoursInLookbackPeriod =
        Prelude.Nothing,
      onDemandHoursInLookbackPeriod = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      tags = Prelude.Nothing,
      reservationCoveredHoursInLookbackPeriod =
        Prelude.Nothing,
      monthlyCost = Prelude.Nothing,
      resourceUtilization = Prelude.Nothing,
      resourceDetails = Prelude.Nothing,
      totalRunningHoursInLookbackPeriod = Prelude.Nothing
    }

-- | Resource ID of the current instance.
currentInstance_resourceId :: Lens.Lens' CurrentInstance (Prelude.Maybe Prelude.Text)
currentInstance_resourceId = Lens.lens (\CurrentInstance' {resourceId} -> resourceId) (\s@CurrentInstance' {} a -> s {resourceId = a} :: CurrentInstance)

-- | The name you\'ve given an instance. This field will show as blank if you
-- haven\'t given the instance a name.
currentInstance_instanceName :: Lens.Lens' CurrentInstance (Prelude.Maybe Prelude.Text)
currentInstance_instanceName = Lens.lens (\CurrentInstance' {instanceName} -> instanceName) (\s@CurrentInstance' {} a -> s {instanceName = a} :: CurrentInstance)

-- | Number of hours during the lookback period covered by Savings Plans.
currentInstance_savingsPlansCoveredHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Prelude.Maybe Prelude.Text)
currentInstance_savingsPlansCoveredHoursInLookbackPeriod = Lens.lens (\CurrentInstance' {savingsPlansCoveredHoursInLookbackPeriod} -> savingsPlansCoveredHoursInLookbackPeriod) (\s@CurrentInstance' {} a -> s {savingsPlansCoveredHoursInLookbackPeriod = a} :: CurrentInstance)

-- | Number of hours during the lookback period billed at On-Demand rates.
currentInstance_onDemandHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Prelude.Maybe Prelude.Text)
currentInstance_onDemandHoursInLookbackPeriod = Lens.lens (\CurrentInstance' {onDemandHoursInLookbackPeriod} -> onDemandHoursInLookbackPeriod) (\s@CurrentInstance' {} a -> s {onDemandHoursInLookbackPeriod = a} :: CurrentInstance)

-- | The currency code that AWS used to calculate the costs for this
-- instance.
currentInstance_currencyCode :: Lens.Lens' CurrentInstance (Prelude.Maybe Prelude.Text)
currentInstance_currencyCode = Lens.lens (\CurrentInstance' {currencyCode} -> currencyCode) (\s@CurrentInstance' {} a -> s {currencyCode = a} :: CurrentInstance)

-- | Cost allocation resource tags applied to the instance.
currentInstance_tags :: Lens.Lens' CurrentInstance (Prelude.Maybe [TagValues])
currentInstance_tags = Lens.lens (\CurrentInstance' {tags} -> tags) (\s@CurrentInstance' {} a -> s {tags = a} :: CurrentInstance) Prelude.. Lens.mapping Prelude._Coerce

-- | Number of hours during the lookback period covered by reservations.
currentInstance_reservationCoveredHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Prelude.Maybe Prelude.Text)
currentInstance_reservationCoveredHoursInLookbackPeriod = Lens.lens (\CurrentInstance' {reservationCoveredHoursInLookbackPeriod} -> reservationCoveredHoursInLookbackPeriod) (\s@CurrentInstance' {} a -> s {reservationCoveredHoursInLookbackPeriod = a} :: CurrentInstance)

-- | Current On-Demand cost of operating this instance on a monthly basis.
currentInstance_monthlyCost :: Lens.Lens' CurrentInstance (Prelude.Maybe Prelude.Text)
currentInstance_monthlyCost = Lens.lens (\CurrentInstance' {monthlyCost} -> monthlyCost) (\s@CurrentInstance' {} a -> s {monthlyCost = a} :: CurrentInstance)

-- | Utilization information of the current instance during the lookback
-- period.
currentInstance_resourceUtilization :: Lens.Lens' CurrentInstance (Prelude.Maybe ResourceUtilization)
currentInstance_resourceUtilization = Lens.lens (\CurrentInstance' {resourceUtilization} -> resourceUtilization) (\s@CurrentInstance' {} a -> s {resourceUtilization = a} :: CurrentInstance)

-- | Details about the resource and utilization.
currentInstance_resourceDetails :: Lens.Lens' CurrentInstance (Prelude.Maybe ResourceDetails)
currentInstance_resourceDetails = Lens.lens (\CurrentInstance' {resourceDetails} -> resourceDetails) (\s@CurrentInstance' {} a -> s {resourceDetails = a} :: CurrentInstance)

-- | The total number of hours the instance ran during the lookback period.
currentInstance_totalRunningHoursInLookbackPeriod :: Lens.Lens' CurrentInstance (Prelude.Maybe Prelude.Text)
currentInstance_totalRunningHoursInLookbackPeriod = Lens.lens (\CurrentInstance' {totalRunningHoursInLookbackPeriod} -> totalRunningHoursInLookbackPeriod) (\s@CurrentInstance' {} a -> s {totalRunningHoursInLookbackPeriod = a} :: CurrentInstance)

instance Prelude.FromJSON CurrentInstance where
  parseJSON =
    Prelude.withObject
      "CurrentInstance"
      ( \x ->
          CurrentInstance'
            Prelude.<$> (x Prelude..:? "ResourceId")
            Prelude.<*> (x Prelude..:? "InstanceName")
            Prelude.<*> ( x
                            Prelude..:? "SavingsPlansCoveredHoursInLookbackPeriod"
                        )
            Prelude.<*> (x Prelude..:? "OnDemandHoursInLookbackPeriod")
            Prelude.<*> (x Prelude..:? "CurrencyCode")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x
                            Prelude..:? "ReservationCoveredHoursInLookbackPeriod"
                        )
            Prelude.<*> (x Prelude..:? "MonthlyCost")
            Prelude.<*> (x Prelude..:? "ResourceUtilization")
            Prelude.<*> (x Prelude..:? "ResourceDetails")
            Prelude.<*> (x Prelude..:? "TotalRunningHoursInLookbackPeriod")
      )

instance Prelude.Hashable CurrentInstance

instance Prelude.NFData CurrentInstance
