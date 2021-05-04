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
-- Module      : Network.AWS.CostExplorer.Types.TargetInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TargetInstance where

import Network.AWS.CostExplorer.Types.ResourceDetails
import Network.AWS.CostExplorer.Types.ResourceUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details on recommended instance.
--
-- /See:/ 'newTargetInstance' smart constructor.
data TargetInstance = TargetInstance'
  { -- | Estimated savings resulting from modification, on a monthly basis.
    estimatedMonthlySavings :: Prelude.Maybe Prelude.Text,
    -- | The currency code that AWS used to calculate the costs for this
    -- instance.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | Expected cost to operate this instance type on a monthly basis.
    estimatedMonthlyCost :: Prelude.Maybe Prelude.Text,
    -- | Expected utilization metrics for target instance type.
    expectedResourceUtilization :: Prelude.Maybe ResourceUtilization,
    -- | Indicates whether this recommendation is the defaulted AWS
    -- recommendation.
    defaultTargetInstance :: Prelude.Maybe Prelude.Bool,
    -- | Details on the target instance type.
    resourceDetails :: Prelude.Maybe ResourceDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TargetInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedMonthlySavings', 'targetInstance_estimatedMonthlySavings' - Estimated savings resulting from modification, on a monthly basis.
--
-- 'currencyCode', 'targetInstance_currencyCode' - The currency code that AWS used to calculate the costs for this
-- instance.
--
-- 'estimatedMonthlyCost', 'targetInstance_estimatedMonthlyCost' - Expected cost to operate this instance type on a monthly basis.
--
-- 'expectedResourceUtilization', 'targetInstance_expectedResourceUtilization' - Expected utilization metrics for target instance type.
--
-- 'defaultTargetInstance', 'targetInstance_defaultTargetInstance' - Indicates whether this recommendation is the defaulted AWS
-- recommendation.
--
-- 'resourceDetails', 'targetInstance_resourceDetails' - Details on the target instance type.
newTargetInstance ::
  TargetInstance
newTargetInstance =
  TargetInstance'
    { estimatedMonthlySavings =
        Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      estimatedMonthlyCost = Prelude.Nothing,
      expectedResourceUtilization = Prelude.Nothing,
      defaultTargetInstance = Prelude.Nothing,
      resourceDetails = Prelude.Nothing
    }

-- | Estimated savings resulting from modification, on a monthly basis.
targetInstance_estimatedMonthlySavings :: Lens.Lens' TargetInstance (Prelude.Maybe Prelude.Text)
targetInstance_estimatedMonthlySavings = Lens.lens (\TargetInstance' {estimatedMonthlySavings} -> estimatedMonthlySavings) (\s@TargetInstance' {} a -> s {estimatedMonthlySavings = a} :: TargetInstance)

-- | The currency code that AWS used to calculate the costs for this
-- instance.
targetInstance_currencyCode :: Lens.Lens' TargetInstance (Prelude.Maybe Prelude.Text)
targetInstance_currencyCode = Lens.lens (\TargetInstance' {currencyCode} -> currencyCode) (\s@TargetInstance' {} a -> s {currencyCode = a} :: TargetInstance)

-- | Expected cost to operate this instance type on a monthly basis.
targetInstance_estimatedMonthlyCost :: Lens.Lens' TargetInstance (Prelude.Maybe Prelude.Text)
targetInstance_estimatedMonthlyCost = Lens.lens (\TargetInstance' {estimatedMonthlyCost} -> estimatedMonthlyCost) (\s@TargetInstance' {} a -> s {estimatedMonthlyCost = a} :: TargetInstance)

-- | Expected utilization metrics for target instance type.
targetInstance_expectedResourceUtilization :: Lens.Lens' TargetInstance (Prelude.Maybe ResourceUtilization)
targetInstance_expectedResourceUtilization = Lens.lens (\TargetInstance' {expectedResourceUtilization} -> expectedResourceUtilization) (\s@TargetInstance' {} a -> s {expectedResourceUtilization = a} :: TargetInstance)

-- | Indicates whether this recommendation is the defaulted AWS
-- recommendation.
targetInstance_defaultTargetInstance :: Lens.Lens' TargetInstance (Prelude.Maybe Prelude.Bool)
targetInstance_defaultTargetInstance = Lens.lens (\TargetInstance' {defaultTargetInstance} -> defaultTargetInstance) (\s@TargetInstance' {} a -> s {defaultTargetInstance = a} :: TargetInstance)

-- | Details on the target instance type.
targetInstance_resourceDetails :: Lens.Lens' TargetInstance (Prelude.Maybe ResourceDetails)
targetInstance_resourceDetails = Lens.lens (\TargetInstance' {resourceDetails} -> resourceDetails) (\s@TargetInstance' {} a -> s {resourceDetails = a} :: TargetInstance)

instance Prelude.FromJSON TargetInstance where
  parseJSON =
    Prelude.withObject
      "TargetInstance"
      ( \x ->
          TargetInstance'
            Prelude.<$> (x Prelude..:? "EstimatedMonthlySavings")
            Prelude.<*> (x Prelude..:? "CurrencyCode")
            Prelude.<*> (x Prelude..:? "EstimatedMonthlyCost")
            Prelude.<*> (x Prelude..:? "ExpectedResourceUtilization")
            Prelude.<*> (x Prelude..:? "DefaultTargetInstance")
            Prelude.<*> (x Prelude..:? "ResourceDetails")
      )

instance Prelude.Hashable TargetInstance

instance Prelude.NFData TargetInstance
