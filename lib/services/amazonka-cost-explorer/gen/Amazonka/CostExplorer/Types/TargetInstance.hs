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
-- Module      : Amazonka.CostExplorer.Types.TargetInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.TargetInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.PlatformDifference
import Amazonka.CostExplorer.Types.ResourceDetails
import Amazonka.CostExplorer.Types.ResourceUtilization
import qualified Amazonka.Prelude as Prelude

-- | Details on recommended instance.
--
-- /See:/ 'newTargetInstance' smart constructor.
data TargetInstance = TargetInstance'
  { -- | The expected cost to operate this instance type on a monthly basis.
    estimatedMonthlyCost :: Prelude.Maybe Prelude.Text,
    -- | Explains the actions that you might need to take to successfully migrate
    -- your workloads from the current instance type to the recommended
    -- instance type.
    platformDifferences :: Prelude.Maybe [PlatformDifference],
    -- | The expected utilization metrics for target instance type.
    expectedResourceUtilization :: Prelude.Maybe ResourceUtilization,
    -- | Determines whether this recommendation is the defaulted Amazon Web
    -- Services recommendation.
    defaultTargetInstance :: Prelude.Maybe Prelude.Bool,
    -- | The estimated savings that result from modification, on a monthly basis.
    estimatedMonthlySavings :: Prelude.Maybe Prelude.Text,
    -- | Details on the target instance type.
    resourceDetails :: Prelude.Maybe ResourceDetails,
    -- | The currency code that Amazon Web Services used to calculate the costs
    -- for this instance.
    currencyCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedMonthlyCost', 'targetInstance_estimatedMonthlyCost' - The expected cost to operate this instance type on a monthly basis.
--
-- 'platformDifferences', 'targetInstance_platformDifferences' - Explains the actions that you might need to take to successfully migrate
-- your workloads from the current instance type to the recommended
-- instance type.
--
-- 'expectedResourceUtilization', 'targetInstance_expectedResourceUtilization' - The expected utilization metrics for target instance type.
--
-- 'defaultTargetInstance', 'targetInstance_defaultTargetInstance' - Determines whether this recommendation is the defaulted Amazon Web
-- Services recommendation.
--
-- 'estimatedMonthlySavings', 'targetInstance_estimatedMonthlySavings' - The estimated savings that result from modification, on a monthly basis.
--
-- 'resourceDetails', 'targetInstance_resourceDetails' - Details on the target instance type.
--
-- 'currencyCode', 'targetInstance_currencyCode' - The currency code that Amazon Web Services used to calculate the costs
-- for this instance.
newTargetInstance ::
  TargetInstance
newTargetInstance =
  TargetInstance'
    { estimatedMonthlyCost =
        Prelude.Nothing,
      platformDifferences = Prelude.Nothing,
      expectedResourceUtilization = Prelude.Nothing,
      defaultTargetInstance = Prelude.Nothing,
      estimatedMonthlySavings = Prelude.Nothing,
      resourceDetails = Prelude.Nothing,
      currencyCode = Prelude.Nothing
    }

-- | The expected cost to operate this instance type on a monthly basis.
targetInstance_estimatedMonthlyCost :: Lens.Lens' TargetInstance (Prelude.Maybe Prelude.Text)
targetInstance_estimatedMonthlyCost = Lens.lens (\TargetInstance' {estimatedMonthlyCost} -> estimatedMonthlyCost) (\s@TargetInstance' {} a -> s {estimatedMonthlyCost = a} :: TargetInstance)

-- | Explains the actions that you might need to take to successfully migrate
-- your workloads from the current instance type to the recommended
-- instance type.
targetInstance_platformDifferences :: Lens.Lens' TargetInstance (Prelude.Maybe [PlatformDifference])
targetInstance_platformDifferences = Lens.lens (\TargetInstance' {platformDifferences} -> platformDifferences) (\s@TargetInstance' {} a -> s {platformDifferences = a} :: TargetInstance) Prelude.. Lens.mapping Lens.coerced

-- | The expected utilization metrics for target instance type.
targetInstance_expectedResourceUtilization :: Lens.Lens' TargetInstance (Prelude.Maybe ResourceUtilization)
targetInstance_expectedResourceUtilization = Lens.lens (\TargetInstance' {expectedResourceUtilization} -> expectedResourceUtilization) (\s@TargetInstance' {} a -> s {expectedResourceUtilization = a} :: TargetInstance)

-- | Determines whether this recommendation is the defaulted Amazon Web
-- Services recommendation.
targetInstance_defaultTargetInstance :: Lens.Lens' TargetInstance (Prelude.Maybe Prelude.Bool)
targetInstance_defaultTargetInstance = Lens.lens (\TargetInstance' {defaultTargetInstance} -> defaultTargetInstance) (\s@TargetInstance' {} a -> s {defaultTargetInstance = a} :: TargetInstance)

-- | The estimated savings that result from modification, on a monthly basis.
targetInstance_estimatedMonthlySavings :: Lens.Lens' TargetInstance (Prelude.Maybe Prelude.Text)
targetInstance_estimatedMonthlySavings = Lens.lens (\TargetInstance' {estimatedMonthlySavings} -> estimatedMonthlySavings) (\s@TargetInstance' {} a -> s {estimatedMonthlySavings = a} :: TargetInstance)

-- | Details on the target instance type.
targetInstance_resourceDetails :: Lens.Lens' TargetInstance (Prelude.Maybe ResourceDetails)
targetInstance_resourceDetails = Lens.lens (\TargetInstance' {resourceDetails} -> resourceDetails) (\s@TargetInstance' {} a -> s {resourceDetails = a} :: TargetInstance)

-- | The currency code that Amazon Web Services used to calculate the costs
-- for this instance.
targetInstance_currencyCode :: Lens.Lens' TargetInstance (Prelude.Maybe Prelude.Text)
targetInstance_currencyCode = Lens.lens (\TargetInstance' {currencyCode} -> currencyCode) (\s@TargetInstance' {} a -> s {currencyCode = a} :: TargetInstance)

instance Core.FromJSON TargetInstance where
  parseJSON =
    Core.withObject
      "TargetInstance"
      ( \x ->
          TargetInstance'
            Prelude.<$> (x Core..:? "EstimatedMonthlyCost")
            Prelude.<*> ( x Core..:? "PlatformDifferences"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ExpectedResourceUtilization")
            Prelude.<*> (x Core..:? "DefaultTargetInstance")
            Prelude.<*> (x Core..:? "EstimatedMonthlySavings")
            Prelude.<*> (x Core..:? "ResourceDetails")
            Prelude.<*> (x Core..:? "CurrencyCode")
      )

instance Prelude.Hashable TargetInstance where
  hashWithSalt _salt TargetInstance' {..} =
    _salt `Prelude.hashWithSalt` estimatedMonthlyCost
      `Prelude.hashWithSalt` platformDifferences
      `Prelude.hashWithSalt` expectedResourceUtilization
      `Prelude.hashWithSalt` defaultTargetInstance
      `Prelude.hashWithSalt` estimatedMonthlySavings
      `Prelude.hashWithSalt` resourceDetails
      `Prelude.hashWithSalt` currencyCode

instance Prelude.NFData TargetInstance where
  rnf TargetInstance' {..} =
    Prelude.rnf estimatedMonthlyCost
      `Prelude.seq` Prelude.rnf platformDifferences
      `Prelude.seq` Prelude.rnf expectedResourceUtilization
      `Prelude.seq` Prelude.rnf defaultTargetInstance
      `Prelude.seq` Prelude.rnf estimatedMonthlySavings
      `Prelude.seq` Prelude.rnf resourceDetails
      `Prelude.seq` Prelude.rnf currencyCode
