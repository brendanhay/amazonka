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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details on recommended instance.
--
-- /See:/ 'newTargetInstance' smart constructor.
data TargetInstance = TargetInstance'
  { -- | The currency code that Amazon Web Services used to calculate the costs
    -- for this instance.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | Determines whether this recommendation is the defaulted Amazon Web
    -- Services recommendation.
    defaultTargetInstance :: Prelude.Maybe Prelude.Bool,
    -- | The expected cost to operate this instance type on a monthly basis.
    estimatedMonthlyCost :: Prelude.Maybe Prelude.Text,
    -- | The estimated savings that result from modification, on a monthly basis.
    estimatedMonthlySavings :: Prelude.Maybe Prelude.Text,
    -- | The expected utilization metrics for target instance type.
    expectedResourceUtilization :: Prelude.Maybe ResourceUtilization,
    -- | Explains the actions that you might need to take to successfully migrate
    -- your workloads from the current instance type to the recommended
    -- instance type.
    platformDifferences :: Prelude.Maybe [PlatformDifference],
    -- | Details on the target instance type.
    resourceDetails :: Prelude.Maybe ResourceDetails
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
-- 'currencyCode', 'targetInstance_currencyCode' - The currency code that Amazon Web Services used to calculate the costs
-- for this instance.
--
-- 'defaultTargetInstance', 'targetInstance_defaultTargetInstance' - Determines whether this recommendation is the defaulted Amazon Web
-- Services recommendation.
--
-- 'estimatedMonthlyCost', 'targetInstance_estimatedMonthlyCost' - The expected cost to operate this instance type on a monthly basis.
--
-- 'estimatedMonthlySavings', 'targetInstance_estimatedMonthlySavings' - The estimated savings that result from modification, on a monthly basis.
--
-- 'expectedResourceUtilization', 'targetInstance_expectedResourceUtilization' - The expected utilization metrics for target instance type.
--
-- 'platformDifferences', 'targetInstance_platformDifferences' - Explains the actions that you might need to take to successfully migrate
-- your workloads from the current instance type to the recommended
-- instance type.
--
-- 'resourceDetails', 'targetInstance_resourceDetails' - Details on the target instance type.
newTargetInstance ::
  TargetInstance
newTargetInstance =
  TargetInstance'
    { currencyCode = Prelude.Nothing,
      defaultTargetInstance = Prelude.Nothing,
      estimatedMonthlyCost = Prelude.Nothing,
      estimatedMonthlySavings = Prelude.Nothing,
      expectedResourceUtilization = Prelude.Nothing,
      platformDifferences = Prelude.Nothing,
      resourceDetails = Prelude.Nothing
    }

-- | The currency code that Amazon Web Services used to calculate the costs
-- for this instance.
targetInstance_currencyCode :: Lens.Lens' TargetInstance (Prelude.Maybe Prelude.Text)
targetInstance_currencyCode = Lens.lens (\TargetInstance' {currencyCode} -> currencyCode) (\s@TargetInstance' {} a -> s {currencyCode = a} :: TargetInstance)

-- | Determines whether this recommendation is the defaulted Amazon Web
-- Services recommendation.
targetInstance_defaultTargetInstance :: Lens.Lens' TargetInstance (Prelude.Maybe Prelude.Bool)
targetInstance_defaultTargetInstance = Lens.lens (\TargetInstance' {defaultTargetInstance} -> defaultTargetInstance) (\s@TargetInstance' {} a -> s {defaultTargetInstance = a} :: TargetInstance)

-- | The expected cost to operate this instance type on a monthly basis.
targetInstance_estimatedMonthlyCost :: Lens.Lens' TargetInstance (Prelude.Maybe Prelude.Text)
targetInstance_estimatedMonthlyCost = Lens.lens (\TargetInstance' {estimatedMonthlyCost} -> estimatedMonthlyCost) (\s@TargetInstance' {} a -> s {estimatedMonthlyCost = a} :: TargetInstance)

-- | The estimated savings that result from modification, on a monthly basis.
targetInstance_estimatedMonthlySavings :: Lens.Lens' TargetInstance (Prelude.Maybe Prelude.Text)
targetInstance_estimatedMonthlySavings = Lens.lens (\TargetInstance' {estimatedMonthlySavings} -> estimatedMonthlySavings) (\s@TargetInstance' {} a -> s {estimatedMonthlySavings = a} :: TargetInstance)

-- | The expected utilization metrics for target instance type.
targetInstance_expectedResourceUtilization :: Lens.Lens' TargetInstance (Prelude.Maybe ResourceUtilization)
targetInstance_expectedResourceUtilization = Lens.lens (\TargetInstance' {expectedResourceUtilization} -> expectedResourceUtilization) (\s@TargetInstance' {} a -> s {expectedResourceUtilization = a} :: TargetInstance)

-- | Explains the actions that you might need to take to successfully migrate
-- your workloads from the current instance type to the recommended
-- instance type.
targetInstance_platformDifferences :: Lens.Lens' TargetInstance (Prelude.Maybe [PlatformDifference])
targetInstance_platformDifferences = Lens.lens (\TargetInstance' {platformDifferences} -> platformDifferences) (\s@TargetInstance' {} a -> s {platformDifferences = a} :: TargetInstance) Prelude.. Lens.mapping Lens.coerced

-- | Details on the target instance type.
targetInstance_resourceDetails :: Lens.Lens' TargetInstance (Prelude.Maybe ResourceDetails)
targetInstance_resourceDetails = Lens.lens (\TargetInstance' {resourceDetails} -> resourceDetails) (\s@TargetInstance' {} a -> s {resourceDetails = a} :: TargetInstance)

instance Data.FromJSON TargetInstance where
  parseJSON =
    Data.withObject
      "TargetInstance"
      ( \x ->
          TargetInstance'
            Prelude.<$> (x Data..:? "CurrencyCode")
            Prelude.<*> (x Data..:? "DefaultTargetInstance")
            Prelude.<*> (x Data..:? "EstimatedMonthlyCost")
            Prelude.<*> (x Data..:? "EstimatedMonthlySavings")
            Prelude.<*> (x Data..:? "ExpectedResourceUtilization")
            Prelude.<*> ( x
                            Data..:? "PlatformDifferences"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ResourceDetails")
      )

instance Prelude.Hashable TargetInstance where
  hashWithSalt _salt TargetInstance' {..} =
    _salt
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` defaultTargetInstance
      `Prelude.hashWithSalt` estimatedMonthlyCost
      `Prelude.hashWithSalt` estimatedMonthlySavings
      `Prelude.hashWithSalt` expectedResourceUtilization
      `Prelude.hashWithSalt` platformDifferences
      `Prelude.hashWithSalt` resourceDetails

instance Prelude.NFData TargetInstance where
  rnf TargetInstance' {..} =
    Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf defaultTargetInstance
      `Prelude.seq` Prelude.rnf estimatedMonthlyCost
      `Prelude.seq` Prelude.rnf estimatedMonthlySavings
      `Prelude.seq` Prelude.rnf expectedResourceUtilization
      `Prelude.seq` Prelude.rnf platformDifferences
      `Prelude.seq` Prelude.rnf resourceDetails
