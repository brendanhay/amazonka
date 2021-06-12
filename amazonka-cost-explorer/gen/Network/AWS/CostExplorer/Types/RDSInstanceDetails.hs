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
-- Module      : Network.AWS.CostExplorer.Types.RDSInstanceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RDSInstanceDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details about the Amazon RDS instances that AWS recommends that you
-- purchase.
--
-- /See:/ 'newRDSInstanceDetails' smart constructor.
data RDSInstanceDetails = RDSInstanceDetails'
  { -- | The type of instance that AWS recommends.
    instanceType :: Core.Maybe Core.Text,
    -- | The database edition that the recommended reservation supports.
    databaseEdition :: Core.Maybe Core.Text,
    -- | Whether the recommendation is for a reservation in a single Availability
    -- Zone or a reservation with a backup in a second Availability Zone.
    deploymentOption :: Core.Maybe Core.Text,
    -- | Whether the recommendation is for a current-generation instance.
    currentGeneration :: Core.Maybe Core.Bool,
    -- | Whether the recommended reservation is size flexible.
    sizeFlexEligible :: Core.Maybe Core.Bool,
    -- | The license model that the recommended reservation supports.
    licenseModel :: Core.Maybe Core.Text,
    -- | The instance family of the recommended reservation.
    family :: Core.Maybe Core.Text,
    -- | The database engine that the recommended reservation supports.
    databaseEngine :: Core.Maybe Core.Text,
    -- | The AWS Region of the recommended reservation.
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RDSInstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'rDSInstanceDetails_instanceType' - The type of instance that AWS recommends.
--
-- 'databaseEdition', 'rDSInstanceDetails_databaseEdition' - The database edition that the recommended reservation supports.
--
-- 'deploymentOption', 'rDSInstanceDetails_deploymentOption' - Whether the recommendation is for a reservation in a single Availability
-- Zone or a reservation with a backup in a second Availability Zone.
--
-- 'currentGeneration', 'rDSInstanceDetails_currentGeneration' - Whether the recommendation is for a current-generation instance.
--
-- 'sizeFlexEligible', 'rDSInstanceDetails_sizeFlexEligible' - Whether the recommended reservation is size flexible.
--
-- 'licenseModel', 'rDSInstanceDetails_licenseModel' - The license model that the recommended reservation supports.
--
-- 'family', 'rDSInstanceDetails_family' - The instance family of the recommended reservation.
--
-- 'databaseEngine', 'rDSInstanceDetails_databaseEngine' - The database engine that the recommended reservation supports.
--
-- 'region', 'rDSInstanceDetails_region' - The AWS Region of the recommended reservation.
newRDSInstanceDetails ::
  RDSInstanceDetails
newRDSInstanceDetails =
  RDSInstanceDetails'
    { instanceType = Core.Nothing,
      databaseEdition = Core.Nothing,
      deploymentOption = Core.Nothing,
      currentGeneration = Core.Nothing,
      sizeFlexEligible = Core.Nothing,
      licenseModel = Core.Nothing,
      family = Core.Nothing,
      databaseEngine = Core.Nothing,
      region = Core.Nothing
    }

-- | The type of instance that AWS recommends.
rDSInstanceDetails_instanceType :: Lens.Lens' RDSInstanceDetails (Core.Maybe Core.Text)
rDSInstanceDetails_instanceType = Lens.lens (\RDSInstanceDetails' {instanceType} -> instanceType) (\s@RDSInstanceDetails' {} a -> s {instanceType = a} :: RDSInstanceDetails)

-- | The database edition that the recommended reservation supports.
rDSInstanceDetails_databaseEdition :: Lens.Lens' RDSInstanceDetails (Core.Maybe Core.Text)
rDSInstanceDetails_databaseEdition = Lens.lens (\RDSInstanceDetails' {databaseEdition} -> databaseEdition) (\s@RDSInstanceDetails' {} a -> s {databaseEdition = a} :: RDSInstanceDetails)

-- | Whether the recommendation is for a reservation in a single Availability
-- Zone or a reservation with a backup in a second Availability Zone.
rDSInstanceDetails_deploymentOption :: Lens.Lens' RDSInstanceDetails (Core.Maybe Core.Text)
rDSInstanceDetails_deploymentOption = Lens.lens (\RDSInstanceDetails' {deploymentOption} -> deploymentOption) (\s@RDSInstanceDetails' {} a -> s {deploymentOption = a} :: RDSInstanceDetails)

-- | Whether the recommendation is for a current-generation instance.
rDSInstanceDetails_currentGeneration :: Lens.Lens' RDSInstanceDetails (Core.Maybe Core.Bool)
rDSInstanceDetails_currentGeneration = Lens.lens (\RDSInstanceDetails' {currentGeneration} -> currentGeneration) (\s@RDSInstanceDetails' {} a -> s {currentGeneration = a} :: RDSInstanceDetails)

-- | Whether the recommended reservation is size flexible.
rDSInstanceDetails_sizeFlexEligible :: Lens.Lens' RDSInstanceDetails (Core.Maybe Core.Bool)
rDSInstanceDetails_sizeFlexEligible = Lens.lens (\RDSInstanceDetails' {sizeFlexEligible} -> sizeFlexEligible) (\s@RDSInstanceDetails' {} a -> s {sizeFlexEligible = a} :: RDSInstanceDetails)

-- | The license model that the recommended reservation supports.
rDSInstanceDetails_licenseModel :: Lens.Lens' RDSInstanceDetails (Core.Maybe Core.Text)
rDSInstanceDetails_licenseModel = Lens.lens (\RDSInstanceDetails' {licenseModel} -> licenseModel) (\s@RDSInstanceDetails' {} a -> s {licenseModel = a} :: RDSInstanceDetails)

-- | The instance family of the recommended reservation.
rDSInstanceDetails_family :: Lens.Lens' RDSInstanceDetails (Core.Maybe Core.Text)
rDSInstanceDetails_family = Lens.lens (\RDSInstanceDetails' {family} -> family) (\s@RDSInstanceDetails' {} a -> s {family = a} :: RDSInstanceDetails)

-- | The database engine that the recommended reservation supports.
rDSInstanceDetails_databaseEngine :: Lens.Lens' RDSInstanceDetails (Core.Maybe Core.Text)
rDSInstanceDetails_databaseEngine = Lens.lens (\RDSInstanceDetails' {databaseEngine} -> databaseEngine) (\s@RDSInstanceDetails' {} a -> s {databaseEngine = a} :: RDSInstanceDetails)

-- | The AWS Region of the recommended reservation.
rDSInstanceDetails_region :: Lens.Lens' RDSInstanceDetails (Core.Maybe Core.Text)
rDSInstanceDetails_region = Lens.lens (\RDSInstanceDetails' {region} -> region) (\s@RDSInstanceDetails' {} a -> s {region = a} :: RDSInstanceDetails)

instance Core.FromJSON RDSInstanceDetails where
  parseJSON =
    Core.withObject
      "RDSInstanceDetails"
      ( \x ->
          RDSInstanceDetails'
            Core.<$> (x Core..:? "InstanceType")
            Core.<*> (x Core..:? "DatabaseEdition")
            Core.<*> (x Core..:? "DeploymentOption")
            Core.<*> (x Core..:? "CurrentGeneration")
            Core.<*> (x Core..:? "SizeFlexEligible")
            Core.<*> (x Core..:? "LicenseModel")
            Core.<*> (x Core..:? "Family")
            Core.<*> (x Core..:? "DatabaseEngine")
            Core.<*> (x Core..:? "Region")
      )

instance Core.Hashable RDSInstanceDetails

instance Core.NFData RDSInstanceDetails
