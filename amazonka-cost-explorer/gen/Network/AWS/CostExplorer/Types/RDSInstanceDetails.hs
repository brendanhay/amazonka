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
-- Module      : Network.AWS.CostExplorer.Types.RDSInstanceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RDSInstanceDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about the Amazon RDS instances that AWS recommends that you
-- purchase.
--
-- /See:/ 'newRDSInstanceDetails' smart constructor.
data RDSInstanceDetails = RDSInstanceDetails'
  { -- | The type of instance that AWS recommends.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The database edition that the recommended reservation supports.
    databaseEdition :: Prelude.Maybe Prelude.Text,
    -- | Whether the recommendation is for a reservation in a single Availability
    -- Zone or a reservation with a backup in a second Availability Zone.
    deploymentOption :: Prelude.Maybe Prelude.Text,
    -- | Whether the recommendation is for a current-generation instance.
    currentGeneration :: Prelude.Maybe Prelude.Bool,
    -- | Whether the recommended reservation is size flexible.
    sizeFlexEligible :: Prelude.Maybe Prelude.Bool,
    -- | The license model that the recommended reservation supports.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | The instance family of the recommended reservation.
    family :: Prelude.Maybe Prelude.Text,
    -- | The database engine that the recommended reservation supports.
    databaseEngine :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region of the recommended reservation.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { instanceType = Prelude.Nothing,
      databaseEdition = Prelude.Nothing,
      deploymentOption = Prelude.Nothing,
      currentGeneration = Prelude.Nothing,
      sizeFlexEligible = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      family = Prelude.Nothing,
      databaseEngine = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The type of instance that AWS recommends.
rDSInstanceDetails_instanceType :: Lens.Lens' RDSInstanceDetails (Prelude.Maybe Prelude.Text)
rDSInstanceDetails_instanceType = Lens.lens (\RDSInstanceDetails' {instanceType} -> instanceType) (\s@RDSInstanceDetails' {} a -> s {instanceType = a} :: RDSInstanceDetails)

-- | The database edition that the recommended reservation supports.
rDSInstanceDetails_databaseEdition :: Lens.Lens' RDSInstanceDetails (Prelude.Maybe Prelude.Text)
rDSInstanceDetails_databaseEdition = Lens.lens (\RDSInstanceDetails' {databaseEdition} -> databaseEdition) (\s@RDSInstanceDetails' {} a -> s {databaseEdition = a} :: RDSInstanceDetails)

-- | Whether the recommendation is for a reservation in a single Availability
-- Zone or a reservation with a backup in a second Availability Zone.
rDSInstanceDetails_deploymentOption :: Lens.Lens' RDSInstanceDetails (Prelude.Maybe Prelude.Text)
rDSInstanceDetails_deploymentOption = Lens.lens (\RDSInstanceDetails' {deploymentOption} -> deploymentOption) (\s@RDSInstanceDetails' {} a -> s {deploymentOption = a} :: RDSInstanceDetails)

-- | Whether the recommendation is for a current-generation instance.
rDSInstanceDetails_currentGeneration :: Lens.Lens' RDSInstanceDetails (Prelude.Maybe Prelude.Bool)
rDSInstanceDetails_currentGeneration = Lens.lens (\RDSInstanceDetails' {currentGeneration} -> currentGeneration) (\s@RDSInstanceDetails' {} a -> s {currentGeneration = a} :: RDSInstanceDetails)

-- | Whether the recommended reservation is size flexible.
rDSInstanceDetails_sizeFlexEligible :: Lens.Lens' RDSInstanceDetails (Prelude.Maybe Prelude.Bool)
rDSInstanceDetails_sizeFlexEligible = Lens.lens (\RDSInstanceDetails' {sizeFlexEligible} -> sizeFlexEligible) (\s@RDSInstanceDetails' {} a -> s {sizeFlexEligible = a} :: RDSInstanceDetails)

-- | The license model that the recommended reservation supports.
rDSInstanceDetails_licenseModel :: Lens.Lens' RDSInstanceDetails (Prelude.Maybe Prelude.Text)
rDSInstanceDetails_licenseModel = Lens.lens (\RDSInstanceDetails' {licenseModel} -> licenseModel) (\s@RDSInstanceDetails' {} a -> s {licenseModel = a} :: RDSInstanceDetails)

-- | The instance family of the recommended reservation.
rDSInstanceDetails_family :: Lens.Lens' RDSInstanceDetails (Prelude.Maybe Prelude.Text)
rDSInstanceDetails_family = Lens.lens (\RDSInstanceDetails' {family} -> family) (\s@RDSInstanceDetails' {} a -> s {family = a} :: RDSInstanceDetails)

-- | The database engine that the recommended reservation supports.
rDSInstanceDetails_databaseEngine :: Lens.Lens' RDSInstanceDetails (Prelude.Maybe Prelude.Text)
rDSInstanceDetails_databaseEngine = Lens.lens (\RDSInstanceDetails' {databaseEngine} -> databaseEngine) (\s@RDSInstanceDetails' {} a -> s {databaseEngine = a} :: RDSInstanceDetails)

-- | The AWS Region of the recommended reservation.
rDSInstanceDetails_region :: Lens.Lens' RDSInstanceDetails (Prelude.Maybe Prelude.Text)
rDSInstanceDetails_region = Lens.lens (\RDSInstanceDetails' {region} -> region) (\s@RDSInstanceDetails' {} a -> s {region = a} :: RDSInstanceDetails)

instance Prelude.FromJSON RDSInstanceDetails where
  parseJSON =
    Prelude.withObject
      "RDSInstanceDetails"
      ( \x ->
          RDSInstanceDetails'
            Prelude.<$> (x Prelude..:? "InstanceType")
            Prelude.<*> (x Prelude..:? "DatabaseEdition")
            Prelude.<*> (x Prelude..:? "DeploymentOption")
            Prelude.<*> (x Prelude..:? "CurrentGeneration")
            Prelude.<*> (x Prelude..:? "SizeFlexEligible")
            Prelude.<*> (x Prelude..:? "LicenseModel")
            Prelude.<*> (x Prelude..:? "Family")
            Prelude.<*> (x Prelude..:? "DatabaseEngine")
            Prelude.<*> (x Prelude..:? "Region")
      )

instance Prelude.Hashable RDSInstanceDetails

instance Prelude.NFData RDSInstanceDetails
