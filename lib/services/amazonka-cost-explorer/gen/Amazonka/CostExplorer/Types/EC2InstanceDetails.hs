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
-- Module      : Amazonka.CostExplorer.Types.EC2InstanceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.EC2InstanceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the Amazon EC2 instances that Amazon Web Services
-- recommends that you purchase.
--
-- /See:/ 'newEC2InstanceDetails' smart constructor.
data EC2InstanceDetails = EC2InstanceDetails'
  { -- | The Availability Zone of the recommended reservation.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Determines whether the recommendation is for a current-generation
    -- instance.
    currentGeneration :: Prelude.Maybe Prelude.Bool,
    -- | The instance family of the recommended reservation.
    family :: Prelude.Maybe Prelude.Text,
    -- | The type of instance that Amazon Web Services recommends.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The platform of the recommended reservation. The platform is the
    -- specific combination of operating system, license model, and software on
    -- an instance.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region of the recommended reservation.
    region :: Prelude.Maybe Prelude.Text,
    -- | Determines whether the recommended reservation is size flexible.
    sizeFlexEligible :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether the recommended reservation is dedicated or shared.
    tenancy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2InstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'eC2InstanceDetails_availabilityZone' - The Availability Zone of the recommended reservation.
--
-- 'currentGeneration', 'eC2InstanceDetails_currentGeneration' - Determines whether the recommendation is for a current-generation
-- instance.
--
-- 'family', 'eC2InstanceDetails_family' - The instance family of the recommended reservation.
--
-- 'instanceType', 'eC2InstanceDetails_instanceType' - The type of instance that Amazon Web Services recommends.
--
-- 'platform', 'eC2InstanceDetails_platform' - The platform of the recommended reservation. The platform is the
-- specific combination of operating system, license model, and software on
-- an instance.
--
-- 'region', 'eC2InstanceDetails_region' - The Amazon Web Services Region of the recommended reservation.
--
-- 'sizeFlexEligible', 'eC2InstanceDetails_sizeFlexEligible' - Determines whether the recommended reservation is size flexible.
--
-- 'tenancy', 'eC2InstanceDetails_tenancy' - Determines whether the recommended reservation is dedicated or shared.
newEC2InstanceDetails ::
  EC2InstanceDetails
newEC2InstanceDetails =
  EC2InstanceDetails'
    { availabilityZone =
        Prelude.Nothing,
      currentGeneration = Prelude.Nothing,
      family = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      platform = Prelude.Nothing,
      region = Prelude.Nothing,
      sizeFlexEligible = Prelude.Nothing,
      tenancy = Prelude.Nothing
    }

-- | The Availability Zone of the recommended reservation.
eC2InstanceDetails_availabilityZone :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Text)
eC2InstanceDetails_availabilityZone = Lens.lens (\EC2InstanceDetails' {availabilityZone} -> availabilityZone) (\s@EC2InstanceDetails' {} a -> s {availabilityZone = a} :: EC2InstanceDetails)

-- | Determines whether the recommendation is for a current-generation
-- instance.
eC2InstanceDetails_currentGeneration :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Bool)
eC2InstanceDetails_currentGeneration = Lens.lens (\EC2InstanceDetails' {currentGeneration} -> currentGeneration) (\s@EC2InstanceDetails' {} a -> s {currentGeneration = a} :: EC2InstanceDetails)

-- | The instance family of the recommended reservation.
eC2InstanceDetails_family :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Text)
eC2InstanceDetails_family = Lens.lens (\EC2InstanceDetails' {family} -> family) (\s@EC2InstanceDetails' {} a -> s {family = a} :: EC2InstanceDetails)

-- | The type of instance that Amazon Web Services recommends.
eC2InstanceDetails_instanceType :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Text)
eC2InstanceDetails_instanceType = Lens.lens (\EC2InstanceDetails' {instanceType} -> instanceType) (\s@EC2InstanceDetails' {} a -> s {instanceType = a} :: EC2InstanceDetails)

-- | The platform of the recommended reservation. The platform is the
-- specific combination of operating system, license model, and software on
-- an instance.
eC2InstanceDetails_platform :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Text)
eC2InstanceDetails_platform = Lens.lens (\EC2InstanceDetails' {platform} -> platform) (\s@EC2InstanceDetails' {} a -> s {platform = a} :: EC2InstanceDetails)

-- | The Amazon Web Services Region of the recommended reservation.
eC2InstanceDetails_region :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Text)
eC2InstanceDetails_region = Lens.lens (\EC2InstanceDetails' {region} -> region) (\s@EC2InstanceDetails' {} a -> s {region = a} :: EC2InstanceDetails)

-- | Determines whether the recommended reservation is size flexible.
eC2InstanceDetails_sizeFlexEligible :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Bool)
eC2InstanceDetails_sizeFlexEligible = Lens.lens (\EC2InstanceDetails' {sizeFlexEligible} -> sizeFlexEligible) (\s@EC2InstanceDetails' {} a -> s {sizeFlexEligible = a} :: EC2InstanceDetails)

-- | Determines whether the recommended reservation is dedicated or shared.
eC2InstanceDetails_tenancy :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Text)
eC2InstanceDetails_tenancy = Lens.lens (\EC2InstanceDetails' {tenancy} -> tenancy) (\s@EC2InstanceDetails' {} a -> s {tenancy = a} :: EC2InstanceDetails)

instance Data.FromJSON EC2InstanceDetails where
  parseJSON =
    Data.withObject
      "EC2InstanceDetails"
      ( \x ->
          EC2InstanceDetails'
            Prelude.<$> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "CurrentGeneration")
            Prelude.<*> (x Data..:? "Family")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "Platform")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "SizeFlexEligible")
            Prelude.<*> (x Data..:? "Tenancy")
      )

instance Prelude.Hashable EC2InstanceDetails where
  hashWithSalt _salt EC2InstanceDetails' {..} =
    _salt `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` currentGeneration
      `Prelude.hashWithSalt` family
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` sizeFlexEligible
      `Prelude.hashWithSalt` tenancy

instance Prelude.NFData EC2InstanceDetails where
  rnf EC2InstanceDetails' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf currentGeneration
      `Prelude.seq` Prelude.rnf family
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf sizeFlexEligible
      `Prelude.seq` Prelude.rnf tenancy
