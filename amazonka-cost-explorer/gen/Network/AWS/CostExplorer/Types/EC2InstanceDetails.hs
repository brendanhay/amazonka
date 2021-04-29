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
-- Module      : Network.AWS.CostExplorer.Types.EC2InstanceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2InstanceDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about the Amazon EC2 instances that AWS recommends that you
-- purchase.
--
-- /See:/ 'newEC2InstanceDetails' smart constructor.
data EC2InstanceDetails = EC2InstanceDetails'
  { -- | The platform of the recommended reservation. The platform is the
    -- specific combination of operating system, license model, and software on
    -- an instance.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The type of instance that AWS recommends.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Whether the recommended reservation is dedicated or shared.
    tenancy :: Prelude.Maybe Prelude.Text,
    -- | Whether the recommendation is for a current-generation instance.
    currentGeneration :: Prelude.Maybe Prelude.Bool,
    -- | Whether the recommended reservation is size flexible.
    sizeFlexEligible :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone of the recommended reservation.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The instance family of the recommended reservation.
    family :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region of the recommended reservation.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EC2InstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'eC2InstanceDetails_platform' - The platform of the recommended reservation. The platform is the
-- specific combination of operating system, license model, and software on
-- an instance.
--
-- 'instanceType', 'eC2InstanceDetails_instanceType' - The type of instance that AWS recommends.
--
-- 'tenancy', 'eC2InstanceDetails_tenancy' - Whether the recommended reservation is dedicated or shared.
--
-- 'currentGeneration', 'eC2InstanceDetails_currentGeneration' - Whether the recommendation is for a current-generation instance.
--
-- 'sizeFlexEligible', 'eC2InstanceDetails_sizeFlexEligible' - Whether the recommended reservation is size flexible.
--
-- 'availabilityZone', 'eC2InstanceDetails_availabilityZone' - The Availability Zone of the recommended reservation.
--
-- 'family', 'eC2InstanceDetails_family' - The instance family of the recommended reservation.
--
-- 'region', 'eC2InstanceDetails_region' - The AWS Region of the recommended reservation.
newEC2InstanceDetails ::
  EC2InstanceDetails
newEC2InstanceDetails =
  EC2InstanceDetails'
    { platform = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      currentGeneration = Prelude.Nothing,
      sizeFlexEligible = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      family = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The platform of the recommended reservation. The platform is the
-- specific combination of operating system, license model, and software on
-- an instance.
eC2InstanceDetails_platform :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Text)
eC2InstanceDetails_platform = Lens.lens (\EC2InstanceDetails' {platform} -> platform) (\s@EC2InstanceDetails' {} a -> s {platform = a} :: EC2InstanceDetails)

-- | The type of instance that AWS recommends.
eC2InstanceDetails_instanceType :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Text)
eC2InstanceDetails_instanceType = Lens.lens (\EC2InstanceDetails' {instanceType} -> instanceType) (\s@EC2InstanceDetails' {} a -> s {instanceType = a} :: EC2InstanceDetails)

-- | Whether the recommended reservation is dedicated or shared.
eC2InstanceDetails_tenancy :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Text)
eC2InstanceDetails_tenancy = Lens.lens (\EC2InstanceDetails' {tenancy} -> tenancy) (\s@EC2InstanceDetails' {} a -> s {tenancy = a} :: EC2InstanceDetails)

-- | Whether the recommendation is for a current-generation instance.
eC2InstanceDetails_currentGeneration :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Bool)
eC2InstanceDetails_currentGeneration = Lens.lens (\EC2InstanceDetails' {currentGeneration} -> currentGeneration) (\s@EC2InstanceDetails' {} a -> s {currentGeneration = a} :: EC2InstanceDetails)

-- | Whether the recommended reservation is size flexible.
eC2InstanceDetails_sizeFlexEligible :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Bool)
eC2InstanceDetails_sizeFlexEligible = Lens.lens (\EC2InstanceDetails' {sizeFlexEligible} -> sizeFlexEligible) (\s@EC2InstanceDetails' {} a -> s {sizeFlexEligible = a} :: EC2InstanceDetails)

-- | The Availability Zone of the recommended reservation.
eC2InstanceDetails_availabilityZone :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Text)
eC2InstanceDetails_availabilityZone = Lens.lens (\EC2InstanceDetails' {availabilityZone} -> availabilityZone) (\s@EC2InstanceDetails' {} a -> s {availabilityZone = a} :: EC2InstanceDetails)

-- | The instance family of the recommended reservation.
eC2InstanceDetails_family :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Text)
eC2InstanceDetails_family = Lens.lens (\EC2InstanceDetails' {family} -> family) (\s@EC2InstanceDetails' {} a -> s {family = a} :: EC2InstanceDetails)

-- | The AWS Region of the recommended reservation.
eC2InstanceDetails_region :: Lens.Lens' EC2InstanceDetails (Prelude.Maybe Prelude.Text)
eC2InstanceDetails_region = Lens.lens (\EC2InstanceDetails' {region} -> region) (\s@EC2InstanceDetails' {} a -> s {region = a} :: EC2InstanceDetails)

instance Prelude.FromJSON EC2InstanceDetails where
  parseJSON =
    Prelude.withObject
      "EC2InstanceDetails"
      ( \x ->
          EC2InstanceDetails'
            Prelude.<$> (x Prelude..:? "Platform")
            Prelude.<*> (x Prelude..:? "InstanceType")
            Prelude.<*> (x Prelude..:? "Tenancy")
            Prelude.<*> (x Prelude..:? "CurrentGeneration")
            Prelude.<*> (x Prelude..:? "SizeFlexEligible")
            Prelude.<*> (x Prelude..:? "AvailabilityZone")
            Prelude.<*> (x Prelude..:? "Family")
            Prelude.<*> (x Prelude..:? "Region")
      )

instance Prelude.Hashable EC2InstanceDetails

instance Prelude.NFData EC2InstanceDetails
