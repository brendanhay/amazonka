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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details about the Amazon EC2 instances that AWS recommends that you
-- purchase.
--
-- /See:/ 'newEC2InstanceDetails' smart constructor.
data EC2InstanceDetails = EC2InstanceDetails'
  { -- | The platform of the recommended reservation. The platform is the
    -- specific combination of operating system, license model, and software on
    -- an instance.
    platform :: Core.Maybe Core.Text,
    -- | The type of instance that AWS recommends.
    instanceType :: Core.Maybe Core.Text,
    -- | Whether the recommended reservation is dedicated or shared.
    tenancy :: Core.Maybe Core.Text,
    -- | Whether the recommendation is for a current-generation instance.
    currentGeneration :: Core.Maybe Core.Bool,
    -- | Whether the recommended reservation is size flexible.
    sizeFlexEligible :: Core.Maybe Core.Bool,
    -- | The Availability Zone of the recommended reservation.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The instance family of the recommended reservation.
    family :: Core.Maybe Core.Text,
    -- | The AWS Region of the recommended reservation.
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { platform = Core.Nothing,
      instanceType = Core.Nothing,
      tenancy = Core.Nothing,
      currentGeneration = Core.Nothing,
      sizeFlexEligible = Core.Nothing,
      availabilityZone = Core.Nothing,
      family = Core.Nothing,
      region = Core.Nothing
    }

-- | The platform of the recommended reservation. The platform is the
-- specific combination of operating system, license model, and software on
-- an instance.
eC2InstanceDetails_platform :: Lens.Lens' EC2InstanceDetails (Core.Maybe Core.Text)
eC2InstanceDetails_platform = Lens.lens (\EC2InstanceDetails' {platform} -> platform) (\s@EC2InstanceDetails' {} a -> s {platform = a} :: EC2InstanceDetails)

-- | The type of instance that AWS recommends.
eC2InstanceDetails_instanceType :: Lens.Lens' EC2InstanceDetails (Core.Maybe Core.Text)
eC2InstanceDetails_instanceType = Lens.lens (\EC2InstanceDetails' {instanceType} -> instanceType) (\s@EC2InstanceDetails' {} a -> s {instanceType = a} :: EC2InstanceDetails)

-- | Whether the recommended reservation is dedicated or shared.
eC2InstanceDetails_tenancy :: Lens.Lens' EC2InstanceDetails (Core.Maybe Core.Text)
eC2InstanceDetails_tenancy = Lens.lens (\EC2InstanceDetails' {tenancy} -> tenancy) (\s@EC2InstanceDetails' {} a -> s {tenancy = a} :: EC2InstanceDetails)

-- | Whether the recommendation is for a current-generation instance.
eC2InstanceDetails_currentGeneration :: Lens.Lens' EC2InstanceDetails (Core.Maybe Core.Bool)
eC2InstanceDetails_currentGeneration = Lens.lens (\EC2InstanceDetails' {currentGeneration} -> currentGeneration) (\s@EC2InstanceDetails' {} a -> s {currentGeneration = a} :: EC2InstanceDetails)

-- | Whether the recommended reservation is size flexible.
eC2InstanceDetails_sizeFlexEligible :: Lens.Lens' EC2InstanceDetails (Core.Maybe Core.Bool)
eC2InstanceDetails_sizeFlexEligible = Lens.lens (\EC2InstanceDetails' {sizeFlexEligible} -> sizeFlexEligible) (\s@EC2InstanceDetails' {} a -> s {sizeFlexEligible = a} :: EC2InstanceDetails)

-- | The Availability Zone of the recommended reservation.
eC2InstanceDetails_availabilityZone :: Lens.Lens' EC2InstanceDetails (Core.Maybe Core.Text)
eC2InstanceDetails_availabilityZone = Lens.lens (\EC2InstanceDetails' {availabilityZone} -> availabilityZone) (\s@EC2InstanceDetails' {} a -> s {availabilityZone = a} :: EC2InstanceDetails)

-- | The instance family of the recommended reservation.
eC2InstanceDetails_family :: Lens.Lens' EC2InstanceDetails (Core.Maybe Core.Text)
eC2InstanceDetails_family = Lens.lens (\EC2InstanceDetails' {family} -> family) (\s@EC2InstanceDetails' {} a -> s {family = a} :: EC2InstanceDetails)

-- | The AWS Region of the recommended reservation.
eC2InstanceDetails_region :: Lens.Lens' EC2InstanceDetails (Core.Maybe Core.Text)
eC2InstanceDetails_region = Lens.lens (\EC2InstanceDetails' {region} -> region) (\s@EC2InstanceDetails' {} a -> s {region = a} :: EC2InstanceDetails)

instance Core.FromJSON EC2InstanceDetails where
  parseJSON =
    Core.withObject
      "EC2InstanceDetails"
      ( \x ->
          EC2InstanceDetails'
            Core.<$> (x Core..:? "Platform")
            Core.<*> (x Core..:? "InstanceType")
            Core.<*> (x Core..:? "Tenancy")
            Core.<*> (x Core..:? "CurrentGeneration")
            Core.<*> (x Core..:? "SizeFlexEligible")
            Core.<*> (x Core..:? "AvailabilityZone")
            Core.<*> (x Core..:? "Family")
            Core.<*> (x Core..:? "Region")
      )

instance Core.Hashable EC2InstanceDetails

instance Core.NFData EC2InstanceDetails
