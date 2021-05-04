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
-- Module      : Network.AWS.EC2.Types.LaunchTemplatePlacementRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatePlacementRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tenancy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the placement of an instance.
--
-- /See:/ 'newLaunchTemplatePlacementRequest' smart constructor.
data LaunchTemplatePlacementRequest = LaunchTemplatePlacementRequest'
  { -- | Reserved for future use.
    spreadDomain :: Prelude.Maybe Prelude.Text,
    -- | The name of the placement group for the instance.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance (if the instance is running in a VPC). An
    -- instance with a tenancy of dedicated runs on single-tenant hardware.
    tenancy :: Prelude.Maybe Tenancy,
    -- | The affinity setting for an instance on a Dedicated Host.
    affinity :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone for the instance.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The number of the partition the instance should launch in. Valid only if
    -- the placement group strategy is set to @partition@.
    partitionNumber :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the host resource group in which to launch the instances. If
    -- you specify a host resource group ARN, omit the __Tenancy__ parameter or
    -- set it to @host@.
    hostResourceGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Dedicated Host for the instance.
    hostId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplatePlacementRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spreadDomain', 'launchTemplatePlacementRequest_spreadDomain' - Reserved for future use.
--
-- 'groupName', 'launchTemplatePlacementRequest_groupName' - The name of the placement group for the instance.
--
-- 'tenancy', 'launchTemplatePlacementRequest_tenancy' - The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of dedicated runs on single-tenant hardware.
--
-- 'affinity', 'launchTemplatePlacementRequest_affinity' - The affinity setting for an instance on a Dedicated Host.
--
-- 'availabilityZone', 'launchTemplatePlacementRequest_availabilityZone' - The Availability Zone for the instance.
--
-- 'partitionNumber', 'launchTemplatePlacementRequest_partitionNumber' - The number of the partition the instance should launch in. Valid only if
-- the placement group strategy is set to @partition@.
--
-- 'hostResourceGroupArn', 'launchTemplatePlacementRequest_hostResourceGroupArn' - The ARN of the host resource group in which to launch the instances. If
-- you specify a host resource group ARN, omit the __Tenancy__ parameter or
-- set it to @host@.
--
-- 'hostId', 'launchTemplatePlacementRequest_hostId' - The ID of the Dedicated Host for the instance.
newLaunchTemplatePlacementRequest ::
  LaunchTemplatePlacementRequest
newLaunchTemplatePlacementRequest =
  LaunchTemplatePlacementRequest'
    { spreadDomain =
        Prelude.Nothing,
      groupName = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      affinity = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      partitionNumber = Prelude.Nothing,
      hostResourceGroupArn = Prelude.Nothing,
      hostId = Prelude.Nothing
    }

-- | Reserved for future use.
launchTemplatePlacementRequest_spreadDomain :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Text)
launchTemplatePlacementRequest_spreadDomain = Lens.lens (\LaunchTemplatePlacementRequest' {spreadDomain} -> spreadDomain) (\s@LaunchTemplatePlacementRequest' {} a -> s {spreadDomain = a} :: LaunchTemplatePlacementRequest)

-- | The name of the placement group for the instance.
launchTemplatePlacementRequest_groupName :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Text)
launchTemplatePlacementRequest_groupName = Lens.lens (\LaunchTemplatePlacementRequest' {groupName} -> groupName) (\s@LaunchTemplatePlacementRequest' {} a -> s {groupName = a} :: LaunchTemplatePlacementRequest)

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of dedicated runs on single-tenant hardware.
launchTemplatePlacementRequest_tenancy :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Tenancy)
launchTemplatePlacementRequest_tenancy = Lens.lens (\LaunchTemplatePlacementRequest' {tenancy} -> tenancy) (\s@LaunchTemplatePlacementRequest' {} a -> s {tenancy = a} :: LaunchTemplatePlacementRequest)

-- | The affinity setting for an instance on a Dedicated Host.
launchTemplatePlacementRequest_affinity :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Text)
launchTemplatePlacementRequest_affinity = Lens.lens (\LaunchTemplatePlacementRequest' {affinity} -> affinity) (\s@LaunchTemplatePlacementRequest' {} a -> s {affinity = a} :: LaunchTemplatePlacementRequest)

-- | The Availability Zone for the instance.
launchTemplatePlacementRequest_availabilityZone :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Text)
launchTemplatePlacementRequest_availabilityZone = Lens.lens (\LaunchTemplatePlacementRequest' {availabilityZone} -> availabilityZone) (\s@LaunchTemplatePlacementRequest' {} a -> s {availabilityZone = a} :: LaunchTemplatePlacementRequest)

-- | The number of the partition the instance should launch in. Valid only if
-- the placement group strategy is set to @partition@.
launchTemplatePlacementRequest_partitionNumber :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Int)
launchTemplatePlacementRequest_partitionNumber = Lens.lens (\LaunchTemplatePlacementRequest' {partitionNumber} -> partitionNumber) (\s@LaunchTemplatePlacementRequest' {} a -> s {partitionNumber = a} :: LaunchTemplatePlacementRequest)

-- | The ARN of the host resource group in which to launch the instances. If
-- you specify a host resource group ARN, omit the __Tenancy__ parameter or
-- set it to @host@.
launchTemplatePlacementRequest_hostResourceGroupArn :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Text)
launchTemplatePlacementRequest_hostResourceGroupArn = Lens.lens (\LaunchTemplatePlacementRequest' {hostResourceGroupArn} -> hostResourceGroupArn) (\s@LaunchTemplatePlacementRequest' {} a -> s {hostResourceGroupArn = a} :: LaunchTemplatePlacementRequest)

-- | The ID of the Dedicated Host for the instance.
launchTemplatePlacementRequest_hostId :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Text)
launchTemplatePlacementRequest_hostId = Lens.lens (\LaunchTemplatePlacementRequest' {hostId} -> hostId) (\s@LaunchTemplatePlacementRequest' {} a -> s {hostId = a} :: LaunchTemplatePlacementRequest)

instance
  Prelude.Hashable
    LaunchTemplatePlacementRequest

instance
  Prelude.NFData
    LaunchTemplatePlacementRequest

instance
  Prelude.ToQuery
    LaunchTemplatePlacementRequest
  where
  toQuery LaunchTemplatePlacementRequest' {..} =
    Prelude.mconcat
      [ "SpreadDomain" Prelude.=: spreadDomain,
        "GroupName" Prelude.=: groupName,
        "Tenancy" Prelude.=: tenancy,
        "Affinity" Prelude.=: affinity,
        "AvailabilityZone" Prelude.=: availabilityZone,
        "PartitionNumber" Prelude.=: partitionNumber,
        "HostResourceGroupArn"
          Prelude.=: hostResourceGroupArn,
        "HostId" Prelude.=: hostId
      ]
