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
-- Module      : Amazonka.EC2.Types.LaunchTemplatePlacementRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplatePlacementRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tenancy
import qualified Amazonka.Prelude as Prelude

-- | Describes the placement of an instance.
--
-- /See:/ 'newLaunchTemplatePlacementRequest' smart constructor.
data LaunchTemplatePlacementRequest = LaunchTemplatePlacementRequest'
  { -- | The affinity setting for an instance on a Dedicated Host.
    affinity :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone for the instance.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The Group Id of a placement group. You must specify the Placement Group
    -- __Group Id__ to launch an instance in a shared placement group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the placement group for the instance.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Dedicated Host for the instance.
    hostId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the host resource group in which to launch the instances. If
    -- you specify a host resource group ARN, omit the __Tenancy__ parameter or
    -- set it to @host@.
    hostResourceGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The number of the partition the instance should launch in. Valid only if
    -- the placement group strategy is set to @partition@.
    partitionNumber :: Prelude.Maybe Prelude.Int,
    -- | Reserved for future use.
    spreadDomain :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance (if the instance is running in a VPC). An
    -- instance with a tenancy of dedicated runs on single-tenant hardware.
    tenancy :: Prelude.Maybe Tenancy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplatePlacementRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'affinity', 'launchTemplatePlacementRequest_affinity' - The affinity setting for an instance on a Dedicated Host.
--
-- 'availabilityZone', 'launchTemplatePlacementRequest_availabilityZone' - The Availability Zone for the instance.
--
-- 'groupId', 'launchTemplatePlacementRequest_groupId' - The Group Id of a placement group. You must specify the Placement Group
-- __Group Id__ to launch an instance in a shared placement group.
--
-- 'groupName', 'launchTemplatePlacementRequest_groupName' - The name of the placement group for the instance.
--
-- 'hostId', 'launchTemplatePlacementRequest_hostId' - The ID of the Dedicated Host for the instance.
--
-- 'hostResourceGroupArn', 'launchTemplatePlacementRequest_hostResourceGroupArn' - The ARN of the host resource group in which to launch the instances. If
-- you specify a host resource group ARN, omit the __Tenancy__ parameter or
-- set it to @host@.
--
-- 'partitionNumber', 'launchTemplatePlacementRequest_partitionNumber' - The number of the partition the instance should launch in. Valid only if
-- the placement group strategy is set to @partition@.
--
-- 'spreadDomain', 'launchTemplatePlacementRequest_spreadDomain' - Reserved for future use.
--
-- 'tenancy', 'launchTemplatePlacementRequest_tenancy' - The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of dedicated runs on single-tenant hardware.
newLaunchTemplatePlacementRequest ::
  LaunchTemplatePlacementRequest
newLaunchTemplatePlacementRequest =
  LaunchTemplatePlacementRequest'
    { affinity =
        Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      groupId = Prelude.Nothing,
      groupName = Prelude.Nothing,
      hostId = Prelude.Nothing,
      hostResourceGroupArn = Prelude.Nothing,
      partitionNumber = Prelude.Nothing,
      spreadDomain = Prelude.Nothing,
      tenancy = Prelude.Nothing
    }

-- | The affinity setting for an instance on a Dedicated Host.
launchTemplatePlacementRequest_affinity :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Text)
launchTemplatePlacementRequest_affinity = Lens.lens (\LaunchTemplatePlacementRequest' {affinity} -> affinity) (\s@LaunchTemplatePlacementRequest' {} a -> s {affinity = a} :: LaunchTemplatePlacementRequest)

-- | The Availability Zone for the instance.
launchTemplatePlacementRequest_availabilityZone :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Text)
launchTemplatePlacementRequest_availabilityZone = Lens.lens (\LaunchTemplatePlacementRequest' {availabilityZone} -> availabilityZone) (\s@LaunchTemplatePlacementRequest' {} a -> s {availabilityZone = a} :: LaunchTemplatePlacementRequest)

-- | The Group Id of a placement group. You must specify the Placement Group
-- __Group Id__ to launch an instance in a shared placement group.
launchTemplatePlacementRequest_groupId :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Text)
launchTemplatePlacementRequest_groupId = Lens.lens (\LaunchTemplatePlacementRequest' {groupId} -> groupId) (\s@LaunchTemplatePlacementRequest' {} a -> s {groupId = a} :: LaunchTemplatePlacementRequest)

-- | The name of the placement group for the instance.
launchTemplatePlacementRequest_groupName :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Text)
launchTemplatePlacementRequest_groupName = Lens.lens (\LaunchTemplatePlacementRequest' {groupName} -> groupName) (\s@LaunchTemplatePlacementRequest' {} a -> s {groupName = a} :: LaunchTemplatePlacementRequest)

-- | The ID of the Dedicated Host for the instance.
launchTemplatePlacementRequest_hostId :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Text)
launchTemplatePlacementRequest_hostId = Lens.lens (\LaunchTemplatePlacementRequest' {hostId} -> hostId) (\s@LaunchTemplatePlacementRequest' {} a -> s {hostId = a} :: LaunchTemplatePlacementRequest)

-- | The ARN of the host resource group in which to launch the instances. If
-- you specify a host resource group ARN, omit the __Tenancy__ parameter or
-- set it to @host@.
launchTemplatePlacementRequest_hostResourceGroupArn :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Text)
launchTemplatePlacementRequest_hostResourceGroupArn = Lens.lens (\LaunchTemplatePlacementRequest' {hostResourceGroupArn} -> hostResourceGroupArn) (\s@LaunchTemplatePlacementRequest' {} a -> s {hostResourceGroupArn = a} :: LaunchTemplatePlacementRequest)

-- | The number of the partition the instance should launch in. Valid only if
-- the placement group strategy is set to @partition@.
launchTemplatePlacementRequest_partitionNumber :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Int)
launchTemplatePlacementRequest_partitionNumber = Lens.lens (\LaunchTemplatePlacementRequest' {partitionNumber} -> partitionNumber) (\s@LaunchTemplatePlacementRequest' {} a -> s {partitionNumber = a} :: LaunchTemplatePlacementRequest)

-- | Reserved for future use.
launchTemplatePlacementRequest_spreadDomain :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Prelude.Text)
launchTemplatePlacementRequest_spreadDomain = Lens.lens (\LaunchTemplatePlacementRequest' {spreadDomain} -> spreadDomain) (\s@LaunchTemplatePlacementRequest' {} a -> s {spreadDomain = a} :: LaunchTemplatePlacementRequest)

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of dedicated runs on single-tenant hardware.
launchTemplatePlacementRequest_tenancy :: Lens.Lens' LaunchTemplatePlacementRequest (Prelude.Maybe Tenancy)
launchTemplatePlacementRequest_tenancy = Lens.lens (\LaunchTemplatePlacementRequest' {tenancy} -> tenancy) (\s@LaunchTemplatePlacementRequest' {} a -> s {tenancy = a} :: LaunchTemplatePlacementRequest)

instance
  Prelude.Hashable
    LaunchTemplatePlacementRequest
  where
  hashWithSalt
    _salt
    LaunchTemplatePlacementRequest' {..} =
      _salt
        `Prelude.hashWithSalt` affinity
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` groupId
        `Prelude.hashWithSalt` groupName
        `Prelude.hashWithSalt` hostId
        `Prelude.hashWithSalt` hostResourceGroupArn
        `Prelude.hashWithSalt` partitionNumber
        `Prelude.hashWithSalt` spreadDomain
        `Prelude.hashWithSalt` tenancy

instance
  Prelude.NFData
    LaunchTemplatePlacementRequest
  where
  rnf LaunchTemplatePlacementRequest' {..} =
    Prelude.rnf affinity
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf hostId
      `Prelude.seq` Prelude.rnf hostResourceGroupArn
      `Prelude.seq` Prelude.rnf partitionNumber
      `Prelude.seq` Prelude.rnf spreadDomain
      `Prelude.seq` Prelude.rnf tenancy

instance Data.ToQuery LaunchTemplatePlacementRequest where
  toQuery LaunchTemplatePlacementRequest' {..} =
    Prelude.mconcat
      [ "Affinity" Data.=: affinity,
        "AvailabilityZone" Data.=: availabilityZone,
        "GroupId" Data.=: groupId,
        "GroupName" Data.=: groupName,
        "HostId" Data.=: hostId,
        "HostResourceGroupArn" Data.=: hostResourceGroupArn,
        "PartitionNumber" Data.=: partitionNumber,
        "SpreadDomain" Data.=: spreadDomain,
        "Tenancy" Data.=: tenancy
      ]
