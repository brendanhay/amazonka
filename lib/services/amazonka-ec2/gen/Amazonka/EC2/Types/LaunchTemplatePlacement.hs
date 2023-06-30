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
-- Module      : Amazonka.EC2.Types.LaunchTemplatePlacement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplatePlacement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tenancy
import qualified Amazonka.Prelude as Prelude

-- | Describes the placement of an instance.
--
-- /See:/ 'newLaunchTemplatePlacement' smart constructor.
data LaunchTemplatePlacement = LaunchTemplatePlacement'
  { -- | The affinity setting for the instance on the Dedicated Host.
    affinity :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone of the instance.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The Group ID of the placement group. You must specify the Placement
    -- Group __Group ID__ to launch an instance in a shared placement group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the placement group for the instance.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Dedicated Host for the instance.
    hostId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the host resource group in which to launch the instances.
    hostResourceGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The number of the partition the instance should launch in. Valid only if
    -- the placement group strategy is set to @partition@.
    partitionNumber :: Prelude.Maybe Prelude.Int,
    -- | Reserved for future use.
    spreadDomain :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance (if the instance is running in a VPC). An
    -- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
    tenancy :: Prelude.Maybe Tenancy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplatePlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'affinity', 'launchTemplatePlacement_affinity' - The affinity setting for the instance on the Dedicated Host.
--
-- 'availabilityZone', 'launchTemplatePlacement_availabilityZone' - The Availability Zone of the instance.
--
-- 'groupId', 'launchTemplatePlacement_groupId' - The Group ID of the placement group. You must specify the Placement
-- Group __Group ID__ to launch an instance in a shared placement group.
--
-- 'groupName', 'launchTemplatePlacement_groupName' - The name of the placement group for the instance.
--
-- 'hostId', 'launchTemplatePlacement_hostId' - The ID of the Dedicated Host for the instance.
--
-- 'hostResourceGroupArn', 'launchTemplatePlacement_hostResourceGroupArn' - The ARN of the host resource group in which to launch the instances.
--
-- 'partitionNumber', 'launchTemplatePlacement_partitionNumber' - The number of the partition the instance should launch in. Valid only if
-- the placement group strategy is set to @partition@.
--
-- 'spreadDomain', 'launchTemplatePlacement_spreadDomain' - Reserved for future use.
--
-- 'tenancy', 'launchTemplatePlacement_tenancy' - The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
newLaunchTemplatePlacement ::
  LaunchTemplatePlacement
newLaunchTemplatePlacement =
  LaunchTemplatePlacement'
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

-- | The affinity setting for the instance on the Dedicated Host.
launchTemplatePlacement_affinity :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_affinity = Lens.lens (\LaunchTemplatePlacement' {affinity} -> affinity) (\s@LaunchTemplatePlacement' {} a -> s {affinity = a} :: LaunchTemplatePlacement)

-- | The Availability Zone of the instance.
launchTemplatePlacement_availabilityZone :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_availabilityZone = Lens.lens (\LaunchTemplatePlacement' {availabilityZone} -> availabilityZone) (\s@LaunchTemplatePlacement' {} a -> s {availabilityZone = a} :: LaunchTemplatePlacement)

-- | The Group ID of the placement group. You must specify the Placement
-- Group __Group ID__ to launch an instance in a shared placement group.
launchTemplatePlacement_groupId :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_groupId = Lens.lens (\LaunchTemplatePlacement' {groupId} -> groupId) (\s@LaunchTemplatePlacement' {} a -> s {groupId = a} :: LaunchTemplatePlacement)

-- | The name of the placement group for the instance.
launchTemplatePlacement_groupName :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_groupName = Lens.lens (\LaunchTemplatePlacement' {groupName} -> groupName) (\s@LaunchTemplatePlacement' {} a -> s {groupName = a} :: LaunchTemplatePlacement)

-- | The ID of the Dedicated Host for the instance.
launchTemplatePlacement_hostId :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_hostId = Lens.lens (\LaunchTemplatePlacement' {hostId} -> hostId) (\s@LaunchTemplatePlacement' {} a -> s {hostId = a} :: LaunchTemplatePlacement)

-- | The ARN of the host resource group in which to launch the instances.
launchTemplatePlacement_hostResourceGroupArn :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_hostResourceGroupArn = Lens.lens (\LaunchTemplatePlacement' {hostResourceGroupArn} -> hostResourceGroupArn) (\s@LaunchTemplatePlacement' {} a -> s {hostResourceGroupArn = a} :: LaunchTemplatePlacement)

-- | The number of the partition the instance should launch in. Valid only if
-- the placement group strategy is set to @partition@.
launchTemplatePlacement_partitionNumber :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Int)
launchTemplatePlacement_partitionNumber = Lens.lens (\LaunchTemplatePlacement' {partitionNumber} -> partitionNumber) (\s@LaunchTemplatePlacement' {} a -> s {partitionNumber = a} :: LaunchTemplatePlacement)

-- | Reserved for future use.
launchTemplatePlacement_spreadDomain :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_spreadDomain = Lens.lens (\LaunchTemplatePlacement' {spreadDomain} -> spreadDomain) (\s@LaunchTemplatePlacement' {} a -> s {spreadDomain = a} :: LaunchTemplatePlacement)

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
launchTemplatePlacement_tenancy :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Tenancy)
launchTemplatePlacement_tenancy = Lens.lens (\LaunchTemplatePlacement' {tenancy} -> tenancy) (\s@LaunchTemplatePlacement' {} a -> s {tenancy = a} :: LaunchTemplatePlacement)

instance Data.FromXML LaunchTemplatePlacement where
  parseXML x =
    LaunchTemplatePlacement'
      Prelude.<$> (x Data..@? "affinity")
      Prelude.<*> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "groupId")
      Prelude.<*> (x Data..@? "groupName")
      Prelude.<*> (x Data..@? "hostId")
      Prelude.<*> (x Data..@? "hostResourceGroupArn")
      Prelude.<*> (x Data..@? "partitionNumber")
      Prelude.<*> (x Data..@? "spreadDomain")
      Prelude.<*> (x Data..@? "tenancy")

instance Prelude.Hashable LaunchTemplatePlacement where
  hashWithSalt _salt LaunchTemplatePlacement' {..} =
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

instance Prelude.NFData LaunchTemplatePlacement where
  rnf LaunchTemplatePlacement' {..} =
    Prelude.rnf affinity
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf hostId
      `Prelude.seq` Prelude.rnf hostResourceGroupArn
      `Prelude.seq` Prelude.rnf partitionNumber
      `Prelude.seq` Prelude.rnf spreadDomain
      `Prelude.seq` Prelude.rnf tenancy
