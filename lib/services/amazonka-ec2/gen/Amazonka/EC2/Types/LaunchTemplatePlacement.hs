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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplatePlacement where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tenancy
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the placement of an instance.
--
-- /See:/ 'newLaunchTemplatePlacement' smart constructor.
data LaunchTemplatePlacement = LaunchTemplatePlacement'
  { -- | The affinity setting for the instance on the Dedicated Host.
    affinity :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Dedicated Host for the instance.
    hostId :: Prelude.Maybe Prelude.Text,
    -- | The number of the partition the instance should launch in. Valid only if
    -- the placement group strategy is set to @partition@.
    partitionNumber :: Prelude.Maybe Prelude.Int,
    -- | Reserved for future use.
    spreadDomain :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone of the instance.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance (if the instance is running in a VPC). An
    -- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
    tenancy :: Prelude.Maybe Tenancy,
    -- | The name of the placement group for the instance.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the host resource group in which to launch the instances.
    hostResourceGroupArn :: Prelude.Maybe Prelude.Text
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
-- 'hostId', 'launchTemplatePlacement_hostId' - The ID of the Dedicated Host for the instance.
--
-- 'partitionNumber', 'launchTemplatePlacement_partitionNumber' - The number of the partition the instance should launch in. Valid only if
-- the placement group strategy is set to @partition@.
--
-- 'spreadDomain', 'launchTemplatePlacement_spreadDomain' - Reserved for future use.
--
-- 'availabilityZone', 'launchTemplatePlacement_availabilityZone' - The Availability Zone of the instance.
--
-- 'tenancy', 'launchTemplatePlacement_tenancy' - The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
--
-- 'groupName', 'launchTemplatePlacement_groupName' - The name of the placement group for the instance.
--
-- 'hostResourceGroupArn', 'launchTemplatePlacement_hostResourceGroupArn' - The ARN of the host resource group in which to launch the instances.
newLaunchTemplatePlacement ::
  LaunchTemplatePlacement
newLaunchTemplatePlacement =
  LaunchTemplatePlacement'
    { affinity =
        Prelude.Nothing,
      hostId = Prelude.Nothing,
      partitionNumber = Prelude.Nothing,
      spreadDomain = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      groupName = Prelude.Nothing,
      hostResourceGroupArn = Prelude.Nothing
    }

-- | The affinity setting for the instance on the Dedicated Host.
launchTemplatePlacement_affinity :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_affinity = Lens.lens (\LaunchTemplatePlacement' {affinity} -> affinity) (\s@LaunchTemplatePlacement' {} a -> s {affinity = a} :: LaunchTemplatePlacement)

-- | The ID of the Dedicated Host for the instance.
launchTemplatePlacement_hostId :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_hostId = Lens.lens (\LaunchTemplatePlacement' {hostId} -> hostId) (\s@LaunchTemplatePlacement' {} a -> s {hostId = a} :: LaunchTemplatePlacement)

-- | The number of the partition the instance should launch in. Valid only if
-- the placement group strategy is set to @partition@.
launchTemplatePlacement_partitionNumber :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Int)
launchTemplatePlacement_partitionNumber = Lens.lens (\LaunchTemplatePlacement' {partitionNumber} -> partitionNumber) (\s@LaunchTemplatePlacement' {} a -> s {partitionNumber = a} :: LaunchTemplatePlacement)

-- | Reserved for future use.
launchTemplatePlacement_spreadDomain :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_spreadDomain = Lens.lens (\LaunchTemplatePlacement' {spreadDomain} -> spreadDomain) (\s@LaunchTemplatePlacement' {} a -> s {spreadDomain = a} :: LaunchTemplatePlacement)

-- | The Availability Zone of the instance.
launchTemplatePlacement_availabilityZone :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_availabilityZone = Lens.lens (\LaunchTemplatePlacement' {availabilityZone} -> availabilityZone) (\s@LaunchTemplatePlacement' {} a -> s {availabilityZone = a} :: LaunchTemplatePlacement)

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
launchTemplatePlacement_tenancy :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Tenancy)
launchTemplatePlacement_tenancy = Lens.lens (\LaunchTemplatePlacement' {tenancy} -> tenancy) (\s@LaunchTemplatePlacement' {} a -> s {tenancy = a} :: LaunchTemplatePlacement)

-- | The name of the placement group for the instance.
launchTemplatePlacement_groupName :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_groupName = Lens.lens (\LaunchTemplatePlacement' {groupName} -> groupName) (\s@LaunchTemplatePlacement' {} a -> s {groupName = a} :: LaunchTemplatePlacement)

-- | The ARN of the host resource group in which to launch the instances.
launchTemplatePlacement_hostResourceGroupArn :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_hostResourceGroupArn = Lens.lens (\LaunchTemplatePlacement' {hostResourceGroupArn} -> hostResourceGroupArn) (\s@LaunchTemplatePlacement' {} a -> s {hostResourceGroupArn = a} :: LaunchTemplatePlacement)

instance Core.FromXML LaunchTemplatePlacement where
  parseXML x =
    LaunchTemplatePlacement'
      Prelude.<$> (x Core..@? "affinity")
      Prelude.<*> (x Core..@? "hostId")
      Prelude.<*> (x Core..@? "partitionNumber")
      Prelude.<*> (x Core..@? "spreadDomain")
      Prelude.<*> (x Core..@? "availabilityZone")
      Prelude.<*> (x Core..@? "tenancy")
      Prelude.<*> (x Core..@? "groupName")
      Prelude.<*> (x Core..@? "hostResourceGroupArn")

instance Prelude.Hashable LaunchTemplatePlacement where
  hashWithSalt _salt LaunchTemplatePlacement' {..} =
    _salt `Prelude.hashWithSalt` affinity
      `Prelude.hashWithSalt` hostId
      `Prelude.hashWithSalt` partitionNumber
      `Prelude.hashWithSalt` spreadDomain
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` tenancy
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` hostResourceGroupArn

instance Prelude.NFData LaunchTemplatePlacement where
  rnf LaunchTemplatePlacement' {..} =
    Prelude.rnf affinity
      `Prelude.seq` Prelude.rnf hostId
      `Prelude.seq` Prelude.rnf partitionNumber
      `Prelude.seq` Prelude.rnf spreadDomain
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf tenancy
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf hostResourceGroupArn
