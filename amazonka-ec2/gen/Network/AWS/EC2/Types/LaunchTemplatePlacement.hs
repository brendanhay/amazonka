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
-- Module      : Network.AWS.EC2.Types.LaunchTemplatePlacement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatePlacement where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tenancy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the placement of an instance.
--
-- /See:/ 'newLaunchTemplatePlacement' smart constructor.
data LaunchTemplatePlacement = LaunchTemplatePlacement'
  { -- | Reserved for future use.
    spreadDomain :: Prelude.Maybe Prelude.Text,
    -- | The name of the placement group for the instance.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance (if the instance is running in a VPC). An
    -- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
    tenancy :: Prelude.Maybe Tenancy,
    -- | The affinity setting for the instance on the Dedicated Host.
    affinity :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone of the instance.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The number of the partition the instance should launch in. Valid only if
    -- the placement group strategy is set to @partition@.
    partitionNumber :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the host resource group in which to launch the instances.
    hostResourceGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Dedicated Host for the instance.
    hostId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplatePlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spreadDomain', 'launchTemplatePlacement_spreadDomain' - Reserved for future use.
--
-- 'groupName', 'launchTemplatePlacement_groupName' - The name of the placement group for the instance.
--
-- 'tenancy', 'launchTemplatePlacement_tenancy' - The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
--
-- 'affinity', 'launchTemplatePlacement_affinity' - The affinity setting for the instance on the Dedicated Host.
--
-- 'availabilityZone', 'launchTemplatePlacement_availabilityZone' - The Availability Zone of the instance.
--
-- 'partitionNumber', 'launchTemplatePlacement_partitionNumber' - The number of the partition the instance should launch in. Valid only if
-- the placement group strategy is set to @partition@.
--
-- 'hostResourceGroupArn', 'launchTemplatePlacement_hostResourceGroupArn' - The ARN of the host resource group in which to launch the instances.
--
-- 'hostId', 'launchTemplatePlacement_hostId' - The ID of the Dedicated Host for the instance.
newLaunchTemplatePlacement ::
  LaunchTemplatePlacement
newLaunchTemplatePlacement =
  LaunchTemplatePlacement'
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
launchTemplatePlacement_spreadDomain :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_spreadDomain = Lens.lens (\LaunchTemplatePlacement' {spreadDomain} -> spreadDomain) (\s@LaunchTemplatePlacement' {} a -> s {spreadDomain = a} :: LaunchTemplatePlacement)

-- | The name of the placement group for the instance.
launchTemplatePlacement_groupName :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_groupName = Lens.lens (\LaunchTemplatePlacement' {groupName} -> groupName) (\s@LaunchTemplatePlacement' {} a -> s {groupName = a} :: LaunchTemplatePlacement)

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
launchTemplatePlacement_tenancy :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Tenancy)
launchTemplatePlacement_tenancy = Lens.lens (\LaunchTemplatePlacement' {tenancy} -> tenancy) (\s@LaunchTemplatePlacement' {} a -> s {tenancy = a} :: LaunchTemplatePlacement)

-- | The affinity setting for the instance on the Dedicated Host.
launchTemplatePlacement_affinity :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_affinity = Lens.lens (\LaunchTemplatePlacement' {affinity} -> affinity) (\s@LaunchTemplatePlacement' {} a -> s {affinity = a} :: LaunchTemplatePlacement)

-- | The Availability Zone of the instance.
launchTemplatePlacement_availabilityZone :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_availabilityZone = Lens.lens (\LaunchTemplatePlacement' {availabilityZone} -> availabilityZone) (\s@LaunchTemplatePlacement' {} a -> s {availabilityZone = a} :: LaunchTemplatePlacement)

-- | The number of the partition the instance should launch in. Valid only if
-- the placement group strategy is set to @partition@.
launchTemplatePlacement_partitionNumber :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Int)
launchTemplatePlacement_partitionNumber = Lens.lens (\LaunchTemplatePlacement' {partitionNumber} -> partitionNumber) (\s@LaunchTemplatePlacement' {} a -> s {partitionNumber = a} :: LaunchTemplatePlacement)

-- | The ARN of the host resource group in which to launch the instances.
launchTemplatePlacement_hostResourceGroupArn :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_hostResourceGroupArn = Lens.lens (\LaunchTemplatePlacement' {hostResourceGroupArn} -> hostResourceGroupArn) (\s@LaunchTemplatePlacement' {} a -> s {hostResourceGroupArn = a} :: LaunchTemplatePlacement)

-- | The ID of the Dedicated Host for the instance.
launchTemplatePlacement_hostId :: Lens.Lens' LaunchTemplatePlacement (Prelude.Maybe Prelude.Text)
launchTemplatePlacement_hostId = Lens.lens (\LaunchTemplatePlacement' {hostId} -> hostId) (\s@LaunchTemplatePlacement' {} a -> s {hostId = a} :: LaunchTemplatePlacement)

instance Prelude.FromXML LaunchTemplatePlacement where
  parseXML x =
    LaunchTemplatePlacement'
      Prelude.<$> (x Prelude..@? "spreadDomain")
      Prelude.<*> (x Prelude..@? "groupName")
      Prelude.<*> (x Prelude..@? "tenancy")
      Prelude.<*> (x Prelude..@? "affinity")
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> (x Prelude..@? "partitionNumber")
      Prelude.<*> (x Prelude..@? "hostResourceGroupArn")
      Prelude.<*> (x Prelude..@? "hostId")

instance Prelude.Hashable LaunchTemplatePlacement

instance Prelude.NFData LaunchTemplatePlacement
