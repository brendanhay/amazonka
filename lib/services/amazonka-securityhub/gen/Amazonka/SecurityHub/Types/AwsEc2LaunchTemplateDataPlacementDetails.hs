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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataPlacementDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataPlacementDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the placement of an Amazon EC2 instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataPlacementDetails' smart constructor.
data AwsEc2LaunchTemplateDataPlacementDetails = AwsEc2LaunchTemplateDataPlacementDetails'
  { -- | The affinity setting for an instance on an EC2 Dedicated Host.
    affinity :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone for the instance.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The name of the placement group for the instance.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Dedicated Host for the instance.
    hostId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the host resource group in which to
    -- launch the instances.
    hostResourceGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The number of the partition the instance should launch in.
    partitionNumber :: Prelude.Maybe Prelude.Int,
    -- | Reserved for future use.
    spreadDomain :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance (if the instance is running in a VPC). An
    -- instance with a tenancy of dedicated runs on single-tenant hardware.
    tenancy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataPlacementDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'affinity', 'awsEc2LaunchTemplateDataPlacementDetails_affinity' - The affinity setting for an instance on an EC2 Dedicated Host.
--
-- 'availabilityZone', 'awsEc2LaunchTemplateDataPlacementDetails_availabilityZone' - The Availability Zone for the instance.
--
-- 'groupName', 'awsEc2LaunchTemplateDataPlacementDetails_groupName' - The name of the placement group for the instance.
--
-- 'hostId', 'awsEc2LaunchTemplateDataPlacementDetails_hostId' - The ID of the Dedicated Host for the instance.
--
-- 'hostResourceGroupArn', 'awsEc2LaunchTemplateDataPlacementDetails_hostResourceGroupArn' - The Amazon Resource Name (ARN) of the host resource group in which to
-- launch the instances.
--
-- 'partitionNumber', 'awsEc2LaunchTemplateDataPlacementDetails_partitionNumber' - The number of the partition the instance should launch in.
--
-- 'spreadDomain', 'awsEc2LaunchTemplateDataPlacementDetails_spreadDomain' - Reserved for future use.
--
-- 'tenancy', 'awsEc2LaunchTemplateDataPlacementDetails_tenancy' - The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of dedicated runs on single-tenant hardware.
newAwsEc2LaunchTemplateDataPlacementDetails ::
  AwsEc2LaunchTemplateDataPlacementDetails
newAwsEc2LaunchTemplateDataPlacementDetails =
  AwsEc2LaunchTemplateDataPlacementDetails'
    { affinity =
        Prelude.Nothing,
      availabilityZone =
        Prelude.Nothing,
      groupName = Prelude.Nothing,
      hostId = Prelude.Nothing,
      hostResourceGroupArn =
        Prelude.Nothing,
      partitionNumber = Prelude.Nothing,
      spreadDomain = Prelude.Nothing,
      tenancy = Prelude.Nothing
    }

-- | The affinity setting for an instance on an EC2 Dedicated Host.
awsEc2LaunchTemplateDataPlacementDetails_affinity :: Lens.Lens' AwsEc2LaunchTemplateDataPlacementDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataPlacementDetails_affinity = Lens.lens (\AwsEc2LaunchTemplateDataPlacementDetails' {affinity} -> affinity) (\s@AwsEc2LaunchTemplateDataPlacementDetails' {} a -> s {affinity = a} :: AwsEc2LaunchTemplateDataPlacementDetails)

-- | The Availability Zone for the instance.
awsEc2LaunchTemplateDataPlacementDetails_availabilityZone :: Lens.Lens' AwsEc2LaunchTemplateDataPlacementDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataPlacementDetails_availabilityZone = Lens.lens (\AwsEc2LaunchTemplateDataPlacementDetails' {availabilityZone} -> availabilityZone) (\s@AwsEc2LaunchTemplateDataPlacementDetails' {} a -> s {availabilityZone = a} :: AwsEc2LaunchTemplateDataPlacementDetails)

-- | The name of the placement group for the instance.
awsEc2LaunchTemplateDataPlacementDetails_groupName :: Lens.Lens' AwsEc2LaunchTemplateDataPlacementDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataPlacementDetails_groupName = Lens.lens (\AwsEc2LaunchTemplateDataPlacementDetails' {groupName} -> groupName) (\s@AwsEc2LaunchTemplateDataPlacementDetails' {} a -> s {groupName = a} :: AwsEc2LaunchTemplateDataPlacementDetails)

-- | The ID of the Dedicated Host for the instance.
awsEc2LaunchTemplateDataPlacementDetails_hostId :: Lens.Lens' AwsEc2LaunchTemplateDataPlacementDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataPlacementDetails_hostId = Lens.lens (\AwsEc2LaunchTemplateDataPlacementDetails' {hostId} -> hostId) (\s@AwsEc2LaunchTemplateDataPlacementDetails' {} a -> s {hostId = a} :: AwsEc2LaunchTemplateDataPlacementDetails)

-- | The Amazon Resource Name (ARN) of the host resource group in which to
-- launch the instances.
awsEc2LaunchTemplateDataPlacementDetails_hostResourceGroupArn :: Lens.Lens' AwsEc2LaunchTemplateDataPlacementDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataPlacementDetails_hostResourceGroupArn = Lens.lens (\AwsEc2LaunchTemplateDataPlacementDetails' {hostResourceGroupArn} -> hostResourceGroupArn) (\s@AwsEc2LaunchTemplateDataPlacementDetails' {} a -> s {hostResourceGroupArn = a} :: AwsEc2LaunchTemplateDataPlacementDetails)

-- | The number of the partition the instance should launch in.
awsEc2LaunchTemplateDataPlacementDetails_partitionNumber :: Lens.Lens' AwsEc2LaunchTemplateDataPlacementDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataPlacementDetails_partitionNumber = Lens.lens (\AwsEc2LaunchTemplateDataPlacementDetails' {partitionNumber} -> partitionNumber) (\s@AwsEc2LaunchTemplateDataPlacementDetails' {} a -> s {partitionNumber = a} :: AwsEc2LaunchTemplateDataPlacementDetails)

-- | Reserved for future use.
awsEc2LaunchTemplateDataPlacementDetails_spreadDomain :: Lens.Lens' AwsEc2LaunchTemplateDataPlacementDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataPlacementDetails_spreadDomain = Lens.lens (\AwsEc2LaunchTemplateDataPlacementDetails' {spreadDomain} -> spreadDomain) (\s@AwsEc2LaunchTemplateDataPlacementDetails' {} a -> s {spreadDomain = a} :: AwsEc2LaunchTemplateDataPlacementDetails)

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of dedicated runs on single-tenant hardware.
awsEc2LaunchTemplateDataPlacementDetails_tenancy :: Lens.Lens' AwsEc2LaunchTemplateDataPlacementDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataPlacementDetails_tenancy = Lens.lens (\AwsEc2LaunchTemplateDataPlacementDetails' {tenancy} -> tenancy) (\s@AwsEc2LaunchTemplateDataPlacementDetails' {} a -> s {tenancy = a} :: AwsEc2LaunchTemplateDataPlacementDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataPlacementDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataPlacementDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataPlacementDetails'
            Prelude.<$> (x Data..:? "Affinity")
            Prelude.<*> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "GroupName")
            Prelude.<*> (x Data..:? "HostId")
            Prelude.<*> (x Data..:? "HostResourceGroupArn")
            Prelude.<*> (x Data..:? "PartitionNumber")
            Prelude.<*> (x Data..:? "SpreadDomain")
            Prelude.<*> (x Data..:? "Tenancy")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataPlacementDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataPlacementDetails' {..} =
      _salt
        `Prelude.hashWithSalt` affinity
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` groupName
        `Prelude.hashWithSalt` hostId
        `Prelude.hashWithSalt` hostResourceGroupArn
        `Prelude.hashWithSalt` partitionNumber
        `Prelude.hashWithSalt` spreadDomain
        `Prelude.hashWithSalt` tenancy

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataPlacementDetails
  where
  rnf AwsEc2LaunchTemplateDataPlacementDetails' {..} =
    Prelude.rnf affinity
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf hostId
      `Prelude.seq` Prelude.rnf hostResourceGroupArn
      `Prelude.seq` Prelude.rnf partitionNumber
      `Prelude.seq` Prelude.rnf spreadDomain
      `Prelude.seq` Prelude.rnf tenancy

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataPlacementDetails
  where
  toJSON AwsEc2LaunchTemplateDataPlacementDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Affinity" Data..=) Prelude.<$> affinity,
            ("AvailabilityZone" Data..=)
              Prelude.<$> availabilityZone,
            ("GroupName" Data..=) Prelude.<$> groupName,
            ("HostId" Data..=) Prelude.<$> hostId,
            ("HostResourceGroupArn" Data..=)
              Prelude.<$> hostResourceGroupArn,
            ("PartitionNumber" Data..=)
              Prelude.<$> partitionNumber,
            ("SpreadDomain" Data..=) Prelude.<$> spreadDomain,
            ("Tenancy" Data..=) Prelude.<$> tenancy
          ]
      )
