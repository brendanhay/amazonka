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
-- Module      : Network.AWS.EC2.Types.Placement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Placement where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tenancy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the placement of an instance.
--
-- /See:/ 'newPlacement' smart constructor.
data Placement = Placement'
  { -- | Reserved for future use.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    spreadDomain :: Prelude.Maybe Prelude.Text,
    -- | The name of the placement group the instance is in.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance (if the instance is running in a VPC). An
    -- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
    -- The @host@ tenancy is not supported for the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
    -- command.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    tenancy :: Prelude.Maybe Tenancy,
    -- | The affinity setting for the instance on the Dedicated Host. This
    -- parameter is not supported for the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
    -- command.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    affinity :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone of the instance.
    --
    -- If not specified, an Availability Zone will be automatically chosen for
    -- you based on the load balancing criteria for the Region.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The number of the partition the instance is in. Valid only if the
    -- placement group strategy is set to @partition@.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    partitionNumber :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the host resource group in which to launch the instances. If
    -- you specify a host resource group ARN, omit the __Tenancy__ parameter or
    -- set it to @host@.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    hostResourceGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Dedicated Host on which the instance resides. This
    -- parameter is not supported for the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
    -- command.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    hostId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Placement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spreadDomain', 'placement_spreadDomain' - Reserved for future use.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'groupName', 'placement_groupName' - The name of the placement group the instance is in.
--
-- 'tenancy', 'placement_tenancy' - The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
-- The @host@ tenancy is not supported for the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- command.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'affinity', 'placement_affinity' - The affinity setting for the instance on the Dedicated Host. This
-- parameter is not supported for the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- command.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'availabilityZone', 'placement_availabilityZone' - The Availability Zone of the instance.
--
-- If not specified, an Availability Zone will be automatically chosen for
-- you based on the load balancing criteria for the Region.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'partitionNumber', 'placement_partitionNumber' - The number of the partition the instance is in. Valid only if the
-- placement group strategy is set to @partition@.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'hostResourceGroupArn', 'placement_hostResourceGroupArn' - The ARN of the host resource group in which to launch the instances. If
-- you specify a host resource group ARN, omit the __Tenancy__ parameter or
-- set it to @host@.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'hostId', 'placement_hostId' - The ID of the Dedicated Host on which the instance resides. This
-- parameter is not supported for the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- command.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
newPlacement ::
  Placement
newPlacement =
  Placement'
    { spreadDomain = Prelude.Nothing,
      groupName = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      affinity = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      partitionNumber = Prelude.Nothing,
      hostResourceGroupArn = Prelude.Nothing,
      hostId = Prelude.Nothing
    }

-- | Reserved for future use.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_spreadDomain :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_spreadDomain = Lens.lens (\Placement' {spreadDomain} -> spreadDomain) (\s@Placement' {} a -> s {spreadDomain = a} :: Placement)

-- | The name of the placement group the instance is in.
placement_groupName :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_groupName = Lens.lens (\Placement' {groupName} -> groupName) (\s@Placement' {} a -> s {groupName = a} :: Placement)

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
-- The @host@ tenancy is not supported for the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- command.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_tenancy :: Lens.Lens' Placement (Prelude.Maybe Tenancy)
placement_tenancy = Lens.lens (\Placement' {tenancy} -> tenancy) (\s@Placement' {} a -> s {tenancy = a} :: Placement)

-- | The affinity setting for the instance on the Dedicated Host. This
-- parameter is not supported for the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- command.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_affinity :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_affinity = Lens.lens (\Placement' {affinity} -> affinity) (\s@Placement' {} a -> s {affinity = a} :: Placement)

-- | The Availability Zone of the instance.
--
-- If not specified, an Availability Zone will be automatically chosen for
-- you based on the load balancing criteria for the Region.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_availabilityZone :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_availabilityZone = Lens.lens (\Placement' {availabilityZone} -> availabilityZone) (\s@Placement' {} a -> s {availabilityZone = a} :: Placement)

-- | The number of the partition the instance is in. Valid only if the
-- placement group strategy is set to @partition@.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_partitionNumber :: Lens.Lens' Placement (Prelude.Maybe Prelude.Int)
placement_partitionNumber = Lens.lens (\Placement' {partitionNumber} -> partitionNumber) (\s@Placement' {} a -> s {partitionNumber = a} :: Placement)

-- | The ARN of the host resource group in which to launch the instances. If
-- you specify a host resource group ARN, omit the __Tenancy__ parameter or
-- set it to @host@.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_hostResourceGroupArn :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_hostResourceGroupArn = Lens.lens (\Placement' {hostResourceGroupArn} -> hostResourceGroupArn) (\s@Placement' {} a -> s {hostResourceGroupArn = a} :: Placement)

-- | The ID of the Dedicated Host on which the instance resides. This
-- parameter is not supported for the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- command.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_hostId :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_hostId = Lens.lens (\Placement' {hostId} -> hostId) (\s@Placement' {} a -> s {hostId = a} :: Placement)

instance Prelude.FromXML Placement where
  parseXML x =
    Placement'
      Prelude.<$> (x Prelude..@? "spreadDomain")
      Prelude.<*> (x Prelude..@? "groupName")
      Prelude.<*> (x Prelude..@? "tenancy")
      Prelude.<*> (x Prelude..@? "affinity")
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> (x Prelude..@? "partitionNumber")
      Prelude.<*> (x Prelude..@? "hostResourceGroupArn")
      Prelude.<*> (x Prelude..@? "hostId")

instance Prelude.Hashable Placement

instance Prelude.NFData Placement

instance Prelude.ToQuery Placement where
  toQuery Placement' {..} =
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
