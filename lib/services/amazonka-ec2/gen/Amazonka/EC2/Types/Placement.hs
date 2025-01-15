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
-- Module      : Amazonka.EC2.Types.Placement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Placement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tenancy
import qualified Amazonka.Prelude as Prelude

-- | Describes the placement of an instance.
--
-- /See:/ 'newPlacement' smart constructor.
data Placement = Placement'
  { -- | The affinity setting for the instance on the Dedicated Host.
    --
    -- This parameter is not supported for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>
    -- or
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>.
    affinity :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone of the instance.
    --
    -- If not specified, an Availability Zone will be automatically chosen for
    -- you based on the load balancing criteria for the Region.
    --
    -- This parameter is not supported for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The ID of the placement group that the instance is in. If you specify
    -- @GroupId@, you can\'t specify @GroupName@.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the placement group that the instance is in. If you specify
    -- @GroupName@, you can\'t specify @GroupId@.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Dedicated Host on which the instance resides.
    --
    -- This parameter is not supported for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>
    -- or
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>.
    hostId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the host resource group in which to launch the instances.
    --
    -- If you specify this parameter, either omit the __Tenancy__ parameter or
    -- set it to @host@.
    --
    -- This parameter is not supported for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    hostResourceGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The number of the partition that the instance is in. Valid only if the
    -- placement group strategy is set to @partition@.
    --
    -- This parameter is not supported for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    partitionNumber :: Prelude.Maybe Prelude.Int,
    -- | Reserved for future use.
    spreadDomain :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance (if the instance is running in a VPC). An
    -- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
    --
    -- This parameter is not supported for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    -- The @host@ tenancy is not supported for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
    -- or for T3 instances that are configured for the @unlimited@ CPU credit
    -- option.
    tenancy :: Prelude.Maybe Tenancy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Placement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'affinity', 'placement_affinity' - The affinity setting for the instance on the Dedicated Host.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>
-- or
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>.
--
-- 'availabilityZone', 'placement_availabilityZone' - The Availability Zone of the instance.
--
-- If not specified, an Availability Zone will be automatically chosen for
-- you based on the load balancing criteria for the Region.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'groupId', 'placement_groupId' - The ID of the placement group that the instance is in. If you specify
-- @GroupId@, you can\'t specify @GroupName@.
--
-- 'groupName', 'placement_groupName' - The name of the placement group that the instance is in. If you specify
-- @GroupName@, you can\'t specify @GroupId@.
--
-- 'hostId', 'placement_hostId' - The ID of the Dedicated Host on which the instance resides.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>
-- or
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>.
--
-- 'hostResourceGroupArn', 'placement_hostResourceGroupArn' - The ARN of the host resource group in which to launch the instances.
--
-- If you specify this parameter, either omit the __Tenancy__ parameter or
-- set it to @host@.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'partitionNumber', 'placement_partitionNumber' - The number of the partition that the instance is in. Valid only if the
-- placement group strategy is set to @partition@.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'spreadDomain', 'placement_spreadDomain' - Reserved for future use.
--
-- 'tenancy', 'placement_tenancy' - The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
-- The @host@ tenancy is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- or for T3 instances that are configured for the @unlimited@ CPU credit
-- option.
newPlacement ::
  Placement
newPlacement =
  Placement'
    { affinity = Prelude.Nothing,
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
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>
-- or
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>.
placement_affinity :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_affinity = Lens.lens (\Placement' {affinity} -> affinity) (\s@Placement' {} a -> s {affinity = a} :: Placement)

-- | The Availability Zone of the instance.
--
-- If not specified, an Availability Zone will be automatically chosen for
-- you based on the load balancing criteria for the Region.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_availabilityZone :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_availabilityZone = Lens.lens (\Placement' {availabilityZone} -> availabilityZone) (\s@Placement' {} a -> s {availabilityZone = a} :: Placement)

-- | The ID of the placement group that the instance is in. If you specify
-- @GroupId@, you can\'t specify @GroupName@.
placement_groupId :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_groupId = Lens.lens (\Placement' {groupId} -> groupId) (\s@Placement' {} a -> s {groupId = a} :: Placement)

-- | The name of the placement group that the instance is in. If you specify
-- @GroupName@, you can\'t specify @GroupId@.
placement_groupName :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_groupName = Lens.lens (\Placement' {groupName} -> groupName) (\s@Placement' {} a -> s {groupName = a} :: Placement)

-- | The ID of the Dedicated Host on which the instance resides.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>
-- or
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>.
placement_hostId :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_hostId = Lens.lens (\Placement' {hostId} -> hostId) (\s@Placement' {} a -> s {hostId = a} :: Placement)

-- | The ARN of the host resource group in which to launch the instances.
--
-- If you specify this parameter, either omit the __Tenancy__ parameter or
-- set it to @host@.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_hostResourceGroupArn :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_hostResourceGroupArn = Lens.lens (\Placement' {hostResourceGroupArn} -> hostResourceGroupArn) (\s@Placement' {} a -> s {hostResourceGroupArn = a} :: Placement)

-- | The number of the partition that the instance is in. Valid only if the
-- placement group strategy is set to @partition@.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_partitionNumber :: Lens.Lens' Placement (Prelude.Maybe Prelude.Int)
placement_partitionNumber = Lens.lens (\Placement' {partitionNumber} -> partitionNumber) (\s@Placement' {} a -> s {partitionNumber = a} :: Placement)

-- | Reserved for future use.
placement_spreadDomain :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_spreadDomain = Lens.lens (\Placement' {spreadDomain} -> spreadDomain) (\s@Placement' {} a -> s {spreadDomain = a} :: Placement)

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
--
-- This parameter is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
-- The @host@ tenancy is not supported for
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- or for T3 instances that are configured for the @unlimited@ CPU credit
-- option.
placement_tenancy :: Lens.Lens' Placement (Prelude.Maybe Tenancy)
placement_tenancy = Lens.lens (\Placement' {tenancy} -> tenancy) (\s@Placement' {} a -> s {tenancy = a} :: Placement)

instance Data.FromXML Placement where
  parseXML x =
    Placement'
      Prelude.<$> (x Data..@? "affinity")
      Prelude.<*> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "groupId")
      Prelude.<*> (x Data..@? "groupName")
      Prelude.<*> (x Data..@? "hostId")
      Prelude.<*> (x Data..@? "hostResourceGroupArn")
      Prelude.<*> (x Data..@? "partitionNumber")
      Prelude.<*> (x Data..@? "spreadDomain")
      Prelude.<*> (x Data..@? "tenancy")

instance Prelude.Hashable Placement where
  hashWithSalt _salt Placement' {..} =
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

instance Prelude.NFData Placement where
  rnf Placement' {..} =
    Prelude.rnf affinity `Prelude.seq`
      Prelude.rnf availabilityZone `Prelude.seq`
        Prelude.rnf groupId `Prelude.seq`
          Prelude.rnf groupName `Prelude.seq`
            Prelude.rnf hostId `Prelude.seq`
              Prelude.rnf hostResourceGroupArn `Prelude.seq`
                Prelude.rnf partitionNumber `Prelude.seq`
                  Prelude.rnf spreadDomain `Prelude.seq`
                    Prelude.rnf tenancy

instance Data.ToQuery Placement where
  toQuery Placement' {..} =
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
