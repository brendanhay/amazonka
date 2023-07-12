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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2SubnetDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2SubnetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.Ipv6CidrBlockAssociation

-- | Contains information about a subnet in Amazon EC2.
--
-- /See:/ 'newAwsEc2SubnetDetails' smart constructor.
data AwsEc2SubnetDetails = AwsEc2SubnetDetails'
  { -- | Whether to assign an IPV6 address to a network interface that is created
    -- in this subnet.
    assignIpv6AddressOnCreation :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone for the subnet.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Availability Zone for the subnet.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The number of available IPV4 addresses in the subnet. Does not include
    -- addresses for stopped instances.
    availableIpAddressCount :: Prelude.Maybe Prelude.Int,
    -- | The IPV4 CIDR block that is assigned to the subnet.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Whether this subnet is the default subnet for the Availability Zone.
    defaultForAz :: Prelude.Maybe Prelude.Bool,
    -- | The IPV6 CIDR blocks that are associated with the subnet.
    ipv6CidrBlockAssociationSet :: Prelude.Maybe [Ipv6CidrBlockAssociation],
    -- | Whether instances in this subnet receive a public IP address.
    mapPublicIpOnLaunch :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the Amazon Web Services account that owns the subnet.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the subnet. Valid values are @available@ or
    -- @pending@.
    state :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the subnet.
    subnetArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPC that contains the subnet.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2SubnetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignIpv6AddressOnCreation', 'awsEc2SubnetDetails_assignIpv6AddressOnCreation' - Whether to assign an IPV6 address to a network interface that is created
-- in this subnet.
--
-- 'availabilityZone', 'awsEc2SubnetDetails_availabilityZone' - The Availability Zone for the subnet.
--
-- 'availabilityZoneId', 'awsEc2SubnetDetails_availabilityZoneId' - The identifier of the Availability Zone for the subnet.
--
-- 'availableIpAddressCount', 'awsEc2SubnetDetails_availableIpAddressCount' - The number of available IPV4 addresses in the subnet. Does not include
-- addresses for stopped instances.
--
-- 'cidrBlock', 'awsEc2SubnetDetails_cidrBlock' - The IPV4 CIDR block that is assigned to the subnet.
--
-- 'defaultForAz', 'awsEc2SubnetDetails_defaultForAz' - Whether this subnet is the default subnet for the Availability Zone.
--
-- 'ipv6CidrBlockAssociationSet', 'awsEc2SubnetDetails_ipv6CidrBlockAssociationSet' - The IPV6 CIDR blocks that are associated with the subnet.
--
-- 'mapPublicIpOnLaunch', 'awsEc2SubnetDetails_mapPublicIpOnLaunch' - Whether instances in this subnet receive a public IP address.
--
-- 'ownerId', 'awsEc2SubnetDetails_ownerId' - The identifier of the Amazon Web Services account that owns the subnet.
--
-- 'state', 'awsEc2SubnetDetails_state' - The current state of the subnet. Valid values are @available@ or
-- @pending@.
--
-- 'subnetArn', 'awsEc2SubnetDetails_subnetArn' - The ARN of the subnet.
--
-- 'subnetId', 'awsEc2SubnetDetails_subnetId' - The identifier of the subnet.
--
-- 'vpcId', 'awsEc2SubnetDetails_vpcId' - The identifier of the VPC that contains the subnet.
newAwsEc2SubnetDetails ::
  AwsEc2SubnetDetails
newAwsEc2SubnetDetails =
  AwsEc2SubnetDetails'
    { assignIpv6AddressOnCreation =
        Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      availabilityZoneId = Prelude.Nothing,
      availableIpAddressCount = Prelude.Nothing,
      cidrBlock = Prelude.Nothing,
      defaultForAz = Prelude.Nothing,
      ipv6CidrBlockAssociationSet = Prelude.Nothing,
      mapPublicIpOnLaunch = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      state = Prelude.Nothing,
      subnetArn = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | Whether to assign an IPV6 address to a network interface that is created
-- in this subnet.
awsEc2SubnetDetails_assignIpv6AddressOnCreation :: Lens.Lens' AwsEc2SubnetDetails (Prelude.Maybe Prelude.Bool)
awsEc2SubnetDetails_assignIpv6AddressOnCreation = Lens.lens (\AwsEc2SubnetDetails' {assignIpv6AddressOnCreation} -> assignIpv6AddressOnCreation) (\s@AwsEc2SubnetDetails' {} a -> s {assignIpv6AddressOnCreation = a} :: AwsEc2SubnetDetails)

-- | The Availability Zone for the subnet.
awsEc2SubnetDetails_availabilityZone :: Lens.Lens' AwsEc2SubnetDetails (Prelude.Maybe Prelude.Text)
awsEc2SubnetDetails_availabilityZone = Lens.lens (\AwsEc2SubnetDetails' {availabilityZone} -> availabilityZone) (\s@AwsEc2SubnetDetails' {} a -> s {availabilityZone = a} :: AwsEc2SubnetDetails)

-- | The identifier of the Availability Zone for the subnet.
awsEc2SubnetDetails_availabilityZoneId :: Lens.Lens' AwsEc2SubnetDetails (Prelude.Maybe Prelude.Text)
awsEc2SubnetDetails_availabilityZoneId = Lens.lens (\AwsEc2SubnetDetails' {availabilityZoneId} -> availabilityZoneId) (\s@AwsEc2SubnetDetails' {} a -> s {availabilityZoneId = a} :: AwsEc2SubnetDetails)

-- | The number of available IPV4 addresses in the subnet. Does not include
-- addresses for stopped instances.
awsEc2SubnetDetails_availableIpAddressCount :: Lens.Lens' AwsEc2SubnetDetails (Prelude.Maybe Prelude.Int)
awsEc2SubnetDetails_availableIpAddressCount = Lens.lens (\AwsEc2SubnetDetails' {availableIpAddressCount} -> availableIpAddressCount) (\s@AwsEc2SubnetDetails' {} a -> s {availableIpAddressCount = a} :: AwsEc2SubnetDetails)

-- | The IPV4 CIDR block that is assigned to the subnet.
awsEc2SubnetDetails_cidrBlock :: Lens.Lens' AwsEc2SubnetDetails (Prelude.Maybe Prelude.Text)
awsEc2SubnetDetails_cidrBlock = Lens.lens (\AwsEc2SubnetDetails' {cidrBlock} -> cidrBlock) (\s@AwsEc2SubnetDetails' {} a -> s {cidrBlock = a} :: AwsEc2SubnetDetails)

-- | Whether this subnet is the default subnet for the Availability Zone.
awsEc2SubnetDetails_defaultForAz :: Lens.Lens' AwsEc2SubnetDetails (Prelude.Maybe Prelude.Bool)
awsEc2SubnetDetails_defaultForAz = Lens.lens (\AwsEc2SubnetDetails' {defaultForAz} -> defaultForAz) (\s@AwsEc2SubnetDetails' {} a -> s {defaultForAz = a} :: AwsEc2SubnetDetails)

-- | The IPV6 CIDR blocks that are associated with the subnet.
awsEc2SubnetDetails_ipv6CidrBlockAssociationSet :: Lens.Lens' AwsEc2SubnetDetails (Prelude.Maybe [Ipv6CidrBlockAssociation])
awsEc2SubnetDetails_ipv6CidrBlockAssociationSet = Lens.lens (\AwsEc2SubnetDetails' {ipv6CidrBlockAssociationSet} -> ipv6CidrBlockAssociationSet) (\s@AwsEc2SubnetDetails' {} a -> s {ipv6CidrBlockAssociationSet = a} :: AwsEc2SubnetDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether instances in this subnet receive a public IP address.
awsEc2SubnetDetails_mapPublicIpOnLaunch :: Lens.Lens' AwsEc2SubnetDetails (Prelude.Maybe Prelude.Bool)
awsEc2SubnetDetails_mapPublicIpOnLaunch = Lens.lens (\AwsEc2SubnetDetails' {mapPublicIpOnLaunch} -> mapPublicIpOnLaunch) (\s@AwsEc2SubnetDetails' {} a -> s {mapPublicIpOnLaunch = a} :: AwsEc2SubnetDetails)

-- | The identifier of the Amazon Web Services account that owns the subnet.
awsEc2SubnetDetails_ownerId :: Lens.Lens' AwsEc2SubnetDetails (Prelude.Maybe Prelude.Text)
awsEc2SubnetDetails_ownerId = Lens.lens (\AwsEc2SubnetDetails' {ownerId} -> ownerId) (\s@AwsEc2SubnetDetails' {} a -> s {ownerId = a} :: AwsEc2SubnetDetails)

-- | The current state of the subnet. Valid values are @available@ or
-- @pending@.
awsEc2SubnetDetails_state :: Lens.Lens' AwsEc2SubnetDetails (Prelude.Maybe Prelude.Text)
awsEc2SubnetDetails_state = Lens.lens (\AwsEc2SubnetDetails' {state} -> state) (\s@AwsEc2SubnetDetails' {} a -> s {state = a} :: AwsEc2SubnetDetails)

-- | The ARN of the subnet.
awsEc2SubnetDetails_subnetArn :: Lens.Lens' AwsEc2SubnetDetails (Prelude.Maybe Prelude.Text)
awsEc2SubnetDetails_subnetArn = Lens.lens (\AwsEc2SubnetDetails' {subnetArn} -> subnetArn) (\s@AwsEc2SubnetDetails' {} a -> s {subnetArn = a} :: AwsEc2SubnetDetails)

-- | The identifier of the subnet.
awsEc2SubnetDetails_subnetId :: Lens.Lens' AwsEc2SubnetDetails (Prelude.Maybe Prelude.Text)
awsEc2SubnetDetails_subnetId = Lens.lens (\AwsEc2SubnetDetails' {subnetId} -> subnetId) (\s@AwsEc2SubnetDetails' {} a -> s {subnetId = a} :: AwsEc2SubnetDetails)

-- | The identifier of the VPC that contains the subnet.
awsEc2SubnetDetails_vpcId :: Lens.Lens' AwsEc2SubnetDetails (Prelude.Maybe Prelude.Text)
awsEc2SubnetDetails_vpcId = Lens.lens (\AwsEc2SubnetDetails' {vpcId} -> vpcId) (\s@AwsEc2SubnetDetails' {} a -> s {vpcId = a} :: AwsEc2SubnetDetails)

instance Data.FromJSON AwsEc2SubnetDetails where
  parseJSON =
    Data.withObject
      "AwsEc2SubnetDetails"
      ( \x ->
          AwsEc2SubnetDetails'
            Prelude.<$> (x Data..:? "AssignIpv6AddressOnCreation")
            Prelude.<*> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "AvailabilityZoneId")
            Prelude.<*> (x Data..:? "AvailableIpAddressCount")
            Prelude.<*> (x Data..:? "CidrBlock")
            Prelude.<*> (x Data..:? "DefaultForAz")
            Prelude.<*> ( x
                            Data..:? "Ipv6CidrBlockAssociationSet"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "MapPublicIpOnLaunch")
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "SubnetArn")
            Prelude.<*> (x Data..:? "SubnetId")
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable AwsEc2SubnetDetails where
  hashWithSalt _salt AwsEc2SubnetDetails' {..} =
    _salt
      `Prelude.hashWithSalt` assignIpv6AddressOnCreation
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` availabilityZoneId
      `Prelude.hashWithSalt` availableIpAddressCount
      `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` defaultForAz
      `Prelude.hashWithSalt` ipv6CidrBlockAssociationSet
      `Prelude.hashWithSalt` mapPublicIpOnLaunch
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` subnetArn
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData AwsEc2SubnetDetails where
  rnf AwsEc2SubnetDetails' {..} =
    Prelude.rnf assignIpv6AddressOnCreation
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf availableIpAddressCount
      `Prelude.seq` Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf defaultForAz
      `Prelude.seq` Prelude.rnf ipv6CidrBlockAssociationSet
      `Prelude.seq` Prelude.rnf mapPublicIpOnLaunch
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf subnetArn
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToJSON AwsEc2SubnetDetails where
  toJSON AwsEc2SubnetDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssignIpv6AddressOnCreation" Data..=)
              Prelude.<$> assignIpv6AddressOnCreation,
            ("AvailabilityZone" Data..=)
              Prelude.<$> availabilityZone,
            ("AvailabilityZoneId" Data..=)
              Prelude.<$> availabilityZoneId,
            ("AvailableIpAddressCount" Data..=)
              Prelude.<$> availableIpAddressCount,
            ("CidrBlock" Data..=) Prelude.<$> cidrBlock,
            ("DefaultForAz" Data..=) Prelude.<$> defaultForAz,
            ("Ipv6CidrBlockAssociationSet" Data..=)
              Prelude.<$> ipv6CidrBlockAssociationSet,
            ("MapPublicIpOnLaunch" Data..=)
              Prelude.<$> mapPublicIpOnLaunch,
            ("OwnerId" Data..=) Prelude.<$> ownerId,
            ("State" Data..=) Prelude.<$> state,
            ("SubnetArn" Data..=) Prelude.<$> subnetArn,
            ("SubnetId" Data..=) Prelude.<$> subnetId,
            ("VpcId" Data..=) Prelude.<$> vpcId
          ]
      )
