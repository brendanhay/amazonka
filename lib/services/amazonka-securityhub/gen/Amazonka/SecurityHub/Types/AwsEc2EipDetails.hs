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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2EipDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2EipDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an Elastic IP address.
--
-- /See:/ 'newAwsEc2EipDetails' smart constructor.
data AwsEc2EipDetails = AwsEc2EipDetails'
  { -- | The identifier that Amazon Web Services assigns to represent the
    -- allocation of the Elastic IP address for use with Amazon VPC.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | The name of the location from which the Elastic IP address is
    -- advertised.
    networkBorderGroup :: Prelude.Maybe Prelude.Text,
    -- | The domain in which to allocate the address.
    --
    -- If the address is for use with EC2 instances in a VPC, then @Domain@ is
    -- @vpc@. Otherwise, @Domain@ is @standard@.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the owner of the network
    -- interface.
    networkInterfaceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | A public IP address that is associated with the EC2 instance.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the EC2 instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The private IP address that is associated with the Elastic IP address.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The identifier that represents the association of the Elastic IP address
    -- with an EC2 instance.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of an IP address pool. This parameter allows Amazon EC2
    -- to select an IP address from the address pool.
    publicIpv4Pool :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2EipDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationId', 'awsEc2EipDetails_allocationId' - The identifier that Amazon Web Services assigns to represent the
-- allocation of the Elastic IP address for use with Amazon VPC.
--
-- 'networkBorderGroup', 'awsEc2EipDetails_networkBorderGroup' - The name of the location from which the Elastic IP address is
-- advertised.
--
-- 'domain', 'awsEc2EipDetails_domain' - The domain in which to allocate the address.
--
-- If the address is for use with EC2 instances in a VPC, then @Domain@ is
-- @vpc@. Otherwise, @Domain@ is @standard@.
--
-- 'networkInterfaceOwnerId', 'awsEc2EipDetails_networkInterfaceOwnerId' - The Amazon Web Services account ID of the owner of the network
-- interface.
--
-- 'publicIp', 'awsEc2EipDetails_publicIp' - A public IP address that is associated with the EC2 instance.
--
-- 'instanceId', 'awsEc2EipDetails_instanceId' - The identifier of the EC2 instance.
--
-- 'networkInterfaceId', 'awsEc2EipDetails_networkInterfaceId' - The identifier of the network interface.
--
-- 'privateIpAddress', 'awsEc2EipDetails_privateIpAddress' - The private IP address that is associated with the Elastic IP address.
--
-- 'associationId', 'awsEc2EipDetails_associationId' - The identifier that represents the association of the Elastic IP address
-- with an EC2 instance.
--
-- 'publicIpv4Pool', 'awsEc2EipDetails_publicIpv4Pool' - The identifier of an IP address pool. This parameter allows Amazon EC2
-- to select an IP address from the address pool.
newAwsEc2EipDetails ::
  AwsEc2EipDetails
newAwsEc2EipDetails =
  AwsEc2EipDetails'
    { allocationId = Prelude.Nothing,
      networkBorderGroup = Prelude.Nothing,
      domain = Prelude.Nothing,
      networkInterfaceOwnerId = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      associationId = Prelude.Nothing,
      publicIpv4Pool = Prelude.Nothing
    }

-- | The identifier that Amazon Web Services assigns to represent the
-- allocation of the Elastic IP address for use with Amazon VPC.
awsEc2EipDetails_allocationId :: Lens.Lens' AwsEc2EipDetails (Prelude.Maybe Prelude.Text)
awsEc2EipDetails_allocationId = Lens.lens (\AwsEc2EipDetails' {allocationId} -> allocationId) (\s@AwsEc2EipDetails' {} a -> s {allocationId = a} :: AwsEc2EipDetails)

-- | The name of the location from which the Elastic IP address is
-- advertised.
awsEc2EipDetails_networkBorderGroup :: Lens.Lens' AwsEc2EipDetails (Prelude.Maybe Prelude.Text)
awsEc2EipDetails_networkBorderGroup = Lens.lens (\AwsEc2EipDetails' {networkBorderGroup} -> networkBorderGroup) (\s@AwsEc2EipDetails' {} a -> s {networkBorderGroup = a} :: AwsEc2EipDetails)

-- | The domain in which to allocate the address.
--
-- If the address is for use with EC2 instances in a VPC, then @Domain@ is
-- @vpc@. Otherwise, @Domain@ is @standard@.
awsEc2EipDetails_domain :: Lens.Lens' AwsEc2EipDetails (Prelude.Maybe Prelude.Text)
awsEc2EipDetails_domain = Lens.lens (\AwsEc2EipDetails' {domain} -> domain) (\s@AwsEc2EipDetails' {} a -> s {domain = a} :: AwsEc2EipDetails)

-- | The Amazon Web Services account ID of the owner of the network
-- interface.
awsEc2EipDetails_networkInterfaceOwnerId :: Lens.Lens' AwsEc2EipDetails (Prelude.Maybe Prelude.Text)
awsEc2EipDetails_networkInterfaceOwnerId = Lens.lens (\AwsEc2EipDetails' {networkInterfaceOwnerId} -> networkInterfaceOwnerId) (\s@AwsEc2EipDetails' {} a -> s {networkInterfaceOwnerId = a} :: AwsEc2EipDetails)

-- | A public IP address that is associated with the EC2 instance.
awsEc2EipDetails_publicIp :: Lens.Lens' AwsEc2EipDetails (Prelude.Maybe Prelude.Text)
awsEc2EipDetails_publicIp = Lens.lens (\AwsEc2EipDetails' {publicIp} -> publicIp) (\s@AwsEc2EipDetails' {} a -> s {publicIp = a} :: AwsEc2EipDetails)

-- | The identifier of the EC2 instance.
awsEc2EipDetails_instanceId :: Lens.Lens' AwsEc2EipDetails (Prelude.Maybe Prelude.Text)
awsEc2EipDetails_instanceId = Lens.lens (\AwsEc2EipDetails' {instanceId} -> instanceId) (\s@AwsEc2EipDetails' {} a -> s {instanceId = a} :: AwsEc2EipDetails)

-- | The identifier of the network interface.
awsEc2EipDetails_networkInterfaceId :: Lens.Lens' AwsEc2EipDetails (Prelude.Maybe Prelude.Text)
awsEc2EipDetails_networkInterfaceId = Lens.lens (\AwsEc2EipDetails' {networkInterfaceId} -> networkInterfaceId) (\s@AwsEc2EipDetails' {} a -> s {networkInterfaceId = a} :: AwsEc2EipDetails)

-- | The private IP address that is associated with the Elastic IP address.
awsEc2EipDetails_privateIpAddress :: Lens.Lens' AwsEc2EipDetails (Prelude.Maybe Prelude.Text)
awsEc2EipDetails_privateIpAddress = Lens.lens (\AwsEc2EipDetails' {privateIpAddress} -> privateIpAddress) (\s@AwsEc2EipDetails' {} a -> s {privateIpAddress = a} :: AwsEc2EipDetails)

-- | The identifier that represents the association of the Elastic IP address
-- with an EC2 instance.
awsEc2EipDetails_associationId :: Lens.Lens' AwsEc2EipDetails (Prelude.Maybe Prelude.Text)
awsEc2EipDetails_associationId = Lens.lens (\AwsEc2EipDetails' {associationId} -> associationId) (\s@AwsEc2EipDetails' {} a -> s {associationId = a} :: AwsEc2EipDetails)

-- | The identifier of an IP address pool. This parameter allows Amazon EC2
-- to select an IP address from the address pool.
awsEc2EipDetails_publicIpv4Pool :: Lens.Lens' AwsEc2EipDetails (Prelude.Maybe Prelude.Text)
awsEc2EipDetails_publicIpv4Pool = Lens.lens (\AwsEc2EipDetails' {publicIpv4Pool} -> publicIpv4Pool) (\s@AwsEc2EipDetails' {} a -> s {publicIpv4Pool = a} :: AwsEc2EipDetails)

instance Data.FromJSON AwsEc2EipDetails where
  parseJSON =
    Data.withObject
      "AwsEc2EipDetails"
      ( \x ->
          AwsEc2EipDetails'
            Prelude.<$> (x Data..:? "AllocationId")
            Prelude.<*> (x Data..:? "NetworkBorderGroup")
            Prelude.<*> (x Data..:? "Domain")
            Prelude.<*> (x Data..:? "NetworkInterfaceOwnerId")
            Prelude.<*> (x Data..:? "PublicIp")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "NetworkInterfaceId")
            Prelude.<*> (x Data..:? "PrivateIpAddress")
            Prelude.<*> (x Data..:? "AssociationId")
            Prelude.<*> (x Data..:? "PublicIpv4Pool")
      )

instance Prelude.Hashable AwsEc2EipDetails where
  hashWithSalt _salt AwsEc2EipDetails' {..} =
    _salt `Prelude.hashWithSalt` allocationId
      `Prelude.hashWithSalt` networkBorderGroup
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` networkInterfaceOwnerId
      `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` publicIpv4Pool

instance Prelude.NFData AwsEc2EipDetails where
  rnf AwsEc2EipDetails' {..} =
    Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf networkBorderGroup
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf networkInterfaceOwnerId
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf publicIpv4Pool

instance Data.ToJSON AwsEc2EipDetails where
  toJSON AwsEc2EipDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllocationId" Data..=) Prelude.<$> allocationId,
            ("NetworkBorderGroup" Data..=)
              Prelude.<$> networkBorderGroup,
            ("Domain" Data..=) Prelude.<$> domain,
            ("NetworkInterfaceOwnerId" Data..=)
              Prelude.<$> networkInterfaceOwnerId,
            ("PublicIp" Data..=) Prelude.<$> publicIp,
            ("InstanceId" Data..=) Prelude.<$> instanceId,
            ("NetworkInterfaceId" Data..=)
              Prelude.<$> networkInterfaceId,
            ("PrivateIpAddress" Data..=)
              Prelude.<$> privateIpAddress,
            ("AssociationId" Data..=) Prelude.<$> associationId,
            ("PublicIpv4Pool" Data..=)
              Prelude.<$> publicIpv4Pool
          ]
      )
