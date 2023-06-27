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
-- Module      : Amazonka.EC2.Types.Ec2InstanceConnectEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Ec2InstanceConnectEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Ec2InstanceConnectEndpointState
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The EC2 Instance Connect Endpoint.
--
-- /See:/ 'newEc2InstanceConnectEndpoint' smart constructor.
data Ec2InstanceConnectEndpoint = Ec2InstanceConnectEndpoint'
  { -- | The Availability Zone of the EC2 Instance Connect Endpoint.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the EC2 Instance Connect Endpoint was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The DNS name of the EC2 Instance Connect Endpoint.
    dnsName :: Prelude.Maybe Prelude.Text,
    fipsDnsName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the EC2 Instance Connect Endpoint.
    instanceConnectEndpointArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the EC2 Instance Connect Endpoint.
    instanceConnectEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the elastic network interface that Amazon EC2 automatically
    -- created when creating the EC2 Instance Connect Endpoint.
    networkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the Amazon Web Services account that created the EC2 Instance
    -- Connect Endpoint.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether your client\'s IP address is preserved as the source.
    -- The value is @true@ or @false@.
    --
    -- -   If @true@, your client\'s IP address is used when you connect to a
    --     resource.
    --
    -- -   If @false@, the elastic network interface IP address is used when
    --     you connect to a resource.
    --
    -- Default: @true@
    preserveClientIp :: Prelude.Maybe Prelude.Bool,
    -- | The security groups associated with the endpoint. If you didn\'t specify
    -- a security group, the default security group for your VPC is associated
    -- with the endpoint.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The current state of the EC2 Instance Connect Endpoint.
    state :: Prelude.Maybe Ec2InstanceConnectEndpointState,
    -- | The message for the current state of the EC2 Instance Connect Endpoint.
    -- Can include a failure message.
    stateMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet in which the EC2 Instance Connect Endpoint was
    -- created.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the EC2 Instance Connect Endpoint.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the VPC in which the EC2 Instance Connect Endpoint was
    -- created.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ec2InstanceConnectEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'ec2InstanceConnectEndpoint_availabilityZone' - The Availability Zone of the EC2 Instance Connect Endpoint.
--
-- 'createdAt', 'ec2InstanceConnectEndpoint_createdAt' - The date and time that the EC2 Instance Connect Endpoint was created.
--
-- 'dnsName', 'ec2InstanceConnectEndpoint_dnsName' - The DNS name of the EC2 Instance Connect Endpoint.
--
-- 'fipsDnsName', 'ec2InstanceConnectEndpoint_fipsDnsName' -
--
-- 'instanceConnectEndpointArn', 'ec2InstanceConnectEndpoint_instanceConnectEndpointArn' - The Amazon Resource Name (ARN) of the EC2 Instance Connect Endpoint.
--
-- 'instanceConnectEndpointId', 'ec2InstanceConnectEndpoint_instanceConnectEndpointId' - The ID of the EC2 Instance Connect Endpoint.
--
-- 'networkInterfaceIds', 'ec2InstanceConnectEndpoint_networkInterfaceIds' - The ID of the elastic network interface that Amazon EC2 automatically
-- created when creating the EC2 Instance Connect Endpoint.
--
-- 'ownerId', 'ec2InstanceConnectEndpoint_ownerId' - The ID of the Amazon Web Services account that created the EC2 Instance
-- Connect Endpoint.
--
-- 'preserveClientIp', 'ec2InstanceConnectEndpoint_preserveClientIp' - Indicates whether your client\'s IP address is preserved as the source.
-- The value is @true@ or @false@.
--
-- -   If @true@, your client\'s IP address is used when you connect to a
--     resource.
--
-- -   If @false@, the elastic network interface IP address is used when
--     you connect to a resource.
--
-- Default: @true@
--
-- 'securityGroupIds', 'ec2InstanceConnectEndpoint_securityGroupIds' - The security groups associated with the endpoint. If you didn\'t specify
-- a security group, the default security group for your VPC is associated
-- with the endpoint.
--
-- 'state', 'ec2InstanceConnectEndpoint_state' - The current state of the EC2 Instance Connect Endpoint.
--
-- 'stateMessage', 'ec2InstanceConnectEndpoint_stateMessage' - The message for the current state of the EC2 Instance Connect Endpoint.
-- Can include a failure message.
--
-- 'subnetId', 'ec2InstanceConnectEndpoint_subnetId' - The ID of the subnet in which the EC2 Instance Connect Endpoint was
-- created.
--
-- 'tags', 'ec2InstanceConnectEndpoint_tags' - The tags assigned to the EC2 Instance Connect Endpoint.
--
-- 'vpcId', 'ec2InstanceConnectEndpoint_vpcId' - The ID of the VPC in which the EC2 Instance Connect Endpoint was
-- created.
newEc2InstanceConnectEndpoint ::
  Ec2InstanceConnectEndpoint
newEc2InstanceConnectEndpoint =
  Ec2InstanceConnectEndpoint'
    { availabilityZone =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      fipsDnsName = Prelude.Nothing,
      instanceConnectEndpointArn = Prelude.Nothing,
      instanceConnectEndpointId = Prelude.Nothing,
      networkInterfaceIds = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      preserveClientIp = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      state = Prelude.Nothing,
      stateMessage = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The Availability Zone of the EC2 Instance Connect Endpoint.
ec2InstanceConnectEndpoint_availabilityZone :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe Prelude.Text)
ec2InstanceConnectEndpoint_availabilityZone = Lens.lens (\Ec2InstanceConnectEndpoint' {availabilityZone} -> availabilityZone) (\s@Ec2InstanceConnectEndpoint' {} a -> s {availabilityZone = a} :: Ec2InstanceConnectEndpoint)

-- | The date and time that the EC2 Instance Connect Endpoint was created.
ec2InstanceConnectEndpoint_createdAt :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe Prelude.UTCTime)
ec2InstanceConnectEndpoint_createdAt = Lens.lens (\Ec2InstanceConnectEndpoint' {createdAt} -> createdAt) (\s@Ec2InstanceConnectEndpoint' {} a -> s {createdAt = a} :: Ec2InstanceConnectEndpoint) Prelude.. Lens.mapping Data._Time

-- | The DNS name of the EC2 Instance Connect Endpoint.
ec2InstanceConnectEndpoint_dnsName :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe Prelude.Text)
ec2InstanceConnectEndpoint_dnsName = Lens.lens (\Ec2InstanceConnectEndpoint' {dnsName} -> dnsName) (\s@Ec2InstanceConnectEndpoint' {} a -> s {dnsName = a} :: Ec2InstanceConnectEndpoint)

ec2InstanceConnectEndpoint_fipsDnsName :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe Prelude.Text)
ec2InstanceConnectEndpoint_fipsDnsName = Lens.lens (\Ec2InstanceConnectEndpoint' {fipsDnsName} -> fipsDnsName) (\s@Ec2InstanceConnectEndpoint' {} a -> s {fipsDnsName = a} :: Ec2InstanceConnectEndpoint)

-- | The Amazon Resource Name (ARN) of the EC2 Instance Connect Endpoint.
ec2InstanceConnectEndpoint_instanceConnectEndpointArn :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe Prelude.Text)
ec2InstanceConnectEndpoint_instanceConnectEndpointArn = Lens.lens (\Ec2InstanceConnectEndpoint' {instanceConnectEndpointArn} -> instanceConnectEndpointArn) (\s@Ec2InstanceConnectEndpoint' {} a -> s {instanceConnectEndpointArn = a} :: Ec2InstanceConnectEndpoint)

-- | The ID of the EC2 Instance Connect Endpoint.
ec2InstanceConnectEndpoint_instanceConnectEndpointId :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe Prelude.Text)
ec2InstanceConnectEndpoint_instanceConnectEndpointId = Lens.lens (\Ec2InstanceConnectEndpoint' {instanceConnectEndpointId} -> instanceConnectEndpointId) (\s@Ec2InstanceConnectEndpoint' {} a -> s {instanceConnectEndpointId = a} :: Ec2InstanceConnectEndpoint)

-- | The ID of the elastic network interface that Amazon EC2 automatically
-- created when creating the EC2 Instance Connect Endpoint.
ec2InstanceConnectEndpoint_networkInterfaceIds :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe [Prelude.Text])
ec2InstanceConnectEndpoint_networkInterfaceIds = Lens.lens (\Ec2InstanceConnectEndpoint' {networkInterfaceIds} -> networkInterfaceIds) (\s@Ec2InstanceConnectEndpoint' {} a -> s {networkInterfaceIds = a} :: Ec2InstanceConnectEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that created the EC2 Instance
-- Connect Endpoint.
ec2InstanceConnectEndpoint_ownerId :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe Prelude.Text)
ec2InstanceConnectEndpoint_ownerId = Lens.lens (\Ec2InstanceConnectEndpoint' {ownerId} -> ownerId) (\s@Ec2InstanceConnectEndpoint' {} a -> s {ownerId = a} :: Ec2InstanceConnectEndpoint)

-- | Indicates whether your client\'s IP address is preserved as the source.
-- The value is @true@ or @false@.
--
-- -   If @true@, your client\'s IP address is used when you connect to a
--     resource.
--
-- -   If @false@, the elastic network interface IP address is used when
--     you connect to a resource.
--
-- Default: @true@
ec2InstanceConnectEndpoint_preserveClientIp :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe Prelude.Bool)
ec2InstanceConnectEndpoint_preserveClientIp = Lens.lens (\Ec2InstanceConnectEndpoint' {preserveClientIp} -> preserveClientIp) (\s@Ec2InstanceConnectEndpoint' {} a -> s {preserveClientIp = a} :: Ec2InstanceConnectEndpoint)

-- | The security groups associated with the endpoint. If you didn\'t specify
-- a security group, the default security group for your VPC is associated
-- with the endpoint.
ec2InstanceConnectEndpoint_securityGroupIds :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe [Prelude.Text])
ec2InstanceConnectEndpoint_securityGroupIds = Lens.lens (\Ec2InstanceConnectEndpoint' {securityGroupIds} -> securityGroupIds) (\s@Ec2InstanceConnectEndpoint' {} a -> s {securityGroupIds = a} :: Ec2InstanceConnectEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The current state of the EC2 Instance Connect Endpoint.
ec2InstanceConnectEndpoint_state :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe Ec2InstanceConnectEndpointState)
ec2InstanceConnectEndpoint_state = Lens.lens (\Ec2InstanceConnectEndpoint' {state} -> state) (\s@Ec2InstanceConnectEndpoint' {} a -> s {state = a} :: Ec2InstanceConnectEndpoint)

-- | The message for the current state of the EC2 Instance Connect Endpoint.
-- Can include a failure message.
ec2InstanceConnectEndpoint_stateMessage :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe Prelude.Text)
ec2InstanceConnectEndpoint_stateMessage = Lens.lens (\Ec2InstanceConnectEndpoint' {stateMessage} -> stateMessage) (\s@Ec2InstanceConnectEndpoint' {} a -> s {stateMessage = a} :: Ec2InstanceConnectEndpoint)

-- | The ID of the subnet in which the EC2 Instance Connect Endpoint was
-- created.
ec2InstanceConnectEndpoint_subnetId :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe Prelude.Text)
ec2InstanceConnectEndpoint_subnetId = Lens.lens (\Ec2InstanceConnectEndpoint' {subnetId} -> subnetId) (\s@Ec2InstanceConnectEndpoint' {} a -> s {subnetId = a} :: Ec2InstanceConnectEndpoint)

-- | The tags assigned to the EC2 Instance Connect Endpoint.
ec2InstanceConnectEndpoint_tags :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe [Tag])
ec2InstanceConnectEndpoint_tags = Lens.lens (\Ec2InstanceConnectEndpoint' {tags} -> tags) (\s@Ec2InstanceConnectEndpoint' {} a -> s {tags = a} :: Ec2InstanceConnectEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC in which the EC2 Instance Connect Endpoint was
-- created.
ec2InstanceConnectEndpoint_vpcId :: Lens.Lens' Ec2InstanceConnectEndpoint (Prelude.Maybe Prelude.Text)
ec2InstanceConnectEndpoint_vpcId = Lens.lens (\Ec2InstanceConnectEndpoint' {vpcId} -> vpcId) (\s@Ec2InstanceConnectEndpoint' {} a -> s {vpcId = a} :: Ec2InstanceConnectEndpoint)

instance Data.FromXML Ec2InstanceConnectEndpoint where
  parseXML x =
    Ec2InstanceConnectEndpoint'
      Prelude.<$> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "createdAt")
      Prelude.<*> (x Data..@? "dnsName")
      Prelude.<*> (x Data..@? "fipsDnsName")
      Prelude.<*> (x Data..@? "instanceConnectEndpointArn")
      Prelude.<*> (x Data..@? "instanceConnectEndpointId")
      Prelude.<*> ( x
                      Data..@? "networkInterfaceIdSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "preserveClientIp")
      Prelude.<*> ( x
                      Data..@? "securityGroupIdSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "stateMessage")
      Prelude.<*> (x Data..@? "subnetId")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "vpcId")

instance Prelude.Hashable Ec2InstanceConnectEndpoint where
  hashWithSalt _salt Ec2InstanceConnectEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` fipsDnsName
      `Prelude.hashWithSalt` instanceConnectEndpointArn
      `Prelude.hashWithSalt` instanceConnectEndpointId
      `Prelude.hashWithSalt` networkInterfaceIds
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` preserveClientIp
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateMessage
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData Ec2InstanceConnectEndpoint where
  rnf Ec2InstanceConnectEndpoint' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf fipsDnsName
      `Prelude.seq` Prelude.rnf instanceConnectEndpointArn
      `Prelude.seq` Prelude.rnf instanceConnectEndpointId
      `Prelude.seq` Prelude.rnf networkInterfaceIds
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf preserveClientIp
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateMessage
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcId
