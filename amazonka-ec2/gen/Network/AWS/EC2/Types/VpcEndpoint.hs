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
-- Module      : Network.AWS.EC2.Types.VpcEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpcEndpoint where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DnsEntry
import Network.AWS.EC2.Types.LastError
import Network.AWS.EC2.Types.SecurityGroupIdentifier
import Network.AWS.EC2.Types.State
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VpcEndpointType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a VPC endpoint.
--
-- /See:/ 'newVpcEndpoint' smart constructor.
data VpcEndpoint = VpcEndpoint'
  { -- | The policy document associated with the endpoint, if applicable.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the VPC endpoint was created.
    creationTimestamp :: Prelude.Maybe Core.ISO8601,
    -- | (Interface endpoint) Information about the security groups that are
    -- associated with the network interface.
    groups :: Prelude.Maybe [SecurityGroupIdentifier],
    -- | The ID of the AWS account that owns the VPC endpoint.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | (Gateway endpoint) One or more route tables associated with the
    -- endpoint.
    routeTableIds :: Prelude.Maybe [Prelude.Text],
    -- | The type of endpoint.
    vpcEndpointType :: Prelude.Maybe VpcEndpointType,
    -- | (Interface endpoint) The DNS entries for the endpoint.
    dnsEntries :: Prelude.Maybe [DnsEntry],
    -- | Indicates whether the VPC endpoint is being managed by its service.
    requesterManaged :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC endpoint.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | (Interface endpoint) One or more subnets in which the endpoint is
    -- located.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | (Interface endpoint) One or more network interfaces for the endpoint.
    networkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the service to which the endpoint is associated.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The last error that occurred for VPC endpoint.
    lastError :: Prelude.Maybe LastError,
    -- | The state of the VPC endpoint.
    state :: Prelude.Maybe State,
    -- | Any tags assigned to the VPC endpoint.
    tags :: Prelude.Maybe [Tag],
    -- | (Interface endpoint) Indicates whether the VPC is associated with a
    -- private hosted zone.
    privateDnsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC to which the endpoint is associated.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyDocument', 'vpcEndpoint_policyDocument' - The policy document associated with the endpoint, if applicable.
--
-- 'creationTimestamp', 'vpcEndpoint_creationTimestamp' - The date and time that the VPC endpoint was created.
--
-- 'groups', 'vpcEndpoint_groups' - (Interface endpoint) Information about the security groups that are
-- associated with the network interface.
--
-- 'ownerId', 'vpcEndpoint_ownerId' - The ID of the AWS account that owns the VPC endpoint.
--
-- 'routeTableIds', 'vpcEndpoint_routeTableIds' - (Gateway endpoint) One or more route tables associated with the
-- endpoint.
--
-- 'vpcEndpointType', 'vpcEndpoint_vpcEndpointType' - The type of endpoint.
--
-- 'dnsEntries', 'vpcEndpoint_dnsEntries' - (Interface endpoint) The DNS entries for the endpoint.
--
-- 'requesterManaged', 'vpcEndpoint_requesterManaged' - Indicates whether the VPC endpoint is being managed by its service.
--
-- 'vpcEndpointId', 'vpcEndpoint_vpcEndpointId' - The ID of the VPC endpoint.
--
-- 'subnetIds', 'vpcEndpoint_subnetIds' - (Interface endpoint) One or more subnets in which the endpoint is
-- located.
--
-- 'networkInterfaceIds', 'vpcEndpoint_networkInterfaceIds' - (Interface endpoint) One or more network interfaces for the endpoint.
--
-- 'serviceName', 'vpcEndpoint_serviceName' - The name of the service to which the endpoint is associated.
--
-- 'lastError', 'vpcEndpoint_lastError' - The last error that occurred for VPC endpoint.
--
-- 'state', 'vpcEndpoint_state' - The state of the VPC endpoint.
--
-- 'tags', 'vpcEndpoint_tags' - Any tags assigned to the VPC endpoint.
--
-- 'privateDnsEnabled', 'vpcEndpoint_privateDnsEnabled' - (Interface endpoint) Indicates whether the VPC is associated with a
-- private hosted zone.
--
-- 'vpcId', 'vpcEndpoint_vpcId' - The ID of the VPC to which the endpoint is associated.
newVpcEndpoint ::
  VpcEndpoint
newVpcEndpoint =
  VpcEndpoint'
    { policyDocument = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      groups = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      routeTableIds = Prelude.Nothing,
      vpcEndpointType = Prelude.Nothing,
      dnsEntries = Prelude.Nothing,
      requesterManaged = Prelude.Nothing,
      vpcEndpointId = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      networkInterfaceIds = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      lastError = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      privateDnsEnabled = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The policy document associated with the endpoint, if applicable.
vpcEndpoint_policyDocument :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_policyDocument = Lens.lens (\VpcEndpoint' {policyDocument} -> policyDocument) (\s@VpcEndpoint' {} a -> s {policyDocument = a} :: VpcEndpoint)

-- | The date and time that the VPC endpoint was created.
vpcEndpoint_creationTimestamp :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.UTCTime)
vpcEndpoint_creationTimestamp = Lens.lens (\VpcEndpoint' {creationTimestamp} -> creationTimestamp) (\s@VpcEndpoint' {} a -> s {creationTimestamp = a} :: VpcEndpoint) Prelude.. Lens.mapping Core._Time

-- | (Interface endpoint) Information about the security groups that are
-- associated with the network interface.
vpcEndpoint_groups :: Lens.Lens' VpcEndpoint (Prelude.Maybe [SecurityGroupIdentifier])
vpcEndpoint_groups = Lens.lens (\VpcEndpoint' {groups} -> groups) (\s@VpcEndpoint' {} a -> s {groups = a} :: VpcEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the AWS account that owns the VPC endpoint.
vpcEndpoint_ownerId :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_ownerId = Lens.lens (\VpcEndpoint' {ownerId} -> ownerId) (\s@VpcEndpoint' {} a -> s {ownerId = a} :: VpcEndpoint)

-- | (Gateway endpoint) One or more route tables associated with the
-- endpoint.
vpcEndpoint_routeTableIds :: Lens.Lens' VpcEndpoint (Prelude.Maybe [Prelude.Text])
vpcEndpoint_routeTableIds = Lens.lens (\VpcEndpoint' {routeTableIds} -> routeTableIds) (\s@VpcEndpoint' {} a -> s {routeTableIds = a} :: VpcEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | The type of endpoint.
vpcEndpoint_vpcEndpointType :: Lens.Lens' VpcEndpoint (Prelude.Maybe VpcEndpointType)
vpcEndpoint_vpcEndpointType = Lens.lens (\VpcEndpoint' {vpcEndpointType} -> vpcEndpointType) (\s@VpcEndpoint' {} a -> s {vpcEndpointType = a} :: VpcEndpoint)

-- | (Interface endpoint) The DNS entries for the endpoint.
vpcEndpoint_dnsEntries :: Lens.Lens' VpcEndpoint (Prelude.Maybe [DnsEntry])
vpcEndpoint_dnsEntries = Lens.lens (\VpcEndpoint' {dnsEntries} -> dnsEntries) (\s@VpcEndpoint' {} a -> s {dnsEntries = a} :: VpcEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | Indicates whether the VPC endpoint is being managed by its service.
vpcEndpoint_requesterManaged :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Bool)
vpcEndpoint_requesterManaged = Lens.lens (\VpcEndpoint' {requesterManaged} -> requesterManaged) (\s@VpcEndpoint' {} a -> s {requesterManaged = a} :: VpcEndpoint)

-- | The ID of the VPC endpoint.
vpcEndpoint_vpcEndpointId :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_vpcEndpointId = Lens.lens (\VpcEndpoint' {vpcEndpointId} -> vpcEndpointId) (\s@VpcEndpoint' {} a -> s {vpcEndpointId = a} :: VpcEndpoint)

-- | (Interface endpoint) One or more subnets in which the endpoint is
-- located.
vpcEndpoint_subnetIds :: Lens.Lens' VpcEndpoint (Prelude.Maybe [Prelude.Text])
vpcEndpoint_subnetIds = Lens.lens (\VpcEndpoint' {subnetIds} -> subnetIds) (\s@VpcEndpoint' {} a -> s {subnetIds = a} :: VpcEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | (Interface endpoint) One or more network interfaces for the endpoint.
vpcEndpoint_networkInterfaceIds :: Lens.Lens' VpcEndpoint (Prelude.Maybe [Prelude.Text])
vpcEndpoint_networkInterfaceIds = Lens.lens (\VpcEndpoint' {networkInterfaceIds} -> networkInterfaceIds) (\s@VpcEndpoint' {} a -> s {networkInterfaceIds = a} :: VpcEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the service to which the endpoint is associated.
vpcEndpoint_serviceName :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_serviceName = Lens.lens (\VpcEndpoint' {serviceName} -> serviceName) (\s@VpcEndpoint' {} a -> s {serviceName = a} :: VpcEndpoint)

-- | The last error that occurred for VPC endpoint.
vpcEndpoint_lastError :: Lens.Lens' VpcEndpoint (Prelude.Maybe LastError)
vpcEndpoint_lastError = Lens.lens (\VpcEndpoint' {lastError} -> lastError) (\s@VpcEndpoint' {} a -> s {lastError = a} :: VpcEndpoint)

-- | The state of the VPC endpoint.
vpcEndpoint_state :: Lens.Lens' VpcEndpoint (Prelude.Maybe State)
vpcEndpoint_state = Lens.lens (\VpcEndpoint' {state} -> state) (\s@VpcEndpoint' {} a -> s {state = a} :: VpcEndpoint)

-- | Any tags assigned to the VPC endpoint.
vpcEndpoint_tags :: Lens.Lens' VpcEndpoint (Prelude.Maybe [Tag])
vpcEndpoint_tags = Lens.lens (\VpcEndpoint' {tags} -> tags) (\s@VpcEndpoint' {} a -> s {tags = a} :: VpcEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | (Interface endpoint) Indicates whether the VPC is associated with a
-- private hosted zone.
vpcEndpoint_privateDnsEnabled :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Bool)
vpcEndpoint_privateDnsEnabled = Lens.lens (\VpcEndpoint' {privateDnsEnabled} -> privateDnsEnabled) (\s@VpcEndpoint' {} a -> s {privateDnsEnabled = a} :: VpcEndpoint)

-- | The ID of the VPC to which the endpoint is associated.
vpcEndpoint_vpcId :: Lens.Lens' VpcEndpoint (Prelude.Maybe Prelude.Text)
vpcEndpoint_vpcId = Lens.lens (\VpcEndpoint' {vpcId} -> vpcId) (\s@VpcEndpoint' {} a -> s {vpcId = a} :: VpcEndpoint)

instance Core.FromXML VpcEndpoint where
  parseXML x =
    VpcEndpoint'
      Prelude.<$> (x Core..@? "policyDocument")
      Prelude.<*> (x Core..@? "creationTimestamp")
      Prelude.<*> ( x Core..@? "groupSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> ( x Core..@? "routeTableIdSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "vpcEndpointType")
      Prelude.<*> ( x Core..@? "dnsEntrySet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "requesterManaged")
      Prelude.<*> (x Core..@? "vpcEndpointId")
      Prelude.<*> ( x Core..@? "subnetIdSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "networkInterfaceIdSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "serviceName")
      Prelude.<*> (x Core..@? "lastError")
      Prelude.<*> (x Core..@? "state")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "privateDnsEnabled")
      Prelude.<*> (x Core..@? "vpcId")

instance Prelude.Hashable VpcEndpoint

instance Prelude.NFData VpcEndpoint
