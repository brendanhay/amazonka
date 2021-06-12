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

-- | Describes a VPC endpoint.
--
-- /See:/ 'newVpcEndpoint' smart constructor.
data VpcEndpoint = VpcEndpoint'
  { -- | The date and time that the VPC endpoint was created.
    creationTimestamp :: Core.Maybe Core.ISO8601,
    -- | The policy document associated with the endpoint, if applicable.
    policyDocument :: Core.Maybe Core.Text,
    -- | (Interface endpoint) Information about the security groups that are
    -- associated with the network interface.
    groups :: Core.Maybe [SecurityGroupIdentifier],
    -- | The ID of the AWS account that owns the VPC endpoint.
    ownerId :: Core.Maybe Core.Text,
    -- | (Gateway endpoint) One or more route tables associated with the
    -- endpoint.
    routeTableIds :: Core.Maybe [Core.Text],
    -- | The type of endpoint.
    vpcEndpointType :: Core.Maybe VpcEndpointType,
    -- | Indicates whether the VPC endpoint is being managed by its service.
    requesterManaged :: Core.Maybe Core.Bool,
    -- | (Interface endpoint) The DNS entries for the endpoint.
    dnsEntries :: Core.Maybe [DnsEntry],
    -- | The ID of the VPC endpoint.
    vpcEndpointId :: Core.Maybe Core.Text,
    -- | (Interface endpoint) One or more subnets in which the endpoint is
    -- located.
    subnetIds :: Core.Maybe [Core.Text],
    -- | (Interface endpoint) One or more network interfaces for the endpoint.
    networkInterfaceIds :: Core.Maybe [Core.Text],
    -- | The name of the service to which the endpoint is associated.
    serviceName :: Core.Maybe Core.Text,
    -- | The last error that occurred for VPC endpoint.
    lastError :: Core.Maybe LastError,
    -- | The state of the VPC endpoint.
    state :: Core.Maybe State,
    -- | (Interface endpoint) Indicates whether the VPC is associated with a
    -- private hosted zone.
    privateDnsEnabled :: Core.Maybe Core.Bool,
    -- | Any tags assigned to the VPC endpoint.
    tags :: Core.Maybe [Tag],
    -- | The ID of the VPC to which the endpoint is associated.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VpcEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'vpcEndpoint_creationTimestamp' - The date and time that the VPC endpoint was created.
--
-- 'policyDocument', 'vpcEndpoint_policyDocument' - The policy document associated with the endpoint, if applicable.
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
-- 'requesterManaged', 'vpcEndpoint_requesterManaged' - Indicates whether the VPC endpoint is being managed by its service.
--
-- 'dnsEntries', 'vpcEndpoint_dnsEntries' - (Interface endpoint) The DNS entries for the endpoint.
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
-- 'privateDnsEnabled', 'vpcEndpoint_privateDnsEnabled' - (Interface endpoint) Indicates whether the VPC is associated with a
-- private hosted zone.
--
-- 'tags', 'vpcEndpoint_tags' - Any tags assigned to the VPC endpoint.
--
-- 'vpcId', 'vpcEndpoint_vpcId' - The ID of the VPC to which the endpoint is associated.
newVpcEndpoint ::
  VpcEndpoint
newVpcEndpoint =
  VpcEndpoint'
    { creationTimestamp = Core.Nothing,
      policyDocument = Core.Nothing,
      groups = Core.Nothing,
      ownerId = Core.Nothing,
      routeTableIds = Core.Nothing,
      vpcEndpointType = Core.Nothing,
      requesterManaged = Core.Nothing,
      dnsEntries = Core.Nothing,
      vpcEndpointId = Core.Nothing,
      subnetIds = Core.Nothing,
      networkInterfaceIds = Core.Nothing,
      serviceName = Core.Nothing,
      lastError = Core.Nothing,
      state = Core.Nothing,
      privateDnsEnabled = Core.Nothing,
      tags = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The date and time that the VPC endpoint was created.
vpcEndpoint_creationTimestamp :: Lens.Lens' VpcEndpoint (Core.Maybe Core.UTCTime)
vpcEndpoint_creationTimestamp = Lens.lens (\VpcEndpoint' {creationTimestamp} -> creationTimestamp) (\s@VpcEndpoint' {} a -> s {creationTimestamp = a} :: VpcEndpoint) Core.. Lens.mapping Core._Time

-- | The policy document associated with the endpoint, if applicable.
vpcEndpoint_policyDocument :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Text)
vpcEndpoint_policyDocument = Lens.lens (\VpcEndpoint' {policyDocument} -> policyDocument) (\s@VpcEndpoint' {} a -> s {policyDocument = a} :: VpcEndpoint)

-- | (Interface endpoint) Information about the security groups that are
-- associated with the network interface.
vpcEndpoint_groups :: Lens.Lens' VpcEndpoint (Core.Maybe [SecurityGroupIdentifier])
vpcEndpoint_groups = Lens.lens (\VpcEndpoint' {groups} -> groups) (\s@VpcEndpoint' {} a -> s {groups = a} :: VpcEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The ID of the AWS account that owns the VPC endpoint.
vpcEndpoint_ownerId :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Text)
vpcEndpoint_ownerId = Lens.lens (\VpcEndpoint' {ownerId} -> ownerId) (\s@VpcEndpoint' {} a -> s {ownerId = a} :: VpcEndpoint)

-- | (Gateway endpoint) One or more route tables associated with the
-- endpoint.
vpcEndpoint_routeTableIds :: Lens.Lens' VpcEndpoint (Core.Maybe [Core.Text])
vpcEndpoint_routeTableIds = Lens.lens (\VpcEndpoint' {routeTableIds} -> routeTableIds) (\s@VpcEndpoint' {} a -> s {routeTableIds = a} :: VpcEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The type of endpoint.
vpcEndpoint_vpcEndpointType :: Lens.Lens' VpcEndpoint (Core.Maybe VpcEndpointType)
vpcEndpoint_vpcEndpointType = Lens.lens (\VpcEndpoint' {vpcEndpointType} -> vpcEndpointType) (\s@VpcEndpoint' {} a -> s {vpcEndpointType = a} :: VpcEndpoint)

-- | Indicates whether the VPC endpoint is being managed by its service.
vpcEndpoint_requesterManaged :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Bool)
vpcEndpoint_requesterManaged = Lens.lens (\VpcEndpoint' {requesterManaged} -> requesterManaged) (\s@VpcEndpoint' {} a -> s {requesterManaged = a} :: VpcEndpoint)

-- | (Interface endpoint) The DNS entries for the endpoint.
vpcEndpoint_dnsEntries :: Lens.Lens' VpcEndpoint (Core.Maybe [DnsEntry])
vpcEndpoint_dnsEntries = Lens.lens (\VpcEndpoint' {dnsEntries} -> dnsEntries) (\s@VpcEndpoint' {} a -> s {dnsEntries = a} :: VpcEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The ID of the VPC endpoint.
vpcEndpoint_vpcEndpointId :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Text)
vpcEndpoint_vpcEndpointId = Lens.lens (\VpcEndpoint' {vpcEndpointId} -> vpcEndpointId) (\s@VpcEndpoint' {} a -> s {vpcEndpointId = a} :: VpcEndpoint)

-- | (Interface endpoint) One or more subnets in which the endpoint is
-- located.
vpcEndpoint_subnetIds :: Lens.Lens' VpcEndpoint (Core.Maybe [Core.Text])
vpcEndpoint_subnetIds = Lens.lens (\VpcEndpoint' {subnetIds} -> subnetIds) (\s@VpcEndpoint' {} a -> s {subnetIds = a} :: VpcEndpoint) Core.. Lens.mapping Lens._Coerce

-- | (Interface endpoint) One or more network interfaces for the endpoint.
vpcEndpoint_networkInterfaceIds :: Lens.Lens' VpcEndpoint (Core.Maybe [Core.Text])
vpcEndpoint_networkInterfaceIds = Lens.lens (\VpcEndpoint' {networkInterfaceIds} -> networkInterfaceIds) (\s@VpcEndpoint' {} a -> s {networkInterfaceIds = a} :: VpcEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The name of the service to which the endpoint is associated.
vpcEndpoint_serviceName :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Text)
vpcEndpoint_serviceName = Lens.lens (\VpcEndpoint' {serviceName} -> serviceName) (\s@VpcEndpoint' {} a -> s {serviceName = a} :: VpcEndpoint)

-- | The last error that occurred for VPC endpoint.
vpcEndpoint_lastError :: Lens.Lens' VpcEndpoint (Core.Maybe LastError)
vpcEndpoint_lastError = Lens.lens (\VpcEndpoint' {lastError} -> lastError) (\s@VpcEndpoint' {} a -> s {lastError = a} :: VpcEndpoint)

-- | The state of the VPC endpoint.
vpcEndpoint_state :: Lens.Lens' VpcEndpoint (Core.Maybe State)
vpcEndpoint_state = Lens.lens (\VpcEndpoint' {state} -> state) (\s@VpcEndpoint' {} a -> s {state = a} :: VpcEndpoint)

-- | (Interface endpoint) Indicates whether the VPC is associated with a
-- private hosted zone.
vpcEndpoint_privateDnsEnabled :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Bool)
vpcEndpoint_privateDnsEnabled = Lens.lens (\VpcEndpoint' {privateDnsEnabled} -> privateDnsEnabled) (\s@VpcEndpoint' {} a -> s {privateDnsEnabled = a} :: VpcEndpoint)

-- | Any tags assigned to the VPC endpoint.
vpcEndpoint_tags :: Lens.Lens' VpcEndpoint (Core.Maybe [Tag])
vpcEndpoint_tags = Lens.lens (\VpcEndpoint' {tags} -> tags) (\s@VpcEndpoint' {} a -> s {tags = a} :: VpcEndpoint) Core.. Lens.mapping Lens._Coerce

-- | The ID of the VPC to which the endpoint is associated.
vpcEndpoint_vpcId :: Lens.Lens' VpcEndpoint (Core.Maybe Core.Text)
vpcEndpoint_vpcId = Lens.lens (\VpcEndpoint' {vpcId} -> vpcId) (\s@VpcEndpoint' {} a -> s {vpcId = a} :: VpcEndpoint)

instance Core.FromXML VpcEndpoint where
  parseXML x =
    VpcEndpoint'
      Core.<$> (x Core..@? "creationTimestamp")
      Core.<*> (x Core..@? "policyDocument")
      Core.<*> ( x Core..@? "groupSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "ownerId")
      Core.<*> ( x Core..@? "routeTableIdSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "vpcEndpointType")
      Core.<*> (x Core..@? "requesterManaged")
      Core.<*> ( x Core..@? "dnsEntrySet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "vpcEndpointId")
      Core.<*> ( x Core..@? "subnetIdSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "networkInterfaceIdSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "serviceName")
      Core.<*> (x Core..@? "lastError")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "privateDnsEnabled")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "vpcId")

instance Core.Hashable VpcEndpoint

instance Core.NFData VpcEndpoint
