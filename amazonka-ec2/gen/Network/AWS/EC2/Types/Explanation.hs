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
-- Module      : Network.AWS.EC2.Types.Explanation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Explanation where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AnalysisAclRule
import Network.AWS.EC2.Types.AnalysisComponent
import Network.AWS.EC2.Types.AnalysisLoadBalancerListener
import Network.AWS.EC2.Types.AnalysisLoadBalancerTarget
import Network.AWS.EC2.Types.AnalysisRouteTableRoute
import Network.AWS.EC2.Types.AnalysisSecurityGroupRule
import Network.AWS.EC2.Types.PortRange
import qualified Network.AWS.Lens as Lens

-- | Describes an explanation code for an unreachable path. For more
-- information, see
-- <https://docs.aws.amazon.com/vpc/latest/reachability/explanation-codes.html Reachability Analyzer explanation codes>.
--
-- /See:/ 'newExplanation' smart constructor.
data Explanation = Explanation'
  { -- | The target groups.
    loadBalancerTargetGroups :: Core.Maybe [AnalysisComponent],
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Core.Maybe Core.Text,
    -- | The security group rule.
    securityGroupRule :: Core.Maybe AnalysisSecurityGroupRule,
    -- | The customer gateway.
    customerGateway :: Core.Maybe AnalysisComponent,
    -- | The Availability Zones.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | The target group.
    loadBalancerTargetGroup :: Core.Maybe AnalysisComponent,
    -- | The explanation code.
    explanationCode :: Core.Maybe Core.Text,
    -- | The IPv4 address, in CIDR notation.
    address :: Core.Maybe Core.Text,
    -- | The NAT gateway.
    natGateway :: Core.Maybe AnalysisComponent,
    -- | The target.
    loadBalancerTarget :: Core.Maybe AnalysisLoadBalancerTarget,
    -- | The internet gateway.
    internetGateway :: Core.Maybe AnalysisComponent,
    -- | The network interface.
    networkInterface :: Core.Maybe AnalysisComponent,
    -- | The subnet.
    subnet :: Core.Maybe AnalysisComponent,
    -- | The source VPC.
    sourceVpc :: Core.Maybe AnalysisComponent,
    -- | The network ACL rule.
    aclRule :: Core.Maybe AnalysisAclRule,
    -- | The protocols.
    protocols :: Core.Maybe [Core.Text],
    -- | The direction. The following are possible values:
    --
    -- -   egress
    --
    -- -   ingress
    direction :: Core.Maybe Core.Text,
    -- | The security groups.
    securityGroups :: Core.Maybe [AnalysisComponent],
    -- | The prefix list.
    prefixList :: Core.Maybe AnalysisComponent,
    -- | The packet field.
    packetField :: Core.Maybe Core.Text,
    -- | The CIDR ranges.
    cidrs :: Core.Maybe [Core.Text],
    -- | The component.
    component :: Core.Maybe AnalysisComponent,
    -- | The state.
    state :: Core.Maybe Core.Text,
    -- | The route table.
    routeTable :: Core.Maybe AnalysisComponent,
    -- | The destination.
    destination :: Core.Maybe AnalysisComponent,
    -- | The route table for the subnet.
    subnetRouteTable :: Core.Maybe AnalysisComponent,
    -- | The resource to which the component is attached.
    attachedTo :: Core.Maybe AnalysisComponent,
    -- | The security group.
    securityGroup :: Core.Maybe AnalysisComponent,
    -- | The VPC peering connection.
    vpcPeeringConnection :: Core.Maybe AnalysisComponent,
    -- | The destination VPC.
    destinationVpc :: Core.Maybe AnalysisComponent,
    -- | The load balancer listener.
    elasticLoadBalancerListener :: Core.Maybe AnalysisComponent,
    -- | The port.
    port :: Core.Maybe Core.Natural,
    -- | The network ACL.
    acl :: Core.Maybe AnalysisComponent,
    -- | The VPC endpoint.
    vpcEndpoint :: Core.Maybe AnalysisComponent,
    -- | The route table route.
    routeTableRoute :: Core.Maybe AnalysisRouteTableRoute,
    -- | The VPN connection.
    vpnConnection :: Core.Maybe AnalysisComponent,
    -- | The missing component.
    missingComponent :: Core.Maybe Core.Text,
    -- | The IPv4 addresses, in CIDR notation.
    addresses :: Core.Maybe [Core.Text],
    -- | The listener for a Classic Load Balancer.
    classicLoadBalancerListener :: Core.Maybe AnalysisLoadBalancerListener,
    -- | The route table.
    ingressRouteTable :: Core.Maybe AnalysisComponent,
    -- | The VPN gateway.
    vpnGateway :: Core.Maybe AnalysisComponent,
    -- | The port ranges.
    portRanges :: Core.Maybe [PortRange],
    -- | The listener port of the load balancer.
    loadBalancerListenerPort :: Core.Maybe Core.Natural,
    -- | The component VPC.
    vpc :: Core.Maybe AnalysisComponent,
    -- | The target port.
    loadBalancerTargetPort :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Explanation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerTargetGroups', 'explanation_loadBalancerTargetGroups' - The target groups.
--
-- 'loadBalancerArn', 'explanation_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'securityGroupRule', 'explanation_securityGroupRule' - The security group rule.
--
-- 'customerGateway', 'explanation_customerGateway' - The customer gateway.
--
-- 'availabilityZones', 'explanation_availabilityZones' - The Availability Zones.
--
-- 'loadBalancerTargetGroup', 'explanation_loadBalancerTargetGroup' - The target group.
--
-- 'explanationCode', 'explanation_explanationCode' - The explanation code.
--
-- 'address', 'explanation_address' - The IPv4 address, in CIDR notation.
--
-- 'natGateway', 'explanation_natGateway' - The NAT gateway.
--
-- 'loadBalancerTarget', 'explanation_loadBalancerTarget' - The target.
--
-- 'internetGateway', 'explanation_internetGateway' - The internet gateway.
--
-- 'networkInterface', 'explanation_networkInterface' - The network interface.
--
-- 'subnet', 'explanation_subnet' - The subnet.
--
-- 'sourceVpc', 'explanation_sourceVpc' - The source VPC.
--
-- 'aclRule', 'explanation_aclRule' - The network ACL rule.
--
-- 'protocols', 'explanation_protocols' - The protocols.
--
-- 'direction', 'explanation_direction' - The direction. The following are possible values:
--
-- -   egress
--
-- -   ingress
--
-- 'securityGroups', 'explanation_securityGroups' - The security groups.
--
-- 'prefixList', 'explanation_prefixList' - The prefix list.
--
-- 'packetField', 'explanation_packetField' - The packet field.
--
-- 'cidrs', 'explanation_cidrs' - The CIDR ranges.
--
-- 'component', 'explanation_component' - The component.
--
-- 'state', 'explanation_state' - The state.
--
-- 'routeTable', 'explanation_routeTable' - The route table.
--
-- 'destination', 'explanation_destination' - The destination.
--
-- 'subnetRouteTable', 'explanation_subnetRouteTable' - The route table for the subnet.
--
-- 'attachedTo', 'explanation_attachedTo' - The resource to which the component is attached.
--
-- 'securityGroup', 'explanation_securityGroup' - The security group.
--
-- 'vpcPeeringConnection', 'explanation_vpcPeeringConnection' - The VPC peering connection.
--
-- 'destinationVpc', 'explanation_destinationVpc' - The destination VPC.
--
-- 'elasticLoadBalancerListener', 'explanation_elasticLoadBalancerListener' - The load balancer listener.
--
-- 'port', 'explanation_port' - The port.
--
-- 'acl', 'explanation_acl' - The network ACL.
--
-- 'vpcEndpoint', 'explanation_vpcEndpoint' - The VPC endpoint.
--
-- 'routeTableRoute', 'explanation_routeTableRoute' - The route table route.
--
-- 'vpnConnection', 'explanation_vpnConnection' - The VPN connection.
--
-- 'missingComponent', 'explanation_missingComponent' - The missing component.
--
-- 'addresses', 'explanation_addresses' - The IPv4 addresses, in CIDR notation.
--
-- 'classicLoadBalancerListener', 'explanation_classicLoadBalancerListener' - The listener for a Classic Load Balancer.
--
-- 'ingressRouteTable', 'explanation_ingressRouteTable' - The route table.
--
-- 'vpnGateway', 'explanation_vpnGateway' - The VPN gateway.
--
-- 'portRanges', 'explanation_portRanges' - The port ranges.
--
-- 'loadBalancerListenerPort', 'explanation_loadBalancerListenerPort' - The listener port of the load balancer.
--
-- 'vpc', 'explanation_vpc' - The component VPC.
--
-- 'loadBalancerTargetPort', 'explanation_loadBalancerTargetPort' - The target port.
newExplanation ::
  Explanation
newExplanation =
  Explanation'
    { loadBalancerTargetGroups =
        Core.Nothing,
      loadBalancerArn = Core.Nothing,
      securityGroupRule = Core.Nothing,
      customerGateway = Core.Nothing,
      availabilityZones = Core.Nothing,
      loadBalancerTargetGroup = Core.Nothing,
      explanationCode = Core.Nothing,
      address = Core.Nothing,
      natGateway = Core.Nothing,
      loadBalancerTarget = Core.Nothing,
      internetGateway = Core.Nothing,
      networkInterface = Core.Nothing,
      subnet = Core.Nothing,
      sourceVpc = Core.Nothing,
      aclRule = Core.Nothing,
      protocols = Core.Nothing,
      direction = Core.Nothing,
      securityGroups = Core.Nothing,
      prefixList = Core.Nothing,
      packetField = Core.Nothing,
      cidrs = Core.Nothing,
      component = Core.Nothing,
      state = Core.Nothing,
      routeTable = Core.Nothing,
      destination = Core.Nothing,
      subnetRouteTable = Core.Nothing,
      attachedTo = Core.Nothing,
      securityGroup = Core.Nothing,
      vpcPeeringConnection = Core.Nothing,
      destinationVpc = Core.Nothing,
      elasticLoadBalancerListener = Core.Nothing,
      port = Core.Nothing,
      acl = Core.Nothing,
      vpcEndpoint = Core.Nothing,
      routeTableRoute = Core.Nothing,
      vpnConnection = Core.Nothing,
      missingComponent = Core.Nothing,
      addresses = Core.Nothing,
      classicLoadBalancerListener = Core.Nothing,
      ingressRouteTable = Core.Nothing,
      vpnGateway = Core.Nothing,
      portRanges = Core.Nothing,
      loadBalancerListenerPort = Core.Nothing,
      vpc = Core.Nothing,
      loadBalancerTargetPort = Core.Nothing
    }

-- | The target groups.
explanation_loadBalancerTargetGroups :: Lens.Lens' Explanation (Core.Maybe [AnalysisComponent])
explanation_loadBalancerTargetGroups = Lens.lens (\Explanation' {loadBalancerTargetGroups} -> loadBalancerTargetGroups) (\s@Explanation' {} a -> s {loadBalancerTargetGroups = a} :: Explanation) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the load balancer.
explanation_loadBalancerArn :: Lens.Lens' Explanation (Core.Maybe Core.Text)
explanation_loadBalancerArn = Lens.lens (\Explanation' {loadBalancerArn} -> loadBalancerArn) (\s@Explanation' {} a -> s {loadBalancerArn = a} :: Explanation)

-- | The security group rule.
explanation_securityGroupRule :: Lens.Lens' Explanation (Core.Maybe AnalysisSecurityGroupRule)
explanation_securityGroupRule = Lens.lens (\Explanation' {securityGroupRule} -> securityGroupRule) (\s@Explanation' {} a -> s {securityGroupRule = a} :: Explanation)

-- | The customer gateway.
explanation_customerGateway :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_customerGateway = Lens.lens (\Explanation' {customerGateway} -> customerGateway) (\s@Explanation' {} a -> s {customerGateway = a} :: Explanation)

-- | The Availability Zones.
explanation_availabilityZones :: Lens.Lens' Explanation (Core.Maybe [Core.Text])
explanation_availabilityZones = Lens.lens (\Explanation' {availabilityZones} -> availabilityZones) (\s@Explanation' {} a -> s {availabilityZones = a} :: Explanation) Core.. Lens.mapping Lens._Coerce

-- | The target group.
explanation_loadBalancerTargetGroup :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_loadBalancerTargetGroup = Lens.lens (\Explanation' {loadBalancerTargetGroup} -> loadBalancerTargetGroup) (\s@Explanation' {} a -> s {loadBalancerTargetGroup = a} :: Explanation)

-- | The explanation code.
explanation_explanationCode :: Lens.Lens' Explanation (Core.Maybe Core.Text)
explanation_explanationCode = Lens.lens (\Explanation' {explanationCode} -> explanationCode) (\s@Explanation' {} a -> s {explanationCode = a} :: Explanation)

-- | The IPv4 address, in CIDR notation.
explanation_address :: Lens.Lens' Explanation (Core.Maybe Core.Text)
explanation_address = Lens.lens (\Explanation' {address} -> address) (\s@Explanation' {} a -> s {address = a} :: Explanation)

-- | The NAT gateway.
explanation_natGateway :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_natGateway = Lens.lens (\Explanation' {natGateway} -> natGateway) (\s@Explanation' {} a -> s {natGateway = a} :: Explanation)

-- | The target.
explanation_loadBalancerTarget :: Lens.Lens' Explanation (Core.Maybe AnalysisLoadBalancerTarget)
explanation_loadBalancerTarget = Lens.lens (\Explanation' {loadBalancerTarget} -> loadBalancerTarget) (\s@Explanation' {} a -> s {loadBalancerTarget = a} :: Explanation)

-- | The internet gateway.
explanation_internetGateway :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_internetGateway = Lens.lens (\Explanation' {internetGateway} -> internetGateway) (\s@Explanation' {} a -> s {internetGateway = a} :: Explanation)

-- | The network interface.
explanation_networkInterface :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_networkInterface = Lens.lens (\Explanation' {networkInterface} -> networkInterface) (\s@Explanation' {} a -> s {networkInterface = a} :: Explanation)

-- | The subnet.
explanation_subnet :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_subnet = Lens.lens (\Explanation' {subnet} -> subnet) (\s@Explanation' {} a -> s {subnet = a} :: Explanation)

-- | The source VPC.
explanation_sourceVpc :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_sourceVpc = Lens.lens (\Explanation' {sourceVpc} -> sourceVpc) (\s@Explanation' {} a -> s {sourceVpc = a} :: Explanation)

-- | The network ACL rule.
explanation_aclRule :: Lens.Lens' Explanation (Core.Maybe AnalysisAclRule)
explanation_aclRule = Lens.lens (\Explanation' {aclRule} -> aclRule) (\s@Explanation' {} a -> s {aclRule = a} :: Explanation)

-- | The protocols.
explanation_protocols :: Lens.Lens' Explanation (Core.Maybe [Core.Text])
explanation_protocols = Lens.lens (\Explanation' {protocols} -> protocols) (\s@Explanation' {} a -> s {protocols = a} :: Explanation) Core.. Lens.mapping Lens._Coerce

-- | The direction. The following are possible values:
--
-- -   egress
--
-- -   ingress
explanation_direction :: Lens.Lens' Explanation (Core.Maybe Core.Text)
explanation_direction = Lens.lens (\Explanation' {direction} -> direction) (\s@Explanation' {} a -> s {direction = a} :: Explanation)

-- | The security groups.
explanation_securityGroups :: Lens.Lens' Explanation (Core.Maybe [AnalysisComponent])
explanation_securityGroups = Lens.lens (\Explanation' {securityGroups} -> securityGroups) (\s@Explanation' {} a -> s {securityGroups = a} :: Explanation) Core.. Lens.mapping Lens._Coerce

-- | The prefix list.
explanation_prefixList :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_prefixList = Lens.lens (\Explanation' {prefixList} -> prefixList) (\s@Explanation' {} a -> s {prefixList = a} :: Explanation)

-- | The packet field.
explanation_packetField :: Lens.Lens' Explanation (Core.Maybe Core.Text)
explanation_packetField = Lens.lens (\Explanation' {packetField} -> packetField) (\s@Explanation' {} a -> s {packetField = a} :: Explanation)

-- | The CIDR ranges.
explanation_cidrs :: Lens.Lens' Explanation (Core.Maybe [Core.Text])
explanation_cidrs = Lens.lens (\Explanation' {cidrs} -> cidrs) (\s@Explanation' {} a -> s {cidrs = a} :: Explanation) Core.. Lens.mapping Lens._Coerce

-- | The component.
explanation_component :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_component = Lens.lens (\Explanation' {component} -> component) (\s@Explanation' {} a -> s {component = a} :: Explanation)

-- | The state.
explanation_state :: Lens.Lens' Explanation (Core.Maybe Core.Text)
explanation_state = Lens.lens (\Explanation' {state} -> state) (\s@Explanation' {} a -> s {state = a} :: Explanation)

-- | The route table.
explanation_routeTable :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_routeTable = Lens.lens (\Explanation' {routeTable} -> routeTable) (\s@Explanation' {} a -> s {routeTable = a} :: Explanation)

-- | The destination.
explanation_destination :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_destination = Lens.lens (\Explanation' {destination} -> destination) (\s@Explanation' {} a -> s {destination = a} :: Explanation)

-- | The route table for the subnet.
explanation_subnetRouteTable :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_subnetRouteTable = Lens.lens (\Explanation' {subnetRouteTable} -> subnetRouteTable) (\s@Explanation' {} a -> s {subnetRouteTable = a} :: Explanation)

-- | The resource to which the component is attached.
explanation_attachedTo :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_attachedTo = Lens.lens (\Explanation' {attachedTo} -> attachedTo) (\s@Explanation' {} a -> s {attachedTo = a} :: Explanation)

-- | The security group.
explanation_securityGroup :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_securityGroup = Lens.lens (\Explanation' {securityGroup} -> securityGroup) (\s@Explanation' {} a -> s {securityGroup = a} :: Explanation)

-- | The VPC peering connection.
explanation_vpcPeeringConnection :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_vpcPeeringConnection = Lens.lens (\Explanation' {vpcPeeringConnection} -> vpcPeeringConnection) (\s@Explanation' {} a -> s {vpcPeeringConnection = a} :: Explanation)

-- | The destination VPC.
explanation_destinationVpc :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_destinationVpc = Lens.lens (\Explanation' {destinationVpc} -> destinationVpc) (\s@Explanation' {} a -> s {destinationVpc = a} :: Explanation)

-- | The load balancer listener.
explanation_elasticLoadBalancerListener :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_elasticLoadBalancerListener = Lens.lens (\Explanation' {elasticLoadBalancerListener} -> elasticLoadBalancerListener) (\s@Explanation' {} a -> s {elasticLoadBalancerListener = a} :: Explanation)

-- | The port.
explanation_port :: Lens.Lens' Explanation (Core.Maybe Core.Natural)
explanation_port = Lens.lens (\Explanation' {port} -> port) (\s@Explanation' {} a -> s {port = a} :: Explanation)

-- | The network ACL.
explanation_acl :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_acl = Lens.lens (\Explanation' {acl} -> acl) (\s@Explanation' {} a -> s {acl = a} :: Explanation)

-- | The VPC endpoint.
explanation_vpcEndpoint :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_vpcEndpoint = Lens.lens (\Explanation' {vpcEndpoint} -> vpcEndpoint) (\s@Explanation' {} a -> s {vpcEndpoint = a} :: Explanation)

-- | The route table route.
explanation_routeTableRoute :: Lens.Lens' Explanation (Core.Maybe AnalysisRouteTableRoute)
explanation_routeTableRoute = Lens.lens (\Explanation' {routeTableRoute} -> routeTableRoute) (\s@Explanation' {} a -> s {routeTableRoute = a} :: Explanation)

-- | The VPN connection.
explanation_vpnConnection :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_vpnConnection = Lens.lens (\Explanation' {vpnConnection} -> vpnConnection) (\s@Explanation' {} a -> s {vpnConnection = a} :: Explanation)

-- | The missing component.
explanation_missingComponent :: Lens.Lens' Explanation (Core.Maybe Core.Text)
explanation_missingComponent = Lens.lens (\Explanation' {missingComponent} -> missingComponent) (\s@Explanation' {} a -> s {missingComponent = a} :: Explanation)

-- | The IPv4 addresses, in CIDR notation.
explanation_addresses :: Lens.Lens' Explanation (Core.Maybe [Core.Text])
explanation_addresses = Lens.lens (\Explanation' {addresses} -> addresses) (\s@Explanation' {} a -> s {addresses = a} :: Explanation) Core.. Lens.mapping Lens._Coerce

-- | The listener for a Classic Load Balancer.
explanation_classicLoadBalancerListener :: Lens.Lens' Explanation (Core.Maybe AnalysisLoadBalancerListener)
explanation_classicLoadBalancerListener = Lens.lens (\Explanation' {classicLoadBalancerListener} -> classicLoadBalancerListener) (\s@Explanation' {} a -> s {classicLoadBalancerListener = a} :: Explanation)

-- | The route table.
explanation_ingressRouteTable :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_ingressRouteTable = Lens.lens (\Explanation' {ingressRouteTable} -> ingressRouteTable) (\s@Explanation' {} a -> s {ingressRouteTable = a} :: Explanation)

-- | The VPN gateway.
explanation_vpnGateway :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_vpnGateway = Lens.lens (\Explanation' {vpnGateway} -> vpnGateway) (\s@Explanation' {} a -> s {vpnGateway = a} :: Explanation)

-- | The port ranges.
explanation_portRanges :: Lens.Lens' Explanation (Core.Maybe [PortRange])
explanation_portRanges = Lens.lens (\Explanation' {portRanges} -> portRanges) (\s@Explanation' {} a -> s {portRanges = a} :: Explanation) Core.. Lens.mapping Lens._Coerce

-- | The listener port of the load balancer.
explanation_loadBalancerListenerPort :: Lens.Lens' Explanation (Core.Maybe Core.Natural)
explanation_loadBalancerListenerPort = Lens.lens (\Explanation' {loadBalancerListenerPort} -> loadBalancerListenerPort) (\s@Explanation' {} a -> s {loadBalancerListenerPort = a} :: Explanation)

-- | The component VPC.
explanation_vpc :: Lens.Lens' Explanation (Core.Maybe AnalysisComponent)
explanation_vpc = Lens.lens (\Explanation' {vpc} -> vpc) (\s@Explanation' {} a -> s {vpc = a} :: Explanation)

-- | The target port.
explanation_loadBalancerTargetPort :: Lens.Lens' Explanation (Core.Maybe Core.Natural)
explanation_loadBalancerTargetPort = Lens.lens (\Explanation' {loadBalancerTargetPort} -> loadBalancerTargetPort) (\s@Explanation' {} a -> s {loadBalancerTargetPort = a} :: Explanation)

instance Core.FromXML Explanation where
  parseXML x =
    Explanation'
      Core.<$> ( x Core..@? "loadBalancerTargetGroupSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "loadBalancerArn")
      Core.<*> (x Core..@? "securityGroupRule")
      Core.<*> (x Core..@? "customerGateway")
      Core.<*> ( x Core..@? "availabilityZoneSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "loadBalancerTargetGroup")
      Core.<*> (x Core..@? "explanationCode")
      Core.<*> (x Core..@? "address")
      Core.<*> (x Core..@? "natGateway")
      Core.<*> (x Core..@? "loadBalancerTarget")
      Core.<*> (x Core..@? "internetGateway")
      Core.<*> (x Core..@? "networkInterface")
      Core.<*> (x Core..@? "subnet")
      Core.<*> (x Core..@? "sourceVpc")
      Core.<*> (x Core..@? "aclRule")
      Core.<*> ( x Core..@? "protocolSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "direction")
      Core.<*> ( x Core..@? "securityGroupSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "prefixList")
      Core.<*> (x Core..@? "packetField")
      Core.<*> ( x Core..@? "cidrSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "component")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "routeTable")
      Core.<*> (x Core..@? "destination")
      Core.<*> (x Core..@? "subnetRouteTable")
      Core.<*> (x Core..@? "attachedTo")
      Core.<*> (x Core..@? "securityGroup")
      Core.<*> (x Core..@? "vpcPeeringConnection")
      Core.<*> (x Core..@? "destinationVpc")
      Core.<*> (x Core..@? "elasticLoadBalancerListener")
      Core.<*> (x Core..@? "port")
      Core.<*> (x Core..@? "acl")
      Core.<*> (x Core..@? "vpcEndpoint")
      Core.<*> (x Core..@? "routeTableRoute")
      Core.<*> (x Core..@? "vpnConnection")
      Core.<*> (x Core..@? "missingComponent")
      Core.<*> ( x Core..@? "addressSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "classicLoadBalancerListener")
      Core.<*> (x Core..@? "ingressRouteTable")
      Core.<*> (x Core..@? "vpnGateway")
      Core.<*> ( x Core..@? "portRangeSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "loadBalancerListenerPort")
      Core.<*> (x Core..@? "vpc")
      Core.<*> (x Core..@? "loadBalancerTargetPort")

instance Core.Hashable Explanation

instance Core.NFData Explanation
