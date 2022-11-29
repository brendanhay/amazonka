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
-- Module      : Amazonka.EC2.Types.Explanation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Explanation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AnalysisAclRule
import Amazonka.EC2.Types.AnalysisComponent
import Amazonka.EC2.Types.AnalysisLoadBalancerListener
import Amazonka.EC2.Types.AnalysisLoadBalancerTarget
import Amazonka.EC2.Types.AnalysisRouteTableRoute
import Amazonka.EC2.Types.AnalysisSecurityGroupRule
import Amazonka.EC2.Types.PortRange
import Amazonka.EC2.Types.TransitGatewayRouteTableRoute
import qualified Amazonka.Prelude as Prelude

-- | Describes an explanation code for an unreachable path. For more
-- information, see
-- <https://docs.aws.amazon.com/vpc/latest/reachability/explanation-codes.html Reachability Analyzer explanation codes>.
--
-- /See:/ 'newExplanation' smart constructor.
data Explanation = Explanation'
  { -- | The destination.
    destination :: Prelude.Maybe AnalysisComponent,
    -- | The port.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The VPN connection.
    vpnConnection :: Prelude.Maybe AnalysisComponent,
    -- | The listener port of the load balancer.
    loadBalancerListenerPort :: Prelude.Maybe Prelude.Natural,
    -- | The transit gateway attachment.
    transitGatewayAttachment :: Prelude.Maybe AnalysisComponent,
    -- | The Amazon Web Services account for the component.
    componentAccount :: Prelude.Maybe Prelude.Text,
    -- | The missing component.
    missingComponent :: Prelude.Maybe Prelude.Text,
    -- | The route table route.
    routeTableRoute :: Prelude.Maybe AnalysisRouteTableRoute,
    -- | The route table for the subnet.
    subnetRouteTable :: Prelude.Maybe AnalysisComponent,
    -- | The component VPC.
    vpc :: Prelude.Maybe AnalysisComponent,
    -- | The NAT gateway.
    natGateway :: Prelude.Maybe AnalysisComponent,
    -- | The listener for a Classic Load Balancer.
    classicLoadBalancerListener :: Prelude.Maybe AnalysisLoadBalancerListener,
    -- | The target group.
    loadBalancerTargetGroup :: Prelude.Maybe AnalysisComponent,
    -- | The security group.
    securityGroup :: Prelude.Maybe AnalysisComponent,
    -- | The network ACL.
    acl :: Prelude.Maybe AnalysisComponent,
    -- | The route table.
    routeTable :: Prelude.Maybe AnalysisComponent,
    -- | The internet gateway.
    internetGateway :: Prelude.Maybe AnalysisComponent,
    -- | The resource to which the component is attached.
    attachedTo :: Prelude.Maybe AnalysisComponent,
    -- | The Availability Zones.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The CIDR ranges.
    cidrs :: Prelude.Maybe [Prelude.Text],
    -- | The state.
    state :: Prelude.Maybe Prelude.Text,
    -- | The target.
    loadBalancerTarget :: Prelude.Maybe AnalysisLoadBalancerTarget,
    -- | The customer gateway.
    customerGateway :: Prelude.Maybe AnalysisComponent,
    -- | The VPN gateway.
    vpnGateway :: Prelude.Maybe AnalysisComponent,
    -- | The protocols.
    protocols :: Prelude.Maybe [Prelude.Text],
    -- | The Region for the component.
    componentRegion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Maybe Prelude.Text,
    -- | The source VPC.
    sourceVpc :: Prelude.Maybe AnalysisComponent,
    -- | The route table.
    ingressRouteTable :: Prelude.Maybe AnalysisComponent,
    -- | The network ACL rule.
    aclRule :: Prelude.Maybe AnalysisAclRule,
    -- | The VPC peering connection.
    vpcPeeringConnection :: Prelude.Maybe AnalysisComponent,
    -- | The IPv4 address, in CIDR notation.
    address :: Prelude.Maybe Prelude.Text,
    -- | The target port.
    loadBalancerTargetPort :: Prelude.Maybe Prelude.Natural,
    -- | The security groups.
    securityGroups :: Prelude.Maybe [AnalysisComponent],
    -- | The transit gateway.
    transitGateway :: Prelude.Maybe AnalysisComponent,
    -- | The destination VPC.
    destinationVpc :: Prelude.Maybe AnalysisComponent,
    -- | The IPv4 addresses, in CIDR notation.
    addresses :: Prelude.Maybe [Prelude.Text],
    -- | The prefix list.
    prefixList :: Prelude.Maybe AnalysisComponent,
    -- | The load balancer listener.
    elasticLoadBalancerListener :: Prelude.Maybe AnalysisComponent,
    -- | The subnet.
    subnet :: Prelude.Maybe AnalysisComponent,
    -- | The packet field.
    packetField :: Prelude.Maybe Prelude.Text,
    -- | The explanation code.
    explanationCode :: Prelude.Maybe Prelude.Text,
    -- | The network interface.
    networkInterface :: Prelude.Maybe AnalysisComponent,
    -- | The direction. The following are the possible values:
    --
    -- -   egress
    --
    -- -   ingress
    direction :: Prelude.Maybe Prelude.Text,
    -- | The transit gateway route table route.
    transitGatewayRouteTableRoute :: Prelude.Maybe TransitGatewayRouteTableRoute,
    -- | The target groups.
    loadBalancerTargetGroups :: Prelude.Maybe [AnalysisComponent],
    -- | The port ranges.
    portRanges :: Prelude.Maybe [PortRange],
    -- | The VPC endpoint.
    vpcEndpoint :: Prelude.Maybe AnalysisComponent,
    -- | The transit gateway route table.
    transitGatewayRouteTable :: Prelude.Maybe AnalysisComponent,
    -- | The component.
    component :: Prelude.Maybe AnalysisComponent,
    -- | The security group rule.
    securityGroupRule :: Prelude.Maybe AnalysisSecurityGroupRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Explanation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'explanation_destination' - The destination.
--
-- 'port', 'explanation_port' - The port.
--
-- 'vpnConnection', 'explanation_vpnConnection' - The VPN connection.
--
-- 'loadBalancerListenerPort', 'explanation_loadBalancerListenerPort' - The listener port of the load balancer.
--
-- 'transitGatewayAttachment', 'explanation_transitGatewayAttachment' - The transit gateway attachment.
--
-- 'componentAccount', 'explanation_componentAccount' - The Amazon Web Services account for the component.
--
-- 'missingComponent', 'explanation_missingComponent' - The missing component.
--
-- 'routeTableRoute', 'explanation_routeTableRoute' - The route table route.
--
-- 'subnetRouteTable', 'explanation_subnetRouteTable' - The route table for the subnet.
--
-- 'vpc', 'explanation_vpc' - The component VPC.
--
-- 'natGateway', 'explanation_natGateway' - The NAT gateway.
--
-- 'classicLoadBalancerListener', 'explanation_classicLoadBalancerListener' - The listener for a Classic Load Balancer.
--
-- 'loadBalancerTargetGroup', 'explanation_loadBalancerTargetGroup' - The target group.
--
-- 'securityGroup', 'explanation_securityGroup' - The security group.
--
-- 'acl', 'explanation_acl' - The network ACL.
--
-- 'routeTable', 'explanation_routeTable' - The route table.
--
-- 'internetGateway', 'explanation_internetGateway' - The internet gateway.
--
-- 'attachedTo', 'explanation_attachedTo' - The resource to which the component is attached.
--
-- 'availabilityZones', 'explanation_availabilityZones' - The Availability Zones.
--
-- 'cidrs', 'explanation_cidrs' - The CIDR ranges.
--
-- 'state', 'explanation_state' - The state.
--
-- 'loadBalancerTarget', 'explanation_loadBalancerTarget' - The target.
--
-- 'customerGateway', 'explanation_customerGateway' - The customer gateway.
--
-- 'vpnGateway', 'explanation_vpnGateway' - The VPN gateway.
--
-- 'protocols', 'explanation_protocols' - The protocols.
--
-- 'componentRegion', 'explanation_componentRegion' - The Region for the component.
--
-- 'loadBalancerArn', 'explanation_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'sourceVpc', 'explanation_sourceVpc' - The source VPC.
--
-- 'ingressRouteTable', 'explanation_ingressRouteTable' - The route table.
--
-- 'aclRule', 'explanation_aclRule' - The network ACL rule.
--
-- 'vpcPeeringConnection', 'explanation_vpcPeeringConnection' - The VPC peering connection.
--
-- 'address', 'explanation_address' - The IPv4 address, in CIDR notation.
--
-- 'loadBalancerTargetPort', 'explanation_loadBalancerTargetPort' - The target port.
--
-- 'securityGroups', 'explanation_securityGroups' - The security groups.
--
-- 'transitGateway', 'explanation_transitGateway' - The transit gateway.
--
-- 'destinationVpc', 'explanation_destinationVpc' - The destination VPC.
--
-- 'addresses', 'explanation_addresses' - The IPv4 addresses, in CIDR notation.
--
-- 'prefixList', 'explanation_prefixList' - The prefix list.
--
-- 'elasticLoadBalancerListener', 'explanation_elasticLoadBalancerListener' - The load balancer listener.
--
-- 'subnet', 'explanation_subnet' - The subnet.
--
-- 'packetField', 'explanation_packetField' - The packet field.
--
-- 'explanationCode', 'explanation_explanationCode' - The explanation code.
--
-- 'networkInterface', 'explanation_networkInterface' - The network interface.
--
-- 'direction', 'explanation_direction' - The direction. The following are the possible values:
--
-- -   egress
--
-- -   ingress
--
-- 'transitGatewayRouteTableRoute', 'explanation_transitGatewayRouteTableRoute' - The transit gateway route table route.
--
-- 'loadBalancerTargetGroups', 'explanation_loadBalancerTargetGroups' - The target groups.
--
-- 'portRanges', 'explanation_portRanges' - The port ranges.
--
-- 'vpcEndpoint', 'explanation_vpcEndpoint' - The VPC endpoint.
--
-- 'transitGatewayRouteTable', 'explanation_transitGatewayRouteTable' - The transit gateway route table.
--
-- 'component', 'explanation_component' - The component.
--
-- 'securityGroupRule', 'explanation_securityGroupRule' - The security group rule.
newExplanation ::
  Explanation
newExplanation =
  Explanation'
    { destination = Prelude.Nothing,
      port = Prelude.Nothing,
      vpnConnection = Prelude.Nothing,
      loadBalancerListenerPort = Prelude.Nothing,
      transitGatewayAttachment = Prelude.Nothing,
      componentAccount = Prelude.Nothing,
      missingComponent = Prelude.Nothing,
      routeTableRoute = Prelude.Nothing,
      subnetRouteTable = Prelude.Nothing,
      vpc = Prelude.Nothing,
      natGateway = Prelude.Nothing,
      classicLoadBalancerListener = Prelude.Nothing,
      loadBalancerTargetGroup = Prelude.Nothing,
      securityGroup = Prelude.Nothing,
      acl = Prelude.Nothing,
      routeTable = Prelude.Nothing,
      internetGateway = Prelude.Nothing,
      attachedTo = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      cidrs = Prelude.Nothing,
      state = Prelude.Nothing,
      loadBalancerTarget = Prelude.Nothing,
      customerGateway = Prelude.Nothing,
      vpnGateway = Prelude.Nothing,
      protocols = Prelude.Nothing,
      componentRegion = Prelude.Nothing,
      loadBalancerArn = Prelude.Nothing,
      sourceVpc = Prelude.Nothing,
      ingressRouteTable = Prelude.Nothing,
      aclRule = Prelude.Nothing,
      vpcPeeringConnection = Prelude.Nothing,
      address = Prelude.Nothing,
      loadBalancerTargetPort = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      transitGateway = Prelude.Nothing,
      destinationVpc = Prelude.Nothing,
      addresses = Prelude.Nothing,
      prefixList = Prelude.Nothing,
      elasticLoadBalancerListener = Prelude.Nothing,
      subnet = Prelude.Nothing,
      packetField = Prelude.Nothing,
      explanationCode = Prelude.Nothing,
      networkInterface = Prelude.Nothing,
      direction = Prelude.Nothing,
      transitGatewayRouteTableRoute = Prelude.Nothing,
      loadBalancerTargetGroups = Prelude.Nothing,
      portRanges = Prelude.Nothing,
      vpcEndpoint = Prelude.Nothing,
      transitGatewayRouteTable = Prelude.Nothing,
      component = Prelude.Nothing,
      securityGroupRule = Prelude.Nothing
    }

-- | The destination.
explanation_destination :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_destination = Lens.lens (\Explanation' {destination} -> destination) (\s@Explanation' {} a -> s {destination = a} :: Explanation)

-- | The port.
explanation_port :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Natural)
explanation_port = Lens.lens (\Explanation' {port} -> port) (\s@Explanation' {} a -> s {port = a} :: Explanation)

-- | The VPN connection.
explanation_vpnConnection :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_vpnConnection = Lens.lens (\Explanation' {vpnConnection} -> vpnConnection) (\s@Explanation' {} a -> s {vpnConnection = a} :: Explanation)

-- | The listener port of the load balancer.
explanation_loadBalancerListenerPort :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Natural)
explanation_loadBalancerListenerPort = Lens.lens (\Explanation' {loadBalancerListenerPort} -> loadBalancerListenerPort) (\s@Explanation' {} a -> s {loadBalancerListenerPort = a} :: Explanation)

-- | The transit gateway attachment.
explanation_transitGatewayAttachment :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_transitGatewayAttachment = Lens.lens (\Explanation' {transitGatewayAttachment} -> transitGatewayAttachment) (\s@Explanation' {} a -> s {transitGatewayAttachment = a} :: Explanation)

-- | The Amazon Web Services account for the component.
explanation_componentAccount :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_componentAccount = Lens.lens (\Explanation' {componentAccount} -> componentAccount) (\s@Explanation' {} a -> s {componentAccount = a} :: Explanation)

-- | The missing component.
explanation_missingComponent :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_missingComponent = Lens.lens (\Explanation' {missingComponent} -> missingComponent) (\s@Explanation' {} a -> s {missingComponent = a} :: Explanation)

-- | The route table route.
explanation_routeTableRoute :: Lens.Lens' Explanation (Prelude.Maybe AnalysisRouteTableRoute)
explanation_routeTableRoute = Lens.lens (\Explanation' {routeTableRoute} -> routeTableRoute) (\s@Explanation' {} a -> s {routeTableRoute = a} :: Explanation)

-- | The route table for the subnet.
explanation_subnetRouteTable :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_subnetRouteTable = Lens.lens (\Explanation' {subnetRouteTable} -> subnetRouteTable) (\s@Explanation' {} a -> s {subnetRouteTable = a} :: Explanation)

-- | The component VPC.
explanation_vpc :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_vpc = Lens.lens (\Explanation' {vpc} -> vpc) (\s@Explanation' {} a -> s {vpc = a} :: Explanation)

-- | The NAT gateway.
explanation_natGateway :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_natGateway = Lens.lens (\Explanation' {natGateway} -> natGateway) (\s@Explanation' {} a -> s {natGateway = a} :: Explanation)

-- | The listener for a Classic Load Balancer.
explanation_classicLoadBalancerListener :: Lens.Lens' Explanation (Prelude.Maybe AnalysisLoadBalancerListener)
explanation_classicLoadBalancerListener = Lens.lens (\Explanation' {classicLoadBalancerListener} -> classicLoadBalancerListener) (\s@Explanation' {} a -> s {classicLoadBalancerListener = a} :: Explanation)

-- | The target group.
explanation_loadBalancerTargetGroup :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_loadBalancerTargetGroup = Lens.lens (\Explanation' {loadBalancerTargetGroup} -> loadBalancerTargetGroup) (\s@Explanation' {} a -> s {loadBalancerTargetGroup = a} :: Explanation)

-- | The security group.
explanation_securityGroup :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_securityGroup = Lens.lens (\Explanation' {securityGroup} -> securityGroup) (\s@Explanation' {} a -> s {securityGroup = a} :: Explanation)

-- | The network ACL.
explanation_acl :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_acl = Lens.lens (\Explanation' {acl} -> acl) (\s@Explanation' {} a -> s {acl = a} :: Explanation)

-- | The route table.
explanation_routeTable :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_routeTable = Lens.lens (\Explanation' {routeTable} -> routeTable) (\s@Explanation' {} a -> s {routeTable = a} :: Explanation)

-- | The internet gateway.
explanation_internetGateway :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_internetGateway = Lens.lens (\Explanation' {internetGateway} -> internetGateway) (\s@Explanation' {} a -> s {internetGateway = a} :: Explanation)

-- | The resource to which the component is attached.
explanation_attachedTo :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_attachedTo = Lens.lens (\Explanation' {attachedTo} -> attachedTo) (\s@Explanation' {} a -> s {attachedTo = a} :: Explanation)

-- | The Availability Zones.
explanation_availabilityZones :: Lens.Lens' Explanation (Prelude.Maybe [Prelude.Text])
explanation_availabilityZones = Lens.lens (\Explanation' {availabilityZones} -> availabilityZones) (\s@Explanation' {} a -> s {availabilityZones = a} :: Explanation) Prelude.. Lens.mapping Lens.coerced

-- | The CIDR ranges.
explanation_cidrs :: Lens.Lens' Explanation (Prelude.Maybe [Prelude.Text])
explanation_cidrs = Lens.lens (\Explanation' {cidrs} -> cidrs) (\s@Explanation' {} a -> s {cidrs = a} :: Explanation) Prelude.. Lens.mapping Lens.coerced

-- | The state.
explanation_state :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_state = Lens.lens (\Explanation' {state} -> state) (\s@Explanation' {} a -> s {state = a} :: Explanation)

-- | The target.
explanation_loadBalancerTarget :: Lens.Lens' Explanation (Prelude.Maybe AnalysisLoadBalancerTarget)
explanation_loadBalancerTarget = Lens.lens (\Explanation' {loadBalancerTarget} -> loadBalancerTarget) (\s@Explanation' {} a -> s {loadBalancerTarget = a} :: Explanation)

-- | The customer gateway.
explanation_customerGateway :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_customerGateway = Lens.lens (\Explanation' {customerGateway} -> customerGateway) (\s@Explanation' {} a -> s {customerGateway = a} :: Explanation)

-- | The VPN gateway.
explanation_vpnGateway :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_vpnGateway = Lens.lens (\Explanation' {vpnGateway} -> vpnGateway) (\s@Explanation' {} a -> s {vpnGateway = a} :: Explanation)

-- | The protocols.
explanation_protocols :: Lens.Lens' Explanation (Prelude.Maybe [Prelude.Text])
explanation_protocols = Lens.lens (\Explanation' {protocols} -> protocols) (\s@Explanation' {} a -> s {protocols = a} :: Explanation) Prelude.. Lens.mapping Lens.coerced

-- | The Region for the component.
explanation_componentRegion :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_componentRegion = Lens.lens (\Explanation' {componentRegion} -> componentRegion) (\s@Explanation' {} a -> s {componentRegion = a} :: Explanation)

-- | The Amazon Resource Name (ARN) of the load balancer.
explanation_loadBalancerArn :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_loadBalancerArn = Lens.lens (\Explanation' {loadBalancerArn} -> loadBalancerArn) (\s@Explanation' {} a -> s {loadBalancerArn = a} :: Explanation)

-- | The source VPC.
explanation_sourceVpc :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_sourceVpc = Lens.lens (\Explanation' {sourceVpc} -> sourceVpc) (\s@Explanation' {} a -> s {sourceVpc = a} :: Explanation)

-- | The route table.
explanation_ingressRouteTable :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_ingressRouteTable = Lens.lens (\Explanation' {ingressRouteTable} -> ingressRouteTable) (\s@Explanation' {} a -> s {ingressRouteTable = a} :: Explanation)

-- | The network ACL rule.
explanation_aclRule :: Lens.Lens' Explanation (Prelude.Maybe AnalysisAclRule)
explanation_aclRule = Lens.lens (\Explanation' {aclRule} -> aclRule) (\s@Explanation' {} a -> s {aclRule = a} :: Explanation)

-- | The VPC peering connection.
explanation_vpcPeeringConnection :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_vpcPeeringConnection = Lens.lens (\Explanation' {vpcPeeringConnection} -> vpcPeeringConnection) (\s@Explanation' {} a -> s {vpcPeeringConnection = a} :: Explanation)

-- | The IPv4 address, in CIDR notation.
explanation_address :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_address = Lens.lens (\Explanation' {address} -> address) (\s@Explanation' {} a -> s {address = a} :: Explanation)

-- | The target port.
explanation_loadBalancerTargetPort :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Natural)
explanation_loadBalancerTargetPort = Lens.lens (\Explanation' {loadBalancerTargetPort} -> loadBalancerTargetPort) (\s@Explanation' {} a -> s {loadBalancerTargetPort = a} :: Explanation)

-- | The security groups.
explanation_securityGroups :: Lens.Lens' Explanation (Prelude.Maybe [AnalysisComponent])
explanation_securityGroups = Lens.lens (\Explanation' {securityGroups} -> securityGroups) (\s@Explanation' {} a -> s {securityGroups = a} :: Explanation) Prelude.. Lens.mapping Lens.coerced

-- | The transit gateway.
explanation_transitGateway :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_transitGateway = Lens.lens (\Explanation' {transitGateway} -> transitGateway) (\s@Explanation' {} a -> s {transitGateway = a} :: Explanation)

-- | The destination VPC.
explanation_destinationVpc :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_destinationVpc = Lens.lens (\Explanation' {destinationVpc} -> destinationVpc) (\s@Explanation' {} a -> s {destinationVpc = a} :: Explanation)

-- | The IPv4 addresses, in CIDR notation.
explanation_addresses :: Lens.Lens' Explanation (Prelude.Maybe [Prelude.Text])
explanation_addresses = Lens.lens (\Explanation' {addresses} -> addresses) (\s@Explanation' {} a -> s {addresses = a} :: Explanation) Prelude.. Lens.mapping Lens.coerced

-- | The prefix list.
explanation_prefixList :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_prefixList = Lens.lens (\Explanation' {prefixList} -> prefixList) (\s@Explanation' {} a -> s {prefixList = a} :: Explanation)

-- | The load balancer listener.
explanation_elasticLoadBalancerListener :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_elasticLoadBalancerListener = Lens.lens (\Explanation' {elasticLoadBalancerListener} -> elasticLoadBalancerListener) (\s@Explanation' {} a -> s {elasticLoadBalancerListener = a} :: Explanation)

-- | The subnet.
explanation_subnet :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_subnet = Lens.lens (\Explanation' {subnet} -> subnet) (\s@Explanation' {} a -> s {subnet = a} :: Explanation)

-- | The packet field.
explanation_packetField :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_packetField = Lens.lens (\Explanation' {packetField} -> packetField) (\s@Explanation' {} a -> s {packetField = a} :: Explanation)

-- | The explanation code.
explanation_explanationCode :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_explanationCode = Lens.lens (\Explanation' {explanationCode} -> explanationCode) (\s@Explanation' {} a -> s {explanationCode = a} :: Explanation)

-- | The network interface.
explanation_networkInterface :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_networkInterface = Lens.lens (\Explanation' {networkInterface} -> networkInterface) (\s@Explanation' {} a -> s {networkInterface = a} :: Explanation)

-- | The direction. The following are the possible values:
--
-- -   egress
--
-- -   ingress
explanation_direction :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_direction = Lens.lens (\Explanation' {direction} -> direction) (\s@Explanation' {} a -> s {direction = a} :: Explanation)

-- | The transit gateway route table route.
explanation_transitGatewayRouteTableRoute :: Lens.Lens' Explanation (Prelude.Maybe TransitGatewayRouteTableRoute)
explanation_transitGatewayRouteTableRoute = Lens.lens (\Explanation' {transitGatewayRouteTableRoute} -> transitGatewayRouteTableRoute) (\s@Explanation' {} a -> s {transitGatewayRouteTableRoute = a} :: Explanation)

-- | The target groups.
explanation_loadBalancerTargetGroups :: Lens.Lens' Explanation (Prelude.Maybe [AnalysisComponent])
explanation_loadBalancerTargetGroups = Lens.lens (\Explanation' {loadBalancerTargetGroups} -> loadBalancerTargetGroups) (\s@Explanation' {} a -> s {loadBalancerTargetGroups = a} :: Explanation) Prelude.. Lens.mapping Lens.coerced

-- | The port ranges.
explanation_portRanges :: Lens.Lens' Explanation (Prelude.Maybe [PortRange])
explanation_portRanges = Lens.lens (\Explanation' {portRanges} -> portRanges) (\s@Explanation' {} a -> s {portRanges = a} :: Explanation) Prelude.. Lens.mapping Lens.coerced

-- | The VPC endpoint.
explanation_vpcEndpoint :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_vpcEndpoint = Lens.lens (\Explanation' {vpcEndpoint} -> vpcEndpoint) (\s@Explanation' {} a -> s {vpcEndpoint = a} :: Explanation)

-- | The transit gateway route table.
explanation_transitGatewayRouteTable :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_transitGatewayRouteTable = Lens.lens (\Explanation' {transitGatewayRouteTable} -> transitGatewayRouteTable) (\s@Explanation' {} a -> s {transitGatewayRouteTable = a} :: Explanation)

-- | The component.
explanation_component :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_component = Lens.lens (\Explanation' {component} -> component) (\s@Explanation' {} a -> s {component = a} :: Explanation)

-- | The security group rule.
explanation_securityGroupRule :: Lens.Lens' Explanation (Prelude.Maybe AnalysisSecurityGroupRule)
explanation_securityGroupRule = Lens.lens (\Explanation' {securityGroupRule} -> securityGroupRule) (\s@Explanation' {} a -> s {securityGroupRule = a} :: Explanation)

instance Core.FromXML Explanation where
  parseXML x =
    Explanation'
      Prelude.<$> (x Core..@? "destination")
      Prelude.<*> (x Core..@? "port")
      Prelude.<*> (x Core..@? "vpnConnection")
      Prelude.<*> (x Core..@? "loadBalancerListenerPort")
      Prelude.<*> (x Core..@? "transitGatewayAttachment")
      Prelude.<*> (x Core..@? "componentAccount")
      Prelude.<*> (x Core..@? "missingComponent")
      Prelude.<*> (x Core..@? "routeTableRoute")
      Prelude.<*> (x Core..@? "subnetRouteTable")
      Prelude.<*> (x Core..@? "vpc")
      Prelude.<*> (x Core..@? "natGateway")
      Prelude.<*> (x Core..@? "classicLoadBalancerListener")
      Prelude.<*> (x Core..@? "loadBalancerTargetGroup")
      Prelude.<*> (x Core..@? "securityGroup")
      Prelude.<*> (x Core..@? "acl")
      Prelude.<*> (x Core..@? "routeTable")
      Prelude.<*> (x Core..@? "internetGateway")
      Prelude.<*> (x Core..@? "attachedTo")
      Prelude.<*> ( x Core..@? "availabilityZoneSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "cidrSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "state")
      Prelude.<*> (x Core..@? "loadBalancerTarget")
      Prelude.<*> (x Core..@? "customerGateway")
      Prelude.<*> (x Core..@? "vpnGateway")
      Prelude.<*> ( x Core..@? "protocolSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "componentRegion")
      Prelude.<*> (x Core..@? "loadBalancerArn")
      Prelude.<*> (x Core..@? "sourceVpc")
      Prelude.<*> (x Core..@? "ingressRouteTable")
      Prelude.<*> (x Core..@? "aclRule")
      Prelude.<*> (x Core..@? "vpcPeeringConnection")
      Prelude.<*> (x Core..@? "address")
      Prelude.<*> (x Core..@? "loadBalancerTargetPort")
      Prelude.<*> ( x Core..@? "securityGroupSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "transitGateway")
      Prelude.<*> (x Core..@? "destinationVpc")
      Prelude.<*> ( x Core..@? "addressSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "prefixList")
      Prelude.<*> (x Core..@? "elasticLoadBalancerListener")
      Prelude.<*> (x Core..@? "subnet")
      Prelude.<*> (x Core..@? "packetField")
      Prelude.<*> (x Core..@? "explanationCode")
      Prelude.<*> (x Core..@? "networkInterface")
      Prelude.<*> (x Core..@? "direction")
      Prelude.<*> (x Core..@? "transitGatewayRouteTableRoute")
      Prelude.<*> ( x Core..@? "loadBalancerTargetGroupSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "portRangeSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "vpcEndpoint")
      Prelude.<*> (x Core..@? "transitGatewayRouteTable")
      Prelude.<*> (x Core..@? "component")
      Prelude.<*> (x Core..@? "securityGroupRule")

instance Prelude.Hashable Explanation where
  hashWithSalt _salt Explanation' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` vpnConnection
      `Prelude.hashWithSalt` loadBalancerListenerPort
      `Prelude.hashWithSalt` transitGatewayAttachment
      `Prelude.hashWithSalt` componentAccount
      `Prelude.hashWithSalt` missingComponent
      `Prelude.hashWithSalt` routeTableRoute
      `Prelude.hashWithSalt` subnetRouteTable
      `Prelude.hashWithSalt` vpc
      `Prelude.hashWithSalt` natGateway
      `Prelude.hashWithSalt` classicLoadBalancerListener
      `Prelude.hashWithSalt` loadBalancerTargetGroup
      `Prelude.hashWithSalt` securityGroup
      `Prelude.hashWithSalt` acl
      `Prelude.hashWithSalt` routeTable
      `Prelude.hashWithSalt` internetGateway
      `Prelude.hashWithSalt` attachedTo
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` cidrs
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` loadBalancerTarget
      `Prelude.hashWithSalt` customerGateway
      `Prelude.hashWithSalt` vpnGateway
      `Prelude.hashWithSalt` protocols
      `Prelude.hashWithSalt` componentRegion
      `Prelude.hashWithSalt` loadBalancerArn
      `Prelude.hashWithSalt` sourceVpc
      `Prelude.hashWithSalt` ingressRouteTable
      `Prelude.hashWithSalt` aclRule
      `Prelude.hashWithSalt` vpcPeeringConnection
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` loadBalancerTargetPort
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` transitGateway
      `Prelude.hashWithSalt` destinationVpc
      `Prelude.hashWithSalt` addresses
      `Prelude.hashWithSalt` prefixList
      `Prelude.hashWithSalt` elasticLoadBalancerListener
      `Prelude.hashWithSalt` subnet
      `Prelude.hashWithSalt` packetField
      `Prelude.hashWithSalt` explanationCode
      `Prelude.hashWithSalt` networkInterface
      `Prelude.hashWithSalt` direction
      `Prelude.hashWithSalt` transitGatewayRouteTableRoute
      `Prelude.hashWithSalt` loadBalancerTargetGroups
      `Prelude.hashWithSalt` portRanges
      `Prelude.hashWithSalt` vpcEndpoint
      `Prelude.hashWithSalt` transitGatewayRouteTable
      `Prelude.hashWithSalt` component
      `Prelude.hashWithSalt` securityGroupRule

instance Prelude.NFData Explanation where
  rnf Explanation' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf vpnConnection
      `Prelude.seq` Prelude.rnf loadBalancerListenerPort
      `Prelude.seq` Prelude.rnf transitGatewayAttachment
      `Prelude.seq` Prelude.rnf componentAccount
      `Prelude.seq` Prelude.rnf missingComponent
      `Prelude.seq` Prelude.rnf routeTableRoute
      `Prelude.seq` Prelude.rnf subnetRouteTable
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf natGateway
      `Prelude.seq` Prelude.rnf classicLoadBalancerListener
      `Prelude.seq` Prelude.rnf loadBalancerTargetGroup
      `Prelude.seq` Prelude.rnf securityGroup
      `Prelude.seq` Prelude.rnf acl
      `Prelude.seq` Prelude.rnf routeTable
      `Prelude.seq` Prelude.rnf internetGateway
      `Prelude.seq` Prelude.rnf attachedTo
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf cidrs
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf
        loadBalancerTarget
      `Prelude.seq` Prelude.rnf
        customerGateway
      `Prelude.seq` Prelude.rnf vpnGateway
      `Prelude.seq` Prelude.rnf
        protocols
      `Prelude.seq` Prelude.rnf
        componentRegion
      `Prelude.seq` Prelude.rnf
        loadBalancerArn
      `Prelude.seq` Prelude.rnf
        sourceVpc
      `Prelude.seq` Prelude.rnf
        ingressRouteTable
      `Prelude.seq` Prelude.rnf
        aclRule
      `Prelude.seq` Prelude.rnf
        vpcPeeringConnection
      `Prelude.seq` Prelude.rnf
        address
      `Prelude.seq` Prelude.rnf
        loadBalancerTargetPort
      `Prelude.seq` Prelude.rnf
        securityGroups
      `Prelude.seq` Prelude.rnf
        transitGateway
      `Prelude.seq` Prelude.rnf
        destinationVpc
      `Prelude.seq` Prelude.rnf
        addresses
      `Prelude.seq` Prelude.rnf
        prefixList
      `Prelude.seq` Prelude.rnf
        elasticLoadBalancerListener
      `Prelude.seq` Prelude.rnf
        subnet
      `Prelude.seq` Prelude.rnf
        packetField
      `Prelude.seq` Prelude.rnf
        explanationCode
      `Prelude.seq` Prelude.rnf
        networkInterface
      `Prelude.seq` Prelude.rnf
        direction
      `Prelude.seq` Prelude.rnf
        transitGatewayRouteTableRoute
      `Prelude.seq` Prelude.rnf
        loadBalancerTargetGroups
      `Prelude.seq` Prelude.rnf
        portRanges
      `Prelude.seq` Prelude.rnf
        vpcEndpoint
      `Prelude.seq` Prelude.rnf
        transitGatewayRouteTable
      `Prelude.seq` Prelude.rnf
        component
      `Prelude.seq` Prelude.rnf
        securityGroupRule
