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
-- Module      : Network.AWS.EC2.Types.Explanation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Explanation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AnalysisAclRule
import Network.AWS.EC2.Types.AnalysisComponent
import Network.AWS.EC2.Types.AnalysisLoadBalancerListener
import Network.AWS.EC2.Types.AnalysisLoadBalancerTarget
import Network.AWS.EC2.Types.AnalysisRouteTableRoute
import Network.AWS.EC2.Types.AnalysisSecurityGroupRule
import Network.AWS.EC2.Types.PortRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an explanation code for an unreachable path. For more
-- information, see
-- <https://docs.aws.amazon.com/vpc/latest/reachability/explanation-codes.html Reachability Analyzer explanation codes>.
--
-- /See:/ 'newExplanation' smart constructor.
data Explanation = Explanation'
  { -- | The target groups.
    loadBalancerTargetGroups :: Prelude.Maybe [AnalysisComponent],
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Maybe Prelude.Text,
    -- | The security group rule.
    securityGroupRule :: Prelude.Maybe AnalysisSecurityGroupRule,
    -- | The customer gateway.
    customerGateway :: Prelude.Maybe AnalysisComponent,
    -- | The Availability Zones.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The target group.
    loadBalancerTargetGroup :: Prelude.Maybe AnalysisComponent,
    -- | The explanation code.
    explanationCode :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 address, in CIDR notation.
    address :: Prelude.Maybe Prelude.Text,
    -- | The NAT gateway.
    natGateway :: Prelude.Maybe AnalysisComponent,
    -- | The target.
    loadBalancerTarget :: Prelude.Maybe AnalysisLoadBalancerTarget,
    -- | The internet gateway.
    internetGateway :: Prelude.Maybe AnalysisComponent,
    -- | The network interface.
    networkInterface :: Prelude.Maybe AnalysisComponent,
    -- | The subnet.
    subnet :: Prelude.Maybe AnalysisComponent,
    -- | The source VPC.
    sourceVpc :: Prelude.Maybe AnalysisComponent,
    -- | The network ACL rule.
    aclRule :: Prelude.Maybe AnalysisAclRule,
    -- | The protocols.
    protocols :: Prelude.Maybe [Prelude.Text],
    -- | The direction. The following are possible values:
    --
    -- -   egress
    --
    -- -   ingress
    direction :: Prelude.Maybe Prelude.Text,
    -- | The security groups.
    securityGroups :: Prelude.Maybe [AnalysisComponent],
    -- | The prefix list.
    prefixList :: Prelude.Maybe AnalysisComponent,
    -- | The packet field.
    packetField :: Prelude.Maybe Prelude.Text,
    -- | The CIDR ranges.
    cidrs :: Prelude.Maybe [Prelude.Text],
    -- | The component.
    component :: Prelude.Maybe AnalysisComponent,
    -- | The state.
    state :: Prelude.Maybe Prelude.Text,
    -- | The route table.
    routeTable :: Prelude.Maybe AnalysisComponent,
    -- | The destination.
    destination :: Prelude.Maybe AnalysisComponent,
    -- | The route table for the subnet.
    subnetRouteTable :: Prelude.Maybe AnalysisComponent,
    -- | The resource to which the component is attached.
    attachedTo :: Prelude.Maybe AnalysisComponent,
    -- | The security group.
    securityGroup :: Prelude.Maybe AnalysisComponent,
    -- | The VPC peering connection.
    vpcPeeringConnection :: Prelude.Maybe AnalysisComponent,
    -- | The destination VPC.
    destinationVpc :: Prelude.Maybe AnalysisComponent,
    -- | The load balancer listener.
    elasticLoadBalancerListener :: Prelude.Maybe AnalysisComponent,
    -- | The port.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The network ACL.
    acl :: Prelude.Maybe AnalysisComponent,
    -- | The VPC endpoint.
    vpcEndpoint :: Prelude.Maybe AnalysisComponent,
    -- | The route table route.
    routeTableRoute :: Prelude.Maybe AnalysisRouteTableRoute,
    -- | The VPN connection.
    vpnConnection :: Prelude.Maybe AnalysisComponent,
    -- | The missing component.
    missingComponent :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 addresses, in CIDR notation.
    addresses :: Prelude.Maybe [Prelude.Text],
    -- | The listener for a Classic Load Balancer.
    classicLoadBalancerListener :: Prelude.Maybe AnalysisLoadBalancerListener,
    -- | The route table.
    ingressRouteTable :: Prelude.Maybe AnalysisComponent,
    -- | The VPN gateway.
    vpnGateway :: Prelude.Maybe AnalysisComponent,
    -- | The port ranges.
    portRanges :: Prelude.Maybe [PortRange],
    -- | The listener port of the load balancer.
    loadBalancerListenerPort :: Prelude.Maybe Prelude.Natural,
    -- | The component VPC.
    vpc :: Prelude.Maybe AnalysisComponent,
    -- | The target port.
    loadBalancerTargetPort :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      loadBalancerArn = Prelude.Nothing,
      securityGroupRule = Prelude.Nothing,
      customerGateway = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      loadBalancerTargetGroup = Prelude.Nothing,
      explanationCode = Prelude.Nothing,
      address = Prelude.Nothing,
      natGateway = Prelude.Nothing,
      loadBalancerTarget = Prelude.Nothing,
      internetGateway = Prelude.Nothing,
      networkInterface = Prelude.Nothing,
      subnet = Prelude.Nothing,
      sourceVpc = Prelude.Nothing,
      aclRule = Prelude.Nothing,
      protocols = Prelude.Nothing,
      direction = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      prefixList = Prelude.Nothing,
      packetField = Prelude.Nothing,
      cidrs = Prelude.Nothing,
      component = Prelude.Nothing,
      state = Prelude.Nothing,
      routeTable = Prelude.Nothing,
      destination = Prelude.Nothing,
      subnetRouteTable = Prelude.Nothing,
      attachedTo = Prelude.Nothing,
      securityGroup = Prelude.Nothing,
      vpcPeeringConnection = Prelude.Nothing,
      destinationVpc = Prelude.Nothing,
      elasticLoadBalancerListener = Prelude.Nothing,
      port = Prelude.Nothing,
      acl = Prelude.Nothing,
      vpcEndpoint = Prelude.Nothing,
      routeTableRoute = Prelude.Nothing,
      vpnConnection = Prelude.Nothing,
      missingComponent = Prelude.Nothing,
      addresses = Prelude.Nothing,
      classicLoadBalancerListener = Prelude.Nothing,
      ingressRouteTable = Prelude.Nothing,
      vpnGateway = Prelude.Nothing,
      portRanges = Prelude.Nothing,
      loadBalancerListenerPort = Prelude.Nothing,
      vpc = Prelude.Nothing,
      loadBalancerTargetPort = Prelude.Nothing
    }

-- | The target groups.
explanation_loadBalancerTargetGroups :: Lens.Lens' Explanation (Prelude.Maybe [AnalysisComponent])
explanation_loadBalancerTargetGroups = Lens.lens (\Explanation' {loadBalancerTargetGroups} -> loadBalancerTargetGroups) (\s@Explanation' {} a -> s {loadBalancerTargetGroups = a} :: Explanation) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the load balancer.
explanation_loadBalancerArn :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_loadBalancerArn = Lens.lens (\Explanation' {loadBalancerArn} -> loadBalancerArn) (\s@Explanation' {} a -> s {loadBalancerArn = a} :: Explanation)

-- | The security group rule.
explanation_securityGroupRule :: Lens.Lens' Explanation (Prelude.Maybe AnalysisSecurityGroupRule)
explanation_securityGroupRule = Lens.lens (\Explanation' {securityGroupRule} -> securityGroupRule) (\s@Explanation' {} a -> s {securityGroupRule = a} :: Explanation)

-- | The customer gateway.
explanation_customerGateway :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_customerGateway = Lens.lens (\Explanation' {customerGateway} -> customerGateway) (\s@Explanation' {} a -> s {customerGateway = a} :: Explanation)

-- | The Availability Zones.
explanation_availabilityZones :: Lens.Lens' Explanation (Prelude.Maybe [Prelude.Text])
explanation_availabilityZones = Lens.lens (\Explanation' {availabilityZones} -> availabilityZones) (\s@Explanation' {} a -> s {availabilityZones = a} :: Explanation) Prelude.. Lens.mapping Prelude._Coerce

-- | The target group.
explanation_loadBalancerTargetGroup :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_loadBalancerTargetGroup = Lens.lens (\Explanation' {loadBalancerTargetGroup} -> loadBalancerTargetGroup) (\s@Explanation' {} a -> s {loadBalancerTargetGroup = a} :: Explanation)

-- | The explanation code.
explanation_explanationCode :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_explanationCode = Lens.lens (\Explanation' {explanationCode} -> explanationCode) (\s@Explanation' {} a -> s {explanationCode = a} :: Explanation)

-- | The IPv4 address, in CIDR notation.
explanation_address :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_address = Lens.lens (\Explanation' {address} -> address) (\s@Explanation' {} a -> s {address = a} :: Explanation)

-- | The NAT gateway.
explanation_natGateway :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_natGateway = Lens.lens (\Explanation' {natGateway} -> natGateway) (\s@Explanation' {} a -> s {natGateway = a} :: Explanation)

-- | The target.
explanation_loadBalancerTarget :: Lens.Lens' Explanation (Prelude.Maybe AnalysisLoadBalancerTarget)
explanation_loadBalancerTarget = Lens.lens (\Explanation' {loadBalancerTarget} -> loadBalancerTarget) (\s@Explanation' {} a -> s {loadBalancerTarget = a} :: Explanation)

-- | The internet gateway.
explanation_internetGateway :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_internetGateway = Lens.lens (\Explanation' {internetGateway} -> internetGateway) (\s@Explanation' {} a -> s {internetGateway = a} :: Explanation)

-- | The network interface.
explanation_networkInterface :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_networkInterface = Lens.lens (\Explanation' {networkInterface} -> networkInterface) (\s@Explanation' {} a -> s {networkInterface = a} :: Explanation)

-- | The subnet.
explanation_subnet :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_subnet = Lens.lens (\Explanation' {subnet} -> subnet) (\s@Explanation' {} a -> s {subnet = a} :: Explanation)

-- | The source VPC.
explanation_sourceVpc :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_sourceVpc = Lens.lens (\Explanation' {sourceVpc} -> sourceVpc) (\s@Explanation' {} a -> s {sourceVpc = a} :: Explanation)

-- | The network ACL rule.
explanation_aclRule :: Lens.Lens' Explanation (Prelude.Maybe AnalysisAclRule)
explanation_aclRule = Lens.lens (\Explanation' {aclRule} -> aclRule) (\s@Explanation' {} a -> s {aclRule = a} :: Explanation)

-- | The protocols.
explanation_protocols :: Lens.Lens' Explanation (Prelude.Maybe [Prelude.Text])
explanation_protocols = Lens.lens (\Explanation' {protocols} -> protocols) (\s@Explanation' {} a -> s {protocols = a} :: Explanation) Prelude.. Lens.mapping Prelude._Coerce

-- | The direction. The following are possible values:
--
-- -   egress
--
-- -   ingress
explanation_direction :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_direction = Lens.lens (\Explanation' {direction} -> direction) (\s@Explanation' {} a -> s {direction = a} :: Explanation)

-- | The security groups.
explanation_securityGroups :: Lens.Lens' Explanation (Prelude.Maybe [AnalysisComponent])
explanation_securityGroups = Lens.lens (\Explanation' {securityGroups} -> securityGroups) (\s@Explanation' {} a -> s {securityGroups = a} :: Explanation) Prelude.. Lens.mapping Prelude._Coerce

-- | The prefix list.
explanation_prefixList :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_prefixList = Lens.lens (\Explanation' {prefixList} -> prefixList) (\s@Explanation' {} a -> s {prefixList = a} :: Explanation)

-- | The packet field.
explanation_packetField :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_packetField = Lens.lens (\Explanation' {packetField} -> packetField) (\s@Explanation' {} a -> s {packetField = a} :: Explanation)

-- | The CIDR ranges.
explanation_cidrs :: Lens.Lens' Explanation (Prelude.Maybe [Prelude.Text])
explanation_cidrs = Lens.lens (\Explanation' {cidrs} -> cidrs) (\s@Explanation' {} a -> s {cidrs = a} :: Explanation) Prelude.. Lens.mapping Prelude._Coerce

-- | The component.
explanation_component :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_component = Lens.lens (\Explanation' {component} -> component) (\s@Explanation' {} a -> s {component = a} :: Explanation)

-- | The state.
explanation_state :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_state = Lens.lens (\Explanation' {state} -> state) (\s@Explanation' {} a -> s {state = a} :: Explanation)

-- | The route table.
explanation_routeTable :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_routeTable = Lens.lens (\Explanation' {routeTable} -> routeTable) (\s@Explanation' {} a -> s {routeTable = a} :: Explanation)

-- | The destination.
explanation_destination :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_destination = Lens.lens (\Explanation' {destination} -> destination) (\s@Explanation' {} a -> s {destination = a} :: Explanation)

-- | The route table for the subnet.
explanation_subnetRouteTable :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_subnetRouteTable = Lens.lens (\Explanation' {subnetRouteTable} -> subnetRouteTable) (\s@Explanation' {} a -> s {subnetRouteTable = a} :: Explanation)

-- | The resource to which the component is attached.
explanation_attachedTo :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_attachedTo = Lens.lens (\Explanation' {attachedTo} -> attachedTo) (\s@Explanation' {} a -> s {attachedTo = a} :: Explanation)

-- | The security group.
explanation_securityGroup :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_securityGroup = Lens.lens (\Explanation' {securityGroup} -> securityGroup) (\s@Explanation' {} a -> s {securityGroup = a} :: Explanation)

-- | The VPC peering connection.
explanation_vpcPeeringConnection :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_vpcPeeringConnection = Lens.lens (\Explanation' {vpcPeeringConnection} -> vpcPeeringConnection) (\s@Explanation' {} a -> s {vpcPeeringConnection = a} :: Explanation)

-- | The destination VPC.
explanation_destinationVpc :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_destinationVpc = Lens.lens (\Explanation' {destinationVpc} -> destinationVpc) (\s@Explanation' {} a -> s {destinationVpc = a} :: Explanation)

-- | The load balancer listener.
explanation_elasticLoadBalancerListener :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_elasticLoadBalancerListener = Lens.lens (\Explanation' {elasticLoadBalancerListener} -> elasticLoadBalancerListener) (\s@Explanation' {} a -> s {elasticLoadBalancerListener = a} :: Explanation)

-- | The port.
explanation_port :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Natural)
explanation_port = Lens.lens (\Explanation' {port} -> port) (\s@Explanation' {} a -> s {port = a} :: Explanation)

-- | The network ACL.
explanation_acl :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_acl = Lens.lens (\Explanation' {acl} -> acl) (\s@Explanation' {} a -> s {acl = a} :: Explanation)

-- | The VPC endpoint.
explanation_vpcEndpoint :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_vpcEndpoint = Lens.lens (\Explanation' {vpcEndpoint} -> vpcEndpoint) (\s@Explanation' {} a -> s {vpcEndpoint = a} :: Explanation)

-- | The route table route.
explanation_routeTableRoute :: Lens.Lens' Explanation (Prelude.Maybe AnalysisRouteTableRoute)
explanation_routeTableRoute = Lens.lens (\Explanation' {routeTableRoute} -> routeTableRoute) (\s@Explanation' {} a -> s {routeTableRoute = a} :: Explanation)

-- | The VPN connection.
explanation_vpnConnection :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_vpnConnection = Lens.lens (\Explanation' {vpnConnection} -> vpnConnection) (\s@Explanation' {} a -> s {vpnConnection = a} :: Explanation)

-- | The missing component.
explanation_missingComponent :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Text)
explanation_missingComponent = Lens.lens (\Explanation' {missingComponent} -> missingComponent) (\s@Explanation' {} a -> s {missingComponent = a} :: Explanation)

-- | The IPv4 addresses, in CIDR notation.
explanation_addresses :: Lens.Lens' Explanation (Prelude.Maybe [Prelude.Text])
explanation_addresses = Lens.lens (\Explanation' {addresses} -> addresses) (\s@Explanation' {} a -> s {addresses = a} :: Explanation) Prelude.. Lens.mapping Prelude._Coerce

-- | The listener for a Classic Load Balancer.
explanation_classicLoadBalancerListener :: Lens.Lens' Explanation (Prelude.Maybe AnalysisLoadBalancerListener)
explanation_classicLoadBalancerListener = Lens.lens (\Explanation' {classicLoadBalancerListener} -> classicLoadBalancerListener) (\s@Explanation' {} a -> s {classicLoadBalancerListener = a} :: Explanation)

-- | The route table.
explanation_ingressRouteTable :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_ingressRouteTable = Lens.lens (\Explanation' {ingressRouteTable} -> ingressRouteTable) (\s@Explanation' {} a -> s {ingressRouteTable = a} :: Explanation)

-- | The VPN gateway.
explanation_vpnGateway :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_vpnGateway = Lens.lens (\Explanation' {vpnGateway} -> vpnGateway) (\s@Explanation' {} a -> s {vpnGateway = a} :: Explanation)

-- | The port ranges.
explanation_portRanges :: Lens.Lens' Explanation (Prelude.Maybe [PortRange])
explanation_portRanges = Lens.lens (\Explanation' {portRanges} -> portRanges) (\s@Explanation' {} a -> s {portRanges = a} :: Explanation) Prelude.. Lens.mapping Prelude._Coerce

-- | The listener port of the load balancer.
explanation_loadBalancerListenerPort :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Natural)
explanation_loadBalancerListenerPort = Lens.lens (\Explanation' {loadBalancerListenerPort} -> loadBalancerListenerPort) (\s@Explanation' {} a -> s {loadBalancerListenerPort = a} :: Explanation)

-- | The component VPC.
explanation_vpc :: Lens.Lens' Explanation (Prelude.Maybe AnalysisComponent)
explanation_vpc = Lens.lens (\Explanation' {vpc} -> vpc) (\s@Explanation' {} a -> s {vpc = a} :: Explanation)

-- | The target port.
explanation_loadBalancerTargetPort :: Lens.Lens' Explanation (Prelude.Maybe Prelude.Natural)
explanation_loadBalancerTargetPort = Lens.lens (\Explanation' {loadBalancerTargetPort} -> loadBalancerTargetPort) (\s@Explanation' {} a -> s {loadBalancerTargetPort = a} :: Explanation)

instance Prelude.FromXML Explanation where
  parseXML x =
    Explanation'
      Prelude.<$> ( x Prelude..@? "loadBalancerTargetGroupSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "loadBalancerArn")
      Prelude.<*> (x Prelude..@? "securityGroupRule")
      Prelude.<*> (x Prelude..@? "customerGateway")
      Prelude.<*> ( x Prelude..@? "availabilityZoneSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "loadBalancerTargetGroup")
      Prelude.<*> (x Prelude..@? "explanationCode")
      Prelude.<*> (x Prelude..@? "address")
      Prelude.<*> (x Prelude..@? "natGateway")
      Prelude.<*> (x Prelude..@? "loadBalancerTarget")
      Prelude.<*> (x Prelude..@? "internetGateway")
      Prelude.<*> (x Prelude..@? "networkInterface")
      Prelude.<*> (x Prelude..@? "subnet")
      Prelude.<*> (x Prelude..@? "sourceVpc")
      Prelude.<*> (x Prelude..@? "aclRule")
      Prelude.<*> ( x Prelude..@? "protocolSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "direction")
      Prelude.<*> ( x Prelude..@? "securityGroupSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "prefixList")
      Prelude.<*> (x Prelude..@? "packetField")
      Prelude.<*> ( x Prelude..@? "cidrSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "component")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "routeTable")
      Prelude.<*> (x Prelude..@? "destination")
      Prelude.<*> (x Prelude..@? "subnetRouteTable")
      Prelude.<*> (x Prelude..@? "attachedTo")
      Prelude.<*> (x Prelude..@? "securityGroup")
      Prelude.<*> (x Prelude..@? "vpcPeeringConnection")
      Prelude.<*> (x Prelude..@? "destinationVpc")
      Prelude.<*> (x Prelude..@? "elasticLoadBalancerListener")
      Prelude.<*> (x Prelude..@? "port")
      Prelude.<*> (x Prelude..@? "acl")
      Prelude.<*> (x Prelude..@? "vpcEndpoint")
      Prelude.<*> (x Prelude..@? "routeTableRoute")
      Prelude.<*> (x Prelude..@? "vpnConnection")
      Prelude.<*> (x Prelude..@? "missingComponent")
      Prelude.<*> ( x Prelude..@? "addressSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "classicLoadBalancerListener")
      Prelude.<*> (x Prelude..@? "ingressRouteTable")
      Prelude.<*> (x Prelude..@? "vpnGateway")
      Prelude.<*> ( x Prelude..@? "portRangeSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "loadBalancerListenerPort")
      Prelude.<*> (x Prelude..@? "vpc")
      Prelude.<*> (x Prelude..@? "loadBalancerTargetPort")

instance Prelude.Hashable Explanation

instance Prelude.NFData Explanation
