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
-- Module      : Amazonka.EC2.Types.PathComponent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PathComponent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AdditionalDetail
import Amazonka.EC2.Types.AnalysisAclRule
import Amazonka.EC2.Types.AnalysisComponent
import Amazonka.EC2.Types.AnalysisPacketHeader
import Amazonka.EC2.Types.AnalysisRouteTableRoute
import Amazonka.EC2.Types.AnalysisSecurityGroupRule
import Amazonka.EC2.Types.Explanation
import Amazonka.EC2.Types.TransitGatewayRouteTableRoute
import qualified Amazonka.Prelude as Prelude

-- | Describes a path component.
--
-- /See:/ 'newPathComponent' smart constructor.
data PathComponent = PathComponent'
  { -- | The route table route.
    routeTableRoute :: Prelude.Maybe AnalysisRouteTableRoute,
    -- | The component VPC.
    vpc :: Prelude.Maybe AnalysisComponent,
    -- | The additional details.
    additionalDetails :: Prelude.Maybe [AdditionalDetail],
    -- | The outbound header.
    outboundHeader :: Prelude.Maybe AnalysisPacketHeader,
    -- | The resource to which the path component is attached.
    attachedTo :: Prelude.Maybe AnalysisComponent,
    -- | The source VPC.
    sourceVpc :: Prelude.Maybe AnalysisComponent,
    -- | The network ACL rule.
    aclRule :: Prelude.Maybe AnalysisAclRule,
    -- | The explanation codes.
    explanations :: Prelude.Maybe [Explanation],
    -- | The transit gateway.
    transitGateway :: Prelude.Maybe AnalysisComponent,
    -- | The sequence number.
    sequenceNumber :: Prelude.Maybe Prelude.Int,
    -- | The destination VPC.
    destinationVpc :: Prelude.Maybe AnalysisComponent,
    -- | The load balancer listener.
    elasticLoadBalancerListener :: Prelude.Maybe AnalysisComponent,
    -- | The subnet.
    subnet :: Prelude.Maybe AnalysisComponent,
    -- | The inbound header.
    inboundHeader :: Prelude.Maybe AnalysisPacketHeader,
    -- | The route in a transit gateway route table.
    transitGatewayRouteTableRoute :: Prelude.Maybe TransitGatewayRouteTableRoute,
    -- | The component.
    component :: Prelude.Maybe AnalysisComponent,
    -- | The security group rule.
    securityGroupRule :: Prelude.Maybe AnalysisSecurityGroupRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PathComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routeTableRoute', 'pathComponent_routeTableRoute' - The route table route.
--
-- 'vpc', 'pathComponent_vpc' - The component VPC.
--
-- 'additionalDetails', 'pathComponent_additionalDetails' - The additional details.
--
-- 'outboundHeader', 'pathComponent_outboundHeader' - The outbound header.
--
-- 'attachedTo', 'pathComponent_attachedTo' - The resource to which the path component is attached.
--
-- 'sourceVpc', 'pathComponent_sourceVpc' - The source VPC.
--
-- 'aclRule', 'pathComponent_aclRule' - The network ACL rule.
--
-- 'explanations', 'pathComponent_explanations' - The explanation codes.
--
-- 'transitGateway', 'pathComponent_transitGateway' - The transit gateway.
--
-- 'sequenceNumber', 'pathComponent_sequenceNumber' - The sequence number.
--
-- 'destinationVpc', 'pathComponent_destinationVpc' - The destination VPC.
--
-- 'elasticLoadBalancerListener', 'pathComponent_elasticLoadBalancerListener' - The load balancer listener.
--
-- 'subnet', 'pathComponent_subnet' - The subnet.
--
-- 'inboundHeader', 'pathComponent_inboundHeader' - The inbound header.
--
-- 'transitGatewayRouteTableRoute', 'pathComponent_transitGatewayRouteTableRoute' - The route in a transit gateway route table.
--
-- 'component', 'pathComponent_component' - The component.
--
-- 'securityGroupRule', 'pathComponent_securityGroupRule' - The security group rule.
newPathComponent ::
  PathComponent
newPathComponent =
  PathComponent'
    { routeTableRoute = Prelude.Nothing,
      vpc = Prelude.Nothing,
      additionalDetails = Prelude.Nothing,
      outboundHeader = Prelude.Nothing,
      attachedTo = Prelude.Nothing,
      sourceVpc = Prelude.Nothing,
      aclRule = Prelude.Nothing,
      explanations = Prelude.Nothing,
      transitGateway = Prelude.Nothing,
      sequenceNumber = Prelude.Nothing,
      destinationVpc = Prelude.Nothing,
      elasticLoadBalancerListener = Prelude.Nothing,
      subnet = Prelude.Nothing,
      inboundHeader = Prelude.Nothing,
      transitGatewayRouteTableRoute = Prelude.Nothing,
      component = Prelude.Nothing,
      securityGroupRule = Prelude.Nothing
    }

-- | The route table route.
pathComponent_routeTableRoute :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisRouteTableRoute)
pathComponent_routeTableRoute = Lens.lens (\PathComponent' {routeTableRoute} -> routeTableRoute) (\s@PathComponent' {} a -> s {routeTableRoute = a} :: PathComponent)

-- | The component VPC.
pathComponent_vpc :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_vpc = Lens.lens (\PathComponent' {vpc} -> vpc) (\s@PathComponent' {} a -> s {vpc = a} :: PathComponent)

-- | The additional details.
pathComponent_additionalDetails :: Lens.Lens' PathComponent (Prelude.Maybe [AdditionalDetail])
pathComponent_additionalDetails = Lens.lens (\PathComponent' {additionalDetails} -> additionalDetails) (\s@PathComponent' {} a -> s {additionalDetails = a} :: PathComponent) Prelude.. Lens.mapping Lens.coerced

-- | The outbound header.
pathComponent_outboundHeader :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisPacketHeader)
pathComponent_outboundHeader = Lens.lens (\PathComponent' {outboundHeader} -> outboundHeader) (\s@PathComponent' {} a -> s {outboundHeader = a} :: PathComponent)

-- | The resource to which the path component is attached.
pathComponent_attachedTo :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_attachedTo = Lens.lens (\PathComponent' {attachedTo} -> attachedTo) (\s@PathComponent' {} a -> s {attachedTo = a} :: PathComponent)

-- | The source VPC.
pathComponent_sourceVpc :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_sourceVpc = Lens.lens (\PathComponent' {sourceVpc} -> sourceVpc) (\s@PathComponent' {} a -> s {sourceVpc = a} :: PathComponent)

-- | The network ACL rule.
pathComponent_aclRule :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisAclRule)
pathComponent_aclRule = Lens.lens (\PathComponent' {aclRule} -> aclRule) (\s@PathComponent' {} a -> s {aclRule = a} :: PathComponent)

-- | The explanation codes.
pathComponent_explanations :: Lens.Lens' PathComponent (Prelude.Maybe [Explanation])
pathComponent_explanations = Lens.lens (\PathComponent' {explanations} -> explanations) (\s@PathComponent' {} a -> s {explanations = a} :: PathComponent) Prelude.. Lens.mapping Lens.coerced

-- | The transit gateway.
pathComponent_transitGateway :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_transitGateway = Lens.lens (\PathComponent' {transitGateway} -> transitGateway) (\s@PathComponent' {} a -> s {transitGateway = a} :: PathComponent)

-- | The sequence number.
pathComponent_sequenceNumber :: Lens.Lens' PathComponent (Prelude.Maybe Prelude.Int)
pathComponent_sequenceNumber = Lens.lens (\PathComponent' {sequenceNumber} -> sequenceNumber) (\s@PathComponent' {} a -> s {sequenceNumber = a} :: PathComponent)

-- | The destination VPC.
pathComponent_destinationVpc :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_destinationVpc = Lens.lens (\PathComponent' {destinationVpc} -> destinationVpc) (\s@PathComponent' {} a -> s {destinationVpc = a} :: PathComponent)

-- | The load balancer listener.
pathComponent_elasticLoadBalancerListener :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_elasticLoadBalancerListener = Lens.lens (\PathComponent' {elasticLoadBalancerListener} -> elasticLoadBalancerListener) (\s@PathComponent' {} a -> s {elasticLoadBalancerListener = a} :: PathComponent)

-- | The subnet.
pathComponent_subnet :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_subnet = Lens.lens (\PathComponent' {subnet} -> subnet) (\s@PathComponent' {} a -> s {subnet = a} :: PathComponent)

-- | The inbound header.
pathComponent_inboundHeader :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisPacketHeader)
pathComponent_inboundHeader = Lens.lens (\PathComponent' {inboundHeader} -> inboundHeader) (\s@PathComponent' {} a -> s {inboundHeader = a} :: PathComponent)

-- | The route in a transit gateway route table.
pathComponent_transitGatewayRouteTableRoute :: Lens.Lens' PathComponent (Prelude.Maybe TransitGatewayRouteTableRoute)
pathComponent_transitGatewayRouteTableRoute = Lens.lens (\PathComponent' {transitGatewayRouteTableRoute} -> transitGatewayRouteTableRoute) (\s@PathComponent' {} a -> s {transitGatewayRouteTableRoute = a} :: PathComponent)

-- | The component.
pathComponent_component :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_component = Lens.lens (\PathComponent' {component} -> component) (\s@PathComponent' {} a -> s {component = a} :: PathComponent)

-- | The security group rule.
pathComponent_securityGroupRule :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisSecurityGroupRule)
pathComponent_securityGroupRule = Lens.lens (\PathComponent' {securityGroupRule} -> securityGroupRule) (\s@PathComponent' {} a -> s {securityGroupRule = a} :: PathComponent)

instance Core.FromXML PathComponent where
  parseXML x =
    PathComponent'
      Prelude.<$> (x Core..@? "routeTableRoute")
      Prelude.<*> (x Core..@? "vpc")
      Prelude.<*> ( x Core..@? "additionalDetailSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "outboundHeader")
      Prelude.<*> (x Core..@? "attachedTo")
      Prelude.<*> (x Core..@? "sourceVpc")
      Prelude.<*> (x Core..@? "aclRule")
      Prelude.<*> ( x Core..@? "explanationSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "transitGateway")
      Prelude.<*> (x Core..@? "sequenceNumber")
      Prelude.<*> (x Core..@? "destinationVpc")
      Prelude.<*> (x Core..@? "elasticLoadBalancerListener")
      Prelude.<*> (x Core..@? "subnet")
      Prelude.<*> (x Core..@? "inboundHeader")
      Prelude.<*> (x Core..@? "transitGatewayRouteTableRoute")
      Prelude.<*> (x Core..@? "component")
      Prelude.<*> (x Core..@? "securityGroupRule")

instance Prelude.Hashable PathComponent where
  hashWithSalt _salt PathComponent' {..} =
    _salt `Prelude.hashWithSalt` routeTableRoute
      `Prelude.hashWithSalt` vpc
      `Prelude.hashWithSalt` additionalDetails
      `Prelude.hashWithSalt` outboundHeader
      `Prelude.hashWithSalt` attachedTo
      `Prelude.hashWithSalt` sourceVpc
      `Prelude.hashWithSalt` aclRule
      `Prelude.hashWithSalt` explanations
      `Prelude.hashWithSalt` transitGateway
      `Prelude.hashWithSalt` sequenceNumber
      `Prelude.hashWithSalt` destinationVpc
      `Prelude.hashWithSalt` elasticLoadBalancerListener
      `Prelude.hashWithSalt` subnet
      `Prelude.hashWithSalt` inboundHeader
      `Prelude.hashWithSalt` transitGatewayRouteTableRoute
      `Prelude.hashWithSalt` component
      `Prelude.hashWithSalt` securityGroupRule

instance Prelude.NFData PathComponent where
  rnf PathComponent' {..} =
    Prelude.rnf routeTableRoute
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf additionalDetails
      `Prelude.seq` Prelude.rnf outboundHeader
      `Prelude.seq` Prelude.rnf attachedTo
      `Prelude.seq` Prelude.rnf sourceVpc
      `Prelude.seq` Prelude.rnf aclRule
      `Prelude.seq` Prelude.rnf explanations
      `Prelude.seq` Prelude.rnf transitGateway
      `Prelude.seq` Prelude.rnf sequenceNumber
      `Prelude.seq` Prelude.rnf destinationVpc
      `Prelude.seq` Prelude.rnf elasticLoadBalancerListener
      `Prelude.seq` Prelude.rnf subnet
      `Prelude.seq` Prelude.rnf inboundHeader
      `Prelude.seq` Prelude.rnf
        transitGatewayRouteTableRoute
      `Prelude.seq` Prelude.rnf component
      `Prelude.seq` Prelude.rnf securityGroupRule
