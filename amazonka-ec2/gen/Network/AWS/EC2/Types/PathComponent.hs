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
-- Module      : Network.AWS.EC2.Types.PathComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PathComponent where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AnalysisAclRule
import Network.AWS.EC2.Types.AnalysisComponent
import Network.AWS.EC2.Types.AnalysisPacketHeader
import Network.AWS.EC2.Types.AnalysisRouteTableRoute
import Network.AWS.EC2.Types.AnalysisSecurityGroupRule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a path component.
--
-- /See:/ 'newPathComponent' smart constructor.
data PathComponent = PathComponent'
  { -- | The security group rule.
    securityGroupRule :: Prelude.Maybe AnalysisSecurityGroupRule,
    -- | The sequence number.
    sequenceNumber :: Prelude.Maybe Prelude.Int,
    -- | The subnet.
    subnet :: Prelude.Maybe AnalysisComponent,
    -- | The source VPC.
    sourceVpc :: Prelude.Maybe AnalysisComponent,
    -- | The network ACL rule.
    aclRule :: Prelude.Maybe AnalysisAclRule,
    -- | The inbound header.
    inboundHeader :: Prelude.Maybe AnalysisPacketHeader,
    -- | The component.
    component :: Prelude.Maybe AnalysisComponent,
    -- | The destination VPC.
    destinationVpc :: Prelude.Maybe AnalysisComponent,
    -- | The route table route.
    routeTableRoute :: Prelude.Maybe AnalysisRouteTableRoute,
    -- | The outbound header.
    outboundHeader :: Prelude.Maybe AnalysisPacketHeader,
    -- | The component VPC.
    vpc :: Prelude.Maybe AnalysisComponent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PathComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupRule', 'pathComponent_securityGroupRule' - The security group rule.
--
-- 'sequenceNumber', 'pathComponent_sequenceNumber' - The sequence number.
--
-- 'subnet', 'pathComponent_subnet' - The subnet.
--
-- 'sourceVpc', 'pathComponent_sourceVpc' - The source VPC.
--
-- 'aclRule', 'pathComponent_aclRule' - The network ACL rule.
--
-- 'inboundHeader', 'pathComponent_inboundHeader' - The inbound header.
--
-- 'component', 'pathComponent_component' - The component.
--
-- 'destinationVpc', 'pathComponent_destinationVpc' - The destination VPC.
--
-- 'routeTableRoute', 'pathComponent_routeTableRoute' - The route table route.
--
-- 'outboundHeader', 'pathComponent_outboundHeader' - The outbound header.
--
-- 'vpc', 'pathComponent_vpc' - The component VPC.
newPathComponent ::
  PathComponent
newPathComponent =
  PathComponent'
    { securityGroupRule = Prelude.Nothing,
      sequenceNumber = Prelude.Nothing,
      subnet = Prelude.Nothing,
      sourceVpc = Prelude.Nothing,
      aclRule = Prelude.Nothing,
      inboundHeader = Prelude.Nothing,
      component = Prelude.Nothing,
      destinationVpc = Prelude.Nothing,
      routeTableRoute = Prelude.Nothing,
      outboundHeader = Prelude.Nothing,
      vpc = Prelude.Nothing
    }

-- | The security group rule.
pathComponent_securityGroupRule :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisSecurityGroupRule)
pathComponent_securityGroupRule = Lens.lens (\PathComponent' {securityGroupRule} -> securityGroupRule) (\s@PathComponent' {} a -> s {securityGroupRule = a} :: PathComponent)

-- | The sequence number.
pathComponent_sequenceNumber :: Lens.Lens' PathComponent (Prelude.Maybe Prelude.Int)
pathComponent_sequenceNumber = Lens.lens (\PathComponent' {sequenceNumber} -> sequenceNumber) (\s@PathComponent' {} a -> s {sequenceNumber = a} :: PathComponent)

-- | The subnet.
pathComponent_subnet :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_subnet = Lens.lens (\PathComponent' {subnet} -> subnet) (\s@PathComponent' {} a -> s {subnet = a} :: PathComponent)

-- | The source VPC.
pathComponent_sourceVpc :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_sourceVpc = Lens.lens (\PathComponent' {sourceVpc} -> sourceVpc) (\s@PathComponent' {} a -> s {sourceVpc = a} :: PathComponent)

-- | The network ACL rule.
pathComponent_aclRule :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisAclRule)
pathComponent_aclRule = Lens.lens (\PathComponent' {aclRule} -> aclRule) (\s@PathComponent' {} a -> s {aclRule = a} :: PathComponent)

-- | The inbound header.
pathComponent_inboundHeader :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisPacketHeader)
pathComponent_inboundHeader = Lens.lens (\PathComponent' {inboundHeader} -> inboundHeader) (\s@PathComponent' {} a -> s {inboundHeader = a} :: PathComponent)

-- | The component.
pathComponent_component :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_component = Lens.lens (\PathComponent' {component} -> component) (\s@PathComponent' {} a -> s {component = a} :: PathComponent)

-- | The destination VPC.
pathComponent_destinationVpc :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_destinationVpc = Lens.lens (\PathComponent' {destinationVpc} -> destinationVpc) (\s@PathComponent' {} a -> s {destinationVpc = a} :: PathComponent)

-- | The route table route.
pathComponent_routeTableRoute :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisRouteTableRoute)
pathComponent_routeTableRoute = Lens.lens (\PathComponent' {routeTableRoute} -> routeTableRoute) (\s@PathComponent' {} a -> s {routeTableRoute = a} :: PathComponent)

-- | The outbound header.
pathComponent_outboundHeader :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisPacketHeader)
pathComponent_outboundHeader = Lens.lens (\PathComponent' {outboundHeader} -> outboundHeader) (\s@PathComponent' {} a -> s {outboundHeader = a} :: PathComponent)

-- | The component VPC.
pathComponent_vpc :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_vpc = Lens.lens (\PathComponent' {vpc} -> vpc) (\s@PathComponent' {} a -> s {vpc = a} :: PathComponent)

instance Prelude.FromXML PathComponent where
  parseXML x =
    PathComponent'
      Prelude.<$> (x Prelude..@? "securityGroupRule")
      Prelude.<*> (x Prelude..@? "sequenceNumber")
      Prelude.<*> (x Prelude..@? "subnet")
      Prelude.<*> (x Prelude..@? "sourceVpc")
      Prelude.<*> (x Prelude..@? "aclRule")
      Prelude.<*> (x Prelude..@? "inboundHeader")
      Prelude.<*> (x Prelude..@? "component")
      Prelude.<*> (x Prelude..@? "destinationVpc")
      Prelude.<*> (x Prelude..@? "routeTableRoute")
      Prelude.<*> (x Prelude..@? "outboundHeader")
      Prelude.<*> (x Prelude..@? "vpc")

instance Prelude.Hashable PathComponent

instance Prelude.NFData PathComponent
