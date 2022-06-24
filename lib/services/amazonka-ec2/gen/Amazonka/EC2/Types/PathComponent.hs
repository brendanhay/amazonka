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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PathComponent where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AnalysisAclRule
import Amazonka.EC2.Types.AnalysisComponent
import Amazonka.EC2.Types.AnalysisPacketHeader
import Amazonka.EC2.Types.AnalysisRouteTableRoute
import Amazonka.EC2.Types.AnalysisSecurityGroupRule
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a path component.
--
-- /See:/ 'newPathComponent' smart constructor.
data PathComponent = PathComponent'
  { -- | The route table route.
    routeTableRoute :: Prelude.Maybe AnalysisRouteTableRoute,
    -- | The component VPC.
    vpc :: Prelude.Maybe AnalysisComponent,
    -- | The outbound header.
    outboundHeader :: Prelude.Maybe AnalysisPacketHeader,
    -- | The source VPC.
    sourceVpc :: Prelude.Maybe AnalysisComponent,
    -- | The network ACL rule.
    aclRule :: Prelude.Maybe AnalysisAclRule,
    -- | The sequence number.
    sequenceNumber :: Prelude.Maybe Prelude.Int,
    -- | The destination VPC.
    destinationVpc :: Prelude.Maybe AnalysisComponent,
    -- | The subnet.
    subnet :: Prelude.Maybe AnalysisComponent,
    -- | The inbound header.
    inboundHeader :: Prelude.Maybe AnalysisPacketHeader,
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
-- 'outboundHeader', 'pathComponent_outboundHeader' - The outbound header.
--
-- 'sourceVpc', 'pathComponent_sourceVpc' - The source VPC.
--
-- 'aclRule', 'pathComponent_aclRule' - The network ACL rule.
--
-- 'sequenceNumber', 'pathComponent_sequenceNumber' - The sequence number.
--
-- 'destinationVpc', 'pathComponent_destinationVpc' - The destination VPC.
--
-- 'subnet', 'pathComponent_subnet' - The subnet.
--
-- 'inboundHeader', 'pathComponent_inboundHeader' - The inbound header.
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
      outboundHeader = Prelude.Nothing,
      sourceVpc = Prelude.Nothing,
      aclRule = Prelude.Nothing,
      sequenceNumber = Prelude.Nothing,
      destinationVpc = Prelude.Nothing,
      subnet = Prelude.Nothing,
      inboundHeader = Prelude.Nothing,
      component = Prelude.Nothing,
      securityGroupRule = Prelude.Nothing
    }

-- | The route table route.
pathComponent_routeTableRoute :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisRouteTableRoute)
pathComponent_routeTableRoute = Lens.lens (\PathComponent' {routeTableRoute} -> routeTableRoute) (\s@PathComponent' {} a -> s {routeTableRoute = a} :: PathComponent)

-- | The component VPC.
pathComponent_vpc :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_vpc = Lens.lens (\PathComponent' {vpc} -> vpc) (\s@PathComponent' {} a -> s {vpc = a} :: PathComponent)

-- | The outbound header.
pathComponent_outboundHeader :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisPacketHeader)
pathComponent_outboundHeader = Lens.lens (\PathComponent' {outboundHeader} -> outboundHeader) (\s@PathComponent' {} a -> s {outboundHeader = a} :: PathComponent)

-- | The source VPC.
pathComponent_sourceVpc :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_sourceVpc = Lens.lens (\PathComponent' {sourceVpc} -> sourceVpc) (\s@PathComponent' {} a -> s {sourceVpc = a} :: PathComponent)

-- | The network ACL rule.
pathComponent_aclRule :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisAclRule)
pathComponent_aclRule = Lens.lens (\PathComponent' {aclRule} -> aclRule) (\s@PathComponent' {} a -> s {aclRule = a} :: PathComponent)

-- | The sequence number.
pathComponent_sequenceNumber :: Lens.Lens' PathComponent (Prelude.Maybe Prelude.Int)
pathComponent_sequenceNumber = Lens.lens (\PathComponent' {sequenceNumber} -> sequenceNumber) (\s@PathComponent' {} a -> s {sequenceNumber = a} :: PathComponent)

-- | The destination VPC.
pathComponent_destinationVpc :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_destinationVpc = Lens.lens (\PathComponent' {destinationVpc} -> destinationVpc) (\s@PathComponent' {} a -> s {destinationVpc = a} :: PathComponent)

-- | The subnet.
pathComponent_subnet :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisComponent)
pathComponent_subnet = Lens.lens (\PathComponent' {subnet} -> subnet) (\s@PathComponent' {} a -> s {subnet = a} :: PathComponent)

-- | The inbound header.
pathComponent_inboundHeader :: Lens.Lens' PathComponent (Prelude.Maybe AnalysisPacketHeader)
pathComponent_inboundHeader = Lens.lens (\PathComponent' {inboundHeader} -> inboundHeader) (\s@PathComponent' {} a -> s {inboundHeader = a} :: PathComponent)

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
      Prelude.<*> (x Core..@? "outboundHeader")
      Prelude.<*> (x Core..@? "sourceVpc")
      Prelude.<*> (x Core..@? "aclRule")
      Prelude.<*> (x Core..@? "sequenceNumber")
      Prelude.<*> (x Core..@? "destinationVpc")
      Prelude.<*> (x Core..@? "subnet")
      Prelude.<*> (x Core..@? "inboundHeader")
      Prelude.<*> (x Core..@? "component")
      Prelude.<*> (x Core..@? "securityGroupRule")

instance Prelude.Hashable PathComponent where
  hashWithSalt _salt PathComponent' {..} =
    _salt `Prelude.hashWithSalt` routeTableRoute
      `Prelude.hashWithSalt` vpc
      `Prelude.hashWithSalt` outboundHeader
      `Prelude.hashWithSalt` sourceVpc
      `Prelude.hashWithSalt` aclRule
      `Prelude.hashWithSalt` sequenceNumber
      `Prelude.hashWithSalt` destinationVpc
      `Prelude.hashWithSalt` subnet
      `Prelude.hashWithSalt` inboundHeader
      `Prelude.hashWithSalt` component
      `Prelude.hashWithSalt` securityGroupRule

instance Prelude.NFData PathComponent where
  rnf PathComponent' {..} =
    Prelude.rnf routeTableRoute
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf outboundHeader
      `Prelude.seq` Prelude.rnf sourceVpc
      `Prelude.seq` Prelude.rnf aclRule
      `Prelude.seq` Prelude.rnf sequenceNumber
      `Prelude.seq` Prelude.rnf destinationVpc
      `Prelude.seq` Prelude.rnf subnet
      `Prelude.seq` Prelude.rnf inboundHeader
      `Prelude.seq` Prelude.rnf component
      `Prelude.seq` Prelude.rnf securityGroupRule
