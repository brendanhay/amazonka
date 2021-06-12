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
-- Module      : Network.AWS.EC2.Types.TargetNetwork
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetNetwork where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AssociationStatus
import qualified Network.AWS.Lens as Lens

-- | Describes a target network associated with a Client VPN endpoint.
--
-- /See:/ 'newTargetNetwork' smart constructor.
data TargetNetwork = TargetNetwork'
  { -- | The ID of the Client VPN endpoint with which the target network is
    -- associated.
    clientVpnEndpointId :: Core.Maybe Core.Text,
    -- | The current state of the target network association.
    status :: Core.Maybe AssociationStatus,
    -- | The IDs of the security groups applied to the target network
    -- association.
    securityGroups :: Core.Maybe [Core.Text],
    -- | The ID of the association.
    associationId :: Core.Maybe Core.Text,
    -- | The ID of the VPC in which the target network (subnet) is located.
    vpcId :: Core.Maybe Core.Text,
    -- | The ID of the subnet specified as the target network.
    targetNetworkId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TargetNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientVpnEndpointId', 'targetNetwork_clientVpnEndpointId' - The ID of the Client VPN endpoint with which the target network is
-- associated.
--
-- 'status', 'targetNetwork_status' - The current state of the target network association.
--
-- 'securityGroups', 'targetNetwork_securityGroups' - The IDs of the security groups applied to the target network
-- association.
--
-- 'associationId', 'targetNetwork_associationId' - The ID of the association.
--
-- 'vpcId', 'targetNetwork_vpcId' - The ID of the VPC in which the target network (subnet) is located.
--
-- 'targetNetworkId', 'targetNetwork_targetNetworkId' - The ID of the subnet specified as the target network.
newTargetNetwork ::
  TargetNetwork
newTargetNetwork =
  TargetNetwork'
    { clientVpnEndpointId = Core.Nothing,
      status = Core.Nothing,
      securityGroups = Core.Nothing,
      associationId = Core.Nothing,
      vpcId = Core.Nothing,
      targetNetworkId = Core.Nothing
    }

-- | The ID of the Client VPN endpoint with which the target network is
-- associated.
targetNetwork_clientVpnEndpointId :: Lens.Lens' TargetNetwork (Core.Maybe Core.Text)
targetNetwork_clientVpnEndpointId = Lens.lens (\TargetNetwork' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@TargetNetwork' {} a -> s {clientVpnEndpointId = a} :: TargetNetwork)

-- | The current state of the target network association.
targetNetwork_status :: Lens.Lens' TargetNetwork (Core.Maybe AssociationStatus)
targetNetwork_status = Lens.lens (\TargetNetwork' {status} -> status) (\s@TargetNetwork' {} a -> s {status = a} :: TargetNetwork)

-- | The IDs of the security groups applied to the target network
-- association.
targetNetwork_securityGroups :: Lens.Lens' TargetNetwork (Core.Maybe [Core.Text])
targetNetwork_securityGroups = Lens.lens (\TargetNetwork' {securityGroups} -> securityGroups) (\s@TargetNetwork' {} a -> s {securityGroups = a} :: TargetNetwork) Core.. Lens.mapping Lens._Coerce

-- | The ID of the association.
targetNetwork_associationId :: Lens.Lens' TargetNetwork (Core.Maybe Core.Text)
targetNetwork_associationId = Lens.lens (\TargetNetwork' {associationId} -> associationId) (\s@TargetNetwork' {} a -> s {associationId = a} :: TargetNetwork)

-- | The ID of the VPC in which the target network (subnet) is located.
targetNetwork_vpcId :: Lens.Lens' TargetNetwork (Core.Maybe Core.Text)
targetNetwork_vpcId = Lens.lens (\TargetNetwork' {vpcId} -> vpcId) (\s@TargetNetwork' {} a -> s {vpcId = a} :: TargetNetwork)

-- | The ID of the subnet specified as the target network.
targetNetwork_targetNetworkId :: Lens.Lens' TargetNetwork (Core.Maybe Core.Text)
targetNetwork_targetNetworkId = Lens.lens (\TargetNetwork' {targetNetworkId} -> targetNetworkId) (\s@TargetNetwork' {} a -> s {targetNetworkId = a} :: TargetNetwork)

instance Core.FromXML TargetNetwork where
  parseXML x =
    TargetNetwork'
      Core.<$> (x Core..@? "clientVpnEndpointId")
      Core.<*> (x Core..@? "status")
      Core.<*> ( x Core..@? "securityGroups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "associationId")
      Core.<*> (x Core..@? "vpcId")
      Core.<*> (x Core..@? "targetNetworkId")

instance Core.Hashable TargetNetwork

instance Core.NFData TargetNetwork
