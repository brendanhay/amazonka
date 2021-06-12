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
-- Module      : Network.AWS.EC2.Types.VpnGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpnGateway where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GatewayType
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VpcAttachment
import Network.AWS.EC2.Types.VpnState
import qualified Network.AWS.Lens as Lens

-- | Describes a virtual private gateway.
--
-- /See:/ 'newVpnGateway' smart constructor.
data VpnGateway = VpnGateway'
  { -- | Any VPCs attached to the virtual private gateway.
    vpcAttachments :: Core.Maybe [VpcAttachment],
    -- | The current state of the virtual private gateway.
    state :: Core.Maybe VpnState,
    -- | The Availability Zone where the virtual private gateway was created, if
    -- applicable. This field may be empty or not returned.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The private Autonomous System Number (ASN) for the Amazon side of a BGP
    -- session.
    amazonSideAsn :: Core.Maybe Core.Integer,
    -- | Any tags assigned to the virtual private gateway.
    tags :: Core.Maybe [Tag],
    -- | The type of VPN connection the virtual private gateway supports.
    type' :: Core.Maybe GatewayType,
    -- | The ID of the virtual private gateway.
    vpnGatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VpnGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcAttachments', 'vpnGateway_vpcAttachments' - Any VPCs attached to the virtual private gateway.
--
-- 'state', 'vpnGateway_state' - The current state of the virtual private gateway.
--
-- 'availabilityZone', 'vpnGateway_availabilityZone' - The Availability Zone where the virtual private gateway was created, if
-- applicable. This field may be empty or not returned.
--
-- 'amazonSideAsn', 'vpnGateway_amazonSideAsn' - The private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session.
--
-- 'tags', 'vpnGateway_tags' - Any tags assigned to the virtual private gateway.
--
-- 'type'', 'vpnGateway_type' - The type of VPN connection the virtual private gateway supports.
--
-- 'vpnGatewayId', 'vpnGateway_vpnGatewayId' - The ID of the virtual private gateway.
newVpnGateway ::
  VpnGateway
newVpnGateway =
  VpnGateway'
    { vpcAttachments = Core.Nothing,
      state = Core.Nothing,
      availabilityZone = Core.Nothing,
      amazonSideAsn = Core.Nothing,
      tags = Core.Nothing,
      type' = Core.Nothing,
      vpnGatewayId = Core.Nothing
    }

-- | Any VPCs attached to the virtual private gateway.
vpnGateway_vpcAttachments :: Lens.Lens' VpnGateway (Core.Maybe [VpcAttachment])
vpnGateway_vpcAttachments = Lens.lens (\VpnGateway' {vpcAttachments} -> vpcAttachments) (\s@VpnGateway' {} a -> s {vpcAttachments = a} :: VpnGateway) Core.. Lens.mapping Lens._Coerce

-- | The current state of the virtual private gateway.
vpnGateway_state :: Lens.Lens' VpnGateway (Core.Maybe VpnState)
vpnGateway_state = Lens.lens (\VpnGateway' {state} -> state) (\s@VpnGateway' {} a -> s {state = a} :: VpnGateway)

-- | The Availability Zone where the virtual private gateway was created, if
-- applicable. This field may be empty or not returned.
vpnGateway_availabilityZone :: Lens.Lens' VpnGateway (Core.Maybe Core.Text)
vpnGateway_availabilityZone = Lens.lens (\VpnGateway' {availabilityZone} -> availabilityZone) (\s@VpnGateway' {} a -> s {availabilityZone = a} :: VpnGateway)

-- | The private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session.
vpnGateway_amazonSideAsn :: Lens.Lens' VpnGateway (Core.Maybe Core.Integer)
vpnGateway_amazonSideAsn = Lens.lens (\VpnGateway' {amazonSideAsn} -> amazonSideAsn) (\s@VpnGateway' {} a -> s {amazonSideAsn = a} :: VpnGateway)

-- | Any tags assigned to the virtual private gateway.
vpnGateway_tags :: Lens.Lens' VpnGateway (Core.Maybe [Tag])
vpnGateway_tags = Lens.lens (\VpnGateway' {tags} -> tags) (\s@VpnGateway' {} a -> s {tags = a} :: VpnGateway) Core.. Lens.mapping Lens._Coerce

-- | The type of VPN connection the virtual private gateway supports.
vpnGateway_type :: Lens.Lens' VpnGateway (Core.Maybe GatewayType)
vpnGateway_type = Lens.lens (\VpnGateway' {type'} -> type') (\s@VpnGateway' {} a -> s {type' = a} :: VpnGateway)

-- | The ID of the virtual private gateway.
vpnGateway_vpnGatewayId :: Lens.Lens' VpnGateway (Core.Maybe Core.Text)
vpnGateway_vpnGatewayId = Lens.lens (\VpnGateway' {vpnGatewayId} -> vpnGatewayId) (\s@VpnGateway' {} a -> s {vpnGatewayId = a} :: VpnGateway)

instance Core.FromXML VpnGateway where
  parseXML x =
    VpnGateway'
      Core.<$> ( x Core..@? "attachments" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "amazonSideAsn")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "type")
      Core.<*> (x Core..@? "vpnGatewayId")

instance Core.Hashable VpnGateway

instance Core.NFData VpnGateway
