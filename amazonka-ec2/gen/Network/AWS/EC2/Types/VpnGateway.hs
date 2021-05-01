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
-- Module      : Network.AWS.EC2.Types.VpnGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpnGateway where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GatewayType
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VpcAttachment
import Network.AWS.EC2.Types.VpnState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a virtual private gateway.
--
-- /See:/ 'newVpnGateway' smart constructor.
data VpnGateway = VpnGateway'
  { -- | Any VPCs attached to the virtual private gateway.
    vpcAttachments :: Prelude.Maybe [VpcAttachment],
    -- | The current state of the virtual private gateway.
    state :: Prelude.Maybe VpnState,
    -- | The Availability Zone where the virtual private gateway was created, if
    -- applicable. This field may be empty or not returned.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The private Autonomous System Number (ASN) for the Amazon side of a BGP
    -- session.
    amazonSideAsn :: Prelude.Maybe Prelude.Integer,
    -- | Any tags assigned to the virtual private gateway.
    tags :: Prelude.Maybe [Tag],
    -- | The type of VPN connection the virtual private gateway supports.
    type' :: Prelude.Maybe GatewayType,
    -- | The ID of the virtual private gateway.
    vpnGatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { vpcAttachments = Prelude.Nothing,
      state = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      amazonSideAsn = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      vpnGatewayId = Prelude.Nothing
    }

-- | Any VPCs attached to the virtual private gateway.
vpnGateway_vpcAttachments :: Lens.Lens' VpnGateway (Prelude.Maybe [VpcAttachment])
vpnGateway_vpcAttachments = Lens.lens (\VpnGateway' {vpcAttachments} -> vpcAttachments) (\s@VpnGateway' {} a -> s {vpcAttachments = a} :: VpnGateway) Prelude.. Lens.mapping Prelude._Coerce

-- | The current state of the virtual private gateway.
vpnGateway_state :: Lens.Lens' VpnGateway (Prelude.Maybe VpnState)
vpnGateway_state = Lens.lens (\VpnGateway' {state} -> state) (\s@VpnGateway' {} a -> s {state = a} :: VpnGateway)

-- | The Availability Zone where the virtual private gateway was created, if
-- applicable. This field may be empty or not returned.
vpnGateway_availabilityZone :: Lens.Lens' VpnGateway (Prelude.Maybe Prelude.Text)
vpnGateway_availabilityZone = Lens.lens (\VpnGateway' {availabilityZone} -> availabilityZone) (\s@VpnGateway' {} a -> s {availabilityZone = a} :: VpnGateway)

-- | The private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session.
vpnGateway_amazonSideAsn :: Lens.Lens' VpnGateway (Prelude.Maybe Prelude.Integer)
vpnGateway_amazonSideAsn = Lens.lens (\VpnGateway' {amazonSideAsn} -> amazonSideAsn) (\s@VpnGateway' {} a -> s {amazonSideAsn = a} :: VpnGateway)

-- | Any tags assigned to the virtual private gateway.
vpnGateway_tags :: Lens.Lens' VpnGateway (Prelude.Maybe [Tag])
vpnGateway_tags = Lens.lens (\VpnGateway' {tags} -> tags) (\s@VpnGateway' {} a -> s {tags = a} :: VpnGateway) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of VPN connection the virtual private gateway supports.
vpnGateway_type :: Lens.Lens' VpnGateway (Prelude.Maybe GatewayType)
vpnGateway_type = Lens.lens (\VpnGateway' {type'} -> type') (\s@VpnGateway' {} a -> s {type' = a} :: VpnGateway)

-- | The ID of the virtual private gateway.
vpnGateway_vpnGatewayId :: Lens.Lens' VpnGateway (Prelude.Maybe Prelude.Text)
vpnGateway_vpnGatewayId = Lens.lens (\VpnGateway' {vpnGatewayId} -> vpnGatewayId) (\s@VpnGateway' {} a -> s {vpnGatewayId = a} :: VpnGateway)

instance Prelude.FromXML VpnGateway where
  parseXML x =
    VpnGateway'
      Prelude.<$> ( x Prelude..@? "attachments"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> (x Prelude..@? "amazonSideAsn")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "type")
      Prelude.<*> (x Prelude..@? "vpnGatewayId")

instance Prelude.Hashable VpnGateway

instance Prelude.NFData VpnGateway
