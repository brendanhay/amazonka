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
-- Module      : Amazonka.EC2.Types.VpnGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpnGateway where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.GatewayType
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.VpcAttachment
import Amazonka.EC2.Types.VpnState
import qualified Amazonka.Prelude as Prelude

-- | Describes a virtual private gateway.
--
-- /See:/ 'newVpnGateway' smart constructor.
data VpnGateway = VpnGateway'
  { -- | The private Autonomous System Number (ASN) for the Amazon side of a BGP
    -- session.
    amazonSideAsn :: Prelude.Maybe Prelude.Integer,
    -- | The Availability Zone where the virtual private gateway was created, if
    -- applicable. This field may be empty or not returned.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The current state of the virtual private gateway.
    state :: Prelude.Maybe VpnState,
    -- | Any tags assigned to the virtual private gateway.
    tags :: Prelude.Maybe [Tag],
    -- | The type of VPN connection the virtual private gateway supports.
    type' :: Prelude.Maybe GatewayType,
    -- | Any VPCs attached to the virtual private gateway.
    vpcAttachments :: Prelude.Maybe [VpcAttachment],
    -- | The ID of the virtual private gateway.
    vpnGatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpnGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amazonSideAsn', 'vpnGateway_amazonSideAsn' - The private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session.
--
-- 'availabilityZone', 'vpnGateway_availabilityZone' - The Availability Zone where the virtual private gateway was created, if
-- applicable. This field may be empty or not returned.
--
-- 'state', 'vpnGateway_state' - The current state of the virtual private gateway.
--
-- 'tags', 'vpnGateway_tags' - Any tags assigned to the virtual private gateway.
--
-- 'type'', 'vpnGateway_type' - The type of VPN connection the virtual private gateway supports.
--
-- 'vpcAttachments', 'vpnGateway_vpcAttachments' - Any VPCs attached to the virtual private gateway.
--
-- 'vpnGatewayId', 'vpnGateway_vpnGatewayId' - The ID of the virtual private gateway.
newVpnGateway ::
  VpnGateway
newVpnGateway =
  VpnGateway'
    { amazonSideAsn = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      vpcAttachments = Prelude.Nothing,
      vpnGatewayId = Prelude.Nothing
    }

-- | The private Autonomous System Number (ASN) for the Amazon side of a BGP
-- session.
vpnGateway_amazonSideAsn :: Lens.Lens' VpnGateway (Prelude.Maybe Prelude.Integer)
vpnGateway_amazonSideAsn = Lens.lens (\VpnGateway' {amazonSideAsn} -> amazonSideAsn) (\s@VpnGateway' {} a -> s {amazonSideAsn = a} :: VpnGateway)

-- | The Availability Zone where the virtual private gateway was created, if
-- applicable. This field may be empty or not returned.
vpnGateway_availabilityZone :: Lens.Lens' VpnGateway (Prelude.Maybe Prelude.Text)
vpnGateway_availabilityZone = Lens.lens (\VpnGateway' {availabilityZone} -> availabilityZone) (\s@VpnGateway' {} a -> s {availabilityZone = a} :: VpnGateway)

-- | The current state of the virtual private gateway.
vpnGateway_state :: Lens.Lens' VpnGateway (Prelude.Maybe VpnState)
vpnGateway_state = Lens.lens (\VpnGateway' {state} -> state) (\s@VpnGateway' {} a -> s {state = a} :: VpnGateway)

-- | Any tags assigned to the virtual private gateway.
vpnGateway_tags :: Lens.Lens' VpnGateway (Prelude.Maybe [Tag])
vpnGateway_tags = Lens.lens (\VpnGateway' {tags} -> tags) (\s@VpnGateway' {} a -> s {tags = a} :: VpnGateway) Prelude.. Lens.mapping Lens.coerced

-- | The type of VPN connection the virtual private gateway supports.
vpnGateway_type :: Lens.Lens' VpnGateway (Prelude.Maybe GatewayType)
vpnGateway_type = Lens.lens (\VpnGateway' {type'} -> type') (\s@VpnGateway' {} a -> s {type' = a} :: VpnGateway)

-- | Any VPCs attached to the virtual private gateway.
vpnGateway_vpcAttachments :: Lens.Lens' VpnGateway (Prelude.Maybe [VpcAttachment])
vpnGateway_vpcAttachments = Lens.lens (\VpnGateway' {vpcAttachments} -> vpcAttachments) (\s@VpnGateway' {} a -> s {vpcAttachments = a} :: VpnGateway) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the virtual private gateway.
vpnGateway_vpnGatewayId :: Lens.Lens' VpnGateway (Prelude.Maybe Prelude.Text)
vpnGateway_vpnGatewayId = Lens.lens (\VpnGateway' {vpnGatewayId} -> vpnGatewayId) (\s@VpnGateway' {} a -> s {vpnGatewayId = a} :: VpnGateway)

instance Data.FromXML VpnGateway where
  parseXML x =
    VpnGateway'
      Prelude.<$> (x Data..@? "amazonSideAsn")
      Prelude.<*> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "type")
      Prelude.<*> ( x Data..@? "attachments" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "vpnGatewayId")

instance Prelude.Hashable VpnGateway where
  hashWithSalt _salt VpnGateway' {..} =
    _salt `Prelude.hashWithSalt` amazonSideAsn
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` vpcAttachments
      `Prelude.hashWithSalt` vpnGatewayId

instance Prelude.NFData VpnGateway where
  rnf VpnGateway' {..} =
    Prelude.rnf amazonSideAsn
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf vpcAttachments
      `Prelude.seq` Prelude.rnf vpnGatewayId
