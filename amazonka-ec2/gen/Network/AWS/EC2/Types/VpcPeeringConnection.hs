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
-- Module      : Network.AWS.EC2.Types.VpcPeeringConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpcPeeringConnection where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VpcPeeringConnectionStateReason
import Network.AWS.EC2.Types.VpcPeeringConnectionVpcInfo
import qualified Network.AWS.Lens as Lens

-- | Describes a VPC peering connection.
--
-- /See:/ 'newVpcPeeringConnection' smart constructor.
data VpcPeeringConnection = VpcPeeringConnection'
  { -- | The status of the VPC peering connection.
    status :: Core.Maybe VpcPeeringConnectionStateReason,
    -- | Information about the accepter VPC. CIDR block information is only
    -- returned when describing an active VPC peering connection.
    accepterVpcInfo :: Core.Maybe VpcPeeringConnectionVpcInfo,
    -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Core.Maybe Core.Text,
    -- | The time that an unaccepted VPC peering connection will expire.
    expirationTime :: Core.Maybe Core.ISO8601,
    -- | Information about the requester VPC. CIDR block information is only
    -- returned when describing an active VPC peering connection.
    requesterVpcInfo :: Core.Maybe VpcPeeringConnectionVpcInfo,
    -- | Any tags assigned to the resource.
    tags :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VpcPeeringConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'vpcPeeringConnection_status' - The status of the VPC peering connection.
--
-- 'accepterVpcInfo', 'vpcPeeringConnection_accepterVpcInfo' - Information about the accepter VPC. CIDR block information is only
-- returned when describing an active VPC peering connection.
--
-- 'vpcPeeringConnectionId', 'vpcPeeringConnection_vpcPeeringConnectionId' - The ID of the VPC peering connection.
--
-- 'expirationTime', 'vpcPeeringConnection_expirationTime' - The time that an unaccepted VPC peering connection will expire.
--
-- 'requesterVpcInfo', 'vpcPeeringConnection_requesterVpcInfo' - Information about the requester VPC. CIDR block information is only
-- returned when describing an active VPC peering connection.
--
-- 'tags', 'vpcPeeringConnection_tags' - Any tags assigned to the resource.
newVpcPeeringConnection ::
  VpcPeeringConnection
newVpcPeeringConnection =
  VpcPeeringConnection'
    { status = Core.Nothing,
      accepterVpcInfo = Core.Nothing,
      vpcPeeringConnectionId = Core.Nothing,
      expirationTime = Core.Nothing,
      requesterVpcInfo = Core.Nothing,
      tags = Core.Nothing
    }

-- | The status of the VPC peering connection.
vpcPeeringConnection_status :: Lens.Lens' VpcPeeringConnection (Core.Maybe VpcPeeringConnectionStateReason)
vpcPeeringConnection_status = Lens.lens (\VpcPeeringConnection' {status} -> status) (\s@VpcPeeringConnection' {} a -> s {status = a} :: VpcPeeringConnection)

-- | Information about the accepter VPC. CIDR block information is only
-- returned when describing an active VPC peering connection.
vpcPeeringConnection_accepterVpcInfo :: Lens.Lens' VpcPeeringConnection (Core.Maybe VpcPeeringConnectionVpcInfo)
vpcPeeringConnection_accepterVpcInfo = Lens.lens (\VpcPeeringConnection' {accepterVpcInfo} -> accepterVpcInfo) (\s@VpcPeeringConnection' {} a -> s {accepterVpcInfo = a} :: VpcPeeringConnection)

-- | The ID of the VPC peering connection.
vpcPeeringConnection_vpcPeeringConnectionId :: Lens.Lens' VpcPeeringConnection (Core.Maybe Core.Text)
vpcPeeringConnection_vpcPeeringConnectionId = Lens.lens (\VpcPeeringConnection' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@VpcPeeringConnection' {} a -> s {vpcPeeringConnectionId = a} :: VpcPeeringConnection)

-- | The time that an unaccepted VPC peering connection will expire.
vpcPeeringConnection_expirationTime :: Lens.Lens' VpcPeeringConnection (Core.Maybe Core.UTCTime)
vpcPeeringConnection_expirationTime = Lens.lens (\VpcPeeringConnection' {expirationTime} -> expirationTime) (\s@VpcPeeringConnection' {} a -> s {expirationTime = a} :: VpcPeeringConnection) Core.. Lens.mapping Core._Time

-- | Information about the requester VPC. CIDR block information is only
-- returned when describing an active VPC peering connection.
vpcPeeringConnection_requesterVpcInfo :: Lens.Lens' VpcPeeringConnection (Core.Maybe VpcPeeringConnectionVpcInfo)
vpcPeeringConnection_requesterVpcInfo = Lens.lens (\VpcPeeringConnection' {requesterVpcInfo} -> requesterVpcInfo) (\s@VpcPeeringConnection' {} a -> s {requesterVpcInfo = a} :: VpcPeeringConnection)

-- | Any tags assigned to the resource.
vpcPeeringConnection_tags :: Lens.Lens' VpcPeeringConnection (Core.Maybe [Tag])
vpcPeeringConnection_tags = Lens.lens (\VpcPeeringConnection' {tags} -> tags) (\s@VpcPeeringConnection' {} a -> s {tags = a} :: VpcPeeringConnection) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML VpcPeeringConnection where
  parseXML x =
    VpcPeeringConnection'
      Core.<$> (x Core..@? "status")
      Core.<*> (x Core..@? "accepterVpcInfo")
      Core.<*> (x Core..@? "vpcPeeringConnectionId")
      Core.<*> (x Core..@? "expirationTime")
      Core.<*> (x Core..@? "requesterVpcInfo")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable VpcPeeringConnection

instance Core.NFData VpcPeeringConnection
