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
-- Module      : Network.AWS.EC2.Types.VpcPeeringConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpcPeeringConnection where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VpcPeeringConnectionStateReason
import Network.AWS.EC2.Types.VpcPeeringConnectionVpcInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a VPC peering connection.
--
-- /See:/ 'newVpcPeeringConnection' smart constructor.
data VpcPeeringConnection = VpcPeeringConnection'
  { -- | The status of the VPC peering connection.
    status :: Prelude.Maybe VpcPeeringConnectionStateReason,
    -- | Information about the accepter VPC. CIDR block information is only
    -- returned when describing an active VPC peering connection.
    accepterVpcInfo :: Prelude.Maybe VpcPeeringConnectionVpcInfo,
    -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The time that an unaccepted VPC peering connection will expire.
    expirationTime :: Prelude.Maybe Prelude.ISO8601,
    -- | Information about the requester VPC. CIDR block information is only
    -- returned when describing an active VPC peering connection.
    requesterVpcInfo :: Prelude.Maybe VpcPeeringConnectionVpcInfo,
    -- | Any tags assigned to the resource.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { status = Prelude.Nothing,
      accepterVpcInfo = Prelude.Nothing,
      vpcPeeringConnectionId = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      requesterVpcInfo = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The status of the VPC peering connection.
vpcPeeringConnection_status :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe VpcPeeringConnectionStateReason)
vpcPeeringConnection_status = Lens.lens (\VpcPeeringConnection' {status} -> status) (\s@VpcPeeringConnection' {} a -> s {status = a} :: VpcPeeringConnection)

-- | Information about the accepter VPC. CIDR block information is only
-- returned when describing an active VPC peering connection.
vpcPeeringConnection_accepterVpcInfo :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe VpcPeeringConnectionVpcInfo)
vpcPeeringConnection_accepterVpcInfo = Lens.lens (\VpcPeeringConnection' {accepterVpcInfo} -> accepterVpcInfo) (\s@VpcPeeringConnection' {} a -> s {accepterVpcInfo = a} :: VpcPeeringConnection)

-- | The ID of the VPC peering connection.
vpcPeeringConnection_vpcPeeringConnectionId :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_vpcPeeringConnectionId = Lens.lens (\VpcPeeringConnection' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@VpcPeeringConnection' {} a -> s {vpcPeeringConnectionId = a} :: VpcPeeringConnection)

-- | The time that an unaccepted VPC peering connection will expire.
vpcPeeringConnection_expirationTime :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.UTCTime)
vpcPeeringConnection_expirationTime = Lens.lens (\VpcPeeringConnection' {expirationTime} -> expirationTime) (\s@VpcPeeringConnection' {} a -> s {expirationTime = a} :: VpcPeeringConnection) Prelude.. Lens.mapping Prelude._Time

-- | Information about the requester VPC. CIDR block information is only
-- returned when describing an active VPC peering connection.
vpcPeeringConnection_requesterVpcInfo :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe VpcPeeringConnectionVpcInfo)
vpcPeeringConnection_requesterVpcInfo = Lens.lens (\VpcPeeringConnection' {requesterVpcInfo} -> requesterVpcInfo) (\s@VpcPeeringConnection' {} a -> s {requesterVpcInfo = a} :: VpcPeeringConnection)

-- | Any tags assigned to the resource.
vpcPeeringConnection_tags :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe [Tag])
vpcPeeringConnection_tags = Lens.lens (\VpcPeeringConnection' {tags} -> tags) (\s@VpcPeeringConnection' {} a -> s {tags = a} :: VpcPeeringConnection) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML VpcPeeringConnection where
  parseXML x =
    VpcPeeringConnection'
      Prelude.<$> (x Prelude..@? "status")
      Prelude.<*> (x Prelude..@? "accepterVpcInfo")
      Prelude.<*> (x Prelude..@? "vpcPeeringConnectionId")
      Prelude.<*> (x Prelude..@? "expirationTime")
      Prelude.<*> (x Prelude..@? "requesterVpcInfo")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable VpcPeeringConnection

instance Prelude.NFData VpcPeeringConnection
