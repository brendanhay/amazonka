{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVpcPeeringConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a VPC peering connection between two VPCs: a requester VPC that
-- you own and an accepter VPC with which to create the connection. The
-- accepter VPC can belong to another AWS account and can be in a different
-- Region to the requester VPC. The requester VPC and accepter VPC cannot
-- have overlapping CIDR blocks.
--
-- Limitations and rules apply to a VPC peering connection. For more
-- information, see the
-- <https://docs.aws.amazon.com/vpc/latest/peering/vpc-peering-basics.html#vpc-peering-limitations limitations>
-- section in the /VPC Peering Guide/.
--
-- The owner of the accepter VPC must accept the peering request to
-- activate the peering connection. The VPC peering connection request
-- expires after 7 days, after which it cannot be accepted or rejected.
--
-- If you create a VPC peering connection request between VPCs with
-- overlapping CIDR blocks, the VPC peering connection has a status of
-- @failed@.
module Network.AWS.EC2.CreateVpcPeeringConnection
  ( -- * Creating a Request
    CreateVpcPeeringConnection (..),
    newCreateVpcPeeringConnection,

    -- * Request Lenses
    createVpcPeeringConnection_tagSpecifications,
    createVpcPeeringConnection_dryRun,
    createVpcPeeringConnection_peerOwnerId,
    createVpcPeeringConnection_vpcId,
    createVpcPeeringConnection_peerRegion,
    createVpcPeeringConnection_peerVpcId,

    -- * Destructuring the Response
    CreateVpcPeeringConnectionResponse (..),
    newCreateVpcPeeringConnectionResponse,

    -- * Response Lenses
    createVpcPeeringConnectionResponse_vpcPeeringConnection,
    createVpcPeeringConnectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateVpcPeeringConnection' smart constructor.
data CreateVpcPeeringConnection = CreateVpcPeeringConnection'
  { -- | The tags to assign to the peering connection.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The AWS account ID of the owner of the accepter VPC.
    --
    -- Default: Your AWS account ID
    peerOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the requester VPC. You must specify this parameter in the
    -- request.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The Region code for the accepter VPC, if the accepter VPC is located in
    -- a Region other than the Region in which you make the request.
    --
    -- Default: The Region in which you make the request.
    peerRegion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC with which you are creating the VPC peering
    -- connection. You must specify this parameter in the request.
    peerVpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcPeeringConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createVpcPeeringConnection_tagSpecifications' - The tags to assign to the peering connection.
--
-- 'dryRun', 'createVpcPeeringConnection_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'peerOwnerId', 'createVpcPeeringConnection_peerOwnerId' - The AWS account ID of the owner of the accepter VPC.
--
-- Default: Your AWS account ID
--
-- 'vpcId', 'createVpcPeeringConnection_vpcId' - The ID of the requester VPC. You must specify this parameter in the
-- request.
--
-- 'peerRegion', 'createVpcPeeringConnection_peerRegion' - The Region code for the accepter VPC, if the accepter VPC is located in
-- a Region other than the Region in which you make the request.
--
-- Default: The Region in which you make the request.
--
-- 'peerVpcId', 'createVpcPeeringConnection_peerVpcId' - The ID of the VPC with which you are creating the VPC peering
-- connection. You must specify this parameter in the request.
newCreateVpcPeeringConnection ::
  CreateVpcPeeringConnection
newCreateVpcPeeringConnection =
  CreateVpcPeeringConnection'
    { tagSpecifications =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      peerOwnerId = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      peerRegion = Prelude.Nothing,
      peerVpcId = Prelude.Nothing
    }

-- | The tags to assign to the peering connection.
createVpcPeeringConnection_tagSpecifications :: Lens.Lens' CreateVpcPeeringConnection (Prelude.Maybe [TagSpecification])
createVpcPeeringConnection_tagSpecifications = Lens.lens (\CreateVpcPeeringConnection' {tagSpecifications} -> tagSpecifications) (\s@CreateVpcPeeringConnection' {} a -> s {tagSpecifications = a} :: CreateVpcPeeringConnection) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createVpcPeeringConnection_dryRun :: Lens.Lens' CreateVpcPeeringConnection (Prelude.Maybe Prelude.Bool)
createVpcPeeringConnection_dryRun = Lens.lens (\CreateVpcPeeringConnection' {dryRun} -> dryRun) (\s@CreateVpcPeeringConnection' {} a -> s {dryRun = a} :: CreateVpcPeeringConnection)

-- | The AWS account ID of the owner of the accepter VPC.
--
-- Default: Your AWS account ID
createVpcPeeringConnection_peerOwnerId :: Lens.Lens' CreateVpcPeeringConnection (Prelude.Maybe Prelude.Text)
createVpcPeeringConnection_peerOwnerId = Lens.lens (\CreateVpcPeeringConnection' {peerOwnerId} -> peerOwnerId) (\s@CreateVpcPeeringConnection' {} a -> s {peerOwnerId = a} :: CreateVpcPeeringConnection)

-- | The ID of the requester VPC. You must specify this parameter in the
-- request.
createVpcPeeringConnection_vpcId :: Lens.Lens' CreateVpcPeeringConnection (Prelude.Maybe Prelude.Text)
createVpcPeeringConnection_vpcId = Lens.lens (\CreateVpcPeeringConnection' {vpcId} -> vpcId) (\s@CreateVpcPeeringConnection' {} a -> s {vpcId = a} :: CreateVpcPeeringConnection)

-- | The Region code for the accepter VPC, if the accepter VPC is located in
-- a Region other than the Region in which you make the request.
--
-- Default: The Region in which you make the request.
createVpcPeeringConnection_peerRegion :: Lens.Lens' CreateVpcPeeringConnection (Prelude.Maybe Prelude.Text)
createVpcPeeringConnection_peerRegion = Lens.lens (\CreateVpcPeeringConnection' {peerRegion} -> peerRegion) (\s@CreateVpcPeeringConnection' {} a -> s {peerRegion = a} :: CreateVpcPeeringConnection)

-- | The ID of the VPC with which you are creating the VPC peering
-- connection. You must specify this parameter in the request.
createVpcPeeringConnection_peerVpcId :: Lens.Lens' CreateVpcPeeringConnection (Prelude.Maybe Prelude.Text)
createVpcPeeringConnection_peerVpcId = Lens.lens (\CreateVpcPeeringConnection' {peerVpcId} -> peerVpcId) (\s@CreateVpcPeeringConnection' {} a -> s {peerVpcId = a} :: CreateVpcPeeringConnection)

instance Core.AWSRequest CreateVpcPeeringConnection where
  type
    AWSResponse CreateVpcPeeringConnection =
      CreateVpcPeeringConnectionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVpcPeeringConnectionResponse'
            Prelude.<$> (x Core..@? "vpcPeeringConnection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVpcPeeringConnection

instance Prelude.NFData CreateVpcPeeringConnection

instance Core.ToHeaders CreateVpcPeeringConnection where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateVpcPeeringConnection where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateVpcPeeringConnection where
  toQuery CreateVpcPeeringConnection' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateVpcPeeringConnection" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "PeerOwnerId" Core.=: peerOwnerId,
        "VpcId" Core.=: vpcId,
        "PeerRegion" Core.=: peerRegion,
        "PeerVpcId" Core.=: peerVpcId
      ]

-- | /See:/ 'newCreateVpcPeeringConnectionResponse' smart constructor.
data CreateVpcPeeringConnectionResponse = CreateVpcPeeringConnectionResponse'
  { -- | Information about the VPC peering connection.
    vpcPeeringConnection :: Prelude.Maybe VpcPeeringConnection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcPeeringConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcPeeringConnection', 'createVpcPeeringConnectionResponse_vpcPeeringConnection' - Information about the VPC peering connection.
--
-- 'httpStatus', 'createVpcPeeringConnectionResponse_httpStatus' - The response's http status code.
newCreateVpcPeeringConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVpcPeeringConnectionResponse
newCreateVpcPeeringConnectionResponse pHttpStatus_ =
  CreateVpcPeeringConnectionResponse'
    { vpcPeeringConnection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the VPC peering connection.
createVpcPeeringConnectionResponse_vpcPeeringConnection :: Lens.Lens' CreateVpcPeeringConnectionResponse (Prelude.Maybe VpcPeeringConnection)
createVpcPeeringConnectionResponse_vpcPeeringConnection = Lens.lens (\CreateVpcPeeringConnectionResponse' {vpcPeeringConnection} -> vpcPeeringConnection) (\s@CreateVpcPeeringConnectionResponse' {} a -> s {vpcPeeringConnection = a} :: CreateVpcPeeringConnectionResponse)

-- | The response's http status code.
createVpcPeeringConnectionResponse_httpStatus :: Lens.Lens' CreateVpcPeeringConnectionResponse Prelude.Int
createVpcPeeringConnectionResponse_httpStatus = Lens.lens (\CreateVpcPeeringConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateVpcPeeringConnectionResponse' {} a -> s {httpStatus = a} :: CreateVpcPeeringConnectionResponse)

instance
  Prelude.NFData
    CreateVpcPeeringConnectionResponse
