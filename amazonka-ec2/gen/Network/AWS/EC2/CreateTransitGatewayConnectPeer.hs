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
-- Module      : Network.AWS.EC2.CreateTransitGatewayConnectPeer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Connect peer for a specified transit gateway Connect
-- attachment between a transit gateway and an appliance.
--
-- The peer address and transit gateway address must be the same IP address
-- family (IPv4 or IPv6).
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/tgw/tgw-connect.html#tgw-connect-peer Connect peers>
-- in the /Transit Gateways Guide/.
module Network.AWS.EC2.CreateTransitGatewayConnectPeer
  ( -- * Creating a Request
    CreateTransitGatewayConnectPeer (..),
    newCreateTransitGatewayConnectPeer,

    -- * Request Lenses
    createTransitGatewayConnectPeer_tagSpecifications,
    createTransitGatewayConnectPeer_dryRun,
    createTransitGatewayConnectPeer_transitGatewayAddress,
    createTransitGatewayConnectPeer_bgpOptions,
    createTransitGatewayConnectPeer_transitGatewayAttachmentId,
    createTransitGatewayConnectPeer_peerAddress,
    createTransitGatewayConnectPeer_insideCidrBlocks,

    -- * Destructuring the Response
    CreateTransitGatewayConnectPeerResponse (..),
    newCreateTransitGatewayConnectPeerResponse,

    -- * Response Lenses
    createTransitGatewayConnectPeerResponse_transitGatewayConnectPeer,
    createTransitGatewayConnectPeerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTransitGatewayConnectPeer' smart constructor.
data CreateTransitGatewayConnectPeer = CreateTransitGatewayConnectPeer'
  { -- | The tags to apply to the Connect peer.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The peer IP address (GRE outer IP address) on the transit gateway side
    -- of the Connect peer, which must be specified from a transit gateway CIDR
    -- block. If not specified, Amazon automatically assigns the first
    -- available IP address from the transit gateway CIDR block.
    transitGatewayAddress :: Prelude.Maybe Prelude.Text,
    -- | The BGP options for the Connect peer.
    bgpOptions :: Prelude.Maybe TransitGatewayConnectRequestBgpOptions,
    -- | The ID of the Connect attachment.
    transitGatewayAttachmentId :: Prelude.Text,
    -- | The peer IP address (GRE outer IP address) on the appliance side of the
    -- Connect peer.
    peerAddress :: Prelude.Text,
    -- | The range of inside IP addresses that are used for BGP peering. You must
    -- specify a size \/29 IPv4 CIDR block from the @169.254.0.0\/16@ range.
    -- The first address from the range must be configured on the appliance as
    -- the BGP IP address. You can also optionally specify a size \/125 IPv6
    -- CIDR block from the @fd00::\/8@ range.
    insideCidrBlocks :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayConnectPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createTransitGatewayConnectPeer_tagSpecifications' - The tags to apply to the Connect peer.
--
-- 'dryRun', 'createTransitGatewayConnectPeer_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayAddress', 'createTransitGatewayConnectPeer_transitGatewayAddress' - The peer IP address (GRE outer IP address) on the transit gateway side
-- of the Connect peer, which must be specified from a transit gateway CIDR
-- block. If not specified, Amazon automatically assigns the first
-- available IP address from the transit gateway CIDR block.
--
-- 'bgpOptions', 'createTransitGatewayConnectPeer_bgpOptions' - The BGP options for the Connect peer.
--
-- 'transitGatewayAttachmentId', 'createTransitGatewayConnectPeer_transitGatewayAttachmentId' - The ID of the Connect attachment.
--
-- 'peerAddress', 'createTransitGatewayConnectPeer_peerAddress' - The peer IP address (GRE outer IP address) on the appliance side of the
-- Connect peer.
--
-- 'insideCidrBlocks', 'createTransitGatewayConnectPeer_insideCidrBlocks' - The range of inside IP addresses that are used for BGP peering. You must
-- specify a size \/29 IPv4 CIDR block from the @169.254.0.0\/16@ range.
-- The first address from the range must be configured on the appliance as
-- the BGP IP address. You can also optionally specify a size \/125 IPv6
-- CIDR block from the @fd00::\/8@ range.
newCreateTransitGatewayConnectPeer ::
  -- | 'transitGatewayAttachmentId'
  Prelude.Text ->
  -- | 'peerAddress'
  Prelude.Text ->
  CreateTransitGatewayConnectPeer
newCreateTransitGatewayConnectPeer
  pTransitGatewayAttachmentId_
  pPeerAddress_ =
    CreateTransitGatewayConnectPeer'
      { tagSpecifications =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        transitGatewayAddress = Prelude.Nothing,
        bgpOptions = Prelude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_,
        peerAddress = pPeerAddress_,
        insideCidrBlocks = Prelude.mempty
      }

-- | The tags to apply to the Connect peer.
createTransitGatewayConnectPeer_tagSpecifications :: Lens.Lens' CreateTransitGatewayConnectPeer (Prelude.Maybe [TagSpecification])
createTransitGatewayConnectPeer_tagSpecifications = Lens.lens (\CreateTransitGatewayConnectPeer' {tagSpecifications} -> tagSpecifications) (\s@CreateTransitGatewayConnectPeer' {} a -> s {tagSpecifications = a} :: CreateTransitGatewayConnectPeer) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayConnectPeer_dryRun :: Lens.Lens' CreateTransitGatewayConnectPeer (Prelude.Maybe Prelude.Bool)
createTransitGatewayConnectPeer_dryRun = Lens.lens (\CreateTransitGatewayConnectPeer' {dryRun} -> dryRun) (\s@CreateTransitGatewayConnectPeer' {} a -> s {dryRun = a} :: CreateTransitGatewayConnectPeer)

-- | The peer IP address (GRE outer IP address) on the transit gateway side
-- of the Connect peer, which must be specified from a transit gateway CIDR
-- block. If not specified, Amazon automatically assigns the first
-- available IP address from the transit gateway CIDR block.
createTransitGatewayConnectPeer_transitGatewayAddress :: Lens.Lens' CreateTransitGatewayConnectPeer (Prelude.Maybe Prelude.Text)
createTransitGatewayConnectPeer_transitGatewayAddress = Lens.lens (\CreateTransitGatewayConnectPeer' {transitGatewayAddress} -> transitGatewayAddress) (\s@CreateTransitGatewayConnectPeer' {} a -> s {transitGatewayAddress = a} :: CreateTransitGatewayConnectPeer)

-- | The BGP options for the Connect peer.
createTransitGatewayConnectPeer_bgpOptions :: Lens.Lens' CreateTransitGatewayConnectPeer (Prelude.Maybe TransitGatewayConnectRequestBgpOptions)
createTransitGatewayConnectPeer_bgpOptions = Lens.lens (\CreateTransitGatewayConnectPeer' {bgpOptions} -> bgpOptions) (\s@CreateTransitGatewayConnectPeer' {} a -> s {bgpOptions = a} :: CreateTransitGatewayConnectPeer)

-- | The ID of the Connect attachment.
createTransitGatewayConnectPeer_transitGatewayAttachmentId :: Lens.Lens' CreateTransitGatewayConnectPeer Prelude.Text
createTransitGatewayConnectPeer_transitGatewayAttachmentId = Lens.lens (\CreateTransitGatewayConnectPeer' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@CreateTransitGatewayConnectPeer' {} a -> s {transitGatewayAttachmentId = a} :: CreateTransitGatewayConnectPeer)

-- | The peer IP address (GRE outer IP address) on the appliance side of the
-- Connect peer.
createTransitGatewayConnectPeer_peerAddress :: Lens.Lens' CreateTransitGatewayConnectPeer Prelude.Text
createTransitGatewayConnectPeer_peerAddress = Lens.lens (\CreateTransitGatewayConnectPeer' {peerAddress} -> peerAddress) (\s@CreateTransitGatewayConnectPeer' {} a -> s {peerAddress = a} :: CreateTransitGatewayConnectPeer)

-- | The range of inside IP addresses that are used for BGP peering. You must
-- specify a size \/29 IPv4 CIDR block from the @169.254.0.0\/16@ range.
-- The first address from the range must be configured on the appliance as
-- the BGP IP address. You can also optionally specify a size \/125 IPv6
-- CIDR block from the @fd00::\/8@ range.
createTransitGatewayConnectPeer_insideCidrBlocks :: Lens.Lens' CreateTransitGatewayConnectPeer [Prelude.Text]
createTransitGatewayConnectPeer_insideCidrBlocks = Lens.lens (\CreateTransitGatewayConnectPeer' {insideCidrBlocks} -> insideCidrBlocks) (\s@CreateTransitGatewayConnectPeer' {} a -> s {insideCidrBlocks = a} :: CreateTransitGatewayConnectPeer) Prelude.. Lens._Coerce

instance
  Core.AWSRequest
    CreateTransitGatewayConnectPeer
  where
  type
    AWSResponse CreateTransitGatewayConnectPeer =
      CreateTransitGatewayConnectPeerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayConnectPeerResponse'
            Prelude.<$> (x Core..@? "transitGatewayConnectPeer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTransitGatewayConnectPeer

instance
  Prelude.NFData
    CreateTransitGatewayConnectPeer

instance
  Core.ToHeaders
    CreateTransitGatewayConnectPeer
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateTransitGatewayConnectPeer where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateTransitGatewayConnectPeer where
  toQuery CreateTransitGatewayConnectPeer' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateTransitGatewayConnectPeer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "TransitGatewayAddress"
          Core.=: transitGatewayAddress,
        "BgpOptions" Core.=: bgpOptions,
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId,
        "PeerAddress" Core.=: peerAddress,
        Core.toQueryList "InsideCidrBlocks" insideCidrBlocks
      ]

-- | /See:/ 'newCreateTransitGatewayConnectPeerResponse' smart constructor.
data CreateTransitGatewayConnectPeerResponse = CreateTransitGatewayConnectPeerResponse'
  { -- | Information about the Connect peer.
    transitGatewayConnectPeer :: Prelude.Maybe TransitGatewayConnectPeer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayConnectPeerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayConnectPeer', 'createTransitGatewayConnectPeerResponse_transitGatewayConnectPeer' - Information about the Connect peer.
--
-- 'httpStatus', 'createTransitGatewayConnectPeerResponse_httpStatus' - The response's http status code.
newCreateTransitGatewayConnectPeerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTransitGatewayConnectPeerResponse
newCreateTransitGatewayConnectPeerResponse
  pHttpStatus_ =
    CreateTransitGatewayConnectPeerResponse'
      { transitGatewayConnectPeer =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the Connect peer.
createTransitGatewayConnectPeerResponse_transitGatewayConnectPeer :: Lens.Lens' CreateTransitGatewayConnectPeerResponse (Prelude.Maybe TransitGatewayConnectPeer)
createTransitGatewayConnectPeerResponse_transitGatewayConnectPeer = Lens.lens (\CreateTransitGatewayConnectPeerResponse' {transitGatewayConnectPeer} -> transitGatewayConnectPeer) (\s@CreateTransitGatewayConnectPeerResponse' {} a -> s {transitGatewayConnectPeer = a} :: CreateTransitGatewayConnectPeerResponse)

-- | The response's http status code.
createTransitGatewayConnectPeerResponse_httpStatus :: Lens.Lens' CreateTransitGatewayConnectPeerResponse Prelude.Int
createTransitGatewayConnectPeerResponse_httpStatus = Lens.lens (\CreateTransitGatewayConnectPeerResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayConnectPeerResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayConnectPeerResponse)

instance
  Prelude.NFData
    CreateTransitGatewayConnectPeerResponse
