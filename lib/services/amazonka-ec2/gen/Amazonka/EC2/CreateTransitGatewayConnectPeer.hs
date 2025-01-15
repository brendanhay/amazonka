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
-- Module      : Amazonka.EC2.CreateTransitGatewayConnectPeer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.EC2.CreateTransitGatewayConnectPeer
  ( -- * Creating a Request
    CreateTransitGatewayConnectPeer (..),
    newCreateTransitGatewayConnectPeer,

    -- * Request Lenses
    createTransitGatewayConnectPeer_bgpOptions,
    createTransitGatewayConnectPeer_dryRun,
    createTransitGatewayConnectPeer_tagSpecifications,
    createTransitGatewayConnectPeer_transitGatewayAddress,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTransitGatewayConnectPeer' smart constructor.
data CreateTransitGatewayConnectPeer = CreateTransitGatewayConnectPeer'
  { -- | The BGP options for the Connect peer.
    bgpOptions :: Prelude.Maybe TransitGatewayConnectRequestBgpOptions,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply to the Connect peer.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The peer IP address (GRE outer IP address) on the transit gateway side
    -- of the Connect peer, which must be specified from a transit gateway CIDR
    -- block. If not specified, Amazon automatically assigns the first
    -- available IP address from the transit gateway CIDR block.
    transitGatewayAddress :: Prelude.Maybe Prelude.Text,
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
-- 'bgpOptions', 'createTransitGatewayConnectPeer_bgpOptions' - The BGP options for the Connect peer.
--
-- 'dryRun', 'createTransitGatewayConnectPeer_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createTransitGatewayConnectPeer_tagSpecifications' - The tags to apply to the Connect peer.
--
-- 'transitGatewayAddress', 'createTransitGatewayConnectPeer_transitGatewayAddress' - The peer IP address (GRE outer IP address) on the transit gateway side
-- of the Connect peer, which must be specified from a transit gateway CIDR
-- block. If not specified, Amazon automatically assigns the first
-- available IP address from the transit gateway CIDR block.
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
      { bgpOptions =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        transitGatewayAddress = Prelude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_,
        peerAddress = pPeerAddress_,
        insideCidrBlocks = Prelude.mempty
      }

-- | The BGP options for the Connect peer.
createTransitGatewayConnectPeer_bgpOptions :: Lens.Lens' CreateTransitGatewayConnectPeer (Prelude.Maybe TransitGatewayConnectRequestBgpOptions)
createTransitGatewayConnectPeer_bgpOptions = Lens.lens (\CreateTransitGatewayConnectPeer' {bgpOptions} -> bgpOptions) (\s@CreateTransitGatewayConnectPeer' {} a -> s {bgpOptions = a} :: CreateTransitGatewayConnectPeer)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayConnectPeer_dryRun :: Lens.Lens' CreateTransitGatewayConnectPeer (Prelude.Maybe Prelude.Bool)
createTransitGatewayConnectPeer_dryRun = Lens.lens (\CreateTransitGatewayConnectPeer' {dryRun} -> dryRun) (\s@CreateTransitGatewayConnectPeer' {} a -> s {dryRun = a} :: CreateTransitGatewayConnectPeer)

-- | The tags to apply to the Connect peer.
createTransitGatewayConnectPeer_tagSpecifications :: Lens.Lens' CreateTransitGatewayConnectPeer (Prelude.Maybe [TagSpecification])
createTransitGatewayConnectPeer_tagSpecifications = Lens.lens (\CreateTransitGatewayConnectPeer' {tagSpecifications} -> tagSpecifications) (\s@CreateTransitGatewayConnectPeer' {} a -> s {tagSpecifications = a} :: CreateTransitGatewayConnectPeer) Prelude.. Lens.mapping Lens.coerced

-- | The peer IP address (GRE outer IP address) on the transit gateway side
-- of the Connect peer, which must be specified from a transit gateway CIDR
-- block. If not specified, Amazon automatically assigns the first
-- available IP address from the transit gateway CIDR block.
createTransitGatewayConnectPeer_transitGatewayAddress :: Lens.Lens' CreateTransitGatewayConnectPeer (Prelude.Maybe Prelude.Text)
createTransitGatewayConnectPeer_transitGatewayAddress = Lens.lens (\CreateTransitGatewayConnectPeer' {transitGatewayAddress} -> transitGatewayAddress) (\s@CreateTransitGatewayConnectPeer' {} a -> s {transitGatewayAddress = a} :: CreateTransitGatewayConnectPeer)

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
createTransitGatewayConnectPeer_insideCidrBlocks = Lens.lens (\CreateTransitGatewayConnectPeer' {insideCidrBlocks} -> insideCidrBlocks) (\s@CreateTransitGatewayConnectPeer' {} a -> s {insideCidrBlocks = a} :: CreateTransitGatewayConnectPeer) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    CreateTransitGatewayConnectPeer
  where
  type
    AWSResponse CreateTransitGatewayConnectPeer =
      CreateTransitGatewayConnectPeerResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayConnectPeerResponse'
            Prelude.<$> (x Data..@? "transitGatewayConnectPeer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTransitGatewayConnectPeer
  where
  hashWithSalt
    _salt
    CreateTransitGatewayConnectPeer' {..} =
      _salt
        `Prelude.hashWithSalt` bgpOptions
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` tagSpecifications
        `Prelude.hashWithSalt` transitGatewayAddress
        `Prelude.hashWithSalt` transitGatewayAttachmentId
        `Prelude.hashWithSalt` peerAddress
        `Prelude.hashWithSalt` insideCidrBlocks

instance
  Prelude.NFData
    CreateTransitGatewayConnectPeer
  where
  rnf CreateTransitGatewayConnectPeer' {..} =
    Prelude.rnf bgpOptions `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf tagSpecifications `Prelude.seq`
          Prelude.rnf transitGatewayAddress `Prelude.seq`
            Prelude.rnf transitGatewayAttachmentId `Prelude.seq`
              Prelude.rnf peerAddress `Prelude.seq`
                Prelude.rnf insideCidrBlocks

instance
  Data.ToHeaders
    CreateTransitGatewayConnectPeer
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateTransitGatewayConnectPeer where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTransitGatewayConnectPeer where
  toQuery CreateTransitGatewayConnectPeer' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateTransitGatewayConnectPeer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "BgpOptions" Data.=: bgpOptions,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "TransitGatewayAddress"
          Data.=: transitGatewayAddress,
        "TransitGatewayAttachmentId"
          Data.=: transitGatewayAttachmentId,
        "PeerAddress" Data.=: peerAddress,
        Data.toQueryList "InsideCidrBlocks" insideCidrBlocks
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
  where
  rnf CreateTransitGatewayConnectPeerResponse' {..} =
    Prelude.rnf transitGatewayConnectPeer `Prelude.seq`
      Prelude.rnf httpStatus
