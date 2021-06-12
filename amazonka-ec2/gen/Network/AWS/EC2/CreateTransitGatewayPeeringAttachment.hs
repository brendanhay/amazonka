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
-- Module      : Network.AWS.EC2.CreateTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a transit gateway peering attachment between the specified
-- transit gateway (requester) and a peer transit gateway (accepter). The
-- transit gateways must be in different Regions. The peer transit gateway
-- can be in your account or a different AWS account.
--
-- After you create the peering attachment, the owner of the accepter
-- transit gateway must accept the attachment request.
module Network.AWS.EC2.CreateTransitGatewayPeeringAttachment
  ( -- * Creating a Request
    CreateTransitGatewayPeeringAttachment (..),
    newCreateTransitGatewayPeeringAttachment,

    -- * Request Lenses
    createTransitGatewayPeeringAttachment_tagSpecifications,
    createTransitGatewayPeeringAttachment_dryRun,
    createTransitGatewayPeeringAttachment_transitGatewayId,
    createTransitGatewayPeeringAttachment_peerTransitGatewayId,
    createTransitGatewayPeeringAttachment_peerAccountId,
    createTransitGatewayPeeringAttachment_peerRegion,

    -- * Destructuring the Response
    CreateTransitGatewayPeeringAttachmentResponse (..),
    newCreateTransitGatewayPeeringAttachmentResponse,

    -- * Response Lenses
    createTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment,
    createTransitGatewayPeeringAttachmentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTransitGatewayPeeringAttachment' smart constructor.
data CreateTransitGatewayPeeringAttachment = CreateTransitGatewayPeeringAttachment'
  { -- | The tags to apply to the transit gateway peering attachment.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the transit gateway.
    transitGatewayId :: Core.Text,
    -- | The ID of the peer transit gateway with which to create the peering
    -- attachment.
    peerTransitGatewayId :: Core.Text,
    -- | The AWS account ID of the owner of the peer transit gateway.
    peerAccountId :: Core.Text,
    -- | The Region where the peer transit gateway is located.
    peerRegion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTransitGatewayPeeringAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createTransitGatewayPeeringAttachment_tagSpecifications' - The tags to apply to the transit gateway peering attachment.
--
-- 'dryRun', 'createTransitGatewayPeeringAttachment_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayId', 'createTransitGatewayPeeringAttachment_transitGatewayId' - The ID of the transit gateway.
--
-- 'peerTransitGatewayId', 'createTransitGatewayPeeringAttachment_peerTransitGatewayId' - The ID of the peer transit gateway with which to create the peering
-- attachment.
--
-- 'peerAccountId', 'createTransitGatewayPeeringAttachment_peerAccountId' - The AWS account ID of the owner of the peer transit gateway.
--
-- 'peerRegion', 'createTransitGatewayPeeringAttachment_peerRegion' - The Region where the peer transit gateway is located.
newCreateTransitGatewayPeeringAttachment ::
  -- | 'transitGatewayId'
  Core.Text ->
  -- | 'peerTransitGatewayId'
  Core.Text ->
  -- | 'peerAccountId'
  Core.Text ->
  -- | 'peerRegion'
  Core.Text ->
  CreateTransitGatewayPeeringAttachment
newCreateTransitGatewayPeeringAttachment
  pTransitGatewayId_
  pPeerTransitGatewayId_
  pPeerAccountId_
  pPeerRegion_ =
    CreateTransitGatewayPeeringAttachment'
      { tagSpecifications =
          Core.Nothing,
        dryRun = Core.Nothing,
        transitGatewayId =
          pTransitGatewayId_,
        peerTransitGatewayId =
          pPeerTransitGatewayId_,
        peerAccountId = pPeerAccountId_,
        peerRegion = pPeerRegion_
      }

-- | The tags to apply to the transit gateway peering attachment.
createTransitGatewayPeeringAttachment_tagSpecifications :: Lens.Lens' CreateTransitGatewayPeeringAttachment (Core.Maybe [TagSpecification])
createTransitGatewayPeeringAttachment_tagSpecifications = Lens.lens (\CreateTransitGatewayPeeringAttachment' {tagSpecifications} -> tagSpecifications) (\s@CreateTransitGatewayPeeringAttachment' {} a -> s {tagSpecifications = a} :: CreateTransitGatewayPeeringAttachment) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayPeeringAttachment_dryRun :: Lens.Lens' CreateTransitGatewayPeeringAttachment (Core.Maybe Core.Bool)
createTransitGatewayPeeringAttachment_dryRun = Lens.lens (\CreateTransitGatewayPeeringAttachment' {dryRun} -> dryRun) (\s@CreateTransitGatewayPeeringAttachment' {} a -> s {dryRun = a} :: CreateTransitGatewayPeeringAttachment)

-- | The ID of the transit gateway.
createTransitGatewayPeeringAttachment_transitGatewayId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Core.Text
createTransitGatewayPeeringAttachment_transitGatewayId = Lens.lens (\CreateTransitGatewayPeeringAttachment' {transitGatewayId} -> transitGatewayId) (\s@CreateTransitGatewayPeeringAttachment' {} a -> s {transitGatewayId = a} :: CreateTransitGatewayPeeringAttachment)

-- | The ID of the peer transit gateway with which to create the peering
-- attachment.
createTransitGatewayPeeringAttachment_peerTransitGatewayId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Core.Text
createTransitGatewayPeeringAttachment_peerTransitGatewayId = Lens.lens (\CreateTransitGatewayPeeringAttachment' {peerTransitGatewayId} -> peerTransitGatewayId) (\s@CreateTransitGatewayPeeringAttachment' {} a -> s {peerTransitGatewayId = a} :: CreateTransitGatewayPeeringAttachment)

-- | The AWS account ID of the owner of the peer transit gateway.
createTransitGatewayPeeringAttachment_peerAccountId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Core.Text
createTransitGatewayPeeringAttachment_peerAccountId = Lens.lens (\CreateTransitGatewayPeeringAttachment' {peerAccountId} -> peerAccountId) (\s@CreateTransitGatewayPeeringAttachment' {} a -> s {peerAccountId = a} :: CreateTransitGatewayPeeringAttachment)

-- | The Region where the peer transit gateway is located.
createTransitGatewayPeeringAttachment_peerRegion :: Lens.Lens' CreateTransitGatewayPeeringAttachment Core.Text
createTransitGatewayPeeringAttachment_peerRegion = Lens.lens (\CreateTransitGatewayPeeringAttachment' {peerRegion} -> peerRegion) (\s@CreateTransitGatewayPeeringAttachment' {} a -> s {peerRegion = a} :: CreateTransitGatewayPeeringAttachment)

instance
  Core.AWSRequest
    CreateTransitGatewayPeeringAttachment
  where
  type
    AWSResponse
      CreateTransitGatewayPeeringAttachment =
      CreateTransitGatewayPeeringAttachmentResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayPeeringAttachmentResponse'
            Core.<$> (x Core..@? "transitGatewayPeeringAttachment")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateTransitGatewayPeeringAttachment

instance
  Core.NFData
    CreateTransitGatewayPeeringAttachment

instance
  Core.ToHeaders
    CreateTransitGatewayPeeringAttachment
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    CreateTransitGatewayPeeringAttachment
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    CreateTransitGatewayPeeringAttachment
  where
  toQuery CreateTransitGatewayPeeringAttachment' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "CreateTransitGatewayPeeringAttachment" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "TransitGatewayId" Core.=: transitGatewayId,
        "PeerTransitGatewayId" Core.=: peerTransitGatewayId,
        "PeerAccountId" Core.=: peerAccountId,
        "PeerRegion" Core.=: peerRegion
      ]

-- | /See:/ 'newCreateTransitGatewayPeeringAttachmentResponse' smart constructor.
data CreateTransitGatewayPeeringAttachmentResponse = CreateTransitGatewayPeeringAttachmentResponse'
  { -- | The transit gateway peering attachment.
    transitGatewayPeeringAttachment :: Core.Maybe TransitGatewayPeeringAttachment,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTransitGatewayPeeringAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayPeeringAttachment', 'createTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment' - The transit gateway peering attachment.
--
-- 'httpStatus', 'createTransitGatewayPeeringAttachmentResponse_httpStatus' - The response's http status code.
newCreateTransitGatewayPeeringAttachmentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateTransitGatewayPeeringAttachmentResponse
newCreateTransitGatewayPeeringAttachmentResponse
  pHttpStatus_ =
    CreateTransitGatewayPeeringAttachmentResponse'
      { transitGatewayPeeringAttachment =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The transit gateway peering attachment.
createTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment :: Lens.Lens' CreateTransitGatewayPeeringAttachmentResponse (Core.Maybe TransitGatewayPeeringAttachment)
createTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment = Lens.lens (\CreateTransitGatewayPeeringAttachmentResponse' {transitGatewayPeeringAttachment} -> transitGatewayPeeringAttachment) (\s@CreateTransitGatewayPeeringAttachmentResponse' {} a -> s {transitGatewayPeeringAttachment = a} :: CreateTransitGatewayPeeringAttachmentResponse)

-- | The response's http status code.
createTransitGatewayPeeringAttachmentResponse_httpStatus :: Lens.Lens' CreateTransitGatewayPeeringAttachmentResponse Core.Int
createTransitGatewayPeeringAttachmentResponse_httpStatus = Lens.lens (\CreateTransitGatewayPeeringAttachmentResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayPeeringAttachmentResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayPeeringAttachmentResponse)

instance
  Core.NFData
    CreateTransitGatewayPeeringAttachmentResponse
