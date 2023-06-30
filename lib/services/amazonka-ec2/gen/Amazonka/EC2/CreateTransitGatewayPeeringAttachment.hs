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
-- Module      : Amazonka.EC2.CreateTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a transit gateway peering attachment between the specified
-- transit gateway (requester) and a peer transit gateway (accepter). The
-- peer transit gateway can be in your account or a different Amazon Web
-- Services account.
--
-- After you create the peering attachment, the owner of the accepter
-- transit gateway must accept the attachment request.
module Amazonka.EC2.CreateTransitGatewayPeeringAttachment
  ( -- * Creating a Request
    CreateTransitGatewayPeeringAttachment (..),
    newCreateTransitGatewayPeeringAttachment,

    -- * Request Lenses
    createTransitGatewayPeeringAttachment_dryRun,
    createTransitGatewayPeeringAttachment_options,
    createTransitGatewayPeeringAttachment_tagSpecifications,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTransitGatewayPeeringAttachment' smart constructor.
data CreateTransitGatewayPeeringAttachment = CreateTransitGatewayPeeringAttachment'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Requests a transit gateway peering attachment.
    options :: Prelude.Maybe CreateTransitGatewayPeeringAttachmentRequestOptions,
    -- | The tags to apply to the transit gateway peering attachment.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Text,
    -- | The ID of the peer transit gateway with which to create the peering
    -- attachment.
    peerTransitGatewayId :: Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the peer transit
    -- gateway.
    peerAccountId :: Prelude.Text,
    -- | The Region where the peer transit gateway is located.
    peerRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayPeeringAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createTransitGatewayPeeringAttachment_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'options', 'createTransitGatewayPeeringAttachment_options' - Requests a transit gateway peering attachment.
--
-- 'tagSpecifications', 'createTransitGatewayPeeringAttachment_tagSpecifications' - The tags to apply to the transit gateway peering attachment.
--
-- 'transitGatewayId', 'createTransitGatewayPeeringAttachment_transitGatewayId' - The ID of the transit gateway.
--
-- 'peerTransitGatewayId', 'createTransitGatewayPeeringAttachment_peerTransitGatewayId' - The ID of the peer transit gateway with which to create the peering
-- attachment.
--
-- 'peerAccountId', 'createTransitGatewayPeeringAttachment_peerAccountId' - The ID of the Amazon Web Services account that owns the peer transit
-- gateway.
--
-- 'peerRegion', 'createTransitGatewayPeeringAttachment_peerRegion' - The Region where the peer transit gateway is located.
newCreateTransitGatewayPeeringAttachment ::
  -- | 'transitGatewayId'
  Prelude.Text ->
  -- | 'peerTransitGatewayId'
  Prelude.Text ->
  -- | 'peerAccountId'
  Prelude.Text ->
  -- | 'peerRegion'
  Prelude.Text ->
  CreateTransitGatewayPeeringAttachment
newCreateTransitGatewayPeeringAttachment
  pTransitGatewayId_
  pPeerTransitGatewayId_
  pPeerAccountId_
  pPeerRegion_ =
    CreateTransitGatewayPeeringAttachment'
      { dryRun =
          Prelude.Nothing,
        options = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        transitGatewayId =
          pTransitGatewayId_,
        peerTransitGatewayId =
          pPeerTransitGatewayId_,
        peerAccountId = pPeerAccountId_,
        peerRegion = pPeerRegion_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTransitGatewayPeeringAttachment_dryRun :: Lens.Lens' CreateTransitGatewayPeeringAttachment (Prelude.Maybe Prelude.Bool)
createTransitGatewayPeeringAttachment_dryRun = Lens.lens (\CreateTransitGatewayPeeringAttachment' {dryRun} -> dryRun) (\s@CreateTransitGatewayPeeringAttachment' {} a -> s {dryRun = a} :: CreateTransitGatewayPeeringAttachment)

-- | Requests a transit gateway peering attachment.
createTransitGatewayPeeringAttachment_options :: Lens.Lens' CreateTransitGatewayPeeringAttachment (Prelude.Maybe CreateTransitGatewayPeeringAttachmentRequestOptions)
createTransitGatewayPeeringAttachment_options = Lens.lens (\CreateTransitGatewayPeeringAttachment' {options} -> options) (\s@CreateTransitGatewayPeeringAttachment' {} a -> s {options = a} :: CreateTransitGatewayPeeringAttachment)

-- | The tags to apply to the transit gateway peering attachment.
createTransitGatewayPeeringAttachment_tagSpecifications :: Lens.Lens' CreateTransitGatewayPeeringAttachment (Prelude.Maybe [TagSpecification])
createTransitGatewayPeeringAttachment_tagSpecifications = Lens.lens (\CreateTransitGatewayPeeringAttachment' {tagSpecifications} -> tagSpecifications) (\s@CreateTransitGatewayPeeringAttachment' {} a -> s {tagSpecifications = a} :: CreateTransitGatewayPeeringAttachment) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway.
createTransitGatewayPeeringAttachment_transitGatewayId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Prelude.Text
createTransitGatewayPeeringAttachment_transitGatewayId = Lens.lens (\CreateTransitGatewayPeeringAttachment' {transitGatewayId} -> transitGatewayId) (\s@CreateTransitGatewayPeeringAttachment' {} a -> s {transitGatewayId = a} :: CreateTransitGatewayPeeringAttachment)

-- | The ID of the peer transit gateway with which to create the peering
-- attachment.
createTransitGatewayPeeringAttachment_peerTransitGatewayId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Prelude.Text
createTransitGatewayPeeringAttachment_peerTransitGatewayId = Lens.lens (\CreateTransitGatewayPeeringAttachment' {peerTransitGatewayId} -> peerTransitGatewayId) (\s@CreateTransitGatewayPeeringAttachment' {} a -> s {peerTransitGatewayId = a} :: CreateTransitGatewayPeeringAttachment)

-- | The ID of the Amazon Web Services account that owns the peer transit
-- gateway.
createTransitGatewayPeeringAttachment_peerAccountId :: Lens.Lens' CreateTransitGatewayPeeringAttachment Prelude.Text
createTransitGatewayPeeringAttachment_peerAccountId = Lens.lens (\CreateTransitGatewayPeeringAttachment' {peerAccountId} -> peerAccountId) (\s@CreateTransitGatewayPeeringAttachment' {} a -> s {peerAccountId = a} :: CreateTransitGatewayPeeringAttachment)

-- | The Region where the peer transit gateway is located.
createTransitGatewayPeeringAttachment_peerRegion :: Lens.Lens' CreateTransitGatewayPeeringAttachment Prelude.Text
createTransitGatewayPeeringAttachment_peerRegion = Lens.lens (\CreateTransitGatewayPeeringAttachment' {peerRegion} -> peerRegion) (\s@CreateTransitGatewayPeeringAttachment' {} a -> s {peerRegion = a} :: CreateTransitGatewayPeeringAttachment)

instance
  Core.AWSRequest
    CreateTransitGatewayPeeringAttachment
  where
  type
    AWSResponse
      CreateTransitGatewayPeeringAttachment =
      CreateTransitGatewayPeeringAttachmentResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayPeeringAttachmentResponse'
            Prelude.<$> (x Data..@? "transitGatewayPeeringAttachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTransitGatewayPeeringAttachment
  where
  hashWithSalt
    _salt
    CreateTransitGatewayPeeringAttachment' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` options
        `Prelude.hashWithSalt` tagSpecifications
        `Prelude.hashWithSalt` transitGatewayId
        `Prelude.hashWithSalt` peerTransitGatewayId
        `Prelude.hashWithSalt` peerAccountId
        `Prelude.hashWithSalt` peerRegion

instance
  Prelude.NFData
    CreateTransitGatewayPeeringAttachment
  where
  rnf CreateTransitGatewayPeeringAttachment' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf peerTransitGatewayId
      `Prelude.seq` Prelude.rnf peerAccountId
      `Prelude.seq` Prelude.rnf peerRegion

instance
  Data.ToHeaders
    CreateTransitGatewayPeeringAttachment
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    CreateTransitGatewayPeeringAttachment
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateTransitGatewayPeeringAttachment
  where
  toQuery CreateTransitGatewayPeeringAttachment' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateTransitGatewayPeeringAttachment" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Options" Data.=: options,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "TransitGatewayId" Data.=: transitGatewayId,
        "PeerTransitGatewayId" Data.=: peerTransitGatewayId,
        "PeerAccountId" Data.=: peerAccountId,
        "PeerRegion" Data.=: peerRegion
      ]

-- | /See:/ 'newCreateTransitGatewayPeeringAttachmentResponse' smart constructor.
data CreateTransitGatewayPeeringAttachmentResponse = CreateTransitGatewayPeeringAttachmentResponse'
  { -- | The transit gateway peering attachment.
    transitGatewayPeeringAttachment :: Prelude.Maybe TransitGatewayPeeringAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateTransitGatewayPeeringAttachmentResponse
newCreateTransitGatewayPeeringAttachmentResponse
  pHttpStatus_ =
    CreateTransitGatewayPeeringAttachmentResponse'
      { transitGatewayPeeringAttachment =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The transit gateway peering attachment.
createTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment :: Lens.Lens' CreateTransitGatewayPeeringAttachmentResponse (Prelude.Maybe TransitGatewayPeeringAttachment)
createTransitGatewayPeeringAttachmentResponse_transitGatewayPeeringAttachment = Lens.lens (\CreateTransitGatewayPeeringAttachmentResponse' {transitGatewayPeeringAttachment} -> transitGatewayPeeringAttachment) (\s@CreateTransitGatewayPeeringAttachmentResponse' {} a -> s {transitGatewayPeeringAttachment = a} :: CreateTransitGatewayPeeringAttachmentResponse)

-- | The response's http status code.
createTransitGatewayPeeringAttachmentResponse_httpStatus :: Lens.Lens' CreateTransitGatewayPeeringAttachmentResponse Prelude.Int
createTransitGatewayPeeringAttachmentResponse_httpStatus = Lens.lens (\CreateTransitGatewayPeeringAttachmentResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayPeeringAttachmentResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayPeeringAttachmentResponse)

instance
  Prelude.NFData
    CreateTransitGatewayPeeringAttachmentResponse
  where
  rnf
    CreateTransitGatewayPeeringAttachmentResponse' {..} =
      Prelude.rnf transitGatewayPeeringAttachment
        `Prelude.seq` Prelude.rnf httpStatus
