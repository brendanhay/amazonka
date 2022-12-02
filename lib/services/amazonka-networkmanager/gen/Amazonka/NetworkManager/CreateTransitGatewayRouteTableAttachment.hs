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
-- Module      : Amazonka.NetworkManager.CreateTransitGatewayRouteTableAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a transit gateway route table attachment.
module Amazonka.NetworkManager.CreateTransitGatewayRouteTableAttachment
  ( -- * Creating a Request
    CreateTransitGatewayRouteTableAttachment (..),
    newCreateTransitGatewayRouteTableAttachment,

    -- * Request Lenses
    createTransitGatewayRouteTableAttachment_tags,
    createTransitGatewayRouteTableAttachment_clientToken,
    createTransitGatewayRouteTableAttachment_peeringId,
    createTransitGatewayRouteTableAttachment_transitGatewayRouteTableArn,

    -- * Destructuring the Response
    CreateTransitGatewayRouteTableAttachmentResponse (..),
    newCreateTransitGatewayRouteTableAttachmentResponse,

    -- * Response Lenses
    createTransitGatewayRouteTableAttachmentResponse_transitGatewayRouteTableAttachment,
    createTransitGatewayRouteTableAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTransitGatewayRouteTableAttachment' smart constructor.
data CreateTransitGatewayRouteTableAttachment = CreateTransitGatewayRouteTableAttachment'
  { -- | The list of key-value tags associated with the request.
    tags :: Prelude.Maybe [Tag],
    -- | The client token associated with the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the peer for the
    peeringId :: Prelude.Text,
    -- | The ARN of the transit gateway route table for the attachment request.
    transitGatewayRouteTableArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayRouteTableAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createTransitGatewayRouteTableAttachment_tags' - The list of key-value tags associated with the request.
--
-- 'clientToken', 'createTransitGatewayRouteTableAttachment_clientToken' - The client token associated with the request.
--
-- 'peeringId', 'createTransitGatewayRouteTableAttachment_peeringId' - The ID of the peer for the
--
-- 'transitGatewayRouteTableArn', 'createTransitGatewayRouteTableAttachment_transitGatewayRouteTableArn' - The ARN of the transit gateway route table for the attachment request.
newCreateTransitGatewayRouteTableAttachment ::
  -- | 'peeringId'
  Prelude.Text ->
  -- | 'transitGatewayRouteTableArn'
  Prelude.Text ->
  CreateTransitGatewayRouteTableAttachment
newCreateTransitGatewayRouteTableAttachment
  pPeeringId_
  pTransitGatewayRouteTableArn_ =
    CreateTransitGatewayRouteTableAttachment'
      { tags =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        peeringId = pPeeringId_,
        transitGatewayRouteTableArn =
          pTransitGatewayRouteTableArn_
      }

-- | The list of key-value tags associated with the request.
createTransitGatewayRouteTableAttachment_tags :: Lens.Lens' CreateTransitGatewayRouteTableAttachment (Prelude.Maybe [Tag])
createTransitGatewayRouteTableAttachment_tags = Lens.lens (\CreateTransitGatewayRouteTableAttachment' {tags} -> tags) (\s@CreateTransitGatewayRouteTableAttachment' {} a -> s {tags = a} :: CreateTransitGatewayRouteTableAttachment) Prelude.. Lens.mapping Lens.coerced

-- | The client token associated with the request.
createTransitGatewayRouteTableAttachment_clientToken :: Lens.Lens' CreateTransitGatewayRouteTableAttachment (Prelude.Maybe Prelude.Text)
createTransitGatewayRouteTableAttachment_clientToken = Lens.lens (\CreateTransitGatewayRouteTableAttachment' {clientToken} -> clientToken) (\s@CreateTransitGatewayRouteTableAttachment' {} a -> s {clientToken = a} :: CreateTransitGatewayRouteTableAttachment)

-- | The ID of the peer for the
createTransitGatewayRouteTableAttachment_peeringId :: Lens.Lens' CreateTransitGatewayRouteTableAttachment Prelude.Text
createTransitGatewayRouteTableAttachment_peeringId = Lens.lens (\CreateTransitGatewayRouteTableAttachment' {peeringId} -> peeringId) (\s@CreateTransitGatewayRouteTableAttachment' {} a -> s {peeringId = a} :: CreateTransitGatewayRouteTableAttachment)

-- | The ARN of the transit gateway route table for the attachment request.
createTransitGatewayRouteTableAttachment_transitGatewayRouteTableArn :: Lens.Lens' CreateTransitGatewayRouteTableAttachment Prelude.Text
createTransitGatewayRouteTableAttachment_transitGatewayRouteTableArn = Lens.lens (\CreateTransitGatewayRouteTableAttachment' {transitGatewayRouteTableArn} -> transitGatewayRouteTableArn) (\s@CreateTransitGatewayRouteTableAttachment' {} a -> s {transitGatewayRouteTableArn = a} :: CreateTransitGatewayRouteTableAttachment)

instance
  Core.AWSRequest
    CreateTransitGatewayRouteTableAttachment
  where
  type
    AWSResponse
      CreateTransitGatewayRouteTableAttachment =
      CreateTransitGatewayRouteTableAttachmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTransitGatewayRouteTableAttachmentResponse'
            Prelude.<$> (x Data..?> "TransitGatewayRouteTableAttachment")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateTransitGatewayRouteTableAttachment
  where
  hashWithSalt
    _salt
    CreateTransitGatewayRouteTableAttachment' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` peeringId
        `Prelude.hashWithSalt` transitGatewayRouteTableArn

instance
  Prelude.NFData
    CreateTransitGatewayRouteTableAttachment
  where
  rnf CreateTransitGatewayRouteTableAttachment' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf peeringId
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableArn

instance
  Data.ToHeaders
    CreateTransitGatewayRouteTableAttachment
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    CreateTransitGatewayRouteTableAttachment
  where
  toJSON CreateTransitGatewayRouteTableAttachment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("PeeringId" Data..= peeringId),
            Prelude.Just
              ( "TransitGatewayRouteTableArn"
                  Data..= transitGatewayRouteTableArn
              )
          ]
      )

instance
  Data.ToPath
    CreateTransitGatewayRouteTableAttachment
  where
  toPath =
    Prelude.const
      "/transit-gateway-route-table-attachments"

instance
  Data.ToQuery
    CreateTransitGatewayRouteTableAttachment
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTransitGatewayRouteTableAttachmentResponse' smart constructor.
data CreateTransitGatewayRouteTableAttachmentResponse = CreateTransitGatewayRouteTableAttachmentResponse'
  { -- | The route table associated with the create transit gateway route table
    -- attachment request.
    transitGatewayRouteTableAttachment :: Prelude.Maybe TransitGatewayRouteTableAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayRouteTableAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayRouteTableAttachment', 'createTransitGatewayRouteTableAttachmentResponse_transitGatewayRouteTableAttachment' - The route table associated with the create transit gateway route table
-- attachment request.
--
-- 'httpStatus', 'createTransitGatewayRouteTableAttachmentResponse_httpStatus' - The response's http status code.
newCreateTransitGatewayRouteTableAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTransitGatewayRouteTableAttachmentResponse
newCreateTransitGatewayRouteTableAttachmentResponse
  pHttpStatus_ =
    CreateTransitGatewayRouteTableAttachmentResponse'
      { transitGatewayRouteTableAttachment =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The route table associated with the create transit gateway route table
-- attachment request.
createTransitGatewayRouteTableAttachmentResponse_transitGatewayRouteTableAttachment :: Lens.Lens' CreateTransitGatewayRouteTableAttachmentResponse (Prelude.Maybe TransitGatewayRouteTableAttachment)
createTransitGatewayRouteTableAttachmentResponse_transitGatewayRouteTableAttachment = Lens.lens (\CreateTransitGatewayRouteTableAttachmentResponse' {transitGatewayRouteTableAttachment} -> transitGatewayRouteTableAttachment) (\s@CreateTransitGatewayRouteTableAttachmentResponse' {} a -> s {transitGatewayRouteTableAttachment = a} :: CreateTransitGatewayRouteTableAttachmentResponse)

-- | The response's http status code.
createTransitGatewayRouteTableAttachmentResponse_httpStatus :: Lens.Lens' CreateTransitGatewayRouteTableAttachmentResponse Prelude.Int
createTransitGatewayRouteTableAttachmentResponse_httpStatus = Lens.lens (\CreateTransitGatewayRouteTableAttachmentResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayRouteTableAttachmentResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayRouteTableAttachmentResponse)

instance
  Prelude.NFData
    CreateTransitGatewayRouteTableAttachmentResponse
  where
  rnf
    CreateTransitGatewayRouteTableAttachmentResponse' {..} =
      Prelude.rnf transitGatewayRouteTableAttachment
        `Prelude.seq` Prelude.rnf httpStatus
