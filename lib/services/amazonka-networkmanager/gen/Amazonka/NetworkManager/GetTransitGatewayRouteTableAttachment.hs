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
-- Module      : Amazonka.NetworkManager.GetTransitGatewayRouteTableAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a transit gateway route table attachment.
module Amazonka.NetworkManager.GetTransitGatewayRouteTableAttachment
  ( -- * Creating a Request
    GetTransitGatewayRouteTableAttachment (..),
    newGetTransitGatewayRouteTableAttachment,

    -- * Request Lenses
    getTransitGatewayRouteTableAttachment_attachmentId,

    -- * Destructuring the Response
    GetTransitGatewayRouteTableAttachmentResponse (..),
    newGetTransitGatewayRouteTableAttachmentResponse,

    -- * Response Lenses
    getTransitGatewayRouteTableAttachmentResponse_transitGatewayRouteTableAttachment,
    getTransitGatewayRouteTableAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTransitGatewayRouteTableAttachment' smart constructor.
data GetTransitGatewayRouteTableAttachment = GetTransitGatewayRouteTableAttachment'
  { -- | The ID of the transit gateway route table attachment.
    attachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayRouteTableAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'getTransitGatewayRouteTableAttachment_attachmentId' - The ID of the transit gateway route table attachment.
newGetTransitGatewayRouteTableAttachment ::
  -- | 'attachmentId'
  Prelude.Text ->
  GetTransitGatewayRouteTableAttachment
newGetTransitGatewayRouteTableAttachment
  pAttachmentId_ =
    GetTransitGatewayRouteTableAttachment'
      { attachmentId =
          pAttachmentId_
      }

-- | The ID of the transit gateway route table attachment.
getTransitGatewayRouteTableAttachment_attachmentId :: Lens.Lens' GetTransitGatewayRouteTableAttachment Prelude.Text
getTransitGatewayRouteTableAttachment_attachmentId = Lens.lens (\GetTransitGatewayRouteTableAttachment' {attachmentId} -> attachmentId) (\s@GetTransitGatewayRouteTableAttachment' {} a -> s {attachmentId = a} :: GetTransitGatewayRouteTableAttachment)

instance
  Core.AWSRequest
    GetTransitGatewayRouteTableAttachment
  where
  type
    AWSResponse
      GetTransitGatewayRouteTableAttachment =
      GetTransitGatewayRouteTableAttachmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTransitGatewayRouteTableAttachmentResponse'
            Prelude.<$> (x Data..?> "TransitGatewayRouteTableAttachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetTransitGatewayRouteTableAttachment
  where
  hashWithSalt
    _salt
    GetTransitGatewayRouteTableAttachment' {..} =
      _salt `Prelude.hashWithSalt` attachmentId

instance
  Prelude.NFData
    GetTransitGatewayRouteTableAttachment
  where
  rnf GetTransitGatewayRouteTableAttachment' {..} =
    Prelude.rnf attachmentId

instance
  Data.ToHeaders
    GetTransitGatewayRouteTableAttachment
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
  Data.ToPath
    GetTransitGatewayRouteTableAttachment
  where
  toPath GetTransitGatewayRouteTableAttachment' {..} =
    Prelude.mconcat
      [ "/transit-gateway-route-table-attachments/",
        Data.toBS attachmentId
      ]

instance
  Data.ToQuery
    GetTransitGatewayRouteTableAttachment
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTransitGatewayRouteTableAttachmentResponse' smart constructor.
data GetTransitGatewayRouteTableAttachmentResponse = GetTransitGatewayRouteTableAttachmentResponse'
  { -- | Returns information about the transit gateway route table attachment.
    transitGatewayRouteTableAttachment :: Prelude.Maybe TransitGatewayRouteTableAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayRouteTableAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayRouteTableAttachment', 'getTransitGatewayRouteTableAttachmentResponse_transitGatewayRouteTableAttachment' - Returns information about the transit gateway route table attachment.
--
-- 'httpStatus', 'getTransitGatewayRouteTableAttachmentResponse_httpStatus' - The response's http status code.
newGetTransitGatewayRouteTableAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTransitGatewayRouteTableAttachmentResponse
newGetTransitGatewayRouteTableAttachmentResponse
  pHttpStatus_ =
    GetTransitGatewayRouteTableAttachmentResponse'
      { transitGatewayRouteTableAttachment =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns information about the transit gateway route table attachment.
getTransitGatewayRouteTableAttachmentResponse_transitGatewayRouteTableAttachment :: Lens.Lens' GetTransitGatewayRouteTableAttachmentResponse (Prelude.Maybe TransitGatewayRouteTableAttachment)
getTransitGatewayRouteTableAttachmentResponse_transitGatewayRouteTableAttachment = Lens.lens (\GetTransitGatewayRouteTableAttachmentResponse' {transitGatewayRouteTableAttachment} -> transitGatewayRouteTableAttachment) (\s@GetTransitGatewayRouteTableAttachmentResponse' {} a -> s {transitGatewayRouteTableAttachment = a} :: GetTransitGatewayRouteTableAttachmentResponse)

-- | The response's http status code.
getTransitGatewayRouteTableAttachmentResponse_httpStatus :: Lens.Lens' GetTransitGatewayRouteTableAttachmentResponse Prelude.Int
getTransitGatewayRouteTableAttachmentResponse_httpStatus = Lens.lens (\GetTransitGatewayRouteTableAttachmentResponse' {httpStatus} -> httpStatus) (\s@GetTransitGatewayRouteTableAttachmentResponse' {} a -> s {httpStatus = a} :: GetTransitGatewayRouteTableAttachmentResponse)

instance
  Prelude.NFData
    GetTransitGatewayRouteTableAttachmentResponse
  where
  rnf
    GetTransitGatewayRouteTableAttachmentResponse' {..} =
      Prelude.rnf transitGatewayRouteTableAttachment
        `Prelude.seq` Prelude.rnf httpStatus
