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
-- Module      : Amazonka.NetworkManager.GetConnectAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a core network Connect attachment.
module Amazonka.NetworkManager.GetConnectAttachment
  ( -- * Creating a Request
    GetConnectAttachment (..),
    newGetConnectAttachment,

    -- * Request Lenses
    getConnectAttachment_attachmentId,

    -- * Destructuring the Response
    GetConnectAttachmentResponse (..),
    newGetConnectAttachmentResponse,

    -- * Response Lenses
    getConnectAttachmentResponse_connectAttachment,
    getConnectAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConnectAttachment' smart constructor.
data GetConnectAttachment = GetConnectAttachment'
  { -- | The ID of the attachment.
    attachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'getConnectAttachment_attachmentId' - The ID of the attachment.
newGetConnectAttachment ::
  -- | 'attachmentId'
  Prelude.Text ->
  GetConnectAttachment
newGetConnectAttachment pAttachmentId_ =
  GetConnectAttachment'
    { attachmentId =
        pAttachmentId_
    }

-- | The ID of the attachment.
getConnectAttachment_attachmentId :: Lens.Lens' GetConnectAttachment Prelude.Text
getConnectAttachment_attachmentId = Lens.lens (\GetConnectAttachment' {attachmentId} -> attachmentId) (\s@GetConnectAttachment' {} a -> s {attachmentId = a} :: GetConnectAttachment)

instance Core.AWSRequest GetConnectAttachment where
  type
    AWSResponse GetConnectAttachment =
      GetConnectAttachmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectAttachmentResponse'
            Prelude.<$> (x Data..?> "ConnectAttachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnectAttachment where
  hashWithSalt _salt GetConnectAttachment' {..} =
    _salt `Prelude.hashWithSalt` attachmentId

instance Prelude.NFData GetConnectAttachment where
  rnf GetConnectAttachment' {..} =
    Prelude.rnf attachmentId

instance Data.ToHeaders GetConnectAttachment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetConnectAttachment where
  toPath GetConnectAttachment' {..} =
    Prelude.mconcat
      ["/connect-attachments/", Data.toBS attachmentId]

instance Data.ToQuery GetConnectAttachment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectAttachmentResponse' smart constructor.
data GetConnectAttachmentResponse = GetConnectAttachmentResponse'
  { -- | Details about the Connect attachment.
    connectAttachment :: Prelude.Maybe ConnectAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectAttachment', 'getConnectAttachmentResponse_connectAttachment' - Details about the Connect attachment.
--
-- 'httpStatus', 'getConnectAttachmentResponse_httpStatus' - The response's http status code.
newGetConnectAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConnectAttachmentResponse
newGetConnectAttachmentResponse pHttpStatus_ =
  GetConnectAttachmentResponse'
    { connectAttachment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the Connect attachment.
getConnectAttachmentResponse_connectAttachment :: Lens.Lens' GetConnectAttachmentResponse (Prelude.Maybe ConnectAttachment)
getConnectAttachmentResponse_connectAttachment = Lens.lens (\GetConnectAttachmentResponse' {connectAttachment} -> connectAttachment) (\s@GetConnectAttachmentResponse' {} a -> s {connectAttachment = a} :: GetConnectAttachmentResponse)

-- | The response's http status code.
getConnectAttachmentResponse_httpStatus :: Lens.Lens' GetConnectAttachmentResponse Prelude.Int
getConnectAttachmentResponse_httpStatus = Lens.lens (\GetConnectAttachmentResponse' {httpStatus} -> httpStatus) (\s@GetConnectAttachmentResponse' {} a -> s {httpStatus = a} :: GetConnectAttachmentResponse)

instance Prelude.NFData GetConnectAttachmentResponse where
  rnf GetConnectAttachmentResponse' {..} =
    Prelude.rnf connectAttachment
      `Prelude.seq` Prelude.rnf httpStatus
