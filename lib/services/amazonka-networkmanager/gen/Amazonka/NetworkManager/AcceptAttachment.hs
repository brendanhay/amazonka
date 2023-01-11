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
-- Module      : Amazonka.NetworkManager.AcceptAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a core network attachment request.
--
-- Once the attachment request is accepted by a core network owner, the
-- attachment is created and connected to a core network.
module Amazonka.NetworkManager.AcceptAttachment
  ( -- * Creating a Request
    AcceptAttachment (..),
    newAcceptAttachment,

    -- * Request Lenses
    acceptAttachment_attachmentId,

    -- * Destructuring the Response
    AcceptAttachmentResponse (..),
    newAcceptAttachmentResponse,

    -- * Response Lenses
    acceptAttachmentResponse_attachment,
    acceptAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptAttachment' smart constructor.
data AcceptAttachment = AcceptAttachment'
  { -- | The ID of the attachment.
    attachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'acceptAttachment_attachmentId' - The ID of the attachment.
newAcceptAttachment ::
  -- | 'attachmentId'
  Prelude.Text ->
  AcceptAttachment
newAcceptAttachment pAttachmentId_ =
  AcceptAttachment' {attachmentId = pAttachmentId_}

-- | The ID of the attachment.
acceptAttachment_attachmentId :: Lens.Lens' AcceptAttachment Prelude.Text
acceptAttachment_attachmentId = Lens.lens (\AcceptAttachment' {attachmentId} -> attachmentId) (\s@AcceptAttachment' {} a -> s {attachmentId = a} :: AcceptAttachment)

instance Core.AWSRequest AcceptAttachment where
  type
    AWSResponse AcceptAttachment =
      AcceptAttachmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptAttachmentResponse'
            Prelude.<$> (x Data..?> "Attachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptAttachment where
  hashWithSalt _salt AcceptAttachment' {..} =
    _salt `Prelude.hashWithSalt` attachmentId

instance Prelude.NFData AcceptAttachment where
  rnf AcceptAttachment' {..} = Prelude.rnf attachmentId

instance Data.ToHeaders AcceptAttachment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AcceptAttachment where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath AcceptAttachment where
  toPath AcceptAttachment' {..} =
    Prelude.mconcat
      ["/attachments/", Data.toBS attachmentId, "/accept"]

instance Data.ToQuery AcceptAttachment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptAttachmentResponse' smart constructor.
data AcceptAttachmentResponse = AcceptAttachmentResponse'
  { -- | The response to the attachment request.
    attachment :: Prelude.Maybe Attachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachment', 'acceptAttachmentResponse_attachment' - The response to the attachment request.
--
-- 'httpStatus', 'acceptAttachmentResponse_httpStatus' - The response's http status code.
newAcceptAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptAttachmentResponse
newAcceptAttachmentResponse pHttpStatus_ =
  AcceptAttachmentResponse'
    { attachment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The response to the attachment request.
acceptAttachmentResponse_attachment :: Lens.Lens' AcceptAttachmentResponse (Prelude.Maybe Attachment)
acceptAttachmentResponse_attachment = Lens.lens (\AcceptAttachmentResponse' {attachment} -> attachment) (\s@AcceptAttachmentResponse' {} a -> s {attachment = a} :: AcceptAttachmentResponse)

-- | The response's http status code.
acceptAttachmentResponse_httpStatus :: Lens.Lens' AcceptAttachmentResponse Prelude.Int
acceptAttachmentResponse_httpStatus = Lens.lens (\AcceptAttachmentResponse' {httpStatus} -> httpStatus) (\s@AcceptAttachmentResponse' {} a -> s {httpStatus = a} :: AcceptAttachmentResponse)

instance Prelude.NFData AcceptAttachmentResponse where
  rnf AcceptAttachmentResponse' {..} =
    Prelude.rnf attachment
      `Prelude.seq` Prelude.rnf httpStatus
