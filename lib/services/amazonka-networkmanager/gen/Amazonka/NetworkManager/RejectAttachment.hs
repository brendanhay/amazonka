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
-- Module      : Amazonka.NetworkManager.RejectAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a core network attachment request.
module Amazonka.NetworkManager.RejectAttachment
  ( -- * Creating a Request
    RejectAttachment (..),
    newRejectAttachment,

    -- * Request Lenses
    rejectAttachment_attachmentId,

    -- * Destructuring the Response
    RejectAttachmentResponse (..),
    newRejectAttachmentResponse,

    -- * Response Lenses
    rejectAttachmentResponse_attachment,
    rejectAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRejectAttachment' smart constructor.
data RejectAttachment = RejectAttachment'
  { -- | The ID of the attachment.
    attachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'rejectAttachment_attachmentId' - The ID of the attachment.
newRejectAttachment ::
  -- | 'attachmentId'
  Prelude.Text ->
  RejectAttachment
newRejectAttachment pAttachmentId_ =
  RejectAttachment' {attachmentId = pAttachmentId_}

-- | The ID of the attachment.
rejectAttachment_attachmentId :: Lens.Lens' RejectAttachment Prelude.Text
rejectAttachment_attachmentId = Lens.lens (\RejectAttachment' {attachmentId} -> attachmentId) (\s@RejectAttachment' {} a -> s {attachmentId = a} :: RejectAttachment)

instance Core.AWSRequest RejectAttachment where
  type
    AWSResponse RejectAttachment =
      RejectAttachmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RejectAttachmentResponse'
            Prelude.<$> (x Core..?> "Attachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RejectAttachment where
  hashWithSalt _salt RejectAttachment' {..} =
    _salt `Prelude.hashWithSalt` attachmentId

instance Prelude.NFData RejectAttachment where
  rnf RejectAttachment' {..} = Prelude.rnf attachmentId

instance Core.ToHeaders RejectAttachment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RejectAttachment where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath RejectAttachment where
  toPath RejectAttachment' {..} =
    Prelude.mconcat
      ["/attachments/", Core.toBS attachmentId, "/reject"]

instance Core.ToQuery RejectAttachment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectAttachmentResponse' smart constructor.
data RejectAttachmentResponse = RejectAttachmentResponse'
  { -- | Describes the rejected attachment request.
    attachment :: Prelude.Maybe Attachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachment', 'rejectAttachmentResponse_attachment' - Describes the rejected attachment request.
--
-- 'httpStatus', 'rejectAttachmentResponse_httpStatus' - The response's http status code.
newRejectAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectAttachmentResponse
newRejectAttachmentResponse pHttpStatus_ =
  RejectAttachmentResponse'
    { attachment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the rejected attachment request.
rejectAttachmentResponse_attachment :: Lens.Lens' RejectAttachmentResponse (Prelude.Maybe Attachment)
rejectAttachmentResponse_attachment = Lens.lens (\RejectAttachmentResponse' {attachment} -> attachment) (\s@RejectAttachmentResponse' {} a -> s {attachment = a} :: RejectAttachmentResponse)

-- | The response's http status code.
rejectAttachmentResponse_httpStatus :: Lens.Lens' RejectAttachmentResponse Prelude.Int
rejectAttachmentResponse_httpStatus = Lens.lens (\RejectAttachmentResponse' {httpStatus} -> httpStatus) (\s@RejectAttachmentResponse' {} a -> s {httpStatus = a} :: RejectAttachmentResponse)

instance Prelude.NFData RejectAttachmentResponse where
  rnf RejectAttachmentResponse' {..} =
    Prelude.rnf attachment
      `Prelude.seq` Prelude.rnf httpStatus
