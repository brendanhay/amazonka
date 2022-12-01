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
-- Module      : Amazonka.NetworkManager.DeleteAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an attachment. Supports all attachment types.
module Amazonka.NetworkManager.DeleteAttachment
  ( -- * Creating a Request
    DeleteAttachment (..),
    newDeleteAttachment,

    -- * Request Lenses
    deleteAttachment_attachmentId,

    -- * Destructuring the Response
    DeleteAttachmentResponse (..),
    newDeleteAttachmentResponse,

    -- * Response Lenses
    deleteAttachmentResponse_attachment,
    deleteAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAttachment' smart constructor.
data DeleteAttachment = DeleteAttachment'
  { -- | The ID of the attachment to delete.
    attachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'deleteAttachment_attachmentId' - The ID of the attachment to delete.
newDeleteAttachment ::
  -- | 'attachmentId'
  Prelude.Text ->
  DeleteAttachment
newDeleteAttachment pAttachmentId_ =
  DeleteAttachment' {attachmentId = pAttachmentId_}

-- | The ID of the attachment to delete.
deleteAttachment_attachmentId :: Lens.Lens' DeleteAttachment Prelude.Text
deleteAttachment_attachmentId = Lens.lens (\DeleteAttachment' {attachmentId} -> attachmentId) (\s@DeleteAttachment' {} a -> s {attachmentId = a} :: DeleteAttachment)

instance Core.AWSRequest DeleteAttachment where
  type
    AWSResponse DeleteAttachment =
      DeleteAttachmentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAttachmentResponse'
            Prelude.<$> (x Core..?> "Attachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAttachment where
  hashWithSalt _salt DeleteAttachment' {..} =
    _salt `Prelude.hashWithSalt` attachmentId

instance Prelude.NFData DeleteAttachment where
  rnf DeleteAttachment' {..} = Prelude.rnf attachmentId

instance Core.ToHeaders DeleteAttachment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteAttachment where
  toPath DeleteAttachment' {..} =
    Prelude.mconcat
      ["/attachments/", Core.toBS attachmentId]

instance Core.ToQuery DeleteAttachment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAttachmentResponse' smart constructor.
data DeleteAttachmentResponse = DeleteAttachmentResponse'
  { -- | Information about the deleted attachment.
    attachment :: Prelude.Maybe Attachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachment', 'deleteAttachmentResponse_attachment' - Information about the deleted attachment.
--
-- 'httpStatus', 'deleteAttachmentResponse_httpStatus' - The response's http status code.
newDeleteAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAttachmentResponse
newDeleteAttachmentResponse pHttpStatus_ =
  DeleteAttachmentResponse'
    { attachment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deleted attachment.
deleteAttachmentResponse_attachment :: Lens.Lens' DeleteAttachmentResponse (Prelude.Maybe Attachment)
deleteAttachmentResponse_attachment = Lens.lens (\DeleteAttachmentResponse' {attachment} -> attachment) (\s@DeleteAttachmentResponse' {} a -> s {attachment = a} :: DeleteAttachmentResponse)

-- | The response's http status code.
deleteAttachmentResponse_httpStatus :: Lens.Lens' DeleteAttachmentResponse Prelude.Int
deleteAttachmentResponse_httpStatus = Lens.lens (\DeleteAttachmentResponse' {httpStatus} -> httpStatus) (\s@DeleteAttachmentResponse' {} a -> s {httpStatus = a} :: DeleteAttachmentResponse)

instance Prelude.NFData DeleteAttachmentResponse where
  rnf DeleteAttachmentResponse' {..} =
    Prelude.rnf attachment
      `Prelude.seq` Prelude.rnf httpStatus
