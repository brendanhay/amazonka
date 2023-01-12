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
-- Module      : Amazonka.ConnectParticipant.GetAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a pre-signed URL for download of a completed attachment. This
-- is an asynchronous API for use with active contacts.
--
-- @ConnectionToken@ is used for invoking this API instead of
-- @ParticipantToken@.
--
-- The Amazon Connect Participant Service APIs do not use
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 authentication>.
module Amazonka.ConnectParticipant.GetAttachment
  ( -- * Creating a Request
    GetAttachment (..),
    newGetAttachment,

    -- * Request Lenses
    getAttachment_attachmentId,
    getAttachment_connectionToken,

    -- * Destructuring the Response
    GetAttachmentResponse (..),
    newGetAttachmentResponse,

    -- * Response Lenses
    getAttachmentResponse_url,
    getAttachmentResponse_urlExpiry,
    getAttachmentResponse_httpStatus,
  )
where

import Amazonka.ConnectParticipant.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAttachment' smart constructor.
data GetAttachment = GetAttachment'
  { -- | A unique identifier for the attachment.
    attachmentId :: Prelude.Text,
    -- | The authentication token associated with the participant\'s connection.
    connectionToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'getAttachment_attachmentId' - A unique identifier for the attachment.
--
-- 'connectionToken', 'getAttachment_connectionToken' - The authentication token associated with the participant\'s connection.
newGetAttachment ::
  -- | 'attachmentId'
  Prelude.Text ->
  -- | 'connectionToken'
  Prelude.Text ->
  GetAttachment
newGetAttachment pAttachmentId_ pConnectionToken_ =
  GetAttachment'
    { attachmentId = pAttachmentId_,
      connectionToken = pConnectionToken_
    }

-- | A unique identifier for the attachment.
getAttachment_attachmentId :: Lens.Lens' GetAttachment Prelude.Text
getAttachment_attachmentId = Lens.lens (\GetAttachment' {attachmentId} -> attachmentId) (\s@GetAttachment' {} a -> s {attachmentId = a} :: GetAttachment)

-- | The authentication token associated with the participant\'s connection.
getAttachment_connectionToken :: Lens.Lens' GetAttachment Prelude.Text
getAttachment_connectionToken = Lens.lens (\GetAttachment' {connectionToken} -> connectionToken) (\s@GetAttachment' {} a -> s {connectionToken = a} :: GetAttachment)

instance Core.AWSRequest GetAttachment where
  type
    AWSResponse GetAttachment =
      GetAttachmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAttachmentResponse'
            Prelude.<$> (x Data..?> "Url")
            Prelude.<*> (x Data..?> "UrlExpiry")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAttachment where
  hashWithSalt _salt GetAttachment' {..} =
    _salt `Prelude.hashWithSalt` attachmentId
      `Prelude.hashWithSalt` connectionToken

instance Prelude.NFData GetAttachment where
  rnf GetAttachment' {..} =
    Prelude.rnf attachmentId
      `Prelude.seq` Prelude.rnf connectionToken

instance Data.ToHeaders GetAttachment where
  toHeaders GetAttachment' {..} =
    Prelude.mconcat
      [ "X-Amz-Bearer" Data.=# connectionToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON GetAttachment where
  toJSON GetAttachment' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AttachmentId" Data..= attachmentId)]
      )

instance Data.ToPath GetAttachment where
  toPath = Prelude.const "/participant/attachment"

instance Data.ToQuery GetAttachment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAttachmentResponse' smart constructor.
data GetAttachmentResponse = GetAttachmentResponse'
  { -- | This is the pre-signed URL that can be used for uploading the file to
    -- Amazon S3 when used in response to
    -- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_StartAttachmentUpload.html StartAttachmentUpload>.
    url :: Prelude.Maybe Prelude.Text,
    -- | The expiration time of the URL in ISO timestamp. It\'s specified in ISO
    -- 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For example,
    -- 2019-11-08T02:41:28.172Z.
    urlExpiry :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'getAttachmentResponse_url' - This is the pre-signed URL that can be used for uploading the file to
-- Amazon S3 when used in response to
-- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_StartAttachmentUpload.html StartAttachmentUpload>.
--
-- 'urlExpiry', 'getAttachmentResponse_urlExpiry' - The expiration time of the URL in ISO timestamp. It\'s specified in ISO
-- 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For example,
-- 2019-11-08T02:41:28.172Z.
--
-- 'httpStatus', 'getAttachmentResponse_httpStatus' - The response's http status code.
newGetAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAttachmentResponse
newGetAttachmentResponse pHttpStatus_ =
  GetAttachmentResponse'
    { url = Prelude.Nothing,
      urlExpiry = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This is the pre-signed URL that can be used for uploading the file to
-- Amazon S3 when used in response to
-- <https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_StartAttachmentUpload.html StartAttachmentUpload>.
getAttachmentResponse_url :: Lens.Lens' GetAttachmentResponse (Prelude.Maybe Prelude.Text)
getAttachmentResponse_url = Lens.lens (\GetAttachmentResponse' {url} -> url) (\s@GetAttachmentResponse' {} a -> s {url = a} :: GetAttachmentResponse)

-- | The expiration time of the URL in ISO timestamp. It\'s specified in ISO
-- 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For example,
-- 2019-11-08T02:41:28.172Z.
getAttachmentResponse_urlExpiry :: Lens.Lens' GetAttachmentResponse (Prelude.Maybe Prelude.Text)
getAttachmentResponse_urlExpiry = Lens.lens (\GetAttachmentResponse' {urlExpiry} -> urlExpiry) (\s@GetAttachmentResponse' {} a -> s {urlExpiry = a} :: GetAttachmentResponse)

-- | The response's http status code.
getAttachmentResponse_httpStatus :: Lens.Lens' GetAttachmentResponse Prelude.Int
getAttachmentResponse_httpStatus = Lens.lens (\GetAttachmentResponse' {httpStatus} -> httpStatus) (\s@GetAttachmentResponse' {} a -> s {httpStatus = a} :: GetAttachmentResponse)

instance Prelude.NFData GetAttachmentResponse where
  rnf GetAttachmentResponse' {..} =
    Prelude.rnf url
      `Prelude.seq` Prelude.rnf urlExpiry
      `Prelude.seq` Prelude.rnf httpStatus
