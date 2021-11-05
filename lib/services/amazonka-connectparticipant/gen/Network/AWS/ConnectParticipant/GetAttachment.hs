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
-- Module      : Network.AWS.ConnectParticipant.GetAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a pre-signed URL for download of a completed attachment. This
-- is an asynchronous API for use with active contacts.
module Network.AWS.ConnectParticipant.GetAttachment
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
    getAttachmentResponse_urlExpiry,
    getAttachmentResponse_url,
    getAttachmentResponse_httpStatus,
  )
where

import Network.AWS.ConnectParticipant.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAttachmentResponse'
            Prelude.<$> (x Core..?> "UrlExpiry")
            Prelude.<*> (x Core..?> "Url")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAttachment

instance Prelude.NFData GetAttachment

instance Core.ToHeaders GetAttachment where
  toHeaders GetAttachment' {..} =
    Prelude.mconcat
      [ "X-Amz-Bearer" Core.=# connectionToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON GetAttachment where
  toJSON GetAttachment' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("AttachmentId" Core..= attachmentId)]
      )

instance Core.ToPath GetAttachment where
  toPath = Prelude.const "/participant/attachment"

instance Core.ToQuery GetAttachment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAttachmentResponse' smart constructor.
data GetAttachmentResponse = GetAttachmentResponse'
  { -- | The expiration time of the URL in ISO timestamp. It\'s specified in ISO
    -- 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For example,
    -- 2019-11-08T02:41:28.172Z.
    urlExpiry :: Prelude.Maybe Prelude.Text,
    -- | The pre-signed URL using which file would be downloaded from Amazon S3
    -- by the API caller.
    url :: Prelude.Maybe Prelude.Text,
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
-- 'urlExpiry', 'getAttachmentResponse_urlExpiry' - The expiration time of the URL in ISO timestamp. It\'s specified in ISO
-- 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For example,
-- 2019-11-08T02:41:28.172Z.
--
-- 'url', 'getAttachmentResponse_url' - The pre-signed URL using which file would be downloaded from Amazon S3
-- by the API caller.
--
-- 'httpStatus', 'getAttachmentResponse_httpStatus' - The response's http status code.
newGetAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAttachmentResponse
newGetAttachmentResponse pHttpStatus_ =
  GetAttachmentResponse'
    { urlExpiry = Prelude.Nothing,
      url = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The expiration time of the URL in ISO timestamp. It\'s specified in ISO
-- 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For example,
-- 2019-11-08T02:41:28.172Z.
getAttachmentResponse_urlExpiry :: Lens.Lens' GetAttachmentResponse (Prelude.Maybe Prelude.Text)
getAttachmentResponse_urlExpiry = Lens.lens (\GetAttachmentResponse' {urlExpiry} -> urlExpiry) (\s@GetAttachmentResponse' {} a -> s {urlExpiry = a} :: GetAttachmentResponse)

-- | The pre-signed URL using which file would be downloaded from Amazon S3
-- by the API caller.
getAttachmentResponse_url :: Lens.Lens' GetAttachmentResponse (Prelude.Maybe Prelude.Text)
getAttachmentResponse_url = Lens.lens (\GetAttachmentResponse' {url} -> url) (\s@GetAttachmentResponse' {} a -> s {url = a} :: GetAttachmentResponse)

-- | The response's http status code.
getAttachmentResponse_httpStatus :: Lens.Lens' GetAttachmentResponse Prelude.Int
getAttachmentResponse_httpStatus = Lens.lens (\GetAttachmentResponse' {httpStatus} -> httpStatus) (\s@GetAttachmentResponse' {} a -> s {httpStatus = a} :: GetAttachmentResponse)

instance Prelude.NFData GetAttachmentResponse
