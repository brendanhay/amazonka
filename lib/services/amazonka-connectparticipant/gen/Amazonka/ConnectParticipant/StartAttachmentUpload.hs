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
-- Module      : Amazonka.ConnectParticipant.StartAttachmentUpload
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a pre-signed Amazon S3 URL in response for uploading the file
-- directly to S3.
--
-- The Amazon Connect Participant Service APIs do not use
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 authentication>.
module Amazonka.ConnectParticipant.StartAttachmentUpload
  ( -- * Creating a Request
    StartAttachmentUpload (..),
    newStartAttachmentUpload,

    -- * Request Lenses
    startAttachmentUpload_contentType,
    startAttachmentUpload_attachmentSizeInBytes,
    startAttachmentUpload_attachmentName,
    startAttachmentUpload_clientToken,
    startAttachmentUpload_connectionToken,

    -- * Destructuring the Response
    StartAttachmentUploadResponse (..),
    newStartAttachmentUploadResponse,

    -- * Response Lenses
    startAttachmentUploadResponse_attachmentId,
    startAttachmentUploadResponse_uploadMetadata,
    startAttachmentUploadResponse_httpStatus,
  )
where

import Amazonka.ConnectParticipant.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartAttachmentUpload' smart constructor.
data StartAttachmentUpload = StartAttachmentUpload'
  { -- | Describes the MIME file type of the attachment. For a list of supported
    -- file types, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html#feature-limits Feature specifications>
    -- in the /Amazon Connect Administrator Guide/.
    contentType :: Prelude.Text,
    -- | The size of the attachment in bytes.
    attachmentSizeInBytes :: Prelude.Natural,
    -- | A case-sensitive name of the attachment being uploaded.
    attachmentName :: Prelude.Text,
    -- | A unique case sensitive identifier to support idempotency of request.
    clientToken :: Prelude.Text,
    -- | The authentication token associated with the participant\'s connection.
    connectionToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAttachmentUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'startAttachmentUpload_contentType' - Describes the MIME file type of the attachment. For a list of supported
-- file types, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html#feature-limits Feature specifications>
-- in the /Amazon Connect Administrator Guide/.
--
-- 'attachmentSizeInBytes', 'startAttachmentUpload_attachmentSizeInBytes' - The size of the attachment in bytes.
--
-- 'attachmentName', 'startAttachmentUpload_attachmentName' - A case-sensitive name of the attachment being uploaded.
--
-- 'clientToken', 'startAttachmentUpload_clientToken' - A unique case sensitive identifier to support idempotency of request.
--
-- 'connectionToken', 'startAttachmentUpload_connectionToken' - The authentication token associated with the participant\'s connection.
newStartAttachmentUpload ::
  -- | 'contentType'
  Prelude.Text ->
  -- | 'attachmentSizeInBytes'
  Prelude.Natural ->
  -- | 'attachmentName'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'connectionToken'
  Prelude.Text ->
  StartAttachmentUpload
newStartAttachmentUpload
  pContentType_
  pAttachmentSizeInBytes_
  pAttachmentName_
  pClientToken_
  pConnectionToken_ =
    StartAttachmentUpload'
      { contentType = pContentType_,
        attachmentSizeInBytes = pAttachmentSizeInBytes_,
        attachmentName = pAttachmentName_,
        clientToken = pClientToken_,
        connectionToken = pConnectionToken_
      }

-- | Describes the MIME file type of the attachment. For a list of supported
-- file types, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html#feature-limits Feature specifications>
-- in the /Amazon Connect Administrator Guide/.
startAttachmentUpload_contentType :: Lens.Lens' StartAttachmentUpload Prelude.Text
startAttachmentUpload_contentType = Lens.lens (\StartAttachmentUpload' {contentType} -> contentType) (\s@StartAttachmentUpload' {} a -> s {contentType = a} :: StartAttachmentUpload)

-- | The size of the attachment in bytes.
startAttachmentUpload_attachmentSizeInBytes :: Lens.Lens' StartAttachmentUpload Prelude.Natural
startAttachmentUpload_attachmentSizeInBytes = Lens.lens (\StartAttachmentUpload' {attachmentSizeInBytes} -> attachmentSizeInBytes) (\s@StartAttachmentUpload' {} a -> s {attachmentSizeInBytes = a} :: StartAttachmentUpload)

-- | A case-sensitive name of the attachment being uploaded.
startAttachmentUpload_attachmentName :: Lens.Lens' StartAttachmentUpload Prelude.Text
startAttachmentUpload_attachmentName = Lens.lens (\StartAttachmentUpload' {attachmentName} -> attachmentName) (\s@StartAttachmentUpload' {} a -> s {attachmentName = a} :: StartAttachmentUpload)

-- | A unique case sensitive identifier to support idempotency of request.
startAttachmentUpload_clientToken :: Lens.Lens' StartAttachmentUpload Prelude.Text
startAttachmentUpload_clientToken = Lens.lens (\StartAttachmentUpload' {clientToken} -> clientToken) (\s@StartAttachmentUpload' {} a -> s {clientToken = a} :: StartAttachmentUpload)

-- | The authentication token associated with the participant\'s connection.
startAttachmentUpload_connectionToken :: Lens.Lens' StartAttachmentUpload Prelude.Text
startAttachmentUpload_connectionToken = Lens.lens (\StartAttachmentUpload' {connectionToken} -> connectionToken) (\s@StartAttachmentUpload' {} a -> s {connectionToken = a} :: StartAttachmentUpload)

instance Core.AWSRequest StartAttachmentUpload where
  type
    AWSResponse StartAttachmentUpload =
      StartAttachmentUploadResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAttachmentUploadResponse'
            Prelude.<$> (x Data..?> "AttachmentId")
            Prelude.<*> (x Data..?> "UploadMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartAttachmentUpload where
  hashWithSalt _salt StartAttachmentUpload' {..} =
    _salt `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` attachmentSizeInBytes
      `Prelude.hashWithSalt` attachmentName
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` connectionToken

instance Prelude.NFData StartAttachmentUpload where
  rnf StartAttachmentUpload' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf attachmentSizeInBytes
      `Prelude.seq` Prelude.rnf attachmentName
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf connectionToken

instance Data.ToHeaders StartAttachmentUpload where
  toHeaders StartAttachmentUpload' {..} =
    Prelude.mconcat
      [ "X-Amz-Bearer" Data.=# connectionToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON StartAttachmentUpload where
  toJSON StartAttachmentUpload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ContentType" Data..= contentType),
            Prelude.Just
              ( "AttachmentSizeInBytes"
                  Data..= attachmentSizeInBytes
              ),
            Prelude.Just
              ("AttachmentName" Data..= attachmentName),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath StartAttachmentUpload where
  toPath =
    Prelude.const
      "/participant/start-attachment-upload"

instance Data.ToQuery StartAttachmentUpload where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAttachmentUploadResponse' smart constructor.
data StartAttachmentUploadResponse = StartAttachmentUploadResponse'
  { -- | A unique identifier for the attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | Fields to be used while uploading the attachment.
    uploadMetadata :: Prelude.Maybe UploadMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAttachmentUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'startAttachmentUploadResponse_attachmentId' - A unique identifier for the attachment.
--
-- 'uploadMetadata', 'startAttachmentUploadResponse_uploadMetadata' - Fields to be used while uploading the attachment.
--
-- 'httpStatus', 'startAttachmentUploadResponse_httpStatus' - The response's http status code.
newStartAttachmentUploadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartAttachmentUploadResponse
newStartAttachmentUploadResponse pHttpStatus_ =
  StartAttachmentUploadResponse'
    { attachmentId =
        Prelude.Nothing,
      uploadMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the attachment.
startAttachmentUploadResponse_attachmentId :: Lens.Lens' StartAttachmentUploadResponse (Prelude.Maybe Prelude.Text)
startAttachmentUploadResponse_attachmentId = Lens.lens (\StartAttachmentUploadResponse' {attachmentId} -> attachmentId) (\s@StartAttachmentUploadResponse' {} a -> s {attachmentId = a} :: StartAttachmentUploadResponse)

-- | Fields to be used while uploading the attachment.
startAttachmentUploadResponse_uploadMetadata :: Lens.Lens' StartAttachmentUploadResponse (Prelude.Maybe UploadMetadata)
startAttachmentUploadResponse_uploadMetadata = Lens.lens (\StartAttachmentUploadResponse' {uploadMetadata} -> uploadMetadata) (\s@StartAttachmentUploadResponse' {} a -> s {uploadMetadata = a} :: StartAttachmentUploadResponse)

-- | The response's http status code.
startAttachmentUploadResponse_httpStatus :: Lens.Lens' StartAttachmentUploadResponse Prelude.Int
startAttachmentUploadResponse_httpStatus = Lens.lens (\StartAttachmentUploadResponse' {httpStatus} -> httpStatus) (\s@StartAttachmentUploadResponse' {} a -> s {httpStatus = a} :: StartAttachmentUploadResponse)

instance Prelude.NFData StartAttachmentUploadResponse where
  rnf StartAttachmentUploadResponse' {..} =
    Prelude.rnf attachmentId
      `Prelude.seq` Prelude.rnf uploadMetadata
      `Prelude.seq` Prelude.rnf httpStatus
