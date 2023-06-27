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
-- Module      : Amazonka.Wisdom.StartContentUpload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a URL to upload content to a knowledge base. To upload content,
-- first make a PUT request to the returned URL with your file, making sure
-- to include the required headers. Then use
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_CreateContent.html CreateContent>
-- to finalize the content creation process or
-- <https://docs.aws.amazon.com/wisdom/latest/APIReference/API_UpdateContent.html UpdateContent>
-- to modify an existing resource. You can only upload content to a
-- knowledge base of type CUSTOM.
module Amazonka.Wisdom.StartContentUpload
  ( -- * Creating a Request
    StartContentUpload (..),
    newStartContentUpload,

    -- * Request Lenses
    startContentUpload_contentType,
    startContentUpload_knowledgeBaseId,

    -- * Destructuring the Response
    StartContentUploadResponse (..),
    newStartContentUploadResponse,

    -- * Response Lenses
    startContentUploadResponse_httpStatus,
    startContentUploadResponse_headersToInclude,
    startContentUploadResponse_uploadId,
    startContentUploadResponse_url,
    startContentUploadResponse_urlExpiry,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newStartContentUpload' smart constructor.
data StartContentUpload = StartContentUpload'
  { -- | The type of content to upload.
    contentType :: Prelude.Text,
    -- | The identifier of the knowledge base. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    knowledgeBaseId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartContentUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'startContentUpload_contentType' - The type of content to upload.
--
-- 'knowledgeBaseId', 'startContentUpload_knowledgeBaseId' - The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
newStartContentUpload ::
  -- | 'contentType'
  Prelude.Text ->
  -- | 'knowledgeBaseId'
  Prelude.Text ->
  StartContentUpload
newStartContentUpload pContentType_ pKnowledgeBaseId_ =
  StartContentUpload'
    { contentType = pContentType_,
      knowledgeBaseId = pKnowledgeBaseId_
    }

-- | The type of content to upload.
startContentUpload_contentType :: Lens.Lens' StartContentUpload Prelude.Text
startContentUpload_contentType = Lens.lens (\StartContentUpload' {contentType} -> contentType) (\s@StartContentUpload' {} a -> s {contentType = a} :: StartContentUpload)

-- | The identifier of the knowledge base. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
startContentUpload_knowledgeBaseId :: Lens.Lens' StartContentUpload Prelude.Text
startContentUpload_knowledgeBaseId = Lens.lens (\StartContentUpload' {knowledgeBaseId} -> knowledgeBaseId) (\s@StartContentUpload' {} a -> s {knowledgeBaseId = a} :: StartContentUpload)

instance Core.AWSRequest StartContentUpload where
  type
    AWSResponse StartContentUpload =
      StartContentUploadResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartContentUploadResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "headersToInclude"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..:> "uploadId")
            Prelude.<*> (x Data..:> "url")
            Prelude.<*> (x Data..:> "urlExpiry")
      )

instance Prelude.Hashable StartContentUpload where
  hashWithSalt _salt StartContentUpload' {..} =
    _salt
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` knowledgeBaseId

instance Prelude.NFData StartContentUpload where
  rnf StartContentUpload' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf knowledgeBaseId

instance Data.ToHeaders StartContentUpload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartContentUpload where
  toJSON StartContentUpload' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("contentType" Data..= contentType)]
      )

instance Data.ToPath StartContentUpload where
  toPath StartContentUpload' {..} =
    Prelude.mconcat
      [ "/knowledgeBases/",
        Data.toBS knowledgeBaseId,
        "/upload"
      ]

instance Data.ToQuery StartContentUpload where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartContentUploadResponse' smart constructor.
data StartContentUploadResponse = StartContentUploadResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The headers to include in the upload.
    headersToInclude :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The identifier of the upload.
    uploadId :: Prelude.Text,
    -- | The URL of the upload.
    url :: Data.Sensitive Prelude.Text,
    -- | The expiration time of the URL as an epoch timestamp.
    urlExpiry :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartContentUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startContentUploadResponse_httpStatus' - The response's http status code.
--
-- 'headersToInclude', 'startContentUploadResponse_headersToInclude' - The headers to include in the upload.
--
-- 'uploadId', 'startContentUploadResponse_uploadId' - The identifier of the upload.
--
-- 'url', 'startContentUploadResponse_url' - The URL of the upload.
--
-- 'urlExpiry', 'startContentUploadResponse_urlExpiry' - The expiration time of the URL as an epoch timestamp.
newStartContentUploadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'uploadId'
  Prelude.Text ->
  -- | 'url'
  Prelude.Text ->
  -- | 'urlExpiry'
  Prelude.UTCTime ->
  StartContentUploadResponse
newStartContentUploadResponse
  pHttpStatus_
  pUploadId_
  pUrl_
  pUrlExpiry_ =
    StartContentUploadResponse'
      { httpStatus =
          pHttpStatus_,
        headersToInclude = Prelude.mempty,
        uploadId = pUploadId_,
        url = Data._Sensitive Lens.# pUrl_,
        urlExpiry = Data._Time Lens.# pUrlExpiry_
      }

-- | The response's http status code.
startContentUploadResponse_httpStatus :: Lens.Lens' StartContentUploadResponse Prelude.Int
startContentUploadResponse_httpStatus = Lens.lens (\StartContentUploadResponse' {httpStatus} -> httpStatus) (\s@StartContentUploadResponse' {} a -> s {httpStatus = a} :: StartContentUploadResponse)

-- | The headers to include in the upload.
startContentUploadResponse_headersToInclude :: Lens.Lens' StartContentUploadResponse (Prelude.HashMap Prelude.Text Prelude.Text)
startContentUploadResponse_headersToInclude = Lens.lens (\StartContentUploadResponse' {headersToInclude} -> headersToInclude) (\s@StartContentUploadResponse' {} a -> s {headersToInclude = a} :: StartContentUploadResponse) Prelude.. Lens.coerced

-- | The identifier of the upload.
startContentUploadResponse_uploadId :: Lens.Lens' StartContentUploadResponse Prelude.Text
startContentUploadResponse_uploadId = Lens.lens (\StartContentUploadResponse' {uploadId} -> uploadId) (\s@StartContentUploadResponse' {} a -> s {uploadId = a} :: StartContentUploadResponse)

-- | The URL of the upload.
startContentUploadResponse_url :: Lens.Lens' StartContentUploadResponse Prelude.Text
startContentUploadResponse_url = Lens.lens (\StartContentUploadResponse' {url} -> url) (\s@StartContentUploadResponse' {} a -> s {url = a} :: StartContentUploadResponse) Prelude.. Data._Sensitive

-- | The expiration time of the URL as an epoch timestamp.
startContentUploadResponse_urlExpiry :: Lens.Lens' StartContentUploadResponse Prelude.UTCTime
startContentUploadResponse_urlExpiry = Lens.lens (\StartContentUploadResponse' {urlExpiry} -> urlExpiry) (\s@StartContentUploadResponse' {} a -> s {urlExpiry = a} :: StartContentUploadResponse) Prelude.. Data._Time

instance Prelude.NFData StartContentUploadResponse where
  rnf StartContentUploadResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf headersToInclude
      `Prelude.seq` Prelude.rnf uploadId
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf urlExpiry
