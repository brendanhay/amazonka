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
-- Module      : Amazonka.Omics.CreateMultipartReadSetUpload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Begins a multipart read set upload.
module Amazonka.Omics.CreateMultipartReadSetUpload
  ( -- * Creating a Request
    CreateMultipartReadSetUpload (..),
    newCreateMultipartReadSetUpload,

    -- * Request Lenses
    createMultipartReadSetUpload_clientToken,
    createMultipartReadSetUpload_description,
    createMultipartReadSetUpload_generatedFrom,
    createMultipartReadSetUpload_tags,
    createMultipartReadSetUpload_sequenceStoreId,
    createMultipartReadSetUpload_sourceFileType,
    createMultipartReadSetUpload_subjectId,
    createMultipartReadSetUpload_sampleId,
    createMultipartReadSetUpload_referenceArn,
    createMultipartReadSetUpload_name,

    -- * Destructuring the Response
    CreateMultipartReadSetUploadResponse (..),
    newCreateMultipartReadSetUploadResponse,

    -- * Response Lenses
    createMultipartReadSetUploadResponse_description,
    createMultipartReadSetUploadResponse_generatedFrom,
    createMultipartReadSetUploadResponse_name,
    createMultipartReadSetUploadResponse_tags,
    createMultipartReadSetUploadResponse_httpStatus,
    createMultipartReadSetUploadResponse_sequenceStoreId,
    createMultipartReadSetUploadResponse_uploadId,
    createMultipartReadSetUploadResponse_sourceFileType,
    createMultipartReadSetUploadResponse_subjectId,
    createMultipartReadSetUploadResponse_sampleId,
    createMultipartReadSetUploadResponse_referenceArn,
    createMultipartReadSetUploadResponse_creationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMultipartReadSetUpload' smart constructor.
data CreateMultipartReadSetUpload = CreateMultipartReadSetUpload'
  { -- | An idempotency token that can be used to avoid triggering multiple
    -- multipart uploads.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the read set.
    description :: Prelude.Maybe Prelude.Text,
    -- | Where the source originated.
    generatedFrom :: Prelude.Maybe Prelude.Text,
    -- | Any tags to add to the read set.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The sequence store ID for the store that is the destination of the
    -- multipart uploads.
    sequenceStoreId :: Prelude.Text,
    -- | The type of file being uploaded.
    sourceFileType :: FileType,
    -- | The source\'s subject ID.
    subjectId :: Prelude.Text,
    -- | The source\'s sample ID.
    sampleId :: Prelude.Text,
    -- | The ARN of the reference.
    referenceArn :: Prelude.Text,
    -- | The name of the read set.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMultipartReadSetUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createMultipartReadSetUpload_clientToken' - An idempotency token that can be used to avoid triggering multiple
-- multipart uploads.
--
-- 'description', 'createMultipartReadSetUpload_description' - The description of the read set.
--
-- 'generatedFrom', 'createMultipartReadSetUpload_generatedFrom' - Where the source originated.
--
-- 'tags', 'createMultipartReadSetUpload_tags' - Any tags to add to the read set.
--
-- 'sequenceStoreId', 'createMultipartReadSetUpload_sequenceStoreId' - The sequence store ID for the store that is the destination of the
-- multipart uploads.
--
-- 'sourceFileType', 'createMultipartReadSetUpload_sourceFileType' - The type of file being uploaded.
--
-- 'subjectId', 'createMultipartReadSetUpload_subjectId' - The source\'s subject ID.
--
-- 'sampleId', 'createMultipartReadSetUpload_sampleId' - The source\'s sample ID.
--
-- 'referenceArn', 'createMultipartReadSetUpload_referenceArn' - The ARN of the reference.
--
-- 'name', 'createMultipartReadSetUpload_name' - The name of the read set.
newCreateMultipartReadSetUpload ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'sourceFileType'
  FileType ->
  -- | 'subjectId'
  Prelude.Text ->
  -- | 'sampleId'
  Prelude.Text ->
  -- | 'referenceArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateMultipartReadSetUpload
newCreateMultipartReadSetUpload
  pSequenceStoreId_
  pSourceFileType_
  pSubjectId_
  pSampleId_
  pReferenceArn_
  pName_ =
    CreateMultipartReadSetUpload'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        generatedFrom = Prelude.Nothing,
        tags = Prelude.Nothing,
        sequenceStoreId = pSequenceStoreId_,
        sourceFileType = pSourceFileType_,
        subjectId = pSubjectId_,
        sampleId = pSampleId_,
        referenceArn = pReferenceArn_,
        name = pName_
      }

-- | An idempotency token that can be used to avoid triggering multiple
-- multipart uploads.
createMultipartReadSetUpload_clientToken :: Lens.Lens' CreateMultipartReadSetUpload (Prelude.Maybe Prelude.Text)
createMultipartReadSetUpload_clientToken = Lens.lens (\CreateMultipartReadSetUpload' {clientToken} -> clientToken) (\s@CreateMultipartReadSetUpload' {} a -> s {clientToken = a} :: CreateMultipartReadSetUpload)

-- | The description of the read set.
createMultipartReadSetUpload_description :: Lens.Lens' CreateMultipartReadSetUpload (Prelude.Maybe Prelude.Text)
createMultipartReadSetUpload_description = Lens.lens (\CreateMultipartReadSetUpload' {description} -> description) (\s@CreateMultipartReadSetUpload' {} a -> s {description = a} :: CreateMultipartReadSetUpload)

-- | Where the source originated.
createMultipartReadSetUpload_generatedFrom :: Lens.Lens' CreateMultipartReadSetUpload (Prelude.Maybe Prelude.Text)
createMultipartReadSetUpload_generatedFrom = Lens.lens (\CreateMultipartReadSetUpload' {generatedFrom} -> generatedFrom) (\s@CreateMultipartReadSetUpload' {} a -> s {generatedFrom = a} :: CreateMultipartReadSetUpload)

-- | Any tags to add to the read set.
createMultipartReadSetUpload_tags :: Lens.Lens' CreateMultipartReadSetUpload (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createMultipartReadSetUpload_tags = Lens.lens (\CreateMultipartReadSetUpload' {tags} -> tags) (\s@CreateMultipartReadSetUpload' {} a -> s {tags = a} :: CreateMultipartReadSetUpload) Prelude.. Lens.mapping Lens.coerced

-- | The sequence store ID for the store that is the destination of the
-- multipart uploads.
createMultipartReadSetUpload_sequenceStoreId :: Lens.Lens' CreateMultipartReadSetUpload Prelude.Text
createMultipartReadSetUpload_sequenceStoreId = Lens.lens (\CreateMultipartReadSetUpload' {sequenceStoreId} -> sequenceStoreId) (\s@CreateMultipartReadSetUpload' {} a -> s {sequenceStoreId = a} :: CreateMultipartReadSetUpload)

-- | The type of file being uploaded.
createMultipartReadSetUpload_sourceFileType :: Lens.Lens' CreateMultipartReadSetUpload FileType
createMultipartReadSetUpload_sourceFileType = Lens.lens (\CreateMultipartReadSetUpload' {sourceFileType} -> sourceFileType) (\s@CreateMultipartReadSetUpload' {} a -> s {sourceFileType = a} :: CreateMultipartReadSetUpload)

-- | The source\'s subject ID.
createMultipartReadSetUpload_subjectId :: Lens.Lens' CreateMultipartReadSetUpload Prelude.Text
createMultipartReadSetUpload_subjectId = Lens.lens (\CreateMultipartReadSetUpload' {subjectId} -> subjectId) (\s@CreateMultipartReadSetUpload' {} a -> s {subjectId = a} :: CreateMultipartReadSetUpload)

-- | The source\'s sample ID.
createMultipartReadSetUpload_sampleId :: Lens.Lens' CreateMultipartReadSetUpload Prelude.Text
createMultipartReadSetUpload_sampleId = Lens.lens (\CreateMultipartReadSetUpload' {sampleId} -> sampleId) (\s@CreateMultipartReadSetUpload' {} a -> s {sampleId = a} :: CreateMultipartReadSetUpload)

-- | The ARN of the reference.
createMultipartReadSetUpload_referenceArn :: Lens.Lens' CreateMultipartReadSetUpload Prelude.Text
createMultipartReadSetUpload_referenceArn = Lens.lens (\CreateMultipartReadSetUpload' {referenceArn} -> referenceArn) (\s@CreateMultipartReadSetUpload' {} a -> s {referenceArn = a} :: CreateMultipartReadSetUpload)

-- | The name of the read set.
createMultipartReadSetUpload_name :: Lens.Lens' CreateMultipartReadSetUpload Prelude.Text
createMultipartReadSetUpload_name = Lens.lens (\CreateMultipartReadSetUpload' {name} -> name) (\s@CreateMultipartReadSetUpload' {} a -> s {name = a} :: CreateMultipartReadSetUpload)

instance Core.AWSRequest CreateMultipartReadSetUpload where
  type
    AWSResponse CreateMultipartReadSetUpload =
      CreateMultipartReadSetUploadResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMultipartReadSetUploadResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "generatedFrom")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "sequenceStoreId")
            Prelude.<*> (x Data..:> "uploadId")
            Prelude.<*> (x Data..:> "sourceFileType")
            Prelude.<*> (x Data..:> "subjectId")
            Prelude.<*> (x Data..:> "sampleId")
            Prelude.<*> (x Data..:> "referenceArn")
            Prelude.<*> (x Data..:> "creationTime")
      )

instance
  Prelude.Hashable
    CreateMultipartReadSetUpload
  where
  hashWithSalt _salt CreateMultipartReadSetUpload' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` generatedFrom
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sequenceStoreId
      `Prelude.hashWithSalt` sourceFileType
      `Prelude.hashWithSalt` subjectId
      `Prelude.hashWithSalt` sampleId
      `Prelude.hashWithSalt` referenceArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateMultipartReadSetUpload where
  rnf CreateMultipartReadSetUpload' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf generatedFrom
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf sourceFileType
      `Prelude.seq` Prelude.rnf subjectId
      `Prelude.seq` Prelude.rnf sampleId
      `Prelude.seq` Prelude.rnf referenceArn
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateMultipartReadSetUpload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMultipartReadSetUpload where
  toJSON CreateMultipartReadSetUpload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("generatedFrom" Data..=) Prelude.<$> generatedFrom,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("sourceFileType" Data..= sourceFileType),
            Prelude.Just ("subjectId" Data..= subjectId),
            Prelude.Just ("sampleId" Data..= sampleId),
            Prelude.Just ("referenceArn" Data..= referenceArn),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateMultipartReadSetUpload where
  toPath CreateMultipartReadSetUpload' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/upload"
      ]

instance Data.ToQuery CreateMultipartReadSetUpload where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMultipartReadSetUploadResponse' smart constructor.
data CreateMultipartReadSetUploadResponse = CreateMultipartReadSetUploadResponse'
  { -- | The description of the read set.
    description :: Prelude.Maybe Prelude.Text,
    -- | The source of the read set.
    generatedFrom :: Prelude.Maybe Prelude.Text,
    -- | The name of the read set.
    name :: Prelude.Maybe Prelude.Text,
    -- | The tags to add to the read set.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The sequence store ID for the store that the read set will be created
    -- in.
    sequenceStoreId :: Prelude.Text,
    -- | he ID for the initiated multipart upload.
    uploadId :: Prelude.Text,
    -- | The file type of the read set source.
    sourceFileType :: FileType,
    -- | The source\'s subject ID.
    subjectId :: Prelude.Text,
    -- | The source\'s sample ID.
    sampleId :: Prelude.Text,
    -- | The read set source\'s reference ARN.
    referenceArn :: Prelude.Text,
    -- | The creation time of the multipart upload.
    creationTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMultipartReadSetUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createMultipartReadSetUploadResponse_description' - The description of the read set.
--
-- 'generatedFrom', 'createMultipartReadSetUploadResponse_generatedFrom' - The source of the read set.
--
-- 'name', 'createMultipartReadSetUploadResponse_name' - The name of the read set.
--
-- 'tags', 'createMultipartReadSetUploadResponse_tags' - The tags to add to the read set.
--
-- 'httpStatus', 'createMultipartReadSetUploadResponse_httpStatus' - The response's http status code.
--
-- 'sequenceStoreId', 'createMultipartReadSetUploadResponse_sequenceStoreId' - The sequence store ID for the store that the read set will be created
-- in.
--
-- 'uploadId', 'createMultipartReadSetUploadResponse_uploadId' - he ID for the initiated multipart upload.
--
-- 'sourceFileType', 'createMultipartReadSetUploadResponse_sourceFileType' - The file type of the read set source.
--
-- 'subjectId', 'createMultipartReadSetUploadResponse_subjectId' - The source\'s subject ID.
--
-- 'sampleId', 'createMultipartReadSetUploadResponse_sampleId' - The source\'s sample ID.
--
-- 'referenceArn', 'createMultipartReadSetUploadResponse_referenceArn' - The read set source\'s reference ARN.
--
-- 'creationTime', 'createMultipartReadSetUploadResponse_creationTime' - The creation time of the multipart upload.
newCreateMultipartReadSetUploadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  -- | 'sourceFileType'
  FileType ->
  -- | 'subjectId'
  Prelude.Text ->
  -- | 'sampleId'
  Prelude.Text ->
  -- | 'referenceArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  CreateMultipartReadSetUploadResponse
newCreateMultipartReadSetUploadResponse
  pHttpStatus_
  pSequenceStoreId_
  pUploadId_
  pSourceFileType_
  pSubjectId_
  pSampleId_
  pReferenceArn_
  pCreationTime_ =
    CreateMultipartReadSetUploadResponse'
      { description =
          Prelude.Nothing,
        generatedFrom = Prelude.Nothing,
        name = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        sequenceStoreId = pSequenceStoreId_,
        uploadId = pUploadId_,
        sourceFileType = pSourceFileType_,
        subjectId = pSubjectId_,
        sampleId = pSampleId_,
        referenceArn = pReferenceArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_
      }

-- | The description of the read set.
createMultipartReadSetUploadResponse_description :: Lens.Lens' CreateMultipartReadSetUploadResponse (Prelude.Maybe Prelude.Text)
createMultipartReadSetUploadResponse_description = Lens.lens (\CreateMultipartReadSetUploadResponse' {description} -> description) (\s@CreateMultipartReadSetUploadResponse' {} a -> s {description = a} :: CreateMultipartReadSetUploadResponse)

-- | The source of the read set.
createMultipartReadSetUploadResponse_generatedFrom :: Lens.Lens' CreateMultipartReadSetUploadResponse (Prelude.Maybe Prelude.Text)
createMultipartReadSetUploadResponse_generatedFrom = Lens.lens (\CreateMultipartReadSetUploadResponse' {generatedFrom} -> generatedFrom) (\s@CreateMultipartReadSetUploadResponse' {} a -> s {generatedFrom = a} :: CreateMultipartReadSetUploadResponse)

-- | The name of the read set.
createMultipartReadSetUploadResponse_name :: Lens.Lens' CreateMultipartReadSetUploadResponse (Prelude.Maybe Prelude.Text)
createMultipartReadSetUploadResponse_name = Lens.lens (\CreateMultipartReadSetUploadResponse' {name} -> name) (\s@CreateMultipartReadSetUploadResponse' {} a -> s {name = a} :: CreateMultipartReadSetUploadResponse)

-- | The tags to add to the read set.
createMultipartReadSetUploadResponse_tags :: Lens.Lens' CreateMultipartReadSetUploadResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createMultipartReadSetUploadResponse_tags = Lens.lens (\CreateMultipartReadSetUploadResponse' {tags} -> tags) (\s@CreateMultipartReadSetUploadResponse' {} a -> s {tags = a} :: CreateMultipartReadSetUploadResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createMultipartReadSetUploadResponse_httpStatus :: Lens.Lens' CreateMultipartReadSetUploadResponse Prelude.Int
createMultipartReadSetUploadResponse_httpStatus = Lens.lens (\CreateMultipartReadSetUploadResponse' {httpStatus} -> httpStatus) (\s@CreateMultipartReadSetUploadResponse' {} a -> s {httpStatus = a} :: CreateMultipartReadSetUploadResponse)

-- | The sequence store ID for the store that the read set will be created
-- in.
createMultipartReadSetUploadResponse_sequenceStoreId :: Lens.Lens' CreateMultipartReadSetUploadResponse Prelude.Text
createMultipartReadSetUploadResponse_sequenceStoreId = Lens.lens (\CreateMultipartReadSetUploadResponse' {sequenceStoreId} -> sequenceStoreId) (\s@CreateMultipartReadSetUploadResponse' {} a -> s {sequenceStoreId = a} :: CreateMultipartReadSetUploadResponse)

-- | he ID for the initiated multipart upload.
createMultipartReadSetUploadResponse_uploadId :: Lens.Lens' CreateMultipartReadSetUploadResponse Prelude.Text
createMultipartReadSetUploadResponse_uploadId = Lens.lens (\CreateMultipartReadSetUploadResponse' {uploadId} -> uploadId) (\s@CreateMultipartReadSetUploadResponse' {} a -> s {uploadId = a} :: CreateMultipartReadSetUploadResponse)

-- | The file type of the read set source.
createMultipartReadSetUploadResponse_sourceFileType :: Lens.Lens' CreateMultipartReadSetUploadResponse FileType
createMultipartReadSetUploadResponse_sourceFileType = Lens.lens (\CreateMultipartReadSetUploadResponse' {sourceFileType} -> sourceFileType) (\s@CreateMultipartReadSetUploadResponse' {} a -> s {sourceFileType = a} :: CreateMultipartReadSetUploadResponse)

-- | The source\'s subject ID.
createMultipartReadSetUploadResponse_subjectId :: Lens.Lens' CreateMultipartReadSetUploadResponse Prelude.Text
createMultipartReadSetUploadResponse_subjectId = Lens.lens (\CreateMultipartReadSetUploadResponse' {subjectId} -> subjectId) (\s@CreateMultipartReadSetUploadResponse' {} a -> s {subjectId = a} :: CreateMultipartReadSetUploadResponse)

-- | The source\'s sample ID.
createMultipartReadSetUploadResponse_sampleId :: Lens.Lens' CreateMultipartReadSetUploadResponse Prelude.Text
createMultipartReadSetUploadResponse_sampleId = Lens.lens (\CreateMultipartReadSetUploadResponse' {sampleId} -> sampleId) (\s@CreateMultipartReadSetUploadResponse' {} a -> s {sampleId = a} :: CreateMultipartReadSetUploadResponse)

-- | The read set source\'s reference ARN.
createMultipartReadSetUploadResponse_referenceArn :: Lens.Lens' CreateMultipartReadSetUploadResponse Prelude.Text
createMultipartReadSetUploadResponse_referenceArn = Lens.lens (\CreateMultipartReadSetUploadResponse' {referenceArn} -> referenceArn) (\s@CreateMultipartReadSetUploadResponse' {} a -> s {referenceArn = a} :: CreateMultipartReadSetUploadResponse)

-- | The creation time of the multipart upload.
createMultipartReadSetUploadResponse_creationTime :: Lens.Lens' CreateMultipartReadSetUploadResponse Prelude.UTCTime
createMultipartReadSetUploadResponse_creationTime = Lens.lens (\CreateMultipartReadSetUploadResponse' {creationTime} -> creationTime) (\s@CreateMultipartReadSetUploadResponse' {} a -> s {creationTime = a} :: CreateMultipartReadSetUploadResponse) Prelude.. Data._Time

instance
  Prelude.NFData
    CreateMultipartReadSetUploadResponse
  where
  rnf CreateMultipartReadSetUploadResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf generatedFrom
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf uploadId
      `Prelude.seq` Prelude.rnf sourceFileType
      `Prelude.seq` Prelude.rnf subjectId
      `Prelude.seq` Prelude.rnf sampleId
      `Prelude.seq` Prelude.rnf referenceArn
      `Prelude.seq` Prelude.rnf creationTime
