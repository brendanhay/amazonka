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
-- Module      : Amazonka.Omics.GetReadSetMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a read set.
module Amazonka.Omics.GetReadSetMetadata
  ( -- * Creating a Request
    GetReadSetMetadata (..),
    newGetReadSetMetadata,

    -- * Request Lenses
    getReadSetMetadata_id,
    getReadSetMetadata_sequenceStoreId,

    -- * Destructuring the Response
    GetReadSetMetadataResponse (..),
    newGetReadSetMetadataResponse,

    -- * Response Lenses
    getReadSetMetadataResponse_description,
    getReadSetMetadataResponse_files,
    getReadSetMetadataResponse_name,
    getReadSetMetadataResponse_referenceArn,
    getReadSetMetadataResponse_sampleId,
    getReadSetMetadataResponse_sequenceInformation,
    getReadSetMetadataResponse_statusMessage,
    getReadSetMetadataResponse_subjectId,
    getReadSetMetadataResponse_httpStatus,
    getReadSetMetadataResponse_id,
    getReadSetMetadataResponse_arn,
    getReadSetMetadataResponse_sequenceStoreId,
    getReadSetMetadataResponse_status,
    getReadSetMetadataResponse_fileType,
    getReadSetMetadataResponse_creationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetReadSetMetadata' smart constructor.
data GetReadSetMetadata = GetReadSetMetadata'
  { -- | The read set\'s ID.
    id :: Prelude.Text,
    -- | The read set\'s sequence store ID.
    sequenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadSetMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getReadSetMetadata_id' - The read set\'s ID.
--
-- 'sequenceStoreId', 'getReadSetMetadata_sequenceStoreId' - The read set\'s sequence store ID.
newGetReadSetMetadata ::
  -- | 'id'
  Prelude.Text ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  GetReadSetMetadata
newGetReadSetMetadata pId_ pSequenceStoreId_ =
  GetReadSetMetadata'
    { id = pId_,
      sequenceStoreId = pSequenceStoreId_
    }

-- | The read set\'s ID.
getReadSetMetadata_id :: Lens.Lens' GetReadSetMetadata Prelude.Text
getReadSetMetadata_id = Lens.lens (\GetReadSetMetadata' {id} -> id) (\s@GetReadSetMetadata' {} a -> s {id = a} :: GetReadSetMetadata)

-- | The read set\'s sequence store ID.
getReadSetMetadata_sequenceStoreId :: Lens.Lens' GetReadSetMetadata Prelude.Text
getReadSetMetadata_sequenceStoreId = Lens.lens (\GetReadSetMetadata' {sequenceStoreId} -> sequenceStoreId) (\s@GetReadSetMetadata' {} a -> s {sequenceStoreId = a} :: GetReadSetMetadata)

instance Core.AWSRequest GetReadSetMetadata where
  type
    AWSResponse GetReadSetMetadata =
      GetReadSetMetadataResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReadSetMetadataResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "files")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "referenceArn")
            Prelude.<*> (x Data..?> "sampleId")
            Prelude.<*> (x Data..?> "sequenceInformation")
            Prelude.<*> (x Data..?> "statusMessage")
            Prelude.<*> (x Data..?> "subjectId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "sequenceStoreId")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "fileType")
            Prelude.<*> (x Data..:> "creationTime")
      )

instance Prelude.Hashable GetReadSetMetadata where
  hashWithSalt _salt GetReadSetMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` sequenceStoreId

instance Prelude.NFData GetReadSetMetadata where
  rnf GetReadSetMetadata' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf sequenceStoreId

instance Data.ToHeaders GetReadSetMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetReadSetMetadata where
  toPath GetReadSetMetadata' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/readset/",
        Data.toBS id,
        "/metadata"
      ]

instance Data.ToQuery GetReadSetMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReadSetMetadataResponse' smart constructor.
data GetReadSetMetadataResponse = GetReadSetMetadataResponse'
  { -- | The read set\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The read set\'s files.
    files :: Prelude.Maybe ReadSetFiles,
    -- | The read set\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The read set\'s genome reference ARN.
    referenceArn :: Prelude.Maybe Prelude.Text,
    -- | The read set\'s sample ID.
    sampleId :: Prelude.Maybe Prelude.Text,
    -- | The read set\'s sequence information.
    sequenceInformation :: Prelude.Maybe SequenceInformation,
    -- | The status message for a read set. It provides more detail as to why the
    -- read set has a status.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The read set\'s subject ID.
    subjectId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The read set\'s ID.
    id :: Prelude.Text,
    -- | The read set\'s ARN.
    arn :: Prelude.Text,
    -- | The read set\'s sequence store ID.
    sequenceStoreId :: Prelude.Text,
    -- | The read set\'s status.
    status :: ReadSetStatus,
    -- | The read set\'s file type.
    fileType :: FileType,
    -- | When the read set was created.
    creationTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadSetMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getReadSetMetadataResponse_description' - The read set\'s description.
--
-- 'files', 'getReadSetMetadataResponse_files' - The read set\'s files.
--
-- 'name', 'getReadSetMetadataResponse_name' - The read set\'s name.
--
-- 'referenceArn', 'getReadSetMetadataResponse_referenceArn' - The read set\'s genome reference ARN.
--
-- 'sampleId', 'getReadSetMetadataResponse_sampleId' - The read set\'s sample ID.
--
-- 'sequenceInformation', 'getReadSetMetadataResponse_sequenceInformation' - The read set\'s sequence information.
--
-- 'statusMessage', 'getReadSetMetadataResponse_statusMessage' - The status message for a read set. It provides more detail as to why the
-- read set has a status.
--
-- 'subjectId', 'getReadSetMetadataResponse_subjectId' - The read set\'s subject ID.
--
-- 'httpStatus', 'getReadSetMetadataResponse_httpStatus' - The response's http status code.
--
-- 'id', 'getReadSetMetadataResponse_id' - The read set\'s ID.
--
-- 'arn', 'getReadSetMetadataResponse_arn' - The read set\'s ARN.
--
-- 'sequenceStoreId', 'getReadSetMetadataResponse_sequenceStoreId' - The read set\'s sequence store ID.
--
-- 'status', 'getReadSetMetadataResponse_status' - The read set\'s status.
--
-- 'fileType', 'getReadSetMetadataResponse_fileType' - The read set\'s file type.
--
-- 'creationTime', 'getReadSetMetadataResponse_creationTime' - When the read set was created.
newGetReadSetMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'status'
  ReadSetStatus ->
  -- | 'fileType'
  FileType ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  GetReadSetMetadataResponse
newGetReadSetMetadataResponse
  pHttpStatus_
  pId_
  pArn_
  pSequenceStoreId_
  pStatus_
  pFileType_
  pCreationTime_ =
    GetReadSetMetadataResponse'
      { description =
          Prelude.Nothing,
        files = Prelude.Nothing,
        name = Prelude.Nothing,
        referenceArn = Prelude.Nothing,
        sampleId = Prelude.Nothing,
        sequenceInformation = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        subjectId = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        id = pId_,
        arn = pArn_,
        sequenceStoreId = pSequenceStoreId_,
        status = pStatus_,
        fileType = pFileType_,
        creationTime = Data._Time Lens.# pCreationTime_
      }

-- | The read set\'s description.
getReadSetMetadataResponse_description :: Lens.Lens' GetReadSetMetadataResponse (Prelude.Maybe Prelude.Text)
getReadSetMetadataResponse_description = Lens.lens (\GetReadSetMetadataResponse' {description} -> description) (\s@GetReadSetMetadataResponse' {} a -> s {description = a} :: GetReadSetMetadataResponse)

-- | The read set\'s files.
getReadSetMetadataResponse_files :: Lens.Lens' GetReadSetMetadataResponse (Prelude.Maybe ReadSetFiles)
getReadSetMetadataResponse_files = Lens.lens (\GetReadSetMetadataResponse' {files} -> files) (\s@GetReadSetMetadataResponse' {} a -> s {files = a} :: GetReadSetMetadataResponse)

-- | The read set\'s name.
getReadSetMetadataResponse_name :: Lens.Lens' GetReadSetMetadataResponse (Prelude.Maybe Prelude.Text)
getReadSetMetadataResponse_name = Lens.lens (\GetReadSetMetadataResponse' {name} -> name) (\s@GetReadSetMetadataResponse' {} a -> s {name = a} :: GetReadSetMetadataResponse)

-- | The read set\'s genome reference ARN.
getReadSetMetadataResponse_referenceArn :: Lens.Lens' GetReadSetMetadataResponse (Prelude.Maybe Prelude.Text)
getReadSetMetadataResponse_referenceArn = Lens.lens (\GetReadSetMetadataResponse' {referenceArn} -> referenceArn) (\s@GetReadSetMetadataResponse' {} a -> s {referenceArn = a} :: GetReadSetMetadataResponse)

-- | The read set\'s sample ID.
getReadSetMetadataResponse_sampleId :: Lens.Lens' GetReadSetMetadataResponse (Prelude.Maybe Prelude.Text)
getReadSetMetadataResponse_sampleId = Lens.lens (\GetReadSetMetadataResponse' {sampleId} -> sampleId) (\s@GetReadSetMetadataResponse' {} a -> s {sampleId = a} :: GetReadSetMetadataResponse)

-- | The read set\'s sequence information.
getReadSetMetadataResponse_sequenceInformation :: Lens.Lens' GetReadSetMetadataResponse (Prelude.Maybe SequenceInformation)
getReadSetMetadataResponse_sequenceInformation = Lens.lens (\GetReadSetMetadataResponse' {sequenceInformation} -> sequenceInformation) (\s@GetReadSetMetadataResponse' {} a -> s {sequenceInformation = a} :: GetReadSetMetadataResponse)

-- | The status message for a read set. It provides more detail as to why the
-- read set has a status.
getReadSetMetadataResponse_statusMessage :: Lens.Lens' GetReadSetMetadataResponse (Prelude.Maybe Prelude.Text)
getReadSetMetadataResponse_statusMessage = Lens.lens (\GetReadSetMetadataResponse' {statusMessage} -> statusMessage) (\s@GetReadSetMetadataResponse' {} a -> s {statusMessage = a} :: GetReadSetMetadataResponse)

-- | The read set\'s subject ID.
getReadSetMetadataResponse_subjectId :: Lens.Lens' GetReadSetMetadataResponse (Prelude.Maybe Prelude.Text)
getReadSetMetadataResponse_subjectId = Lens.lens (\GetReadSetMetadataResponse' {subjectId} -> subjectId) (\s@GetReadSetMetadataResponse' {} a -> s {subjectId = a} :: GetReadSetMetadataResponse)

-- | The response's http status code.
getReadSetMetadataResponse_httpStatus :: Lens.Lens' GetReadSetMetadataResponse Prelude.Int
getReadSetMetadataResponse_httpStatus = Lens.lens (\GetReadSetMetadataResponse' {httpStatus} -> httpStatus) (\s@GetReadSetMetadataResponse' {} a -> s {httpStatus = a} :: GetReadSetMetadataResponse)

-- | The read set\'s ID.
getReadSetMetadataResponse_id :: Lens.Lens' GetReadSetMetadataResponse Prelude.Text
getReadSetMetadataResponse_id = Lens.lens (\GetReadSetMetadataResponse' {id} -> id) (\s@GetReadSetMetadataResponse' {} a -> s {id = a} :: GetReadSetMetadataResponse)

-- | The read set\'s ARN.
getReadSetMetadataResponse_arn :: Lens.Lens' GetReadSetMetadataResponse Prelude.Text
getReadSetMetadataResponse_arn = Lens.lens (\GetReadSetMetadataResponse' {arn} -> arn) (\s@GetReadSetMetadataResponse' {} a -> s {arn = a} :: GetReadSetMetadataResponse)

-- | The read set\'s sequence store ID.
getReadSetMetadataResponse_sequenceStoreId :: Lens.Lens' GetReadSetMetadataResponse Prelude.Text
getReadSetMetadataResponse_sequenceStoreId = Lens.lens (\GetReadSetMetadataResponse' {sequenceStoreId} -> sequenceStoreId) (\s@GetReadSetMetadataResponse' {} a -> s {sequenceStoreId = a} :: GetReadSetMetadataResponse)

-- | The read set\'s status.
getReadSetMetadataResponse_status :: Lens.Lens' GetReadSetMetadataResponse ReadSetStatus
getReadSetMetadataResponse_status = Lens.lens (\GetReadSetMetadataResponse' {status} -> status) (\s@GetReadSetMetadataResponse' {} a -> s {status = a} :: GetReadSetMetadataResponse)

-- | The read set\'s file type.
getReadSetMetadataResponse_fileType :: Lens.Lens' GetReadSetMetadataResponse FileType
getReadSetMetadataResponse_fileType = Lens.lens (\GetReadSetMetadataResponse' {fileType} -> fileType) (\s@GetReadSetMetadataResponse' {} a -> s {fileType = a} :: GetReadSetMetadataResponse)

-- | When the read set was created.
getReadSetMetadataResponse_creationTime :: Lens.Lens' GetReadSetMetadataResponse Prelude.UTCTime
getReadSetMetadataResponse_creationTime = Lens.lens (\GetReadSetMetadataResponse' {creationTime} -> creationTime) (\s@GetReadSetMetadataResponse' {} a -> s {creationTime = a} :: GetReadSetMetadataResponse) Prelude.. Data._Time

instance Prelude.NFData GetReadSetMetadataResponse where
  rnf GetReadSetMetadataResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf files
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf referenceArn
      `Prelude.seq` Prelude.rnf sampleId
      `Prelude.seq` Prelude.rnf sequenceInformation
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf subjectId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf fileType
      `Prelude.seq` Prelude.rnf creationTime
