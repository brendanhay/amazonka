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
-- Module      : Amazonka.BackupStorage.NotifyObjectComplete
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Complete upload
module Amazonka.BackupStorage.NotifyObjectComplete
  ( -- * Creating a Request
    NotifyObjectComplete (..),
    newNotifyObjectComplete,

    -- * Request Lenses
    notifyObjectComplete_metadataBlobChecksum,
    notifyObjectComplete_metadataBlobChecksumAlgorithm,
    notifyObjectComplete_metadataBlobLength,
    notifyObjectComplete_metadataString,
    notifyObjectComplete_backupJobId,
    notifyObjectComplete_uploadId,
    notifyObjectComplete_objectChecksum,
    notifyObjectComplete_objectChecksumAlgorithm,
    notifyObjectComplete_metadataBlob,

    -- * Destructuring the Response
    NotifyObjectCompleteResponse (..),
    newNotifyObjectCompleteResponse,

    -- * Response Lenses
    notifyObjectCompleteResponse_httpStatus,
    notifyObjectCompleteResponse_objectChecksum,
    notifyObjectCompleteResponse_objectChecksumAlgorithm,
  )
where

import Amazonka.BackupStorage.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newNotifyObjectComplete' smart constructor.
data NotifyObjectComplete = NotifyObjectComplete'
  { -- | Checksum of MetadataBlob.
    metadataBlobChecksum :: Prelude.Maybe Prelude.Text,
    -- | Checksum algorithm.
    metadataBlobChecksumAlgorithm :: Prelude.Maybe DataChecksumAlgorithm,
    -- | The size of MetadataBlob.
    metadataBlobLength :: Prelude.Maybe Prelude.Integer,
    -- | Optional metadata associated with an Object. Maximum string length is
    -- 256 bytes.
    metadataString :: Prelude.Maybe Prelude.Text,
    -- | Backup job Id for the in-progress backup
    backupJobId :: Prelude.Text,
    -- | Upload Id for the in-progress upload
    uploadId :: Prelude.Text,
    -- | Object checksum
    objectChecksum :: Prelude.Text,
    -- | Checksum algorithm
    objectChecksumAlgorithm :: SummaryChecksumAlgorithm,
    -- | Optional metadata associated with an Object. Maximum length is 4MB.
    metadataBlob :: Data.HashedBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyObjectComplete' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadataBlobChecksum', 'notifyObjectComplete_metadataBlobChecksum' - Checksum of MetadataBlob.
--
-- 'metadataBlobChecksumAlgorithm', 'notifyObjectComplete_metadataBlobChecksumAlgorithm' - Checksum algorithm.
--
-- 'metadataBlobLength', 'notifyObjectComplete_metadataBlobLength' - The size of MetadataBlob.
--
-- 'metadataString', 'notifyObjectComplete_metadataString' - Optional metadata associated with an Object. Maximum string length is
-- 256 bytes.
--
-- 'backupJobId', 'notifyObjectComplete_backupJobId' - Backup job Id for the in-progress backup
--
-- 'uploadId', 'notifyObjectComplete_uploadId' - Upload Id for the in-progress upload
--
-- 'objectChecksum', 'notifyObjectComplete_objectChecksum' - Object checksum
--
-- 'objectChecksumAlgorithm', 'notifyObjectComplete_objectChecksumAlgorithm' - Checksum algorithm
--
-- 'metadataBlob', 'notifyObjectComplete_metadataBlob' - Optional metadata associated with an Object. Maximum length is 4MB.
newNotifyObjectComplete ::
  -- | 'backupJobId'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  -- | 'objectChecksum'
  Prelude.Text ->
  -- | 'objectChecksumAlgorithm'
  SummaryChecksumAlgorithm ->
  -- | 'metadataBlob'
  Data.HashedBody ->
  NotifyObjectComplete
newNotifyObjectComplete
  pBackupJobId_
  pUploadId_
  pObjectChecksum_
  pObjectChecksumAlgorithm_
  pMetadataBlob_ =
    NotifyObjectComplete'
      { metadataBlobChecksum =
          Prelude.Nothing,
        metadataBlobChecksumAlgorithm = Prelude.Nothing,
        metadataBlobLength = Prelude.Nothing,
        metadataString = Prelude.Nothing,
        backupJobId = pBackupJobId_,
        uploadId = pUploadId_,
        objectChecksum = pObjectChecksum_,
        objectChecksumAlgorithm = pObjectChecksumAlgorithm_,
        metadataBlob = pMetadataBlob_
      }

-- | Checksum of MetadataBlob.
notifyObjectComplete_metadataBlobChecksum :: Lens.Lens' NotifyObjectComplete (Prelude.Maybe Prelude.Text)
notifyObjectComplete_metadataBlobChecksum = Lens.lens (\NotifyObjectComplete' {metadataBlobChecksum} -> metadataBlobChecksum) (\s@NotifyObjectComplete' {} a -> s {metadataBlobChecksum = a} :: NotifyObjectComplete)

-- | Checksum algorithm.
notifyObjectComplete_metadataBlobChecksumAlgorithm :: Lens.Lens' NotifyObjectComplete (Prelude.Maybe DataChecksumAlgorithm)
notifyObjectComplete_metadataBlobChecksumAlgorithm = Lens.lens (\NotifyObjectComplete' {metadataBlobChecksumAlgorithm} -> metadataBlobChecksumAlgorithm) (\s@NotifyObjectComplete' {} a -> s {metadataBlobChecksumAlgorithm = a} :: NotifyObjectComplete)

-- | The size of MetadataBlob.
notifyObjectComplete_metadataBlobLength :: Lens.Lens' NotifyObjectComplete (Prelude.Maybe Prelude.Integer)
notifyObjectComplete_metadataBlobLength = Lens.lens (\NotifyObjectComplete' {metadataBlobLength} -> metadataBlobLength) (\s@NotifyObjectComplete' {} a -> s {metadataBlobLength = a} :: NotifyObjectComplete)

-- | Optional metadata associated with an Object. Maximum string length is
-- 256 bytes.
notifyObjectComplete_metadataString :: Lens.Lens' NotifyObjectComplete (Prelude.Maybe Prelude.Text)
notifyObjectComplete_metadataString = Lens.lens (\NotifyObjectComplete' {metadataString} -> metadataString) (\s@NotifyObjectComplete' {} a -> s {metadataString = a} :: NotifyObjectComplete)

-- | Backup job Id for the in-progress backup
notifyObjectComplete_backupJobId :: Lens.Lens' NotifyObjectComplete Prelude.Text
notifyObjectComplete_backupJobId = Lens.lens (\NotifyObjectComplete' {backupJobId} -> backupJobId) (\s@NotifyObjectComplete' {} a -> s {backupJobId = a} :: NotifyObjectComplete)

-- | Upload Id for the in-progress upload
notifyObjectComplete_uploadId :: Lens.Lens' NotifyObjectComplete Prelude.Text
notifyObjectComplete_uploadId = Lens.lens (\NotifyObjectComplete' {uploadId} -> uploadId) (\s@NotifyObjectComplete' {} a -> s {uploadId = a} :: NotifyObjectComplete)

-- | Object checksum
notifyObjectComplete_objectChecksum :: Lens.Lens' NotifyObjectComplete Prelude.Text
notifyObjectComplete_objectChecksum = Lens.lens (\NotifyObjectComplete' {objectChecksum} -> objectChecksum) (\s@NotifyObjectComplete' {} a -> s {objectChecksum = a} :: NotifyObjectComplete)

-- | Checksum algorithm
notifyObjectComplete_objectChecksumAlgorithm :: Lens.Lens' NotifyObjectComplete SummaryChecksumAlgorithm
notifyObjectComplete_objectChecksumAlgorithm = Lens.lens (\NotifyObjectComplete' {objectChecksumAlgorithm} -> objectChecksumAlgorithm) (\s@NotifyObjectComplete' {} a -> s {objectChecksumAlgorithm = a} :: NotifyObjectComplete)

-- | Optional metadata associated with an Object. Maximum length is 4MB.
notifyObjectComplete_metadataBlob :: Lens.Lens' NotifyObjectComplete Data.HashedBody
notifyObjectComplete_metadataBlob = Lens.lens (\NotifyObjectComplete' {metadataBlob} -> metadataBlob) (\s@NotifyObjectComplete' {} a -> s {metadataBlob = a} :: NotifyObjectComplete)

instance Core.AWSRequest NotifyObjectComplete where
  type
    AWSResponse NotifyObjectComplete =
      NotifyObjectCompleteResponse
  request overrides =
    Request.putBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          NotifyObjectCompleteResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ObjectChecksum")
            Prelude.<*> (x Data..:> "ObjectChecksumAlgorithm")
      )

instance Data.ToBody NotifyObjectComplete where
  toBody NotifyObjectComplete' {..} =
    Data.toBody metadataBlob

instance Data.ToHeaders NotifyObjectComplete where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath NotifyObjectComplete where
  toPath NotifyObjectComplete' {..} =
    Prelude.mconcat
      [ "/backup-jobs/",
        Data.toBS backupJobId,
        "/object/",
        Data.toBS uploadId,
        "/complete"
      ]

instance Data.ToQuery NotifyObjectComplete where
  toQuery NotifyObjectComplete' {..} =
    Prelude.mconcat
      [ "metadata-checksum" Data.=: metadataBlobChecksum,
        "metadata-checksum-algorithm"
          Data.=: metadataBlobChecksumAlgorithm,
        "metadata-blob-length" Data.=: metadataBlobLength,
        "metadata-string" Data.=: metadataString,
        "checksum" Data.=: objectChecksum,
        "checksum-algorithm" Data.=: objectChecksumAlgorithm
      ]

-- | /See:/ 'newNotifyObjectCompleteResponse' smart constructor.
data NotifyObjectCompleteResponse = NotifyObjectCompleteResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Object checksum
    objectChecksum :: Prelude.Text,
    -- | Checksum algorithm
    objectChecksumAlgorithm :: SummaryChecksumAlgorithm
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyObjectCompleteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'notifyObjectCompleteResponse_httpStatus' - The response's http status code.
--
-- 'objectChecksum', 'notifyObjectCompleteResponse_objectChecksum' - Object checksum
--
-- 'objectChecksumAlgorithm', 'notifyObjectCompleteResponse_objectChecksumAlgorithm' - Checksum algorithm
newNotifyObjectCompleteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'objectChecksum'
  Prelude.Text ->
  -- | 'objectChecksumAlgorithm'
  SummaryChecksumAlgorithm ->
  NotifyObjectCompleteResponse
newNotifyObjectCompleteResponse
  pHttpStatus_
  pObjectChecksum_
  pObjectChecksumAlgorithm_ =
    NotifyObjectCompleteResponse'
      { httpStatus =
          pHttpStatus_,
        objectChecksum = pObjectChecksum_,
        objectChecksumAlgorithm =
          pObjectChecksumAlgorithm_
      }

-- | The response's http status code.
notifyObjectCompleteResponse_httpStatus :: Lens.Lens' NotifyObjectCompleteResponse Prelude.Int
notifyObjectCompleteResponse_httpStatus = Lens.lens (\NotifyObjectCompleteResponse' {httpStatus} -> httpStatus) (\s@NotifyObjectCompleteResponse' {} a -> s {httpStatus = a} :: NotifyObjectCompleteResponse)

-- | Object checksum
notifyObjectCompleteResponse_objectChecksum :: Lens.Lens' NotifyObjectCompleteResponse Prelude.Text
notifyObjectCompleteResponse_objectChecksum = Lens.lens (\NotifyObjectCompleteResponse' {objectChecksum} -> objectChecksum) (\s@NotifyObjectCompleteResponse' {} a -> s {objectChecksum = a} :: NotifyObjectCompleteResponse)

-- | Checksum algorithm
notifyObjectCompleteResponse_objectChecksumAlgorithm :: Lens.Lens' NotifyObjectCompleteResponse SummaryChecksumAlgorithm
notifyObjectCompleteResponse_objectChecksumAlgorithm = Lens.lens (\NotifyObjectCompleteResponse' {objectChecksumAlgorithm} -> objectChecksumAlgorithm) (\s@NotifyObjectCompleteResponse' {} a -> s {objectChecksumAlgorithm = a} :: NotifyObjectCompleteResponse)

instance Prelude.NFData NotifyObjectCompleteResponse where
  rnf NotifyObjectCompleteResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf objectChecksum
      `Prelude.seq` Prelude.rnf objectChecksumAlgorithm
