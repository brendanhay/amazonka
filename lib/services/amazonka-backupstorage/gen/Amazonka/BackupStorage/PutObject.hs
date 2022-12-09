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
-- Module      : Amazonka.BackupStorage.PutObject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Upload object that can store object metadata String and data blob in
-- single API call using inline chunk field.
module Amazonka.BackupStorage.PutObject
  ( -- * Creating a Request
    PutObject (..),
    newPutObject,

    -- * Request Lenses
    putObject_inlineChunkChecksum,
    putObject_inlineChunkChecksumAlgorithm,
    putObject_inlineChunkLength,
    putObject_metadataString,
    putObject_objectChecksum,
    putObject_objectChecksumAlgorithm,
    putObject_throwOnDuplicate,
    putObject_backupJobId,
    putObject_objectName,
    putObject_inlineChunk,

    -- * Destructuring the Response
    PutObjectResponse (..),
    newPutObjectResponse,

    -- * Response Lenses
    putObjectResponse_httpStatus,
    putObjectResponse_inlineChunkChecksum,
    putObjectResponse_inlineChunkChecksumAlgorithm,
    putObjectResponse_objectChecksum,
    putObjectResponse_objectChecksumAlgorithm,
  )
where

import Amazonka.BackupStorage.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutObject' smart constructor.
data PutObject = PutObject'
  { -- | Inline chunk checksum
    inlineChunkChecksum :: Prelude.Maybe Prelude.Text,
    -- | Inline chunk checksum algorithm
    inlineChunkChecksumAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Length of the inline chunk data.
    inlineChunkLength :: Prelude.Maybe Prelude.Integer,
    -- | Store user defined metadata like backup checksum, disk ids, restore
    -- metadata etc.
    metadataString :: Prelude.Maybe Prelude.Text,
    -- | object checksum
    objectChecksum :: Prelude.Maybe Prelude.Text,
    -- | object checksum algorithm
    objectChecksumAlgorithm :: Prelude.Maybe SummaryChecksumAlgorithm,
    -- | Throw an exception if Object name is already exist.
    throwOnDuplicate :: Prelude.Maybe Prelude.Bool,
    -- | Backup job Id for the in-progress backup.
    backupJobId :: Prelude.Text,
    -- | The name of the Object to be uploaded.
    objectName :: Prelude.Text,
    -- | Inline chunk data to be uploaded.
    inlineChunk :: Data.HashedBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inlineChunkChecksum', 'putObject_inlineChunkChecksum' - Inline chunk checksum
--
-- 'inlineChunkChecksumAlgorithm', 'putObject_inlineChunkChecksumAlgorithm' - Inline chunk checksum algorithm
--
-- 'inlineChunkLength', 'putObject_inlineChunkLength' - Length of the inline chunk data.
--
-- 'metadataString', 'putObject_metadataString' - Store user defined metadata like backup checksum, disk ids, restore
-- metadata etc.
--
-- 'objectChecksum', 'putObject_objectChecksum' - object checksum
--
-- 'objectChecksumAlgorithm', 'putObject_objectChecksumAlgorithm' - object checksum algorithm
--
-- 'throwOnDuplicate', 'putObject_throwOnDuplicate' - Throw an exception if Object name is already exist.
--
-- 'backupJobId', 'putObject_backupJobId' - Backup job Id for the in-progress backup.
--
-- 'objectName', 'putObject_objectName' - The name of the Object to be uploaded.
--
-- 'inlineChunk', 'putObject_inlineChunk' - Inline chunk data to be uploaded.
newPutObject ::
  -- | 'backupJobId'
  Prelude.Text ->
  -- | 'objectName'
  Prelude.Text ->
  -- | 'inlineChunk'
  Data.HashedBody ->
  PutObject
newPutObject pBackupJobId_ pObjectName_ pInlineChunk_ =
  PutObject'
    { inlineChunkChecksum = Prelude.Nothing,
      inlineChunkChecksumAlgorithm = Prelude.Nothing,
      inlineChunkLength = Prelude.Nothing,
      metadataString = Prelude.Nothing,
      objectChecksum = Prelude.Nothing,
      objectChecksumAlgorithm = Prelude.Nothing,
      throwOnDuplicate = Prelude.Nothing,
      backupJobId = pBackupJobId_,
      objectName = pObjectName_,
      inlineChunk = pInlineChunk_
    }

-- | Inline chunk checksum
putObject_inlineChunkChecksum :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_inlineChunkChecksum = Lens.lens (\PutObject' {inlineChunkChecksum} -> inlineChunkChecksum) (\s@PutObject' {} a -> s {inlineChunkChecksum = a} :: PutObject)

-- | Inline chunk checksum algorithm
putObject_inlineChunkChecksumAlgorithm :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_inlineChunkChecksumAlgorithm = Lens.lens (\PutObject' {inlineChunkChecksumAlgorithm} -> inlineChunkChecksumAlgorithm) (\s@PutObject' {} a -> s {inlineChunkChecksumAlgorithm = a} :: PutObject)

-- | Length of the inline chunk data.
putObject_inlineChunkLength :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Integer)
putObject_inlineChunkLength = Lens.lens (\PutObject' {inlineChunkLength} -> inlineChunkLength) (\s@PutObject' {} a -> s {inlineChunkLength = a} :: PutObject)

-- | Store user defined metadata like backup checksum, disk ids, restore
-- metadata etc.
putObject_metadataString :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_metadataString = Lens.lens (\PutObject' {metadataString} -> metadataString) (\s@PutObject' {} a -> s {metadataString = a} :: PutObject)

-- | object checksum
putObject_objectChecksum :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Text)
putObject_objectChecksum = Lens.lens (\PutObject' {objectChecksum} -> objectChecksum) (\s@PutObject' {} a -> s {objectChecksum = a} :: PutObject)

-- | object checksum algorithm
putObject_objectChecksumAlgorithm :: Lens.Lens' PutObject (Prelude.Maybe SummaryChecksumAlgorithm)
putObject_objectChecksumAlgorithm = Lens.lens (\PutObject' {objectChecksumAlgorithm} -> objectChecksumAlgorithm) (\s@PutObject' {} a -> s {objectChecksumAlgorithm = a} :: PutObject)

-- | Throw an exception if Object name is already exist.
putObject_throwOnDuplicate :: Lens.Lens' PutObject (Prelude.Maybe Prelude.Bool)
putObject_throwOnDuplicate = Lens.lens (\PutObject' {throwOnDuplicate} -> throwOnDuplicate) (\s@PutObject' {} a -> s {throwOnDuplicate = a} :: PutObject)

-- | Backup job Id for the in-progress backup.
putObject_backupJobId :: Lens.Lens' PutObject Prelude.Text
putObject_backupJobId = Lens.lens (\PutObject' {backupJobId} -> backupJobId) (\s@PutObject' {} a -> s {backupJobId = a} :: PutObject)

-- | The name of the Object to be uploaded.
putObject_objectName :: Lens.Lens' PutObject Prelude.Text
putObject_objectName = Lens.lens (\PutObject' {objectName} -> objectName) (\s@PutObject' {} a -> s {objectName = a} :: PutObject)

-- | Inline chunk data to be uploaded.
putObject_inlineChunk :: Lens.Lens' PutObject Data.HashedBody
putObject_inlineChunk = Lens.lens (\PutObject' {inlineChunk} -> inlineChunk) (\s@PutObject' {} a -> s {inlineChunk = a} :: PutObject)

instance Core.AWSRequest PutObject where
  type AWSResponse PutObject = PutObjectResponse
  request overrides =
    Request.putBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutObjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "InlineChunkChecksum")
            Prelude.<*> (x Data..:> "InlineChunkChecksumAlgorithm")
            Prelude.<*> (x Data..:> "ObjectChecksum")
            Prelude.<*> (x Data..:> "ObjectChecksumAlgorithm")
      )

instance Data.ToBody PutObject where
  toBody PutObject' {..} = Data.toBody inlineChunk

instance Data.ToHeaders PutObject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath PutObject where
  toPath PutObject' {..} =
    Prelude.mconcat
      [ "/backup-jobs/",
        Data.toBS backupJobId,
        "/object/",
        Data.toBS objectName,
        "/put-object"
      ]

instance Data.ToQuery PutObject where
  toQuery PutObject' {..} =
    Prelude.mconcat
      [ "checksum" Data.=: inlineChunkChecksum,
        "checksum-algorithm"
          Data.=: inlineChunkChecksumAlgorithm,
        "length" Data.=: inlineChunkLength,
        "metadata-string" Data.=: metadataString,
        "object-checksum" Data.=: objectChecksum,
        "object-checksum-algorithm"
          Data.=: objectChecksumAlgorithm,
        "throwOnDuplicate" Data.=: throwOnDuplicate
      ]

-- | /See:/ 'newPutObjectResponse' smart constructor.
data PutObjectResponse = PutObjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Inline chunk checksum
    inlineChunkChecksum :: Prelude.Text,
    -- | Inline chunk checksum algorithm
    inlineChunkChecksumAlgorithm :: DataChecksumAlgorithm,
    -- | object checksum
    objectChecksum :: Prelude.Text,
    -- | object checksum algorithm
    objectChecksumAlgorithm :: SummaryChecksumAlgorithm
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putObjectResponse_httpStatus' - The response's http status code.
--
-- 'inlineChunkChecksum', 'putObjectResponse_inlineChunkChecksum' - Inline chunk checksum
--
-- 'inlineChunkChecksumAlgorithm', 'putObjectResponse_inlineChunkChecksumAlgorithm' - Inline chunk checksum algorithm
--
-- 'objectChecksum', 'putObjectResponse_objectChecksum' - object checksum
--
-- 'objectChecksumAlgorithm', 'putObjectResponse_objectChecksumAlgorithm' - object checksum algorithm
newPutObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'inlineChunkChecksum'
  Prelude.Text ->
  -- | 'inlineChunkChecksumAlgorithm'
  DataChecksumAlgorithm ->
  -- | 'objectChecksum'
  Prelude.Text ->
  -- | 'objectChecksumAlgorithm'
  SummaryChecksumAlgorithm ->
  PutObjectResponse
newPutObjectResponse
  pHttpStatus_
  pInlineChunkChecksum_
  pInlineChunkChecksumAlgorithm_
  pObjectChecksum_
  pObjectChecksumAlgorithm_ =
    PutObjectResponse'
      { httpStatus = pHttpStatus_,
        inlineChunkChecksum = pInlineChunkChecksum_,
        inlineChunkChecksumAlgorithm =
          pInlineChunkChecksumAlgorithm_,
        objectChecksum = pObjectChecksum_,
        objectChecksumAlgorithm = pObjectChecksumAlgorithm_
      }

-- | The response's http status code.
putObjectResponse_httpStatus :: Lens.Lens' PutObjectResponse Prelude.Int
putObjectResponse_httpStatus = Lens.lens (\PutObjectResponse' {httpStatus} -> httpStatus) (\s@PutObjectResponse' {} a -> s {httpStatus = a} :: PutObjectResponse)

-- | Inline chunk checksum
putObjectResponse_inlineChunkChecksum :: Lens.Lens' PutObjectResponse Prelude.Text
putObjectResponse_inlineChunkChecksum = Lens.lens (\PutObjectResponse' {inlineChunkChecksum} -> inlineChunkChecksum) (\s@PutObjectResponse' {} a -> s {inlineChunkChecksum = a} :: PutObjectResponse)

-- | Inline chunk checksum algorithm
putObjectResponse_inlineChunkChecksumAlgorithm :: Lens.Lens' PutObjectResponse DataChecksumAlgorithm
putObjectResponse_inlineChunkChecksumAlgorithm = Lens.lens (\PutObjectResponse' {inlineChunkChecksumAlgorithm} -> inlineChunkChecksumAlgorithm) (\s@PutObjectResponse' {} a -> s {inlineChunkChecksumAlgorithm = a} :: PutObjectResponse)

-- | object checksum
putObjectResponse_objectChecksum :: Lens.Lens' PutObjectResponse Prelude.Text
putObjectResponse_objectChecksum = Lens.lens (\PutObjectResponse' {objectChecksum} -> objectChecksum) (\s@PutObjectResponse' {} a -> s {objectChecksum = a} :: PutObjectResponse)

-- | object checksum algorithm
putObjectResponse_objectChecksumAlgorithm :: Lens.Lens' PutObjectResponse SummaryChecksumAlgorithm
putObjectResponse_objectChecksumAlgorithm = Lens.lens (\PutObjectResponse' {objectChecksumAlgorithm} -> objectChecksumAlgorithm) (\s@PutObjectResponse' {} a -> s {objectChecksumAlgorithm = a} :: PutObjectResponse)

instance Prelude.NFData PutObjectResponse where
  rnf PutObjectResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf inlineChunkChecksum
      `Prelude.seq` Prelude.rnf inlineChunkChecksumAlgorithm
      `Prelude.seq` Prelude.rnf objectChecksum
      `Prelude.seq` Prelude.rnf objectChecksumAlgorithm
