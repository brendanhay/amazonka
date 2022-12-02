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
-- Module      : Amazonka.BackupStorage.GetObjectMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get metadata associated with an Object.
module Amazonka.BackupStorage.GetObjectMetadata
  ( -- * Creating a Request
    GetObjectMetadata (..),
    newGetObjectMetadata,

    -- * Request Lenses
    getObjectMetadata_storageJobId,
    getObjectMetadata_objectToken,

    -- * Destructuring the Response
    GetObjectMetadataResponse (..),
    newGetObjectMetadataResponse,

    -- * Response Lenses
    getObjectMetadataResponse_metadataString,
    getObjectMetadataResponse_metadataBlobChecksum,
    getObjectMetadataResponse_metadataBlobChecksumAlgorithm,
    getObjectMetadataResponse_metadataBlobLength,
    getObjectMetadataResponse_httpStatus,
    getObjectMetadataResponse_metadataBlob,
  )
where

import Amazonka.BackupStorage.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetObjectMetadata' smart constructor.
data GetObjectMetadata = GetObjectMetadata'
  { -- | Backup job id for the in-progress backup.
    storageJobId :: Prelude.Text,
    -- | Object token.
    objectToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageJobId', 'getObjectMetadata_storageJobId' - Backup job id for the in-progress backup.
--
-- 'objectToken', 'getObjectMetadata_objectToken' - Object token.
newGetObjectMetadata ::
  -- | 'storageJobId'
  Prelude.Text ->
  -- | 'objectToken'
  Prelude.Text ->
  GetObjectMetadata
newGetObjectMetadata pStorageJobId_ pObjectToken_ =
  GetObjectMetadata'
    { storageJobId = pStorageJobId_,
      objectToken = pObjectToken_
    }

-- | Backup job id for the in-progress backup.
getObjectMetadata_storageJobId :: Lens.Lens' GetObjectMetadata Prelude.Text
getObjectMetadata_storageJobId = Lens.lens (\GetObjectMetadata' {storageJobId} -> storageJobId) (\s@GetObjectMetadata' {} a -> s {storageJobId = a} :: GetObjectMetadata)

-- | Object token.
getObjectMetadata_objectToken :: Lens.Lens' GetObjectMetadata Prelude.Text
getObjectMetadata_objectToken = Lens.lens (\GetObjectMetadata' {objectToken} -> objectToken) (\s@GetObjectMetadata' {} a -> s {objectToken = a} :: GetObjectMetadata)

instance Core.AWSRequest GetObjectMetadata where
  type
    AWSResponse GetObjectMetadata =
      GetObjectMetadataResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          GetObjectMetadataResponse'
            Prelude.<$> (h Data..#? "x-amz-metadata-string")
            Prelude.<*> (h Data..#? "x-amz-checksum")
            Prelude.<*> (h Data..#? "x-amz-checksum-algorithm")
            Prelude.<*> (h Data..#? "x-amz-data-length")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetObjectMetadata where
  hashWithSalt _salt GetObjectMetadata' {..} =
    _salt `Prelude.hashWithSalt` storageJobId
      `Prelude.hashWithSalt` objectToken

instance Prelude.NFData GetObjectMetadata where
  rnf GetObjectMetadata' {..} =
    Prelude.rnf storageJobId
      `Prelude.seq` Prelude.rnf objectToken

instance Data.ToHeaders GetObjectMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetObjectMetadata where
  toPath GetObjectMetadata' {..} =
    Prelude.mconcat
      [ "/restore-jobs/",
        Data.toBS storageJobId,
        "/object/",
        Data.toBS objectToken,
        "/metadata"
      ]

instance Data.ToQuery GetObjectMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetObjectMetadataResponse' smart constructor.
data GetObjectMetadataResponse = GetObjectMetadataResponse'
  { -- | Metadata string.
    metadataString :: Prelude.Maybe Prelude.Text,
    -- | MetadataBlob checksum.
    metadataBlobChecksum :: Prelude.Maybe Prelude.Text,
    -- | Checksum algorithm.
    metadataBlobChecksumAlgorithm :: Prelude.Maybe DataChecksumAlgorithm,
    -- | The size of MetadataBlob.
    metadataBlobLength :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Metadata blob.
    metadataBlob :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadataString', 'getObjectMetadataResponse_metadataString' - Metadata string.
--
-- 'metadataBlobChecksum', 'getObjectMetadataResponse_metadataBlobChecksum' - MetadataBlob checksum.
--
-- 'metadataBlobChecksumAlgorithm', 'getObjectMetadataResponse_metadataBlobChecksumAlgorithm' - Checksum algorithm.
--
-- 'metadataBlobLength', 'getObjectMetadataResponse_metadataBlobLength' - The size of MetadataBlob.
--
-- 'httpStatus', 'getObjectMetadataResponse_httpStatus' - The response's http status code.
--
-- 'metadataBlob', 'getObjectMetadataResponse_metadataBlob' - Metadata blob.
newGetObjectMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'metadataBlob'
  Data.ResponseBody ->
  GetObjectMetadataResponse
newGetObjectMetadataResponse
  pHttpStatus_
  pMetadataBlob_ =
    GetObjectMetadataResponse'
      { metadataString =
          Prelude.Nothing,
        metadataBlobChecksum = Prelude.Nothing,
        metadataBlobChecksumAlgorithm = Prelude.Nothing,
        metadataBlobLength = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        metadataBlob = pMetadataBlob_
      }

-- | Metadata string.
getObjectMetadataResponse_metadataString :: Lens.Lens' GetObjectMetadataResponse (Prelude.Maybe Prelude.Text)
getObjectMetadataResponse_metadataString = Lens.lens (\GetObjectMetadataResponse' {metadataString} -> metadataString) (\s@GetObjectMetadataResponse' {} a -> s {metadataString = a} :: GetObjectMetadataResponse)

-- | MetadataBlob checksum.
getObjectMetadataResponse_metadataBlobChecksum :: Lens.Lens' GetObjectMetadataResponse (Prelude.Maybe Prelude.Text)
getObjectMetadataResponse_metadataBlobChecksum = Lens.lens (\GetObjectMetadataResponse' {metadataBlobChecksum} -> metadataBlobChecksum) (\s@GetObjectMetadataResponse' {} a -> s {metadataBlobChecksum = a} :: GetObjectMetadataResponse)

-- | Checksum algorithm.
getObjectMetadataResponse_metadataBlobChecksumAlgorithm :: Lens.Lens' GetObjectMetadataResponse (Prelude.Maybe DataChecksumAlgorithm)
getObjectMetadataResponse_metadataBlobChecksumAlgorithm = Lens.lens (\GetObjectMetadataResponse' {metadataBlobChecksumAlgorithm} -> metadataBlobChecksumAlgorithm) (\s@GetObjectMetadataResponse' {} a -> s {metadataBlobChecksumAlgorithm = a} :: GetObjectMetadataResponse)

-- | The size of MetadataBlob.
getObjectMetadataResponse_metadataBlobLength :: Lens.Lens' GetObjectMetadataResponse (Prelude.Maybe Prelude.Integer)
getObjectMetadataResponse_metadataBlobLength = Lens.lens (\GetObjectMetadataResponse' {metadataBlobLength} -> metadataBlobLength) (\s@GetObjectMetadataResponse' {} a -> s {metadataBlobLength = a} :: GetObjectMetadataResponse)

-- | The response's http status code.
getObjectMetadataResponse_httpStatus :: Lens.Lens' GetObjectMetadataResponse Prelude.Int
getObjectMetadataResponse_httpStatus = Lens.lens (\GetObjectMetadataResponse' {httpStatus} -> httpStatus) (\s@GetObjectMetadataResponse' {} a -> s {httpStatus = a} :: GetObjectMetadataResponse)

-- | Metadata blob.
getObjectMetadataResponse_metadataBlob :: Lens.Lens' GetObjectMetadataResponse Data.ResponseBody
getObjectMetadataResponse_metadataBlob = Lens.lens (\GetObjectMetadataResponse' {metadataBlob} -> metadataBlob) (\s@GetObjectMetadataResponse' {} a -> s {metadataBlob = a} :: GetObjectMetadataResponse)
