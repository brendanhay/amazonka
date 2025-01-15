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
-- Module      : Amazonka.Omics.GetReferenceMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a genome reference\'s metadata.
module Amazonka.Omics.GetReferenceMetadata
  ( -- * Creating a Request
    GetReferenceMetadata (..),
    newGetReferenceMetadata,

    -- * Request Lenses
    getReferenceMetadata_id,
    getReferenceMetadata_referenceStoreId,

    -- * Destructuring the Response
    GetReferenceMetadataResponse (..),
    newGetReferenceMetadataResponse,

    -- * Response Lenses
    getReferenceMetadataResponse_description,
    getReferenceMetadataResponse_files,
    getReferenceMetadataResponse_name,
    getReferenceMetadataResponse_status,
    getReferenceMetadataResponse_httpStatus,
    getReferenceMetadataResponse_arn,
    getReferenceMetadataResponse_creationTime,
    getReferenceMetadataResponse_id,
    getReferenceMetadataResponse_md5,
    getReferenceMetadataResponse_referenceStoreId,
    getReferenceMetadataResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetReferenceMetadata' smart constructor.
data GetReferenceMetadata = GetReferenceMetadata'
  { -- | The reference\'s ID.
    id :: Prelude.Text,
    -- | The reference\'s reference store ID.
    referenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReferenceMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getReferenceMetadata_id' - The reference\'s ID.
--
-- 'referenceStoreId', 'getReferenceMetadata_referenceStoreId' - The reference\'s reference store ID.
newGetReferenceMetadata ::
  -- | 'id'
  Prelude.Text ->
  -- | 'referenceStoreId'
  Prelude.Text ->
  GetReferenceMetadata
newGetReferenceMetadata pId_ pReferenceStoreId_ =
  GetReferenceMetadata'
    { id = pId_,
      referenceStoreId = pReferenceStoreId_
    }

-- | The reference\'s ID.
getReferenceMetadata_id :: Lens.Lens' GetReferenceMetadata Prelude.Text
getReferenceMetadata_id = Lens.lens (\GetReferenceMetadata' {id} -> id) (\s@GetReferenceMetadata' {} a -> s {id = a} :: GetReferenceMetadata)

-- | The reference\'s reference store ID.
getReferenceMetadata_referenceStoreId :: Lens.Lens' GetReferenceMetadata Prelude.Text
getReferenceMetadata_referenceStoreId = Lens.lens (\GetReferenceMetadata' {referenceStoreId} -> referenceStoreId) (\s@GetReferenceMetadata' {} a -> s {referenceStoreId = a} :: GetReferenceMetadata)

instance Core.AWSRequest GetReferenceMetadata where
  type
    AWSResponse GetReferenceMetadata =
      GetReferenceMetadataResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReferenceMetadataResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "files")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "md5")
            Prelude.<*> (x Data..:> "referenceStoreId")
            Prelude.<*> (x Data..:> "updateTime")
      )

instance Prelude.Hashable GetReferenceMetadata where
  hashWithSalt _salt GetReferenceMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` referenceStoreId

instance Prelude.NFData GetReferenceMetadata where
  rnf GetReferenceMetadata' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf referenceStoreId

instance Data.ToHeaders GetReferenceMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetReferenceMetadata where
  toPath GetReferenceMetadata' {..} =
    Prelude.mconcat
      [ "/referencestore/",
        Data.toBS referenceStoreId,
        "/reference/",
        Data.toBS id,
        "/metadata"
      ]

instance Data.ToQuery GetReferenceMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReferenceMetadataResponse' smart constructor.
data GetReferenceMetadataResponse = GetReferenceMetadataResponse'
  { -- | The reference\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The reference\'s files.
    files :: Prelude.Maybe ReferenceFiles,
    -- | The reference\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The reference\'s status.
    status :: Prelude.Maybe ReferenceStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The reference\'s ARN.
    arn :: Prelude.Text,
    -- | When the reference was created.
    creationTime :: Data.ISO8601,
    -- | The reference\'s ID.
    id :: Prelude.Text,
    -- | The reference\'s MD5 checksum.
    md5 :: Prelude.Text,
    -- | The reference\'s reference store ID.
    referenceStoreId :: Prelude.Text,
    -- | When the reference was updated.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReferenceMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getReferenceMetadataResponse_description' - The reference\'s description.
--
-- 'files', 'getReferenceMetadataResponse_files' - The reference\'s files.
--
-- 'name', 'getReferenceMetadataResponse_name' - The reference\'s name.
--
-- 'status', 'getReferenceMetadataResponse_status' - The reference\'s status.
--
-- 'httpStatus', 'getReferenceMetadataResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getReferenceMetadataResponse_arn' - The reference\'s ARN.
--
-- 'creationTime', 'getReferenceMetadataResponse_creationTime' - When the reference was created.
--
-- 'id', 'getReferenceMetadataResponse_id' - The reference\'s ID.
--
-- 'md5', 'getReferenceMetadataResponse_md5' - The reference\'s MD5 checksum.
--
-- 'referenceStoreId', 'getReferenceMetadataResponse_referenceStoreId' - The reference\'s reference store ID.
--
-- 'updateTime', 'getReferenceMetadataResponse_updateTime' - When the reference was updated.
newGetReferenceMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  -- | 'md5'
  Prelude.Text ->
  -- | 'referenceStoreId'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  GetReferenceMetadataResponse
newGetReferenceMetadataResponse
  pHttpStatus_
  pArn_
  pCreationTime_
  pId_
  pMd5_
  pReferenceStoreId_
  pUpdateTime_ =
    GetReferenceMetadataResponse'
      { description =
          Prelude.Nothing,
        files = Prelude.Nothing,
        name = Prelude.Nothing,
        status = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        id = pId_,
        md5 = pMd5_,
        referenceStoreId = pReferenceStoreId_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The reference\'s description.
getReferenceMetadataResponse_description :: Lens.Lens' GetReferenceMetadataResponse (Prelude.Maybe Prelude.Text)
getReferenceMetadataResponse_description = Lens.lens (\GetReferenceMetadataResponse' {description} -> description) (\s@GetReferenceMetadataResponse' {} a -> s {description = a} :: GetReferenceMetadataResponse)

-- | The reference\'s files.
getReferenceMetadataResponse_files :: Lens.Lens' GetReferenceMetadataResponse (Prelude.Maybe ReferenceFiles)
getReferenceMetadataResponse_files = Lens.lens (\GetReferenceMetadataResponse' {files} -> files) (\s@GetReferenceMetadataResponse' {} a -> s {files = a} :: GetReferenceMetadataResponse)

-- | The reference\'s name.
getReferenceMetadataResponse_name :: Lens.Lens' GetReferenceMetadataResponse (Prelude.Maybe Prelude.Text)
getReferenceMetadataResponse_name = Lens.lens (\GetReferenceMetadataResponse' {name} -> name) (\s@GetReferenceMetadataResponse' {} a -> s {name = a} :: GetReferenceMetadataResponse)

-- | The reference\'s status.
getReferenceMetadataResponse_status :: Lens.Lens' GetReferenceMetadataResponse (Prelude.Maybe ReferenceStatus)
getReferenceMetadataResponse_status = Lens.lens (\GetReferenceMetadataResponse' {status} -> status) (\s@GetReferenceMetadataResponse' {} a -> s {status = a} :: GetReferenceMetadataResponse)

-- | The response's http status code.
getReferenceMetadataResponse_httpStatus :: Lens.Lens' GetReferenceMetadataResponse Prelude.Int
getReferenceMetadataResponse_httpStatus = Lens.lens (\GetReferenceMetadataResponse' {httpStatus} -> httpStatus) (\s@GetReferenceMetadataResponse' {} a -> s {httpStatus = a} :: GetReferenceMetadataResponse)

-- | The reference\'s ARN.
getReferenceMetadataResponse_arn :: Lens.Lens' GetReferenceMetadataResponse Prelude.Text
getReferenceMetadataResponse_arn = Lens.lens (\GetReferenceMetadataResponse' {arn} -> arn) (\s@GetReferenceMetadataResponse' {} a -> s {arn = a} :: GetReferenceMetadataResponse)

-- | When the reference was created.
getReferenceMetadataResponse_creationTime :: Lens.Lens' GetReferenceMetadataResponse Prelude.UTCTime
getReferenceMetadataResponse_creationTime = Lens.lens (\GetReferenceMetadataResponse' {creationTime} -> creationTime) (\s@GetReferenceMetadataResponse' {} a -> s {creationTime = a} :: GetReferenceMetadataResponse) Prelude.. Data._Time

-- | The reference\'s ID.
getReferenceMetadataResponse_id :: Lens.Lens' GetReferenceMetadataResponse Prelude.Text
getReferenceMetadataResponse_id = Lens.lens (\GetReferenceMetadataResponse' {id} -> id) (\s@GetReferenceMetadataResponse' {} a -> s {id = a} :: GetReferenceMetadataResponse)

-- | The reference\'s MD5 checksum.
getReferenceMetadataResponse_md5 :: Lens.Lens' GetReferenceMetadataResponse Prelude.Text
getReferenceMetadataResponse_md5 = Lens.lens (\GetReferenceMetadataResponse' {md5} -> md5) (\s@GetReferenceMetadataResponse' {} a -> s {md5 = a} :: GetReferenceMetadataResponse)

-- | The reference\'s reference store ID.
getReferenceMetadataResponse_referenceStoreId :: Lens.Lens' GetReferenceMetadataResponse Prelude.Text
getReferenceMetadataResponse_referenceStoreId = Lens.lens (\GetReferenceMetadataResponse' {referenceStoreId} -> referenceStoreId) (\s@GetReferenceMetadataResponse' {} a -> s {referenceStoreId = a} :: GetReferenceMetadataResponse)

-- | When the reference was updated.
getReferenceMetadataResponse_updateTime :: Lens.Lens' GetReferenceMetadataResponse Prelude.UTCTime
getReferenceMetadataResponse_updateTime = Lens.lens (\GetReferenceMetadataResponse' {updateTime} -> updateTime) (\s@GetReferenceMetadataResponse' {} a -> s {updateTime = a} :: GetReferenceMetadataResponse) Prelude.. Data._Time

instance Prelude.NFData GetReferenceMetadataResponse where
  rnf GetReferenceMetadataResponse' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf files `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf status `Prelude.seq`
            Prelude.rnf httpStatus `Prelude.seq`
              Prelude.rnf arn `Prelude.seq`
                Prelude.rnf creationTime `Prelude.seq`
                  Prelude.rnf id `Prelude.seq`
                    Prelude.rnf md5 `Prelude.seq`
                      Prelude.rnf referenceStoreId `Prelude.seq`
                        Prelude.rnf updateTime
