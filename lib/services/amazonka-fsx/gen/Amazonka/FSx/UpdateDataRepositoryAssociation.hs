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
-- Module      : Amazonka.FSx.UpdateDataRepositoryAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of an existing data repository association on
-- an Amazon FSx for Lustre file system. Data repository associations are
-- supported only for file systems with the @Persistent_2@ deployment type.
module Amazonka.FSx.UpdateDataRepositoryAssociation
  ( -- * Creating a Request
    UpdateDataRepositoryAssociation (..),
    newUpdateDataRepositoryAssociation,

    -- * Request Lenses
    updateDataRepositoryAssociation_clientRequestToken,
    updateDataRepositoryAssociation_importedFileChunkSize,
    updateDataRepositoryAssociation_s3,
    updateDataRepositoryAssociation_associationId,

    -- * Destructuring the Response
    UpdateDataRepositoryAssociationResponse (..),
    newUpdateDataRepositoryAssociationResponse,

    -- * Response Lenses
    updateDataRepositoryAssociationResponse_association,
    updateDataRepositoryAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDataRepositoryAssociation' smart constructor.
data UpdateDataRepositoryAssociation = UpdateDataRepositoryAssociation'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | For files imported from a data repository, this value determines the
    -- stripe count and maximum amount of data per file (in MiB) stored on a
    -- single physical disk. The maximum number of disks that a single file can
    -- be striped across is limited by the total number of disks that make up
    -- the file system.
    --
    -- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
    -- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
    importedFileChunkSize :: Prelude.Maybe Prelude.Natural,
    -- | The configuration for an Amazon S3 data repository linked to an Amazon
    -- FSx Lustre file system with a data repository association. The
    -- configuration defines which file events (new, changed, or deleted files
    -- or directories) are automatically imported from the linked data
    -- repository to the file system or automatically exported from the file
    -- system to the data repository.
    s3 :: Prelude.Maybe S3DataRepositoryConfiguration,
    -- | The ID of the data repository association that you are updating.
    associationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataRepositoryAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'updateDataRepositoryAssociation_clientRequestToken' - Undocumented member.
--
-- 'importedFileChunkSize', 'updateDataRepositoryAssociation_importedFileChunkSize' - For files imported from a data repository, this value determines the
-- stripe count and maximum amount of data per file (in MiB) stored on a
-- single physical disk. The maximum number of disks that a single file can
-- be striped across is limited by the total number of disks that make up
-- the file system.
--
-- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
-- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
--
-- 's3', 'updateDataRepositoryAssociation_s3' - The configuration for an Amazon S3 data repository linked to an Amazon
-- FSx Lustre file system with a data repository association. The
-- configuration defines which file events (new, changed, or deleted files
-- or directories) are automatically imported from the linked data
-- repository to the file system or automatically exported from the file
-- system to the data repository.
--
-- 'associationId', 'updateDataRepositoryAssociation_associationId' - The ID of the data repository association that you are updating.
newUpdateDataRepositoryAssociation ::
  -- | 'associationId'
  Prelude.Text ->
  UpdateDataRepositoryAssociation
newUpdateDataRepositoryAssociation pAssociationId_ =
  UpdateDataRepositoryAssociation'
    { clientRequestToken =
        Prelude.Nothing,
      importedFileChunkSize = Prelude.Nothing,
      s3 = Prelude.Nothing,
      associationId = pAssociationId_
    }

-- | Undocumented member.
updateDataRepositoryAssociation_clientRequestToken :: Lens.Lens' UpdateDataRepositoryAssociation (Prelude.Maybe Prelude.Text)
updateDataRepositoryAssociation_clientRequestToken = Lens.lens (\UpdateDataRepositoryAssociation' {clientRequestToken} -> clientRequestToken) (\s@UpdateDataRepositoryAssociation' {} a -> s {clientRequestToken = a} :: UpdateDataRepositoryAssociation)

-- | For files imported from a data repository, this value determines the
-- stripe count and maximum amount of data per file (in MiB) stored on a
-- single physical disk. The maximum number of disks that a single file can
-- be striped across is limited by the total number of disks that make up
-- the file system.
--
-- The default chunk size is 1,024 MiB (1 GiB) and can go as high as
-- 512,000 MiB (500 GiB). Amazon S3 objects have a maximum size of 5 TB.
updateDataRepositoryAssociation_importedFileChunkSize :: Lens.Lens' UpdateDataRepositoryAssociation (Prelude.Maybe Prelude.Natural)
updateDataRepositoryAssociation_importedFileChunkSize = Lens.lens (\UpdateDataRepositoryAssociation' {importedFileChunkSize} -> importedFileChunkSize) (\s@UpdateDataRepositoryAssociation' {} a -> s {importedFileChunkSize = a} :: UpdateDataRepositoryAssociation)

-- | The configuration for an Amazon S3 data repository linked to an Amazon
-- FSx Lustre file system with a data repository association. The
-- configuration defines which file events (new, changed, or deleted files
-- or directories) are automatically imported from the linked data
-- repository to the file system or automatically exported from the file
-- system to the data repository.
updateDataRepositoryAssociation_s3 :: Lens.Lens' UpdateDataRepositoryAssociation (Prelude.Maybe S3DataRepositoryConfiguration)
updateDataRepositoryAssociation_s3 = Lens.lens (\UpdateDataRepositoryAssociation' {s3} -> s3) (\s@UpdateDataRepositoryAssociation' {} a -> s {s3 = a} :: UpdateDataRepositoryAssociation)

-- | The ID of the data repository association that you are updating.
updateDataRepositoryAssociation_associationId :: Lens.Lens' UpdateDataRepositoryAssociation Prelude.Text
updateDataRepositoryAssociation_associationId = Lens.lens (\UpdateDataRepositoryAssociation' {associationId} -> associationId) (\s@UpdateDataRepositoryAssociation' {} a -> s {associationId = a} :: UpdateDataRepositoryAssociation)

instance
  Core.AWSRequest
    UpdateDataRepositoryAssociation
  where
  type
    AWSResponse UpdateDataRepositoryAssociation =
      UpdateDataRepositoryAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDataRepositoryAssociationResponse'
            Prelude.<$> (x Data..?> "Association")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateDataRepositoryAssociation
  where
  hashWithSalt
    _salt
    UpdateDataRepositoryAssociation' {..} =
      _salt `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` importedFileChunkSize
        `Prelude.hashWithSalt` s3
        `Prelude.hashWithSalt` associationId

instance
  Prelude.NFData
    UpdateDataRepositoryAssociation
  where
  rnf UpdateDataRepositoryAssociation' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf importedFileChunkSize
      `Prelude.seq` Prelude.rnf s3
      `Prelude.seq` Prelude.rnf associationId

instance
  Data.ToHeaders
    UpdateDataRepositoryAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.UpdateDataRepositoryAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDataRepositoryAssociation where
  toJSON UpdateDataRepositoryAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("ImportedFileChunkSize" Data..=)
              Prelude.<$> importedFileChunkSize,
            ("S3" Data..=) Prelude.<$> s3,
            Prelude.Just
              ("AssociationId" Data..= associationId)
          ]
      )

instance Data.ToPath UpdateDataRepositoryAssociation where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDataRepositoryAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataRepositoryAssociationResponse' smart constructor.
data UpdateDataRepositoryAssociationResponse = UpdateDataRepositoryAssociationResponse'
  { -- | The response object returned after the data repository association is
    -- updated.
    association :: Prelude.Maybe DataRepositoryAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataRepositoryAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'association', 'updateDataRepositoryAssociationResponse_association' - The response object returned after the data repository association is
-- updated.
--
-- 'httpStatus', 'updateDataRepositoryAssociationResponse_httpStatus' - The response's http status code.
newUpdateDataRepositoryAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDataRepositoryAssociationResponse
newUpdateDataRepositoryAssociationResponse
  pHttpStatus_ =
    UpdateDataRepositoryAssociationResponse'
      { association =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The response object returned after the data repository association is
-- updated.
updateDataRepositoryAssociationResponse_association :: Lens.Lens' UpdateDataRepositoryAssociationResponse (Prelude.Maybe DataRepositoryAssociation)
updateDataRepositoryAssociationResponse_association = Lens.lens (\UpdateDataRepositoryAssociationResponse' {association} -> association) (\s@UpdateDataRepositoryAssociationResponse' {} a -> s {association = a} :: UpdateDataRepositoryAssociationResponse)

-- | The response's http status code.
updateDataRepositoryAssociationResponse_httpStatus :: Lens.Lens' UpdateDataRepositoryAssociationResponse Prelude.Int
updateDataRepositoryAssociationResponse_httpStatus = Lens.lens (\UpdateDataRepositoryAssociationResponse' {httpStatus} -> httpStatus) (\s@UpdateDataRepositoryAssociationResponse' {} a -> s {httpStatus = a} :: UpdateDataRepositoryAssociationResponse)

instance
  Prelude.NFData
    UpdateDataRepositoryAssociationResponse
  where
  rnf UpdateDataRepositoryAssociationResponse' {..} =
    Prelude.rnf association
      `Prelude.seq` Prelude.rnf httpStatus
