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
-- Module      : Amazonka.Glacier.InitiateMultipartUpload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates a multipart upload. Amazon S3 Glacier creates a
-- multipart upload resource and returns its ID in the response. The
-- multipart upload ID is used in subsequent requests to upload parts of an
-- archive (see UploadMultipartPart).
--
-- When you initiate a multipart upload, you specify the part size in
-- number of bytes. The part size must be a megabyte (1024 KB) multiplied
-- by a power of 2-for example, 1048576 (1 MB), 2097152 (2 MB), 4194304 (4
-- MB), 8388608 (8 MB), and so on. The minimum allowable part size is 1 MB,
-- and the maximum is 4 GB.
--
-- Every part you upload to this resource (see UploadMultipartPart), except
-- the last one, must have the same size. The last one can be the same size
-- or smaller. For example, suppose you want to upload a 16.2 MB file. If
-- you initiate the multipart upload with a part size of 4 MB, you will
-- upload four parts of 4 MB each and one part of 0.2 MB.
--
-- You don\'t need to know the size of the archive when you start a
-- multipart upload because Amazon S3 Glacier does not require you to
-- specify the overall archive size.
--
-- After you complete the multipart upload, Amazon S3 Glacier (Glacier)
-- removes the multipart upload resource referenced by the ID. Glacier also
-- removes the multipart upload resource if you cancel the multipart upload
-- or it may be removed if there is no activity for a period of 24 hours.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html Uploading Large Archives in Parts (Multipart Upload)>
-- and
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-initiate-upload.html Initiate Multipart Upload>
-- in the /Amazon Glacier Developer Guide/.
module Amazonka.Glacier.InitiateMultipartUpload
  ( -- * Creating a Request
    InitiateMultipartUpload (..),
    newInitiateMultipartUpload,

    -- * Request Lenses
    initiateMultipartUpload_archiveDescription,
    initiateMultipartUpload_accountId,
    initiateMultipartUpload_vaultName,
    initiateMultipartUpload_partSize,

    -- * Destructuring the Response
    InitiateMultipartUploadResponse (..),
    newInitiateMultipartUploadResponse,

    -- * Response Lenses
    initiateMultipartUploadResponse_location,
    initiateMultipartUploadResponse_httpStatus,
    initiateMultipartUploadResponse_uploadId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Provides options for initiating a multipart upload to an Amazon S3
-- Glacier vault.
--
-- /See:/ 'newInitiateMultipartUpload' smart constructor.
data InitiateMultipartUpload = InitiateMultipartUpload'
  { -- | The archive description that you are uploading in parts.
    --
    -- The part size must be a megabyte (1024 KB) multiplied by a power of 2,
    -- for example 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8
    -- MB), and so on. The minimum allowable part size is 1 MB, and the maximum
    -- is 4 GB (4096 MB).
    archiveDescription :: Prelude.Maybe Prelude.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text,
    -- | The size of each part except the last, in bytes. The last part can be
    -- smaller than this part size.
    partSize :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitiateMultipartUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveDescription', 'initiateMultipartUpload_archiveDescription' - The archive description that you are uploading in parts.
--
-- The part size must be a megabyte (1024 KB) multiplied by a power of 2,
-- for example 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8
-- MB), and so on. The minimum allowable part size is 1 MB, and the maximum
-- is 4 GB (4096 MB).
--
-- 'accountId', 'initiateMultipartUpload_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'initiateMultipartUpload_vaultName' - The name of the vault.
--
-- 'partSize', 'initiateMultipartUpload_partSize' - The size of each part except the last, in bytes. The last part can be
-- smaller than this part size.
newInitiateMultipartUpload ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  -- | 'partSize'
  Prelude.Text ->
  InitiateMultipartUpload
newInitiateMultipartUpload
  pAccountId_
  pVaultName_
  pPartSize_ =
    InitiateMultipartUpload'
      { archiveDescription =
          Prelude.Nothing,
        accountId = pAccountId_,
        vaultName = pVaultName_,
        partSize = pPartSize_
      }

-- | The archive description that you are uploading in parts.
--
-- The part size must be a megabyte (1024 KB) multiplied by a power of 2,
-- for example 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8
-- MB), and so on. The minimum allowable part size is 1 MB, and the maximum
-- is 4 GB (4096 MB).
initiateMultipartUpload_archiveDescription :: Lens.Lens' InitiateMultipartUpload (Prelude.Maybe Prelude.Text)
initiateMultipartUpload_archiveDescription = Lens.lens (\InitiateMultipartUpload' {archiveDescription} -> archiveDescription) (\s@InitiateMultipartUpload' {} a -> s {archiveDescription = a} :: InitiateMultipartUpload)

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
initiateMultipartUpload_accountId :: Lens.Lens' InitiateMultipartUpload Prelude.Text
initiateMultipartUpload_accountId = Lens.lens (\InitiateMultipartUpload' {accountId} -> accountId) (\s@InitiateMultipartUpload' {} a -> s {accountId = a} :: InitiateMultipartUpload)

-- | The name of the vault.
initiateMultipartUpload_vaultName :: Lens.Lens' InitiateMultipartUpload Prelude.Text
initiateMultipartUpload_vaultName = Lens.lens (\InitiateMultipartUpload' {vaultName} -> vaultName) (\s@InitiateMultipartUpload' {} a -> s {vaultName = a} :: InitiateMultipartUpload)

-- | The size of each part except the last, in bytes. The last part can be
-- smaller than this part size.
initiateMultipartUpload_partSize :: Lens.Lens' InitiateMultipartUpload Prelude.Text
initiateMultipartUpload_partSize = Lens.lens (\InitiateMultipartUpload' {partSize} -> partSize) (\s@InitiateMultipartUpload' {} a -> s {partSize = a} :: InitiateMultipartUpload)

instance Core.AWSRequest InitiateMultipartUpload where
  type
    AWSResponse InitiateMultipartUpload =
      InitiateMultipartUploadResponse
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          InitiateMultipartUploadResponse'
            Prelude.<$> (h Data..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (h Data..# "x-amz-multipart-upload-id")
      )

instance Prelude.Hashable InitiateMultipartUpload where
  hashWithSalt _salt InitiateMultipartUpload' {..} =
    _salt
      `Prelude.hashWithSalt` archiveDescription
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vaultName
      `Prelude.hashWithSalt` partSize

instance Prelude.NFData InitiateMultipartUpload where
  rnf InitiateMultipartUpload' {..} =
    Prelude.rnf archiveDescription
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf vaultName
      `Prelude.seq` Prelude.rnf partSize

instance Data.ToHeaders InitiateMultipartUpload where
  toHeaders InitiateMultipartUpload' {..} =
    Prelude.mconcat
      [ "x-amz-archive-description"
          Data.=# archiveDescription,
        "x-amz-part-size" Data.=# partSize
      ]

instance Data.ToJSON InitiateMultipartUpload where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath InitiateMultipartUpload where
  toPath InitiateMultipartUpload' {..} =
    Prelude.mconcat
      [ "/",
        Data.toBS accountId,
        "/vaults/",
        Data.toBS vaultName,
        "/multipart-uploads"
      ]

instance Data.ToQuery InitiateMultipartUpload where
  toQuery = Prelude.const Prelude.mempty

-- | The Amazon S3 Glacier response to your request.
--
-- /See:/ 'newInitiateMultipartUploadResponse' smart constructor.
data InitiateMultipartUploadResponse = InitiateMultipartUploadResponse'
  { -- | The relative URI path of the multipart upload ID Amazon S3 Glacier
    -- created.
    location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the multipart upload. This value is also included as part of
    -- the location.
    uploadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitiateMultipartUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'initiateMultipartUploadResponse_location' - The relative URI path of the multipart upload ID Amazon S3 Glacier
-- created.
--
-- 'httpStatus', 'initiateMultipartUploadResponse_httpStatus' - The response's http status code.
--
-- 'uploadId', 'initiateMultipartUploadResponse_uploadId' - The ID of the multipart upload. This value is also included as part of
-- the location.
newInitiateMultipartUploadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'uploadId'
  Prelude.Text ->
  InitiateMultipartUploadResponse
newInitiateMultipartUploadResponse
  pHttpStatus_
  pUploadId_ =
    InitiateMultipartUploadResponse'
      { location =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        uploadId = pUploadId_
      }

-- | The relative URI path of the multipart upload ID Amazon S3 Glacier
-- created.
initiateMultipartUploadResponse_location :: Lens.Lens' InitiateMultipartUploadResponse (Prelude.Maybe Prelude.Text)
initiateMultipartUploadResponse_location = Lens.lens (\InitiateMultipartUploadResponse' {location} -> location) (\s@InitiateMultipartUploadResponse' {} a -> s {location = a} :: InitiateMultipartUploadResponse)

-- | The response's http status code.
initiateMultipartUploadResponse_httpStatus :: Lens.Lens' InitiateMultipartUploadResponse Prelude.Int
initiateMultipartUploadResponse_httpStatus = Lens.lens (\InitiateMultipartUploadResponse' {httpStatus} -> httpStatus) (\s@InitiateMultipartUploadResponse' {} a -> s {httpStatus = a} :: InitiateMultipartUploadResponse)

-- | The ID of the multipart upload. This value is also included as part of
-- the location.
initiateMultipartUploadResponse_uploadId :: Lens.Lens' InitiateMultipartUploadResponse Prelude.Text
initiateMultipartUploadResponse_uploadId = Lens.lens (\InitiateMultipartUploadResponse' {uploadId} -> uploadId) (\s@InitiateMultipartUploadResponse' {} a -> s {uploadId = a} :: InitiateMultipartUploadResponse)

instance
  Prelude.NFData
    InitiateMultipartUploadResponse
  where
  rnf InitiateMultipartUploadResponse' {..} =
    Prelude.rnf location
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf uploadId
