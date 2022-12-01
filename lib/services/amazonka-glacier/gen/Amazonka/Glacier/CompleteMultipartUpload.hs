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
-- Module      : Amazonka.Glacier.CompleteMultipartUpload
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You call this operation to inform Amazon S3 Glacier (Glacier) that all
-- the archive parts have been uploaded and that Glacier can now assemble
-- the archive from the uploaded parts. After assembling and saving the
-- archive to the vault, Glacier returns the URI path of the newly created
-- archive resource. Using the URI path, you can then access the archive.
-- After you upload an archive, you should save the archive ID returned to
-- retrieve the archive at a later point. You can also get the vault
-- inventory to obtain a list of archive IDs in a vault. For more
-- information, see InitiateJob.
--
-- In the request, you must include the computed SHA256 tree hash of the
-- entire archive you have uploaded. For information about computing a
-- SHA256 tree hash, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html Computing Checksums>.
-- On the server side, Glacier also constructs the SHA256 tree hash of the
-- assembled archive. If the values match, Glacier saves the archive to the
-- vault; otherwise, it returns an error, and the operation fails. The
-- ListParts operation returns a list of parts uploaded for a specific
-- multipart upload. It includes checksum information for each uploaded
-- part that can be used to debug a bad checksum issue.
--
-- Additionally, Glacier also checks for any missing content ranges when
-- assembling the archive, if missing content ranges are found, Glacier
-- returns an error and the operation fails.
--
-- Complete Multipart Upload is an idempotent operation. After your first
-- successful complete multipart upload, if you call the operation again
-- within a short period, the operation will succeed and return the same
-- archive ID. This is useful in the event you experience a network issue
-- that causes an aborted connection or receive a 500 server error, in
-- which case you can repeat your Complete Multipart Upload request and get
-- the same archive ID without creating duplicate archives. Note, however,
-- that after the multipart upload completes, you cannot call the List
-- Parts operation and the multipart upload will not appear in List
-- Multipart Uploads response, even if idempotent complete is possible.
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
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-complete-upload.html Complete Multipart Upload>
-- in the /Amazon Glacier Developer Guide/.
module Amazonka.Glacier.CompleteMultipartUpload
  ( -- * Creating a Request
    CompleteMultipartUpload (..),
    newCompleteMultipartUpload,

    -- * Request Lenses
    completeMultipartUpload_accountId,
    completeMultipartUpload_vaultName,
    completeMultipartUpload_uploadId,
    completeMultipartUpload_archiveSize,
    completeMultipartUpload_checksum,

    -- * Destructuring the Response
    ArchiveCreationOutput (..),
    newArchiveCreationOutput,

    -- * Response Lenses
    archiveCreationOutput_archiveId,
    archiveCreationOutput_checksum,
    archiveCreationOutput_location,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Provides options to complete a multipart upload operation. This informs
-- Amazon Glacier that all the archive parts have been uploaded and Amazon
-- S3 Glacier (Glacier) can now assemble the archive from the uploaded
-- parts. After assembling and saving the archive to the vault, Glacier
-- returns the URI path of the newly created archive resource.
--
-- /See:/ 'newCompleteMultipartUpload' smart constructor.
data CompleteMultipartUpload = CompleteMultipartUpload'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text,
    -- | The upload ID of the multipart upload.
    uploadId :: Prelude.Text,
    -- | The total size, in bytes, of the entire archive. This value should be
    -- the sum of all the sizes of the individual parts that you uploaded.
    archiveSize :: Prelude.Text,
    -- | The SHA256 tree hash of the entire archive. It is the tree hash of
    -- SHA256 tree hash of the individual parts. If the value you specify in
    -- the request does not match the SHA256 tree hash of the final assembled
    -- archive as computed by Amazon S3 Glacier (Glacier), Glacier returns an
    -- error and the request fails.
    checksum :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompleteMultipartUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'completeMultipartUpload_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'completeMultipartUpload_vaultName' - The name of the vault.
--
-- 'uploadId', 'completeMultipartUpload_uploadId' - The upload ID of the multipart upload.
--
-- 'archiveSize', 'completeMultipartUpload_archiveSize' - The total size, in bytes, of the entire archive. This value should be
-- the sum of all the sizes of the individual parts that you uploaded.
--
-- 'checksum', 'completeMultipartUpload_checksum' - The SHA256 tree hash of the entire archive. It is the tree hash of
-- SHA256 tree hash of the individual parts. If the value you specify in
-- the request does not match the SHA256 tree hash of the final assembled
-- archive as computed by Amazon S3 Glacier (Glacier), Glacier returns an
-- error and the request fails.
newCompleteMultipartUpload ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  -- | 'archiveSize'
  Prelude.Text ->
  -- | 'checksum'
  Prelude.Text ->
  CompleteMultipartUpload
newCompleteMultipartUpload
  pAccountId_
  pVaultName_
  pUploadId_
  pArchiveSize_
  pChecksum_ =
    CompleteMultipartUpload'
      { accountId = pAccountId_,
        vaultName = pVaultName_,
        uploadId = pUploadId_,
        archiveSize = pArchiveSize_,
        checksum = pChecksum_
      }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
completeMultipartUpload_accountId :: Lens.Lens' CompleteMultipartUpload Prelude.Text
completeMultipartUpload_accountId = Lens.lens (\CompleteMultipartUpload' {accountId} -> accountId) (\s@CompleteMultipartUpload' {} a -> s {accountId = a} :: CompleteMultipartUpload)

-- | The name of the vault.
completeMultipartUpload_vaultName :: Lens.Lens' CompleteMultipartUpload Prelude.Text
completeMultipartUpload_vaultName = Lens.lens (\CompleteMultipartUpload' {vaultName} -> vaultName) (\s@CompleteMultipartUpload' {} a -> s {vaultName = a} :: CompleteMultipartUpload)

-- | The upload ID of the multipart upload.
completeMultipartUpload_uploadId :: Lens.Lens' CompleteMultipartUpload Prelude.Text
completeMultipartUpload_uploadId = Lens.lens (\CompleteMultipartUpload' {uploadId} -> uploadId) (\s@CompleteMultipartUpload' {} a -> s {uploadId = a} :: CompleteMultipartUpload)

-- | The total size, in bytes, of the entire archive. This value should be
-- the sum of all the sizes of the individual parts that you uploaded.
completeMultipartUpload_archiveSize :: Lens.Lens' CompleteMultipartUpload Prelude.Text
completeMultipartUpload_archiveSize = Lens.lens (\CompleteMultipartUpload' {archiveSize} -> archiveSize) (\s@CompleteMultipartUpload' {} a -> s {archiveSize = a} :: CompleteMultipartUpload)

-- | The SHA256 tree hash of the entire archive. It is the tree hash of
-- SHA256 tree hash of the individual parts. If the value you specify in
-- the request does not match the SHA256 tree hash of the final assembled
-- archive as computed by Amazon S3 Glacier (Glacier), Glacier returns an
-- error and the request fails.
completeMultipartUpload_checksum :: Lens.Lens' CompleteMultipartUpload Prelude.Text
completeMultipartUpload_checksum = Lens.lens (\CompleteMultipartUpload' {checksum} -> checksum) (\s@CompleteMultipartUpload' {} a -> s {checksum = a} :: CompleteMultipartUpload)

instance Core.AWSRequest CompleteMultipartUpload where
  type
    AWSResponse CompleteMultipartUpload =
      ArchiveCreationOutput
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ArchiveCreationOutput'
            Prelude.<$> (h Core..#? "x-amz-archive-id")
            Prelude.<*> (h Core..#? "x-amz-sha256-tree-hash")
            Prelude.<*> (h Core..#? "Location")
      )

instance Prelude.Hashable CompleteMultipartUpload where
  hashWithSalt _salt CompleteMultipartUpload' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vaultName
      `Prelude.hashWithSalt` uploadId
      `Prelude.hashWithSalt` archiveSize
      `Prelude.hashWithSalt` checksum

instance Prelude.NFData CompleteMultipartUpload where
  rnf CompleteMultipartUpload' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf vaultName
      `Prelude.seq` Prelude.rnf uploadId
      `Prelude.seq` Prelude.rnf archiveSize
      `Prelude.seq` Prelude.rnf checksum

instance Core.ToHeaders CompleteMultipartUpload where
  toHeaders CompleteMultipartUpload' {..} =
    Prelude.mconcat
      [ "x-amz-archive-size" Core.=# archiveSize,
        "x-amz-sha256-tree-hash" Core.=# checksum
      ]

instance Core.ToJSON CompleteMultipartUpload where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath CompleteMultipartUpload where
  toPath CompleteMultipartUpload' {..} =
    Prelude.mconcat
      [ "/",
        Core.toBS accountId,
        "/vaults/",
        Core.toBS vaultName,
        "/multipart-uploads/",
        Core.toBS uploadId
      ]

instance Core.ToQuery CompleteMultipartUpload where
  toQuery = Prelude.const Prelude.mempty
