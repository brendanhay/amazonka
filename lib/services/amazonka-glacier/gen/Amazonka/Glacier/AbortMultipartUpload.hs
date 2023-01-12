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
-- Module      : Amazonka.Glacier.AbortMultipartUpload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation aborts a multipart upload identified by the upload ID.
--
-- After the Abort Multipart Upload request succeeds, you cannot upload any
-- more parts to the multipart upload or complete the multipart upload.
-- Aborting a completed upload fails. However, aborting an already-aborted
-- upload will succeed, for a short time. For more information about
-- uploading a part and completing a multipart upload, see
-- UploadMultipartPart and CompleteMultipartUpload.
--
-- This operation is idempotent.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier>
-- and
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-abort-upload.html Abort Multipart Upload>
-- in the /Amazon Glacier Developer Guide/.
module Amazonka.Glacier.AbortMultipartUpload
  ( -- * Creating a Request
    AbortMultipartUpload (..),
    newAbortMultipartUpload,

    -- * Request Lenses
    abortMultipartUpload_accountId,
    abortMultipartUpload_vaultName,
    abortMultipartUpload_uploadId,

    -- * Destructuring the Response
    AbortMultipartUploadResponse (..),
    newAbortMultipartUploadResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Provides options to abort a multipart upload identified by the upload
-- ID.
--
-- For information about the underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-abort-upload.html Abort Multipart Upload>.
-- For conceptual information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier>.
--
-- /See:/ 'newAbortMultipartUpload' smart constructor.
data AbortMultipartUpload = AbortMultipartUpload'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text,
    -- | The upload ID of the multipart upload to delete.
    uploadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbortMultipartUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'abortMultipartUpload_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'abortMultipartUpload_vaultName' - The name of the vault.
--
-- 'uploadId', 'abortMultipartUpload_uploadId' - The upload ID of the multipart upload to delete.
newAbortMultipartUpload ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  AbortMultipartUpload
newAbortMultipartUpload
  pAccountId_
  pVaultName_
  pUploadId_ =
    AbortMultipartUpload'
      { accountId = pAccountId_,
        vaultName = pVaultName_,
        uploadId = pUploadId_
      }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
abortMultipartUpload_accountId :: Lens.Lens' AbortMultipartUpload Prelude.Text
abortMultipartUpload_accountId = Lens.lens (\AbortMultipartUpload' {accountId} -> accountId) (\s@AbortMultipartUpload' {} a -> s {accountId = a} :: AbortMultipartUpload)

-- | The name of the vault.
abortMultipartUpload_vaultName :: Lens.Lens' AbortMultipartUpload Prelude.Text
abortMultipartUpload_vaultName = Lens.lens (\AbortMultipartUpload' {vaultName} -> vaultName) (\s@AbortMultipartUpload' {} a -> s {vaultName = a} :: AbortMultipartUpload)

-- | The upload ID of the multipart upload to delete.
abortMultipartUpload_uploadId :: Lens.Lens' AbortMultipartUpload Prelude.Text
abortMultipartUpload_uploadId = Lens.lens (\AbortMultipartUpload' {uploadId} -> uploadId) (\s@AbortMultipartUpload' {} a -> s {uploadId = a} :: AbortMultipartUpload)

instance Core.AWSRequest AbortMultipartUpload where
  type
    AWSResponse AbortMultipartUpload =
      AbortMultipartUploadResponse
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.delete (overrides defaultService)
  response =
    Response.receiveNull AbortMultipartUploadResponse'

instance Prelude.Hashable AbortMultipartUpload where
  hashWithSalt _salt AbortMultipartUpload' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vaultName
      `Prelude.hashWithSalt` uploadId

instance Prelude.NFData AbortMultipartUpload where
  rnf AbortMultipartUpload' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf vaultName
      `Prelude.seq` Prelude.rnf uploadId

instance Data.ToHeaders AbortMultipartUpload where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AbortMultipartUpload where
  toPath AbortMultipartUpload' {..} =
    Prelude.mconcat
      [ "/",
        Data.toBS accountId,
        "/vaults/",
        Data.toBS vaultName,
        "/multipart-uploads/",
        Data.toBS uploadId
      ]

instance Data.ToQuery AbortMultipartUpload where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAbortMultipartUploadResponse' smart constructor.
data AbortMultipartUploadResponse = AbortMultipartUploadResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbortMultipartUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAbortMultipartUploadResponse ::
  AbortMultipartUploadResponse
newAbortMultipartUploadResponse =
  AbortMultipartUploadResponse'

instance Prelude.NFData AbortMultipartUploadResponse where
  rnf _ = ()
