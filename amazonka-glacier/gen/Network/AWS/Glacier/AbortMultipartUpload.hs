{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glacier.AbortMultipartUpload
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Glacier.AbortMultipartUpload
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

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest AbortMultipartUpload where
  type
    Rs AbortMultipartUpload =
      AbortMultipartUploadResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull AbortMultipartUploadResponse'

instance Prelude.Hashable AbortMultipartUpload

instance Prelude.NFData AbortMultipartUpload

instance Prelude.ToHeaders AbortMultipartUpload where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AbortMultipartUpload where
  toPath AbortMultipartUpload' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName,
        "/multipart-uploads/",
        Prelude.toBS uploadId
      ]

instance Prelude.ToQuery AbortMultipartUpload where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAbortMultipartUploadResponse' smart constructor.
data AbortMultipartUploadResponse = AbortMultipartUploadResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AbortMultipartUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAbortMultipartUploadResponse ::
  AbortMultipartUploadResponse
newAbortMultipartUploadResponse =
  AbortMultipartUploadResponse'

instance Prelude.NFData AbortMultipartUploadResponse
