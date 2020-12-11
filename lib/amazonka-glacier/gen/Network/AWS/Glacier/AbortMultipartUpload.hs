{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.AbortMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation aborts a multipart upload identified by the upload ID.
--
-- After the Abort Multipart Upload request succeeds, you cannot upload any more parts to the multipart upload or complete the multipart upload. Aborting a completed upload fails. However, aborting an already-aborted upload will succeed, for a short time. For more information about uploading a part and completing a multipart upload, see 'UploadMultipartPart' and 'CompleteMultipartUpload' .
-- This operation is idempotent.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-abort-upload.html Abort Multipart Upload> in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.AbortMultipartUpload
  ( -- * Creating a request
    AbortMultipartUpload (..),
    mkAbortMultipartUpload,

    -- ** Request lenses
    amuAccountId,
    amuVaultName,
    amuUploadId,

    -- * Destructuring the response
    AbortMultipartUploadResponse (..),
    mkAbortMultipartUploadResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options to abort a multipart upload identified by the upload ID.
--
-- For information about the underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-abort-upload.html Abort Multipart Upload> . For conceptual information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier> .
--
-- /See:/ 'mkAbortMultipartUpload' smart constructor.
data AbortMultipartUpload = AbortMultipartUpload'
  { accountId ::
      Lude.Text,
    vaultName :: Lude.Text,
    uploadId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AbortMultipartUpload' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'uploadId' - The upload ID of the multipart upload to delete.
-- * 'vaultName' - The name of the vault.
mkAbortMultipartUpload ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  -- | 'uploadId'
  Lude.Text ->
  AbortMultipartUpload
mkAbortMultipartUpload pAccountId_ pVaultName_ pUploadId_ =
  AbortMultipartUpload'
    { accountId = pAccountId_,
      vaultName = pVaultName_,
      uploadId = pUploadId_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuAccountId :: Lens.Lens' AbortMultipartUpload Lude.Text
amuAccountId = Lens.lens (accountId :: AbortMultipartUpload -> Lude.Text) (\s a -> s {accountId = a} :: AbortMultipartUpload)
{-# DEPRECATED amuAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuVaultName :: Lens.Lens' AbortMultipartUpload Lude.Text
amuVaultName = Lens.lens (vaultName :: AbortMultipartUpload -> Lude.Text) (\s a -> s {vaultName = a} :: AbortMultipartUpload)
{-# DEPRECATED amuVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The upload ID of the multipart upload to delete.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amuUploadId :: Lens.Lens' AbortMultipartUpload Lude.Text
amuUploadId = Lens.lens (uploadId :: AbortMultipartUpload -> Lude.Text) (\s a -> s {uploadId = a} :: AbortMultipartUpload)
{-# DEPRECATED amuUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

instance Lude.AWSRequest AbortMultipartUpload where
  type Rs AbortMultipartUpload = AbortMultipartUploadResponse
  request = Req.delete glacierService
  response = Res.receiveNull AbortMultipartUploadResponse'

instance Lude.ToHeaders AbortMultipartUpload where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AbortMultipartUpload where
  toPath AbortMultipartUpload' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/multipart-uploads/",
        Lude.toBS uploadId
      ]

instance Lude.ToQuery AbortMultipartUpload where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAbortMultipartUploadResponse' smart constructor.
data AbortMultipartUploadResponse = AbortMultipartUploadResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AbortMultipartUploadResponse' with the minimum fields required to make a request.
mkAbortMultipartUploadResponse ::
  AbortMultipartUploadResponse
mkAbortMultipartUploadResponse = AbortMultipartUploadResponse'
