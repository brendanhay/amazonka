{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.InitiateMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates a multipart upload. Amazon S3 Glacier creates a multipart upload resource and returns its ID in the response. The multipart upload ID is used in subsequent requests to upload parts of an archive (see 'UploadMultipartPart' ).
--
-- When you initiate a multipart upload, you specify the part size in number of bytes. The part size must be a megabyte (1024 KB) multiplied by a power of 2-for example, 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8 MB), and so on. The minimum allowable part size is 1 MB, and the maximum is 4 GB.
-- Every part you upload to this resource (see 'UploadMultipartPart' ), except the last one, must have the same size. The last one can be the same size or smaller. For example, suppose you want to upload a 16.2 MB file. If you initiate the multipart upload with a part size of 4 MB, you will upload four parts of 4 MB each and one part of 0.2 MB.
-- After you complete the multipart upload, Amazon S3 Glacier (Glacier) removes the multipart upload resource referenced by the ID. Glacier also removes the multipart upload resource if you cancel the multipart upload or it may be removed if there is no activity for a period of 24 hours.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-archive-mpu.html Uploading Large Archives in Parts (Multipart Upload)> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-initiate-upload.html Initiate Multipart Upload> in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.InitiateMultipartUpload
  ( -- * Creating a request
    InitiateMultipartUpload (..),
    mkInitiateMultipartUpload,

    -- ** Request lenses
    imuPartSize,
    imuArchiveDescription,
    imuAccountId,
    imuVaultName,

    -- * Destructuring the response
    InitiateMultipartUploadResponse (..),
    mkInitiateMultipartUploadResponse,

    -- ** Response lenses
    imursLocation,
    imursUploadId,
    imursResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options for initiating a multipart upload to an Amazon S3 Glacier vault.
--
-- /See:/ 'mkInitiateMultipartUpload' smart constructor.
data InitiateMultipartUpload = InitiateMultipartUpload'
  { partSize ::
      Lude.Maybe Lude.Text,
    archiveDescription :: Lude.Maybe Lude.Text,
    accountId :: Lude.Text,
    vaultName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitiateMultipartUpload' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'archiveDescription' - The archive description that you are uploading in parts.
--
-- The part size must be a megabyte (1024 KB) multiplied by a power of 2, for example 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8 MB), and so on. The minimum allowable part size is 1 MB, and the maximum is 4 GB (4096 MB).
-- * 'partSize' - The size of each part except the last, in bytes. The last part can be smaller than this part size.
-- * 'vaultName' - The name of the vault.
mkInitiateMultipartUpload ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  InitiateMultipartUpload
mkInitiateMultipartUpload pAccountId_ pVaultName_ =
  InitiateMultipartUpload'
    { partSize = Lude.Nothing,
      archiveDescription = Lude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The size of each part except the last, in bytes. The last part can be smaller than this part size.
--
-- /Note:/ Consider using 'partSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imuPartSize :: Lens.Lens' InitiateMultipartUpload (Lude.Maybe Lude.Text)
imuPartSize = Lens.lens (partSize :: InitiateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {partSize = a} :: InitiateMultipartUpload)
{-# DEPRECATED imuPartSize "Use generic-lens or generic-optics with 'partSize' instead." #-}

-- | The archive description that you are uploading in parts.
--
-- The part size must be a megabyte (1024 KB) multiplied by a power of 2, for example 1048576 (1 MB), 2097152 (2 MB), 4194304 (4 MB), 8388608 (8 MB), and so on. The minimum allowable part size is 1 MB, and the maximum is 4 GB (4096 MB).
--
-- /Note:/ Consider using 'archiveDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imuArchiveDescription :: Lens.Lens' InitiateMultipartUpload (Lude.Maybe Lude.Text)
imuArchiveDescription = Lens.lens (archiveDescription :: InitiateMultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {archiveDescription = a} :: InitiateMultipartUpload)
{-# DEPRECATED imuArchiveDescription "Use generic-lens or generic-optics with 'archiveDescription' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imuAccountId :: Lens.Lens' InitiateMultipartUpload Lude.Text
imuAccountId = Lens.lens (accountId :: InitiateMultipartUpload -> Lude.Text) (\s a -> s {accountId = a} :: InitiateMultipartUpload)
{-# DEPRECATED imuAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imuVaultName :: Lens.Lens' InitiateMultipartUpload Lude.Text
imuVaultName = Lens.lens (vaultName :: InitiateMultipartUpload -> Lude.Text) (\s a -> s {vaultName = a} :: InitiateMultipartUpload)
{-# DEPRECATED imuVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Lude.AWSRequest InitiateMultipartUpload where
  type Rs InitiateMultipartUpload = InitiateMultipartUploadResponse
  request = Req.postJSON glacierService
  response =
    Res.receiveEmpty
      ( \s h x ->
          InitiateMultipartUploadResponse'
            Lude.<$> (h Lude..#? "Location")
            Lude.<*> (h Lude..#? "x-amz-multipart-upload-id")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders InitiateMultipartUpload where
  toHeaders InitiateMultipartUpload' {..} =
    Lude.mconcat
      [ "x-amz-part-size" Lude.=# partSize,
        "x-amz-archive-description" Lude.=# archiveDescription
      ]

instance Lude.ToJSON InitiateMultipartUpload where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath InitiateMultipartUpload where
  toPath InitiateMultipartUpload' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/multipart-uploads"
      ]

instance Lude.ToQuery InitiateMultipartUpload where
  toQuery = Lude.const Lude.mempty

-- | The Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkInitiateMultipartUploadResponse' smart constructor.
data InitiateMultipartUploadResponse = InitiateMultipartUploadResponse'
  { location ::
      Lude.Maybe Lude.Text,
    uploadId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitiateMultipartUploadResponse' with the minimum fields required to make a request.
--
-- * 'location' - The relative URI path of the multipart upload ID Amazon S3 Glacier created.
-- * 'responseStatus' - The response status code.
-- * 'uploadId' - The ID of the multipart upload. This value is also included as part of the location.
mkInitiateMultipartUploadResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  InitiateMultipartUploadResponse
mkInitiateMultipartUploadResponse pResponseStatus_ =
  InitiateMultipartUploadResponse'
    { location = Lude.Nothing,
      uploadId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The relative URI path of the multipart upload ID Amazon S3 Glacier created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imursLocation :: Lens.Lens' InitiateMultipartUploadResponse (Lude.Maybe Lude.Text)
imursLocation = Lens.lens (location :: InitiateMultipartUploadResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: InitiateMultipartUploadResponse)
{-# DEPRECATED imursLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The ID of the multipart upload. This value is also included as part of the location.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imursUploadId :: Lens.Lens' InitiateMultipartUploadResponse (Lude.Maybe Lude.Text)
imursUploadId = Lens.lens (uploadId :: InitiateMultipartUploadResponse -> Lude.Maybe Lude.Text) (\s a -> s {uploadId = a} :: InitiateMultipartUploadResponse)
{-# DEPRECATED imursUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imursResponseStatus :: Lens.Lens' InitiateMultipartUploadResponse Lude.Int
imursResponseStatus = Lens.lens (responseStatus :: InitiateMultipartUploadResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: InitiateMultipartUploadResponse)
{-# DEPRECATED imursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
