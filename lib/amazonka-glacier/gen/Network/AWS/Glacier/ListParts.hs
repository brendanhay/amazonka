{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListParts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists the parts of an archive that have been uploaded in a specific multipart upload. You can make this request at any time during an in-progress multipart upload before you complete the upload (see 'CompleteMultipartUpload' . List Parts returns an error for completed uploads. The list returned in the List Parts response is sorted by part range.
--
-- The List Parts operation supports pagination. By default, this operation returns up to 50 uploaded parts in the response. You should always check the response for a @marker@ at which to continue the list; if there are no more items the @marker@ is @null@ . To return a list of parts that begins at a specific part, set the @marker@ request parameter to the value you obtained from a previous List Parts request. You can also limit the number of parts returned in the response by specifying the @limit@ parameter in the request.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and the underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-list-parts.html List Parts> in the /Amazon Glacier Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Glacier.ListParts
  ( -- * Creating a request
    ListParts (..),
    mkListParts,

    -- ** Request lenses
    lpMarker,
    lpLimit,
    lpAccountId,
    lpVaultName,
    lpUploadId,

    -- * Destructuring the response
    ListPartsResponse (..),
    mkListPartsResponse,

    -- ** Response lenses
    lprsParts,
    lprsMultipartUploadId,
    lprsPartSizeInBytes,
    lprsArchiveDescription,
    lprsVaultARN,
    lprsMarker,
    lprsCreationDate,
    lprsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options for retrieving a list of parts of an archive that have been uploaded in a specific multipart upload.
--
-- /See:/ 'mkListParts' smart constructor.
data ListParts = ListParts'
  { marker :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Text,
    accountId :: Lude.Text,
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

-- | Creates a value of 'ListParts' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'limit' - The maximum number of parts to be returned. The default limit is 50. The number of parts returned might be fewer than the specified limit, but the number of returned parts never exceeds the limit.
-- * 'marker' - An opaque string used for pagination. This value specifies the part at which the listing of parts should begin. Get the marker value from the response of a previous List Parts response. You need only include the marker if you are continuing the pagination of results started in a previous List Parts request.
-- * 'uploadId' - The upload ID of the multipart upload.
-- * 'vaultName' - The name of the vault.
mkListParts ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  -- | 'uploadId'
  Lude.Text ->
  ListParts
mkListParts pAccountId_ pVaultName_ pUploadId_ =
  ListParts'
    { marker = Lude.Nothing,
      limit = Lude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_,
      uploadId = pUploadId_
    }

-- | An opaque string used for pagination. This value specifies the part at which the listing of parts should begin. Get the marker value from the response of a previous List Parts response. You need only include the marker if you are continuing the pagination of results started in a previous List Parts request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMarker :: Lens.Lens' ListParts (Lude.Maybe Lude.Text)
lpMarker = Lens.lens (marker :: ListParts -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListParts)
{-# DEPRECATED lpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of parts to be returned. The default limit is 50. The number of parts returned might be fewer than the specified limit, but the number of returned parts never exceeds the limit.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpLimit :: Lens.Lens' ListParts (Lude.Maybe Lude.Text)
lpLimit = Lens.lens (limit :: ListParts -> Lude.Maybe Lude.Text) (\s a -> s {limit = a} :: ListParts)
{-# DEPRECATED lpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpAccountId :: Lens.Lens' ListParts Lude.Text
lpAccountId = Lens.lens (accountId :: ListParts -> Lude.Text) (\s a -> s {accountId = a} :: ListParts)
{-# DEPRECATED lpAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpVaultName :: Lens.Lens' ListParts Lude.Text
lpVaultName = Lens.lens (vaultName :: ListParts -> Lude.Text) (\s a -> s {vaultName = a} :: ListParts)
{-# DEPRECATED lpVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The upload ID of the multipart upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpUploadId :: Lens.Lens' ListParts Lude.Text
lpUploadId = Lens.lens (uploadId :: ListParts -> Lude.Text) (\s a -> s {uploadId = a} :: ListParts)
{-# DEPRECATED lpUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

instance Page.AWSPager ListParts where
  page rq rs
    | Page.stop (rs Lens.^. lprsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsParts) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lpMarker Lens..~ rs Lens.^. lprsMarker

instance Lude.AWSRequest ListParts where
  type Rs ListParts = ListPartsResponse
  request = Req.get glacierService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPartsResponse'
            Lude.<$> (x Lude..?> "Parts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "MultipartUploadId")
            Lude.<*> (x Lude..?> "PartSizeInBytes")
            Lude.<*> (x Lude..?> "ArchiveDescription")
            Lude.<*> (x Lude..?> "VaultARN")
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "CreationDate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListParts where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListParts where
  toPath ListParts' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/multipart-uploads/",
        Lude.toBS uploadId
      ]

instance Lude.ToQuery ListParts where
  toQuery ListParts' {..} =
    Lude.mconcat ["marker" Lude.=: marker, "limit" Lude.=: limit]

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkListPartsResponse' smart constructor.
data ListPartsResponse = ListPartsResponse'
  { parts ::
      Lude.Maybe [PartListElement],
    multipartUploadId :: Lude.Maybe Lude.Text,
    partSizeInBytes :: Lude.Maybe Lude.Integer,
    archiveDescription :: Lude.Maybe Lude.Text,
    vaultARN :: Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListPartsResponse' with the minimum fields required to make a request.
--
-- * 'archiveDescription' - The description of the archive that was specified in the Initiate Multipart Upload request.
-- * 'creationDate' - The UTC time at which the multipart upload was initiated.
-- * 'marker' - An opaque string that represents where to continue pagination of the results. You use the marker in a new List Parts request to obtain more jobs in the list. If there are no more parts, this value is @null@ .
-- * 'multipartUploadId' - The ID of the upload to which the parts are associated.
-- * 'partSizeInBytes' - The part size in bytes. This is the same value that you specified in the Initiate Multipart Upload request.
-- * 'parts' - A list of the part sizes of the multipart upload. Each object in the array contains a @RangeBytes@ and @sha256-tree-hash@ name/value pair.
-- * 'responseStatus' - The response status code.
-- * 'vaultARN' - The Amazon Resource Name (ARN) of the vault to which the multipart upload was initiated.
mkListPartsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPartsResponse
mkListPartsResponse pResponseStatus_ =
  ListPartsResponse'
    { parts = Lude.Nothing,
      multipartUploadId = Lude.Nothing,
      partSizeInBytes = Lude.Nothing,
      archiveDescription = Lude.Nothing,
      vaultARN = Lude.Nothing,
      marker = Lude.Nothing,
      creationDate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the part sizes of the multipart upload. Each object in the array contains a @RangeBytes@ and @sha256-tree-hash@ name/value pair.
--
-- /Note:/ Consider using 'parts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsParts :: Lens.Lens' ListPartsResponse (Lude.Maybe [PartListElement])
lprsParts = Lens.lens (parts :: ListPartsResponse -> Lude.Maybe [PartListElement]) (\s a -> s {parts = a} :: ListPartsResponse)
{-# DEPRECATED lprsParts "Use generic-lens or generic-optics with 'parts' instead." #-}

-- | The ID of the upload to which the parts are associated.
--
-- /Note:/ Consider using 'multipartUploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsMultipartUploadId :: Lens.Lens' ListPartsResponse (Lude.Maybe Lude.Text)
lprsMultipartUploadId = Lens.lens (multipartUploadId :: ListPartsResponse -> Lude.Maybe Lude.Text) (\s a -> s {multipartUploadId = a} :: ListPartsResponse)
{-# DEPRECATED lprsMultipartUploadId "Use generic-lens or generic-optics with 'multipartUploadId' instead." #-}

-- | The part size in bytes. This is the same value that you specified in the Initiate Multipart Upload request.
--
-- /Note:/ Consider using 'partSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPartSizeInBytes :: Lens.Lens' ListPartsResponse (Lude.Maybe Lude.Integer)
lprsPartSizeInBytes = Lens.lens (partSizeInBytes :: ListPartsResponse -> Lude.Maybe Lude.Integer) (\s a -> s {partSizeInBytes = a} :: ListPartsResponse)
{-# DEPRECATED lprsPartSizeInBytes "Use generic-lens or generic-optics with 'partSizeInBytes' instead." #-}

-- | The description of the archive that was specified in the Initiate Multipart Upload request.
--
-- /Note:/ Consider using 'archiveDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsArchiveDescription :: Lens.Lens' ListPartsResponse (Lude.Maybe Lude.Text)
lprsArchiveDescription = Lens.lens (archiveDescription :: ListPartsResponse -> Lude.Maybe Lude.Text) (\s a -> s {archiveDescription = a} :: ListPartsResponse)
{-# DEPRECATED lprsArchiveDescription "Use generic-lens or generic-optics with 'archiveDescription' instead." #-}

-- | The Amazon Resource Name (ARN) of the vault to which the multipart upload was initiated.
--
-- /Note:/ Consider using 'vaultARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsVaultARN :: Lens.Lens' ListPartsResponse (Lude.Maybe Lude.Text)
lprsVaultARN = Lens.lens (vaultARN :: ListPartsResponse -> Lude.Maybe Lude.Text) (\s a -> s {vaultARN = a} :: ListPartsResponse)
{-# DEPRECATED lprsVaultARN "Use generic-lens or generic-optics with 'vaultARN' instead." #-}

-- | An opaque string that represents where to continue pagination of the results. You use the marker in a new List Parts request to obtain more jobs in the list. If there are no more parts, this value is @null@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsMarker :: Lens.Lens' ListPartsResponse (Lude.Maybe Lude.Text)
lprsMarker = Lens.lens (marker :: ListPartsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListPartsResponse)
{-# DEPRECATED lprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The UTC time at which the multipart upload was initiated.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsCreationDate :: Lens.Lens' ListPartsResponse (Lude.Maybe Lude.Text)
lprsCreationDate = Lens.lens (creationDate :: ListPartsResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: ListPartsResponse)
{-# DEPRECATED lprsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPartsResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPartsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPartsResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
