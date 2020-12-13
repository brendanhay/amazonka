{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.ListMultipartUploads
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists in-progress multipart uploads for the specified vault. An in-progress multipart upload is a multipart upload that has been initiated by an 'InitiateMultipartUpload' request, but has not yet been completed or aborted. The list returned in the List Multipart Upload response has no guaranteed order.
--
-- The List Multipart Uploads operation supports pagination. By default, this operation returns up to 50 multipart uploads in the response. You should always check the response for a @marker@ at which to continue the list; if there are no more items the @marker@ is @null@ . To return a list of multipart uploads that begins at a specific upload, set the @marker@ request parameter to the value you obtained from a previous List Multipart Upload request. You can also limit the number of uploads returned in the response by specifying the @limit@ parameter in the request.
-- Note the difference between this operation and listing parts ('ListParts' ). The List Multipart Uploads operation lists all multipart uploads for a vault and does not require a multipart upload ID. The List Parts operation requires a multipart upload ID since parts are associated with a single upload.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and the underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-list-uploads.html List Multipart Uploads > in the /Amazon Glacier Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Glacier.ListMultipartUploads
  ( -- * Creating a request
    ListMultipartUploads (..),
    mkListMultipartUploads,

    -- ** Request lenses
    lmuVaultName,
    lmuAccountId,
    lmuMarker,
    lmuLimit,

    -- * Destructuring the response
    ListMultipartUploadsResponse (..),
    mkListMultipartUploadsResponse,

    -- ** Response lenses
    lmursUploadsList,
    lmursMarker,
    lmursResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options for retrieving list of in-progress multipart uploads for an Amazon Glacier vault.
--
-- /See:/ 'mkListMultipartUploads' smart constructor.
data ListMultipartUploads = ListMultipartUploads'
  { -- | The name of the vault.
    vaultName :: Lude.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Lude.Text,
    -- | An opaque string used for pagination. This value specifies the upload at which the listing of uploads should begin. Get the marker value from a previous List Uploads response. You need only include the marker if you are continuing the pagination of results started in a previous List Uploads request.
    marker :: Lude.Maybe Lude.Text,
    -- | Specifies the maximum number of uploads returned in the response body. If this value is not specified, the List Uploads operation returns up to 50 uploads.
    limit :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMultipartUploads' with the minimum fields required to make a request.
--
-- * 'vaultName' - The name of the vault.
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'marker' - An opaque string used for pagination. This value specifies the upload at which the listing of uploads should begin. Get the marker value from a previous List Uploads response. You need only include the marker if you are continuing the pagination of results started in a previous List Uploads request.
-- * 'limit' - Specifies the maximum number of uploads returned in the response body. If this value is not specified, the List Uploads operation returns up to 50 uploads.
mkListMultipartUploads ::
  -- | 'vaultName'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  ListMultipartUploads
mkListMultipartUploads pVaultName_ pAccountId_ =
  ListMultipartUploads'
    { vaultName = pVaultName_,
      accountId = pAccountId_,
      marker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuVaultName :: Lens.Lens' ListMultipartUploads Lude.Text
lmuVaultName = Lens.lens (vaultName :: ListMultipartUploads -> Lude.Text) (\s a -> s {vaultName = a} :: ListMultipartUploads)
{-# DEPRECATED lmuVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuAccountId :: Lens.Lens' ListMultipartUploads Lude.Text
lmuAccountId = Lens.lens (accountId :: ListMultipartUploads -> Lude.Text) (\s a -> s {accountId = a} :: ListMultipartUploads)
{-# DEPRECATED lmuAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | An opaque string used for pagination. This value specifies the upload at which the listing of uploads should begin. Get the marker value from a previous List Uploads response. You need only include the marker if you are continuing the pagination of results started in a previous List Uploads request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuMarker :: Lens.Lens' ListMultipartUploads (Lude.Maybe Lude.Text)
lmuMarker = Lens.lens (marker :: ListMultipartUploads -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListMultipartUploads)
{-# DEPRECATED lmuMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies the maximum number of uploads returned in the response body. If this value is not specified, the List Uploads operation returns up to 50 uploads.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuLimit :: Lens.Lens' ListMultipartUploads (Lude.Maybe Lude.Text)
lmuLimit = Lens.lens (limit :: ListMultipartUploads -> Lude.Maybe Lude.Text) (\s a -> s {limit = a} :: ListMultipartUploads)
{-# DEPRECATED lmuLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListMultipartUploads where
  page rq rs
    | Page.stop (rs Lens.^. lmursMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lmursUploadsList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lmuMarker Lens..~ rs Lens.^. lmursMarker

instance Lude.AWSRequest ListMultipartUploads where
  type Rs ListMultipartUploads = ListMultipartUploadsResponse
  request = Req.get glacierService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMultipartUploadsResponse'
            Lude.<$> (x Lude..?> "UploadsList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMultipartUploads where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListMultipartUploads where
  toPath ListMultipartUploads' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/multipart-uploads"
      ]

instance Lude.ToQuery ListMultipartUploads where
  toQuery ListMultipartUploads' {..} =
    Lude.mconcat ["marker" Lude.=: marker, "limit" Lude.=: limit]

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkListMultipartUploadsResponse' smart constructor.
data ListMultipartUploadsResponse = ListMultipartUploadsResponse'
  { -- | A list of in-progress multipart uploads.
    uploadsList :: Lude.Maybe [UploadListElement],
    -- | An opaque string that represents where to continue pagination of the results. You use the marker in a new List Multipart Uploads request to obtain more uploads in the list. If there are no more uploads, this value is @null@ .
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMultipartUploadsResponse' with the minimum fields required to make a request.
--
-- * 'uploadsList' - A list of in-progress multipart uploads.
-- * 'marker' - An opaque string that represents where to continue pagination of the results. You use the marker in a new List Multipart Uploads request to obtain more uploads in the list. If there are no more uploads, this value is @null@ .
-- * 'responseStatus' - The response status code.
mkListMultipartUploadsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMultipartUploadsResponse
mkListMultipartUploadsResponse pResponseStatus_ =
  ListMultipartUploadsResponse'
    { uploadsList = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of in-progress multipart uploads.
--
-- /Note:/ Consider using 'uploadsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursUploadsList :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe [UploadListElement])
lmursUploadsList = Lens.lens (uploadsList :: ListMultipartUploadsResponse -> Lude.Maybe [UploadListElement]) (\s a -> s {uploadsList = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursUploadsList "Use generic-lens or generic-optics with 'uploadsList' instead." #-}

-- | An opaque string that represents where to continue pagination of the results. You use the marker in a new List Multipart Uploads request to obtain more uploads in the list. If there are no more uploads, this value is @null@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursMarker :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe Lude.Text)
lmursMarker = Lens.lens (marker :: ListMultipartUploadsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursResponseStatus :: Lens.Lens' ListMultipartUploadsResponse Lude.Int
lmursResponseStatus = Lens.lens (responseStatus :: ListMultipartUploadsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
