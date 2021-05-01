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
-- Module      : Network.AWS.Glacier.ListMultipartUploads
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists in-progress multipart uploads for the specified
-- vault. An in-progress multipart upload is a multipart upload that has
-- been initiated by an InitiateMultipartUpload request, but has not yet
-- been completed or aborted. The list returned in the List Multipart
-- Upload response has no guaranteed order.
--
-- The List Multipart Uploads operation supports pagination. By default,
-- this operation returns up to 50 multipart uploads in the response. You
-- should always check the response for a @marker@ at which to continue the
-- list; if there are no more items the @marker@ is @null@. To return a
-- list of multipart uploads that begins at a specific upload, set the
-- @marker@ request parameter to the value you obtained from a previous
-- List Multipart Upload request. You can also limit the number of uploads
-- returned in the response by specifying the @limit@ parameter in the
-- request.
--
-- Note the difference between this operation and listing parts
-- (ListParts). The List Multipart Uploads operation lists all multipart
-- uploads for a vault and does not require a multipart upload ID. The List
-- Parts operation requires a multipart upload ID since parts are
-- associated with a single upload.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and the underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier>
-- and
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-list-uploads.html List Multipart Uploads>
-- in the /Amazon Glacier Developer Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Glacier.ListMultipartUploads
  ( -- * Creating a Request
    ListMultipartUploads (..),
    newListMultipartUploads,

    -- * Request Lenses
    listMultipartUploads_limit,
    listMultipartUploads_marker,
    listMultipartUploads_accountId,
    listMultipartUploads_vaultName,

    -- * Destructuring the Response
    ListMultipartUploadsResponse (..),
    newListMultipartUploadsResponse,

    -- * Response Lenses
    listMultipartUploadsResponse_uploadsList,
    listMultipartUploadsResponse_marker,
    listMultipartUploadsResponse_httpStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for retrieving list of in-progress multipart uploads
-- for an Amazon Glacier vault.
--
-- /See:/ 'newListMultipartUploads' smart constructor.
data ListMultipartUploads = ListMultipartUploads'
  { -- | Specifies the maximum number of uploads returned in the response body.
    -- If this value is not specified, the List Uploads operation returns up to
    -- 50 uploads.
    limit :: Prelude.Maybe Prelude.Text,
    -- | An opaque string used for pagination. This value specifies the upload at
    -- which the listing of uploads should begin. Get the marker value from a
    -- previous List Uploads response. You need only include the marker if you
    -- are continuing the pagination of results started in a previous List
    -- Uploads request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListMultipartUploads' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listMultipartUploads_limit' - Specifies the maximum number of uploads returned in the response body.
-- If this value is not specified, the List Uploads operation returns up to
-- 50 uploads.
--
-- 'marker', 'listMultipartUploads_marker' - An opaque string used for pagination. This value specifies the upload at
-- which the listing of uploads should begin. Get the marker value from a
-- previous List Uploads response. You need only include the marker if you
-- are continuing the pagination of results started in a previous List
-- Uploads request.
--
-- 'accountId', 'listMultipartUploads_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'listMultipartUploads_vaultName' - The name of the vault.
newListMultipartUploads ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  ListMultipartUploads
newListMultipartUploads pAccountId_ pVaultName_ =
  ListMultipartUploads'
    { limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | Specifies the maximum number of uploads returned in the response body.
-- If this value is not specified, the List Uploads operation returns up to
-- 50 uploads.
listMultipartUploads_limit :: Lens.Lens' ListMultipartUploads (Prelude.Maybe Prelude.Text)
listMultipartUploads_limit = Lens.lens (\ListMultipartUploads' {limit} -> limit) (\s@ListMultipartUploads' {} a -> s {limit = a} :: ListMultipartUploads)

-- | An opaque string used for pagination. This value specifies the upload at
-- which the listing of uploads should begin. Get the marker value from a
-- previous List Uploads response. You need only include the marker if you
-- are continuing the pagination of results started in a previous List
-- Uploads request.
listMultipartUploads_marker :: Lens.Lens' ListMultipartUploads (Prelude.Maybe Prelude.Text)
listMultipartUploads_marker = Lens.lens (\ListMultipartUploads' {marker} -> marker) (\s@ListMultipartUploads' {} a -> s {marker = a} :: ListMultipartUploads)

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
listMultipartUploads_accountId :: Lens.Lens' ListMultipartUploads Prelude.Text
listMultipartUploads_accountId = Lens.lens (\ListMultipartUploads' {accountId} -> accountId) (\s@ListMultipartUploads' {} a -> s {accountId = a} :: ListMultipartUploads)

-- | The name of the vault.
listMultipartUploads_vaultName :: Lens.Lens' ListMultipartUploads Prelude.Text
listMultipartUploads_vaultName = Lens.lens (\ListMultipartUploads' {vaultName} -> vaultName) (\s@ListMultipartUploads' {} a -> s {vaultName = a} :: ListMultipartUploads)

instance Pager.AWSPager ListMultipartUploads where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listMultipartUploadsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listMultipartUploadsResponse_uploadsList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listMultipartUploads_marker
          Lens..~ rs
          Lens.^? listMultipartUploadsResponse_marker
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListMultipartUploads where
  type
    Rs ListMultipartUploads =
      ListMultipartUploadsResponse
  request =
    Request.glacierVersionHeader (Prelude._svcVersion defaultService)
      Prelude.. Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMultipartUploadsResponse'
            Prelude.<$> ( x Prelude..?> "UploadsList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMultipartUploads

instance Prelude.NFData ListMultipartUploads

instance Prelude.ToHeaders ListMultipartUploads where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListMultipartUploads where
  toPath ListMultipartUploads' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName,
        "/multipart-uploads"
      ]

instance Prelude.ToQuery ListMultipartUploads where
  toQuery ListMultipartUploads' {..} =
    Prelude.mconcat
      [ "limit" Prelude.=: limit,
        "marker" Prelude.=: marker
      ]

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newListMultipartUploadsResponse' smart constructor.
data ListMultipartUploadsResponse = ListMultipartUploadsResponse'
  { -- | A list of in-progress multipart uploads.
    uploadsList :: Prelude.Maybe [UploadListElement],
    -- | An opaque string that represents where to continue pagination of the
    -- results. You use the marker in a new List Multipart Uploads request to
    -- obtain more uploads in the list. If there are no more uploads, this
    -- value is @null@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListMultipartUploadsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uploadsList', 'listMultipartUploadsResponse_uploadsList' - A list of in-progress multipart uploads.
--
-- 'marker', 'listMultipartUploadsResponse_marker' - An opaque string that represents where to continue pagination of the
-- results. You use the marker in a new List Multipart Uploads request to
-- obtain more uploads in the list. If there are no more uploads, this
-- value is @null@.
--
-- 'httpStatus', 'listMultipartUploadsResponse_httpStatus' - The response's http status code.
newListMultipartUploadsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMultipartUploadsResponse
newListMultipartUploadsResponse pHttpStatus_ =
  ListMultipartUploadsResponse'
    { uploadsList =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of in-progress multipart uploads.
listMultipartUploadsResponse_uploadsList :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe [UploadListElement])
listMultipartUploadsResponse_uploadsList = Lens.lens (\ListMultipartUploadsResponse' {uploadsList} -> uploadsList) (\s@ListMultipartUploadsResponse' {} a -> s {uploadsList = a} :: ListMultipartUploadsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | An opaque string that represents where to continue pagination of the
-- results. You use the marker in a new List Multipart Uploads request to
-- obtain more uploads in the list. If there are no more uploads, this
-- value is @null@.
listMultipartUploadsResponse_marker :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Text)
listMultipartUploadsResponse_marker = Lens.lens (\ListMultipartUploadsResponse' {marker} -> marker) (\s@ListMultipartUploadsResponse' {} a -> s {marker = a} :: ListMultipartUploadsResponse)

-- | The response's http status code.
listMultipartUploadsResponse_httpStatus :: Lens.Lens' ListMultipartUploadsResponse Prelude.Int
listMultipartUploadsResponse_httpStatus = Lens.lens (\ListMultipartUploadsResponse' {httpStatus} -> httpStatus) (\s@ListMultipartUploadsResponse' {} a -> s {httpStatus = a} :: ListMultipartUploadsResponse)

instance Prelude.NFData ListMultipartUploadsResponse
