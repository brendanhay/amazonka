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
-- Module      : Amazonka.Glacier.ListParts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists the parts of an archive that have been uploaded in
-- a specific multipart upload. You can make this request at any time
-- during an in-progress multipart upload before you complete the upload
-- (see CompleteMultipartUpload. List Parts returns an error for completed
-- uploads. The list returned in the List Parts response is sorted by part
-- range.
--
-- The List Parts operation supports pagination. By default, this operation
-- returns up to 50 uploaded parts in the response. You should always check
-- the response for a @marker@ at which to continue the list; if there are
-- no more items the @marker@ is @null@. To return a list of parts that
-- begins at a specific part, set the @marker@ request parameter to the
-- value you obtained from a previous List Parts request. You can also
-- limit the number of parts returned in the response by specifying the
-- @limit@ parameter in the request.
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
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-list-parts.html List Parts>
-- in the /Amazon Glacier Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.Glacier.ListParts
  ( -- * Creating a Request
    ListParts (..),
    newListParts,

    -- * Request Lenses
    listParts_limit,
    listParts_marker,
    listParts_accountId,
    listParts_vaultName,
    listParts_uploadId,

    -- * Destructuring the Response
    ListPartsResponse (..),
    newListPartsResponse,

    -- * Response Lenses
    listPartsResponse_archiveDescription,
    listPartsResponse_creationDate,
    listPartsResponse_marker,
    listPartsResponse_multipartUploadId,
    listPartsResponse_partSizeInBytes,
    listPartsResponse_parts,
    listPartsResponse_vaultARN,
    listPartsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Provides options for retrieving a list of parts of an archive that have
-- been uploaded in a specific multipart upload.
--
-- /See:/ 'newListParts' smart constructor.
data ListParts = ListParts'
  { -- | The maximum number of parts to be returned. The default limit is 50. The
    -- number of parts returned might be fewer than the specified limit, but
    -- the number of returned parts never exceeds the limit.
    limit :: Prelude.Maybe Prelude.Text,
    -- | An opaque string used for pagination. This value specifies the part at
    -- which the listing of parts should begin. Get the marker value from the
    -- response of a previous List Parts response. You need only include the
    -- marker if you are continuing the pagination of results started in a
    -- previous List Parts request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text,
    -- | The upload ID of the multipart upload.
    uploadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListParts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listParts_limit' - The maximum number of parts to be returned. The default limit is 50. The
-- number of parts returned might be fewer than the specified limit, but
-- the number of returned parts never exceeds the limit.
--
-- 'marker', 'listParts_marker' - An opaque string used for pagination. This value specifies the part at
-- which the listing of parts should begin. Get the marker value from the
-- response of a previous List Parts response. You need only include the
-- marker if you are continuing the pagination of results started in a
-- previous List Parts request.
--
-- 'accountId', 'listParts_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'listParts_vaultName' - The name of the vault.
--
-- 'uploadId', 'listParts_uploadId' - The upload ID of the multipart upload.
newListParts ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  ListParts
newListParts pAccountId_ pVaultName_ pUploadId_ =
  ListParts'
    { limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_,
      uploadId = pUploadId_
    }

-- | The maximum number of parts to be returned. The default limit is 50. The
-- number of parts returned might be fewer than the specified limit, but
-- the number of returned parts never exceeds the limit.
listParts_limit :: Lens.Lens' ListParts (Prelude.Maybe Prelude.Text)
listParts_limit = Lens.lens (\ListParts' {limit} -> limit) (\s@ListParts' {} a -> s {limit = a} :: ListParts)

-- | An opaque string used for pagination. This value specifies the part at
-- which the listing of parts should begin. Get the marker value from the
-- response of a previous List Parts response. You need only include the
-- marker if you are continuing the pagination of results started in a
-- previous List Parts request.
listParts_marker :: Lens.Lens' ListParts (Prelude.Maybe Prelude.Text)
listParts_marker = Lens.lens (\ListParts' {marker} -> marker) (\s@ListParts' {} a -> s {marker = a} :: ListParts)

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
listParts_accountId :: Lens.Lens' ListParts Prelude.Text
listParts_accountId = Lens.lens (\ListParts' {accountId} -> accountId) (\s@ListParts' {} a -> s {accountId = a} :: ListParts)

-- | The name of the vault.
listParts_vaultName :: Lens.Lens' ListParts Prelude.Text
listParts_vaultName = Lens.lens (\ListParts' {vaultName} -> vaultName) (\s@ListParts' {} a -> s {vaultName = a} :: ListParts)

-- | The upload ID of the multipart upload.
listParts_uploadId :: Lens.Lens' ListParts Prelude.Text
listParts_uploadId = Lens.lens (\ListParts' {uploadId} -> uploadId) (\s@ListParts' {} a -> s {uploadId = a} :: ListParts)

instance Core.AWSPager ListParts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPartsResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPartsResponse_parts Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listParts_marker
          Lens..~ rs
          Lens.^? listPartsResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListParts where
  type AWSResponse ListParts = ListPartsResponse
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPartsResponse'
            Prelude.<$> (x Data..?> "ArchiveDescription")
            Prelude.<*> (x Data..?> "CreationDate")
            Prelude.<*> (x Data..?> "Marker")
            Prelude.<*> (x Data..?> "MultipartUploadId")
            Prelude.<*> (x Data..?> "PartSizeInBytes")
            Prelude.<*> (x Data..?> "Parts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "VaultARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListParts where
  hashWithSalt _salt ListParts' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vaultName
      `Prelude.hashWithSalt` uploadId

instance Prelude.NFData ListParts where
  rnf ListParts' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf vaultName
      `Prelude.seq` Prelude.rnf uploadId

instance Data.ToHeaders ListParts where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListParts where
  toPath ListParts' {..} =
    Prelude.mconcat
      [ "/",
        Data.toBS accountId,
        "/vaults/",
        Data.toBS vaultName,
        "/multipart-uploads/",
        Data.toBS uploadId
      ]

instance Data.ToQuery ListParts where
  toQuery ListParts' {..} =
    Prelude.mconcat
      ["limit" Data.=: limit, "marker" Data.=: marker]

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'newListPartsResponse' smart constructor.
data ListPartsResponse = ListPartsResponse'
  { -- | The description of the archive that was specified in the Initiate
    -- Multipart Upload request.
    archiveDescription :: Prelude.Maybe Prelude.Text,
    -- | The UTC time at which the multipart upload was initiated.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | An opaque string that represents where to continue pagination of the
    -- results. You use the marker in a new List Parts request to obtain more
    -- jobs in the list. If there are no more parts, this value is @null@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The ID of the upload to which the parts are associated.
    multipartUploadId :: Prelude.Maybe Prelude.Text,
    -- | The part size in bytes. This is the same value that you specified in the
    -- Initiate Multipart Upload request.
    partSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | A list of the part sizes of the multipart upload. Each object in the
    -- array contains a @RangeBytes@ and @sha256-tree-hash@ name\/value pair.
    parts :: Prelude.Maybe [PartListElement],
    -- | The Amazon Resource Name (ARN) of the vault to which the multipart
    -- upload was initiated.
    vaultARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPartsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveDescription', 'listPartsResponse_archiveDescription' - The description of the archive that was specified in the Initiate
-- Multipart Upload request.
--
-- 'creationDate', 'listPartsResponse_creationDate' - The UTC time at which the multipart upload was initiated.
--
-- 'marker', 'listPartsResponse_marker' - An opaque string that represents where to continue pagination of the
-- results. You use the marker in a new List Parts request to obtain more
-- jobs in the list. If there are no more parts, this value is @null@.
--
-- 'multipartUploadId', 'listPartsResponse_multipartUploadId' - The ID of the upload to which the parts are associated.
--
-- 'partSizeInBytes', 'listPartsResponse_partSizeInBytes' - The part size in bytes. This is the same value that you specified in the
-- Initiate Multipart Upload request.
--
-- 'parts', 'listPartsResponse_parts' - A list of the part sizes of the multipart upload. Each object in the
-- array contains a @RangeBytes@ and @sha256-tree-hash@ name\/value pair.
--
-- 'vaultARN', 'listPartsResponse_vaultARN' - The Amazon Resource Name (ARN) of the vault to which the multipart
-- upload was initiated.
--
-- 'httpStatus', 'listPartsResponse_httpStatus' - The response's http status code.
newListPartsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPartsResponse
newListPartsResponse pHttpStatus_ =
  ListPartsResponse'
    { archiveDescription =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      marker = Prelude.Nothing,
      multipartUploadId = Prelude.Nothing,
      partSizeInBytes = Prelude.Nothing,
      parts = Prelude.Nothing,
      vaultARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the archive that was specified in the Initiate
-- Multipart Upload request.
listPartsResponse_archiveDescription :: Lens.Lens' ListPartsResponse (Prelude.Maybe Prelude.Text)
listPartsResponse_archiveDescription = Lens.lens (\ListPartsResponse' {archiveDescription} -> archiveDescription) (\s@ListPartsResponse' {} a -> s {archiveDescription = a} :: ListPartsResponse)

-- | The UTC time at which the multipart upload was initiated.
listPartsResponse_creationDate :: Lens.Lens' ListPartsResponse (Prelude.Maybe Prelude.Text)
listPartsResponse_creationDate = Lens.lens (\ListPartsResponse' {creationDate} -> creationDate) (\s@ListPartsResponse' {} a -> s {creationDate = a} :: ListPartsResponse)

-- | An opaque string that represents where to continue pagination of the
-- results. You use the marker in a new List Parts request to obtain more
-- jobs in the list. If there are no more parts, this value is @null@.
listPartsResponse_marker :: Lens.Lens' ListPartsResponse (Prelude.Maybe Prelude.Text)
listPartsResponse_marker = Lens.lens (\ListPartsResponse' {marker} -> marker) (\s@ListPartsResponse' {} a -> s {marker = a} :: ListPartsResponse)

-- | The ID of the upload to which the parts are associated.
listPartsResponse_multipartUploadId :: Lens.Lens' ListPartsResponse (Prelude.Maybe Prelude.Text)
listPartsResponse_multipartUploadId = Lens.lens (\ListPartsResponse' {multipartUploadId} -> multipartUploadId) (\s@ListPartsResponse' {} a -> s {multipartUploadId = a} :: ListPartsResponse)

-- | The part size in bytes. This is the same value that you specified in the
-- Initiate Multipart Upload request.
listPartsResponse_partSizeInBytes :: Lens.Lens' ListPartsResponse (Prelude.Maybe Prelude.Integer)
listPartsResponse_partSizeInBytes = Lens.lens (\ListPartsResponse' {partSizeInBytes} -> partSizeInBytes) (\s@ListPartsResponse' {} a -> s {partSizeInBytes = a} :: ListPartsResponse)

-- | A list of the part sizes of the multipart upload. Each object in the
-- array contains a @RangeBytes@ and @sha256-tree-hash@ name\/value pair.
listPartsResponse_parts :: Lens.Lens' ListPartsResponse (Prelude.Maybe [PartListElement])
listPartsResponse_parts = Lens.lens (\ListPartsResponse' {parts} -> parts) (\s@ListPartsResponse' {} a -> s {parts = a} :: ListPartsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the vault to which the multipart
-- upload was initiated.
listPartsResponse_vaultARN :: Lens.Lens' ListPartsResponse (Prelude.Maybe Prelude.Text)
listPartsResponse_vaultARN = Lens.lens (\ListPartsResponse' {vaultARN} -> vaultARN) (\s@ListPartsResponse' {} a -> s {vaultARN = a} :: ListPartsResponse)

-- | The response's http status code.
listPartsResponse_httpStatus :: Lens.Lens' ListPartsResponse Prelude.Int
listPartsResponse_httpStatus = Lens.lens (\ListPartsResponse' {httpStatus} -> httpStatus) (\s@ListPartsResponse' {} a -> s {httpStatus = a} :: ListPartsResponse)

instance Prelude.NFData ListPartsResponse where
  rnf ListPartsResponse' {..} =
    Prelude.rnf archiveDescription
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf multipartUploadId
      `Prelude.seq` Prelude.rnf partSizeInBytes
      `Prelude.seq` Prelude.rnf parts
      `Prelude.seq` Prelude.rnf vaultARN
      `Prelude.seq` Prelude.rnf httpStatus
