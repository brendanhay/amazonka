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
-- Module      : Amazonka.S3.ListMultipartUploads
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action lists in-progress multipart uploads. An in-progress
-- multipart upload is a multipart upload that has been initiated using the
-- Initiate Multipart Upload request, but has not yet been completed or
-- aborted.
--
-- This action returns at most 1,000 multipart uploads in the response.
-- 1,000 multipart uploads is the maximum number of uploads a response can
-- include, which is also the default value. You can further limit the
-- number of uploads in a response by specifying the @max-uploads@
-- parameter in the response. If additional multipart uploads satisfy the
-- list criteria, the response will contain an @IsTruncated@ element with
-- the value true. To list the additional multipart uploads, use the
-- @key-marker@ and @upload-id-marker@ request parameters.
--
-- In the response, the uploads are sorted by key. If your application has
-- initiated more than one multipart upload using the same object key, then
-- uploads in the response are first sorted by key. Additionally, uploads
-- are sorted in ascending order within each key by the upload initiation
-- time.
--
-- For more information on multipart uploads, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/uploadobjusingmpu.html Uploading Objects Using Multipart Upload>.
--
-- For information on permissions required to use the multipart upload API,
-- see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload and Permissions>.
--
-- The following operations are related to @ListMultipartUploads@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
-- This operation returns paginated results.
module Amazonka.S3.ListMultipartUploads
  ( -- * Creating a Request
    ListMultipartUploads (..),
    newListMultipartUploads,

    -- * Request Lenses
    listMultipartUploads_expectedBucketOwner,
    listMultipartUploads_maxUploads,
    listMultipartUploads_keyMarker,
    listMultipartUploads_delimiter,
    listMultipartUploads_prefix,
    listMultipartUploads_uploadIdMarker,
    listMultipartUploads_encodingType,
    listMultipartUploads_bucket,

    -- * Destructuring the Response
    ListMultipartUploadsResponse (..),
    newListMultipartUploadsResponse,

    -- * Response Lenses
    listMultipartUploadsResponse_commonPrefixes,
    listMultipartUploadsResponse_uploads,
    listMultipartUploadsResponse_bucket,
    listMultipartUploadsResponse_maxUploads,
    listMultipartUploadsResponse_isTruncated,
    listMultipartUploadsResponse_keyMarker,
    listMultipartUploadsResponse_delimiter,
    listMultipartUploadsResponse_nextUploadIdMarker,
    listMultipartUploadsResponse_prefix,
    listMultipartUploadsResponse_nextKeyMarker,
    listMultipartUploadsResponse_uploadIdMarker,
    listMultipartUploadsResponse_encodingType,
    listMultipartUploadsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newListMultipartUploads' smart constructor.
data ListMultipartUploads = ListMultipartUploads'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Sets the maximum number of multipart uploads, from 1 to 1,000, to return
    -- in the response body. 1,000 is the maximum number of uploads that can be
    -- returned in a response.
    maxUploads :: Prelude.Maybe Prelude.Int,
    -- | Together with upload-id-marker, this parameter specifies the multipart
    -- upload after which listing should begin.
    --
    -- If @upload-id-marker@ is not specified, only the keys lexicographically
    -- greater than the specified @key-marker@ will be included in the list.
    --
    -- If @upload-id-marker@ is specified, any multipart uploads for a key
    -- equal to the @key-marker@ might also be included, provided those
    -- multipart uploads have upload IDs lexicographically greater than the
    -- specified @upload-id-marker@.
    keyMarker :: Prelude.Maybe Prelude.Text,
    -- | Character you use to group keys.
    --
    -- All keys that contain the same string between the prefix, if specified,
    -- and the first occurrence of the delimiter after the prefix are grouped
    -- under a single result element, @CommonPrefixes@. If you don\'t specify
    -- the prefix parameter, then the substring starts at the beginning of the
    -- key. The keys that are grouped under @CommonPrefixes@ result element are
    -- not returned elsewhere in the response.
    delimiter :: Prelude.Maybe Delimiter,
    -- | Lists in-progress uploads only for those keys that begin with the
    -- specified prefix. You can use prefixes to separate a bucket into
    -- different grouping of keys. (You can think of using prefix to make
    -- groups in the same way you\'d use a folder in a file system.)
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Together with key-marker, specifies the multipart upload after which
    -- listing should begin. If key-marker is not specified, the
    -- upload-id-marker parameter is ignored. Otherwise, any multipart uploads
    -- for a key equal to the key-marker might be included in the list only if
    -- they have an upload ID lexicographically greater than the specified
    -- @upload-id-marker@.
    uploadIdMarker :: Prelude.Maybe Prelude.Text,
    encodingType :: Prelude.Maybe EncodingType,
    -- | The name of the bucket to which the multipart upload was initiated.
    --
    -- When using this action with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this action with an access point through the Amazon Web
    -- Services SDKs, you provide the access point ARN in place of the bucket
    -- name. For more information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
    -- in the /Amazon S3 User Guide/.
    --
    -- When using this action with Amazon S3 on Outposts, you must direct
    -- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
    -- takes the form
    -- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
    -- When using this action with S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts bucket ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMultipartUploads' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'listMultipartUploads_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'maxUploads', 'listMultipartUploads_maxUploads' - Sets the maximum number of multipart uploads, from 1 to 1,000, to return
-- in the response body. 1,000 is the maximum number of uploads that can be
-- returned in a response.
--
-- 'keyMarker', 'listMultipartUploads_keyMarker' - Together with upload-id-marker, this parameter specifies the multipart
-- upload after which listing should begin.
--
-- If @upload-id-marker@ is not specified, only the keys lexicographically
-- greater than the specified @key-marker@ will be included in the list.
--
-- If @upload-id-marker@ is specified, any multipart uploads for a key
-- equal to the @key-marker@ might also be included, provided those
-- multipart uploads have upload IDs lexicographically greater than the
-- specified @upload-id-marker@.
--
-- 'delimiter', 'listMultipartUploads_delimiter' - Character you use to group keys.
--
-- All keys that contain the same string between the prefix, if specified,
-- and the first occurrence of the delimiter after the prefix are grouped
-- under a single result element, @CommonPrefixes@. If you don\'t specify
-- the prefix parameter, then the substring starts at the beginning of the
-- key. The keys that are grouped under @CommonPrefixes@ result element are
-- not returned elsewhere in the response.
--
-- 'prefix', 'listMultipartUploads_prefix' - Lists in-progress uploads only for those keys that begin with the
-- specified prefix. You can use prefixes to separate a bucket into
-- different grouping of keys. (You can think of using prefix to make
-- groups in the same way you\'d use a folder in a file system.)
--
-- 'uploadIdMarker', 'listMultipartUploads_uploadIdMarker' - Together with key-marker, specifies the multipart upload after which
-- listing should begin. If key-marker is not specified, the
-- upload-id-marker parameter is ignored. Otherwise, any multipart uploads
-- for a key equal to the key-marker might be included in the list only if
-- they have an upload ID lexicographically greater than the specified
-- @upload-id-marker@.
--
-- 'encodingType', 'listMultipartUploads_encodingType' - Undocumented member.
--
-- 'bucket', 'listMultipartUploads_bucket' - The name of the bucket to which the multipart upload was initiated.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When using this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
newListMultipartUploads ::
  -- | 'bucket'
  BucketName ->
  ListMultipartUploads
newListMultipartUploads pBucket_ =
  ListMultipartUploads'
    { expectedBucketOwner =
        Prelude.Nothing,
      maxUploads = Prelude.Nothing,
      keyMarker = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      prefix = Prelude.Nothing,
      uploadIdMarker = Prelude.Nothing,
      encodingType = Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
listMultipartUploads_expectedBucketOwner :: Lens.Lens' ListMultipartUploads (Prelude.Maybe Prelude.Text)
listMultipartUploads_expectedBucketOwner = Lens.lens (\ListMultipartUploads' {expectedBucketOwner} -> expectedBucketOwner) (\s@ListMultipartUploads' {} a -> s {expectedBucketOwner = a} :: ListMultipartUploads)

-- | Sets the maximum number of multipart uploads, from 1 to 1,000, to return
-- in the response body. 1,000 is the maximum number of uploads that can be
-- returned in a response.
listMultipartUploads_maxUploads :: Lens.Lens' ListMultipartUploads (Prelude.Maybe Prelude.Int)
listMultipartUploads_maxUploads = Lens.lens (\ListMultipartUploads' {maxUploads} -> maxUploads) (\s@ListMultipartUploads' {} a -> s {maxUploads = a} :: ListMultipartUploads)

-- | Together with upload-id-marker, this parameter specifies the multipart
-- upload after which listing should begin.
--
-- If @upload-id-marker@ is not specified, only the keys lexicographically
-- greater than the specified @key-marker@ will be included in the list.
--
-- If @upload-id-marker@ is specified, any multipart uploads for a key
-- equal to the @key-marker@ might also be included, provided those
-- multipart uploads have upload IDs lexicographically greater than the
-- specified @upload-id-marker@.
listMultipartUploads_keyMarker :: Lens.Lens' ListMultipartUploads (Prelude.Maybe Prelude.Text)
listMultipartUploads_keyMarker = Lens.lens (\ListMultipartUploads' {keyMarker} -> keyMarker) (\s@ListMultipartUploads' {} a -> s {keyMarker = a} :: ListMultipartUploads)

-- | Character you use to group keys.
--
-- All keys that contain the same string between the prefix, if specified,
-- and the first occurrence of the delimiter after the prefix are grouped
-- under a single result element, @CommonPrefixes@. If you don\'t specify
-- the prefix parameter, then the substring starts at the beginning of the
-- key. The keys that are grouped under @CommonPrefixes@ result element are
-- not returned elsewhere in the response.
listMultipartUploads_delimiter :: Lens.Lens' ListMultipartUploads (Prelude.Maybe Delimiter)
listMultipartUploads_delimiter = Lens.lens (\ListMultipartUploads' {delimiter} -> delimiter) (\s@ListMultipartUploads' {} a -> s {delimiter = a} :: ListMultipartUploads)

-- | Lists in-progress uploads only for those keys that begin with the
-- specified prefix. You can use prefixes to separate a bucket into
-- different grouping of keys. (You can think of using prefix to make
-- groups in the same way you\'d use a folder in a file system.)
listMultipartUploads_prefix :: Lens.Lens' ListMultipartUploads (Prelude.Maybe Prelude.Text)
listMultipartUploads_prefix = Lens.lens (\ListMultipartUploads' {prefix} -> prefix) (\s@ListMultipartUploads' {} a -> s {prefix = a} :: ListMultipartUploads)

-- | Together with key-marker, specifies the multipart upload after which
-- listing should begin. If key-marker is not specified, the
-- upload-id-marker parameter is ignored. Otherwise, any multipart uploads
-- for a key equal to the key-marker might be included in the list only if
-- they have an upload ID lexicographically greater than the specified
-- @upload-id-marker@.
listMultipartUploads_uploadIdMarker :: Lens.Lens' ListMultipartUploads (Prelude.Maybe Prelude.Text)
listMultipartUploads_uploadIdMarker = Lens.lens (\ListMultipartUploads' {uploadIdMarker} -> uploadIdMarker) (\s@ListMultipartUploads' {} a -> s {uploadIdMarker = a} :: ListMultipartUploads)

-- | Undocumented member.
listMultipartUploads_encodingType :: Lens.Lens' ListMultipartUploads (Prelude.Maybe EncodingType)
listMultipartUploads_encodingType = Lens.lens (\ListMultipartUploads' {encodingType} -> encodingType) (\s@ListMultipartUploads' {} a -> s {encodingType = a} :: ListMultipartUploads)

-- | The name of the bucket to which the multipart upload was initiated.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When using this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
listMultipartUploads_bucket :: Lens.Lens' ListMultipartUploads BucketName
listMultipartUploads_bucket = Lens.lens (\ListMultipartUploads' {bucket} -> bucket) (\s@ListMultipartUploads' {} a -> s {bucket = a} :: ListMultipartUploads)

instance Core.AWSPager ListMultipartUploads where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMultipartUploadsResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listMultipartUploadsResponse_nextKeyMarker
              Prelude.. Lens._Just
        )
        Prelude.&& Prelude.isNothing
          ( rs
              Lens.^? listMultipartUploadsResponse_nextUploadIdMarker
                Prelude.. Lens._Just
          ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMultipartUploads_keyMarker
          Lens..~ rs
          Lens.^? listMultipartUploadsResponse_nextKeyMarker
            Prelude.. Lens._Just
          Prelude.& listMultipartUploads_uploadIdMarker
          Lens..~ rs
          Lens.^? listMultipartUploadsResponse_nextUploadIdMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListMultipartUploads where
  type
    AWSResponse ListMultipartUploads =
      ListMultipartUploadsResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListMultipartUploadsResponse'
            Prelude.<$> (Core.may (Data.parseXMLList "CommonPrefixes") x)
            Prelude.<*> (Core.may (Data.parseXMLList "Upload") x)
            Prelude.<*> (x Data..@? "Bucket")
            Prelude.<*> (x Data..@? "MaxUploads")
            Prelude.<*> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "KeyMarker")
            Prelude.<*> (x Data..@? "Delimiter")
            Prelude.<*> (x Data..@? "NextUploadIdMarker")
            Prelude.<*> (x Data..@? "Prefix")
            Prelude.<*> (x Data..@? "NextKeyMarker")
            Prelude.<*> (x Data..@? "UploadIdMarker")
            Prelude.<*> (x Data..@? "EncodingType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMultipartUploads where
  hashWithSalt _salt ListMultipartUploads' {..} =
    _salt `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` maxUploads
      `Prelude.hashWithSalt` keyMarker
      `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` uploadIdMarker
      `Prelude.hashWithSalt` encodingType
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData ListMultipartUploads where
  rnf ListMultipartUploads' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf maxUploads
      `Prelude.seq` Prelude.rnf keyMarker
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf uploadIdMarker
      `Prelude.seq` Prelude.rnf encodingType
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders ListMultipartUploads where
  toHeaders ListMultipartUploads' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath ListMultipartUploads where
  toPath ListMultipartUploads' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery ListMultipartUploads where
  toQuery ListMultipartUploads' {..} =
    Prelude.mconcat
      [ "max-uploads" Data.=: maxUploads,
        "key-marker" Data.=: keyMarker,
        "delimiter" Data.=: delimiter,
        "prefix" Data.=: prefix,
        "upload-id-marker" Data.=: uploadIdMarker,
        "encoding-type" Data.=: encodingType,
        "uploads"
      ]

-- | /See:/ 'newListMultipartUploadsResponse' smart constructor.
data ListMultipartUploadsResponse = ListMultipartUploadsResponse'
  { -- | If you specify a delimiter in the request, then the result returns each
    -- distinct key prefix containing the delimiter in a @CommonPrefixes@
    -- element. The distinct key prefixes are returned in the @Prefix@ child
    -- element.
    commonPrefixes :: Prelude.Maybe [CommonPrefix],
    -- | Container for elements related to a particular multipart upload. A
    -- response can contain zero or more @Upload@ elements.
    uploads :: Prelude.Maybe [MultipartUpload],
    -- | The name of the bucket to which the multipart upload was initiated. Does
    -- not return the access point ARN or access point alias if used.
    bucket :: Prelude.Maybe BucketName,
    -- | Maximum number of multipart uploads that could have been included in the
    -- response.
    maxUploads :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the returned list of multipart uploads is truncated. A
    -- value of true indicates that the list was truncated. The list can be
    -- truncated if the number of multipart uploads exceeds the limit allowed
    -- or specified by max uploads.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | The key at or after which the listing began.
    keyMarker :: Prelude.Maybe Prelude.Text,
    -- | Contains the delimiter you specified in the request. If you don\'t
    -- specify a delimiter in your request, this element is absent from the
    -- response.
    delimiter :: Prelude.Maybe Delimiter,
    -- | When a list is truncated, this element specifies the value that should
    -- be used for the @upload-id-marker@ request parameter in a subsequent
    -- request.
    nextUploadIdMarker :: Prelude.Maybe Prelude.Text,
    -- | When a prefix is provided in the request, this field contains the
    -- specified prefix. The result contains only keys starting with the
    -- specified prefix.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | When a list is truncated, this element specifies the value that should
    -- be used for the key-marker request parameter in a subsequent request.
    nextKeyMarker :: Prelude.Maybe Prelude.Text,
    -- | Upload ID after which listing began.
    uploadIdMarker :: Prelude.Maybe Prelude.Text,
    -- | Encoding type used by Amazon S3 to encode object keys in the response.
    --
    -- If you specify @encoding-type@ request parameter, Amazon S3 includes
    -- this element in the response, and returns encoded key name values in the
    -- following response elements:
    --
    -- @Delimiter@, @KeyMarker@, @Prefix@, @NextKeyMarker@, @Key@.
    encodingType :: Prelude.Maybe EncodingType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMultipartUploadsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commonPrefixes', 'listMultipartUploadsResponse_commonPrefixes' - If you specify a delimiter in the request, then the result returns each
-- distinct key prefix containing the delimiter in a @CommonPrefixes@
-- element. The distinct key prefixes are returned in the @Prefix@ child
-- element.
--
-- 'uploads', 'listMultipartUploadsResponse_uploads' - Container for elements related to a particular multipart upload. A
-- response can contain zero or more @Upload@ elements.
--
-- 'bucket', 'listMultipartUploadsResponse_bucket' - The name of the bucket to which the multipart upload was initiated. Does
-- not return the access point ARN or access point alias if used.
--
-- 'maxUploads', 'listMultipartUploadsResponse_maxUploads' - Maximum number of multipart uploads that could have been included in the
-- response.
--
-- 'isTruncated', 'listMultipartUploadsResponse_isTruncated' - Indicates whether the returned list of multipart uploads is truncated. A
-- value of true indicates that the list was truncated. The list can be
-- truncated if the number of multipart uploads exceeds the limit allowed
-- or specified by max uploads.
--
-- 'keyMarker', 'listMultipartUploadsResponse_keyMarker' - The key at or after which the listing began.
--
-- 'delimiter', 'listMultipartUploadsResponse_delimiter' - Contains the delimiter you specified in the request. If you don\'t
-- specify a delimiter in your request, this element is absent from the
-- response.
--
-- 'nextUploadIdMarker', 'listMultipartUploadsResponse_nextUploadIdMarker' - When a list is truncated, this element specifies the value that should
-- be used for the @upload-id-marker@ request parameter in a subsequent
-- request.
--
-- 'prefix', 'listMultipartUploadsResponse_prefix' - When a prefix is provided in the request, this field contains the
-- specified prefix. The result contains only keys starting with the
-- specified prefix.
--
-- 'nextKeyMarker', 'listMultipartUploadsResponse_nextKeyMarker' - When a list is truncated, this element specifies the value that should
-- be used for the key-marker request parameter in a subsequent request.
--
-- 'uploadIdMarker', 'listMultipartUploadsResponse_uploadIdMarker' - Upload ID after which listing began.
--
-- 'encodingType', 'listMultipartUploadsResponse_encodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- If you specify @encoding-type@ request parameter, Amazon S3 includes
-- this element in the response, and returns encoded key name values in the
-- following response elements:
--
-- @Delimiter@, @KeyMarker@, @Prefix@, @NextKeyMarker@, @Key@.
--
-- 'httpStatus', 'listMultipartUploadsResponse_httpStatus' - The response's http status code.
newListMultipartUploadsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMultipartUploadsResponse
newListMultipartUploadsResponse pHttpStatus_ =
  ListMultipartUploadsResponse'
    { commonPrefixes =
        Prelude.Nothing,
      uploads = Prelude.Nothing,
      bucket = Prelude.Nothing,
      maxUploads = Prelude.Nothing,
      isTruncated = Prelude.Nothing,
      keyMarker = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      nextUploadIdMarker = Prelude.Nothing,
      prefix = Prelude.Nothing,
      nextKeyMarker = Prelude.Nothing,
      uploadIdMarker = Prelude.Nothing,
      encodingType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you specify a delimiter in the request, then the result returns each
-- distinct key prefix containing the delimiter in a @CommonPrefixes@
-- element. The distinct key prefixes are returned in the @Prefix@ child
-- element.
listMultipartUploadsResponse_commonPrefixes :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe [CommonPrefix])
listMultipartUploadsResponse_commonPrefixes = Lens.lens (\ListMultipartUploadsResponse' {commonPrefixes} -> commonPrefixes) (\s@ListMultipartUploadsResponse' {} a -> s {commonPrefixes = a} :: ListMultipartUploadsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Container for elements related to a particular multipart upload. A
-- response can contain zero or more @Upload@ elements.
listMultipartUploadsResponse_uploads :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe [MultipartUpload])
listMultipartUploadsResponse_uploads = Lens.lens (\ListMultipartUploadsResponse' {uploads} -> uploads) (\s@ListMultipartUploadsResponse' {} a -> s {uploads = a} :: ListMultipartUploadsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the bucket to which the multipart upload was initiated. Does
-- not return the access point ARN or access point alias if used.
listMultipartUploadsResponse_bucket :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe BucketName)
listMultipartUploadsResponse_bucket = Lens.lens (\ListMultipartUploadsResponse' {bucket} -> bucket) (\s@ListMultipartUploadsResponse' {} a -> s {bucket = a} :: ListMultipartUploadsResponse)

-- | Maximum number of multipart uploads that could have been included in the
-- response.
listMultipartUploadsResponse_maxUploads :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Int)
listMultipartUploadsResponse_maxUploads = Lens.lens (\ListMultipartUploadsResponse' {maxUploads} -> maxUploads) (\s@ListMultipartUploadsResponse' {} a -> s {maxUploads = a} :: ListMultipartUploadsResponse)

-- | Indicates whether the returned list of multipart uploads is truncated. A
-- value of true indicates that the list was truncated. The list can be
-- truncated if the number of multipart uploads exceeds the limit allowed
-- or specified by max uploads.
listMultipartUploadsResponse_isTruncated :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Bool)
listMultipartUploadsResponse_isTruncated = Lens.lens (\ListMultipartUploadsResponse' {isTruncated} -> isTruncated) (\s@ListMultipartUploadsResponse' {} a -> s {isTruncated = a} :: ListMultipartUploadsResponse)

-- | The key at or after which the listing began.
listMultipartUploadsResponse_keyMarker :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Text)
listMultipartUploadsResponse_keyMarker = Lens.lens (\ListMultipartUploadsResponse' {keyMarker} -> keyMarker) (\s@ListMultipartUploadsResponse' {} a -> s {keyMarker = a} :: ListMultipartUploadsResponse)

-- | Contains the delimiter you specified in the request. If you don\'t
-- specify a delimiter in your request, this element is absent from the
-- response.
listMultipartUploadsResponse_delimiter :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Delimiter)
listMultipartUploadsResponse_delimiter = Lens.lens (\ListMultipartUploadsResponse' {delimiter} -> delimiter) (\s@ListMultipartUploadsResponse' {} a -> s {delimiter = a} :: ListMultipartUploadsResponse)

-- | When a list is truncated, this element specifies the value that should
-- be used for the @upload-id-marker@ request parameter in a subsequent
-- request.
listMultipartUploadsResponse_nextUploadIdMarker :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Text)
listMultipartUploadsResponse_nextUploadIdMarker = Lens.lens (\ListMultipartUploadsResponse' {nextUploadIdMarker} -> nextUploadIdMarker) (\s@ListMultipartUploadsResponse' {} a -> s {nextUploadIdMarker = a} :: ListMultipartUploadsResponse)

-- | When a prefix is provided in the request, this field contains the
-- specified prefix. The result contains only keys starting with the
-- specified prefix.
listMultipartUploadsResponse_prefix :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Text)
listMultipartUploadsResponse_prefix = Lens.lens (\ListMultipartUploadsResponse' {prefix} -> prefix) (\s@ListMultipartUploadsResponse' {} a -> s {prefix = a} :: ListMultipartUploadsResponse)

-- | When a list is truncated, this element specifies the value that should
-- be used for the key-marker request parameter in a subsequent request.
listMultipartUploadsResponse_nextKeyMarker :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Text)
listMultipartUploadsResponse_nextKeyMarker = Lens.lens (\ListMultipartUploadsResponse' {nextKeyMarker} -> nextKeyMarker) (\s@ListMultipartUploadsResponse' {} a -> s {nextKeyMarker = a} :: ListMultipartUploadsResponse)

-- | Upload ID after which listing began.
listMultipartUploadsResponse_uploadIdMarker :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Text)
listMultipartUploadsResponse_uploadIdMarker = Lens.lens (\ListMultipartUploadsResponse' {uploadIdMarker} -> uploadIdMarker) (\s@ListMultipartUploadsResponse' {} a -> s {uploadIdMarker = a} :: ListMultipartUploadsResponse)

-- | Encoding type used by Amazon S3 to encode object keys in the response.
--
-- If you specify @encoding-type@ request parameter, Amazon S3 includes
-- this element in the response, and returns encoded key name values in the
-- following response elements:
--
-- @Delimiter@, @KeyMarker@, @Prefix@, @NextKeyMarker@, @Key@.
listMultipartUploadsResponse_encodingType :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe EncodingType)
listMultipartUploadsResponse_encodingType = Lens.lens (\ListMultipartUploadsResponse' {encodingType} -> encodingType) (\s@ListMultipartUploadsResponse' {} a -> s {encodingType = a} :: ListMultipartUploadsResponse)

-- | The response's http status code.
listMultipartUploadsResponse_httpStatus :: Lens.Lens' ListMultipartUploadsResponse Prelude.Int
listMultipartUploadsResponse_httpStatus = Lens.lens (\ListMultipartUploadsResponse' {httpStatus} -> httpStatus) (\s@ListMultipartUploadsResponse' {} a -> s {httpStatus = a} :: ListMultipartUploadsResponse)

instance Prelude.NFData ListMultipartUploadsResponse where
  rnf ListMultipartUploadsResponse' {..} =
    Prelude.rnf commonPrefixes
      `Prelude.seq` Prelude.rnf uploads
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf maxUploads
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf keyMarker
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf nextUploadIdMarker
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf nextKeyMarker
      `Prelude.seq` Prelude.rnf uploadIdMarker
      `Prelude.seq` Prelude.rnf encodingType
      `Prelude.seq` Prelude.rnf httpStatus
