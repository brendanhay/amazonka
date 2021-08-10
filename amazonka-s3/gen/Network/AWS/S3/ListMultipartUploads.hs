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
-- Module      : Network.AWS.S3.ListMultipartUploads
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists in-progress multipart uploads. An in-progress
-- multipart upload is a multipart upload that has been initiated using the
-- Initiate Multipart Upload request, but has not yet been completed or
-- aborted.
--
-- This operation returns at most 1,000 multipart uploads in the response.
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
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions>.
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
module Network.AWS.S3.ListMultipartUploads
  ( -- * Creating a Request
    ListMultipartUploads (..),
    newListMultipartUploads,

    -- * Request Lenses
    listMultipartUploads_expectedBucketOwner,
    listMultipartUploads_encodingType,
    listMultipartUploads_delimiter,
    listMultipartUploads_prefix,
    listMultipartUploads_keyMarker,
    listMultipartUploads_maxUploads,
    listMultipartUploads_uploadIdMarker,
    listMultipartUploads_bucket,

    -- * Destructuring the Response
    ListMultipartUploadsResponse (..),
    newListMultipartUploadsResponse,

    -- * Response Lenses
    listMultipartUploadsResponse_commonPrefixes,
    listMultipartUploadsResponse_encodingType,
    listMultipartUploadsResponse_delimiter,
    listMultipartUploadsResponse_uploads,
    listMultipartUploadsResponse_prefix,
    listMultipartUploadsResponse_isTruncated,
    listMultipartUploadsResponse_keyMarker,
    listMultipartUploadsResponse_nextKeyMarker,
    listMultipartUploadsResponse_maxUploads,
    listMultipartUploadsResponse_uploadIdMarker,
    listMultipartUploadsResponse_bucket,
    listMultipartUploadsResponse_nextUploadIdMarker,
    listMultipartUploadsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newListMultipartUploads' smart constructor.
data ListMultipartUploads = ListMultipartUploads'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    encodingType :: Prelude.Maybe EncodingType,
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
    -- | Sets the maximum number of multipart uploads, from 1 to 1,000, to return
    -- in the response body. 1,000 is the maximum number of uploads that can be
    -- returned in a response.
    maxUploads :: Prelude.Maybe Prelude.Int,
    -- | Together with key-marker, specifies the multipart upload after which
    -- listing should begin. If key-marker is not specified, the
    -- upload-id-marker parameter is ignored. Otherwise, any multipart uploads
    -- for a key equal to the key-marker might be included in the list only if
    -- they have an upload ID lexicographically greater than the specified
    -- @upload-id-marker@.
    uploadIdMarker :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket to which the multipart upload was initiated.
    --
    -- When using this API with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this operation with an access point through the AWS SDKs, you
    -- provide the access point ARN in place of the bucket name. For more
    -- information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    --
    -- When using this API with Amazon S3 on Outposts, you must direct requests
    -- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
    -- form
    -- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
    -- When using this operation using S3 on Outposts through the AWS SDKs, you
    -- provide the Outposts bucket ARN in place of the bucket name. For more
    -- information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
    -- in the /Amazon Simple Storage Service Developer Guide/.
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
-- 'expectedBucketOwner', 'listMultipartUploads_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'encodingType', 'listMultipartUploads_encodingType' - Undocumented member.
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
-- 'maxUploads', 'listMultipartUploads_maxUploads' - Sets the maximum number of multipart uploads, from 1 to 1,000, to return
-- in the response body. 1,000 is the maximum number of uploads that can be
-- returned in a response.
--
-- 'uploadIdMarker', 'listMultipartUploads_uploadIdMarker' - Together with key-marker, specifies the multipart upload after which
-- listing should begin. If key-marker is not specified, the
-- upload-id-marker parameter is ignored. Otherwise, any multipart uploads
-- for a key equal to the key-marker might be included in the list only if
-- they have an upload ID lexicographically greater than the specified
-- @upload-id-marker@.
--
-- 'bucket', 'listMultipartUploads_bucket' - The name of the bucket to which the multipart upload was initiated.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
newListMultipartUploads ::
  -- | 'bucket'
  BucketName ->
  ListMultipartUploads
newListMultipartUploads pBucket_ =
  ListMultipartUploads'
    { expectedBucketOwner =
        Prelude.Nothing,
      encodingType = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      prefix = Prelude.Nothing,
      keyMarker = Prelude.Nothing,
      maxUploads = Prelude.Nothing,
      uploadIdMarker = Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
listMultipartUploads_expectedBucketOwner :: Lens.Lens' ListMultipartUploads (Prelude.Maybe Prelude.Text)
listMultipartUploads_expectedBucketOwner = Lens.lens (\ListMultipartUploads' {expectedBucketOwner} -> expectedBucketOwner) (\s@ListMultipartUploads' {} a -> s {expectedBucketOwner = a} :: ListMultipartUploads)

-- | Undocumented member.
listMultipartUploads_encodingType :: Lens.Lens' ListMultipartUploads (Prelude.Maybe EncodingType)
listMultipartUploads_encodingType = Lens.lens (\ListMultipartUploads' {encodingType} -> encodingType) (\s@ListMultipartUploads' {} a -> s {encodingType = a} :: ListMultipartUploads)

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

-- | Sets the maximum number of multipart uploads, from 1 to 1,000, to return
-- in the response body. 1,000 is the maximum number of uploads that can be
-- returned in a response.
listMultipartUploads_maxUploads :: Lens.Lens' ListMultipartUploads (Prelude.Maybe Prelude.Int)
listMultipartUploads_maxUploads = Lens.lens (\ListMultipartUploads' {maxUploads} -> maxUploads) (\s@ListMultipartUploads' {} a -> s {maxUploads = a} :: ListMultipartUploads)

-- | Together with key-marker, specifies the multipart upload after which
-- listing should begin. If key-marker is not specified, the
-- upload-id-marker parameter is ignored. Otherwise, any multipart uploads
-- for a key equal to the key-marker might be included in the list only if
-- they have an upload ID lexicographically greater than the specified
-- @upload-id-marker@.
listMultipartUploads_uploadIdMarker :: Lens.Lens' ListMultipartUploads (Prelude.Maybe Prelude.Text)
listMultipartUploads_uploadIdMarker = Lens.lens (\ListMultipartUploads' {uploadIdMarker} -> uploadIdMarker) (\s@ListMultipartUploads' {} a -> s {uploadIdMarker = a} :: ListMultipartUploads)

-- | The name of the bucket to which the multipart upload was initiated.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
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
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListMultipartUploadsResponse'
            Prelude.<$> (Core.may (Core.parseXMLList "CommonPrefixes") x)
            Prelude.<*> (x Core..@? "EncodingType")
            Prelude.<*> (x Core..@? "Delimiter")
            Prelude.<*> (Core.may (Core.parseXMLList "Upload") x)
            Prelude.<*> (x Core..@? "Prefix")
            Prelude.<*> (x Core..@? "IsTruncated")
            Prelude.<*> (x Core..@? "KeyMarker")
            Prelude.<*> (x Core..@? "NextKeyMarker")
            Prelude.<*> (x Core..@? "MaxUploads")
            Prelude.<*> (x Core..@? "UploadIdMarker")
            Prelude.<*> (x Core..@? "Bucket")
            Prelude.<*> (x Core..@? "NextUploadIdMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMultipartUploads

instance Prelude.NFData ListMultipartUploads

instance Core.ToHeaders ListMultipartUploads where
  toHeaders ListMultipartUploads' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath ListMultipartUploads where
  toPath ListMultipartUploads' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery ListMultipartUploads where
  toQuery ListMultipartUploads' {..} =
    Prelude.mconcat
      [ "encoding-type" Core.=: encodingType,
        "delimiter" Core.=: delimiter,
        "prefix" Core.=: prefix,
        "key-marker" Core.=: keyMarker,
        "max-uploads" Core.=: maxUploads,
        "upload-id-marker" Core.=: uploadIdMarker,
        "uploads"
      ]

-- | /See:/ 'newListMultipartUploadsResponse' smart constructor.
data ListMultipartUploadsResponse = ListMultipartUploadsResponse'
  { -- | If you specify a delimiter in the request, then the result returns each
    -- distinct key prefix containing the delimiter in a @CommonPrefixes@
    -- element. The distinct key prefixes are returned in the @Prefix@ child
    -- element.
    commonPrefixes :: Prelude.Maybe [CommonPrefix],
    -- | Encoding type used by Amazon S3 to encode object keys in the response.
    --
    -- If you specify @encoding-type@ request parameter, Amazon S3 includes
    -- this element in the response, and returns encoded key name values in the
    -- following response elements:
    --
    -- @Delimiter@, @KeyMarker@, @Prefix@, @NextKeyMarker@, @Key@.
    encodingType :: Prelude.Maybe EncodingType,
    -- | Contains the delimiter you specified in the request. If you don\'t
    -- specify a delimiter in your request, this element is absent from the
    -- response.
    delimiter :: Prelude.Maybe Delimiter,
    -- | Container for elements related to a particular multipart upload. A
    -- response can contain zero or more @Upload@ elements.
    uploads :: Prelude.Maybe [MultipartUpload],
    -- | When a prefix is provided in the request, this field contains the
    -- specified prefix. The result contains only keys starting with the
    -- specified prefix.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the returned list of multipart uploads is truncated. A
    -- value of true indicates that the list was truncated. The list can be
    -- truncated if the number of multipart uploads exceeds the limit allowed
    -- or specified by max uploads.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | The key at or after which the listing began.
    keyMarker :: Prelude.Maybe Prelude.Text,
    -- | When a list is truncated, this element specifies the value that should
    -- be used for the key-marker request parameter in a subsequent request.
    nextKeyMarker :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of multipart uploads that could have been included in the
    -- response.
    maxUploads :: Prelude.Maybe Prelude.Int,
    -- | Upload ID after which listing began.
    uploadIdMarker :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket to which the multipart upload was initiated.
    bucket :: Prelude.Maybe BucketName,
    -- | When a list is truncated, this element specifies the value that should
    -- be used for the @upload-id-marker@ request parameter in a subsequent
    -- request.
    nextUploadIdMarker :: Prelude.Maybe Prelude.Text,
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
-- 'encodingType', 'listMultipartUploadsResponse_encodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- If you specify @encoding-type@ request parameter, Amazon S3 includes
-- this element in the response, and returns encoded key name values in the
-- following response elements:
--
-- @Delimiter@, @KeyMarker@, @Prefix@, @NextKeyMarker@, @Key@.
--
-- 'delimiter', 'listMultipartUploadsResponse_delimiter' - Contains the delimiter you specified in the request. If you don\'t
-- specify a delimiter in your request, this element is absent from the
-- response.
--
-- 'uploads', 'listMultipartUploadsResponse_uploads' - Container for elements related to a particular multipart upload. A
-- response can contain zero or more @Upload@ elements.
--
-- 'prefix', 'listMultipartUploadsResponse_prefix' - When a prefix is provided in the request, this field contains the
-- specified prefix. The result contains only keys starting with the
-- specified prefix.
--
-- 'isTruncated', 'listMultipartUploadsResponse_isTruncated' - Indicates whether the returned list of multipart uploads is truncated. A
-- value of true indicates that the list was truncated. The list can be
-- truncated if the number of multipart uploads exceeds the limit allowed
-- or specified by max uploads.
--
-- 'keyMarker', 'listMultipartUploadsResponse_keyMarker' - The key at or after which the listing began.
--
-- 'nextKeyMarker', 'listMultipartUploadsResponse_nextKeyMarker' - When a list is truncated, this element specifies the value that should
-- be used for the key-marker request parameter in a subsequent request.
--
-- 'maxUploads', 'listMultipartUploadsResponse_maxUploads' - Maximum number of multipart uploads that could have been included in the
-- response.
--
-- 'uploadIdMarker', 'listMultipartUploadsResponse_uploadIdMarker' - Upload ID after which listing began.
--
-- 'bucket', 'listMultipartUploadsResponse_bucket' - The name of the bucket to which the multipart upload was initiated.
--
-- 'nextUploadIdMarker', 'listMultipartUploadsResponse_nextUploadIdMarker' - When a list is truncated, this element specifies the value that should
-- be used for the @upload-id-marker@ request parameter in a subsequent
-- request.
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
      encodingType = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      uploads = Prelude.Nothing,
      prefix = Prelude.Nothing,
      isTruncated = Prelude.Nothing,
      keyMarker = Prelude.Nothing,
      nextKeyMarker = Prelude.Nothing,
      maxUploads = Prelude.Nothing,
      uploadIdMarker = Prelude.Nothing,
      bucket = Prelude.Nothing,
      nextUploadIdMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you specify a delimiter in the request, then the result returns each
-- distinct key prefix containing the delimiter in a @CommonPrefixes@
-- element. The distinct key prefixes are returned in the @Prefix@ child
-- element.
listMultipartUploadsResponse_commonPrefixes :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe [CommonPrefix])
listMultipartUploadsResponse_commonPrefixes = Lens.lens (\ListMultipartUploadsResponse' {commonPrefixes} -> commonPrefixes) (\s@ListMultipartUploadsResponse' {} a -> s {commonPrefixes = a} :: ListMultipartUploadsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Encoding type used by Amazon S3 to encode object keys in the response.
--
-- If you specify @encoding-type@ request parameter, Amazon S3 includes
-- this element in the response, and returns encoded key name values in the
-- following response elements:
--
-- @Delimiter@, @KeyMarker@, @Prefix@, @NextKeyMarker@, @Key@.
listMultipartUploadsResponse_encodingType :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe EncodingType)
listMultipartUploadsResponse_encodingType = Lens.lens (\ListMultipartUploadsResponse' {encodingType} -> encodingType) (\s@ListMultipartUploadsResponse' {} a -> s {encodingType = a} :: ListMultipartUploadsResponse)

-- | Contains the delimiter you specified in the request. If you don\'t
-- specify a delimiter in your request, this element is absent from the
-- response.
listMultipartUploadsResponse_delimiter :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Delimiter)
listMultipartUploadsResponse_delimiter = Lens.lens (\ListMultipartUploadsResponse' {delimiter} -> delimiter) (\s@ListMultipartUploadsResponse' {} a -> s {delimiter = a} :: ListMultipartUploadsResponse)

-- | Container for elements related to a particular multipart upload. A
-- response can contain zero or more @Upload@ elements.
listMultipartUploadsResponse_uploads :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe [MultipartUpload])
listMultipartUploadsResponse_uploads = Lens.lens (\ListMultipartUploadsResponse' {uploads} -> uploads) (\s@ListMultipartUploadsResponse' {} a -> s {uploads = a} :: ListMultipartUploadsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | When a prefix is provided in the request, this field contains the
-- specified prefix. The result contains only keys starting with the
-- specified prefix.
listMultipartUploadsResponse_prefix :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Text)
listMultipartUploadsResponse_prefix = Lens.lens (\ListMultipartUploadsResponse' {prefix} -> prefix) (\s@ListMultipartUploadsResponse' {} a -> s {prefix = a} :: ListMultipartUploadsResponse)

-- | Indicates whether the returned list of multipart uploads is truncated. A
-- value of true indicates that the list was truncated. The list can be
-- truncated if the number of multipart uploads exceeds the limit allowed
-- or specified by max uploads.
listMultipartUploadsResponse_isTruncated :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Bool)
listMultipartUploadsResponse_isTruncated = Lens.lens (\ListMultipartUploadsResponse' {isTruncated} -> isTruncated) (\s@ListMultipartUploadsResponse' {} a -> s {isTruncated = a} :: ListMultipartUploadsResponse)

-- | The key at or after which the listing began.
listMultipartUploadsResponse_keyMarker :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Text)
listMultipartUploadsResponse_keyMarker = Lens.lens (\ListMultipartUploadsResponse' {keyMarker} -> keyMarker) (\s@ListMultipartUploadsResponse' {} a -> s {keyMarker = a} :: ListMultipartUploadsResponse)

-- | When a list is truncated, this element specifies the value that should
-- be used for the key-marker request parameter in a subsequent request.
listMultipartUploadsResponse_nextKeyMarker :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Text)
listMultipartUploadsResponse_nextKeyMarker = Lens.lens (\ListMultipartUploadsResponse' {nextKeyMarker} -> nextKeyMarker) (\s@ListMultipartUploadsResponse' {} a -> s {nextKeyMarker = a} :: ListMultipartUploadsResponse)

-- | Maximum number of multipart uploads that could have been included in the
-- response.
listMultipartUploadsResponse_maxUploads :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Int)
listMultipartUploadsResponse_maxUploads = Lens.lens (\ListMultipartUploadsResponse' {maxUploads} -> maxUploads) (\s@ListMultipartUploadsResponse' {} a -> s {maxUploads = a} :: ListMultipartUploadsResponse)

-- | Upload ID after which listing began.
listMultipartUploadsResponse_uploadIdMarker :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Text)
listMultipartUploadsResponse_uploadIdMarker = Lens.lens (\ListMultipartUploadsResponse' {uploadIdMarker} -> uploadIdMarker) (\s@ListMultipartUploadsResponse' {} a -> s {uploadIdMarker = a} :: ListMultipartUploadsResponse)

-- | The name of the bucket to which the multipart upload was initiated.
listMultipartUploadsResponse_bucket :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe BucketName)
listMultipartUploadsResponse_bucket = Lens.lens (\ListMultipartUploadsResponse' {bucket} -> bucket) (\s@ListMultipartUploadsResponse' {} a -> s {bucket = a} :: ListMultipartUploadsResponse)

-- | When a list is truncated, this element specifies the value that should
-- be used for the @upload-id-marker@ request parameter in a subsequent
-- request.
listMultipartUploadsResponse_nextUploadIdMarker :: Lens.Lens' ListMultipartUploadsResponse (Prelude.Maybe Prelude.Text)
listMultipartUploadsResponse_nextUploadIdMarker = Lens.lens (\ListMultipartUploadsResponse' {nextUploadIdMarker} -> nextUploadIdMarker) (\s@ListMultipartUploadsResponse' {} a -> s {nextUploadIdMarker = a} :: ListMultipartUploadsResponse)

-- | The response's http status code.
listMultipartUploadsResponse_httpStatus :: Lens.Lens' ListMultipartUploadsResponse Prelude.Int
listMultipartUploadsResponse_httpStatus = Lens.lens (\ListMultipartUploadsResponse' {httpStatus} -> httpStatus) (\s@ListMultipartUploadsResponse' {} a -> s {httpStatus = a} :: ListMultipartUploadsResponse)

instance Prelude.NFData ListMultipartUploadsResponse
