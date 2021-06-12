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
-- Module      : Network.AWS.S3.ListObjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns some or all (up to 1,000) of the objects in a bucket. You can
-- use the request parameters as selection criteria to return a subset of
-- the objects in a bucket. A 200 OK response can contain valid or invalid
-- XML. Be sure to design your application to parse the contents of the
-- response and handle it appropriately.
--
-- This API has been revised. We recommend that you use the newer version,
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjectsV2.html ListObjectsV2>,
-- when developing applications. For backward compatibility, Amazon S3
-- continues to support @ListObjects@.
--
-- The following operations are related to @ListObjects@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjectsV2.html ListObjectsV2>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBuckets.html ListBuckets>
--
-- This operation returns paginated results.
module Network.AWS.S3.ListObjects
  ( -- * Creating a Request
    ListObjects (..),
    newListObjects,

    -- * Request Lenses
    listObjects_expectedBucketOwner,
    listObjects_encodingType,
    listObjects_delimiter,
    listObjects_prefix,
    listObjects_maxKeys,
    listObjects_requestPayer,
    listObjects_marker,
    listObjects_bucket,

    -- * Destructuring the Response
    ListObjectsResponse (..),
    newListObjectsResponse,

    -- * Response Lenses
    listObjectsResponse_commonPrefixes,
    listObjectsResponse_encodingType,
    listObjectsResponse_delimiter,
    listObjectsResponse_prefix,
    listObjectsResponse_isTruncated,
    listObjectsResponse_maxKeys,
    listObjectsResponse_nextMarker,
    listObjectsResponse_contents,
    listObjectsResponse_name,
    listObjectsResponse_marker,
    listObjectsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newListObjects' smart constructor.
data ListObjects = ListObjects'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    encodingType :: Core.Maybe EncodingType,
    -- | A delimiter is a character you use to group keys.
    delimiter :: Core.Maybe Delimiter,
    -- | Limits the response to keys that begin with the specified prefix.
    prefix :: Core.Maybe Core.Text,
    -- | Sets the maximum number of keys returned in the response. By default the
    -- API returns up to 1,000 key names. The response might contain fewer keys
    -- but will never contain more.
    maxKeys :: Core.Maybe Core.Int,
    -- | Confirms that the requester knows that she or he will be charged for the
    -- list objects request. Bucket owners need not specify this parameter in
    -- their requests.
    requestPayer :: Core.Maybe RequestPayer,
    -- | Specifies the key to start with when listing objects in a bucket.
    marker :: Core.Maybe Core.Text,
    -- | The name of the bucket containing the objects.
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
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListObjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'listObjects_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'encodingType', 'listObjects_encodingType' - Undocumented member.
--
-- 'delimiter', 'listObjects_delimiter' - A delimiter is a character you use to group keys.
--
-- 'prefix', 'listObjects_prefix' - Limits the response to keys that begin with the specified prefix.
--
-- 'maxKeys', 'listObjects_maxKeys' - Sets the maximum number of keys returned in the response. By default the
-- API returns up to 1,000 key names. The response might contain fewer keys
-- but will never contain more.
--
-- 'requestPayer', 'listObjects_requestPayer' - Confirms that the requester knows that she or he will be charged for the
-- list objects request. Bucket owners need not specify this parameter in
-- their requests.
--
-- 'marker', 'listObjects_marker' - Specifies the key to start with when listing objects in a bucket.
--
-- 'bucket', 'listObjects_bucket' - The name of the bucket containing the objects.
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
newListObjects ::
  -- | 'bucket'
  BucketName ->
  ListObjects
newListObjects pBucket_ =
  ListObjects'
    { expectedBucketOwner = Core.Nothing,
      encodingType = Core.Nothing,
      delimiter = Core.Nothing,
      prefix = Core.Nothing,
      maxKeys = Core.Nothing,
      requestPayer = Core.Nothing,
      marker = Core.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
listObjects_expectedBucketOwner :: Lens.Lens' ListObjects (Core.Maybe Core.Text)
listObjects_expectedBucketOwner = Lens.lens (\ListObjects' {expectedBucketOwner} -> expectedBucketOwner) (\s@ListObjects' {} a -> s {expectedBucketOwner = a} :: ListObjects)

-- | Undocumented member.
listObjects_encodingType :: Lens.Lens' ListObjects (Core.Maybe EncodingType)
listObjects_encodingType = Lens.lens (\ListObjects' {encodingType} -> encodingType) (\s@ListObjects' {} a -> s {encodingType = a} :: ListObjects)

-- | A delimiter is a character you use to group keys.
listObjects_delimiter :: Lens.Lens' ListObjects (Core.Maybe Delimiter)
listObjects_delimiter = Lens.lens (\ListObjects' {delimiter} -> delimiter) (\s@ListObjects' {} a -> s {delimiter = a} :: ListObjects)

-- | Limits the response to keys that begin with the specified prefix.
listObjects_prefix :: Lens.Lens' ListObjects (Core.Maybe Core.Text)
listObjects_prefix = Lens.lens (\ListObjects' {prefix} -> prefix) (\s@ListObjects' {} a -> s {prefix = a} :: ListObjects)

-- | Sets the maximum number of keys returned in the response. By default the
-- API returns up to 1,000 key names. The response might contain fewer keys
-- but will never contain more.
listObjects_maxKeys :: Lens.Lens' ListObjects (Core.Maybe Core.Int)
listObjects_maxKeys = Lens.lens (\ListObjects' {maxKeys} -> maxKeys) (\s@ListObjects' {} a -> s {maxKeys = a} :: ListObjects)

-- | Confirms that the requester knows that she or he will be charged for the
-- list objects request. Bucket owners need not specify this parameter in
-- their requests.
listObjects_requestPayer :: Lens.Lens' ListObjects (Core.Maybe RequestPayer)
listObjects_requestPayer = Lens.lens (\ListObjects' {requestPayer} -> requestPayer) (\s@ListObjects' {} a -> s {requestPayer = a} :: ListObjects)

-- | Specifies the key to start with when listing objects in a bucket.
listObjects_marker :: Lens.Lens' ListObjects (Core.Maybe Core.Text)
listObjects_marker = Lens.lens (\ListObjects' {marker} -> marker) (\s@ListObjects' {} a -> s {marker = a} :: ListObjects)

-- | The name of the bucket containing the objects.
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
listObjects_bucket :: Lens.Lens' ListObjects BucketName
listObjects_bucket = Lens.lens (\ListObjects' {bucket} -> bucket) (\s@ListObjects' {} a -> s {bucket = a} :: ListObjects)

instance Core.AWSPager ListObjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listObjectsResponse_isTruncated Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^. Core.choice
              ( Lens.^?
                  (listObjectsResponse_nextMarker Core.. Lens._Just)
              )
              ( Lens.^?
                  ( listObjectsResponse_contents Core.. Lens._Just
                      Core.. Lens._last
                      Core.. object_key
                  )
              )
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listObjects_marker
          Lens..~ rs
          Lens.^. Core.choice
            ( Lens.^?
                (listObjectsResponse_nextMarker Core.. Lens._Just)
            )
            ( Lens.^?
                ( listObjectsResponse_contents Core.. Lens._Just
                    Core.. Lens._last
                    Core.. object_key
                )
            )

instance Core.AWSRequest ListObjects where
  type AWSResponse ListObjects = ListObjectsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListObjectsResponse'
            Core.<$> (Core.may (Core.parseXMLList "CommonPrefixes") x)
            Core.<*> (x Core..@? "EncodingType")
            Core.<*> (x Core..@? "Delimiter")
            Core.<*> (x Core..@? "Prefix")
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> (x Core..@? "MaxKeys")
            Core.<*> (x Core..@? "NextMarker")
            Core.<*> (Core.may (Core.parseXMLList "Contents") x)
            Core.<*> (x Core..@? "Name")
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListObjects

instance Core.NFData ListObjects

instance Core.ToHeaders ListObjects where
  toHeaders ListObjects' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "x-amz-request-payer" Core.=# requestPayer
      ]

instance Core.ToPath ListObjects where
  toPath ListObjects' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery ListObjects where
  toQuery ListObjects' {..} =
    Core.mconcat
      [ "encoding-type" Core.=: encodingType,
        "delimiter" Core.=: delimiter,
        "prefix" Core.=: prefix,
        "max-keys" Core.=: maxKeys,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newListObjectsResponse' smart constructor.
data ListObjectsResponse = ListObjectsResponse'
  { -- | All of the keys (up to 1,000) rolled up in a common prefix count as a
    -- single return when calculating the number of returns.
    --
    -- A response can contain CommonPrefixes only if you specify a delimiter.
    --
    -- CommonPrefixes contains all (if there are any) keys between Prefix and
    -- the next occurrence of the string specified by the delimiter.
    --
    -- CommonPrefixes lists keys that act like subdirectories in the directory
    -- specified by Prefix.
    --
    -- For example, if the prefix is notes\/ and the delimiter is a slash (\/)
    -- as in notes\/summer\/july, the common prefix is notes\/summer\/. All of
    -- the keys that roll up into a common prefix count as a single return when
    -- calculating the number of returns.
    commonPrefixes :: Core.Maybe [CommonPrefix],
    -- | Encoding type used by Amazon S3 to encode object keys in the response.
    encodingType :: Core.Maybe EncodingType,
    -- | Causes keys that contain the same string between the prefix and the
    -- first occurrence of the delimiter to be rolled up into a single result
    -- element in the @CommonPrefixes@ collection. These rolled-up keys are not
    -- returned elsewhere in the response. Each rolled-up result counts as only
    -- one return against the @MaxKeys@ value.
    delimiter :: Core.Maybe Delimiter,
    -- | Keys that begin with the indicated prefix.
    prefix :: Core.Maybe Core.Text,
    -- | A flag that indicates whether Amazon S3 returned all of the results that
    -- satisfied the search criteria.
    isTruncated :: Core.Maybe Core.Bool,
    -- | The maximum number of keys returned in the response body.
    maxKeys :: Core.Maybe Core.Int,
    -- | When response is truncated (the IsTruncated element value in the
    -- response is true), you can use the key name in this field as marker in
    -- the subsequent request to get next set of objects. Amazon S3 lists
    -- objects in alphabetical order Note: This element is returned only if you
    -- have delimiter request parameter specified. If response does not include
    -- the NextMarker and it is truncated, you can use the value of the last
    -- Key in the response as the marker in the subsequent request to get the
    -- next set of object keys.
    nextMarker :: Core.Maybe Core.Text,
    -- | Metadata about each object returned.
    contents :: Core.Maybe [Object],
    -- | The bucket name.
    name :: Core.Maybe BucketName,
    -- | Indicates where in the bucket listing begins. Marker is included in the
    -- response if it was sent with the request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListObjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commonPrefixes', 'listObjectsResponse_commonPrefixes' - All of the keys (up to 1,000) rolled up in a common prefix count as a
-- single return when calculating the number of returns.
--
-- A response can contain CommonPrefixes only if you specify a delimiter.
--
-- CommonPrefixes contains all (if there are any) keys between Prefix and
-- the next occurrence of the string specified by the delimiter.
--
-- CommonPrefixes lists keys that act like subdirectories in the directory
-- specified by Prefix.
--
-- For example, if the prefix is notes\/ and the delimiter is a slash (\/)
-- as in notes\/summer\/july, the common prefix is notes\/summer\/. All of
-- the keys that roll up into a common prefix count as a single return when
-- calculating the number of returns.
--
-- 'encodingType', 'listObjectsResponse_encodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- 'delimiter', 'listObjectsResponse_delimiter' - Causes keys that contain the same string between the prefix and the
-- first occurrence of the delimiter to be rolled up into a single result
-- element in the @CommonPrefixes@ collection. These rolled-up keys are not
-- returned elsewhere in the response. Each rolled-up result counts as only
-- one return against the @MaxKeys@ value.
--
-- 'prefix', 'listObjectsResponse_prefix' - Keys that begin with the indicated prefix.
--
-- 'isTruncated', 'listObjectsResponse_isTruncated' - A flag that indicates whether Amazon S3 returned all of the results that
-- satisfied the search criteria.
--
-- 'maxKeys', 'listObjectsResponse_maxKeys' - The maximum number of keys returned in the response body.
--
-- 'nextMarker', 'listObjectsResponse_nextMarker' - When response is truncated (the IsTruncated element value in the
-- response is true), you can use the key name in this field as marker in
-- the subsequent request to get next set of objects. Amazon S3 lists
-- objects in alphabetical order Note: This element is returned only if you
-- have delimiter request parameter specified. If response does not include
-- the NextMarker and it is truncated, you can use the value of the last
-- Key in the response as the marker in the subsequent request to get the
-- next set of object keys.
--
-- 'contents', 'listObjectsResponse_contents' - Metadata about each object returned.
--
-- 'name', 'listObjectsResponse_name' - The bucket name.
--
-- 'marker', 'listObjectsResponse_marker' - Indicates where in the bucket listing begins. Marker is included in the
-- response if it was sent with the request.
--
-- 'httpStatus', 'listObjectsResponse_httpStatus' - The response's http status code.
newListObjectsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListObjectsResponse
newListObjectsResponse pHttpStatus_ =
  ListObjectsResponse'
    { commonPrefixes = Core.Nothing,
      encodingType = Core.Nothing,
      delimiter = Core.Nothing,
      prefix = Core.Nothing,
      isTruncated = Core.Nothing,
      maxKeys = Core.Nothing,
      nextMarker = Core.Nothing,
      contents = Core.Nothing,
      name = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | All of the keys (up to 1,000) rolled up in a common prefix count as a
-- single return when calculating the number of returns.
--
-- A response can contain CommonPrefixes only if you specify a delimiter.
--
-- CommonPrefixes contains all (if there are any) keys between Prefix and
-- the next occurrence of the string specified by the delimiter.
--
-- CommonPrefixes lists keys that act like subdirectories in the directory
-- specified by Prefix.
--
-- For example, if the prefix is notes\/ and the delimiter is a slash (\/)
-- as in notes\/summer\/july, the common prefix is notes\/summer\/. All of
-- the keys that roll up into a common prefix count as a single return when
-- calculating the number of returns.
listObjectsResponse_commonPrefixes :: Lens.Lens' ListObjectsResponse (Core.Maybe [CommonPrefix])
listObjectsResponse_commonPrefixes = Lens.lens (\ListObjectsResponse' {commonPrefixes} -> commonPrefixes) (\s@ListObjectsResponse' {} a -> s {commonPrefixes = a} :: ListObjectsResponse) Core.. Lens.mapping Lens._Coerce

-- | Encoding type used by Amazon S3 to encode object keys in the response.
listObjectsResponse_encodingType :: Lens.Lens' ListObjectsResponse (Core.Maybe EncodingType)
listObjectsResponse_encodingType = Lens.lens (\ListObjectsResponse' {encodingType} -> encodingType) (\s@ListObjectsResponse' {} a -> s {encodingType = a} :: ListObjectsResponse)

-- | Causes keys that contain the same string between the prefix and the
-- first occurrence of the delimiter to be rolled up into a single result
-- element in the @CommonPrefixes@ collection. These rolled-up keys are not
-- returned elsewhere in the response. Each rolled-up result counts as only
-- one return against the @MaxKeys@ value.
listObjectsResponse_delimiter :: Lens.Lens' ListObjectsResponse (Core.Maybe Delimiter)
listObjectsResponse_delimiter = Lens.lens (\ListObjectsResponse' {delimiter} -> delimiter) (\s@ListObjectsResponse' {} a -> s {delimiter = a} :: ListObjectsResponse)

-- | Keys that begin with the indicated prefix.
listObjectsResponse_prefix :: Lens.Lens' ListObjectsResponse (Core.Maybe Core.Text)
listObjectsResponse_prefix = Lens.lens (\ListObjectsResponse' {prefix} -> prefix) (\s@ListObjectsResponse' {} a -> s {prefix = a} :: ListObjectsResponse)

-- | A flag that indicates whether Amazon S3 returned all of the results that
-- satisfied the search criteria.
listObjectsResponse_isTruncated :: Lens.Lens' ListObjectsResponse (Core.Maybe Core.Bool)
listObjectsResponse_isTruncated = Lens.lens (\ListObjectsResponse' {isTruncated} -> isTruncated) (\s@ListObjectsResponse' {} a -> s {isTruncated = a} :: ListObjectsResponse)

-- | The maximum number of keys returned in the response body.
listObjectsResponse_maxKeys :: Lens.Lens' ListObjectsResponse (Core.Maybe Core.Int)
listObjectsResponse_maxKeys = Lens.lens (\ListObjectsResponse' {maxKeys} -> maxKeys) (\s@ListObjectsResponse' {} a -> s {maxKeys = a} :: ListObjectsResponse)

-- | When response is truncated (the IsTruncated element value in the
-- response is true), you can use the key name in this field as marker in
-- the subsequent request to get next set of objects. Amazon S3 lists
-- objects in alphabetical order Note: This element is returned only if you
-- have delimiter request parameter specified. If response does not include
-- the NextMarker and it is truncated, you can use the value of the last
-- Key in the response as the marker in the subsequent request to get the
-- next set of object keys.
listObjectsResponse_nextMarker :: Lens.Lens' ListObjectsResponse (Core.Maybe Core.Text)
listObjectsResponse_nextMarker = Lens.lens (\ListObjectsResponse' {nextMarker} -> nextMarker) (\s@ListObjectsResponse' {} a -> s {nextMarker = a} :: ListObjectsResponse)

-- | Metadata about each object returned.
listObjectsResponse_contents :: Lens.Lens' ListObjectsResponse (Core.Maybe [Object])
listObjectsResponse_contents = Lens.lens (\ListObjectsResponse' {contents} -> contents) (\s@ListObjectsResponse' {} a -> s {contents = a} :: ListObjectsResponse) Core.. Lens.mapping Lens._Coerce

-- | The bucket name.
listObjectsResponse_name :: Lens.Lens' ListObjectsResponse (Core.Maybe BucketName)
listObjectsResponse_name = Lens.lens (\ListObjectsResponse' {name} -> name) (\s@ListObjectsResponse' {} a -> s {name = a} :: ListObjectsResponse)

-- | Indicates where in the bucket listing begins. Marker is included in the
-- response if it was sent with the request.
listObjectsResponse_marker :: Lens.Lens' ListObjectsResponse (Core.Maybe Core.Text)
listObjectsResponse_marker = Lens.lens (\ListObjectsResponse' {marker} -> marker) (\s@ListObjectsResponse' {} a -> s {marker = a} :: ListObjectsResponse)

-- | The response's http status code.
listObjectsResponse_httpStatus :: Lens.Lens' ListObjectsResponse Core.Int
listObjectsResponse_httpStatus = Lens.lens (\ListObjectsResponse' {httpStatus} -> httpStatus) (\s@ListObjectsResponse' {} a -> s {httpStatus = a} :: ListObjectsResponse)

instance Core.NFData ListObjectsResponse
