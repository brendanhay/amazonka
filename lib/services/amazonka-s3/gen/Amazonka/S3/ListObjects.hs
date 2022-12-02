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
-- Module      : Amazonka.S3.ListObjects
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- This action has been revised. We recommend that you use the newer
-- version,
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
module Amazonka.S3.ListObjects
  ( -- * Creating a Request
    ListObjects (..),
    newListObjects,

    -- * Request Lenses
    listObjects_maxKeys,
    listObjects_marker,
    listObjects_expectedBucketOwner,
    listObjects_requestPayer,
    listObjects_delimiter,
    listObjects_prefix,
    listObjects_encodingType,
    listObjects_bucket,

    -- * Destructuring the Response
    ListObjectsResponse (..),
    newListObjectsResponse,

    -- * Response Lenses
    listObjectsResponse_maxKeys,
    listObjectsResponse_commonPrefixes,
    listObjectsResponse_name,
    listObjectsResponse_marker,
    listObjectsResponse_contents,
    listObjectsResponse_isTruncated,
    listObjectsResponse_delimiter,
    listObjectsResponse_nextMarker,
    listObjectsResponse_prefix,
    listObjectsResponse_encodingType,
    listObjectsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newListObjects' smart constructor.
data ListObjects = ListObjects'
  { -- | Sets the maximum number of keys returned in the response. By default the
    -- action returns up to 1,000 key names. The response might contain fewer
    -- keys but will never contain more.
    maxKeys :: Prelude.Maybe Prelude.Int,
    -- | Marker is where you want Amazon S3 to start listing from. Amazon S3
    -- starts listing after this specified key. Marker can be any key in the
    -- bucket.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Confirms that the requester knows that she or he will be charged for the
    -- list objects request. Bucket owners need not specify this parameter in
    -- their requests.
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | A delimiter is a character you use to group keys.
    delimiter :: Prelude.Maybe Delimiter,
    -- | Limits the response to keys that begin with the specified prefix.
    prefix :: Prelude.Maybe Prelude.Text,
    encodingType :: Prelude.Maybe EncodingType,
    -- | The name of the bucket containing the objects.
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
-- Create a value of 'ListObjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxKeys', 'listObjects_maxKeys' - Sets the maximum number of keys returned in the response. By default the
-- action returns up to 1,000 key names. The response might contain fewer
-- keys but will never contain more.
--
-- 'marker', 'listObjects_marker' - Marker is where you want Amazon S3 to start listing from. Amazon S3
-- starts listing after this specified key. Marker can be any key in the
-- bucket.
--
-- 'expectedBucketOwner', 'listObjects_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'requestPayer', 'listObjects_requestPayer' - Confirms that the requester knows that she or he will be charged for the
-- list objects request. Bucket owners need not specify this parameter in
-- their requests.
--
-- 'delimiter', 'listObjects_delimiter' - A delimiter is a character you use to group keys.
--
-- 'prefix', 'listObjects_prefix' - Limits the response to keys that begin with the specified prefix.
--
-- 'encodingType', 'listObjects_encodingType' - Undocumented member.
--
-- 'bucket', 'listObjects_bucket' - The name of the bucket containing the objects.
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
newListObjects ::
  -- | 'bucket'
  BucketName ->
  ListObjects
newListObjects pBucket_ =
  ListObjects'
    { maxKeys = Prelude.Nothing,
      marker = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      prefix = Prelude.Nothing,
      encodingType = Prelude.Nothing,
      bucket = pBucket_
    }

-- | Sets the maximum number of keys returned in the response. By default the
-- action returns up to 1,000 key names. The response might contain fewer
-- keys but will never contain more.
listObjects_maxKeys :: Lens.Lens' ListObjects (Prelude.Maybe Prelude.Int)
listObjects_maxKeys = Lens.lens (\ListObjects' {maxKeys} -> maxKeys) (\s@ListObjects' {} a -> s {maxKeys = a} :: ListObjects)

-- | Marker is where you want Amazon S3 to start listing from. Amazon S3
-- starts listing after this specified key. Marker can be any key in the
-- bucket.
listObjects_marker :: Lens.Lens' ListObjects (Prelude.Maybe Prelude.Text)
listObjects_marker = Lens.lens (\ListObjects' {marker} -> marker) (\s@ListObjects' {} a -> s {marker = a} :: ListObjects)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
listObjects_expectedBucketOwner :: Lens.Lens' ListObjects (Prelude.Maybe Prelude.Text)
listObjects_expectedBucketOwner = Lens.lens (\ListObjects' {expectedBucketOwner} -> expectedBucketOwner) (\s@ListObjects' {} a -> s {expectedBucketOwner = a} :: ListObjects)

-- | Confirms that the requester knows that she or he will be charged for the
-- list objects request. Bucket owners need not specify this parameter in
-- their requests.
listObjects_requestPayer :: Lens.Lens' ListObjects (Prelude.Maybe RequestPayer)
listObjects_requestPayer = Lens.lens (\ListObjects' {requestPayer} -> requestPayer) (\s@ListObjects' {} a -> s {requestPayer = a} :: ListObjects)

-- | A delimiter is a character you use to group keys.
listObjects_delimiter :: Lens.Lens' ListObjects (Prelude.Maybe Delimiter)
listObjects_delimiter = Lens.lens (\ListObjects' {delimiter} -> delimiter) (\s@ListObjects' {} a -> s {delimiter = a} :: ListObjects)

-- | Limits the response to keys that begin with the specified prefix.
listObjects_prefix :: Lens.Lens' ListObjects (Prelude.Maybe Prelude.Text)
listObjects_prefix = Lens.lens (\ListObjects' {prefix} -> prefix) (\s@ListObjects' {} a -> s {prefix = a} :: ListObjects)

-- | Undocumented member.
listObjects_encodingType :: Lens.Lens' ListObjects (Prelude.Maybe EncodingType)
listObjects_encodingType = Lens.lens (\ListObjects' {encodingType} -> encodingType) (\s@ListObjects' {} a -> s {encodingType = a} :: ListObjects)

-- | The name of the bucket containing the objects.
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
listObjects_bucket :: Lens.Lens' ListObjects BucketName
listObjects_bucket = Lens.lens (\ListObjects' {bucket} -> bucket) (\s@ListObjects' {} a -> s {bucket = a} :: ListObjects)

instance Core.AWSPager ListObjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listObjectsResponse_isTruncated Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^. Core.choice
              ( Lens.^?
                  ( listObjectsResponse_nextMarker
                      Prelude.. Lens._Just
                  )
              )
              ( Lens.^?
                  ( listObjectsResponse_contents Prelude.. Lens._Just
                      Prelude.. Lens._last
                      Prelude.. object_key
                  )
              )
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listObjects_marker
          Lens..~ rs
          Lens.^. Core.choice
            ( Lens.^?
                ( listObjectsResponse_nextMarker
                    Prelude.. Lens._Just
                )
            )
            ( Lens.^?
                ( listObjectsResponse_contents Prelude.. Lens._Just
                    Prelude.. Lens._last
                    Prelude.. object_key
                )
            )

instance Core.AWSRequest ListObjects where
  type AWSResponse ListObjects = ListObjectsResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListObjectsResponse'
            Prelude.<$> (x Data..@? "MaxKeys")
            Prelude.<*> (Core.may (Data.parseXMLList "CommonPrefixes") x)
            Prelude.<*> (x Data..@? "Name")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Core.may (Data.parseXMLList "Contents") x)
            Prelude.<*> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Delimiter")
            Prelude.<*> (x Data..@? "NextMarker")
            Prelude.<*> (x Data..@? "Prefix")
            Prelude.<*> (x Data..@? "EncodingType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListObjects where
  hashWithSalt _salt ListObjects' {..} =
    _salt `Prelude.hashWithSalt` maxKeys
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` encodingType
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData ListObjects where
  rnf ListObjects' {..} =
    Prelude.rnf maxKeys
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf encodingType
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders ListObjects where
  toHeaders ListObjects' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-request-payer" Data.=# requestPayer
      ]

instance Data.ToPath ListObjects where
  toPath ListObjects' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery ListObjects where
  toQuery ListObjects' {..} =
    Prelude.mconcat
      [ "max-keys" Data.=: maxKeys,
        "marker" Data.=: marker,
        "delimiter" Data.=: delimiter,
        "prefix" Data.=: prefix,
        "encoding-type" Data.=: encodingType
      ]

-- | /See:/ 'newListObjectsResponse' smart constructor.
data ListObjectsResponse = ListObjectsResponse'
  { -- | The maximum number of keys returned in the response body.
    maxKeys :: Prelude.Maybe Prelude.Int,
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
    commonPrefixes :: Prelude.Maybe [CommonPrefix],
    -- | The bucket name.
    name :: Prelude.Maybe BucketName,
    -- | Indicates where in the bucket listing begins. Marker is included in the
    -- response if it was sent with the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Metadata about each object returned.
    contents :: Prelude.Maybe [Object],
    -- | A flag that indicates whether Amazon S3 returned all of the results that
    -- satisfied the search criteria.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | Causes keys that contain the same string between the prefix and the
    -- first occurrence of the delimiter to be rolled up into a single result
    -- element in the @CommonPrefixes@ collection. These rolled-up keys are not
    -- returned elsewhere in the response. Each rolled-up result counts as only
    -- one return against the @MaxKeys@ value.
    delimiter :: Prelude.Maybe Delimiter,
    -- | When response is truncated (the IsTruncated element value in the
    -- response is true), you can use the key name in this field as marker in
    -- the subsequent request to get next set of objects. Amazon S3 lists
    -- objects in alphabetical order Note: This element is returned only if you
    -- have delimiter request parameter specified. If response does not include
    -- the NextMarker and it is truncated, you can use the value of the last
    -- Key in the response as the marker in the subsequent request to get the
    -- next set of object keys.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Keys that begin with the indicated prefix.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Encoding type used by Amazon S3 to encode object keys in the response.
    encodingType :: Prelude.Maybe EncodingType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxKeys', 'listObjectsResponse_maxKeys' - The maximum number of keys returned in the response body.
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
-- 'name', 'listObjectsResponse_name' - The bucket name.
--
-- 'marker', 'listObjectsResponse_marker' - Indicates where in the bucket listing begins. Marker is included in the
-- response if it was sent with the request.
--
-- 'contents', 'listObjectsResponse_contents' - Metadata about each object returned.
--
-- 'isTruncated', 'listObjectsResponse_isTruncated' - A flag that indicates whether Amazon S3 returned all of the results that
-- satisfied the search criteria.
--
-- 'delimiter', 'listObjectsResponse_delimiter' - Causes keys that contain the same string between the prefix and the
-- first occurrence of the delimiter to be rolled up into a single result
-- element in the @CommonPrefixes@ collection. These rolled-up keys are not
-- returned elsewhere in the response. Each rolled-up result counts as only
-- one return against the @MaxKeys@ value.
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
-- 'prefix', 'listObjectsResponse_prefix' - Keys that begin with the indicated prefix.
--
-- 'encodingType', 'listObjectsResponse_encodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- 'httpStatus', 'listObjectsResponse_httpStatus' - The response's http status code.
newListObjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListObjectsResponse
newListObjectsResponse pHttpStatus_ =
  ListObjectsResponse'
    { maxKeys = Prelude.Nothing,
      commonPrefixes = Prelude.Nothing,
      name = Prelude.Nothing,
      marker = Prelude.Nothing,
      contents = Prelude.Nothing,
      isTruncated = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      prefix = Prelude.Nothing,
      encodingType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The maximum number of keys returned in the response body.
listObjectsResponse_maxKeys :: Lens.Lens' ListObjectsResponse (Prelude.Maybe Prelude.Int)
listObjectsResponse_maxKeys = Lens.lens (\ListObjectsResponse' {maxKeys} -> maxKeys) (\s@ListObjectsResponse' {} a -> s {maxKeys = a} :: ListObjectsResponse)

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
listObjectsResponse_commonPrefixes :: Lens.Lens' ListObjectsResponse (Prelude.Maybe [CommonPrefix])
listObjectsResponse_commonPrefixes = Lens.lens (\ListObjectsResponse' {commonPrefixes} -> commonPrefixes) (\s@ListObjectsResponse' {} a -> s {commonPrefixes = a} :: ListObjectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The bucket name.
listObjectsResponse_name :: Lens.Lens' ListObjectsResponse (Prelude.Maybe BucketName)
listObjectsResponse_name = Lens.lens (\ListObjectsResponse' {name} -> name) (\s@ListObjectsResponse' {} a -> s {name = a} :: ListObjectsResponse)

-- | Indicates where in the bucket listing begins. Marker is included in the
-- response if it was sent with the request.
listObjectsResponse_marker :: Lens.Lens' ListObjectsResponse (Prelude.Maybe Prelude.Text)
listObjectsResponse_marker = Lens.lens (\ListObjectsResponse' {marker} -> marker) (\s@ListObjectsResponse' {} a -> s {marker = a} :: ListObjectsResponse)

-- | Metadata about each object returned.
listObjectsResponse_contents :: Lens.Lens' ListObjectsResponse (Prelude.Maybe [Object])
listObjectsResponse_contents = Lens.lens (\ListObjectsResponse' {contents} -> contents) (\s@ListObjectsResponse' {} a -> s {contents = a} :: ListObjectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A flag that indicates whether Amazon S3 returned all of the results that
-- satisfied the search criteria.
listObjectsResponse_isTruncated :: Lens.Lens' ListObjectsResponse (Prelude.Maybe Prelude.Bool)
listObjectsResponse_isTruncated = Lens.lens (\ListObjectsResponse' {isTruncated} -> isTruncated) (\s@ListObjectsResponse' {} a -> s {isTruncated = a} :: ListObjectsResponse)

-- | Causes keys that contain the same string between the prefix and the
-- first occurrence of the delimiter to be rolled up into a single result
-- element in the @CommonPrefixes@ collection. These rolled-up keys are not
-- returned elsewhere in the response. Each rolled-up result counts as only
-- one return against the @MaxKeys@ value.
listObjectsResponse_delimiter :: Lens.Lens' ListObjectsResponse (Prelude.Maybe Delimiter)
listObjectsResponse_delimiter = Lens.lens (\ListObjectsResponse' {delimiter} -> delimiter) (\s@ListObjectsResponse' {} a -> s {delimiter = a} :: ListObjectsResponse)

-- | When response is truncated (the IsTruncated element value in the
-- response is true), you can use the key name in this field as marker in
-- the subsequent request to get next set of objects. Amazon S3 lists
-- objects in alphabetical order Note: This element is returned only if you
-- have delimiter request parameter specified. If response does not include
-- the NextMarker and it is truncated, you can use the value of the last
-- Key in the response as the marker in the subsequent request to get the
-- next set of object keys.
listObjectsResponse_nextMarker :: Lens.Lens' ListObjectsResponse (Prelude.Maybe Prelude.Text)
listObjectsResponse_nextMarker = Lens.lens (\ListObjectsResponse' {nextMarker} -> nextMarker) (\s@ListObjectsResponse' {} a -> s {nextMarker = a} :: ListObjectsResponse)

-- | Keys that begin with the indicated prefix.
listObjectsResponse_prefix :: Lens.Lens' ListObjectsResponse (Prelude.Maybe Prelude.Text)
listObjectsResponse_prefix = Lens.lens (\ListObjectsResponse' {prefix} -> prefix) (\s@ListObjectsResponse' {} a -> s {prefix = a} :: ListObjectsResponse)

-- | Encoding type used by Amazon S3 to encode object keys in the response.
listObjectsResponse_encodingType :: Lens.Lens' ListObjectsResponse (Prelude.Maybe EncodingType)
listObjectsResponse_encodingType = Lens.lens (\ListObjectsResponse' {encodingType} -> encodingType) (\s@ListObjectsResponse' {} a -> s {encodingType = a} :: ListObjectsResponse)

-- | The response's http status code.
listObjectsResponse_httpStatus :: Lens.Lens' ListObjectsResponse Prelude.Int
listObjectsResponse_httpStatus = Lens.lens (\ListObjectsResponse' {httpStatus} -> httpStatus) (\s@ListObjectsResponse' {} a -> s {httpStatus = a} :: ListObjectsResponse)

instance Prelude.NFData ListObjectsResponse where
  rnf ListObjectsResponse' {..} =
    Prelude.rnf maxKeys
      `Prelude.seq` Prelude.rnf commonPrefixes
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf contents
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf encodingType
      `Prelude.seq` Prelude.rnf httpStatus
