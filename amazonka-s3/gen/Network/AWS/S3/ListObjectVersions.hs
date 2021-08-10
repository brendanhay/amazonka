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
-- Module      : Network.AWS.S3.ListObjectVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about all versions of the objects in a bucket. You can
-- also use request parameters as selection criteria to return metadata
-- about a subset of all the object versions.
--
-- A 200 OK response can contain valid or invalid XML. Make sure to design
-- your application to parse the contents of the response and handle it
-- appropriately.
--
-- To use this operation, you must have READ access to the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- The following operations are related to @ListObjectVersions@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjectsV2.html ListObjectsV2>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
--
-- This operation returns paginated results.
module Network.AWS.S3.ListObjectVersions
  ( -- * Creating a Request
    ListObjectVersions (..),
    newListObjectVersions,

    -- * Request Lenses
    listObjectVersions_expectedBucketOwner,
    listObjectVersions_encodingType,
    listObjectVersions_delimiter,
    listObjectVersions_prefix,
    listObjectVersions_maxKeys,
    listObjectVersions_keyMarker,
    listObjectVersions_versionIdMarker,
    listObjectVersions_bucket,

    -- * Destructuring the Response
    ListObjectVersionsResponse (..),
    newListObjectVersionsResponse,

    -- * Response Lenses
    listObjectVersionsResponse_versions,
    listObjectVersionsResponse_commonPrefixes,
    listObjectVersionsResponse_encodingType,
    listObjectVersionsResponse_delimiter,
    listObjectVersionsResponse_prefix,
    listObjectVersionsResponse_isTruncated,
    listObjectVersionsResponse_maxKeys,
    listObjectVersionsResponse_keyMarker,
    listObjectVersionsResponse_nextKeyMarker,
    listObjectVersionsResponse_name,
    listObjectVersionsResponse_deleteMarkers,
    listObjectVersionsResponse_versionIdMarker,
    listObjectVersionsResponse_nextVersionIdMarker,
    listObjectVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newListObjectVersions' smart constructor.
data ListObjectVersions = ListObjectVersions'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    encodingType :: Prelude.Maybe EncodingType,
    -- | A delimiter is a character that you specify to group keys. All keys that
    -- contain the same string between the @prefix@ and the first occurrence of
    -- the delimiter are grouped under a single result element in
    -- CommonPrefixes. These groups are counted as one result against the
    -- max-keys limitation. These keys are not returned elsewhere in the
    -- response.
    delimiter :: Prelude.Maybe Delimiter,
    -- | Use this parameter to select only those keys that begin with the
    -- specified prefix. You can use prefixes to separate a bucket into
    -- different groupings of keys. (You can think of using prefix to make
    -- groups in the same way you\'d use a folder in a file system.) You can
    -- use prefix with delimiter to roll up numerous objects into a single
    -- result under CommonPrefixes.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Sets the maximum number of keys returned in the response. By default the
    -- API returns up to 1,000 key names. The response might contain fewer keys
    -- but will never contain more. If additional keys satisfy the search
    -- criteria, but were not returned because max-keys was exceeded, the
    -- response contains \<isTruncated>true\<\/isTruncated>. To return the
    -- additional keys, see key-marker and version-id-marker.
    maxKeys :: Prelude.Maybe Prelude.Int,
    -- | Specifies the key to start with when listing objects in a bucket.
    keyMarker :: Prelude.Maybe Prelude.Text,
    -- | Specifies the object version you want to start listing from.
    versionIdMarker :: Prelude.Maybe Prelude.Text,
    -- | The bucket name that contains the objects.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObjectVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'listObjectVersions_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'encodingType', 'listObjectVersions_encodingType' - Undocumented member.
--
-- 'delimiter', 'listObjectVersions_delimiter' - A delimiter is a character that you specify to group keys. All keys that
-- contain the same string between the @prefix@ and the first occurrence of
-- the delimiter are grouped under a single result element in
-- CommonPrefixes. These groups are counted as one result against the
-- max-keys limitation. These keys are not returned elsewhere in the
-- response.
--
-- 'prefix', 'listObjectVersions_prefix' - Use this parameter to select only those keys that begin with the
-- specified prefix. You can use prefixes to separate a bucket into
-- different groupings of keys. (You can think of using prefix to make
-- groups in the same way you\'d use a folder in a file system.) You can
-- use prefix with delimiter to roll up numerous objects into a single
-- result under CommonPrefixes.
--
-- 'maxKeys', 'listObjectVersions_maxKeys' - Sets the maximum number of keys returned in the response. By default the
-- API returns up to 1,000 key names. The response might contain fewer keys
-- but will never contain more. If additional keys satisfy the search
-- criteria, but were not returned because max-keys was exceeded, the
-- response contains \<isTruncated>true\<\/isTruncated>. To return the
-- additional keys, see key-marker and version-id-marker.
--
-- 'keyMarker', 'listObjectVersions_keyMarker' - Specifies the key to start with when listing objects in a bucket.
--
-- 'versionIdMarker', 'listObjectVersions_versionIdMarker' - Specifies the object version you want to start listing from.
--
-- 'bucket', 'listObjectVersions_bucket' - The bucket name that contains the objects.
newListObjectVersions ::
  -- | 'bucket'
  BucketName ->
  ListObjectVersions
newListObjectVersions pBucket_ =
  ListObjectVersions'
    { expectedBucketOwner =
        Prelude.Nothing,
      encodingType = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      prefix = Prelude.Nothing,
      maxKeys = Prelude.Nothing,
      keyMarker = Prelude.Nothing,
      versionIdMarker = Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
listObjectVersions_expectedBucketOwner :: Lens.Lens' ListObjectVersions (Prelude.Maybe Prelude.Text)
listObjectVersions_expectedBucketOwner = Lens.lens (\ListObjectVersions' {expectedBucketOwner} -> expectedBucketOwner) (\s@ListObjectVersions' {} a -> s {expectedBucketOwner = a} :: ListObjectVersions)

-- | Undocumented member.
listObjectVersions_encodingType :: Lens.Lens' ListObjectVersions (Prelude.Maybe EncodingType)
listObjectVersions_encodingType = Lens.lens (\ListObjectVersions' {encodingType} -> encodingType) (\s@ListObjectVersions' {} a -> s {encodingType = a} :: ListObjectVersions)

-- | A delimiter is a character that you specify to group keys. All keys that
-- contain the same string between the @prefix@ and the first occurrence of
-- the delimiter are grouped under a single result element in
-- CommonPrefixes. These groups are counted as one result against the
-- max-keys limitation. These keys are not returned elsewhere in the
-- response.
listObjectVersions_delimiter :: Lens.Lens' ListObjectVersions (Prelude.Maybe Delimiter)
listObjectVersions_delimiter = Lens.lens (\ListObjectVersions' {delimiter} -> delimiter) (\s@ListObjectVersions' {} a -> s {delimiter = a} :: ListObjectVersions)

-- | Use this parameter to select only those keys that begin with the
-- specified prefix. You can use prefixes to separate a bucket into
-- different groupings of keys. (You can think of using prefix to make
-- groups in the same way you\'d use a folder in a file system.) You can
-- use prefix with delimiter to roll up numerous objects into a single
-- result under CommonPrefixes.
listObjectVersions_prefix :: Lens.Lens' ListObjectVersions (Prelude.Maybe Prelude.Text)
listObjectVersions_prefix = Lens.lens (\ListObjectVersions' {prefix} -> prefix) (\s@ListObjectVersions' {} a -> s {prefix = a} :: ListObjectVersions)

-- | Sets the maximum number of keys returned in the response. By default the
-- API returns up to 1,000 key names. The response might contain fewer keys
-- but will never contain more. If additional keys satisfy the search
-- criteria, but were not returned because max-keys was exceeded, the
-- response contains \<isTruncated>true\<\/isTruncated>. To return the
-- additional keys, see key-marker and version-id-marker.
listObjectVersions_maxKeys :: Lens.Lens' ListObjectVersions (Prelude.Maybe Prelude.Int)
listObjectVersions_maxKeys = Lens.lens (\ListObjectVersions' {maxKeys} -> maxKeys) (\s@ListObjectVersions' {} a -> s {maxKeys = a} :: ListObjectVersions)

-- | Specifies the key to start with when listing objects in a bucket.
listObjectVersions_keyMarker :: Lens.Lens' ListObjectVersions (Prelude.Maybe Prelude.Text)
listObjectVersions_keyMarker = Lens.lens (\ListObjectVersions' {keyMarker} -> keyMarker) (\s@ListObjectVersions' {} a -> s {keyMarker = a} :: ListObjectVersions)

-- | Specifies the object version you want to start listing from.
listObjectVersions_versionIdMarker :: Lens.Lens' ListObjectVersions (Prelude.Maybe Prelude.Text)
listObjectVersions_versionIdMarker = Lens.lens (\ListObjectVersions' {versionIdMarker} -> versionIdMarker) (\s@ListObjectVersions' {} a -> s {versionIdMarker = a} :: ListObjectVersions)

-- | The bucket name that contains the objects.
listObjectVersions_bucket :: Lens.Lens' ListObjectVersions BucketName
listObjectVersions_bucket = Lens.lens (\ListObjectVersions' {bucket} -> bucket) (\s@ListObjectVersions' {} a -> s {bucket = a} :: ListObjectVersions)

instance Core.AWSPager ListObjectVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listObjectVersionsResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listObjectVersionsResponse_nextKeyMarker
              Prelude.. Lens._Just
        )
        Prelude.&& Prelude.isNothing
          ( rs
              Lens.^? listObjectVersionsResponse_nextVersionIdMarker
                Prelude.. Lens._Just
          ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listObjectVersions_keyMarker
          Lens..~ rs
          Lens.^? listObjectVersionsResponse_nextKeyMarker
            Prelude.. Lens._Just
          Prelude.& listObjectVersions_versionIdMarker
          Lens..~ rs
          Lens.^? listObjectVersionsResponse_nextVersionIdMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListObjectVersions where
  type
    AWSResponse ListObjectVersions =
      ListObjectVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListObjectVersionsResponse'
            Prelude.<$> (Core.may (Core.parseXMLList "Version") x)
            Prelude.<*> (Core.may (Core.parseXMLList "CommonPrefixes") x)
            Prelude.<*> (x Core..@? "EncodingType")
            Prelude.<*> (x Core..@? "Delimiter")
            Prelude.<*> (x Core..@? "Prefix")
            Prelude.<*> (x Core..@? "IsTruncated")
            Prelude.<*> (x Core..@? "MaxKeys")
            Prelude.<*> (x Core..@? "KeyMarker")
            Prelude.<*> (x Core..@? "NextKeyMarker")
            Prelude.<*> (x Core..@? "Name")
            Prelude.<*> (Core.may (Core.parseXMLList "DeleteMarker") x)
            Prelude.<*> (x Core..@? "VersionIdMarker")
            Prelude.<*> (x Core..@? "NextVersionIdMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListObjectVersions

instance Prelude.NFData ListObjectVersions

instance Core.ToHeaders ListObjectVersions where
  toHeaders ListObjectVersions' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath ListObjectVersions where
  toPath ListObjectVersions' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery ListObjectVersions where
  toQuery ListObjectVersions' {..} =
    Prelude.mconcat
      [ "encoding-type" Core.=: encodingType,
        "delimiter" Core.=: delimiter,
        "prefix" Core.=: prefix,
        "max-keys" Core.=: maxKeys,
        "key-marker" Core.=: keyMarker,
        "version-id-marker" Core.=: versionIdMarker,
        "versions"
      ]

-- | /See:/ 'newListObjectVersionsResponse' smart constructor.
data ListObjectVersionsResponse = ListObjectVersionsResponse'
  { -- | Container for version information.
    versions :: Prelude.Maybe [ObjectVersion],
    -- | All of the keys rolled up into a common prefix count as a single return
    -- when calculating the number of returns.
    commonPrefixes :: Prelude.Maybe [CommonPrefix],
    -- | Encoding type used by Amazon S3 to encode object key names in the XML
    -- response.
    --
    -- If you specify encoding-type request parameter, Amazon S3 includes this
    -- element in the response, and returns encoded key name values in the
    -- following response elements:
    --
    -- @KeyMarker, NextKeyMarker, Prefix, Key@, and @Delimiter@.
    encodingType :: Prelude.Maybe EncodingType,
    -- | The delimiter grouping the included keys. A delimiter is a character
    -- that you specify to group keys. All keys that contain the same string
    -- between the prefix and the first occurrence of the delimiter are grouped
    -- under a single result element in @CommonPrefixes@. These groups are
    -- counted as one result against the max-keys limitation. These keys are
    -- not returned elsewhere in the response.
    delimiter :: Prelude.Maybe Delimiter,
    -- | Selects objects that start with the value supplied by this parameter.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | A flag that indicates whether Amazon S3 returned all of the results that
    -- satisfied the search criteria. If your results were truncated, you can
    -- make a follow-up paginated request using the NextKeyMarker and
    -- NextVersionIdMarker response parameters as a starting place in another
    -- request to return the rest of the results.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the maximum number of objects to return.
    maxKeys :: Prelude.Maybe Prelude.Int,
    -- | Marks the last key returned in a truncated response.
    keyMarker :: Prelude.Maybe Prelude.Text,
    -- | When the number of responses exceeds the value of @MaxKeys@,
    -- @NextKeyMarker@ specifies the first key not returned that satisfies the
    -- search criteria. Use this value for the key-marker request parameter in
    -- a subsequent request.
    nextKeyMarker :: Prelude.Maybe Prelude.Text,
    -- | The bucket name.
    name :: Prelude.Maybe BucketName,
    -- | Container for an object that is a delete marker.
    deleteMarkers :: Prelude.Maybe [DeleteMarkerEntry],
    -- | Marks the last version of the key returned in a truncated response.
    versionIdMarker :: Prelude.Maybe Prelude.Text,
    -- | When the number of responses exceeds the value of @MaxKeys@,
    -- @NextVersionIdMarker@ specifies the first object version not returned
    -- that satisfies the search criteria. Use this value for the
    -- version-id-marker request parameter in a subsequent request.
    nextVersionIdMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObjectVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versions', 'listObjectVersionsResponse_versions' - Container for version information.
--
-- 'commonPrefixes', 'listObjectVersionsResponse_commonPrefixes' - All of the keys rolled up into a common prefix count as a single return
-- when calculating the number of returns.
--
-- 'encodingType', 'listObjectVersionsResponse_encodingType' - Encoding type used by Amazon S3 to encode object key names in the XML
-- response.
--
-- If you specify encoding-type request parameter, Amazon S3 includes this
-- element in the response, and returns encoded key name values in the
-- following response elements:
--
-- @KeyMarker, NextKeyMarker, Prefix, Key@, and @Delimiter@.
--
-- 'delimiter', 'listObjectVersionsResponse_delimiter' - The delimiter grouping the included keys. A delimiter is a character
-- that you specify to group keys. All keys that contain the same string
-- between the prefix and the first occurrence of the delimiter are grouped
-- under a single result element in @CommonPrefixes@. These groups are
-- counted as one result against the max-keys limitation. These keys are
-- not returned elsewhere in the response.
--
-- 'prefix', 'listObjectVersionsResponse_prefix' - Selects objects that start with the value supplied by this parameter.
--
-- 'isTruncated', 'listObjectVersionsResponse_isTruncated' - A flag that indicates whether Amazon S3 returned all of the results that
-- satisfied the search criteria. If your results were truncated, you can
-- make a follow-up paginated request using the NextKeyMarker and
-- NextVersionIdMarker response parameters as a starting place in another
-- request to return the rest of the results.
--
-- 'maxKeys', 'listObjectVersionsResponse_maxKeys' - Specifies the maximum number of objects to return.
--
-- 'keyMarker', 'listObjectVersionsResponse_keyMarker' - Marks the last key returned in a truncated response.
--
-- 'nextKeyMarker', 'listObjectVersionsResponse_nextKeyMarker' - When the number of responses exceeds the value of @MaxKeys@,
-- @NextKeyMarker@ specifies the first key not returned that satisfies the
-- search criteria. Use this value for the key-marker request parameter in
-- a subsequent request.
--
-- 'name', 'listObjectVersionsResponse_name' - The bucket name.
--
-- 'deleteMarkers', 'listObjectVersionsResponse_deleteMarkers' - Container for an object that is a delete marker.
--
-- 'versionIdMarker', 'listObjectVersionsResponse_versionIdMarker' - Marks the last version of the key returned in a truncated response.
--
-- 'nextVersionIdMarker', 'listObjectVersionsResponse_nextVersionIdMarker' - When the number of responses exceeds the value of @MaxKeys@,
-- @NextVersionIdMarker@ specifies the first object version not returned
-- that satisfies the search criteria. Use this value for the
-- version-id-marker request parameter in a subsequent request.
--
-- 'httpStatus', 'listObjectVersionsResponse_httpStatus' - The response's http status code.
newListObjectVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListObjectVersionsResponse
newListObjectVersionsResponse pHttpStatus_ =
  ListObjectVersionsResponse'
    { versions =
        Prelude.Nothing,
      commonPrefixes = Prelude.Nothing,
      encodingType = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      prefix = Prelude.Nothing,
      isTruncated = Prelude.Nothing,
      maxKeys = Prelude.Nothing,
      keyMarker = Prelude.Nothing,
      nextKeyMarker = Prelude.Nothing,
      name = Prelude.Nothing,
      deleteMarkers = Prelude.Nothing,
      versionIdMarker = Prelude.Nothing,
      nextVersionIdMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Container for version information.
listObjectVersionsResponse_versions :: Lens.Lens' ListObjectVersionsResponse (Prelude.Maybe [ObjectVersion])
listObjectVersionsResponse_versions = Lens.lens (\ListObjectVersionsResponse' {versions} -> versions) (\s@ListObjectVersionsResponse' {} a -> s {versions = a} :: ListObjectVersionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | All of the keys rolled up into a common prefix count as a single return
-- when calculating the number of returns.
listObjectVersionsResponse_commonPrefixes :: Lens.Lens' ListObjectVersionsResponse (Prelude.Maybe [CommonPrefix])
listObjectVersionsResponse_commonPrefixes = Lens.lens (\ListObjectVersionsResponse' {commonPrefixes} -> commonPrefixes) (\s@ListObjectVersionsResponse' {} a -> s {commonPrefixes = a} :: ListObjectVersionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Encoding type used by Amazon S3 to encode object key names in the XML
-- response.
--
-- If you specify encoding-type request parameter, Amazon S3 includes this
-- element in the response, and returns encoded key name values in the
-- following response elements:
--
-- @KeyMarker, NextKeyMarker, Prefix, Key@, and @Delimiter@.
listObjectVersionsResponse_encodingType :: Lens.Lens' ListObjectVersionsResponse (Prelude.Maybe EncodingType)
listObjectVersionsResponse_encodingType = Lens.lens (\ListObjectVersionsResponse' {encodingType} -> encodingType) (\s@ListObjectVersionsResponse' {} a -> s {encodingType = a} :: ListObjectVersionsResponse)

-- | The delimiter grouping the included keys. A delimiter is a character
-- that you specify to group keys. All keys that contain the same string
-- between the prefix and the first occurrence of the delimiter are grouped
-- under a single result element in @CommonPrefixes@. These groups are
-- counted as one result against the max-keys limitation. These keys are
-- not returned elsewhere in the response.
listObjectVersionsResponse_delimiter :: Lens.Lens' ListObjectVersionsResponse (Prelude.Maybe Delimiter)
listObjectVersionsResponse_delimiter = Lens.lens (\ListObjectVersionsResponse' {delimiter} -> delimiter) (\s@ListObjectVersionsResponse' {} a -> s {delimiter = a} :: ListObjectVersionsResponse)

-- | Selects objects that start with the value supplied by this parameter.
listObjectVersionsResponse_prefix :: Lens.Lens' ListObjectVersionsResponse (Prelude.Maybe Prelude.Text)
listObjectVersionsResponse_prefix = Lens.lens (\ListObjectVersionsResponse' {prefix} -> prefix) (\s@ListObjectVersionsResponse' {} a -> s {prefix = a} :: ListObjectVersionsResponse)

-- | A flag that indicates whether Amazon S3 returned all of the results that
-- satisfied the search criteria. If your results were truncated, you can
-- make a follow-up paginated request using the NextKeyMarker and
-- NextVersionIdMarker response parameters as a starting place in another
-- request to return the rest of the results.
listObjectVersionsResponse_isTruncated :: Lens.Lens' ListObjectVersionsResponse (Prelude.Maybe Prelude.Bool)
listObjectVersionsResponse_isTruncated = Lens.lens (\ListObjectVersionsResponse' {isTruncated} -> isTruncated) (\s@ListObjectVersionsResponse' {} a -> s {isTruncated = a} :: ListObjectVersionsResponse)

-- | Specifies the maximum number of objects to return.
listObjectVersionsResponse_maxKeys :: Lens.Lens' ListObjectVersionsResponse (Prelude.Maybe Prelude.Int)
listObjectVersionsResponse_maxKeys = Lens.lens (\ListObjectVersionsResponse' {maxKeys} -> maxKeys) (\s@ListObjectVersionsResponse' {} a -> s {maxKeys = a} :: ListObjectVersionsResponse)

-- | Marks the last key returned in a truncated response.
listObjectVersionsResponse_keyMarker :: Lens.Lens' ListObjectVersionsResponse (Prelude.Maybe Prelude.Text)
listObjectVersionsResponse_keyMarker = Lens.lens (\ListObjectVersionsResponse' {keyMarker} -> keyMarker) (\s@ListObjectVersionsResponse' {} a -> s {keyMarker = a} :: ListObjectVersionsResponse)

-- | When the number of responses exceeds the value of @MaxKeys@,
-- @NextKeyMarker@ specifies the first key not returned that satisfies the
-- search criteria. Use this value for the key-marker request parameter in
-- a subsequent request.
listObjectVersionsResponse_nextKeyMarker :: Lens.Lens' ListObjectVersionsResponse (Prelude.Maybe Prelude.Text)
listObjectVersionsResponse_nextKeyMarker = Lens.lens (\ListObjectVersionsResponse' {nextKeyMarker} -> nextKeyMarker) (\s@ListObjectVersionsResponse' {} a -> s {nextKeyMarker = a} :: ListObjectVersionsResponse)

-- | The bucket name.
listObjectVersionsResponse_name :: Lens.Lens' ListObjectVersionsResponse (Prelude.Maybe BucketName)
listObjectVersionsResponse_name = Lens.lens (\ListObjectVersionsResponse' {name} -> name) (\s@ListObjectVersionsResponse' {} a -> s {name = a} :: ListObjectVersionsResponse)

-- | Container for an object that is a delete marker.
listObjectVersionsResponse_deleteMarkers :: Lens.Lens' ListObjectVersionsResponse (Prelude.Maybe [DeleteMarkerEntry])
listObjectVersionsResponse_deleteMarkers = Lens.lens (\ListObjectVersionsResponse' {deleteMarkers} -> deleteMarkers) (\s@ListObjectVersionsResponse' {} a -> s {deleteMarkers = a} :: ListObjectVersionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Marks the last version of the key returned in a truncated response.
listObjectVersionsResponse_versionIdMarker :: Lens.Lens' ListObjectVersionsResponse (Prelude.Maybe Prelude.Text)
listObjectVersionsResponse_versionIdMarker = Lens.lens (\ListObjectVersionsResponse' {versionIdMarker} -> versionIdMarker) (\s@ListObjectVersionsResponse' {} a -> s {versionIdMarker = a} :: ListObjectVersionsResponse)

-- | When the number of responses exceeds the value of @MaxKeys@,
-- @NextVersionIdMarker@ specifies the first object version not returned
-- that satisfies the search criteria. Use this value for the
-- version-id-marker request parameter in a subsequent request.
listObjectVersionsResponse_nextVersionIdMarker :: Lens.Lens' ListObjectVersionsResponse (Prelude.Maybe Prelude.Text)
listObjectVersionsResponse_nextVersionIdMarker = Lens.lens (\ListObjectVersionsResponse' {nextVersionIdMarker} -> nextVersionIdMarker) (\s@ListObjectVersionsResponse' {} a -> s {nextVersionIdMarker = a} :: ListObjectVersionsResponse)

-- | The response's http status code.
listObjectVersionsResponse_httpStatus :: Lens.Lens' ListObjectVersionsResponse Prelude.Int
listObjectVersionsResponse_httpStatus = Lens.lens (\ListObjectVersionsResponse' {httpStatus} -> httpStatus) (\s@ListObjectVersionsResponse' {} a -> s {httpStatus = a} :: ListObjectVersionsResponse)

instance Prelude.NFData ListObjectVersionsResponse
