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
-- Module      : Network.AWS.S3.ListObjectsV
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns some or all (up to 1,000) of the objects in a bucket with each
-- request. You can use the request parameters as selection criteria to
-- return a subset of the objects in a bucket. A @200 OK@ response can
-- contain valid or invalid XML. Make sure to design your application to
-- parse the contents of the response and handle it appropriately. Objects
-- are returned sorted in an ascending order of the respective key names in
-- the list. For more information about listing objects, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/ListingKeysUsingAPIs.html Listing object keys programmatically>
--
-- To use this operation, you must have READ access to the bucket.
--
-- To use this action in an Identity and Access Management (IAM) policy,
-- you must have permissions to perform the @s3:ListBucket@ action. The
-- bucket owner has this permission by default and can grant this
-- permission to others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- This section describes the latest revision of this action. We recommend
-- that you use this revised API for application development. For backward
-- compatibility, Amazon S3 continues to support the prior version of this
-- API,
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjects.html ListObjects>.
--
-- To get a list of your buckets, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBuckets.html ListBuckets>.
--
-- The following operations are related to @ListObjectsV2@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
module Network.AWS.S3.ListObjectsV
  ( -- * Creating a Request
    ListObjectsV (..),
    newListObjectsV,

    -- * Request Lenses
    listObjectsV_startAfter,
    listObjectsV_continuationToken,
    listObjectsV_fetchOwner,
    listObjectsV_prefix,
    listObjectsV_encodingType,
    listObjectsV_requestPayer,
    listObjectsV_maxKeys,
    listObjectsV_delimiter,
    listObjectsV_expectedBucketOwner,
    listObjectsV_bucket,

    -- * Destructuring the Response
    ListObjectsVResponse (..),
    newListObjectsVResponse,

    -- * Response Lenses
    listObjectsVResponse_startAfter,
    listObjectsVResponse_keyCount,
    listObjectsVResponse_contents,
    listObjectsVResponse_continuationToken,
    listObjectsVResponse_prefix,
    listObjectsVResponse_commonPrefixes,
    listObjectsVResponse_encodingType,
    listObjectsVResponse_name,
    listObjectsVResponse_nextContinuationToken,
    listObjectsVResponse_maxKeys,
    listObjectsVResponse_isTruncated,
    listObjectsVResponse_delimiter,
    listObjectsVResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newListObjectsV' smart constructor.
data ListObjectsV = ListObjectsV'
  { -- | StartAfter is where you want Amazon S3 to start listing from. Amazon S3
    -- starts listing after this specified key. StartAfter can be any key in
    -- the bucket.
    startAfter :: Prelude.Maybe Prelude.Text,
    -- | ContinuationToken indicates Amazon S3 that the list is being continued
    -- on this bucket with a token. ContinuationToken is obfuscated and is not
    -- a real key.
    continuationToken :: Prelude.Maybe Prelude.Text,
    -- | The owner field is not present in listV2 by default, if you want to
    -- return owner field with each key in the result then set the fetch owner
    -- field to true.
    fetchOwner :: Prelude.Maybe Prelude.Bool,
    -- | Limits the response to keys that begin with the specified prefix.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Encoding type used by Amazon S3 to encode object keys in the response.
    encodingType :: Prelude.Maybe EncodingType,
    -- | Confirms that the requester knows that she or he will be charged for the
    -- list objects request in V2 style. Bucket owners need not specify this
    -- parameter in their requests.
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | Sets the maximum number of keys returned in the response. By default the
    -- action returns up to 1,000 key names. The response might contain fewer
    -- keys but will never contain more.
    maxKeys :: Prelude.Maybe Prelude.Int,
    -- | A delimiter is a character you use to group keys.
    delimiter :: Prelude.Maybe Delimiter,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Bucket name to list.
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
    -- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
    -- When using this action using S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts bucket ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObjectsV' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startAfter', 'listObjectsV_startAfter' - StartAfter is where you want Amazon S3 to start listing from. Amazon S3
-- starts listing after this specified key. StartAfter can be any key in
-- the bucket.
--
-- 'continuationToken', 'listObjectsV_continuationToken' - ContinuationToken indicates Amazon S3 that the list is being continued
-- on this bucket with a token. ContinuationToken is obfuscated and is not
-- a real key.
--
-- 'fetchOwner', 'listObjectsV_fetchOwner' - The owner field is not present in listV2 by default, if you want to
-- return owner field with each key in the result then set the fetch owner
-- field to true.
--
-- 'prefix', 'listObjectsV_prefix' - Limits the response to keys that begin with the specified prefix.
--
-- 'encodingType', 'listObjectsV_encodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- 'requestPayer', 'listObjectsV_requestPayer' - Confirms that the requester knows that she or he will be charged for the
-- list objects request in V2 style. Bucket owners need not specify this
-- parameter in their requests.
--
-- 'maxKeys', 'listObjectsV_maxKeys' - Sets the maximum number of keys returned in the response. By default the
-- action returns up to 1,000 key names. The response might contain fewer
-- keys but will never contain more.
--
-- 'delimiter', 'listObjectsV_delimiter' - A delimiter is a character you use to group keys.
--
-- 'expectedBucketOwner', 'listObjectsV_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'listObjectsV_bucket' - Bucket name to list.
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
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this action using S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon S3 User Guide/.
newListObjectsV ::
  -- | 'bucket'
  BucketName ->
  ListObjectsV
newListObjectsV pBucket_ =
  ListObjectsV'
    { startAfter = Prelude.Nothing,
      continuationToken = Prelude.Nothing,
      fetchOwner = Prelude.Nothing,
      prefix = Prelude.Nothing,
      encodingType = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      maxKeys = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      bucket = pBucket_
    }

-- | StartAfter is where you want Amazon S3 to start listing from. Amazon S3
-- starts listing after this specified key. StartAfter can be any key in
-- the bucket.
listObjectsV_startAfter :: Lens.Lens' ListObjectsV (Prelude.Maybe Prelude.Text)
listObjectsV_startAfter = Lens.lens (\ListObjectsV' {startAfter} -> startAfter) (\s@ListObjectsV' {} a -> s {startAfter = a} :: ListObjectsV)

-- | ContinuationToken indicates Amazon S3 that the list is being continued
-- on this bucket with a token. ContinuationToken is obfuscated and is not
-- a real key.
listObjectsV_continuationToken :: Lens.Lens' ListObjectsV (Prelude.Maybe Prelude.Text)
listObjectsV_continuationToken = Lens.lens (\ListObjectsV' {continuationToken} -> continuationToken) (\s@ListObjectsV' {} a -> s {continuationToken = a} :: ListObjectsV)

-- | The owner field is not present in listV2 by default, if you want to
-- return owner field with each key in the result then set the fetch owner
-- field to true.
listObjectsV_fetchOwner :: Lens.Lens' ListObjectsV (Prelude.Maybe Prelude.Bool)
listObjectsV_fetchOwner = Lens.lens (\ListObjectsV' {fetchOwner} -> fetchOwner) (\s@ListObjectsV' {} a -> s {fetchOwner = a} :: ListObjectsV)

-- | Limits the response to keys that begin with the specified prefix.
listObjectsV_prefix :: Lens.Lens' ListObjectsV (Prelude.Maybe Prelude.Text)
listObjectsV_prefix = Lens.lens (\ListObjectsV' {prefix} -> prefix) (\s@ListObjectsV' {} a -> s {prefix = a} :: ListObjectsV)

-- | Encoding type used by Amazon S3 to encode object keys in the response.
listObjectsV_encodingType :: Lens.Lens' ListObjectsV (Prelude.Maybe EncodingType)
listObjectsV_encodingType = Lens.lens (\ListObjectsV' {encodingType} -> encodingType) (\s@ListObjectsV' {} a -> s {encodingType = a} :: ListObjectsV)

-- | Confirms that the requester knows that she or he will be charged for the
-- list objects request in V2 style. Bucket owners need not specify this
-- parameter in their requests.
listObjectsV_requestPayer :: Lens.Lens' ListObjectsV (Prelude.Maybe RequestPayer)
listObjectsV_requestPayer = Lens.lens (\ListObjectsV' {requestPayer} -> requestPayer) (\s@ListObjectsV' {} a -> s {requestPayer = a} :: ListObjectsV)

-- | Sets the maximum number of keys returned in the response. By default the
-- action returns up to 1,000 key names. The response might contain fewer
-- keys but will never contain more.
listObjectsV_maxKeys :: Lens.Lens' ListObjectsV (Prelude.Maybe Prelude.Int)
listObjectsV_maxKeys = Lens.lens (\ListObjectsV' {maxKeys} -> maxKeys) (\s@ListObjectsV' {} a -> s {maxKeys = a} :: ListObjectsV)

-- | A delimiter is a character you use to group keys.
listObjectsV_delimiter :: Lens.Lens' ListObjectsV (Prelude.Maybe Delimiter)
listObjectsV_delimiter = Lens.lens (\ListObjectsV' {delimiter} -> delimiter) (\s@ListObjectsV' {} a -> s {delimiter = a} :: ListObjectsV)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
listObjectsV_expectedBucketOwner :: Lens.Lens' ListObjectsV (Prelude.Maybe Prelude.Text)
listObjectsV_expectedBucketOwner = Lens.lens (\ListObjectsV' {expectedBucketOwner} -> expectedBucketOwner) (\s@ListObjectsV' {} a -> s {expectedBucketOwner = a} :: ListObjectsV)

-- | Bucket name to list.
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
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this action using S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon S3 User Guide/.
listObjectsV_bucket :: Lens.Lens' ListObjectsV BucketName
listObjectsV_bucket = Lens.lens (\ListObjectsV' {bucket} -> bucket) (\s@ListObjectsV' {} a -> s {bucket = a} :: ListObjectsV)

instance Core.AWSRequest ListObjectsV where
  type AWSResponse ListObjectsV = ListObjectsVResponse
  request =
    Request.s3vhost
      Prelude.. Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListObjectsVResponse'
            Prelude.<$> (x Core..@? "StartAfter")
            Prelude.<*> (x Core..@? "KeyCount")
            Prelude.<*> (Core.may (Core.parseXMLList "Contents") x)
            Prelude.<*> (x Core..@? "ContinuationToken")
            Prelude.<*> (x Core..@? "Prefix")
            Prelude.<*> (Core.may (Core.parseXMLList "CommonPrefixes") x)
            Prelude.<*> (x Core..@? "EncodingType")
            Prelude.<*> (x Core..@? "Name")
            Prelude.<*> (x Core..@? "NextContinuationToken")
            Prelude.<*> (x Core..@? "MaxKeys")
            Prelude.<*> (x Core..@? "IsTruncated")
            Prelude.<*> (x Core..@? "Delimiter")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListObjectsV

instance Prelude.NFData ListObjectsV

instance Core.ToHeaders ListObjectsV where
  toHeaders ListObjectsV' {..} =
    Prelude.mconcat
      [ "x-amz-request-payer" Core.=# requestPayer,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath ListObjectsV where
  toPath ListObjectsV' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery ListObjectsV where
  toQuery ListObjectsV' {..} =
    Prelude.mconcat
      [ "start-after" Core.=: startAfter,
        "continuation-token" Core.=: continuationToken,
        "fetch-owner" Core.=: fetchOwner,
        "prefix" Core.=: prefix,
        "encoding-type" Core.=: encodingType,
        "max-keys" Core.=: maxKeys,
        "delimiter" Core.=: delimiter,
        "list-type=2"
      ]

-- | /See:/ 'newListObjectsVResponse' smart constructor.
data ListObjectsVResponse = ListObjectsVResponse'
  { -- | If StartAfter was sent with the request, it is included in the response.
    startAfter :: Prelude.Maybe Prelude.Text,
    -- | KeyCount is the number of keys returned with this request. KeyCount will
    -- always be less than or equals to MaxKeys field. Say you ask for 50 keys,
    -- your result will include less than equals 50 keys
    keyCount :: Prelude.Maybe Prelude.Int,
    -- | Metadata about each object returned.
    contents :: Prelude.Maybe [Object],
    -- | If ContinuationToken was sent with the request, it is included in the
    -- response.
    continuationToken :: Prelude.Maybe Prelude.Text,
    -- | Keys that begin with the indicated prefix.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | All of the keys (up to 1,000) rolled up into a common prefix count as a
    -- single return when calculating the number of returns.
    --
    -- A response can contain @CommonPrefixes@ only if you specify a delimiter.
    --
    -- @CommonPrefixes@ contains all (if there are any) keys between @Prefix@
    -- and the next occurrence of the string specified by a delimiter.
    --
    -- @CommonPrefixes@ lists keys that act like subdirectories in the
    -- directory specified by @Prefix@.
    --
    -- For example, if the prefix is @notes\/@ and the delimiter is a slash
    -- (@\/@) as in @notes\/summer\/july@, the common prefix is
    -- @notes\/summer\/@. All of the keys that roll up into a common prefix
    -- count as a single return when calculating the number of returns.
    commonPrefixes :: Prelude.Maybe [CommonPrefix],
    -- | Encoding type used by Amazon S3 to encode object key names in the XML
    -- response.
    --
    -- If you specify the encoding-type request parameter, Amazon S3 includes
    -- this element in the response, and returns encoded key name values in the
    -- following response elements:
    --
    -- @Delimiter, Prefix, Key,@ and @StartAfter@.
    encodingType :: Prelude.Maybe EncodingType,
    -- | The bucket name.
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
    -- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
    -- When using this action using S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts bucket ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    name :: Prelude.Maybe BucketName,
    -- | @NextContinuationToken@ is sent when @isTruncated@ is true, which means
    -- there are more keys in the bucket that can be listed. The next list
    -- requests to Amazon S3 can be continued with this
    -- @NextContinuationToken@. @NextContinuationToken@ is obfuscated and is
    -- not a real key
    nextContinuationToken :: Prelude.Maybe Prelude.Text,
    -- | Sets the maximum number of keys returned in the response. By default the
    -- action returns up to 1,000 key names. The response might contain fewer
    -- keys but will never contain more.
    maxKeys :: Prelude.Maybe Prelude.Int,
    -- | Set to false if all of the results were returned. Set to true if more
    -- keys are available to return. If the number of results exceeds that
    -- specified by MaxKeys, all of the results might not be returned.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | Causes keys that contain the same string between the prefix and the
    -- first occurrence of the delimiter to be rolled up into a single result
    -- element in the CommonPrefixes collection. These rolled-up keys are not
    -- returned elsewhere in the response. Each rolled-up result counts as only
    -- one return against the @MaxKeys@ value.
    delimiter :: Prelude.Maybe Delimiter,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObjectsVResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startAfter', 'listObjectsVResponse_startAfter' - If StartAfter was sent with the request, it is included in the response.
--
-- 'keyCount', 'listObjectsVResponse_keyCount' - KeyCount is the number of keys returned with this request. KeyCount will
-- always be less than or equals to MaxKeys field. Say you ask for 50 keys,
-- your result will include less than equals 50 keys
--
-- 'contents', 'listObjectsVResponse_contents' - Metadata about each object returned.
--
-- 'continuationToken', 'listObjectsVResponse_continuationToken' - If ContinuationToken was sent with the request, it is included in the
-- response.
--
-- 'prefix', 'listObjectsVResponse_prefix' - Keys that begin with the indicated prefix.
--
-- 'commonPrefixes', 'listObjectsVResponse_commonPrefixes' - All of the keys (up to 1,000) rolled up into a common prefix count as a
-- single return when calculating the number of returns.
--
-- A response can contain @CommonPrefixes@ only if you specify a delimiter.
--
-- @CommonPrefixes@ contains all (if there are any) keys between @Prefix@
-- and the next occurrence of the string specified by a delimiter.
--
-- @CommonPrefixes@ lists keys that act like subdirectories in the
-- directory specified by @Prefix@.
--
-- For example, if the prefix is @notes\/@ and the delimiter is a slash
-- (@\/@) as in @notes\/summer\/july@, the common prefix is
-- @notes\/summer\/@. All of the keys that roll up into a common prefix
-- count as a single return when calculating the number of returns.
--
-- 'encodingType', 'listObjectsVResponse_encodingType' - Encoding type used by Amazon S3 to encode object key names in the XML
-- response.
--
-- If you specify the encoding-type request parameter, Amazon S3 includes
-- this element in the response, and returns encoded key name values in the
-- following response elements:
--
-- @Delimiter, Prefix, Key,@ and @StartAfter@.
--
-- 'name', 'listObjectsVResponse_name' - The bucket name.
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
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this action using S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon S3 User Guide/.
--
-- 'nextContinuationToken', 'listObjectsVResponse_nextContinuationToken' - @NextContinuationToken@ is sent when @isTruncated@ is true, which means
-- there are more keys in the bucket that can be listed. The next list
-- requests to Amazon S3 can be continued with this
-- @NextContinuationToken@. @NextContinuationToken@ is obfuscated and is
-- not a real key
--
-- 'maxKeys', 'listObjectsVResponse_maxKeys' - Sets the maximum number of keys returned in the response. By default the
-- action returns up to 1,000 key names. The response might contain fewer
-- keys but will never contain more.
--
-- 'isTruncated', 'listObjectsVResponse_isTruncated' - Set to false if all of the results were returned. Set to true if more
-- keys are available to return. If the number of results exceeds that
-- specified by MaxKeys, all of the results might not be returned.
--
-- 'delimiter', 'listObjectsVResponse_delimiter' - Causes keys that contain the same string between the prefix and the
-- first occurrence of the delimiter to be rolled up into a single result
-- element in the CommonPrefixes collection. These rolled-up keys are not
-- returned elsewhere in the response. Each rolled-up result counts as only
-- one return against the @MaxKeys@ value.
--
-- 'httpStatus', 'listObjectsVResponse_httpStatus' - The response's http status code.
newListObjectsVResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListObjectsVResponse
newListObjectsVResponse pHttpStatus_ =
  ListObjectsVResponse'
    { startAfter = Prelude.Nothing,
      keyCount = Prelude.Nothing,
      contents = Prelude.Nothing,
      continuationToken = Prelude.Nothing,
      prefix = Prelude.Nothing,
      commonPrefixes = Prelude.Nothing,
      encodingType = Prelude.Nothing,
      name = Prelude.Nothing,
      nextContinuationToken = Prelude.Nothing,
      maxKeys = Prelude.Nothing,
      isTruncated = Prelude.Nothing,
      delimiter = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If StartAfter was sent with the request, it is included in the response.
listObjectsVResponse_startAfter :: Lens.Lens' ListObjectsVResponse (Prelude.Maybe Prelude.Text)
listObjectsVResponse_startAfter = Lens.lens (\ListObjectsVResponse' {startAfter} -> startAfter) (\s@ListObjectsVResponse' {} a -> s {startAfter = a} :: ListObjectsVResponse)

-- | KeyCount is the number of keys returned with this request. KeyCount will
-- always be less than or equals to MaxKeys field. Say you ask for 50 keys,
-- your result will include less than equals 50 keys
listObjectsVResponse_keyCount :: Lens.Lens' ListObjectsVResponse (Prelude.Maybe Prelude.Int)
listObjectsVResponse_keyCount = Lens.lens (\ListObjectsVResponse' {keyCount} -> keyCount) (\s@ListObjectsVResponse' {} a -> s {keyCount = a} :: ListObjectsVResponse)

-- | Metadata about each object returned.
listObjectsVResponse_contents :: Lens.Lens' ListObjectsVResponse (Prelude.Maybe [Object])
listObjectsVResponse_contents = Lens.lens (\ListObjectsVResponse' {contents} -> contents) (\s@ListObjectsVResponse' {} a -> s {contents = a} :: ListObjectsVResponse) Prelude.. Lens.mapping Lens.coerced

-- | If ContinuationToken was sent with the request, it is included in the
-- response.
listObjectsVResponse_continuationToken :: Lens.Lens' ListObjectsVResponse (Prelude.Maybe Prelude.Text)
listObjectsVResponse_continuationToken = Lens.lens (\ListObjectsVResponse' {continuationToken} -> continuationToken) (\s@ListObjectsVResponse' {} a -> s {continuationToken = a} :: ListObjectsVResponse)

-- | Keys that begin with the indicated prefix.
listObjectsVResponse_prefix :: Lens.Lens' ListObjectsVResponse (Prelude.Maybe Prelude.Text)
listObjectsVResponse_prefix = Lens.lens (\ListObjectsVResponse' {prefix} -> prefix) (\s@ListObjectsVResponse' {} a -> s {prefix = a} :: ListObjectsVResponse)

-- | All of the keys (up to 1,000) rolled up into a common prefix count as a
-- single return when calculating the number of returns.
--
-- A response can contain @CommonPrefixes@ only if you specify a delimiter.
--
-- @CommonPrefixes@ contains all (if there are any) keys between @Prefix@
-- and the next occurrence of the string specified by a delimiter.
--
-- @CommonPrefixes@ lists keys that act like subdirectories in the
-- directory specified by @Prefix@.
--
-- For example, if the prefix is @notes\/@ and the delimiter is a slash
-- (@\/@) as in @notes\/summer\/july@, the common prefix is
-- @notes\/summer\/@. All of the keys that roll up into a common prefix
-- count as a single return when calculating the number of returns.
listObjectsVResponse_commonPrefixes :: Lens.Lens' ListObjectsVResponse (Prelude.Maybe [CommonPrefix])
listObjectsVResponse_commonPrefixes = Lens.lens (\ListObjectsVResponse' {commonPrefixes} -> commonPrefixes) (\s@ListObjectsVResponse' {} a -> s {commonPrefixes = a} :: ListObjectsVResponse) Prelude.. Lens.mapping Lens.coerced

-- | Encoding type used by Amazon S3 to encode object key names in the XML
-- response.
--
-- If you specify the encoding-type request parameter, Amazon S3 includes
-- this element in the response, and returns encoded key name values in the
-- following response elements:
--
-- @Delimiter, Prefix, Key,@ and @StartAfter@.
listObjectsVResponse_encodingType :: Lens.Lens' ListObjectsVResponse (Prelude.Maybe EncodingType)
listObjectsVResponse_encodingType = Lens.lens (\ListObjectsVResponse' {encodingType} -> encodingType) (\s@ListObjectsVResponse' {} a -> s {encodingType = a} :: ListObjectsVResponse)

-- | The bucket name.
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
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this action using S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon S3 User Guide/.
listObjectsVResponse_name :: Lens.Lens' ListObjectsVResponse (Prelude.Maybe BucketName)
listObjectsVResponse_name = Lens.lens (\ListObjectsVResponse' {name} -> name) (\s@ListObjectsVResponse' {} a -> s {name = a} :: ListObjectsVResponse)

-- | @NextContinuationToken@ is sent when @isTruncated@ is true, which means
-- there are more keys in the bucket that can be listed. The next list
-- requests to Amazon S3 can be continued with this
-- @NextContinuationToken@. @NextContinuationToken@ is obfuscated and is
-- not a real key
listObjectsVResponse_nextContinuationToken :: Lens.Lens' ListObjectsVResponse (Prelude.Maybe Prelude.Text)
listObjectsVResponse_nextContinuationToken = Lens.lens (\ListObjectsVResponse' {nextContinuationToken} -> nextContinuationToken) (\s@ListObjectsVResponse' {} a -> s {nextContinuationToken = a} :: ListObjectsVResponse)

-- | Sets the maximum number of keys returned in the response. By default the
-- action returns up to 1,000 key names. The response might contain fewer
-- keys but will never contain more.
listObjectsVResponse_maxKeys :: Lens.Lens' ListObjectsVResponse (Prelude.Maybe Prelude.Int)
listObjectsVResponse_maxKeys = Lens.lens (\ListObjectsVResponse' {maxKeys} -> maxKeys) (\s@ListObjectsVResponse' {} a -> s {maxKeys = a} :: ListObjectsVResponse)

-- | Set to false if all of the results were returned. Set to true if more
-- keys are available to return. If the number of results exceeds that
-- specified by MaxKeys, all of the results might not be returned.
listObjectsVResponse_isTruncated :: Lens.Lens' ListObjectsVResponse (Prelude.Maybe Prelude.Bool)
listObjectsVResponse_isTruncated = Lens.lens (\ListObjectsVResponse' {isTruncated} -> isTruncated) (\s@ListObjectsVResponse' {} a -> s {isTruncated = a} :: ListObjectsVResponse)

-- | Causes keys that contain the same string between the prefix and the
-- first occurrence of the delimiter to be rolled up into a single result
-- element in the CommonPrefixes collection. These rolled-up keys are not
-- returned elsewhere in the response. Each rolled-up result counts as only
-- one return against the @MaxKeys@ value.
listObjectsVResponse_delimiter :: Lens.Lens' ListObjectsVResponse (Prelude.Maybe Delimiter)
listObjectsVResponse_delimiter = Lens.lens (\ListObjectsVResponse' {delimiter} -> delimiter) (\s@ListObjectsVResponse' {} a -> s {delimiter = a} :: ListObjectsVResponse)

-- | The response's http status code.
listObjectsVResponse_httpStatus :: Lens.Lens' ListObjectsVResponse Prelude.Int
listObjectsVResponse_httpStatus = Lens.lens (\ListObjectsVResponse' {httpStatus} -> httpStatus) (\s@ListObjectsVResponse' {} a -> s {httpStatus = a} :: ListObjectsVResponse)

instance Prelude.NFData ListObjectsVResponse
