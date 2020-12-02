{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.HeadObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The HEAD operation retrieves metadata from an object without returning the object itself. This operation is useful if you're only interested in an object's metadata. To use HEAD, you must have READ access to the object.
--
--
-- A @HEAD@ request has the same options as a @GET@ operation on an object. The response is identical to the @GET@ response except that there is no response body.
--
-- If you encrypt an object by using server-side encryption with customer-provided encryption keys (SSE-C) when you store the object in Amazon S3, then when you retrieve the metadata from the object, you must use the following headers:
--
--     * x-amz-server-side-encryption-customer-algorithm
--
--     * x-amz-server-side-encryption-customer-key
--
--     * x-amz-server-side-encryption-customer-key-MD5
--
--
--
-- For more information about SSE-C, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html Server-Side Encryption (Using Customer-Provided Encryption Keys)> .
--
-- Request headers are limited to 8 KB in size. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTCommonRequestHeaders.html Common Request Headers> .
--
-- Consider the following when using request headers:
--
--     * Consideration 1 – If both of the @If-Match@ and @If-Unmodified-Since@ headers are present in the request as follows:
--
--     * @If-Match@ condition evaluates to @true@ , and;
--
--     * @If-Unmodified-Since@ condition evaluates to @false@ ;
--
--
--
-- Then Amazon S3 returns @200 OK@ and the data requested.
--
--     * Consideration 2 – If both of the @If-None-Match@ and @If-Modified-Since@ headers are present in the request as follows:
--
--     * @If-None-Match@ condition evaluates to @false@ , and;
--
--     * @If-Modified-Since@ condition evaluates to @true@ ;
--
--
--
-- Then Amazon S3 returns the @304 Not Modified@ response code.
--
--
--
-- For more information about conditional requests, see <https://tools.ietf.org/html/rfc7232 RFC 7232> .
--
-- __Permissions__
--
-- You need the @s3:GetObject@ permission for this operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> . If the object you request does not exist, the error Amazon S3 returns depends on whether you also have the s3:ListBucket permission.
--
--     * If you have the @s3:ListBucket@ permission on the bucket, Amazon S3 returns an HTTP status code 404 ("no such key") error.
--
--     * If you don’t have the @s3:ListBucket@ permission, Amazon S3 returns an HTTP status code 403 ("access denied") error.
--
--
--
-- The following operation is related to @HeadObject@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Network.AWS.S3.HeadObject
  ( -- * Creating a Request
    headObject,
    HeadObject,

    -- * Request Lenses
    hoIfMatch,
    hoVersionId,
    hoSSECustomerAlgorithm,
    hoSSECustomerKey,
    hoRequestPayer,
    hoIfModifiedSince,
    hoPartNumber,
    hoRange,
    hoIfUnmodifiedSince,
    hoSSECustomerKeyMD5,
    hoIfNoneMatch,
    hoExpectedBucketOwner,
    hoBucket,
    hoKey,

    -- * Destructuring the Response
    headObjectResponse,
    HeadObjectResponse,

    -- * Response Lenses
    horsRequestCharged,
    horsPartsCount,
    horsETag,
    horsVersionId,
    horsContentLength,
    horsObjectLockMode,
    horsExpires,
    horsRestore,
    horsExpiration,
    horsDeleteMarker,
    horsArchiveStatus,
    horsSSECustomerAlgorithm,
    horsMissingMeta,
    horsWebsiteRedirectLocation,
    horsAcceptRanges,
    horsStorageClass,
    horsSSECustomerKeyMD5,
    horsSSEKMSKeyId,
    horsContentEncoding,
    horsObjectLockRetainUntilDate,
    horsMetadata,
    horsReplicationStatus,
    horsCacheControl,
    horsContentLanguage,
    horsLastModified,
    horsObjectLockLegalHoldStatus,
    horsContentDisposition,
    horsServerSideEncryption,
    horsContentType,
    horsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'headObject' smart constructor.
data HeadObject = HeadObject'
  { _hoIfMatch :: !(Maybe Text),
    _hoVersionId :: !(Maybe ObjectVersionId),
    _hoSSECustomerAlgorithm :: !(Maybe Text),
    _hoSSECustomerKey :: !(Maybe (Sensitive Text)),
    _hoRequestPayer :: !(Maybe RequestPayer),
    _hoIfModifiedSince :: !(Maybe ISO8601),
    _hoPartNumber :: !(Maybe Int),
    _hoRange :: !(Maybe Text),
    _hoIfUnmodifiedSince :: !(Maybe ISO8601),
    _hoSSECustomerKeyMD5 :: !(Maybe Text),
    _hoIfNoneMatch :: !(Maybe Text),
    _hoExpectedBucketOwner :: !(Maybe Text),
    _hoBucket :: !BucketName,
    _hoKey :: !ObjectKey
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'HeadObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hoIfMatch' - Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
--
-- * 'hoVersionId' - VersionId used to reference a specific version of the object.
--
-- * 'hoSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- * 'hoSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- * 'hoRequestPayer' - Undocumented member.
--
-- * 'hoIfModifiedSince' - Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
--
-- * 'hoPartNumber' - Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' HEAD request for the part specified. Useful querying about the size of the part and the number of parts in this object.
--
-- * 'hoRange' - Downloads the specified range bytes of an object. For more information about the HTTP Range header, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
--
-- * 'hoIfUnmodifiedSince' - Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
--
-- * 'hoSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- * 'hoIfNoneMatch' - Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
--
-- * 'hoExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'hoBucket' - The name of the bucket containing the object. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'hoKey' - The object key.
headObject ::
  -- | 'hoBucket'
  BucketName ->
  -- | 'hoKey'
  ObjectKey ->
  HeadObject
headObject pBucket_ pKey_ =
  HeadObject'
    { _hoIfMatch = Nothing,
      _hoVersionId = Nothing,
      _hoSSECustomerAlgorithm = Nothing,
      _hoSSECustomerKey = Nothing,
      _hoRequestPayer = Nothing,
      _hoIfModifiedSince = Nothing,
      _hoPartNumber = Nothing,
      _hoRange = Nothing,
      _hoIfUnmodifiedSince = Nothing,
      _hoSSECustomerKeyMD5 = Nothing,
      _hoIfNoneMatch = Nothing,
      _hoExpectedBucketOwner = Nothing,
      _hoBucket = pBucket_,
      _hoKey = pKey_
    }

-- | Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
hoIfMatch :: Lens' HeadObject (Maybe Text)
hoIfMatch = lens _hoIfMatch (\s a -> s {_hoIfMatch = a})

-- | VersionId used to reference a specific version of the object.
hoVersionId :: Lens' HeadObject (Maybe ObjectVersionId)
hoVersionId = lens _hoVersionId (\s a -> s {_hoVersionId = a})

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
hoSSECustomerAlgorithm :: Lens' HeadObject (Maybe Text)
hoSSECustomerAlgorithm = lens _hoSSECustomerAlgorithm (\s a -> s {_hoSSECustomerAlgorithm = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
hoSSECustomerKey :: Lens' HeadObject (Maybe Text)
hoSSECustomerKey = lens _hoSSECustomerKey (\s a -> s {_hoSSECustomerKey = a}) . mapping _Sensitive

-- | Undocumented member.
hoRequestPayer :: Lens' HeadObject (Maybe RequestPayer)
hoRequestPayer = lens _hoRequestPayer (\s a -> s {_hoRequestPayer = a})

-- | Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
hoIfModifiedSince :: Lens' HeadObject (Maybe UTCTime)
hoIfModifiedSince = lens _hoIfModifiedSince (\s a -> s {_hoIfModifiedSince = a}) . mapping _Time

-- | Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' HEAD request for the part specified. Useful querying about the size of the part and the number of parts in this object.
hoPartNumber :: Lens' HeadObject (Maybe Int)
hoPartNumber = lens _hoPartNumber (\s a -> s {_hoPartNumber = a})

-- | Downloads the specified range bytes of an object. For more information about the HTTP Range header, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
hoRange :: Lens' HeadObject (Maybe Text)
hoRange = lens _hoRange (\s a -> s {_hoRange = a})

-- | Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
hoIfUnmodifiedSince :: Lens' HeadObject (Maybe UTCTime)
hoIfUnmodifiedSince = lens _hoIfUnmodifiedSince (\s a -> s {_hoIfUnmodifiedSince = a}) . mapping _Time

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
hoSSECustomerKeyMD5 :: Lens' HeadObject (Maybe Text)
hoSSECustomerKeyMD5 = lens _hoSSECustomerKeyMD5 (\s a -> s {_hoSSECustomerKeyMD5 = a})

-- | Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
hoIfNoneMatch :: Lens' HeadObject (Maybe Text)
hoIfNoneMatch = lens _hoIfNoneMatch (\s a -> s {_hoIfNoneMatch = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
hoExpectedBucketOwner :: Lens' HeadObject (Maybe Text)
hoExpectedBucketOwner = lens _hoExpectedBucketOwner (\s a -> s {_hoExpectedBucketOwner = a})

-- | The name of the bucket containing the object. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
hoBucket :: Lens' HeadObject BucketName
hoBucket = lens _hoBucket (\s a -> s {_hoBucket = a})

-- | The object key.
hoKey :: Lens' HeadObject ObjectKey
hoKey = lens _hoKey (\s a -> s {_hoKey = a})

instance AWSRequest HeadObject where
  type Rs HeadObject = HeadObjectResponse
  request = head' s3
  response =
    receiveEmpty
      ( \s h x ->
          HeadObjectResponse'
            <$> (h .#? "x-amz-request-charged")
            <*> (h .#? "x-amz-mp-parts-count")
            <*> (h .#? "ETag")
            <*> (h .#? "x-amz-version-id")
            <*> (h .#? "Content-Length")
            <*> (h .#? "x-amz-object-lock-mode")
            <*> (h .#? "Expires")
            <*> (h .#? "x-amz-restore")
            <*> (h .#? "x-amz-expiration")
            <*> (h .#? "x-amz-delete-marker")
            <*> (h .#? "x-amz-archive-status")
            <*> (h .#? "x-amz-server-side-encryption-customer-algorithm")
            <*> (h .#? "x-amz-missing-meta")
            <*> (h .#? "x-amz-website-redirect-location")
            <*> (h .#? "accept-ranges")
            <*> (h .#? "x-amz-storage-class")
            <*> (h .#? "x-amz-server-side-encryption-customer-key-MD5")
            <*> (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
            <*> (h .#? "Content-Encoding")
            <*> (h .#? "x-amz-object-lock-retain-until-date")
            <*> (parseHeadersMap "x-amz-meta-" h)
            <*> (h .#? "x-amz-replication-status")
            <*> (h .#? "Cache-Control")
            <*> (h .#? "Content-Language")
            <*> (h .#? "Last-Modified")
            <*> (h .#? "x-amz-object-lock-legal-hold")
            <*> (h .#? "Content-Disposition")
            <*> (h .#? "x-amz-server-side-encryption")
            <*> (h .#? "Content-Type")
            <*> (pure (fromEnum s))
      )

instance Hashable HeadObject

instance NFData HeadObject

instance ToHeaders HeadObject where
  toHeaders HeadObject' {..} =
    mconcat
      [ "If-Match" =# _hoIfMatch,
        "x-amz-server-side-encryption-customer-algorithm"
          =# _hoSSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" =# _hoSSECustomerKey,
        "x-amz-request-payer" =# _hoRequestPayer,
        "If-Modified-Since" =# _hoIfModifiedSince,
        "Range" =# _hoRange,
        "If-Unmodified-Since" =# _hoIfUnmodifiedSince,
        "x-amz-server-side-encryption-customer-key-MD5"
          =# _hoSSECustomerKeyMD5,
        "If-None-Match" =# _hoIfNoneMatch,
        "x-amz-expected-bucket-owner" =# _hoExpectedBucketOwner
      ]

instance ToPath HeadObject where
  toPath HeadObject' {..} =
    mconcat ["/", toBS _hoBucket, "/", toBS _hoKey]

instance ToQuery HeadObject where
  toQuery HeadObject' {..} =
    mconcat
      ["versionId" =: _hoVersionId, "partNumber" =: _hoPartNumber]

-- | /See:/ 'headObjectResponse' smart constructor.
data HeadObjectResponse = HeadObjectResponse'
  { _horsRequestCharged ::
      !(Maybe RequestCharged),
    _horsPartsCount :: !(Maybe Int),
    _horsETag :: !(Maybe ETag),
    _horsVersionId :: !(Maybe ObjectVersionId),
    _horsContentLength :: !(Maybe Integer),
    _horsObjectLockMode :: !(Maybe ObjectLockMode),
    _horsExpires :: !(Maybe ISO8601),
    _horsRestore :: !(Maybe Text),
    _horsExpiration :: !(Maybe Text),
    _horsDeleteMarker :: !(Maybe Bool),
    _horsArchiveStatus :: !(Maybe ArchiveStatus),
    _horsSSECustomerAlgorithm :: !(Maybe Text),
    _horsMissingMeta :: !(Maybe Int),
    _horsWebsiteRedirectLocation :: !(Maybe Text),
    _horsAcceptRanges :: !(Maybe Text),
    _horsStorageClass :: !(Maybe StorageClass),
    _horsSSECustomerKeyMD5 :: !(Maybe Text),
    _horsSSEKMSKeyId :: !(Maybe (Sensitive Text)),
    _horsContentEncoding :: !(Maybe Text),
    _horsObjectLockRetainUntilDate :: !(Maybe ISO8601),
    _horsMetadata :: !(Map Text (Text)),
    _horsReplicationStatus :: !(Maybe ReplicationStatus),
    _horsCacheControl :: !(Maybe Text),
    _horsContentLanguage :: !(Maybe Text),
    _horsLastModified :: !(Maybe ISO8601),
    _horsObjectLockLegalHoldStatus ::
      !(Maybe ObjectLockLegalHoldStatus),
    _horsContentDisposition :: !(Maybe Text),
    _horsServerSideEncryption ::
      !(Maybe ServerSideEncryption),
    _horsContentType :: !(Maybe Text),
    _horsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'HeadObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'horsRequestCharged' - Undocumented member.
--
-- * 'horsPartsCount' - The count of parts this object has.
--
-- * 'horsETag' - An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL.
--
-- * 'horsVersionId' - Version of the object.
--
-- * 'horsContentLength' - Size of the body in bytes.
--
-- * 'horsObjectLockMode' - The Object Lock mode, if any, that's in effect for this object. This header is only returned if the requester has the @s3:GetObjectRetention@ permission. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
--
-- * 'horsExpires' - The date and time at which the object is no longer cacheable.
--
-- * 'horsRestore' - If the object is an archived object (an object whose storage class is GLACIER), the response includes this header if either the archive restoration is in progress (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject> or an archive copy is already restored. If an archive copy is already restored, the header value indicates when Amazon S3 is scheduled to delete the object copy. For example: @x-amz-restore: ongoing-request="false", expiry-date="Fri, 23 Dec 2012 00:00:00 GMT"@  If the object restoration is in progress, the header returns the value @ongoing-request="true"@ . For more information about archiving objects, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-transition-general-considerations Transitioning Objects: General Considerations> .
--
-- * 'horsExpiration' - If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key-value pairs providing object expiration information. The value of the rule-id is URL encoded.
--
-- * 'horsDeleteMarker' - Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
--
-- * 'horsArchiveStatus' - The archive state of the head object.
--
-- * 'horsSSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- * 'horsMissingMeta' - This is set to the number of metadata entries not returned in @x-amz-meta@ headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
--
-- * 'horsWebsiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- * 'horsAcceptRanges' - Indicates that a range of bytes was specified.
--
-- * 'horsStorageClass' - Provides storage class information of the object. Amazon S3 returns this header for all objects except for S3 Standard storage class objects. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> .
--
-- * 'horsSSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- * 'horsSSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- * 'horsContentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- * 'horsObjectLockRetainUntilDate' - The date and time when the Object Lock retention period expires. This header is only returned if the requester has the @s3:GetObjectRetention@ permission.
--
-- * 'horsMetadata' - A map of metadata to store with the object in S3.
--
-- * 'horsReplicationStatus' - Amazon S3 can return this header if your request involves a bucket that is either a source or destination in a replication rule. In replication, you have a source bucket on which you configure replication and destination bucket where Amazon S3 stores object replicas. When you request an object (@GetObject@ ) or object metadata (@HeadObject@ ) from these buckets, Amazon S3 will return the @x-amz-replication-status@ header in the response as follows:     * If requesting an object from the source bucket — Amazon S3 will return the @x-amz-replication-status@ header if the object in your request is eligible for replication. For example, suppose that in your replication configuration, you specify object prefix @TaxDocs@ requesting Amazon S3 to replicate objects with key prefix @TaxDocs@ . Any objects you upload with this key name prefix, for example @TaxDocs/document1.pdf@ , are eligible for replication. For any object request with this key name prefix, Amazon S3 will return the @x-amz-replication-status@ header with value PENDING, COMPLETED or FAILED indicating object replication status.     * If requesting an object from the destination bucket — Amazon S3 will return the @x-amz-replication-status@ header with value REPLICA if the object in your request is a replica that Amazon S3 created. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Replication> .
--
-- * 'horsCacheControl' - Specifies caching behavior along the request/reply chain.
--
-- * 'horsContentLanguage' - The language the content is in.
--
-- * 'horsLastModified' - Last modified date of the object
--
-- * 'horsObjectLockLegalHoldStatus' - Specifies whether a legal hold is in effect for this object. This header is only returned if the requester has the @s3:GetObjectLegalHold@ permission. This header is not returned if the specified version of this object has never had a legal hold applied. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
--
-- * 'horsContentDisposition' - Specifies presentational information for the object.
--
-- * 'horsServerSideEncryption' - If the object is stored using server-side encryption either with an AWS KMS customer master key (CMK) or an Amazon S3-managed encryption key, the response includes this header with the value of the server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- * 'horsContentType' - A standard MIME type describing the format of the object data.
--
-- * 'horsResponseStatus' - -- | The response status code.
headObjectResponse ::
  -- | 'horsResponseStatus'
  Int ->
  HeadObjectResponse
headObjectResponse pResponseStatus_ =
  HeadObjectResponse'
    { _horsRequestCharged = Nothing,
      _horsPartsCount = Nothing,
      _horsETag = Nothing,
      _horsVersionId = Nothing,
      _horsContentLength = Nothing,
      _horsObjectLockMode = Nothing,
      _horsExpires = Nothing,
      _horsRestore = Nothing,
      _horsExpiration = Nothing,
      _horsDeleteMarker = Nothing,
      _horsArchiveStatus = Nothing,
      _horsSSECustomerAlgorithm = Nothing,
      _horsMissingMeta = Nothing,
      _horsWebsiteRedirectLocation = Nothing,
      _horsAcceptRanges = Nothing,
      _horsStorageClass = Nothing,
      _horsSSECustomerKeyMD5 = Nothing,
      _horsSSEKMSKeyId = Nothing,
      _horsContentEncoding = Nothing,
      _horsObjectLockRetainUntilDate = Nothing,
      _horsMetadata = mempty,
      _horsReplicationStatus = Nothing,
      _horsCacheControl = Nothing,
      _horsContentLanguage = Nothing,
      _horsLastModified = Nothing,
      _horsObjectLockLegalHoldStatus = Nothing,
      _horsContentDisposition = Nothing,
      _horsServerSideEncryption = Nothing,
      _horsContentType = Nothing,
      _horsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
horsRequestCharged :: Lens' HeadObjectResponse (Maybe RequestCharged)
horsRequestCharged = lens _horsRequestCharged (\s a -> s {_horsRequestCharged = a})

-- | The count of parts this object has.
horsPartsCount :: Lens' HeadObjectResponse (Maybe Int)
horsPartsCount = lens _horsPartsCount (\s a -> s {_horsPartsCount = a})

-- | An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL.
horsETag :: Lens' HeadObjectResponse (Maybe ETag)
horsETag = lens _horsETag (\s a -> s {_horsETag = a})

-- | Version of the object.
horsVersionId :: Lens' HeadObjectResponse (Maybe ObjectVersionId)
horsVersionId = lens _horsVersionId (\s a -> s {_horsVersionId = a})

-- | Size of the body in bytes.
horsContentLength :: Lens' HeadObjectResponse (Maybe Integer)
horsContentLength = lens _horsContentLength (\s a -> s {_horsContentLength = a})

-- | The Object Lock mode, if any, that's in effect for this object. This header is only returned if the requester has the @s3:GetObjectRetention@ permission. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
horsObjectLockMode :: Lens' HeadObjectResponse (Maybe ObjectLockMode)
horsObjectLockMode = lens _horsObjectLockMode (\s a -> s {_horsObjectLockMode = a})

-- | The date and time at which the object is no longer cacheable.
horsExpires :: Lens' HeadObjectResponse (Maybe UTCTime)
horsExpires = lens _horsExpires (\s a -> s {_horsExpires = a}) . mapping _Time

-- | If the object is an archived object (an object whose storage class is GLACIER), the response includes this header if either the archive restoration is in progress (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject> or an archive copy is already restored. If an archive copy is already restored, the header value indicates when Amazon S3 is scheduled to delete the object copy. For example: @x-amz-restore: ongoing-request="false", expiry-date="Fri, 23 Dec 2012 00:00:00 GMT"@  If the object restoration is in progress, the header returns the value @ongoing-request="true"@ . For more information about archiving objects, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-transition-general-considerations Transitioning Objects: General Considerations> .
horsRestore :: Lens' HeadObjectResponse (Maybe Text)
horsRestore = lens _horsRestore (\s a -> s {_horsRestore = a})

-- | If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key-value pairs providing object expiration information. The value of the rule-id is URL encoded.
horsExpiration :: Lens' HeadObjectResponse (Maybe Text)
horsExpiration = lens _horsExpiration (\s a -> s {_horsExpiration = a})

-- | Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
horsDeleteMarker :: Lens' HeadObjectResponse (Maybe Bool)
horsDeleteMarker = lens _horsDeleteMarker (\s a -> s {_horsDeleteMarker = a})

-- | The archive state of the head object.
horsArchiveStatus :: Lens' HeadObjectResponse (Maybe ArchiveStatus)
horsArchiveStatus = lens _horsArchiveStatus (\s a -> s {_horsArchiveStatus = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
horsSSECustomerAlgorithm :: Lens' HeadObjectResponse (Maybe Text)
horsSSECustomerAlgorithm = lens _horsSSECustomerAlgorithm (\s a -> s {_horsSSECustomerAlgorithm = a})

-- | This is set to the number of metadata entries not returned in @x-amz-meta@ headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
horsMissingMeta :: Lens' HeadObjectResponse (Maybe Int)
horsMissingMeta = lens _horsMissingMeta (\s a -> s {_horsMissingMeta = a})

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
horsWebsiteRedirectLocation :: Lens' HeadObjectResponse (Maybe Text)
horsWebsiteRedirectLocation = lens _horsWebsiteRedirectLocation (\s a -> s {_horsWebsiteRedirectLocation = a})

-- | Indicates that a range of bytes was specified.
horsAcceptRanges :: Lens' HeadObjectResponse (Maybe Text)
horsAcceptRanges = lens _horsAcceptRanges (\s a -> s {_horsAcceptRanges = a})

-- | Provides storage class information of the object. Amazon S3 returns this header for all objects except for S3 Standard storage class objects. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> .
horsStorageClass :: Lens' HeadObjectResponse (Maybe StorageClass)
horsStorageClass = lens _horsStorageClass (\s a -> s {_horsStorageClass = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
horsSSECustomerKeyMD5 :: Lens' HeadObjectResponse (Maybe Text)
horsSSECustomerKeyMD5 = lens _horsSSECustomerKeyMD5 (\s a -> s {_horsSSECustomerKeyMD5 = a})

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
horsSSEKMSKeyId :: Lens' HeadObjectResponse (Maybe Text)
horsSSEKMSKeyId = lens _horsSSEKMSKeyId (\s a -> s {_horsSSEKMSKeyId = a}) . mapping _Sensitive

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
horsContentEncoding :: Lens' HeadObjectResponse (Maybe Text)
horsContentEncoding = lens _horsContentEncoding (\s a -> s {_horsContentEncoding = a})

-- | The date and time when the Object Lock retention period expires. This header is only returned if the requester has the @s3:GetObjectRetention@ permission.
horsObjectLockRetainUntilDate :: Lens' HeadObjectResponse (Maybe UTCTime)
horsObjectLockRetainUntilDate = lens _horsObjectLockRetainUntilDate (\s a -> s {_horsObjectLockRetainUntilDate = a}) . mapping _Time

-- | A map of metadata to store with the object in S3.
horsMetadata :: Lens' HeadObjectResponse (HashMap Text (Text))
horsMetadata = lens _horsMetadata (\s a -> s {_horsMetadata = a}) . _Map

-- | Amazon S3 can return this header if your request involves a bucket that is either a source or destination in a replication rule. In replication, you have a source bucket on which you configure replication and destination bucket where Amazon S3 stores object replicas. When you request an object (@GetObject@ ) or object metadata (@HeadObject@ ) from these buckets, Amazon S3 will return the @x-amz-replication-status@ header in the response as follows:     * If requesting an object from the source bucket — Amazon S3 will return the @x-amz-replication-status@ header if the object in your request is eligible for replication. For example, suppose that in your replication configuration, you specify object prefix @TaxDocs@ requesting Amazon S3 to replicate objects with key prefix @TaxDocs@ . Any objects you upload with this key name prefix, for example @TaxDocs/document1.pdf@ , are eligible for replication. For any object request with this key name prefix, Amazon S3 will return the @x-amz-replication-status@ header with value PENDING, COMPLETED or FAILED indicating object replication status.     * If requesting an object from the destination bucket — Amazon S3 will return the @x-amz-replication-status@ header with value REPLICA if the object in your request is a replica that Amazon S3 created. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Replication> .
horsReplicationStatus :: Lens' HeadObjectResponse (Maybe ReplicationStatus)
horsReplicationStatus = lens _horsReplicationStatus (\s a -> s {_horsReplicationStatus = a})

-- | Specifies caching behavior along the request/reply chain.
horsCacheControl :: Lens' HeadObjectResponse (Maybe Text)
horsCacheControl = lens _horsCacheControl (\s a -> s {_horsCacheControl = a})

-- | The language the content is in.
horsContentLanguage :: Lens' HeadObjectResponse (Maybe Text)
horsContentLanguage = lens _horsContentLanguage (\s a -> s {_horsContentLanguage = a})

-- | Last modified date of the object
horsLastModified :: Lens' HeadObjectResponse (Maybe UTCTime)
horsLastModified = lens _horsLastModified (\s a -> s {_horsLastModified = a}) . mapping _Time

-- | Specifies whether a legal hold is in effect for this object. This header is only returned if the requester has the @s3:GetObjectLegalHold@ permission. This header is not returned if the specified version of this object has never had a legal hold applied. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
horsObjectLockLegalHoldStatus :: Lens' HeadObjectResponse (Maybe ObjectLockLegalHoldStatus)
horsObjectLockLegalHoldStatus = lens _horsObjectLockLegalHoldStatus (\s a -> s {_horsObjectLockLegalHoldStatus = a})

-- | Specifies presentational information for the object.
horsContentDisposition :: Lens' HeadObjectResponse (Maybe Text)
horsContentDisposition = lens _horsContentDisposition (\s a -> s {_horsContentDisposition = a})

-- | If the object is stored using server-side encryption either with an AWS KMS customer master key (CMK) or an Amazon S3-managed encryption key, the response includes this header with the value of the server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
horsServerSideEncryption :: Lens' HeadObjectResponse (Maybe ServerSideEncryption)
horsServerSideEncryption = lens _horsServerSideEncryption (\s a -> s {_horsServerSideEncryption = a})

-- | A standard MIME type describing the format of the object data.
horsContentType :: Lens' HeadObjectResponse (Maybe Text)
horsContentType = lens _horsContentType (\s a -> s {_horsContentType = a})

-- | -- | The response status code.
horsResponseStatus :: Lens' HeadObjectResponse Int
horsResponseStatus = lens _horsResponseStatus (\s a -> s {_horsResponseStatus = a})

instance NFData HeadObjectResponse
