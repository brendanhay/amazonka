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
-- Module      : Network.AWS.S3.GetObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves objects from Amazon S3. To use @GET@ , you must have @READ@ access to the object. If you grant @READ@ access to the anonymous user, you can return the object without using an authorization header.
--
--
-- An Amazon S3 bucket has no directory hierarchy such as you would find in a typical computer file system. You can, however, create a logical hierarchy by using object key names that imply a folder structure. For example, instead of naming an object @sample.jpg@ , you can name it @photos/2006/February/sample.jpg@ .
--
-- To get an object from such a logical hierarchy, specify the full key name for the object in the @GET@ operation. For a virtual hosted-style request example, if you have the object @photos/2006/February/sample.jpg@ , specify the resource as @/photos/2006/February/sample.jpg@ . For a path-style request example, if you have the object @photos/2006/February/sample.jpg@ in the bucket named @examplebucket@ , specify the resource as @/examplebucket/photos/2006/February/sample.jpg@ . For more information about request types, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/VirtualHosting.html#VirtualHostingSpecifyBucket HTTP Host Header Bucket Specification> .
--
-- To distribute large files to many people, you can save bandwidth costs by using BitTorrent. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3Torrent.html Amazon S3 Torrent> . For more information about returning the ACL of an object, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAcl.html GetObjectAcl> .
--
-- If the object you are retrieving is stored in the S3 Glacier or S3 Glacier Deep Archive storage class, or S3 Intelligent-Tiering Archive or S3 Intelligent-Tiering Deep Archive tiers, before you can retrieve the object you must first restore a copy using <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject> . Otherwise, this operation returns an @InvalidObjectStateError@ error. For information about restoring archived objects, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/restoring-objects.html Restoring Archived Objects> .
--
-- Encryption request headers, like @x-amz-server-side-encryption@ , should not be sent for GET requests if your object uses server-side encryption with CMKs stored in AWS KMS (SSE-KMS) or server-side encryption with Amazon S3–managed encryption keys (SSE-S3). If your object does use these types of keys, you’ll get an HTTP 400 BadRequest error.
--
-- If you encrypt an object by using server-side encryption with customer-provided encryption keys (SSE-C) when you store the object in Amazon S3, then when you GET the object, you must use the following headers:
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
-- Assuming you have permission to read object tags (permission for the @s3:GetObjectVersionTagging@ action), the response also returns the @x-amz-tagging-count@ header that provides the count of number of tags associated with the object. You can use <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging> to retrieve the tag set associated with an object.
--
-- __Permissions__
--
-- You need the @s3:GetObject@ permission for this operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> . If the object you request does not exist, the error Amazon S3 returns depends on whether you also have the @s3:ListBucket@ permission.
--
--     * If you have the @s3:ListBucket@ permission on the bucket, Amazon S3 will return an HTTP status code 404 ("no such key") error.
--
--     * If you don’t have the @s3:ListBucket@ permission, Amazon S3 will return an HTTP status code 403 ("access denied") error.
--
--
--
-- __Versioning__
--
-- By default, the GET operation returns the current version of an object. To return a different version, use the @versionId@ subresource.
--
-- For more information about versioning, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketVersioning.html PutBucketVersioning> .
--
-- __Overriding Response Header Values__
--
-- There are times when you want to override certain response header values in a GET response. For example, you might override the Content-Disposition response header value in your GET request.
--
-- You can override values for a set of response headers using the following query parameters. These response header values are sent only on a successful request, that is, when status code 200 OK is returned. The set of headers you can override using these parameters is a subset of the headers that Amazon S3 accepts when you create an object. The response headers that you can override for the GET response are @Content-Type@ , @Content-Language@ , @Expires@ , @Cache-Control@ , @Content-Disposition@ , and @Content-Encoding@ . To override these header values in the GET response, you use the following request parameters.
--
--     * @response-content-type@
--
--     * @response-content-language@
--
--     * @response-expires@
--
--     * @response-cache-control@
--
--     * @response-content-disposition@
--
--     * @response-content-encoding@
--
--
--
-- __Additional Considerations about Request Headers__
--
-- If both of the @If-Match@ and @If-Unmodified-Since@ headers are present in the request as follows: @If-Match@ condition evaluates to @true@ , and; @If-Unmodified-Since@ condition evaluates to @false@ ; then, S3 returns 200 OK and the data requested.
--
-- If both of the @If-None-Match@ and @If-Modified-Since@ headers are present in the request as follows:@If-None-Match@ condition evaluates to @false@ , and; @If-Modified-Since@ condition evaluates to @true@ ; then, S3 returns 304 Not Modified response code.
--
-- For more information about conditional requests, see <https://tools.ietf.org/html/rfc7232 RFC 7232> .
--
-- The following operations are related to @GetObject@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBuckets.html ListBuckets>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAcl.html GetObjectAcl>
module Network.AWS.S3.GetObject
  ( -- * Creating a Request
    getObject,
    GetObject,

    -- * Request Lenses
    goIfMatch,
    goVersionId,
    goResponseContentType,
    goResponseContentDisposition,
    goResponseContentLanguage,
    goSSECustomerAlgorithm,
    goSSECustomerKey,
    goRequestPayer,
    goResponseContentEncoding,
    goIfModifiedSince,
    goPartNumber,
    goRange,
    goIfUnmodifiedSince,
    goSSECustomerKeyMD5,
    goResponseCacheControl,
    goResponseExpires,
    goIfNoneMatch,
    goExpectedBucketOwner,
    goBucket,
    goKey,

    -- * Destructuring the Response
    getObjectResponse,
    GetObjectResponse,

    -- * Response Lenses
    gorsRequestCharged,
    gorsPartsCount,
    gorsETag,
    gorsVersionId,
    gorsContentLength,
    gorsObjectLockMode,
    gorsExpires,
    gorsRestore,
    gorsExpiration,
    gorsDeleteMarker,
    gorsSSECustomerAlgorithm,
    gorsTagCount,
    gorsMissingMeta,
    gorsWebsiteRedirectLocation,
    gorsAcceptRanges,
    gorsStorageClass,
    gorsSSECustomerKeyMD5,
    gorsSSEKMSKeyId,
    gorsContentEncoding,
    gorsObjectLockRetainUntilDate,
    gorsMetadata,
    gorsReplicationStatus,
    gorsCacheControl,
    gorsContentLanguage,
    gorsLastModified,
    gorsObjectLockLegalHoldStatus,
    gorsContentDisposition,
    gorsContentRange,
    gorsServerSideEncryption,
    gorsContentType,
    gorsResponseStatus,
    gorsBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getObject' smart constructor.
data GetObject = GetObject'
  { _goIfMatch :: !(Maybe Text),
    _goVersionId :: !(Maybe ObjectVersionId),
    _goResponseContentType :: !(Maybe Text),
    _goResponseContentDisposition :: !(Maybe Text),
    _goResponseContentLanguage :: !(Maybe Text),
    _goSSECustomerAlgorithm :: !(Maybe Text),
    _goSSECustomerKey :: !(Maybe (Sensitive Text)),
    _goRequestPayer :: !(Maybe RequestPayer),
    _goResponseContentEncoding :: !(Maybe Text),
    _goIfModifiedSince :: !(Maybe ISO8601),
    _goPartNumber :: !(Maybe Int),
    _goRange :: !(Maybe Text),
    _goIfUnmodifiedSince :: !(Maybe ISO8601),
    _goSSECustomerKeyMD5 :: !(Maybe Text),
    _goResponseCacheControl :: !(Maybe Text),
    _goResponseExpires :: !(Maybe ISO8601),
    _goIfNoneMatch :: !(Maybe Text),
    _goExpectedBucketOwner :: !(Maybe Text),
    _goBucket :: !BucketName,
    _goKey :: !ObjectKey
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goIfMatch' - Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
--
-- * 'goVersionId' - VersionId used to reference a specific version of the object.
--
-- * 'goResponseContentType' - Sets the @Content-Type@ header of the response.
--
-- * 'goResponseContentDisposition' - Sets the @Content-Disposition@ header of the response
--
-- * 'goResponseContentLanguage' - Sets the @Content-Language@ header of the response.
--
-- * 'goSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- * 'goSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- * 'goRequestPayer' - Undocumented member.
--
-- * 'goResponseContentEncoding' - Sets the @Content-Encoding@ header of the response.
--
-- * 'goIfModifiedSince' - Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
--
-- * 'goPartNumber' - Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' GET request for the part specified. Useful for downloading just a part of an object.
--
-- * 'goRange' - Downloads the specified range bytes of an object. For more information about the HTTP Range header, see <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
--
-- * 'goIfUnmodifiedSince' - Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
--
-- * 'goSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- * 'goResponseCacheControl' - Sets the @Cache-Control@ header of the response.
--
-- * 'goResponseExpires' - Sets the @Expires@ header of the response.
--
-- * 'goIfNoneMatch' - Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
--
-- * 'goExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'goBucket' - The bucket name containing the object.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'goKey' - Key of the object to get.
getObject ::
  -- | 'goBucket'
  BucketName ->
  -- | 'goKey'
  ObjectKey ->
  GetObject
getObject pBucket_ pKey_ =
  GetObject'
    { _goIfMatch = Nothing,
      _goVersionId = Nothing,
      _goResponseContentType = Nothing,
      _goResponseContentDisposition = Nothing,
      _goResponseContentLanguage = Nothing,
      _goSSECustomerAlgorithm = Nothing,
      _goSSECustomerKey = Nothing,
      _goRequestPayer = Nothing,
      _goResponseContentEncoding = Nothing,
      _goIfModifiedSince = Nothing,
      _goPartNumber = Nothing,
      _goRange = Nothing,
      _goIfUnmodifiedSince = Nothing,
      _goSSECustomerKeyMD5 = Nothing,
      _goResponseCacheControl = Nothing,
      _goResponseExpires = Nothing,
      _goIfNoneMatch = Nothing,
      _goExpectedBucketOwner = Nothing,
      _goBucket = pBucket_,
      _goKey = pKey_
    }

-- | Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
goIfMatch :: Lens' GetObject (Maybe Text)
goIfMatch = lens _goIfMatch (\s a -> s {_goIfMatch = a})

-- | VersionId used to reference a specific version of the object.
goVersionId :: Lens' GetObject (Maybe ObjectVersionId)
goVersionId = lens _goVersionId (\s a -> s {_goVersionId = a})

-- | Sets the @Content-Type@ header of the response.
goResponseContentType :: Lens' GetObject (Maybe Text)
goResponseContentType = lens _goResponseContentType (\s a -> s {_goResponseContentType = a})

-- | Sets the @Content-Disposition@ header of the response
goResponseContentDisposition :: Lens' GetObject (Maybe Text)
goResponseContentDisposition = lens _goResponseContentDisposition (\s a -> s {_goResponseContentDisposition = a})

-- | Sets the @Content-Language@ header of the response.
goResponseContentLanguage :: Lens' GetObject (Maybe Text)
goResponseContentLanguage = lens _goResponseContentLanguage (\s a -> s {_goResponseContentLanguage = a})

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
goSSECustomerAlgorithm :: Lens' GetObject (Maybe Text)
goSSECustomerAlgorithm = lens _goSSECustomerAlgorithm (\s a -> s {_goSSECustomerAlgorithm = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
goSSECustomerKey :: Lens' GetObject (Maybe Text)
goSSECustomerKey = lens _goSSECustomerKey (\s a -> s {_goSSECustomerKey = a}) . mapping _Sensitive

-- | Undocumented member.
goRequestPayer :: Lens' GetObject (Maybe RequestPayer)
goRequestPayer = lens _goRequestPayer (\s a -> s {_goRequestPayer = a})

-- | Sets the @Content-Encoding@ header of the response.
goResponseContentEncoding :: Lens' GetObject (Maybe Text)
goResponseContentEncoding = lens _goResponseContentEncoding (\s a -> s {_goResponseContentEncoding = a})

-- | Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
goIfModifiedSince :: Lens' GetObject (Maybe UTCTime)
goIfModifiedSince = lens _goIfModifiedSince (\s a -> s {_goIfModifiedSince = a}) . mapping _Time

-- | Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' GET request for the part specified. Useful for downloading just a part of an object.
goPartNumber :: Lens' GetObject (Maybe Int)
goPartNumber = lens _goPartNumber (\s a -> s {_goPartNumber = a})

-- | Downloads the specified range bytes of an object. For more information about the HTTP Range header, see <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> .
goRange :: Lens' GetObject (Maybe Text)
goRange = lens _goRange (\s a -> s {_goRange = a})

-- | Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
goIfUnmodifiedSince :: Lens' GetObject (Maybe UTCTime)
goIfUnmodifiedSince = lens _goIfUnmodifiedSince (\s a -> s {_goIfUnmodifiedSince = a}) . mapping _Time

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
goSSECustomerKeyMD5 :: Lens' GetObject (Maybe Text)
goSSECustomerKeyMD5 = lens _goSSECustomerKeyMD5 (\s a -> s {_goSSECustomerKeyMD5 = a})

-- | Sets the @Cache-Control@ header of the response.
goResponseCacheControl :: Lens' GetObject (Maybe Text)
goResponseCacheControl = lens _goResponseCacheControl (\s a -> s {_goResponseCacheControl = a})

-- | Sets the @Expires@ header of the response.
goResponseExpires :: Lens' GetObject (Maybe UTCTime)
goResponseExpires = lens _goResponseExpires (\s a -> s {_goResponseExpires = a}) . mapping _Time

-- | Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
goIfNoneMatch :: Lens' GetObject (Maybe Text)
goIfNoneMatch = lens _goIfNoneMatch (\s a -> s {_goIfNoneMatch = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
goExpectedBucketOwner :: Lens' GetObject (Maybe Text)
goExpectedBucketOwner = lens _goExpectedBucketOwner (\s a -> s {_goExpectedBucketOwner = a})

-- | The bucket name containing the object.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
goBucket :: Lens' GetObject BucketName
goBucket = lens _goBucket (\s a -> s {_goBucket = a})

-- | Key of the object to get.
goKey :: Lens' GetObject ObjectKey
goKey = lens _goKey (\s a -> s {_goKey = a})

instance AWSRequest GetObject where
  type Rs GetObject = GetObjectResponse
  request = get s3
  response =
    receiveBody
      ( \s h x ->
          GetObjectResponse'
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
            <*> (h .#? "x-amz-server-side-encryption-customer-algorithm")
            <*> (h .#? "x-amz-tagging-count")
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
            <*> (h .#? "Content-Range")
            <*> (h .#? "x-amz-server-side-encryption")
            <*> (h .#? "Content-Type")
            <*> (pure (fromEnum s))
            <*> (pure x)
      )

instance Hashable GetObject

instance NFData GetObject

instance ToHeaders GetObject where
  toHeaders GetObject' {..} =
    mconcat
      [ "If-Match" =# _goIfMatch,
        "x-amz-server-side-encryption-customer-algorithm"
          =# _goSSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" =# _goSSECustomerKey,
        "x-amz-request-payer" =# _goRequestPayer,
        "If-Modified-Since" =# _goIfModifiedSince,
        "Range" =# _goRange,
        "If-Unmodified-Since" =# _goIfUnmodifiedSince,
        "x-amz-server-side-encryption-customer-key-MD5"
          =# _goSSECustomerKeyMD5,
        "If-None-Match" =# _goIfNoneMatch,
        "x-amz-expected-bucket-owner" =# _goExpectedBucketOwner
      ]

instance ToPath GetObject where
  toPath GetObject' {..} =
    mconcat ["/", toBS _goBucket, "/", toBS _goKey]

instance ToQuery GetObject where
  toQuery GetObject' {..} =
    mconcat
      [ "versionId" =: _goVersionId,
        "response-content-type" =: _goResponseContentType,
        "response-content-disposition" =: _goResponseContentDisposition,
        "response-content-language" =: _goResponseContentLanguage,
        "response-content-encoding" =: _goResponseContentEncoding,
        "partNumber" =: _goPartNumber,
        "response-cache-control" =: _goResponseCacheControl,
        "response-expires" =: _goResponseExpires
      ]

-- | /See:/ 'getObjectResponse' smart constructor.
data GetObjectResponse = GetObjectResponse'
  { _gorsRequestCharged ::
      !(Maybe RequestCharged),
    _gorsPartsCount :: !(Maybe Int),
    _gorsETag :: !(Maybe ETag),
    _gorsVersionId :: !(Maybe ObjectVersionId),
    _gorsContentLength :: !(Maybe Integer),
    _gorsObjectLockMode :: !(Maybe ObjectLockMode),
    _gorsExpires :: !(Maybe ISO8601),
    _gorsRestore :: !(Maybe Text),
    _gorsExpiration :: !(Maybe Text),
    _gorsDeleteMarker :: !(Maybe Bool),
    _gorsSSECustomerAlgorithm :: !(Maybe Text),
    _gorsTagCount :: !(Maybe Int),
    _gorsMissingMeta :: !(Maybe Int),
    _gorsWebsiteRedirectLocation :: !(Maybe Text),
    _gorsAcceptRanges :: !(Maybe Text),
    _gorsStorageClass :: !(Maybe StorageClass),
    _gorsSSECustomerKeyMD5 :: !(Maybe Text),
    _gorsSSEKMSKeyId :: !(Maybe (Sensitive Text)),
    _gorsContentEncoding :: !(Maybe Text),
    _gorsObjectLockRetainUntilDate :: !(Maybe ISO8601),
    _gorsMetadata :: !(Map Text (Text)),
    _gorsReplicationStatus :: !(Maybe ReplicationStatus),
    _gorsCacheControl :: !(Maybe Text),
    _gorsContentLanguage :: !(Maybe Text),
    _gorsLastModified :: !(Maybe ISO8601),
    _gorsObjectLockLegalHoldStatus ::
      !(Maybe ObjectLockLegalHoldStatus),
    _gorsContentDisposition :: !(Maybe Text),
    _gorsContentRange :: !(Maybe Text),
    _gorsServerSideEncryption ::
      !(Maybe ServerSideEncryption),
    _gorsContentType :: !(Maybe Text),
    _gorsResponseStatus :: !Int,
    _gorsBody :: !RsBody
  }
  deriving (Show, Generic)

-- | Creates a value of 'GetObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gorsRequestCharged' - Undocumented member.
--
-- * 'gorsPartsCount' - The count of parts this object has.
--
-- * 'gorsETag' - An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL.
--
-- * 'gorsVersionId' - Version of the object.
--
-- * 'gorsContentLength' - Size of the body in bytes.
--
-- * 'gorsObjectLockMode' - The Object Lock mode currently in place for this object.
--
-- * 'gorsExpires' - The date and time at which the object is no longer cacheable.
--
-- * 'gorsRestore' - Provides information about object restoration operation and expiration time of the restored object copy.
--
-- * 'gorsExpiration' - If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key-value pairs providing object expiration information. The value of the rule-id is URL encoded.
--
-- * 'gorsDeleteMarker' - Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
--
-- * 'gorsSSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- * 'gorsTagCount' - The number of tags, if any, on the object.
--
-- * 'gorsMissingMeta' - This is set to the number of metadata entries not returned in @x-amz-meta@ headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
--
-- * 'gorsWebsiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- * 'gorsAcceptRanges' - Indicates that a range of bytes was specified.
--
-- * 'gorsStorageClass' - Provides storage class information of the object. Amazon S3 returns this header for all objects except for S3 Standard storage class objects.
--
-- * 'gorsSSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- * 'gorsSSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- * 'gorsContentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- * 'gorsObjectLockRetainUntilDate' - The date and time when this object's Object Lock will expire.
--
-- * 'gorsMetadata' - A map of metadata to store with the object in S3.
--
-- * 'gorsReplicationStatus' - Amazon S3 can return this if your request involves a bucket that is either a source or destination in a replication rule.
--
-- * 'gorsCacheControl' - Specifies caching behavior along the request/reply chain.
--
-- * 'gorsContentLanguage' - The language the content is in.
--
-- * 'gorsLastModified' - Last modified date of the object
--
-- * 'gorsObjectLockLegalHoldStatus' - Indicates whether this object has an active legal hold. This field is only returned if you have permission to view an object's legal hold status.
--
-- * 'gorsContentDisposition' - Specifies presentational information for the object.
--
-- * 'gorsContentRange' - The portion of the object returned in the response.
--
-- * 'gorsServerSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- * 'gorsContentType' - A standard MIME type describing the format of the object data.
--
-- * 'gorsResponseStatus' - -- | The response status code.
--
-- * 'gorsBody' - Object data.
getObjectResponse ::
  -- | 'gorsResponseStatus'
  Int ->
  -- | 'gorsBody'
  RsBody ->
  GetObjectResponse
getObjectResponse pResponseStatus_ pBody_ =
  GetObjectResponse'
    { _gorsRequestCharged = Nothing,
      _gorsPartsCount = Nothing,
      _gorsETag = Nothing,
      _gorsVersionId = Nothing,
      _gorsContentLength = Nothing,
      _gorsObjectLockMode = Nothing,
      _gorsExpires = Nothing,
      _gorsRestore = Nothing,
      _gorsExpiration = Nothing,
      _gorsDeleteMarker = Nothing,
      _gorsSSECustomerAlgorithm = Nothing,
      _gorsTagCount = Nothing,
      _gorsMissingMeta = Nothing,
      _gorsWebsiteRedirectLocation = Nothing,
      _gorsAcceptRanges = Nothing,
      _gorsStorageClass = Nothing,
      _gorsSSECustomerKeyMD5 = Nothing,
      _gorsSSEKMSKeyId = Nothing,
      _gorsContentEncoding = Nothing,
      _gorsObjectLockRetainUntilDate = Nothing,
      _gorsMetadata = mempty,
      _gorsReplicationStatus = Nothing,
      _gorsCacheControl = Nothing,
      _gorsContentLanguage = Nothing,
      _gorsLastModified = Nothing,
      _gorsObjectLockLegalHoldStatus = Nothing,
      _gorsContentDisposition = Nothing,
      _gorsContentRange = Nothing,
      _gorsServerSideEncryption = Nothing,
      _gorsContentType = Nothing,
      _gorsResponseStatus = pResponseStatus_,
      _gorsBody = pBody_
    }

-- | Undocumented member.
gorsRequestCharged :: Lens' GetObjectResponse (Maybe RequestCharged)
gorsRequestCharged = lens _gorsRequestCharged (\s a -> s {_gorsRequestCharged = a})

-- | The count of parts this object has.
gorsPartsCount :: Lens' GetObjectResponse (Maybe Int)
gorsPartsCount = lens _gorsPartsCount (\s a -> s {_gorsPartsCount = a})

-- | An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL.
gorsETag :: Lens' GetObjectResponse (Maybe ETag)
gorsETag = lens _gorsETag (\s a -> s {_gorsETag = a})

-- | Version of the object.
gorsVersionId :: Lens' GetObjectResponse (Maybe ObjectVersionId)
gorsVersionId = lens _gorsVersionId (\s a -> s {_gorsVersionId = a})

-- | Size of the body in bytes.
gorsContentLength :: Lens' GetObjectResponse (Maybe Integer)
gorsContentLength = lens _gorsContentLength (\s a -> s {_gorsContentLength = a})

-- | The Object Lock mode currently in place for this object.
gorsObjectLockMode :: Lens' GetObjectResponse (Maybe ObjectLockMode)
gorsObjectLockMode = lens _gorsObjectLockMode (\s a -> s {_gorsObjectLockMode = a})

-- | The date and time at which the object is no longer cacheable.
gorsExpires :: Lens' GetObjectResponse (Maybe UTCTime)
gorsExpires = lens _gorsExpires (\s a -> s {_gorsExpires = a}) . mapping _Time

-- | Provides information about object restoration operation and expiration time of the restored object copy.
gorsRestore :: Lens' GetObjectResponse (Maybe Text)
gorsRestore = lens _gorsRestore (\s a -> s {_gorsRestore = a})

-- | If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key-value pairs providing object expiration information. The value of the rule-id is URL encoded.
gorsExpiration :: Lens' GetObjectResponse (Maybe Text)
gorsExpiration = lens _gorsExpiration (\s a -> s {_gorsExpiration = a})

-- | Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
gorsDeleteMarker :: Lens' GetObjectResponse (Maybe Bool)
gorsDeleteMarker = lens _gorsDeleteMarker (\s a -> s {_gorsDeleteMarker = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
gorsSSECustomerAlgorithm :: Lens' GetObjectResponse (Maybe Text)
gorsSSECustomerAlgorithm = lens _gorsSSECustomerAlgorithm (\s a -> s {_gorsSSECustomerAlgorithm = a})

-- | The number of tags, if any, on the object.
gorsTagCount :: Lens' GetObjectResponse (Maybe Int)
gorsTagCount = lens _gorsTagCount (\s a -> s {_gorsTagCount = a})

-- | This is set to the number of metadata entries not returned in @x-amz-meta@ headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
gorsMissingMeta :: Lens' GetObjectResponse (Maybe Int)
gorsMissingMeta = lens _gorsMissingMeta (\s a -> s {_gorsMissingMeta = a})

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
gorsWebsiteRedirectLocation :: Lens' GetObjectResponse (Maybe Text)
gorsWebsiteRedirectLocation = lens _gorsWebsiteRedirectLocation (\s a -> s {_gorsWebsiteRedirectLocation = a})

-- | Indicates that a range of bytes was specified.
gorsAcceptRanges :: Lens' GetObjectResponse (Maybe Text)
gorsAcceptRanges = lens _gorsAcceptRanges (\s a -> s {_gorsAcceptRanges = a})

-- | Provides storage class information of the object. Amazon S3 returns this header for all objects except for S3 Standard storage class objects.
gorsStorageClass :: Lens' GetObjectResponse (Maybe StorageClass)
gorsStorageClass = lens _gorsStorageClass (\s a -> s {_gorsStorageClass = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
gorsSSECustomerKeyMD5 :: Lens' GetObjectResponse (Maybe Text)
gorsSSECustomerKeyMD5 = lens _gorsSSECustomerKeyMD5 (\s a -> s {_gorsSSECustomerKeyMD5 = a})

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
gorsSSEKMSKeyId :: Lens' GetObjectResponse (Maybe Text)
gorsSSEKMSKeyId = lens _gorsSSEKMSKeyId (\s a -> s {_gorsSSEKMSKeyId = a}) . mapping _Sensitive

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
gorsContentEncoding :: Lens' GetObjectResponse (Maybe Text)
gorsContentEncoding = lens _gorsContentEncoding (\s a -> s {_gorsContentEncoding = a})

-- | The date and time when this object's Object Lock will expire.
gorsObjectLockRetainUntilDate :: Lens' GetObjectResponse (Maybe UTCTime)
gorsObjectLockRetainUntilDate = lens _gorsObjectLockRetainUntilDate (\s a -> s {_gorsObjectLockRetainUntilDate = a}) . mapping _Time

-- | A map of metadata to store with the object in S3.
gorsMetadata :: Lens' GetObjectResponse (HashMap Text (Text))
gorsMetadata = lens _gorsMetadata (\s a -> s {_gorsMetadata = a}) . _Map

-- | Amazon S3 can return this if your request involves a bucket that is either a source or destination in a replication rule.
gorsReplicationStatus :: Lens' GetObjectResponse (Maybe ReplicationStatus)
gorsReplicationStatus = lens _gorsReplicationStatus (\s a -> s {_gorsReplicationStatus = a})

-- | Specifies caching behavior along the request/reply chain.
gorsCacheControl :: Lens' GetObjectResponse (Maybe Text)
gorsCacheControl = lens _gorsCacheControl (\s a -> s {_gorsCacheControl = a})

-- | The language the content is in.
gorsContentLanguage :: Lens' GetObjectResponse (Maybe Text)
gorsContentLanguage = lens _gorsContentLanguage (\s a -> s {_gorsContentLanguage = a})

-- | Last modified date of the object
gorsLastModified :: Lens' GetObjectResponse (Maybe UTCTime)
gorsLastModified = lens _gorsLastModified (\s a -> s {_gorsLastModified = a}) . mapping _Time

-- | Indicates whether this object has an active legal hold. This field is only returned if you have permission to view an object's legal hold status.
gorsObjectLockLegalHoldStatus :: Lens' GetObjectResponse (Maybe ObjectLockLegalHoldStatus)
gorsObjectLockLegalHoldStatus = lens _gorsObjectLockLegalHoldStatus (\s a -> s {_gorsObjectLockLegalHoldStatus = a})

-- | Specifies presentational information for the object.
gorsContentDisposition :: Lens' GetObjectResponse (Maybe Text)
gorsContentDisposition = lens _gorsContentDisposition (\s a -> s {_gorsContentDisposition = a})

-- | The portion of the object returned in the response.
gorsContentRange :: Lens' GetObjectResponse (Maybe Text)
gorsContentRange = lens _gorsContentRange (\s a -> s {_gorsContentRange = a})

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
gorsServerSideEncryption :: Lens' GetObjectResponse (Maybe ServerSideEncryption)
gorsServerSideEncryption = lens _gorsServerSideEncryption (\s a -> s {_gorsServerSideEncryption = a})

-- | A standard MIME type describing the format of the object data.
gorsContentType :: Lens' GetObjectResponse (Maybe Text)
gorsContentType = lens _gorsContentType (\s a -> s {_gorsContentType = a})

-- | -- | The response status code.
gorsResponseStatus :: Lens' GetObjectResponse Int
gorsResponseStatus = lens _gorsResponseStatus (\s a -> s {_gorsResponseStatus = a})

-- | Object data.
gorsBody :: Lens' GetObjectResponse RsBody
gorsBody = lens _gorsBody (\s a -> s {_gorsBody = a})
