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
-- Module      : Network.AWS.S3.CopyObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a copy of an object that is already stored in Amazon S3.
--
--
-- All copy requests must be authenticated. Additionally, you must have /read/ access to the source object and /write/ access to the destination bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication> . Both the Region that you want to copy the object from and the Region that you want to copy the object to must be enabled for your account.
--
-- A copy request might return an error when Amazon S3 receives the copy request or while Amazon S3 is copying the files. If the error occurs before the copy operation starts, you receive a standard Amazon S3 error. If the error occurs during the copy operation, the error response is embedded in the @200 OK@ response. This means that a @200 OK@ response can contain either a success or an error. Design your application to parse the contents of the response and handle it appropriately.
--
-- If the copy is successful, you receive a response with information about the copied object.
--
-- The copy request charge is based on the storage class and Region that you specify for the destination object. For pricing information, see <https://aws.amazon.com/s3/pricing/ Amazon S3 pricing> .
--
-- /Important:/ Amazon S3 transfer acceleration does not support cross-Region copies. If you request a cross-Region copy using a transfer acceleration endpoint, you get a 400 @Bad Request@ error. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Transfer Acceleration> .
--
-- __Metadata__
--
-- When copying an object, you can preserve all metadata (default) or specify new metadata. However, the ACL is not preserved and is set to private for the user making the request. To override the default ACL setting, specify a new ACL when generating a copy request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3_ACLs_UsingACLs.html Using ACLs> .
--
-- To specify whether you want the object metadata copied from the source object or replaced with metadata provided in the request, you can optionally add the @x-amz-metadata-directive@ header. When you grant permissions, you can use the @s3:x-amz-metadata-directive@ condition key to enforce certain metadata behavior when objects are uploaded. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/amazon-s3-policy-keys.html Specifying Conditions in a Policy> in the /Amazon S3 Developer Guide/ . For a complete list of Amazon S3-specific condition keys, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/list_amazons3.html Actions, Resources, and Condition Keys for Amazon S3> .
--
-- __@x-amz-copy-source-if@ Headers__
--
-- To only copy an object under certain conditions, such as whether the @Etag@ matches or whether the object was modified before or after a specified date, use the following request parameters:
--
--     * @x-amz-copy-source-if-match@
--
--     * @x-amz-copy-source-if-none-match@
--
--     * @x-amz-copy-source-if-unmodified-since@
--
--     * @x-amz-copy-source-if-modified-since@
--
--
--
-- If both the @x-amz-copy-source-if-match@ and @x-amz-copy-source-if-unmodified-since@ headers are present in the request and evaluate as follows, Amazon S3 returns @200 OK@ and copies the data:
--
--     * @x-amz-copy-source-if-match@ condition evaluates to true
--
--     * @x-amz-copy-source-if-unmodified-since@ condition evaluates to false
--
--
--
-- If both the @x-amz-copy-source-if-none-match@ and @x-amz-copy-source-if-modified-since@ headers are present in the request and evaluate as follows, Amazon S3 returns the @412 Precondition Failed@ response code:
--
--     * @x-amz-copy-source-if-none-match@ condition evaluates to false
--
--     * @x-amz-copy-source-if-modified-since@ condition evaluates to true
--
--
--
-- __Encryption__
--
-- The source object that you are copying can be encrypted or unencrypted. The source object can be encrypted with server-side encryption using AWS managed encryption keys (SSE-S3 or SSE-KMS) or by using a customer-provided encryption key. With server-side encryption, Amazon S3 encrypts your data as it writes it to disks in its data centers and decrypts the data when you access it.
--
-- You can optionally use the appropriate encryption-related headers to request server-side encryption for the target object. You have the option to provide your own encryption key or use SSE-S3 or SSE-KMS, regardless of the form of server-side encryption that was used to encrypt the source object. You can even request encryption if the source object was not encrypted. For more information about server-side encryption, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Using Server-Side Encryption> .
--
-- __Access Control List (ACL)-Specific Request Headers__
--
-- When copying an object, you can optionally use headers to grant ACL-based permissions. By default, all objects are private. Only the owner has full access control. When adding a new object, you can grant permissions to individual AWS accounts or to predefined groups defined by Amazon S3. These permissions are then added to the ACL on the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-using-rest-api.html Managing ACLs Using the REST API> .
--
-- __Storage Class Options__
--
-- You can use the @CopyObject@ operation to change the storage class of an object that is already stored in Amazon S3 using the @StorageClass@ parameter. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
--
-- __Versioning__
--
-- By default, @x-amz-copy-source@ identifies the current version of an object to copy. If the current version is a delete marker, Amazon S3 behaves as if the object was deleted. To copy a different version, use the @versionId@ subresource.
--
-- If you enable versioning on the target bucket, Amazon S3 generates a unique version ID for the object being copied. This version ID is different from the version ID of the source object. Amazon S3 returns the version ID of the copied object in the @x-amz-version-id@ response header in the response.
--
-- If you do not enable versioning or suspend it on the target bucket, the version ID that Amazon S3 generates is always null.
--
-- If the source object's storage class is GLACIER, you must restore a copy of this object before you can use it as a source object for the copy operation. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_RestoreObject.html RestoreObject> .
--
-- The following operations are related to @CopyObject@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/CopyingObjectsExamples.html Copying Objects> .
module Network.AWS.S3.CopyObject
  ( -- * Creating a Request
    copyObject,
    CopyObject,

    -- * Request Lenses
    coCopySourceIfModifiedSince,
    coCopySourceIfUnmodifiedSince,
    coCopySourceSSECustomerKeyMD5,
    coTaggingDirective,
    coMetadataDirective,
    coObjectLockMode,
    coExpires,
    coGrantReadACP,
    coCopySourceIfNoneMatch,
    coSSECustomerAlgorithm,
    coSSECustomerKey,
    coRequestPayer,
    coGrantWriteACP,
    coCopySourceIfMatch,
    coWebsiteRedirectLocation,
    coGrantRead,
    coExpectedSourceBucketOwner,
    coStorageClass,
    coSSECustomerKeyMD5,
    coSSEKMSKeyId,
    coGrantFullControl,
    coContentEncoding,
    coTagging,
    coObjectLockRetainUntilDate,
    coMetadata,
    coSSEKMSEncryptionContext,
    coCacheControl,
    coContentLanguage,
    coCopySourceSSECustomerKey,
    coObjectLockLegalHoldStatus,
    coCopySourceSSECustomerAlgorithm,
    coACL,
    coContentDisposition,
    coExpectedBucketOwner,
    coServerSideEncryption,
    coContentType,
    coBucket,
    coCopySource,
    coKey,

    -- * Destructuring the Response
    copyObjectResponse,
    CopyObjectResponse,

    -- * Response Lenses
    corsRequestCharged,
    corsVersionId,
    corsExpiration,
    corsSSECustomerAlgorithm,
    corsCopySourceVersionId,
    corsSSECustomerKeyMD5,
    corsSSEKMSKeyId,
    corsSSEKMSEncryptionContext,
    corsServerSideEncryption,
    corsCopyObjectResult,
    corsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'copyObject' smart constructor.
data CopyObject = CopyObject'
  { _coCopySourceIfModifiedSince ::
      !(Maybe ISO8601),
    _coCopySourceIfUnmodifiedSince :: !(Maybe ISO8601),
    _coCopySourceSSECustomerKeyMD5 :: !(Maybe Text),
    _coTaggingDirective :: !(Maybe TaggingDirective),
    _coMetadataDirective :: !(Maybe MetadataDirective),
    _coObjectLockMode :: !(Maybe ObjectLockMode),
    _coExpires :: !(Maybe ISO8601),
    _coGrantReadACP :: !(Maybe Text),
    _coCopySourceIfNoneMatch :: !(Maybe Text),
    _coSSECustomerAlgorithm :: !(Maybe Text),
    _coSSECustomerKey :: !(Maybe (Sensitive Text)),
    _coRequestPayer :: !(Maybe RequestPayer),
    _coGrantWriteACP :: !(Maybe Text),
    _coCopySourceIfMatch :: !(Maybe Text),
    _coWebsiteRedirectLocation :: !(Maybe Text),
    _coGrantRead :: !(Maybe Text),
    _coExpectedSourceBucketOwner :: !(Maybe Text),
    _coStorageClass :: !(Maybe StorageClass),
    _coSSECustomerKeyMD5 :: !(Maybe Text),
    _coSSEKMSKeyId :: !(Maybe (Sensitive Text)),
    _coGrantFullControl :: !(Maybe Text),
    _coContentEncoding :: !(Maybe Text),
    _coTagging :: !(Maybe Text),
    _coObjectLockRetainUntilDate :: !(Maybe ISO8601),
    _coMetadata :: !(Map Text (Text)),
    _coSSEKMSEncryptionContext :: !(Maybe (Sensitive Text)),
    _coCacheControl :: !(Maybe Text),
    _coContentLanguage :: !(Maybe Text),
    _coCopySourceSSECustomerKey :: !(Maybe (Sensitive Text)),
    _coObjectLockLegalHoldStatus :: !(Maybe ObjectLockLegalHoldStatus),
    _coCopySourceSSECustomerAlgorithm :: !(Maybe Text),
    _coACL :: !(Maybe ObjectCannedACL),
    _coContentDisposition :: !(Maybe Text),
    _coExpectedBucketOwner :: !(Maybe Text),
    _coServerSideEncryption :: !(Maybe ServerSideEncryption),
    _coContentType :: !(Maybe Text),
    _coBucket :: !BucketName,
    _coCopySource :: !Text,
    _coKey :: !ObjectKey
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopyObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCopySourceIfModifiedSince' - Copies the object if it has been modified since the specified time.
--
-- * 'coCopySourceIfUnmodifiedSince' - Copies the object if it hasn't been modified since the specified time.
--
-- * 'coCopySourceSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- * 'coTaggingDirective' - Specifies whether the object tag-set are copied from the source object or replaced with tag-set provided in the request.
--
-- * 'coMetadataDirective' - Specifies whether the metadata is copied from the source object or replaced with metadata provided in the request.
--
-- * 'coObjectLockMode' - The Object Lock mode that you want to apply to the copied object.
--
-- * 'coExpires' - The date and time at which the object is no longer cacheable.
--
-- * 'coGrantReadACP' - Allows grantee to read the object ACL. This action is not supported by Amazon S3 on Outposts.
--
-- * 'coCopySourceIfNoneMatch' - Copies the object if its entity tag (ETag) is different than the specified ETag.
--
-- * 'coSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- * 'coSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- * 'coRequestPayer' - Undocumented member.
--
-- * 'coGrantWriteACP' - Allows grantee to write the ACL for the applicable object. This action is not supported by Amazon S3 on Outposts.
--
-- * 'coCopySourceIfMatch' - Copies the object if its entity tag (ETag) matches the specified tag.
--
-- * 'coWebsiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- * 'coGrantRead' - Allows grantee to read the object data and its metadata. This action is not supported by Amazon S3 on Outposts.
--
-- * 'coExpectedSourceBucketOwner' - The account id of the expected source bucket owner. If the source bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'coStorageClass' - By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
--
-- * 'coSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- * 'coSSEKMSKeyId' - Specifies the AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. For information about configuring using any of the officially supported AWS SDKs and AWS CLI, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication> in the /Amazon S3 Developer Guide/ .
--
-- * 'coGrantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object. This action is not supported by Amazon S3 on Outposts.
--
-- * 'coContentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- * 'coTagging' - The tag-set for the object destination object this value must be used in conjunction with the @TaggingDirective@ . The tag-set must be encoded as URL Query parameters.
--
-- * 'coObjectLockRetainUntilDate' - The date and time when you want the copied object's Object Lock to expire.
--
-- * 'coMetadata' - A map of metadata to store with the object in S3.
--
-- * 'coSSEKMSEncryptionContext' - Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- * 'coCacheControl' - Specifies caching behavior along the request/reply chain.
--
-- * 'coContentLanguage' - The language the content is in.
--
-- * 'coCopySourceSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
--
-- * 'coObjectLockLegalHoldStatus' - Specifies whether you want to apply a Legal Hold to the copied object.
--
-- * 'coCopySourceSSECustomerAlgorithm' - Specifies the algorithm to use when decrypting the source object (for example, AES256).
--
-- * 'coACL' - The canned ACL to apply to the object. This action is not supported by Amazon S3 on Outposts.
--
-- * 'coContentDisposition' - Specifies presentational information for the object.
--
-- * 'coExpectedBucketOwner' - The account id of the expected destination bucket owner. If the destination bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'coServerSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- * 'coContentType' - A standard MIME type describing the format of the object data.
--
-- * 'coBucket' - The name of the destination bucket. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'coCopySource' - Specifies the source object for the copy operation. You specify the value in one of two formats, depending on whether you want to access the source object through an <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point> :     * For objects not accessed through an access point, specify the name of the source bucket and the key of the source object, separated by a slash (/). For example, to copy the object @reports/january.pdf@ from the bucket @awsexamplebucket@ , use @awsexamplebucket/reports/january.pdf@ . The value must be URL encoded.     * For objects accessed through access points, specify the Amazon Resource Name (ARN) of the object as accessed through the access point, in the format @arn:aws:s3:<Region>:<account-id>:accesspoint/<access-point-name>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through access point @my-access-point@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3:us-west-2:123456789012:accesspoint/my-access-point/object/reports/january.pdf@ . The value must be URL encoded. Alternatively, for objects accessed through Amazon S3 on Outposts, specify the ARN of the object as accessed in the format @arn:aws:s3-outposts:<Region>:<account-id>:outpost/<outpost-id>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through outpost @my-outpost@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3-outposts:us-west-2:123456789012:outpost/my-outpost/object/reports/january.pdf@ . The value must be URL encoded.  To copy a specific version of an object, append @?versionId=<version-id>@ to the value (for example, @awsexamplebucket/reports/january.pdf?versionId=QUpfdndhfd8438MNFDN93jdnJFkdmqnh893@ ). If you don't specify a version ID, Amazon S3 copies the latest version of the source object.
--
-- * 'coKey' - The key of the destination object.
copyObject ::
  -- | 'coBucket'
  BucketName ->
  -- | 'coCopySource'
  Text ->
  -- | 'coKey'
  ObjectKey ->
  CopyObject
copyObject pBucket_ pCopySource_ pKey_ =
  CopyObject'
    { _coCopySourceIfModifiedSince = Nothing,
      _coCopySourceIfUnmodifiedSince = Nothing,
      _coCopySourceSSECustomerKeyMD5 = Nothing,
      _coTaggingDirective = Nothing,
      _coMetadataDirective = Nothing,
      _coObjectLockMode = Nothing,
      _coExpires = Nothing,
      _coGrantReadACP = Nothing,
      _coCopySourceIfNoneMatch = Nothing,
      _coSSECustomerAlgorithm = Nothing,
      _coSSECustomerKey = Nothing,
      _coRequestPayer = Nothing,
      _coGrantWriteACP = Nothing,
      _coCopySourceIfMatch = Nothing,
      _coWebsiteRedirectLocation = Nothing,
      _coGrantRead = Nothing,
      _coExpectedSourceBucketOwner = Nothing,
      _coStorageClass = Nothing,
      _coSSECustomerKeyMD5 = Nothing,
      _coSSEKMSKeyId = Nothing,
      _coGrantFullControl = Nothing,
      _coContentEncoding = Nothing,
      _coTagging = Nothing,
      _coObjectLockRetainUntilDate = Nothing,
      _coMetadata = mempty,
      _coSSEKMSEncryptionContext = Nothing,
      _coCacheControl = Nothing,
      _coContentLanguage = Nothing,
      _coCopySourceSSECustomerKey = Nothing,
      _coObjectLockLegalHoldStatus = Nothing,
      _coCopySourceSSECustomerAlgorithm = Nothing,
      _coACL = Nothing,
      _coContentDisposition = Nothing,
      _coExpectedBucketOwner = Nothing,
      _coServerSideEncryption = Nothing,
      _coContentType = Nothing,
      _coBucket = pBucket_,
      _coCopySource = pCopySource_,
      _coKey = pKey_
    }

-- | Copies the object if it has been modified since the specified time.
coCopySourceIfModifiedSince :: Lens' CopyObject (Maybe UTCTime)
coCopySourceIfModifiedSince = lens _coCopySourceIfModifiedSince (\s a -> s {_coCopySourceIfModifiedSince = a}) . mapping _Time

-- | Copies the object if it hasn't been modified since the specified time.
coCopySourceIfUnmodifiedSince :: Lens' CopyObject (Maybe UTCTime)
coCopySourceIfUnmodifiedSince = lens _coCopySourceIfUnmodifiedSince (\s a -> s {_coCopySourceIfUnmodifiedSince = a}) . mapping _Time

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
coCopySourceSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
coCopySourceSSECustomerKeyMD5 = lens _coCopySourceSSECustomerKeyMD5 (\s a -> s {_coCopySourceSSECustomerKeyMD5 = a})

-- | Specifies whether the object tag-set are copied from the source object or replaced with tag-set provided in the request.
coTaggingDirective :: Lens' CopyObject (Maybe TaggingDirective)
coTaggingDirective = lens _coTaggingDirective (\s a -> s {_coTaggingDirective = a})

-- | Specifies whether the metadata is copied from the source object or replaced with metadata provided in the request.
coMetadataDirective :: Lens' CopyObject (Maybe MetadataDirective)
coMetadataDirective = lens _coMetadataDirective (\s a -> s {_coMetadataDirective = a})

-- | The Object Lock mode that you want to apply to the copied object.
coObjectLockMode :: Lens' CopyObject (Maybe ObjectLockMode)
coObjectLockMode = lens _coObjectLockMode (\s a -> s {_coObjectLockMode = a})

-- | The date and time at which the object is no longer cacheable.
coExpires :: Lens' CopyObject (Maybe UTCTime)
coExpires = lens _coExpires (\s a -> s {_coExpires = a}) . mapping _Time

-- | Allows grantee to read the object ACL. This action is not supported by Amazon S3 on Outposts.
coGrantReadACP :: Lens' CopyObject (Maybe Text)
coGrantReadACP = lens _coGrantReadACP (\s a -> s {_coGrantReadACP = a})

-- | Copies the object if its entity tag (ETag) is different than the specified ETag.
coCopySourceIfNoneMatch :: Lens' CopyObject (Maybe Text)
coCopySourceIfNoneMatch = lens _coCopySourceIfNoneMatch (\s a -> s {_coCopySourceIfNoneMatch = a})

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
coSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
coSSECustomerAlgorithm = lens _coSSECustomerAlgorithm (\s a -> s {_coSSECustomerAlgorithm = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
coSSECustomerKey :: Lens' CopyObject (Maybe Text)
coSSECustomerKey = lens _coSSECustomerKey (\s a -> s {_coSSECustomerKey = a}) . mapping _Sensitive

-- | Undocumented member.
coRequestPayer :: Lens' CopyObject (Maybe RequestPayer)
coRequestPayer = lens _coRequestPayer (\s a -> s {_coRequestPayer = a})

-- | Allows grantee to write the ACL for the applicable object. This action is not supported by Amazon S3 on Outposts.
coGrantWriteACP :: Lens' CopyObject (Maybe Text)
coGrantWriteACP = lens _coGrantWriteACP (\s a -> s {_coGrantWriteACP = a})

-- | Copies the object if its entity tag (ETag) matches the specified tag.
coCopySourceIfMatch :: Lens' CopyObject (Maybe Text)
coCopySourceIfMatch = lens _coCopySourceIfMatch (\s a -> s {_coCopySourceIfMatch = a})

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
coWebsiteRedirectLocation :: Lens' CopyObject (Maybe Text)
coWebsiteRedirectLocation = lens _coWebsiteRedirectLocation (\s a -> s {_coWebsiteRedirectLocation = a})

-- | Allows grantee to read the object data and its metadata. This action is not supported by Amazon S3 on Outposts.
coGrantRead :: Lens' CopyObject (Maybe Text)
coGrantRead = lens _coGrantRead (\s a -> s {_coGrantRead = a})

-- | The account id of the expected source bucket owner. If the source bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
coExpectedSourceBucketOwner :: Lens' CopyObject (Maybe Text)
coExpectedSourceBucketOwner = lens _coExpectedSourceBucketOwner (\s a -> s {_coExpectedSourceBucketOwner = a})

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
coStorageClass :: Lens' CopyObject (Maybe StorageClass)
coStorageClass = lens _coStorageClass (\s a -> s {_coStorageClass = a})

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
coSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
coSSECustomerKeyMD5 = lens _coSSECustomerKeyMD5 (\s a -> s {_coSSECustomerKeyMD5 = a})

-- | Specifies the AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. For information about configuring using any of the officially supported AWS SDKs and AWS CLI, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version Specifying the Signature Version in Request Authentication> in the /Amazon S3 Developer Guide/ .
coSSEKMSKeyId :: Lens' CopyObject (Maybe Text)
coSSEKMSKeyId = lens _coSSEKMSKeyId (\s a -> s {_coSSEKMSKeyId = a}) . mapping _Sensitive

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object. This action is not supported by Amazon S3 on Outposts.
coGrantFullControl :: Lens' CopyObject (Maybe Text)
coGrantFullControl = lens _coGrantFullControl (\s a -> s {_coGrantFullControl = a})

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
coContentEncoding :: Lens' CopyObject (Maybe Text)
coContentEncoding = lens _coContentEncoding (\s a -> s {_coContentEncoding = a})

-- | The tag-set for the object destination object this value must be used in conjunction with the @TaggingDirective@ . The tag-set must be encoded as URL Query parameters.
coTagging :: Lens' CopyObject (Maybe Text)
coTagging = lens _coTagging (\s a -> s {_coTagging = a})

-- | The date and time when you want the copied object's Object Lock to expire.
coObjectLockRetainUntilDate :: Lens' CopyObject (Maybe UTCTime)
coObjectLockRetainUntilDate = lens _coObjectLockRetainUntilDate (\s a -> s {_coObjectLockRetainUntilDate = a}) . mapping _Time

-- | A map of metadata to store with the object in S3.
coMetadata :: Lens' CopyObject (HashMap Text (Text))
coMetadata = lens _coMetadata (\s a -> s {_coMetadata = a}) . _Map

-- | Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
coSSEKMSEncryptionContext :: Lens' CopyObject (Maybe Text)
coSSEKMSEncryptionContext = lens _coSSEKMSEncryptionContext (\s a -> s {_coSSEKMSEncryptionContext = a}) . mapping _Sensitive

-- | Specifies caching behavior along the request/reply chain.
coCacheControl :: Lens' CopyObject (Maybe Text)
coCacheControl = lens _coCacheControl (\s a -> s {_coCacheControl = a})

-- | The language the content is in.
coContentLanguage :: Lens' CopyObject (Maybe Text)
coContentLanguage = lens _coContentLanguage (\s a -> s {_coContentLanguage = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use to decrypt the source object. The encryption key provided in this header must be one that was used when the source object was created.
coCopySourceSSECustomerKey :: Lens' CopyObject (Maybe Text)
coCopySourceSSECustomerKey = lens _coCopySourceSSECustomerKey (\s a -> s {_coCopySourceSSECustomerKey = a}) . mapping _Sensitive

-- | Specifies whether you want to apply a Legal Hold to the copied object.
coObjectLockLegalHoldStatus :: Lens' CopyObject (Maybe ObjectLockLegalHoldStatus)
coObjectLockLegalHoldStatus = lens _coObjectLockLegalHoldStatus (\s a -> s {_coObjectLockLegalHoldStatus = a})

-- | Specifies the algorithm to use when decrypting the source object (for example, AES256).
coCopySourceSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
coCopySourceSSECustomerAlgorithm = lens _coCopySourceSSECustomerAlgorithm (\s a -> s {_coCopySourceSSECustomerAlgorithm = a})

-- | The canned ACL to apply to the object. This action is not supported by Amazon S3 on Outposts.
coACL :: Lens' CopyObject (Maybe ObjectCannedACL)
coACL = lens _coACL (\s a -> s {_coACL = a})

-- | Specifies presentational information for the object.
coContentDisposition :: Lens' CopyObject (Maybe Text)
coContentDisposition = lens _coContentDisposition (\s a -> s {_coContentDisposition = a})

-- | The account id of the expected destination bucket owner. If the destination bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
coExpectedBucketOwner :: Lens' CopyObject (Maybe Text)
coExpectedBucketOwner = lens _coExpectedBucketOwner (\s a -> s {_coExpectedBucketOwner = a})

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
coServerSideEncryption :: Lens' CopyObject (Maybe ServerSideEncryption)
coServerSideEncryption = lens _coServerSideEncryption (\s a -> s {_coServerSideEncryption = a})

-- | A standard MIME type describing the format of the object data.
coContentType :: Lens' CopyObject (Maybe Text)
coContentType = lens _coContentType (\s a -> s {_coContentType = a})

-- | The name of the destination bucket. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
coBucket :: Lens' CopyObject BucketName
coBucket = lens _coBucket (\s a -> s {_coBucket = a})

-- | Specifies the source object for the copy operation. You specify the value in one of two formats, depending on whether you want to access the source object through an <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-points.html access point> :     * For objects not accessed through an access point, specify the name of the source bucket and the key of the source object, separated by a slash (/). For example, to copy the object @reports/january.pdf@ from the bucket @awsexamplebucket@ , use @awsexamplebucket/reports/january.pdf@ . The value must be URL encoded.     * For objects accessed through access points, specify the Amazon Resource Name (ARN) of the object as accessed through the access point, in the format @arn:aws:s3:<Region>:<account-id>:accesspoint/<access-point-name>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through access point @my-access-point@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3:us-west-2:123456789012:accesspoint/my-access-point/object/reports/january.pdf@ . The value must be URL encoded. Alternatively, for objects accessed through Amazon S3 on Outposts, specify the ARN of the object as accessed in the format @arn:aws:s3-outposts:<Region>:<account-id>:outpost/<outpost-id>/object/<key>@ . For example, to copy the object @reports/january.pdf@ through outpost @my-outpost@ owned by account @123456789012@ in Region @us-west-2@ , use the URL encoding of @arn:aws:s3-outposts:us-west-2:123456789012:outpost/my-outpost/object/reports/january.pdf@ . The value must be URL encoded.  To copy a specific version of an object, append @?versionId=<version-id>@ to the value (for example, @awsexamplebucket/reports/january.pdf?versionId=QUpfdndhfd8438MNFDN93jdnJFkdmqnh893@ ). If you don't specify a version ID, Amazon S3 copies the latest version of the source object.
coCopySource :: Lens' CopyObject Text
coCopySource = lens _coCopySource (\s a -> s {_coCopySource = a})

-- | The key of the destination object.
coKey :: Lens' CopyObject ObjectKey
coKey = lens _coKey (\s a -> s {_coKey = a})

instance AWSRequest CopyObject where
  type Rs CopyObject = CopyObjectResponse
  request = put s3
  response =
    receiveXML
      ( \s h x ->
          CopyObjectResponse'
            <$> (h .#? "x-amz-request-charged")
            <*> (h .#? "x-amz-version-id")
            <*> (h .#? "x-amz-expiration")
            <*> (h .#? "x-amz-server-side-encryption-customer-algorithm")
            <*> (h .#? "x-amz-copy-source-version-id")
            <*> (h .#? "x-amz-server-side-encryption-customer-key-MD5")
            <*> (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
            <*> (h .#? "x-amz-server-side-encryption-context")
            <*> (h .#? "x-amz-server-side-encryption")
            <*> (parseXML x)
            <*> (pure (fromEnum s))
      )

instance Hashable CopyObject

instance NFData CopyObject

instance ToHeaders CopyObject where
  toHeaders CopyObject' {..} =
    mconcat
      [ "x-amz-copy-source-if-modified-since"
          =# _coCopySourceIfModifiedSince,
        "x-amz-copy-source-if-unmodified-since"
          =# _coCopySourceIfUnmodifiedSince,
        "x-amz-copy-source-server-side-encryption-customer-key-MD5"
          =# _coCopySourceSSECustomerKeyMD5,
        "x-amz-tagging-directive" =# _coTaggingDirective,
        "x-amz-metadata-directive" =# _coMetadataDirective,
        "x-amz-object-lock-mode" =# _coObjectLockMode,
        "Expires" =# _coExpires,
        "x-amz-grant-read-acp" =# _coGrantReadACP,
        "x-amz-copy-source-if-none-match" =# _coCopySourceIfNoneMatch,
        "x-amz-server-side-encryption-customer-algorithm"
          =# _coSSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" =# _coSSECustomerKey,
        "x-amz-request-payer" =# _coRequestPayer,
        "x-amz-grant-write-acp" =# _coGrantWriteACP,
        "x-amz-copy-source-if-match" =# _coCopySourceIfMatch,
        "x-amz-website-redirect-location" =# _coWebsiteRedirectLocation,
        "x-amz-grant-read" =# _coGrantRead,
        "x-amz-source-expected-bucket-owner"
          =# _coExpectedSourceBucketOwner,
        "x-amz-storage-class" =# _coStorageClass,
        "x-amz-server-side-encryption-customer-key-MD5"
          =# _coSSECustomerKeyMD5,
        "x-amz-server-side-encryption-aws-kms-key-id" =# _coSSEKMSKeyId,
        "x-amz-grant-full-control" =# _coGrantFullControl,
        "Content-Encoding" =# _coContentEncoding,
        "x-amz-tagging" =# _coTagging,
        "x-amz-object-lock-retain-until-date"
          =# _coObjectLockRetainUntilDate,
        "x-amz-meta-" =# _coMetadata,
        "x-amz-server-side-encryption-context"
          =# _coSSEKMSEncryptionContext,
        "Cache-Control" =# _coCacheControl,
        "Content-Language" =# _coContentLanguage,
        "x-amz-copy-source-server-side-encryption-customer-key"
          =# _coCopySourceSSECustomerKey,
        "x-amz-object-lock-legal-hold" =# _coObjectLockLegalHoldStatus,
        "x-amz-copy-source-server-side-encryption-customer-algorithm"
          =# _coCopySourceSSECustomerAlgorithm,
        "x-amz-acl" =# _coACL,
        "Content-Disposition" =# _coContentDisposition,
        "x-amz-expected-bucket-owner" =# _coExpectedBucketOwner,
        "x-amz-server-side-encryption" =# _coServerSideEncryption,
        "Content-Type" =# _coContentType,
        "x-amz-copy-source" =# _coCopySource
      ]

instance ToPath CopyObject where
  toPath CopyObject' {..} =
    mconcat ["/", toBS _coBucket, "/", toBS _coKey]

instance ToQuery CopyObject where
  toQuery = const mempty

-- | /See:/ 'copyObjectResponse' smart constructor.
data CopyObjectResponse = CopyObjectResponse'
  { _corsRequestCharged ::
      !(Maybe RequestCharged),
    _corsVersionId :: !(Maybe ObjectVersionId),
    _corsExpiration :: !(Maybe Text),
    _corsSSECustomerAlgorithm :: !(Maybe Text),
    _corsCopySourceVersionId :: !(Maybe Text),
    _corsSSECustomerKeyMD5 :: !(Maybe Text),
    _corsSSEKMSKeyId :: !(Maybe (Sensitive Text)),
    _corsSSEKMSEncryptionContext ::
      !(Maybe (Sensitive Text)),
    _corsServerSideEncryption ::
      !(Maybe ServerSideEncryption),
    _corsCopyObjectResult :: !(Maybe CopyObjectResult),
    _corsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopyObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'corsRequestCharged' - Undocumented member.
--
-- * 'corsVersionId' - Version ID of the newly created copy.
--
-- * 'corsExpiration' - If the object expiration is configured, the response includes this header.
--
-- * 'corsSSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- * 'corsCopySourceVersionId' - Version of the copied object in the destination bucket.
--
-- * 'corsSSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- * 'corsSSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- * 'corsSSEKMSEncryptionContext' - If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- * 'corsServerSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- * 'corsCopyObjectResult' - Container for all response elements.
--
-- * 'corsResponseStatus' - -- | The response status code.
copyObjectResponse ::
  -- | 'corsResponseStatus'
  Int ->
  CopyObjectResponse
copyObjectResponse pResponseStatus_ =
  CopyObjectResponse'
    { _corsRequestCharged = Nothing,
      _corsVersionId = Nothing,
      _corsExpiration = Nothing,
      _corsSSECustomerAlgorithm = Nothing,
      _corsCopySourceVersionId = Nothing,
      _corsSSECustomerKeyMD5 = Nothing,
      _corsSSEKMSKeyId = Nothing,
      _corsSSEKMSEncryptionContext = Nothing,
      _corsServerSideEncryption = Nothing,
      _corsCopyObjectResult = Nothing,
      _corsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
corsRequestCharged :: Lens' CopyObjectResponse (Maybe RequestCharged)
corsRequestCharged = lens _corsRequestCharged (\s a -> s {_corsRequestCharged = a})

-- | Version ID of the newly created copy.
corsVersionId :: Lens' CopyObjectResponse (Maybe ObjectVersionId)
corsVersionId = lens _corsVersionId (\s a -> s {_corsVersionId = a})

-- | If the object expiration is configured, the response includes this header.
corsExpiration :: Lens' CopyObjectResponse (Maybe Text)
corsExpiration = lens _corsExpiration (\s a -> s {_corsExpiration = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
corsSSECustomerAlgorithm :: Lens' CopyObjectResponse (Maybe Text)
corsSSECustomerAlgorithm = lens _corsSSECustomerAlgorithm (\s a -> s {_corsSSECustomerAlgorithm = a})

-- | Version of the copied object in the destination bucket.
corsCopySourceVersionId :: Lens' CopyObjectResponse (Maybe Text)
corsCopySourceVersionId = lens _corsCopySourceVersionId (\s a -> s {_corsCopySourceVersionId = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
corsSSECustomerKeyMD5 :: Lens' CopyObjectResponse (Maybe Text)
corsSSECustomerKeyMD5 = lens _corsSSECustomerKeyMD5 (\s a -> s {_corsSSECustomerKeyMD5 = a})

-- | If present, specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
corsSSEKMSKeyId :: Lens' CopyObjectResponse (Maybe Text)
corsSSEKMSKeyId = lens _corsSSEKMSKeyId (\s a -> s {_corsSSEKMSKeyId = a}) . mapping _Sensitive

-- | If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
corsSSEKMSEncryptionContext :: Lens' CopyObjectResponse (Maybe Text)
corsSSEKMSEncryptionContext = lens _corsSSEKMSEncryptionContext (\s a -> s {_corsSSEKMSEncryptionContext = a}) . mapping _Sensitive

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
corsServerSideEncryption :: Lens' CopyObjectResponse (Maybe ServerSideEncryption)
corsServerSideEncryption = lens _corsServerSideEncryption (\s a -> s {_corsServerSideEncryption = a})

-- | Container for all response elements.
corsCopyObjectResult :: Lens' CopyObjectResponse (Maybe CopyObjectResult)
corsCopyObjectResult = lens _corsCopyObjectResult (\s a -> s {_corsCopyObjectResult = a})

-- | -- | The response status code.
corsResponseStatus :: Lens' CopyObjectResponse Int
corsResponseStatus = lens _corsResponseStatus (\s a -> s {_corsResponseStatus = a})

instance NFData CopyObjectResponse
