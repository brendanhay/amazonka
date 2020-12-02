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
-- Module      : Network.AWS.S3.PutObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an object to a bucket. You must have WRITE permissions on a bucket to add an object to it.
--
--
-- Amazon S3 never adds partial objects; if you receive a success response, Amazon S3 added the entire object to the bucket.
--
-- Amazon S3 is a distributed system. If it receives multiple write requests for the same object simultaneously, it overwrites all but the last object written. Amazon S3 does not provide object locking; if you need this, make sure to build it into your application layer or use versioning instead.
--
-- To ensure that data is not corrupted traversing the network, use the @Content-MD5@ header. When you use this header, Amazon S3 checks the object against the provided MD5 value and, if they do not match, returns an error. Additionally, you can calculate the MD5 while putting an object to Amazon S3 and compare the returned ETag to the calculated MD5 value.
--
-- __Server-side Encryption__
--
-- You can optionally request server-side encryption. With server-side encryption, Amazon S3 encrypts your data as it writes it to disks in its data centers and decrypts the data when you access it. You have the option to provide your own encryption key or use AWS managed encryption keys. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Using Server-Side Encryption> .
--
-- __Access Control List (ACL)-Specific Request Headers__
--
-- You can use headers to grant ACL- based permissions. By default, all objects are private. Only the owner has full access control. When adding a new object, you can grant permissions to individual AWS accounts or to predefined groups defined by Amazon S3. These permissions are then added to the ACL on the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-using-rest-api.html Managing ACLs Using the REST API> .
--
-- __Storage Class Options__
--
-- By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
--
-- __Versioning__
--
-- If you enable versioning for a bucket, Amazon S3 automatically generates a unique version ID for the object being stored. Amazon S3 returns this ID in the response. When you enable versioning for a bucket, if Amazon S3 receives multiple write requests for the same object simultaneously, it stores all of the objects.
--
-- For more information about versioning, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/AddingObjectstoVersioningEnabledBuckets.html Adding Objects to Versioning Enabled Buckets> . For information about returning the versioning state of a bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketVersioning.html GetBucketVersioning> .
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CopyObject.html CopyObject>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Network.AWS.S3.PutObject
  ( -- * Creating a Request
    putObject,
    PutObject,

    -- * Request Lenses
    poContentLength,
    poObjectLockMode,
    poExpires,
    poGrantReadACP,
    poSSECustomerAlgorithm,
    poSSECustomerKey,
    poRequestPayer,
    poGrantWriteACP,
    poWebsiteRedirectLocation,
    poGrantRead,
    poStorageClass,
    poSSECustomerKeyMD5,
    poSSEKMSKeyId,
    poGrantFullControl,
    poContentEncoding,
    poTagging,
    poContentMD5,
    poObjectLockRetainUntilDate,
    poMetadata,
    poSSEKMSEncryptionContext,
    poCacheControl,
    poContentLanguage,
    poObjectLockLegalHoldStatus,
    poACL,
    poContentDisposition,
    poExpectedBucketOwner,
    poServerSideEncryption,
    poContentType,
    poBucket,
    poKey,
    poBody,

    -- * Destructuring the Response
    putObjectResponse,
    PutObjectResponse,

    -- * Response Lenses
    porsRequestCharged,
    porsETag,
    porsVersionId,
    porsExpiration,
    porsSSECustomerAlgorithm,
    porsSSECustomerKeyMD5,
    porsSSEKMSKeyId,
    porsSSEKMSEncryptionContext,
    porsServerSideEncryption,
    porsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putObject' smart constructor.
data PutObject = PutObject'
  { _poContentLength :: !(Maybe Integer),
    _poObjectLockMode :: !(Maybe ObjectLockMode),
    _poExpires :: !(Maybe ISO8601),
    _poGrantReadACP :: !(Maybe Text),
    _poSSECustomerAlgorithm :: !(Maybe Text),
    _poSSECustomerKey :: !(Maybe (Sensitive Text)),
    _poRequestPayer :: !(Maybe RequestPayer),
    _poGrantWriteACP :: !(Maybe Text),
    _poWebsiteRedirectLocation :: !(Maybe Text),
    _poGrantRead :: !(Maybe Text),
    _poStorageClass :: !(Maybe StorageClass),
    _poSSECustomerKeyMD5 :: !(Maybe Text),
    _poSSEKMSKeyId :: !(Maybe (Sensitive Text)),
    _poGrantFullControl :: !(Maybe Text),
    _poContentEncoding :: !(Maybe Text),
    _poTagging :: !(Maybe Text),
    _poContentMD5 :: !(Maybe Text),
    _poObjectLockRetainUntilDate :: !(Maybe ISO8601),
    _poMetadata :: !(Map Text (Text)),
    _poSSEKMSEncryptionContext :: !(Maybe (Sensitive Text)),
    _poCacheControl :: !(Maybe Text),
    _poContentLanguage :: !(Maybe Text),
    _poObjectLockLegalHoldStatus :: !(Maybe ObjectLockLegalHoldStatus),
    _poACL :: !(Maybe ObjectCannedACL),
    _poContentDisposition :: !(Maybe Text),
    _poExpectedBucketOwner :: !(Maybe Text),
    _poServerSideEncryption :: !(Maybe ServerSideEncryption),
    _poContentType :: !(Maybe Text),
    _poBucket :: !BucketName,
    _poKey :: !ObjectKey,
    _poBody :: !RqBody
  }
  deriving (Show, Generic)

-- | Creates a value of 'PutObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poContentLength' - Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13> .
--
-- * 'poObjectLockMode' - The Object Lock mode that you want to apply to this object.
--
-- * 'poExpires' - The date and time at which the object is no longer cacheable. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21> .
--
-- * 'poGrantReadACP' - Allows grantee to read the object ACL. This action is not supported by Amazon S3 on Outposts.
--
-- * 'poSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (for example, AES256).
--
-- * 'poSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
--
-- * 'poRequestPayer' - Undocumented member.
--
-- * 'poGrantWriteACP' - Allows grantee to write the ACL for the applicable object. This action is not supported by Amazon S3 on Outposts.
--
-- * 'poWebsiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata. For information about object metadata, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html Object Key and Metadata> . In the following example, the request header sets the redirect to an object (anotherPage.html) in the same bucket: @x-amz-website-redirect-location: /anotherPage.html@  In the following example, the request header sets the object redirect to another website: @x-amz-website-redirect-location: http://www.example.com/@  For more information about website hosting in Amazon S3, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html How to Configure Website Page Redirects> .
--
-- * 'poGrantRead' - Allows grantee to read the object data and its metadata. This action is not supported by Amazon S3 on Outposts.
--
-- * 'poStorageClass' - By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
--
-- * 'poSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
--
-- * 'poSSEKMSKeyId' - If @x-amz-server-side-encryption@ is present and has the value of @aws:kms@ , this header specifies the ID of the AWS Key Management Service (AWS KMS) symmetrical customer managed customer master key (CMK) that was used for the object. If the value of @x-amz-server-side-encryption@ is @aws:kms@ , this header specifies the ID of the symmetric customer managed AWS KMS CMK that will be used for the object. If you specify @x-amz-server-side-encryption:aws:kms@ , but do not provide@x-amz-server-side-encryption-aws-kms-key-id@ , Amazon S3 uses the AWS managed CMK in AWS to protect the data.
--
-- * 'poGrantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object. This action is not supported by Amazon S3 on Outposts.
--
-- * 'poContentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11> .
--
-- * 'poTagging' - The tag-set for the object. The tag-set must be encoded as URL Query parameters. (For example, "Key1=Value1")
--
-- * 'poContentMD5' - The base64-encoded 128-bit MD5 digest of the message (without the headers) according to RFC 1864. This header can be used as a message integrity check to verify that the data is the same data that was originally sent. Although it is optional, we recommend using the Content-MD5 mechanism as an end-to-end integrity check. For more information about REST request authentication, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication> .
--
-- * 'poObjectLockRetainUntilDate' - The date and time when you want this object's Object Lock to expire.
--
-- * 'poMetadata' - A map of metadata to store with the object in S3.
--
-- * 'poSSEKMSEncryptionContext' - Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- * 'poCacheControl' - Can be used to specify caching behavior along the request/reply chain. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- * 'poContentLanguage' - The language the content is in.
--
-- * 'poObjectLockLegalHoldStatus' - Specifies whether a legal hold will be applied to this object. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
--
-- * 'poACL' - The canned ACL to apply to the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> . This action is not supported by Amazon S3 on Outposts.
--
-- * 'poContentDisposition' - Specifies presentational information for the object. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1 http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1> .
--
-- * 'poExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'poServerSideEncryption' - The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
--
-- * 'poContentType' - A standard MIME type describing the format of the contents. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17> .
--
-- * 'poBucket' - The bucket name to which the PUT operation was initiated.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'poKey' - Object key for which the PUT operation was initiated.
--
-- * 'poBody' - Object data.
putObject ::
  -- | 'poBucket'
  BucketName ->
  -- | 'poKey'
  ObjectKey ->
  -- | 'poBody'
  RqBody ->
  PutObject
putObject pBucket_ pKey_ pBody_ =
  PutObject'
    { _poContentLength = Nothing,
      _poObjectLockMode = Nothing,
      _poExpires = Nothing,
      _poGrantReadACP = Nothing,
      _poSSECustomerAlgorithm = Nothing,
      _poSSECustomerKey = Nothing,
      _poRequestPayer = Nothing,
      _poGrantWriteACP = Nothing,
      _poWebsiteRedirectLocation = Nothing,
      _poGrantRead = Nothing,
      _poStorageClass = Nothing,
      _poSSECustomerKeyMD5 = Nothing,
      _poSSEKMSKeyId = Nothing,
      _poGrantFullControl = Nothing,
      _poContentEncoding = Nothing,
      _poTagging = Nothing,
      _poContentMD5 = Nothing,
      _poObjectLockRetainUntilDate = Nothing,
      _poMetadata = mempty,
      _poSSEKMSEncryptionContext = Nothing,
      _poCacheControl = Nothing,
      _poContentLanguage = Nothing,
      _poObjectLockLegalHoldStatus = Nothing,
      _poACL = Nothing,
      _poContentDisposition = Nothing,
      _poExpectedBucketOwner = Nothing,
      _poServerSideEncryption = Nothing,
      _poContentType = Nothing,
      _poBucket = pBucket_,
      _poKey = pKey_,
      _poBody = pBody_
    }

-- | Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13> .
poContentLength :: Lens' PutObject (Maybe Integer)
poContentLength = lens _poContentLength (\s a -> s {_poContentLength = a})

-- | The Object Lock mode that you want to apply to this object.
poObjectLockMode :: Lens' PutObject (Maybe ObjectLockMode)
poObjectLockMode = lens _poObjectLockMode (\s a -> s {_poObjectLockMode = a})

-- | The date and time at which the object is no longer cacheable. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.21> .
poExpires :: Lens' PutObject (Maybe UTCTime)
poExpires = lens _poExpires (\s a -> s {_poExpires = a}) . mapping _Time

-- | Allows grantee to read the object ACL. This action is not supported by Amazon S3 on Outposts.
poGrantReadACP :: Lens' PutObject (Maybe Text)
poGrantReadACP = lens _poGrantReadACP (\s a -> s {_poGrantReadACP = a})

-- | Specifies the algorithm to use to when encrypting the object (for example, AES256).
poSSECustomerAlgorithm :: Lens' PutObject (Maybe Text)
poSSECustomerAlgorithm = lens _poSSECustomerAlgorithm (\s a -> s {_poSSECustomerAlgorithm = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in the @x-amz-server-side-encryption-customer-algorithm@ header.
poSSECustomerKey :: Lens' PutObject (Maybe Text)
poSSECustomerKey = lens _poSSECustomerKey (\s a -> s {_poSSECustomerKey = a}) . mapping _Sensitive

-- | Undocumented member.
poRequestPayer :: Lens' PutObject (Maybe RequestPayer)
poRequestPayer = lens _poRequestPayer (\s a -> s {_poRequestPayer = a})

-- | Allows grantee to write the ACL for the applicable object. This action is not supported by Amazon S3 on Outposts.
poGrantWriteACP :: Lens' PutObject (Maybe Text)
poGrantWriteACP = lens _poGrantWriteACP (\s a -> s {_poGrantWriteACP = a})

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata. For information about object metadata, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html Object Key and Metadata> . In the following example, the request header sets the redirect to an object (anotherPage.html) in the same bucket: @x-amz-website-redirect-location: /anotherPage.html@  In the following example, the request header sets the object redirect to another website: @x-amz-website-redirect-location: http://www.example.com/@  For more information about website hosting in Amazon S3, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html How to Configure Website Page Redirects> .
poWebsiteRedirectLocation :: Lens' PutObject (Maybe Text)
poWebsiteRedirectLocation = lens _poWebsiteRedirectLocation (\s a -> s {_poWebsiteRedirectLocation = a})

-- | Allows grantee to read the object data and its metadata. This action is not supported by Amazon S3 on Outposts.
poGrantRead :: Lens' PutObject (Maybe Text)
poGrantRead = lens _poGrantRead (\s a -> s {_poGrantRead = a})

-- | By default, Amazon S3 uses the STANDARD Storage Class to store newly created objects. The STANDARD storage class provides high durability and high availability. Depending on performance needs, you can specify a different Storage Class. Amazon S3 on Outposts only uses the OUTPOSTS Storage Class. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html Storage Classes> in the /Amazon S3 Service Developer Guide/ .
poStorageClass :: Lens' PutObject (Maybe StorageClass)
poStorageClass = lens _poStorageClass (\s a -> s {_poStorageClass = a})

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure that the encryption key was transmitted without error.
poSSECustomerKeyMD5 :: Lens' PutObject (Maybe Text)
poSSECustomerKeyMD5 = lens _poSSECustomerKeyMD5 (\s a -> s {_poSSECustomerKeyMD5 = a})

-- | If @x-amz-server-side-encryption@ is present and has the value of @aws:kms@ , this header specifies the ID of the AWS Key Management Service (AWS KMS) symmetrical customer managed customer master key (CMK) that was used for the object. If the value of @x-amz-server-side-encryption@ is @aws:kms@ , this header specifies the ID of the symmetric customer managed AWS KMS CMK that will be used for the object. If you specify @x-amz-server-side-encryption:aws:kms@ , but do not provide@x-amz-server-side-encryption-aws-kms-key-id@ , Amazon S3 uses the AWS managed CMK in AWS to protect the data.
poSSEKMSKeyId :: Lens' PutObject (Maybe Text)
poSSEKMSKeyId = lens _poSSEKMSKeyId (\s a -> s {_poSSEKMSKeyId = a}) . mapping _Sensitive

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object. This action is not supported by Amazon S3 on Outposts.
poGrantFullControl :: Lens' PutObject (Maybe Text)
poGrantFullControl = lens _poGrantFullControl (\s a -> s {_poGrantFullControl = a})

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11> .
poContentEncoding :: Lens' PutObject (Maybe Text)
poContentEncoding = lens _poContentEncoding (\s a -> s {_poContentEncoding = a})

-- | The tag-set for the object. The tag-set must be encoded as URL Query parameters. (For example, "Key1=Value1")
poTagging :: Lens' PutObject (Maybe Text)
poTagging = lens _poTagging (\s a -> s {_poTagging = a})

-- | The base64-encoded 128-bit MD5 digest of the message (without the headers) according to RFC 1864. This header can be used as a message integrity check to verify that the data is the same data that was originally sent. Although it is optional, we recommend using the Content-MD5 mechanism as an end-to-end integrity check. For more information about REST request authentication, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html REST Authentication> .
poContentMD5 :: Lens' PutObject (Maybe Text)
poContentMD5 = lens _poContentMD5 (\s a -> s {_poContentMD5 = a})

-- | The date and time when you want this object's Object Lock to expire.
poObjectLockRetainUntilDate :: Lens' PutObject (Maybe UTCTime)
poObjectLockRetainUntilDate = lens _poObjectLockRetainUntilDate (\s a -> s {_poObjectLockRetainUntilDate = a}) . mapping _Time

-- | A map of metadata to store with the object in S3.
poMetadata :: Lens' PutObject (HashMap Text (Text))
poMetadata = lens _poMetadata (\s a -> s {_poMetadata = a}) . _Map

-- | Specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
poSSEKMSEncryptionContext :: Lens' PutObject (Maybe Text)
poSSEKMSEncryptionContext = lens _poSSEKMSEncryptionContext (\s a -> s {_poSSEKMSEncryptionContext = a}) . mapping _Sensitive

-- | Can be used to specify caching behavior along the request/reply chain. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
poCacheControl :: Lens' PutObject (Maybe Text)
poCacheControl = lens _poCacheControl (\s a -> s {_poCacheControl = a})

-- | The language the content is in.
poContentLanguage :: Lens' PutObject (Maybe Text)
poContentLanguage = lens _poContentLanguage (\s a -> s {_poContentLanguage = a})

-- | Specifies whether a legal hold will be applied to this object. For more information about S3 Object Lock, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Object Lock> .
poObjectLockLegalHoldStatus :: Lens' PutObject (Maybe ObjectLockLegalHoldStatus)
poObjectLockLegalHoldStatus = lens _poObjectLockLegalHoldStatus (\s a -> s {_poObjectLockLegalHoldStatus = a})

-- | The canned ACL to apply to the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> . This action is not supported by Amazon S3 on Outposts.
poACL :: Lens' PutObject (Maybe ObjectCannedACL)
poACL = lens _poACL (\s a -> s {_poACL = a})

-- | Specifies presentational information for the object. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1 http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.5.1> .
poContentDisposition :: Lens' PutObject (Maybe Text)
poContentDisposition = lens _poContentDisposition (\s a -> s {_poContentDisposition = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
poExpectedBucketOwner :: Lens' PutObject (Maybe Text)
poExpectedBucketOwner = lens _poExpectedBucketOwner (\s a -> s {_poExpectedBucketOwner = a})

-- | The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
poServerSideEncryption :: Lens' PutObject (Maybe ServerSideEncryption)
poServerSideEncryption = lens _poServerSideEncryption (\s a -> s {_poServerSideEncryption = a})

-- | A standard MIME type describing the format of the contents. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17> .
poContentType :: Lens' PutObject (Maybe Text)
poContentType = lens _poContentType (\s a -> s {_poContentType = a})

-- | The bucket name to which the PUT operation was initiated.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ . When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
poBucket :: Lens' PutObject BucketName
poBucket = lens _poBucket (\s a -> s {_poBucket = a})

-- | Object key for which the PUT operation was initiated.
poKey :: Lens' PutObject ObjectKey
poKey = lens _poKey (\s a -> s {_poKey = a})

-- | Object data.
poBody :: Lens' PutObject RqBody
poBody = lens _poBody (\s a -> s {_poBody = a})

instance AWSRequest PutObject where
  type Rs PutObject = PutObjectResponse
  request = expectHeader . putBody s3
  response =
    receiveEmpty
      ( \s h x ->
          PutObjectResponse'
            <$> (h .#? "x-amz-request-charged")
            <*> (h .#? "ETag")
            <*> (h .#? "x-amz-version-id")
            <*> (h .#? "x-amz-expiration")
            <*> (h .#? "x-amz-server-side-encryption-customer-algorithm")
            <*> (h .#? "x-amz-server-side-encryption-customer-key-MD5")
            <*> (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
            <*> (h .#? "x-amz-server-side-encryption-context")
            <*> (h .#? "x-amz-server-side-encryption")
            <*> (pure (fromEnum s))
      )

instance ToBody PutObject where
  toBody = toBody . _poBody

instance ToHeaders PutObject where
  toHeaders PutObject' {..} =
    mconcat
      [ "Content-Length" =# _poContentLength,
        "x-amz-object-lock-mode" =# _poObjectLockMode,
        "Expires" =# _poExpires,
        "x-amz-grant-read-acp" =# _poGrantReadACP,
        "x-amz-server-side-encryption-customer-algorithm"
          =# _poSSECustomerAlgorithm,
        "x-amz-server-side-encryption-customer-key" =# _poSSECustomerKey,
        "x-amz-request-payer" =# _poRequestPayer,
        "x-amz-grant-write-acp" =# _poGrantWriteACP,
        "x-amz-website-redirect-location" =# _poWebsiteRedirectLocation,
        "x-amz-grant-read" =# _poGrantRead,
        "x-amz-storage-class" =# _poStorageClass,
        "x-amz-server-side-encryption-customer-key-MD5"
          =# _poSSECustomerKeyMD5,
        "x-amz-server-side-encryption-aws-kms-key-id" =# _poSSEKMSKeyId,
        "x-amz-grant-full-control" =# _poGrantFullControl,
        "Content-Encoding" =# _poContentEncoding,
        "x-amz-tagging" =# _poTagging,
        "Content-MD5" =# _poContentMD5,
        "x-amz-object-lock-retain-until-date"
          =# _poObjectLockRetainUntilDate,
        "x-amz-meta-" =# _poMetadata,
        "x-amz-server-side-encryption-context"
          =# _poSSEKMSEncryptionContext,
        "Cache-Control" =# _poCacheControl,
        "Content-Language" =# _poContentLanguage,
        "x-amz-object-lock-legal-hold" =# _poObjectLockLegalHoldStatus,
        "x-amz-acl" =# _poACL,
        "Content-Disposition" =# _poContentDisposition,
        "x-amz-expected-bucket-owner" =# _poExpectedBucketOwner,
        "x-amz-server-side-encryption" =# _poServerSideEncryption,
        "Content-Type" =# _poContentType
      ]

instance ToPath PutObject where
  toPath PutObject' {..} =
    mconcat ["/", toBS _poBucket, "/", toBS _poKey]

instance ToQuery PutObject where
  toQuery = const mempty

-- | /See:/ 'putObjectResponse' smart constructor.
data PutObjectResponse = PutObjectResponse'
  { _porsRequestCharged ::
      !(Maybe RequestCharged),
    _porsETag :: !(Maybe ETag),
    _porsVersionId :: !(Maybe ObjectVersionId),
    _porsExpiration :: !(Maybe Text),
    _porsSSECustomerAlgorithm :: !(Maybe Text),
    _porsSSECustomerKeyMD5 :: !(Maybe Text),
    _porsSSEKMSKeyId :: !(Maybe (Sensitive Text)),
    _porsSSEKMSEncryptionContext ::
      !(Maybe (Sensitive Text)),
    _porsServerSideEncryption ::
      !(Maybe ServerSideEncryption),
    _porsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'porsRequestCharged' - Undocumented member.
--
-- * 'porsETag' - Entity tag for the uploaded object.
--
-- * 'porsVersionId' - Version of the object.
--
-- * 'porsExpiration' - If the expiration is configured for the object (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration> ), the response includes this header. It includes the expiry-date and rule-id key-value pairs that provide information about object expiration. The value of the rule-id is URL encoded.
--
-- * 'porsSSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- * 'porsSSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
--
-- * 'porsSSEKMSKeyId' - If @x-amz-server-side-encryption@ is present and has the value of @aws:kms@ , this header specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
--
-- * 'porsSSEKMSEncryptionContext' - If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
--
-- * 'porsServerSideEncryption' - If you specified server-side encryption either with an AWS KMS customer master key (CMK) or Amazon S3-managed encryption key in your PUT request, the response includes this header. It confirms the encryption algorithm that Amazon S3 used to encrypt the object.
--
-- * 'porsResponseStatus' - -- | The response status code.
putObjectResponse ::
  -- | 'porsResponseStatus'
  Int ->
  PutObjectResponse
putObjectResponse pResponseStatus_ =
  PutObjectResponse'
    { _porsRequestCharged = Nothing,
      _porsETag = Nothing,
      _porsVersionId = Nothing,
      _porsExpiration = Nothing,
      _porsSSECustomerAlgorithm = Nothing,
      _porsSSECustomerKeyMD5 = Nothing,
      _porsSSEKMSKeyId = Nothing,
      _porsSSEKMSEncryptionContext = Nothing,
      _porsServerSideEncryption = Nothing,
      _porsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
porsRequestCharged :: Lens' PutObjectResponse (Maybe RequestCharged)
porsRequestCharged = lens _porsRequestCharged (\s a -> s {_porsRequestCharged = a})

-- | Entity tag for the uploaded object.
porsETag :: Lens' PutObjectResponse (Maybe ETag)
porsETag = lens _porsETag (\s a -> s {_porsETag = a})

-- | Version of the object.
porsVersionId :: Lens' PutObjectResponse (Maybe ObjectVersionId)
porsVersionId = lens _porsVersionId (\s a -> s {_porsVersionId = a})

-- | If the expiration is configured for the object (see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycleConfiguration.html PutBucketLifecycleConfiguration> ), the response includes this header. It includes the expiry-date and rule-id key-value pairs that provide information about object expiration. The value of the rule-id is URL encoded.
porsExpiration :: Lens' PutObjectResponse (Maybe Text)
porsExpiration = lens _porsExpiration (\s a -> s {_porsExpiration = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
porsSSECustomerAlgorithm :: Lens' PutObjectResponse (Maybe Text)
porsSSECustomerAlgorithm = lens _porsSSECustomerAlgorithm (\s a -> s {_porsSSECustomerAlgorithm = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round-trip message integrity verification of the customer-provided encryption key.
porsSSECustomerKeyMD5 :: Lens' PutObjectResponse (Maybe Text)
porsSSECustomerKeyMD5 = lens _porsSSECustomerKeyMD5 (\s a -> s {_porsSSECustomerKeyMD5 = a})

-- | If @x-amz-server-side-encryption@ is present and has the value of @aws:kms@ , this header specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) that was used for the object.
porsSSEKMSKeyId :: Lens' PutObjectResponse (Maybe Text)
porsSSEKMSKeyId = lens _porsSSEKMSKeyId (\s a -> s {_porsSSEKMSKeyId = a}) . mapping _Sensitive

-- | If present, specifies the AWS KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
porsSSEKMSEncryptionContext :: Lens' PutObjectResponse (Maybe Text)
porsSSEKMSEncryptionContext = lens _porsSSEKMSEncryptionContext (\s a -> s {_porsSSEKMSEncryptionContext = a}) . mapping _Sensitive

-- | If you specified server-side encryption either with an AWS KMS customer master key (CMK) or Amazon S3-managed encryption key in your PUT request, the response includes this header. It confirms the encryption algorithm that Amazon S3 used to encrypt the object.
porsServerSideEncryption :: Lens' PutObjectResponse (Maybe ServerSideEncryption)
porsServerSideEncryption = lens _porsServerSideEncryption (\s a -> s {_porsServerSideEncryption = a})

-- | -- | The response status code.
porsResponseStatus :: Lens' PutObjectResponse Int
porsResponseStatus = lens _porsResponseStatus (\s a -> s {_porsResponseStatus = a})

instance NFData PutObjectResponse
