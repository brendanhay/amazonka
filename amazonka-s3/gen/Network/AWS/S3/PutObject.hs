{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an object to a bucket.
module Network.AWS.S3.PutObject
    (
    -- * Creating a Request
      putObject
    , PutObject
    -- * Request Lenses
    , poContentLength
    , poExpires
    , poGrantReadACP
    , poSSECustomerAlgorithm
    , poSSECustomerKey
    , poRequestPayer
    , poGrantWriteACP
    , poWebsiteRedirectLocation
    , poGrantRead
    , poStorageClass
    , poSSECustomerKeyMD5
    , poSSEKMSKeyId
    , poGrantFullControl
    , poContentEncoding
    , poTagging
    , poContentMD5
    , poMetadata
    , poCacheControl
    , poContentLanguage
    , poACL
    , poContentDisposition
    , poServerSideEncryption
    , poContentType
    , poBucket
    , poKey
    , poBody

    -- * Destructuring the Response
    , putObjectResponse
    , PutObjectResponse
    -- * Response Lenses
    , porsRequestCharged
    , porsETag
    , porsVersionId
    , porsExpiration
    , porsSSECustomerAlgorithm
    , porsSSECustomerKeyMD5
    , porsSSEKMSKeyId
    , porsServerSideEncryption
    , porsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putObject' smart constructor.
data PutObject = PutObject'
  { _poContentLength           :: !(Maybe Integer)
  , _poExpires                 :: !(Maybe RFC822)
  , _poGrantReadACP            :: !(Maybe Text)
  , _poSSECustomerAlgorithm    :: !(Maybe Text)
  , _poSSECustomerKey          :: !(Maybe (Sensitive Text))
  , _poRequestPayer            :: !(Maybe RequestPayer)
  , _poGrantWriteACP           :: !(Maybe Text)
  , _poWebsiteRedirectLocation :: !(Maybe Text)
  , _poGrantRead               :: !(Maybe Text)
  , _poStorageClass            :: !(Maybe StorageClass)
  , _poSSECustomerKeyMD5       :: !(Maybe Text)
  , _poSSEKMSKeyId             :: !(Maybe (Sensitive Text))
  , _poGrantFullControl        :: !(Maybe Text)
  , _poContentEncoding         :: !(Maybe Text)
  , _poTagging                 :: !(Maybe Text)
  , _poContentMD5              :: !(Maybe Text)
  , _poMetadata                :: !(Map Text Text)
  , _poCacheControl            :: !(Maybe Text)
  , _poContentLanguage         :: !(Maybe Text)
  , _poACL                     :: !(Maybe ObjectCannedACL)
  , _poContentDisposition      :: !(Maybe Text)
  , _poServerSideEncryption    :: !(Maybe ServerSideEncryption)
  , _poContentType             :: !(Maybe Text)
  , _poBucket                  :: !BucketName
  , _poKey                     :: !ObjectKey
  , _poBody                    :: !RqBody
  } deriving (Show, Generic)


-- | Creates a value of 'PutObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poContentLength' - Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically.
--
-- * 'poExpires' - The date and time at which the object is no longer cacheable.
--
-- * 'poGrantReadACP' - Allows grantee to read the object ACL.
--
-- * 'poSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (e.g., AES256).
--
-- * 'poSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side​-encryption​-customer-algorithm header.
--
-- * 'poRequestPayer' - Undocumented member.
--
-- * 'poGrantWriteACP' - Allows grantee to write the ACL for the applicable object.
--
-- * 'poWebsiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- * 'poGrantRead' - Allows grantee to read the object data and its metadata.
--
-- * 'poStorageClass' - The type of storage to use for the object. Defaults to 'STANDARD'.
--
-- * 'poSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
--
-- * 'poSSEKMSKeyId' - Specifies the AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. Documentation on configuring any of the officially supported AWS SDKs and CLI can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version
--
-- * 'poGrantFullControl' - Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
--
-- * 'poContentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- * 'poTagging' - The tag-set for the object. The tag-set must be encoded as URL Query parameters
--
-- * 'poContentMD5' - The base64-encoded 128-bit MD5 digest of the part data.
--
-- * 'poMetadata' - A map of metadata to store with the object in S3.
--
-- * 'poCacheControl' - Specifies caching behavior along the request/reply chain.
--
-- * 'poContentLanguage' - The language the content is in.
--
-- * 'poACL' - The canned ACL to apply to the object.
--
-- * 'poContentDisposition' - Specifies presentational information for the object.
--
-- * 'poServerSideEncryption' - The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
--
-- * 'poContentType' - A standard MIME type describing the format of the object data.
--
-- * 'poBucket' - Name of the bucket to which the PUT operation was initiated.
--
-- * 'poKey' - Object key for which the PUT operation was initiated.
--
-- * 'poBody' - Object data.
putObject
    :: BucketName -- ^ 'poBucket'
    -> ObjectKey -- ^ 'poKey'
    -> RqBody -- ^ 'poBody'
    -> PutObject
putObject pBucket_ pKey_ pBody_ =
  PutObject'
    { _poContentLength = Nothing
    , _poExpires = Nothing
    , _poGrantReadACP = Nothing
    , _poSSECustomerAlgorithm = Nothing
    , _poSSECustomerKey = Nothing
    , _poRequestPayer = Nothing
    , _poGrantWriteACP = Nothing
    , _poWebsiteRedirectLocation = Nothing
    , _poGrantRead = Nothing
    , _poStorageClass = Nothing
    , _poSSECustomerKeyMD5 = Nothing
    , _poSSEKMSKeyId = Nothing
    , _poGrantFullControl = Nothing
    , _poContentEncoding = Nothing
    , _poTagging = Nothing
    , _poContentMD5 = Nothing
    , _poMetadata = mempty
    , _poCacheControl = Nothing
    , _poContentLanguage = Nothing
    , _poACL = Nothing
    , _poContentDisposition = Nothing
    , _poServerSideEncryption = Nothing
    , _poContentType = Nothing
    , _poBucket = pBucket_
    , _poKey = pKey_
    , _poBody = pBody_
    }


-- | Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically.
poContentLength :: Lens' PutObject (Maybe Integer)
poContentLength = lens _poContentLength (\ s a -> s{_poContentLength = a})

-- | The date and time at which the object is no longer cacheable.
poExpires :: Lens' PutObject (Maybe UTCTime)
poExpires = lens _poExpires (\ s a -> s{_poExpires = a}) . mapping _Time

-- | Allows grantee to read the object ACL.
poGrantReadACP :: Lens' PutObject (Maybe Text)
poGrantReadACP = lens _poGrantReadACP (\ s a -> s{_poGrantReadACP = a})

-- | Specifies the algorithm to use to when encrypting the object (e.g., AES256).
poSSECustomerAlgorithm :: Lens' PutObject (Maybe Text)
poSSECustomerAlgorithm = lens _poSSECustomerAlgorithm (\ s a -> s{_poSSECustomerAlgorithm = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side​-encryption​-customer-algorithm header.
poSSECustomerKey :: Lens' PutObject (Maybe Text)
poSSECustomerKey = lens _poSSECustomerKey (\ s a -> s{_poSSECustomerKey = a}) . mapping _Sensitive

-- | Undocumented member.
poRequestPayer :: Lens' PutObject (Maybe RequestPayer)
poRequestPayer = lens _poRequestPayer (\ s a -> s{_poRequestPayer = a})

-- | Allows grantee to write the ACL for the applicable object.
poGrantWriteACP :: Lens' PutObject (Maybe Text)
poGrantWriteACP = lens _poGrantWriteACP (\ s a -> s{_poGrantWriteACP = a})

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
poWebsiteRedirectLocation :: Lens' PutObject (Maybe Text)
poWebsiteRedirectLocation = lens _poWebsiteRedirectLocation (\ s a -> s{_poWebsiteRedirectLocation = a})

-- | Allows grantee to read the object data and its metadata.
poGrantRead :: Lens' PutObject (Maybe Text)
poGrantRead = lens _poGrantRead (\ s a -> s{_poGrantRead = a})

-- | The type of storage to use for the object. Defaults to 'STANDARD'.
poStorageClass :: Lens' PutObject (Maybe StorageClass)
poStorageClass = lens _poStorageClass (\ s a -> s{_poStorageClass = a})

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
poSSECustomerKeyMD5 :: Lens' PutObject (Maybe Text)
poSSECustomerKeyMD5 = lens _poSSECustomerKeyMD5 (\ s a -> s{_poSSECustomerKeyMD5 = a})

-- | Specifies the AWS KMS key ID to use for object encryption. All GET and PUT requests for an object protected by AWS KMS will fail if not made via SSL or using SigV4. Documentation on configuring any of the officially supported AWS SDKs and CLI can be found at http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingAWSSDK.html#specify-signature-version
poSSEKMSKeyId :: Lens' PutObject (Maybe Text)
poSSEKMSKeyId = lens _poSSEKMSKeyId (\ s a -> s{_poSSEKMSKeyId = a}) . mapping _Sensitive

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
poGrantFullControl :: Lens' PutObject (Maybe Text)
poGrantFullControl = lens _poGrantFullControl (\ s a -> s{_poGrantFullControl = a})

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
poContentEncoding :: Lens' PutObject (Maybe Text)
poContentEncoding = lens _poContentEncoding (\ s a -> s{_poContentEncoding = a})

-- | The tag-set for the object. The tag-set must be encoded as URL Query parameters
poTagging :: Lens' PutObject (Maybe Text)
poTagging = lens _poTagging (\ s a -> s{_poTagging = a})

-- | The base64-encoded 128-bit MD5 digest of the part data.
poContentMD5 :: Lens' PutObject (Maybe Text)
poContentMD5 = lens _poContentMD5 (\ s a -> s{_poContentMD5 = a})

-- | A map of metadata to store with the object in S3.
poMetadata :: Lens' PutObject (HashMap Text Text)
poMetadata = lens _poMetadata (\ s a -> s{_poMetadata = a}) . _Map

-- | Specifies caching behavior along the request/reply chain.
poCacheControl :: Lens' PutObject (Maybe Text)
poCacheControl = lens _poCacheControl (\ s a -> s{_poCacheControl = a})

-- | The language the content is in.
poContentLanguage :: Lens' PutObject (Maybe Text)
poContentLanguage = lens _poContentLanguage (\ s a -> s{_poContentLanguage = a})

-- | The canned ACL to apply to the object.
poACL :: Lens' PutObject (Maybe ObjectCannedACL)
poACL = lens _poACL (\ s a -> s{_poACL = a})

-- | Specifies presentational information for the object.
poContentDisposition :: Lens' PutObject (Maybe Text)
poContentDisposition = lens _poContentDisposition (\ s a -> s{_poContentDisposition = a})

-- | The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
poServerSideEncryption :: Lens' PutObject (Maybe ServerSideEncryption)
poServerSideEncryption = lens _poServerSideEncryption (\ s a -> s{_poServerSideEncryption = a})

-- | A standard MIME type describing the format of the object data.
poContentType :: Lens' PutObject (Maybe Text)
poContentType = lens _poContentType (\ s a -> s{_poContentType = a})

-- | Name of the bucket to which the PUT operation was initiated.
poBucket :: Lens' PutObject BucketName
poBucket = lens _poBucket (\ s a -> s{_poBucket = a})

-- | Object key for which the PUT operation was initiated.
poKey :: Lens' PutObject ObjectKey
poKey = lens _poKey (\ s a -> s{_poKey = a})

-- | Object data.
poBody :: Lens' PutObject RqBody
poBody = lens _poBody (\ s a -> s{_poBody = a})

instance AWSRequest PutObject where
        type Rs PutObject = PutObjectResponse
        request = expectHeader . putBody s3
        response
          = receiveEmpty
              (\ s h x ->
                 PutObjectResponse' <$>
                   (h .#? "x-amz-request-charged") <*> (h .#? "ETag")
                     <*> (h .#? "x-amz-version-id")
                     <*> (h .#? "x-amz-expiration")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-algorithm")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-key-MD5")
                     <*>
                     (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
                     <*> (h .#? "x-amz-server-side-encryption")
                     <*> (pure (fromEnum s)))

instance ToBody PutObject where
        toBody = toBody . _poBody

instance ToHeaders PutObject where
        toHeaders PutObject'{..}
          = mconcat
              ["Content-Length" =# _poContentLength,
               "Expires" =# _poExpires,
               "x-amz-grant-read-acp" =# _poGrantReadACP,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _poSSECustomerAlgorithm,
               "x-amz-server-side-encryption-customer-key" =#
                 _poSSECustomerKey,
               "x-amz-request-payer" =# _poRequestPayer,
               "x-amz-grant-write-acp" =# _poGrantWriteACP,
               "x-amz-website-redirect-location" =#
                 _poWebsiteRedirectLocation,
               "x-amz-grant-read" =# _poGrantRead,
               "x-amz-storage-class" =# _poStorageClass,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _poSSECustomerKeyMD5,
               "x-amz-server-side-encryption-aws-kms-key-id" =#
                 _poSSEKMSKeyId,
               "x-amz-grant-full-control" =# _poGrantFullControl,
               "Content-Encoding" =# _poContentEncoding,
               "x-amz-tagging" =# _poTagging,
               "Content-MD5" =# _poContentMD5,
               "x-amz-meta-" =# _poMetadata,
               "Cache-Control" =# _poCacheControl,
               "Content-Language" =# _poContentLanguage,
               "x-amz-acl" =# _poACL,
               "Content-Disposition" =# _poContentDisposition,
               "x-amz-server-side-encryption" =#
                 _poServerSideEncryption,
               "Content-Type" =# _poContentType]

instance ToPath PutObject where
        toPath PutObject'{..}
          = mconcat ["/", toBS _poBucket, "/", toBS _poKey]

instance ToQuery PutObject where
        toQuery = const mempty

-- | /See:/ 'putObjectResponse' smart constructor.
data PutObjectResponse = PutObjectResponse'
  { _porsRequestCharged       :: !(Maybe RequestCharged)
  , _porsETag                 :: !(Maybe ETag)
  , _porsVersionId            :: !(Maybe ObjectVersionId)
  , _porsExpiration           :: !(Maybe Text)
  , _porsSSECustomerAlgorithm :: !(Maybe Text)
  , _porsSSECustomerKeyMD5    :: !(Maybe Text)
  , _porsSSEKMSKeyId          :: !(Maybe (Sensitive Text))
  , _porsServerSideEncryption :: !(Maybe ServerSideEncryption)
  , _porsResponseStatus       :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


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
-- * 'porsExpiration' - If the object expiration is configured, this will contain the expiration date (expiry-date) and rule ID (rule-id). The value of rule-id is URL encoded.
--
-- * 'porsSSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- * 'porsSSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key.
--
-- * 'porsSSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (KMS) master encryption key that was used for the object.
--
-- * 'porsServerSideEncryption' - The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
--
-- * 'porsResponseStatus' - -- | The response status code.
putObjectResponse
    :: Int -- ^ 'porsResponseStatus'
    -> PutObjectResponse
putObjectResponse pResponseStatus_ =
  PutObjectResponse'
    { _porsRequestCharged = Nothing
    , _porsETag = Nothing
    , _porsVersionId = Nothing
    , _porsExpiration = Nothing
    , _porsSSECustomerAlgorithm = Nothing
    , _porsSSECustomerKeyMD5 = Nothing
    , _porsSSEKMSKeyId = Nothing
    , _porsServerSideEncryption = Nothing
    , _porsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
porsRequestCharged :: Lens' PutObjectResponse (Maybe RequestCharged)
porsRequestCharged = lens _porsRequestCharged (\ s a -> s{_porsRequestCharged = a})

-- | Entity tag for the uploaded object.
porsETag :: Lens' PutObjectResponse (Maybe ETag)
porsETag = lens _porsETag (\ s a -> s{_porsETag = a})

-- | Version of the object.
porsVersionId :: Lens' PutObjectResponse (Maybe ObjectVersionId)
porsVersionId = lens _porsVersionId (\ s a -> s{_porsVersionId = a})

-- | If the object expiration is configured, this will contain the expiration date (expiry-date) and rule ID (rule-id). The value of rule-id is URL encoded.
porsExpiration :: Lens' PutObjectResponse (Maybe Text)
porsExpiration = lens _porsExpiration (\ s a -> s{_porsExpiration = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
porsSSECustomerAlgorithm :: Lens' PutObjectResponse (Maybe Text)
porsSSECustomerAlgorithm = lens _porsSSECustomerAlgorithm (\ s a -> s{_porsSSECustomerAlgorithm = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key.
porsSSECustomerKeyMD5 :: Lens' PutObjectResponse (Maybe Text)
porsSSECustomerKeyMD5 = lens _porsSSECustomerKeyMD5 (\ s a -> s{_porsSSECustomerKeyMD5 = a})

-- | If present, specifies the ID of the AWS Key Management Service (KMS) master encryption key that was used for the object.
porsSSEKMSKeyId :: Lens' PutObjectResponse (Maybe Text)
porsSSEKMSKeyId = lens _porsSSEKMSKeyId (\ s a -> s{_porsSSEKMSKeyId = a}) . mapping _Sensitive

-- | The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
porsServerSideEncryption :: Lens' PutObjectResponse (Maybe ServerSideEncryption)
porsServerSideEncryption = lens _porsServerSideEncryption (\ s a -> s{_porsServerSideEncryption = a})

-- | -- | The response status code.
porsResponseStatus :: Lens' PutObjectResponse Int
porsResponseStatus = lens _porsResponseStatus (\ s a -> s{_porsResponseStatus = a})

instance NFData PutObjectResponse where
