{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObject
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds an object to a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutObject.html>
module Network.AWS.S3.PutObject
    (
    -- * Request
      PutObject
    -- ** Request constructor
    , putObject
    -- ** Request lenses
    , porqContentLength
    , porqExpires
    , porqSSECustomerAlgorithm
    , porqGrantReadACP
    , porqSSECustomerKey
    , porqRequestPayer
    , porqGrantWriteACP
    , porqWebsiteRedirectLocation
    , porqGrantRead
    , porqStorageClass
    , porqContentEncoding
    , porqSSEKMSKeyId
    , porqGrantFullControl
    , porqSSECustomerKeyMD5
    , porqMetadata
    , porqContentMD5
    , porqCacheControl
    , porqContentLanguage
    , porqACL
    , porqContentDisposition
    , porqServerSideEncryption
    , porqContentType
    , porqBucket
    , porqKey
    , porqBody

    -- * Response
    , PutObjectResponse
    -- ** Response constructor
    , putObjectResponse
    -- ** Response lenses
    , porsVersionId
    , porsETag
    , porsRequestCharged
    , porsExpiration
    , porsSSECustomerAlgorithm
    , porsSSEKMSKeyId
    , porsSSECustomerKeyMD5
    , porsServerSideEncryption
    , porsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putObject' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'porqContentLength'
--
-- * 'porqExpires'
--
-- * 'porqSSECustomerAlgorithm'
--
-- * 'porqGrantReadACP'
--
-- * 'porqSSECustomerKey'
--
-- * 'porqRequestPayer'
--
-- * 'porqGrantWriteACP'
--
-- * 'porqWebsiteRedirectLocation'
--
-- * 'porqGrantRead'
--
-- * 'porqStorageClass'
--
-- * 'porqContentEncoding'
--
-- * 'porqSSEKMSKeyId'
--
-- * 'porqGrantFullControl'
--
-- * 'porqSSECustomerKeyMD5'
--
-- * 'porqMetadata'
--
-- * 'porqContentMD5'
--
-- * 'porqCacheControl'
--
-- * 'porqContentLanguage'
--
-- * 'porqACL'
--
-- * 'porqContentDisposition'
--
-- * 'porqServerSideEncryption'
--
-- * 'porqContentType'
--
-- * 'porqBucket'
--
-- * 'porqKey'
--
-- * 'porqBody'
data PutObject = PutObject'
    { _porqContentLength           :: !(Maybe Int)
    , _porqExpires                 :: !(Maybe RFC822)
    , _porqSSECustomerAlgorithm    :: !(Maybe Text)
    , _porqGrantReadACP            :: !(Maybe Text)
    , _porqSSECustomerKey          :: !(Maybe (Sensitive Text))
    , _porqRequestPayer            :: !(Maybe RequestPayer)
    , _porqGrantWriteACP           :: !(Maybe Text)
    , _porqWebsiteRedirectLocation :: !(Maybe Text)
    , _porqGrantRead               :: !(Maybe Text)
    , _porqStorageClass            :: !(Maybe StorageClass)
    , _porqContentEncoding         :: !(Maybe Text)
    , _porqSSEKMSKeyId             :: !(Maybe (Sensitive Text))
    , _porqGrantFullControl        :: !(Maybe Text)
    , _porqSSECustomerKeyMD5       :: !(Maybe Text)
    , _porqMetadata                :: !(Map Text Text)
    , _porqContentMD5              :: !(Maybe Text)
    , _porqCacheControl            :: !(Maybe Text)
    , _porqContentLanguage         :: !(Maybe Text)
    , _porqACL                     :: !(Maybe ObjectCannedACL)
    , _porqContentDisposition      :: !(Maybe Text)
    , _porqServerSideEncryption    :: !(Maybe ServerSideEncryption)
    , _porqContentType             :: !(Maybe Text)
    , _porqBucket                  :: !BucketName
    , _porqKey                     :: !ObjectKey
    , _porqBody                    :: !RqBody
    } deriving (Show,Generic)

-- | 'PutObject' smart constructor.
putObject :: BucketName -> ObjectKey -> RqBody -> PutObject
putObject pBucket_ pKey_ pBody_ =
    PutObject'
    { _porqContentLength = Nothing
    , _porqExpires = Nothing
    , _porqSSECustomerAlgorithm = Nothing
    , _porqGrantReadACP = Nothing
    , _porqSSECustomerKey = Nothing
    , _porqRequestPayer = Nothing
    , _porqGrantWriteACP = Nothing
    , _porqWebsiteRedirectLocation = Nothing
    , _porqGrantRead = Nothing
    , _porqStorageClass = Nothing
    , _porqContentEncoding = Nothing
    , _porqSSEKMSKeyId = Nothing
    , _porqGrantFullControl = Nothing
    , _porqSSECustomerKeyMD5 = Nothing
    , _porqMetadata = mempty
    , _porqContentMD5 = Nothing
    , _porqCacheControl = Nothing
    , _porqContentLanguage = Nothing
    , _porqACL = Nothing
    , _porqContentDisposition = Nothing
    , _porqServerSideEncryption = Nothing
    , _porqContentType = Nothing
    , _porqBucket = pBucket_
    , _porqKey = pKey_
    , _porqBody = pBody_
    }

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
porqContentLength :: Lens' PutObject (Maybe Int)
porqContentLength = lens _porqContentLength (\ s a -> s{_porqContentLength = a});

-- | The date and time at which the object is no longer cacheable.
porqExpires :: Lens' PutObject (Maybe UTCTime)
porqExpires = lens _porqExpires (\ s a -> s{_porqExpires = a}) . mapping _Time;

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
porqSSECustomerAlgorithm :: Lens' PutObject (Maybe Text)
porqSSECustomerAlgorithm = lens _porqSSECustomerAlgorithm (\ s a -> s{_porqSSECustomerAlgorithm = a});

-- | Allows grantee to read the object ACL.
porqGrantReadACP :: Lens' PutObject (Maybe Text)
porqGrantReadACP = lens _porqGrantReadACP (\ s a -> s{_porqGrantReadACP = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header.
porqSSECustomerKey :: Lens' PutObject (Maybe Text)
porqSSECustomerKey = lens _porqSSECustomerKey (\ s a -> s{_porqSSECustomerKey = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
porqRequestPayer :: Lens' PutObject (Maybe RequestPayer)
porqRequestPayer = lens _porqRequestPayer (\ s a -> s{_porqRequestPayer = a});

-- | Allows grantee to write the ACL for the applicable object.
porqGrantWriteACP :: Lens' PutObject (Maybe Text)
porqGrantWriteACP = lens _porqGrantWriteACP (\ s a -> s{_porqGrantWriteACP = a});

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
porqWebsiteRedirectLocation :: Lens' PutObject (Maybe Text)
porqWebsiteRedirectLocation = lens _porqWebsiteRedirectLocation (\ s a -> s{_porqWebsiteRedirectLocation = a});

-- | Allows grantee to read the object data and its metadata.
porqGrantRead :: Lens' PutObject (Maybe Text)
porqGrantRead = lens _porqGrantRead (\ s a -> s{_porqGrantRead = a});

-- | The type of storage to use for the object. Defaults to \'STANDARD\'.
porqStorageClass :: Lens' PutObject (Maybe StorageClass)
porqStorageClass = lens _porqStorageClass (\ s a -> s{_porqStorageClass = a});

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
porqContentEncoding :: Lens' PutObject (Maybe Text)
porqContentEncoding = lens _porqContentEncoding (\ s a -> s{_porqContentEncoding = a});

-- | Specifies the AWS KMS key ID to use for object encryption. All GET and
-- PUT requests for an object protected by AWS KMS will fail if not made
-- via SSL or using SigV4. Documentation on configuring any of the
-- officially supported AWS SDKs and CLI can be found at
-- http:\/\/docs.aws.amazon.com\/AmazonS3\/latest\/dev\/UsingAWSSDK.html#specify-signature-version
porqSSEKMSKeyId :: Lens' PutObject (Maybe Text)
porqSSEKMSKeyId = lens _porqSSEKMSKeyId (\ s a -> s{_porqSSEKMSKeyId = a}) . mapping _Sensitive;

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
porqGrantFullControl :: Lens' PutObject (Maybe Text)
porqGrantFullControl = lens _porqGrantFullControl (\ s a -> s{_porqGrantFullControl = a});

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
porqSSECustomerKeyMD5 :: Lens' PutObject (Maybe Text)
porqSSECustomerKeyMD5 = lens _porqSSECustomerKeyMD5 (\ s a -> s{_porqSSECustomerKeyMD5 = a});

-- | A map of metadata to store with the object in S3.
porqMetadata :: Lens' PutObject (HashMap Text Text)
porqMetadata = lens _porqMetadata (\ s a -> s{_porqMetadata = a}) . _Map;

-- | FIXME: Undocumented member.
porqContentMD5 :: Lens' PutObject (Maybe Text)
porqContentMD5 = lens _porqContentMD5 (\ s a -> s{_porqContentMD5 = a});

-- | Specifies caching behavior along the request\/reply chain.
porqCacheControl :: Lens' PutObject (Maybe Text)
porqCacheControl = lens _porqCacheControl (\ s a -> s{_porqCacheControl = a});

-- | The language the content is in.
porqContentLanguage :: Lens' PutObject (Maybe Text)
porqContentLanguage = lens _porqContentLanguage (\ s a -> s{_porqContentLanguage = a});

-- | The canned ACL to apply to the object.
porqACL :: Lens' PutObject (Maybe ObjectCannedACL)
porqACL = lens _porqACL (\ s a -> s{_porqACL = a});

-- | Specifies presentational information for the object.
porqContentDisposition :: Lens' PutObject (Maybe Text)
porqContentDisposition = lens _porqContentDisposition (\ s a -> s{_porqContentDisposition = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
porqServerSideEncryption :: Lens' PutObject (Maybe ServerSideEncryption)
porqServerSideEncryption = lens _porqServerSideEncryption (\ s a -> s{_porqServerSideEncryption = a});

-- | A standard MIME type describing the format of the object data.
porqContentType :: Lens' PutObject (Maybe Text)
porqContentType = lens _porqContentType (\ s a -> s{_porqContentType = a});

-- | FIXME: Undocumented member.
porqBucket :: Lens' PutObject BucketName
porqBucket = lens _porqBucket (\ s a -> s{_porqBucket = a});

-- | FIXME: Undocumented member.
porqKey :: Lens' PutObject ObjectKey
porqKey = lens _porqKey (\ s a -> s{_porqKey = a});

-- | Object data.
porqBody :: Lens' PutObject RqBody
porqBody = lens _porqBody (\ s a -> s{_porqBody = a});

instance AWSRequest PutObject where
        type Sv PutObject = S3
        type Rs PutObject = PutObjectResponse
        request = putBody
        response
          = receiveXML
              (\ s h x ->
                 PutObjectResponse' <$>
                   (h .#? "x-amz-version-id") <*> (h .#? "ETag") <*>
                     (h .#? "x-amz-request-charged")
                     <*> (h .#? "x-amz-expiration")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-algorithm")
                     <*>
                     (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-key-MD5")
                     <*> (h .#? "x-amz-server-side-encryption")
                     <*> (pure (fromEnum s)))

instance ToBody PutObject where
        toBody = _porqBody

instance ToHeaders PutObject where
        toHeaders PutObject'{..}
          = mconcat
              ["Content-Length" =# _porqContentLength,
               "Expires" =# _porqExpires,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _porqSSECustomerAlgorithm,
               "x-amz-grant-read-acp" =# _porqGrantReadACP,
               "x-amz-server-side-encryption-customer-key" =#
                 _porqSSECustomerKey,
               "x-amz-request-payer" =# _porqRequestPayer,
               "x-amz-grant-write-acp" =# _porqGrantWriteACP,
               "x-amz-website-redirect-location" =#
                 _porqWebsiteRedirectLocation,
               "x-amz-grant-read" =# _porqGrantRead,
               "x-amz-storage-class" =# _porqStorageClass,
               "Content-Encoding" =# _porqContentEncoding,
               "x-amz-server-side-encryption-aws-kms-key-id" =#
                 _porqSSEKMSKeyId,
               "x-amz-grant-full-control" =# _porqGrantFullControl,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _porqSSECustomerKeyMD5,
               "x-amz-meta-" =# _porqMetadata,
               "Content-MD5" =# _porqContentMD5,
               "Cache-Control" =# _porqCacheControl,
               "Content-Language" =# _porqContentLanguage,
               "x-amz-acl" =# _porqACL,
               "Content-Disposition" =# _porqContentDisposition,
               "x-amz-server-side-encryption" =#
                 _porqServerSideEncryption,
               "Content-Type" =# _porqContentType]

instance ToPath PutObject where
        toPath PutObject'{..}
          = mconcat
              ["/", toText _porqBucket, "/", toText _porqKey]

instance ToQuery PutObject where
        toQuery = const mempty

-- | /See:/ 'putObjectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'porsVersionId'
--
-- * 'porsETag'
--
-- * 'porsRequestCharged'
--
-- * 'porsExpiration'
--
-- * 'porsSSECustomerAlgorithm'
--
-- * 'porsSSEKMSKeyId'
--
-- * 'porsSSECustomerKeyMD5'
--
-- * 'porsServerSideEncryption'
--
-- * 'porsStatus'
data PutObjectResponse = PutObjectResponse'
    { _porsVersionId            :: !(Maybe ObjectVersionId)
    , _porsETag                 :: !(Maybe ETag)
    , _porsRequestCharged       :: !(Maybe RequestCharged)
    , _porsExpiration           :: !(Maybe Text)
    , _porsSSECustomerAlgorithm :: !(Maybe Text)
    , _porsSSEKMSKeyId          :: !(Maybe (Sensitive Text))
    , _porsSSECustomerKeyMD5    :: !(Maybe Text)
    , _porsServerSideEncryption :: !(Maybe ServerSideEncryption)
    , _porsStatus               :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutObjectResponse' smart constructor.
putObjectResponse :: Int -> PutObjectResponse
putObjectResponse pStatus_ =
    PutObjectResponse'
    { _porsVersionId = Nothing
    , _porsETag = Nothing
    , _porsRequestCharged = Nothing
    , _porsExpiration = Nothing
    , _porsSSECustomerAlgorithm = Nothing
    , _porsSSEKMSKeyId = Nothing
    , _porsSSECustomerKeyMD5 = Nothing
    , _porsServerSideEncryption = Nothing
    , _porsStatus = pStatus_
    }

-- | Version of the object.
porsVersionId :: Lens' PutObjectResponse (Maybe ObjectVersionId)
porsVersionId = lens _porsVersionId (\ s a -> s{_porsVersionId = a});

-- | Entity tag for the uploaded object.
porsETag :: Lens' PutObjectResponse (Maybe ETag)
porsETag = lens _porsETag (\ s a -> s{_porsETag = a});

-- | FIXME: Undocumented member.
porsRequestCharged :: Lens' PutObjectResponse (Maybe RequestCharged)
porsRequestCharged = lens _porsRequestCharged (\ s a -> s{_porsRequestCharged = a});

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
porsExpiration :: Lens' PutObjectResponse (Maybe Text)
porsExpiration = lens _porsExpiration (\ s a -> s{_porsExpiration = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
porsSSECustomerAlgorithm :: Lens' PutObjectResponse (Maybe Text)
porsSSECustomerAlgorithm = lens _porsSSECustomerAlgorithm (\ s a -> s{_porsSSECustomerAlgorithm = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
porsSSEKMSKeyId :: Lens' PutObjectResponse (Maybe Text)
porsSSEKMSKeyId = lens _porsSSEKMSKeyId (\ s a -> s{_porsSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
porsSSECustomerKeyMD5 :: Lens' PutObjectResponse (Maybe Text)
porsSSECustomerKeyMD5 = lens _porsSSECustomerKeyMD5 (\ s a -> s{_porsSSECustomerKeyMD5 = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
porsServerSideEncryption :: Lens' PutObjectResponse (Maybe ServerSideEncryption)
porsServerSideEncryption = lens _porsServerSideEncryption (\ s a -> s{_porsServerSideEncryption = a});

-- | FIXME: Undocumented member.
porsStatus :: Lens' PutObjectResponse Int
porsStatus = lens _porsStatus (\ s a -> s{_porsStatus = a});
