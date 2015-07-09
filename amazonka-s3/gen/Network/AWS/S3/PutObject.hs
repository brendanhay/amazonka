{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
    , poContentLength
    , poExpires
    , poSSECustomerAlgorithm
    , poGrantReadACP
    , poSSECustomerKey
    , poRequestPayer
    , poGrantWriteACP
    , poWebsiteRedirectLocation
    , poGrantRead
    , poStorageClass
    , poContentEncoding
    , poSSEKMSKeyId
    , poGrantFullControl
    , poSSECustomerKeyMD5
    , poMetadata
    , poContentMD5
    , poCacheControl
    , poContentLanguage
    , poACL
    , poContentDisposition
    , poServerSideEncryption
    , poContentType
    , poBucket
    , poKey
    , poBody

    -- * Response
    , PutObjectResponse
    -- ** Response constructor
    , putObjectResponse
    -- ** Response lenses
    , porVersionId
    , porETag
    , porRequestCharged
    , porExpiration
    , porSSECustomerAlgorithm
    , porSSEKMSKeyId
    , porSSECustomerKeyMD5
    , porServerSideEncryption
    , porStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putObject' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'poContentLength'
--
-- * 'poExpires'
--
-- * 'poSSECustomerAlgorithm'
--
-- * 'poGrantReadACP'
--
-- * 'poSSECustomerKey'
--
-- * 'poRequestPayer'
--
-- * 'poGrantWriteACP'
--
-- * 'poWebsiteRedirectLocation'
--
-- * 'poGrantRead'
--
-- * 'poStorageClass'
--
-- * 'poContentEncoding'
--
-- * 'poSSEKMSKeyId'
--
-- * 'poGrantFullControl'
--
-- * 'poSSECustomerKeyMD5'
--
-- * 'poMetadata'
--
-- * 'poContentMD5'
--
-- * 'poCacheControl'
--
-- * 'poContentLanguage'
--
-- * 'poACL'
--
-- * 'poContentDisposition'
--
-- * 'poServerSideEncryption'
--
-- * 'poContentType'
--
-- * 'poBucket'
--
-- * 'poKey'
--
-- * 'poBody'
data PutObject = PutObject'
    { _poContentLength           :: !(Maybe Int)
    , _poExpires                 :: !(Maybe RFC822)
    , _poSSECustomerAlgorithm    :: !(Maybe Text)
    , _poGrantReadACP            :: !(Maybe Text)
    , _poSSECustomerKey          :: !(Maybe (Sensitive Text))
    , _poRequestPayer            :: !(Maybe RequestPayer)
    , _poGrantWriteACP           :: !(Maybe Text)
    , _poWebsiteRedirectLocation :: !(Maybe Text)
    , _poGrantRead               :: !(Maybe Text)
    , _poStorageClass            :: !(Maybe StorageClass)
    , _poContentEncoding         :: !(Maybe Text)
    , _poSSEKMSKeyId             :: !(Maybe (Sensitive Text))
    , _poGrantFullControl        :: !(Maybe Text)
    , _poSSECustomerKeyMD5       :: !(Maybe Text)
    , _poMetadata                :: !(Map Text Text)
    , _poContentMD5              :: !(Maybe Text)
    , _poCacheControl            :: !(Maybe Text)
    , _poContentLanguage         :: !(Maybe Text)
    , _poACL                     :: !(Maybe ObjectCannedACL)
    , _poContentDisposition      :: !(Maybe Text)
    , _poServerSideEncryption    :: !(Maybe ServerSideEncryption)
    , _poContentType             :: !(Maybe Text)
    , _poBucket                  :: !BucketName
    , _poKey                     :: !ObjectKey
    , _poBody                    :: !RqBody
    } deriving (Show,Generic)

-- | 'PutObject' smart constructor.
putObject :: BucketName -> ObjectKey -> RqBody -> PutObject
putObject pBucket pKey pBody =
    PutObject'
    { _poContentLength = Nothing
    , _poExpires = Nothing
    , _poSSECustomerAlgorithm = Nothing
    , _poGrantReadACP = Nothing
    , _poSSECustomerKey = Nothing
    , _poRequestPayer = Nothing
    , _poGrantWriteACP = Nothing
    , _poWebsiteRedirectLocation = Nothing
    , _poGrantRead = Nothing
    , _poStorageClass = Nothing
    , _poContentEncoding = Nothing
    , _poSSEKMSKeyId = Nothing
    , _poGrantFullControl = Nothing
    , _poSSECustomerKeyMD5 = Nothing
    , _poMetadata = mempty
    , _poContentMD5 = Nothing
    , _poCacheControl = Nothing
    , _poContentLanguage = Nothing
    , _poACL = Nothing
    , _poContentDisposition = Nothing
    , _poServerSideEncryption = Nothing
    , _poContentType = Nothing
    , _poBucket = pBucket
    , _poKey = pKey
    , _poBody = pBody
    }

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
poContentLength :: Lens' PutObject (Maybe Int)
poContentLength = lens _poContentLength (\ s a -> s{_poContentLength = a});

-- | The date and time at which the object is no longer cacheable.
poExpires :: Lens' PutObject (Maybe UTCTime)
poExpires = lens _poExpires (\ s a -> s{_poExpires = a}) . mapping _Time;

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
poSSECustomerAlgorithm :: Lens' PutObject (Maybe Text)
poSSECustomerAlgorithm = lens _poSSECustomerAlgorithm (\ s a -> s{_poSSECustomerAlgorithm = a});

-- | Allows grantee to read the object ACL.
poGrantReadACP :: Lens' PutObject (Maybe Text)
poGrantReadACP = lens _poGrantReadACP (\ s a -> s{_poGrantReadACP = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header.
poSSECustomerKey :: Lens' PutObject (Maybe Text)
poSSECustomerKey = lens _poSSECustomerKey (\ s a -> s{_poSSECustomerKey = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
poRequestPayer :: Lens' PutObject (Maybe RequestPayer)
poRequestPayer = lens _poRequestPayer (\ s a -> s{_poRequestPayer = a});

-- | Allows grantee to write the ACL for the applicable object.
poGrantWriteACP :: Lens' PutObject (Maybe Text)
poGrantWriteACP = lens _poGrantWriteACP (\ s a -> s{_poGrantWriteACP = a});

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
poWebsiteRedirectLocation :: Lens' PutObject (Maybe Text)
poWebsiteRedirectLocation = lens _poWebsiteRedirectLocation (\ s a -> s{_poWebsiteRedirectLocation = a});

-- | Allows grantee to read the object data and its metadata.
poGrantRead :: Lens' PutObject (Maybe Text)
poGrantRead = lens _poGrantRead (\ s a -> s{_poGrantRead = a});

-- | The type of storage to use for the object. Defaults to \'STANDARD\'.
poStorageClass :: Lens' PutObject (Maybe StorageClass)
poStorageClass = lens _poStorageClass (\ s a -> s{_poStorageClass = a});

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
poContentEncoding :: Lens' PutObject (Maybe Text)
poContentEncoding = lens _poContentEncoding (\ s a -> s{_poContentEncoding = a});

-- | Specifies the AWS KMS key ID to use for object encryption. All GET and
-- PUT requests for an object protected by AWS KMS will fail if not made
-- via SSL or using SigV4. Documentation on configuring any of the
-- officially supported AWS SDKs and CLI can be found at
-- http:\/\/docs.aws.amazon.com\/AmazonS3\/latest\/dev\/UsingAWSSDK.html#specify-signature-version
poSSEKMSKeyId :: Lens' PutObject (Maybe Text)
poSSEKMSKeyId = lens _poSSEKMSKeyId (\ s a -> s{_poSSEKMSKeyId = a}) . mapping _Sensitive;

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
poGrantFullControl :: Lens' PutObject (Maybe Text)
poGrantFullControl = lens _poGrantFullControl (\ s a -> s{_poGrantFullControl = a});

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
poSSECustomerKeyMD5 :: Lens' PutObject (Maybe Text)
poSSECustomerKeyMD5 = lens _poSSECustomerKeyMD5 (\ s a -> s{_poSSECustomerKeyMD5 = a});

-- | A map of metadata to store with the object in S3.
poMetadata :: Lens' PutObject (HashMap Text Text)
poMetadata = lens _poMetadata (\ s a -> s{_poMetadata = a}) . _Map;

-- | FIXME: Undocumented member.
poContentMD5 :: Lens' PutObject (Maybe Text)
poContentMD5 = lens _poContentMD5 (\ s a -> s{_poContentMD5 = a});

-- | Specifies caching behavior along the request\/reply chain.
poCacheControl :: Lens' PutObject (Maybe Text)
poCacheControl = lens _poCacheControl (\ s a -> s{_poCacheControl = a});

-- | The language the content is in.
poContentLanguage :: Lens' PutObject (Maybe Text)
poContentLanguage = lens _poContentLanguage (\ s a -> s{_poContentLanguage = a});

-- | The canned ACL to apply to the object.
poACL :: Lens' PutObject (Maybe ObjectCannedACL)
poACL = lens _poACL (\ s a -> s{_poACL = a});

-- | Specifies presentational information for the object.
poContentDisposition :: Lens' PutObject (Maybe Text)
poContentDisposition = lens _poContentDisposition (\ s a -> s{_poContentDisposition = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
poServerSideEncryption :: Lens' PutObject (Maybe ServerSideEncryption)
poServerSideEncryption = lens _poServerSideEncryption (\ s a -> s{_poServerSideEncryption = a});

-- | A standard MIME type describing the format of the object data.
poContentType :: Lens' PutObject (Maybe Text)
poContentType = lens _poContentType (\ s a -> s{_poContentType = a});

-- | FIXME: Undocumented member.
poBucket :: Lens' PutObject BucketName
poBucket = lens _poBucket (\ s a -> s{_poBucket = a});

-- | FIXME: Undocumented member.
poKey :: Lens' PutObject ObjectKey
poKey = lens _poKey (\ s a -> s{_poKey = a});

-- | Object data.
poBody :: Lens' PutObject RqBody
poBody = lens _poBody (\ s a -> s{_poBody = a});

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
        toBody = _poBody

instance ToHeaders PutObject where
        toHeaders PutObject'{..}
          = mconcat
              ["Content-Length" =# _poContentLength,
               "Expires" =# _poExpires,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _poSSECustomerAlgorithm,
               "x-amz-grant-read-acp" =# _poGrantReadACP,
               "x-amz-server-side-encryption-customer-key" =#
                 _poSSECustomerKey,
               "x-amz-request-payer" =# _poRequestPayer,
               "x-amz-grant-write-acp" =# _poGrantWriteACP,
               "x-amz-website-redirect-location" =#
                 _poWebsiteRedirectLocation,
               "x-amz-grant-read" =# _poGrantRead,
               "x-amz-storage-class" =# _poStorageClass,
               "Content-Encoding" =# _poContentEncoding,
               "x-amz-server-side-encryption-aws-kms-key-id" =#
                 _poSSEKMSKeyId,
               "x-amz-grant-full-control" =# _poGrantFullControl,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _poSSECustomerKeyMD5,
               "x-amz-meta-" =# _poMetadata,
               "Content-MD5" =# _poContentMD5,
               "Cache-Control" =# _poCacheControl,
               "Content-Language" =# _poContentLanguage,
               "x-amz-acl" =# _poACL,
               "Content-Disposition" =# _poContentDisposition,
               "x-amz-server-side-encryption" =#
                 _poServerSideEncryption,
               "Content-Type" =# _poContentType]

instance ToPath PutObject where
        toPath PutObject'{..}
          = mconcat ["/", toText _poBucket, "/", toText _poKey]

instance ToQuery PutObject where
        toQuery = const mempty

-- | /See:/ 'putObjectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'porVersionId'
--
-- * 'porETag'
--
-- * 'porRequestCharged'
--
-- * 'porExpiration'
--
-- * 'porSSECustomerAlgorithm'
--
-- * 'porSSEKMSKeyId'
--
-- * 'porSSECustomerKeyMD5'
--
-- * 'porServerSideEncryption'
--
-- * 'porStatus'
data PutObjectResponse = PutObjectResponse'
    { _porVersionId            :: !(Maybe ObjectVersionId)
    , _porETag                 :: !(Maybe ETag)
    , _porRequestCharged       :: !(Maybe RequestCharged)
    , _porExpiration           :: !(Maybe Text)
    , _porSSECustomerAlgorithm :: !(Maybe Text)
    , _porSSEKMSKeyId          :: !(Maybe (Sensitive Text))
    , _porSSECustomerKeyMD5    :: !(Maybe Text)
    , _porServerSideEncryption :: !(Maybe ServerSideEncryption)
    , _porStatus               :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutObjectResponse' smart constructor.
putObjectResponse :: Int -> PutObjectResponse
putObjectResponse pStatus =
    PutObjectResponse'
    { _porVersionId = Nothing
    , _porETag = Nothing
    , _porRequestCharged = Nothing
    , _porExpiration = Nothing
    , _porSSECustomerAlgorithm = Nothing
    , _porSSEKMSKeyId = Nothing
    , _porSSECustomerKeyMD5 = Nothing
    , _porServerSideEncryption = Nothing
    , _porStatus = pStatus
    }

-- | Version of the object.
porVersionId :: Lens' PutObjectResponse (Maybe ObjectVersionId)
porVersionId = lens _porVersionId (\ s a -> s{_porVersionId = a});

-- | Entity tag for the uploaded object.
porETag :: Lens' PutObjectResponse (Maybe ETag)
porETag = lens _porETag (\ s a -> s{_porETag = a});

-- | FIXME: Undocumented member.
porRequestCharged :: Lens' PutObjectResponse (Maybe RequestCharged)
porRequestCharged = lens _porRequestCharged (\ s a -> s{_porRequestCharged = a});

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
porExpiration :: Lens' PutObjectResponse (Maybe Text)
porExpiration = lens _porExpiration (\ s a -> s{_porExpiration = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
porSSECustomerAlgorithm :: Lens' PutObjectResponse (Maybe Text)
porSSECustomerAlgorithm = lens _porSSECustomerAlgorithm (\ s a -> s{_porSSECustomerAlgorithm = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
porSSEKMSKeyId :: Lens' PutObjectResponse (Maybe Text)
porSSEKMSKeyId = lens _porSSEKMSKeyId (\ s a -> s{_porSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
porSSECustomerKeyMD5 :: Lens' PutObjectResponse (Maybe Text)
porSSECustomerKeyMD5 = lens _porSSECustomerKeyMD5 (\ s a -> s{_porSSECustomerKeyMD5 = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
porServerSideEncryption :: Lens' PutObjectResponse (Maybe ServerSideEncryption)
porServerSideEncryption = lens _porServerSideEncryption (\ s a -> s{_porServerSideEncryption = a});

-- | FIXME: Undocumented member.
porStatus :: Lens' PutObjectResponse Int
porStatus = lens _porStatus (\ s a -> s{_porStatus = a});
