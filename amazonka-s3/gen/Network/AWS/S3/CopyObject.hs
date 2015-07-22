{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.CopyObject
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a copy of an object that is already stored in Amazon S3.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/CopyObject.html>
module Network.AWS.S3.CopyObject
    (
    -- * Request
      CopyObject
    -- ** Request constructor
    , copyObject
    -- ** Request lenses
    , corqCopySourceIfModifiedSince
    , corqCopySourceIfUnmodifiedSince
    , corqCopySourceSSECustomerKeyMD5
    , corqMetadataDirective
    , corqExpires
    , corqSSECustomerAlgorithm
    , corqCopySourceIfNoneMatch
    , corqGrantReadACP
    , corqSSECustomerKey
    , corqRequestPayer
    , corqGrantWriteACP
    , corqWebsiteRedirectLocation
    , corqCopySourceIfMatch
    , corqGrantRead
    , corqStorageClass
    , corqContentEncoding
    , corqSSEKMSKeyId
    , corqGrantFullControl
    , corqSSECustomerKeyMD5
    , corqMetadata
    , corqCacheControl
    , corqContentLanguage
    , corqACL
    , corqCopySourceSSECustomerKey
    , corqContentDisposition
    , corqCopySourceSSECustomerAlgorithm
    , corqServerSideEncryption
    , corqContentType
    , corqBucket
    , corqCopySource
    , corqKey

    -- * Response
    , CopyObjectResponse
    -- ** Response constructor
    , copyObjectResponse
    -- ** Response lenses
    , corsRequestCharged
    , corsExpiration
    , corsSSECustomerAlgorithm
    , corsCopySourceVersionId
    , corsSSEKMSKeyId
    , corsSSECustomerKeyMD5
    , corsServerSideEncryption
    , corsCopyObjectResult
    , corsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'copyObject' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'corqCopySourceIfModifiedSince'
--
-- * 'corqCopySourceIfUnmodifiedSince'
--
-- * 'corqCopySourceSSECustomerKeyMD5'
--
-- * 'corqMetadataDirective'
--
-- * 'corqExpires'
--
-- * 'corqSSECustomerAlgorithm'
--
-- * 'corqCopySourceIfNoneMatch'
--
-- * 'corqGrantReadACP'
--
-- * 'corqSSECustomerKey'
--
-- * 'corqRequestPayer'
--
-- * 'corqGrantWriteACP'
--
-- * 'corqWebsiteRedirectLocation'
--
-- * 'corqCopySourceIfMatch'
--
-- * 'corqGrantRead'
--
-- * 'corqStorageClass'
--
-- * 'corqContentEncoding'
--
-- * 'corqSSEKMSKeyId'
--
-- * 'corqGrantFullControl'
--
-- * 'corqSSECustomerKeyMD5'
--
-- * 'corqMetadata'
--
-- * 'corqCacheControl'
--
-- * 'corqContentLanguage'
--
-- * 'corqACL'
--
-- * 'corqCopySourceSSECustomerKey'
--
-- * 'corqContentDisposition'
--
-- * 'corqCopySourceSSECustomerAlgorithm'
--
-- * 'corqServerSideEncryption'
--
-- * 'corqContentType'
--
-- * 'corqBucket'
--
-- * 'corqCopySource'
--
-- * 'corqKey'
data CopyObject = CopyObject'
    { _corqCopySourceIfModifiedSince      :: !(Maybe RFC822)
    , _corqCopySourceIfUnmodifiedSince    :: !(Maybe RFC822)
    , _corqCopySourceSSECustomerKeyMD5    :: !(Maybe Text)
    , _corqMetadataDirective              :: !(Maybe MetadataDirective)
    , _corqExpires                        :: !(Maybe RFC822)
    , _corqSSECustomerAlgorithm           :: !(Maybe Text)
    , _corqCopySourceIfNoneMatch          :: !(Maybe Text)
    , _corqGrantReadACP                   :: !(Maybe Text)
    , _corqSSECustomerKey                 :: !(Maybe (Sensitive Text))
    , _corqRequestPayer                   :: !(Maybe RequestPayer)
    , _corqGrantWriteACP                  :: !(Maybe Text)
    , _corqWebsiteRedirectLocation        :: !(Maybe Text)
    , _corqCopySourceIfMatch              :: !(Maybe Text)
    , _corqGrantRead                      :: !(Maybe Text)
    , _corqStorageClass                   :: !(Maybe StorageClass)
    , _corqContentEncoding                :: !(Maybe Text)
    , _corqSSEKMSKeyId                    :: !(Maybe (Sensitive Text))
    , _corqGrantFullControl               :: !(Maybe Text)
    , _corqSSECustomerKeyMD5              :: !(Maybe Text)
    , _corqMetadata                       :: !(Map Text Text)
    , _corqCacheControl                   :: !(Maybe Text)
    , _corqContentLanguage                :: !(Maybe Text)
    , _corqACL                            :: !(Maybe ObjectCannedACL)
    , _corqCopySourceSSECustomerKey       :: !(Maybe (Sensitive Text))
    , _corqContentDisposition             :: !(Maybe Text)
    , _corqCopySourceSSECustomerAlgorithm :: !(Maybe Text)
    , _corqServerSideEncryption           :: !(Maybe ServerSideEncryption)
    , _corqContentType                    :: !(Maybe Text)
    , _corqBucket                         :: !BucketName
    , _corqCopySource                     :: !Text
    , _corqKey                            :: !ObjectKey
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CopyObject' smart constructor.
copyObject :: BucketName -> Text -> ObjectKey -> CopyObject
copyObject pBucket_ pCopySource_ pKey_ =
    CopyObject'
    { _corqCopySourceIfModifiedSince = Nothing
    , _corqCopySourceIfUnmodifiedSince = Nothing
    , _corqCopySourceSSECustomerKeyMD5 = Nothing
    , _corqMetadataDirective = Nothing
    , _corqExpires = Nothing
    , _corqSSECustomerAlgorithm = Nothing
    , _corqCopySourceIfNoneMatch = Nothing
    , _corqGrantReadACP = Nothing
    , _corqSSECustomerKey = Nothing
    , _corqRequestPayer = Nothing
    , _corqGrantWriteACP = Nothing
    , _corqWebsiteRedirectLocation = Nothing
    , _corqCopySourceIfMatch = Nothing
    , _corqGrantRead = Nothing
    , _corqStorageClass = Nothing
    , _corqContentEncoding = Nothing
    , _corqSSEKMSKeyId = Nothing
    , _corqGrantFullControl = Nothing
    , _corqSSECustomerKeyMD5 = Nothing
    , _corqMetadata = mempty
    , _corqCacheControl = Nothing
    , _corqContentLanguage = Nothing
    , _corqACL = Nothing
    , _corqCopySourceSSECustomerKey = Nothing
    , _corqContentDisposition = Nothing
    , _corqCopySourceSSECustomerAlgorithm = Nothing
    , _corqServerSideEncryption = Nothing
    , _corqContentType = Nothing
    , _corqBucket = pBucket_
    , _corqCopySource = pCopySource_
    , _corqKey = pKey_
    }

-- | Copies the object if it has been modified since the specified time.
corqCopySourceIfModifiedSince :: Lens' CopyObject (Maybe UTCTime)
corqCopySourceIfModifiedSince = lens _corqCopySourceIfModifiedSince (\ s a -> s{_corqCopySourceIfModifiedSince = a}) . mapping _Time;

-- | Copies the object if it hasn\'t been modified since the specified time.
corqCopySourceIfUnmodifiedSince :: Lens' CopyObject (Maybe UTCTime)
corqCopySourceIfUnmodifiedSince = lens _corqCopySourceIfUnmodifiedSince (\ s a -> s{_corqCopySourceIfUnmodifiedSince = a}) . mapping _Time;

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
corqCopySourceSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
corqCopySourceSSECustomerKeyMD5 = lens _corqCopySourceSSECustomerKeyMD5 (\ s a -> s{_corqCopySourceSSECustomerKeyMD5 = a});

-- | Specifies whether the metadata is copied from the source object or
-- replaced with metadata provided in the request.
corqMetadataDirective :: Lens' CopyObject (Maybe MetadataDirective)
corqMetadataDirective = lens _corqMetadataDirective (\ s a -> s{_corqMetadataDirective = a});

-- | The date and time at which the object is no longer cacheable.
corqExpires :: Lens' CopyObject (Maybe UTCTime)
corqExpires = lens _corqExpires (\ s a -> s{_corqExpires = a}) . mapping _Time;

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
corqSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
corqSSECustomerAlgorithm = lens _corqSSECustomerAlgorithm (\ s a -> s{_corqSSECustomerAlgorithm = a});

-- | Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
corqCopySourceIfNoneMatch :: Lens' CopyObject (Maybe Text)
corqCopySourceIfNoneMatch = lens _corqCopySourceIfNoneMatch (\ s a -> s{_corqCopySourceIfNoneMatch = a});

-- | Allows grantee to read the object ACL.
corqGrantReadACP :: Lens' CopyObject (Maybe Text)
corqGrantReadACP = lens _corqGrantReadACP (\ s a -> s{_corqGrantReadACP = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header.
corqSSECustomerKey :: Lens' CopyObject (Maybe Text)
corqSSECustomerKey = lens _corqSSECustomerKey (\ s a -> s{_corqSSECustomerKey = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
corqRequestPayer :: Lens' CopyObject (Maybe RequestPayer)
corqRequestPayer = lens _corqRequestPayer (\ s a -> s{_corqRequestPayer = a});

-- | Allows grantee to write the ACL for the applicable object.
corqGrantWriteACP :: Lens' CopyObject (Maybe Text)
corqGrantWriteACP = lens _corqGrantWriteACP (\ s a -> s{_corqGrantWriteACP = a});

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
corqWebsiteRedirectLocation :: Lens' CopyObject (Maybe Text)
corqWebsiteRedirectLocation = lens _corqWebsiteRedirectLocation (\ s a -> s{_corqWebsiteRedirectLocation = a});

-- | Copies the object if its entity tag (ETag) matches the specified tag.
corqCopySourceIfMatch :: Lens' CopyObject (Maybe Text)
corqCopySourceIfMatch = lens _corqCopySourceIfMatch (\ s a -> s{_corqCopySourceIfMatch = a});

-- | Allows grantee to read the object data and its metadata.
corqGrantRead :: Lens' CopyObject (Maybe Text)
corqGrantRead = lens _corqGrantRead (\ s a -> s{_corqGrantRead = a});

-- | The type of storage to use for the object. Defaults to \'STANDARD\'.
corqStorageClass :: Lens' CopyObject (Maybe StorageClass)
corqStorageClass = lens _corqStorageClass (\ s a -> s{_corqStorageClass = a});

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
corqContentEncoding :: Lens' CopyObject (Maybe Text)
corqContentEncoding = lens _corqContentEncoding (\ s a -> s{_corqContentEncoding = a});

-- | Specifies the AWS KMS key ID to use for object encryption. All GET and
-- PUT requests for an object protected by AWS KMS will fail if not made
-- via SSL or using SigV4. Documentation on configuring any of the
-- officially supported AWS SDKs and CLI can be found at
-- http:\/\/docs.aws.amazon.com\/AmazonS3\/latest\/dev\/UsingAWSSDK.html#specify-signature-version
corqSSEKMSKeyId :: Lens' CopyObject (Maybe Text)
corqSSEKMSKeyId = lens _corqSSEKMSKeyId (\ s a -> s{_corqSSEKMSKeyId = a}) . mapping _Sensitive;

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
corqGrantFullControl :: Lens' CopyObject (Maybe Text)
corqGrantFullControl = lens _corqGrantFullControl (\ s a -> s{_corqGrantFullControl = a});

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
corqSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
corqSSECustomerKeyMD5 = lens _corqSSECustomerKeyMD5 (\ s a -> s{_corqSSECustomerKeyMD5 = a});

-- | A map of metadata to store with the object in S3.
corqMetadata :: Lens' CopyObject (HashMap Text Text)
corqMetadata = lens _corqMetadata (\ s a -> s{_corqMetadata = a}) . _Map;

-- | Specifies caching behavior along the request\/reply chain.
corqCacheControl :: Lens' CopyObject (Maybe Text)
corqCacheControl = lens _corqCacheControl (\ s a -> s{_corqCacheControl = a});

-- | The language the content is in.
corqContentLanguage :: Lens' CopyObject (Maybe Text)
corqContentLanguage = lens _corqContentLanguage (\ s a -> s{_corqContentLanguage = a});

-- | The canned ACL to apply to the object.
corqACL :: Lens' CopyObject (Maybe ObjectCannedACL)
corqACL = lens _corqACL (\ s a -> s{_corqACL = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
corqCopySourceSSECustomerKey :: Lens' CopyObject (Maybe Text)
corqCopySourceSSECustomerKey = lens _corqCopySourceSSECustomerKey (\ s a -> s{_corqCopySourceSSECustomerKey = a}) . mapping _Sensitive;

-- | Specifies presentational information for the object.
corqContentDisposition :: Lens' CopyObject (Maybe Text)
corqContentDisposition = lens _corqContentDisposition (\ s a -> s{_corqContentDisposition = a});

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
corqCopySourceSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
corqCopySourceSSECustomerAlgorithm = lens _corqCopySourceSSECustomerAlgorithm (\ s a -> s{_corqCopySourceSSECustomerAlgorithm = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
corqServerSideEncryption :: Lens' CopyObject (Maybe ServerSideEncryption)
corqServerSideEncryption = lens _corqServerSideEncryption (\ s a -> s{_corqServerSideEncryption = a});

-- | A standard MIME type describing the format of the object data.
corqContentType :: Lens' CopyObject (Maybe Text)
corqContentType = lens _corqContentType (\ s a -> s{_corqContentType = a});

-- | FIXME: Undocumented member.
corqBucket :: Lens' CopyObject BucketName
corqBucket = lens _corqBucket (\ s a -> s{_corqBucket = a});

-- | The name of the source bucket and key name of the source object,
-- separated by a slash (\/). Must be URL-encoded.
corqCopySource :: Lens' CopyObject Text
corqCopySource = lens _corqCopySource (\ s a -> s{_corqCopySource = a});

-- | FIXME: Undocumented member.
corqKey :: Lens' CopyObject ObjectKey
corqKey = lens _corqKey (\ s a -> s{_corqKey = a});

instance AWSRequest CopyObject where
        type Sv CopyObject = S3
        type Rs CopyObject = CopyObjectResponse
        request = put
        response
          = receiveXML
              (\ s h x ->
                 CopyObjectResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (h .#? "x-amz-expiration")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-algorithm")
                     <*> (h .#? "x-amz-copy-source-version-id")
                     <*>
                     (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-key-MD5")
                     <*> (h .#? "x-amz-server-side-encryption")
                     <*> (parseXML x)
                     <*> (pure (fromEnum s)))

instance ToHeaders CopyObject where
        toHeaders CopyObject'{..}
          = mconcat
              ["x-amz-copy-source-if-modified-since" =#
                 _corqCopySourceIfModifiedSince,
               "x-amz-copy-source-if-unmodified-since" =#
                 _corqCopySourceIfUnmodifiedSince,
               "x-amz-copy-source-server-side-encryption-customer-key-MD5"
                 =# _corqCopySourceSSECustomerKeyMD5,
               "x-amz-metadata-directive" =# _corqMetadataDirective,
               "Expires" =# _corqExpires,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _corqSSECustomerAlgorithm,
               "x-amz-copy-source-if-none-match" =#
                 _corqCopySourceIfNoneMatch,
               "x-amz-grant-read-acp" =# _corqGrantReadACP,
               "x-amz-server-side-encryption-customer-key" =#
                 _corqSSECustomerKey,
               "x-amz-request-payer" =# _corqRequestPayer,
               "x-amz-grant-write-acp" =# _corqGrantWriteACP,
               "x-amz-website-redirect-location" =#
                 _corqWebsiteRedirectLocation,
               "x-amz-copy-source-if-match" =#
                 _corqCopySourceIfMatch,
               "x-amz-grant-read" =# _corqGrantRead,
               "x-amz-storage-class" =# _corqStorageClass,
               "Content-Encoding" =# _corqContentEncoding,
               "x-amz-server-side-encryption-aws-kms-key-id" =#
                 _corqSSEKMSKeyId,
               "x-amz-grant-full-control" =# _corqGrantFullControl,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _corqSSECustomerKeyMD5,
               "x-amz-meta-" =# _corqMetadata,
               "Cache-Control" =# _corqCacheControl,
               "Content-Language" =# _corqContentLanguage,
               "x-amz-acl" =# _corqACL,
               "x-amz-copy-source-server-side-encryption-customer-key"
                 =# _corqCopySourceSSECustomerKey,
               "Content-Disposition" =# _corqContentDisposition,
               "x-amz-copy-source-server-side-encryption-customer-algorithm"
                 =# _corqCopySourceSSECustomerAlgorithm,
               "x-amz-server-side-encryption" =#
                 _corqServerSideEncryption,
               "Content-Type" =# _corqContentType,
               "x-amz-copy-source" =# _corqCopySource]

instance ToPath CopyObject where
        toPath CopyObject'{..}
          = mconcat
              ["/", toText _corqBucket, "/", toText _corqKey]

instance ToQuery CopyObject where
        toQuery = const mempty

-- | /See:/ 'copyObjectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'corsRequestCharged'
--
-- * 'corsExpiration'
--
-- * 'corsSSECustomerAlgorithm'
--
-- * 'corsCopySourceVersionId'
--
-- * 'corsSSEKMSKeyId'
--
-- * 'corsSSECustomerKeyMD5'
--
-- * 'corsServerSideEncryption'
--
-- * 'corsCopyObjectResult'
--
-- * 'corsStatus'
data CopyObjectResponse = CopyObjectResponse'
    { _corsRequestCharged       :: !(Maybe RequestCharged)
    , _corsExpiration           :: !(Maybe Text)
    , _corsSSECustomerAlgorithm :: !(Maybe Text)
    , _corsCopySourceVersionId  :: !(Maybe Text)
    , _corsSSEKMSKeyId          :: !(Maybe (Sensitive Text))
    , _corsSSECustomerKeyMD5    :: !(Maybe Text)
    , _corsServerSideEncryption :: !(Maybe ServerSideEncryption)
    , _corsCopyObjectResult     :: !(Maybe CopyObjectResult)
    , _corsStatus               :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CopyObjectResponse' smart constructor.
copyObjectResponse :: Int -> CopyObjectResponse
copyObjectResponse pStatus_ =
    CopyObjectResponse'
    { _corsRequestCharged = Nothing
    , _corsExpiration = Nothing
    , _corsSSECustomerAlgorithm = Nothing
    , _corsCopySourceVersionId = Nothing
    , _corsSSEKMSKeyId = Nothing
    , _corsSSECustomerKeyMD5 = Nothing
    , _corsServerSideEncryption = Nothing
    , _corsCopyObjectResult = Nothing
    , _corsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
corsRequestCharged :: Lens' CopyObjectResponse (Maybe RequestCharged)
corsRequestCharged = lens _corsRequestCharged (\ s a -> s{_corsRequestCharged = a});

-- | If the object expiration is configured, the response includes this
-- header.
corsExpiration :: Lens' CopyObjectResponse (Maybe Text)
corsExpiration = lens _corsExpiration (\ s a -> s{_corsExpiration = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
corsSSECustomerAlgorithm :: Lens' CopyObjectResponse (Maybe Text)
corsSSECustomerAlgorithm = lens _corsSSECustomerAlgorithm (\ s a -> s{_corsSSECustomerAlgorithm = a});

-- | FIXME: Undocumented member.
corsCopySourceVersionId :: Lens' CopyObjectResponse (Maybe Text)
corsCopySourceVersionId = lens _corsCopySourceVersionId (\ s a -> s{_corsCopySourceVersionId = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
corsSSEKMSKeyId :: Lens' CopyObjectResponse (Maybe Text)
corsSSEKMSKeyId = lens _corsSSEKMSKeyId (\ s a -> s{_corsSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
corsSSECustomerKeyMD5 :: Lens' CopyObjectResponse (Maybe Text)
corsSSECustomerKeyMD5 = lens _corsSSECustomerKeyMD5 (\ s a -> s{_corsSSECustomerKeyMD5 = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
corsServerSideEncryption :: Lens' CopyObjectResponse (Maybe ServerSideEncryption)
corsServerSideEncryption = lens _corsServerSideEncryption (\ s a -> s{_corsServerSideEncryption = a});

-- | FIXME: Undocumented member.
corsCopyObjectResult :: Lens' CopyObjectResponse (Maybe CopyObjectResult)
corsCopyObjectResult = lens _corsCopyObjectResult (\ s a -> s{_corsCopyObjectResult = a});

-- | FIXME: Undocumented member.
corsStatus :: Lens' CopyObjectResponse Int
corsStatus = lens _corsStatus (\ s a -> s{_corsStatus = a});
