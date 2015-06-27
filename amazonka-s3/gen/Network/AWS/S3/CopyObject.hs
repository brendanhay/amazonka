{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.CopyObject
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a copy of an object that is already stored in Amazon S3.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/CopyObject.html>
module Network.AWS.S3.CopyObject
    (
    -- * Request
      CopyObject
    -- ** Request constructor
    , copyObject
    -- ** Request lenses
    , coCopySourceIfModifiedSince
    , coCopySourceIfUnmodifiedSince
    , coCopySourceSSECustomerKeyMD5
    , coMetadataDirective
    , coExpires
    , coSSECustomerAlgorithm
    , coCopySourceIfNoneMatch
    , coGrantReadACP
    , coSSECustomerKey
    , coRequestPayer
    , coGrantWriteACP
    , coWebsiteRedirectLocation
    , coCopySourceIfMatch
    , coGrantRead
    , coStorageClass
    , coContentEncoding
    , coSSEKMSKeyId
    , coGrantFullControl
    , coSSECustomerKeyMD5
    , coMetadata
    , coCacheControl
    , coContentLanguage
    , coACL
    , coCopySourceSSECustomerKey
    , coContentDisposition
    , coCopySourceSSECustomerAlgorithm
    , coServerSideEncryption
    , coContentType
    , coBucket
    , coCopySource
    , coKey

    -- * Response
    , CopyObjectResponse
    -- ** Response constructor
    , copyObjectResponse
    -- ** Response lenses
    , corRequestCharged
    , corExpiration
    , corSSECustomerAlgorithm
    , corCopySourceVersionId
    , corSSEKMSKeyId
    , corSSECustomerKeyMD5
    , corServerSideEncryption
    , corCopyObjectResult
    , corStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'copyObject' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'coCopySourceIfModifiedSince'
--
-- * 'coCopySourceIfUnmodifiedSince'
--
-- * 'coCopySourceSSECustomerKeyMD5'
--
-- * 'coMetadataDirective'
--
-- * 'coExpires'
--
-- * 'coSSECustomerAlgorithm'
--
-- * 'coCopySourceIfNoneMatch'
--
-- * 'coGrantReadACP'
--
-- * 'coSSECustomerKey'
--
-- * 'coRequestPayer'
--
-- * 'coGrantWriteACP'
--
-- * 'coWebsiteRedirectLocation'
--
-- * 'coCopySourceIfMatch'
--
-- * 'coGrantRead'
--
-- * 'coStorageClass'
--
-- * 'coContentEncoding'
--
-- * 'coSSEKMSKeyId'
--
-- * 'coGrantFullControl'
--
-- * 'coSSECustomerKeyMD5'
--
-- * 'coMetadata'
--
-- * 'coCacheControl'
--
-- * 'coContentLanguage'
--
-- * 'coACL'
--
-- * 'coCopySourceSSECustomerKey'
--
-- * 'coContentDisposition'
--
-- * 'coCopySourceSSECustomerAlgorithm'
--
-- * 'coServerSideEncryption'
--
-- * 'coContentType'
--
-- * 'coBucket'
--
-- * 'coCopySource'
--
-- * 'coKey'
data CopyObject = CopyObject'
    { _coCopySourceIfModifiedSince      :: !(Maybe RFC822)
    , _coCopySourceIfUnmodifiedSince    :: !(Maybe RFC822)
    , _coCopySourceSSECustomerKeyMD5    :: !(Maybe Text)
    , _coMetadataDirective              :: !(Maybe MetadataDirective)
    , _coExpires                        :: !(Maybe RFC822)
    , _coSSECustomerAlgorithm           :: !(Maybe Text)
    , _coCopySourceIfNoneMatch          :: !(Maybe Text)
    , _coGrantReadACP                   :: !(Maybe Text)
    , _coSSECustomerKey                 :: !(Maybe (Sensitive Text))
    , _coRequestPayer                   :: !(Maybe RequestPayer)
    , _coGrantWriteACP                  :: !(Maybe Text)
    , _coWebsiteRedirectLocation        :: !(Maybe Text)
    , _coCopySourceIfMatch              :: !(Maybe Text)
    , _coGrantRead                      :: !(Maybe Text)
    , _coStorageClass                   :: !(Maybe StorageClass)
    , _coContentEncoding                :: !(Maybe Text)
    , _coSSEKMSKeyId                    :: !(Maybe (Sensitive Text))
    , _coGrantFullControl               :: !(Maybe Text)
    , _coSSECustomerKeyMD5              :: !(Maybe Text)
    , _coMetadata                       :: !(Map Text Text)
    , _coCacheControl                   :: !(Maybe Text)
    , _coContentLanguage                :: !(Maybe Text)
    , _coACL                            :: !(Maybe ObjectCannedACL)
    , _coCopySourceSSECustomerKey       :: !(Maybe (Sensitive Text))
    , _coContentDisposition             :: !(Maybe Text)
    , _coCopySourceSSECustomerAlgorithm :: !(Maybe Text)
    , _coServerSideEncryption           :: !(Maybe ServerSideEncryption)
    , _coContentType                    :: !(Maybe Text)
    , _coBucket                         :: !BucketName
    , _coCopySource                     :: !Text
    , _coKey                            :: !ObjectKey
    } deriving (Eq,Read,Show)

-- | 'CopyObject' smart constructor.
copyObject :: BucketName -> Text -> ObjectKey -> CopyObject
copyObject pBucket pCopySource pKey =
    CopyObject'
    { _coCopySourceIfModifiedSince = Nothing
    , _coCopySourceIfUnmodifiedSince = Nothing
    , _coCopySourceSSECustomerKeyMD5 = Nothing
    , _coMetadataDirective = Nothing
    , _coExpires = Nothing
    , _coSSECustomerAlgorithm = Nothing
    , _coCopySourceIfNoneMatch = Nothing
    , _coGrantReadACP = Nothing
    , _coSSECustomerKey = Nothing
    , _coRequestPayer = Nothing
    , _coGrantWriteACP = Nothing
    , _coWebsiteRedirectLocation = Nothing
    , _coCopySourceIfMatch = Nothing
    , _coGrantRead = Nothing
    , _coStorageClass = Nothing
    , _coContentEncoding = Nothing
    , _coSSEKMSKeyId = Nothing
    , _coGrantFullControl = Nothing
    , _coSSECustomerKeyMD5 = Nothing
    , _coMetadata = mempty
    , _coCacheControl = Nothing
    , _coContentLanguage = Nothing
    , _coACL = Nothing
    , _coCopySourceSSECustomerKey = Nothing
    , _coContentDisposition = Nothing
    , _coCopySourceSSECustomerAlgorithm = Nothing
    , _coServerSideEncryption = Nothing
    , _coContentType = Nothing
    , _coBucket = pBucket
    , _coCopySource = pCopySource
    , _coKey = pKey
    }

-- | Copies the object if it has been modified since the specified time.
coCopySourceIfModifiedSince :: Lens' CopyObject (Maybe UTCTime)
coCopySourceIfModifiedSince = lens _coCopySourceIfModifiedSince (\ s a -> s{_coCopySourceIfModifiedSince = a}) . mapping _Time;

-- | Copies the object if it hasn\'t been modified since the specified time.
coCopySourceIfUnmodifiedSince :: Lens' CopyObject (Maybe UTCTime)
coCopySourceIfUnmodifiedSince = lens _coCopySourceIfUnmodifiedSince (\ s a -> s{_coCopySourceIfUnmodifiedSince = a}) . mapping _Time;

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
coCopySourceSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
coCopySourceSSECustomerKeyMD5 = lens _coCopySourceSSECustomerKeyMD5 (\ s a -> s{_coCopySourceSSECustomerKeyMD5 = a});

-- | Specifies whether the metadata is copied from the source object or
-- replaced with metadata provided in the request.
coMetadataDirective :: Lens' CopyObject (Maybe MetadataDirective)
coMetadataDirective = lens _coMetadataDirective (\ s a -> s{_coMetadataDirective = a});

-- | The date and time at which the object is no longer cacheable.
coExpires :: Lens' CopyObject (Maybe UTCTime)
coExpires = lens _coExpires (\ s a -> s{_coExpires = a}) . mapping _Time;

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
coSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
coSSECustomerAlgorithm = lens _coSSECustomerAlgorithm (\ s a -> s{_coSSECustomerAlgorithm = a});

-- | Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
coCopySourceIfNoneMatch :: Lens' CopyObject (Maybe Text)
coCopySourceIfNoneMatch = lens _coCopySourceIfNoneMatch (\ s a -> s{_coCopySourceIfNoneMatch = a});

-- | Allows grantee to read the object ACL.
coGrantReadACP :: Lens' CopyObject (Maybe Text)
coGrantReadACP = lens _coGrantReadACP (\ s a -> s{_coGrantReadACP = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header.
coSSECustomerKey :: Lens' CopyObject (Maybe Text)
coSSECustomerKey = lens _coSSECustomerKey (\ s a -> s{_coSSECustomerKey = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
coRequestPayer :: Lens' CopyObject (Maybe RequestPayer)
coRequestPayer = lens _coRequestPayer (\ s a -> s{_coRequestPayer = a});

-- | Allows grantee to write the ACL for the applicable object.
coGrantWriteACP :: Lens' CopyObject (Maybe Text)
coGrantWriteACP = lens _coGrantWriteACP (\ s a -> s{_coGrantWriteACP = a});

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
coWebsiteRedirectLocation :: Lens' CopyObject (Maybe Text)
coWebsiteRedirectLocation = lens _coWebsiteRedirectLocation (\ s a -> s{_coWebsiteRedirectLocation = a});

-- | Copies the object if its entity tag (ETag) matches the specified tag.
coCopySourceIfMatch :: Lens' CopyObject (Maybe Text)
coCopySourceIfMatch = lens _coCopySourceIfMatch (\ s a -> s{_coCopySourceIfMatch = a});

-- | Allows grantee to read the object data and its metadata.
coGrantRead :: Lens' CopyObject (Maybe Text)
coGrantRead = lens _coGrantRead (\ s a -> s{_coGrantRead = a});

-- | The type of storage to use for the object. Defaults to \'STANDARD\'.
coStorageClass :: Lens' CopyObject (Maybe StorageClass)
coStorageClass = lens _coStorageClass (\ s a -> s{_coStorageClass = a});

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
coContentEncoding :: Lens' CopyObject (Maybe Text)
coContentEncoding = lens _coContentEncoding (\ s a -> s{_coContentEncoding = a});

-- | Specifies the AWS KMS key ID to use for object encryption. All GET and
-- PUT requests for an object protected by AWS KMS will fail if not made
-- via SSL or using SigV4. Documentation on configuring any of the
-- officially supported AWS SDKs and CLI can be found at
-- http:\/\/docs.aws.amazon.com\/AmazonS3\/latest\/dev\/UsingAWSSDK.html#specify-signature-version
coSSEKMSKeyId :: Lens' CopyObject (Maybe Text)
coSSEKMSKeyId = lens _coSSEKMSKeyId (\ s a -> s{_coSSEKMSKeyId = a}) . mapping _Sensitive;

-- | Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the
-- object.
coGrantFullControl :: Lens' CopyObject (Maybe Text)
coGrantFullControl = lens _coGrantFullControl (\ s a -> s{_coGrantFullControl = a});

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
coSSECustomerKeyMD5 :: Lens' CopyObject (Maybe Text)
coSSECustomerKeyMD5 = lens _coSSECustomerKeyMD5 (\ s a -> s{_coSSECustomerKeyMD5 = a});

-- | A map of metadata to store with the object in S3.
coMetadata :: Lens' CopyObject (HashMap Text Text)
coMetadata = lens _coMetadata (\ s a -> s{_coMetadata = a}) . _Map;

-- | Specifies caching behavior along the request\/reply chain.
coCacheControl :: Lens' CopyObject (Maybe Text)
coCacheControl = lens _coCacheControl (\ s a -> s{_coCacheControl = a});

-- | The language the content is in.
coContentLanguage :: Lens' CopyObject (Maybe Text)
coContentLanguage = lens _coContentLanguage (\ s a -> s{_coContentLanguage = a});

-- | The canned ACL to apply to the object.
coACL :: Lens' CopyObject (Maybe ObjectCannedACL)
coACL = lens _coACL (\ s a -> s{_coACL = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
coCopySourceSSECustomerKey :: Lens' CopyObject (Maybe Text)
coCopySourceSSECustomerKey = lens _coCopySourceSSECustomerKey (\ s a -> s{_coCopySourceSSECustomerKey = a}) . mapping _Sensitive;

-- | Specifies presentational information for the object.
coContentDisposition :: Lens' CopyObject (Maybe Text)
coContentDisposition = lens _coContentDisposition (\ s a -> s{_coContentDisposition = a});

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
coCopySourceSSECustomerAlgorithm :: Lens' CopyObject (Maybe Text)
coCopySourceSSECustomerAlgorithm = lens _coCopySourceSSECustomerAlgorithm (\ s a -> s{_coCopySourceSSECustomerAlgorithm = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
coServerSideEncryption :: Lens' CopyObject (Maybe ServerSideEncryption)
coServerSideEncryption = lens _coServerSideEncryption (\ s a -> s{_coServerSideEncryption = a});

-- | A standard MIME type describing the format of the object data.
coContentType :: Lens' CopyObject (Maybe Text)
coContentType = lens _coContentType (\ s a -> s{_coContentType = a});

-- | FIXME: Undocumented member.
coBucket :: Lens' CopyObject BucketName
coBucket = lens _coBucket (\ s a -> s{_coBucket = a});

-- | The name of the source bucket and key name of the source object,
-- separated by a slash (\/). Must be URL-encoded.
coCopySource :: Lens' CopyObject Text
coCopySource = lens _coCopySource (\ s a -> s{_coCopySource = a});

-- | FIXME: Undocumented member.
coKey :: Lens' CopyObject ObjectKey
coKey = lens _coKey (\ s a -> s{_coKey = a});

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
                     <*> (x .@? "CopyObjectResult")
                     <*> (pure (fromEnum s)))

instance ToHeaders CopyObject where
        toHeaders CopyObject'{..}
          = mconcat
              ["x-amz-copy-source-if-modified-since" =#
                 _coCopySourceIfModifiedSince,
               "x-amz-copy-source-if-unmodified-since" =#
                 _coCopySourceIfUnmodifiedSince,
               "x-amz-copy-source-server-side-encryption-customer-key-MD5"
                 =# _coCopySourceSSECustomerKeyMD5,
               "x-amz-metadata-directive" =# _coMetadataDirective,
               "Expires" =# _coExpires,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _coSSECustomerAlgorithm,
               "x-amz-copy-source-if-none-match" =#
                 _coCopySourceIfNoneMatch,
               "x-amz-grant-read-acp" =# _coGrantReadACP,
               "x-amz-server-side-encryption-customer-key" =#
                 _coSSECustomerKey,
               "x-amz-request-payer" =# _coRequestPayer,
               "x-amz-grant-write-acp" =# _coGrantWriteACP,
               "x-amz-website-redirect-location" =#
                 _coWebsiteRedirectLocation,
               "x-amz-copy-source-if-match" =# _coCopySourceIfMatch,
               "x-amz-grant-read" =# _coGrantRead,
               "x-amz-storage-class" =# _coStorageClass,
               "Content-Encoding" =# _coContentEncoding,
               "x-amz-server-side-encryption-aws-kms-key-id" =#
                 _coSSEKMSKeyId,
               "x-amz-grant-full-control" =# _coGrantFullControl,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _coSSECustomerKeyMD5,
               "x-amz-meta-" =# _coMetadata,
               "Cache-Control" =# _coCacheControl,
               "Content-Language" =# _coContentLanguage,
               "x-amz-acl" =# _coACL,
               "x-amz-copy-source-server-side-encryption-customer-key"
                 =# _coCopySourceSSECustomerKey,
               "Content-Disposition" =# _coContentDisposition,
               "x-amz-copy-source-server-side-encryption-customer-algorithm"
                 =# _coCopySourceSSECustomerAlgorithm,
               "x-amz-server-side-encryption" =#
                 _coServerSideEncryption,
               "Content-Type" =# _coContentType,
               "x-amz-copy-source" =# _coCopySource]

instance ToPath CopyObject where
        toPath CopyObject'{..}
          = mconcat ["/", toText _coBucket, "/", toText _coKey]

instance ToQuery CopyObject where
        toQuery = const mempty

-- | /See:/ 'copyObjectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'corRequestCharged'
--
-- * 'corExpiration'
--
-- * 'corSSECustomerAlgorithm'
--
-- * 'corCopySourceVersionId'
--
-- * 'corSSEKMSKeyId'
--
-- * 'corSSECustomerKeyMD5'
--
-- * 'corServerSideEncryption'
--
-- * 'corCopyObjectResult'
--
-- * 'corStatus'
data CopyObjectResponse = CopyObjectResponse'
    { _corRequestCharged       :: !(Maybe RequestCharged)
    , _corExpiration           :: !(Maybe Text)
    , _corSSECustomerAlgorithm :: !(Maybe Text)
    , _corCopySourceVersionId  :: !(Maybe Text)
    , _corSSEKMSKeyId          :: !(Maybe (Sensitive Text))
    , _corSSECustomerKeyMD5    :: !(Maybe Text)
    , _corServerSideEncryption :: !(Maybe ServerSideEncryption)
    , _corCopyObjectResult     :: !(Maybe CopyObjectResult)
    , _corStatus               :: !Int
    } deriving (Eq,Read,Show)

-- | 'CopyObjectResponse' smart constructor.
copyObjectResponse :: Int -> CopyObjectResponse
copyObjectResponse pStatus =
    CopyObjectResponse'
    { _corRequestCharged = Nothing
    , _corExpiration = Nothing
    , _corSSECustomerAlgorithm = Nothing
    , _corCopySourceVersionId = Nothing
    , _corSSEKMSKeyId = Nothing
    , _corSSECustomerKeyMD5 = Nothing
    , _corServerSideEncryption = Nothing
    , _corCopyObjectResult = Nothing
    , _corStatus = pStatus
    }

-- | FIXME: Undocumented member.
corRequestCharged :: Lens' CopyObjectResponse (Maybe RequestCharged)
corRequestCharged = lens _corRequestCharged (\ s a -> s{_corRequestCharged = a});

-- | If the object expiration is configured, the response includes this
-- header.
corExpiration :: Lens' CopyObjectResponse (Maybe Text)
corExpiration = lens _corExpiration (\ s a -> s{_corExpiration = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
corSSECustomerAlgorithm :: Lens' CopyObjectResponse (Maybe Text)
corSSECustomerAlgorithm = lens _corSSECustomerAlgorithm (\ s a -> s{_corSSECustomerAlgorithm = a});

-- | FIXME: Undocumented member.
corCopySourceVersionId :: Lens' CopyObjectResponse (Maybe Text)
corCopySourceVersionId = lens _corCopySourceVersionId (\ s a -> s{_corCopySourceVersionId = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
corSSEKMSKeyId :: Lens' CopyObjectResponse (Maybe Text)
corSSEKMSKeyId = lens _corSSEKMSKeyId (\ s a -> s{_corSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
corSSECustomerKeyMD5 :: Lens' CopyObjectResponse (Maybe Text)
corSSECustomerKeyMD5 = lens _corSSECustomerKeyMD5 (\ s a -> s{_corSSECustomerKeyMD5 = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
corServerSideEncryption :: Lens' CopyObjectResponse (Maybe ServerSideEncryption)
corServerSideEncryption = lens _corServerSideEncryption (\ s a -> s{_corServerSideEncryption = a});

-- | FIXME: Undocumented member.
corCopyObjectResult :: Lens' CopyObjectResponse (Maybe CopyObjectResult)
corCopyObjectResult = lens _corCopyObjectResult (\ s a -> s{_corCopyObjectResult = a});

-- | FIXME: Undocumented member.
corStatus :: Lens' CopyObjectResponse Int
corStatus = lens _corStatus (\ s a -> s{_corStatus = a});
