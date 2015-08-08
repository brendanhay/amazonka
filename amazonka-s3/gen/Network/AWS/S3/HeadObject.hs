{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.HeadObject
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The HEAD operation retrieves metadata from an object without returning
-- the object itself. This operation is useful if you\'re only interested
-- in an object\'s metadata. To use HEAD, you must have READ access to the
-- object.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/HeadObject.html AWS API Reference> for HeadObject.
module Network.AWS.S3.HeadObject
    (
    -- * Creating a Request
      HeadObject
    , headObject
    -- * Request Lenses
    , hoIfMatch
    , hoVersionId
    , hoSSECustomerAlgorithm
    , hoSSECustomerKey
    , hoRequestPayer
    , hoIfModifiedSince
    , hoRange
    , hoIfUnmodifiedSince
    , hoSSECustomerKeyMD5
    , hoIfNoneMatch
    , hoBucket
    , hoKey

    -- * Destructuring the Response
    , HeadObjectResponse
    , headObjectResponse
    -- * Response Lenses
    , horsVersionId
    , horsETag
    , horsRequestCharged
    , horsContentLength
    , horsRestore
    , horsExpires
    , horsDeleteMarker
    , horsExpiration
    , horsSSECustomerAlgorithm
    , horsMissingMeta
    , horsWebsiteRedirectLocation
    , horsAcceptRanges
    , horsStorageClass
    , horsContentEncoding
    , horsSSEKMSKeyId
    , horsSSECustomerKeyMD5
    , horsMetadata
    , horsReplicationStatus
    , horsCacheControl
    , horsContentLanguage
    , horsLastModified
    , horsContentDisposition
    , horsServerSideEncryption
    , horsContentType
    , horsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'headObject' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hoIfMatch'
--
-- * 'hoVersionId'
--
-- * 'hoSSECustomerAlgorithm'
--
-- * 'hoSSECustomerKey'
--
-- * 'hoRequestPayer'
--
-- * 'hoIfModifiedSince'
--
-- * 'hoRange'
--
-- * 'hoIfUnmodifiedSince'
--
-- * 'hoSSECustomerKeyMD5'
--
-- * 'hoIfNoneMatch'
--
-- * 'hoBucket'
--
-- * 'hoKey'
data HeadObject = HeadObject'
    { _hoIfMatch              :: !(Maybe Text)
    , _hoVersionId            :: !(Maybe ObjectVersionId)
    , _hoSSECustomerAlgorithm :: !(Maybe Text)
    , _hoSSECustomerKey       :: !(Maybe (Sensitive Text))
    , _hoRequestPayer         :: !(Maybe RequestPayer)
    , _hoIfModifiedSince      :: !(Maybe RFC822)
    , _hoRange                :: !(Maybe Text)
    , _hoIfUnmodifiedSince    :: !(Maybe RFC822)
    , _hoSSECustomerKeyMD5    :: !(Maybe Text)
    , _hoIfNoneMatch          :: !(Maybe Text)
    , _hoBucket               :: !BucketName
    , _hoKey                  :: !ObjectKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'HeadObject' smart constructor.
headObject :: BucketName -> ObjectKey -> HeadObject
headObject pBucket_ pKey_ =
    HeadObject'
    { _hoIfMatch = Nothing
    , _hoVersionId = Nothing
    , _hoSSECustomerAlgorithm = Nothing
    , _hoSSECustomerKey = Nothing
    , _hoRequestPayer = Nothing
    , _hoIfModifiedSince = Nothing
    , _hoRange = Nothing
    , _hoIfUnmodifiedSince = Nothing
    , _hoSSECustomerKeyMD5 = Nothing
    , _hoIfNoneMatch = Nothing
    , _hoBucket = pBucket_
    , _hoKey = pKey_
    }

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
hoIfMatch :: Lens' HeadObject (Maybe Text)
hoIfMatch = lens _hoIfMatch (\ s a -> s{_hoIfMatch = a});

-- | VersionId used to reference a specific version of the object.
hoVersionId :: Lens' HeadObject (Maybe ObjectVersionId)
hoVersionId = lens _hoVersionId (\ s a -> s{_hoVersionId = a});

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
hoSSECustomerAlgorithm :: Lens' HeadObject (Maybe Text)
hoSSECustomerAlgorithm = lens _hoSSECustomerAlgorithm (\ s a -> s{_hoSSECustomerAlgorithm = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header.
hoSSECustomerKey :: Lens' HeadObject (Maybe Text)
hoSSECustomerKey = lens _hoSSECustomerKey (\ s a -> s{_hoSSECustomerKey = a}) . mapping _Sensitive;

-- | Undocumented member.
hoRequestPayer :: Lens' HeadObject (Maybe RequestPayer)
hoRequestPayer = lens _hoRequestPayer (\ s a -> s{_hoRequestPayer = a});

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
hoIfModifiedSince :: Lens' HeadObject (Maybe UTCTime)
hoIfModifiedSince = lens _hoIfModifiedSince (\ s a -> s{_hoIfModifiedSince = a}) . mapping _Time;

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http:\/\/www.w3.org\/Protocols\/rfc2616\/rfc2616-sec14.html#sec14.35.
hoRange :: Lens' HeadObject (Maybe Text)
hoRange = lens _hoRange (\ s a -> s{_hoRange = a});

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
hoIfUnmodifiedSince :: Lens' HeadObject (Maybe UTCTime)
hoIfUnmodifiedSince = lens _hoIfUnmodifiedSince (\ s a -> s{_hoIfUnmodifiedSince = a}) . mapping _Time;

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
hoSSECustomerKeyMD5 :: Lens' HeadObject (Maybe Text)
hoSSECustomerKeyMD5 = lens _hoSSECustomerKeyMD5 (\ s a -> s{_hoSSECustomerKeyMD5 = a});

-- | Return the object only if its entity tag (ETag) is different from the
-- one specified, otherwise return a 304 (not modified).
hoIfNoneMatch :: Lens' HeadObject (Maybe Text)
hoIfNoneMatch = lens _hoIfNoneMatch (\ s a -> s{_hoIfNoneMatch = a});

-- | Undocumented member.
hoBucket :: Lens' HeadObject BucketName
hoBucket = lens _hoBucket (\ s a -> s{_hoBucket = a});

-- | Undocumented member.
hoKey :: Lens' HeadObject ObjectKey
hoKey = lens _hoKey (\ s a -> s{_hoKey = a});

instance AWSRequest HeadObject where
        type Sv HeadObject = S3
        type Rs HeadObject = HeadObjectResponse
        request = head'
        response
          = receiveEmpty
              (\ s h x ->
                 HeadObjectResponse' <$>
                   (h .#? "x-amz-version-id") <*> (h .#? "ETag") <*>
                     (h .#? "x-amz-request-charged")
                     <*> (h .#? "Content-Length")
                     <*> (h .#? "x-amz-restore")
                     <*> (h .#? "Expires")
                     <*> (h .#? "x-amz-delete-marker")
                     <*> (h .#? "x-amz-expiration")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-algorithm")
                     <*> (h .#? "x-amz-missing-meta")
                     <*> (h .#? "x-amz-website-redirect-location")
                     <*> (h .#? "accept-ranges")
                     <*> (h .#? "x-amz-storage-class")
                     <*> (h .#? "Content-Encoding")
                     <*>
                     (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-key-MD5")
                     <*> (parseHeadersMap "x-amz-meta-" h)
                     <*> (h .#? "x-amz-replication-status")
                     <*> (h .#? "Cache-Control")
                     <*> (h .#? "Content-Language")
                     <*> (h .#? "Last-Modified")
                     <*> (h .#? "Content-Disposition")
                     <*> (h .#? "x-amz-server-side-encryption")
                     <*> (h .#? "Content-Type")
                     <*> (pure (fromEnum s)))

instance ToHeaders HeadObject where
        toHeaders HeadObject'{..}
          = mconcat
              ["If-Match" =# _hoIfMatch,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _hoSSECustomerAlgorithm,
               "x-amz-server-side-encryption-customer-key" =#
                 _hoSSECustomerKey,
               "x-amz-request-payer" =# _hoRequestPayer,
               "If-Modified-Since" =# _hoIfModifiedSince,
               "Range" =# _hoRange,
               "If-Unmodified-Since" =# _hoIfUnmodifiedSince,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _hoSSECustomerKeyMD5,
               "If-None-Match" =# _hoIfNoneMatch]

instance ToPath HeadObject where
        toPath HeadObject'{..}
          = mconcat ["/", toBS _hoBucket, "/", toBS _hoKey]

instance ToQuery HeadObject where
        toQuery HeadObject'{..}
          = mconcat ["versionId" =: _hoVersionId]

-- | /See:/ 'headObjectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'horsVersionId'
--
-- * 'horsETag'
--
-- * 'horsRequestCharged'
--
-- * 'horsContentLength'
--
-- * 'horsRestore'
--
-- * 'horsExpires'
--
-- * 'horsDeleteMarker'
--
-- * 'horsExpiration'
--
-- * 'horsSSECustomerAlgorithm'
--
-- * 'horsMissingMeta'
--
-- * 'horsWebsiteRedirectLocation'
--
-- * 'horsAcceptRanges'
--
-- * 'horsStorageClass'
--
-- * 'horsContentEncoding'
--
-- * 'horsSSEKMSKeyId'
--
-- * 'horsSSECustomerKeyMD5'
--
-- * 'horsMetadata'
--
-- * 'horsReplicationStatus'
--
-- * 'horsCacheControl'
--
-- * 'horsContentLanguage'
--
-- * 'horsLastModified'
--
-- * 'horsContentDisposition'
--
-- * 'horsServerSideEncryption'
--
-- * 'horsContentType'
--
-- * 'horsStatus'
data HeadObjectResponse = HeadObjectResponse'
    { _horsVersionId               :: !(Maybe ObjectVersionId)
    , _horsETag                    :: !(Maybe ETag)
    , _horsRequestCharged          :: !(Maybe RequestCharged)
    , _horsContentLength           :: !(Maybe Int)
    , _horsRestore                 :: !(Maybe Text)
    , _horsExpires                 :: !(Maybe RFC822)
    , _horsDeleteMarker            :: !(Maybe Bool)
    , _horsExpiration              :: !(Maybe Text)
    , _horsSSECustomerAlgorithm    :: !(Maybe Text)
    , _horsMissingMeta             :: !(Maybe Int)
    , _horsWebsiteRedirectLocation :: !(Maybe Text)
    , _horsAcceptRanges            :: !(Maybe Text)
    , _horsStorageClass            :: !(Maybe StorageClass)
    , _horsContentEncoding         :: !(Maybe Text)
    , _horsSSEKMSKeyId             :: !(Maybe (Sensitive Text))
    , _horsSSECustomerKeyMD5       :: !(Maybe Text)
    , _horsMetadata                :: !(Map Text Text)
    , _horsReplicationStatus       :: !(Maybe ReplicationStatus)
    , _horsCacheControl            :: !(Maybe Text)
    , _horsContentLanguage         :: !(Maybe Text)
    , _horsLastModified            :: !(Maybe RFC822)
    , _horsContentDisposition      :: !(Maybe Text)
    , _horsServerSideEncryption    :: !(Maybe ServerSideEncryption)
    , _horsContentType             :: !(Maybe Text)
    , _horsStatus                  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'HeadObjectResponse' smart constructor.
headObjectResponse :: Int -> HeadObjectResponse
headObjectResponse pStatus_ =
    HeadObjectResponse'
    { _horsVersionId = Nothing
    , _horsETag = Nothing
    , _horsRequestCharged = Nothing
    , _horsContentLength = Nothing
    , _horsRestore = Nothing
    , _horsExpires = Nothing
    , _horsDeleteMarker = Nothing
    , _horsExpiration = Nothing
    , _horsSSECustomerAlgorithm = Nothing
    , _horsMissingMeta = Nothing
    , _horsWebsiteRedirectLocation = Nothing
    , _horsAcceptRanges = Nothing
    , _horsStorageClass = Nothing
    , _horsContentEncoding = Nothing
    , _horsSSEKMSKeyId = Nothing
    , _horsSSECustomerKeyMD5 = Nothing
    , _horsMetadata = mempty
    , _horsReplicationStatus = Nothing
    , _horsCacheControl = Nothing
    , _horsContentLanguage = Nothing
    , _horsLastModified = Nothing
    , _horsContentDisposition = Nothing
    , _horsServerSideEncryption = Nothing
    , _horsContentType = Nothing
    , _horsStatus = pStatus_
    }

-- | Version of the object.
horsVersionId :: Lens' HeadObjectResponse (Maybe ObjectVersionId)
horsVersionId = lens _horsVersionId (\ s a -> s{_horsVersionId = a});

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL
horsETag :: Lens' HeadObjectResponse (Maybe ETag)
horsETag = lens _horsETag (\ s a -> s{_horsETag = a});

-- | Undocumented member.
horsRequestCharged :: Lens' HeadObjectResponse (Maybe RequestCharged)
horsRequestCharged = lens _horsRequestCharged (\ s a -> s{_horsRequestCharged = a});

-- | Size of the body in bytes.
horsContentLength :: Lens' HeadObjectResponse (Maybe Int)
horsContentLength = lens _horsContentLength (\ s a -> s{_horsContentLength = a});

-- | Provides information about object restoration operation and expiration
-- time of the restored object copy.
horsRestore :: Lens' HeadObjectResponse (Maybe Text)
horsRestore = lens _horsRestore (\ s a -> s{_horsRestore = a});

-- | The date and time at which the object is no longer cacheable.
horsExpires :: Lens' HeadObjectResponse (Maybe UTCTime)
horsExpires = lens _horsExpires (\ s a -> s{_horsExpires = a}) . mapping _Time;

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
horsDeleteMarker :: Lens' HeadObjectResponse (Maybe Bool)
horsDeleteMarker = lens _horsDeleteMarker (\ s a -> s{_horsDeleteMarker = a});

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id
-- key value pairs providing object expiration information. The value of
-- the rule-id is URL encoded.
horsExpiration :: Lens' HeadObjectResponse (Maybe Text)
horsExpiration = lens _horsExpiration (\ s a -> s{_horsExpiration = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
horsSSECustomerAlgorithm :: Lens' HeadObjectResponse (Maybe Text)
horsSSECustomerAlgorithm = lens _horsSSECustomerAlgorithm (\ s a -> s{_horsSSECustomerAlgorithm = a});

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP
-- that supports more flexible metadata than the REST API. For example,
-- using SOAP, you can create metadata whose values are not legal HTTP
-- headers.
horsMissingMeta :: Lens' HeadObjectResponse (Maybe Int)
horsMissingMeta = lens _horsMissingMeta (\ s a -> s{_horsMissingMeta = a});

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
horsWebsiteRedirectLocation :: Lens' HeadObjectResponse (Maybe Text)
horsWebsiteRedirectLocation = lens _horsWebsiteRedirectLocation (\ s a -> s{_horsWebsiteRedirectLocation = a});

-- | Undocumented member.
horsAcceptRanges :: Lens' HeadObjectResponse (Maybe Text)
horsAcceptRanges = lens _horsAcceptRanges (\ s a -> s{_horsAcceptRanges = a});

-- | Undocumented member.
horsStorageClass :: Lens' HeadObjectResponse (Maybe StorageClass)
horsStorageClass = lens _horsStorageClass (\ s a -> s{_horsStorageClass = a});

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
horsContentEncoding :: Lens' HeadObjectResponse (Maybe Text)
horsContentEncoding = lens _horsContentEncoding (\ s a -> s{_horsContentEncoding = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
horsSSEKMSKeyId :: Lens' HeadObjectResponse (Maybe Text)
horsSSEKMSKeyId = lens _horsSSEKMSKeyId (\ s a -> s{_horsSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
horsSSECustomerKeyMD5 :: Lens' HeadObjectResponse (Maybe Text)
horsSSECustomerKeyMD5 = lens _horsSSECustomerKeyMD5 (\ s a -> s{_horsSSECustomerKeyMD5 = a});

-- | A map of metadata to store with the object in S3.
horsMetadata :: Lens' HeadObjectResponse (HashMap Text Text)
horsMetadata = lens _horsMetadata (\ s a -> s{_horsMetadata = a}) . _Map;

-- | Undocumented member.
horsReplicationStatus :: Lens' HeadObjectResponse (Maybe ReplicationStatus)
horsReplicationStatus = lens _horsReplicationStatus (\ s a -> s{_horsReplicationStatus = a});

-- | Specifies caching behavior along the request\/reply chain.
horsCacheControl :: Lens' HeadObjectResponse (Maybe Text)
horsCacheControl = lens _horsCacheControl (\ s a -> s{_horsCacheControl = a});

-- | The language the content is in.
horsContentLanguage :: Lens' HeadObjectResponse (Maybe Text)
horsContentLanguage = lens _horsContentLanguage (\ s a -> s{_horsContentLanguage = a});

-- | Last modified date of the object
horsLastModified :: Lens' HeadObjectResponse (Maybe UTCTime)
horsLastModified = lens _horsLastModified (\ s a -> s{_horsLastModified = a}) . mapping _Time;

-- | Specifies presentational information for the object.
horsContentDisposition :: Lens' HeadObjectResponse (Maybe Text)
horsContentDisposition = lens _horsContentDisposition (\ s a -> s{_horsContentDisposition = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
horsServerSideEncryption :: Lens' HeadObjectResponse (Maybe ServerSideEncryption)
horsServerSideEncryption = lens _horsServerSideEncryption (\ s a -> s{_horsServerSideEncryption = a});

-- | A standard MIME type describing the format of the object data.
horsContentType :: Lens' HeadObjectResponse (Maybe Text)
horsContentType = lens _horsContentType (\ s a -> s{_horsContentType = a});

-- | Undocumented member.
horsStatus :: Lens' HeadObjectResponse Int
horsStatus = lens _horsStatus (\ s a -> s{_horsStatus = a});
