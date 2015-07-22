{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObject
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves objects from Amazon S3.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetObject.html>
module Network.AWS.S3.GetObject
    (
    -- * Request
      GetObject
    -- ** Request constructor
    , getObject
    -- ** Request lenses
    , gorqIfMatch
    , gorqVersionId
    , gorqResponseContentType
    , gorqResponseContentDisposition
    , gorqResponseContentLanguage
    , gorqSSECustomerAlgorithm
    , gorqSSECustomerKey
    , gorqRequestPayer
    , gorqResponseContentEncoding
    , gorqIfModifiedSince
    , gorqRange
    , gorqIfUnmodifiedSince
    , gorqSSECustomerKeyMD5
    , gorqResponseCacheControl
    , gorqResponseExpires
    , gorqIfNoneMatch
    , gorqBucket
    , gorqKey

    -- * Response
    , GetObjectResponse
    -- ** Response constructor
    , getObjectResponse
    -- ** Response lenses
    , gorsVersionId
    , gorsETag
    , gorsRequestCharged
    , gorsContentLength
    , gorsRestore
    , gorsExpires
    , gorsDeleteMarker
    , gorsExpiration
    , gorsSSECustomerAlgorithm
    , gorsMissingMeta
    , gorsWebsiteRedirectLocation
    , gorsAcceptRanges
    , gorsContentEncoding
    , gorsSSEKMSKeyId
    , gorsSSECustomerKeyMD5
    , gorsMetadata
    , gorsReplicationStatus
    , gorsCacheControl
    , gorsContentLanguage
    , gorsLastModified
    , gorsContentDisposition
    , gorsServerSideEncryption
    , gorsContentType
    , gorsStatus
    , gorsBody
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getObject' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gorqIfMatch'
--
-- * 'gorqVersionId'
--
-- * 'gorqResponseContentType'
--
-- * 'gorqResponseContentDisposition'
--
-- * 'gorqResponseContentLanguage'
--
-- * 'gorqSSECustomerAlgorithm'
--
-- * 'gorqSSECustomerKey'
--
-- * 'gorqRequestPayer'
--
-- * 'gorqResponseContentEncoding'
--
-- * 'gorqIfModifiedSince'
--
-- * 'gorqRange'
--
-- * 'gorqIfUnmodifiedSince'
--
-- * 'gorqSSECustomerKeyMD5'
--
-- * 'gorqResponseCacheControl'
--
-- * 'gorqResponseExpires'
--
-- * 'gorqIfNoneMatch'
--
-- * 'gorqBucket'
--
-- * 'gorqKey'
data GetObject = GetObject'
    { _gorqIfMatch                    :: !(Maybe Text)
    , _gorqVersionId                  :: !(Maybe ObjectVersionId)
    , _gorqResponseContentType        :: !(Maybe Text)
    , _gorqResponseContentDisposition :: !(Maybe Text)
    , _gorqResponseContentLanguage    :: !(Maybe Text)
    , _gorqSSECustomerAlgorithm       :: !(Maybe Text)
    , _gorqSSECustomerKey             :: !(Maybe (Sensitive Text))
    , _gorqRequestPayer               :: !(Maybe RequestPayer)
    , _gorqResponseContentEncoding    :: !(Maybe Text)
    , _gorqIfModifiedSince            :: !(Maybe RFC822)
    , _gorqRange                      :: !(Maybe Text)
    , _gorqIfUnmodifiedSince          :: !(Maybe RFC822)
    , _gorqSSECustomerKeyMD5          :: !(Maybe Text)
    , _gorqResponseCacheControl       :: !(Maybe Text)
    , _gorqResponseExpires            :: !(Maybe RFC822)
    , _gorqIfNoneMatch                :: !(Maybe Text)
    , _gorqBucket                     :: !BucketName
    , _gorqKey                        :: !ObjectKey
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetObject' smart constructor.
getObject :: BucketName -> ObjectKey -> GetObject
getObject pBucket pKey =
    GetObject'
    { _gorqIfMatch = Nothing
    , _gorqVersionId = Nothing
    , _gorqResponseContentType = Nothing
    , _gorqResponseContentDisposition = Nothing
    , _gorqResponseContentLanguage = Nothing
    , _gorqSSECustomerAlgorithm = Nothing
    , _gorqSSECustomerKey = Nothing
    , _gorqRequestPayer = Nothing
    , _gorqResponseContentEncoding = Nothing
    , _gorqIfModifiedSince = Nothing
    , _gorqRange = Nothing
    , _gorqIfUnmodifiedSince = Nothing
    , _gorqSSECustomerKeyMD5 = Nothing
    , _gorqResponseCacheControl = Nothing
    , _gorqResponseExpires = Nothing
    , _gorqIfNoneMatch = Nothing
    , _gorqBucket = pBucket
    , _gorqKey = pKey
    }

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
gorqIfMatch :: Lens' GetObject (Maybe Text)
gorqIfMatch = lens _gorqIfMatch (\ s a -> s{_gorqIfMatch = a});

-- | VersionId used to reference a specific version of the object.
gorqVersionId :: Lens' GetObject (Maybe ObjectVersionId)
gorqVersionId = lens _gorqVersionId (\ s a -> s{_gorqVersionId = a});

-- | Sets the Content-Type header of the response.
gorqResponseContentType :: Lens' GetObject (Maybe Text)
gorqResponseContentType = lens _gorqResponseContentType (\ s a -> s{_gorqResponseContentType = a});

-- | Sets the Content-Disposition header of the response
gorqResponseContentDisposition :: Lens' GetObject (Maybe Text)
gorqResponseContentDisposition = lens _gorqResponseContentDisposition (\ s a -> s{_gorqResponseContentDisposition = a});

-- | Sets the Content-Language header of the response.
gorqResponseContentLanguage :: Lens' GetObject (Maybe Text)
gorqResponseContentLanguage = lens _gorqResponseContentLanguage (\ s a -> s{_gorqResponseContentLanguage = a});

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
gorqSSECustomerAlgorithm :: Lens' GetObject (Maybe Text)
gorqSSECustomerAlgorithm = lens _gorqSSECustomerAlgorithm (\ s a -> s{_gorqSSECustomerAlgorithm = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header.
gorqSSECustomerKey :: Lens' GetObject (Maybe Text)
gorqSSECustomerKey = lens _gorqSSECustomerKey (\ s a -> s{_gorqSSECustomerKey = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
gorqRequestPayer :: Lens' GetObject (Maybe RequestPayer)
gorqRequestPayer = lens _gorqRequestPayer (\ s a -> s{_gorqRequestPayer = a});

-- | Sets the Content-Encoding header of the response.
gorqResponseContentEncoding :: Lens' GetObject (Maybe Text)
gorqResponseContentEncoding = lens _gorqResponseContentEncoding (\ s a -> s{_gorqResponseContentEncoding = a});

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
gorqIfModifiedSince :: Lens' GetObject (Maybe UTCTime)
gorqIfModifiedSince = lens _gorqIfModifiedSince (\ s a -> s{_gorqIfModifiedSince = a}) . mapping _Time;

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http:\/\/www.w3.org\/Protocols\/rfc2616\/rfc2616-sec14.html#sec14.35.
gorqRange :: Lens' GetObject (Maybe Text)
gorqRange = lens _gorqRange (\ s a -> s{_gorqRange = a});

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
gorqIfUnmodifiedSince :: Lens' GetObject (Maybe UTCTime)
gorqIfUnmodifiedSince = lens _gorqIfUnmodifiedSince (\ s a -> s{_gorqIfUnmodifiedSince = a}) . mapping _Time;

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
gorqSSECustomerKeyMD5 :: Lens' GetObject (Maybe Text)
gorqSSECustomerKeyMD5 = lens _gorqSSECustomerKeyMD5 (\ s a -> s{_gorqSSECustomerKeyMD5 = a});

-- | Sets the Cache-Control header of the response.
gorqResponseCacheControl :: Lens' GetObject (Maybe Text)
gorqResponseCacheControl = lens _gorqResponseCacheControl (\ s a -> s{_gorqResponseCacheControl = a});

-- | Sets the Expires header of the response.
gorqResponseExpires :: Lens' GetObject (Maybe UTCTime)
gorqResponseExpires = lens _gorqResponseExpires (\ s a -> s{_gorqResponseExpires = a}) . mapping _Time;

-- | Return the object only if its entity tag (ETag) is different from the
-- one specified, otherwise return a 304 (not modified).
gorqIfNoneMatch :: Lens' GetObject (Maybe Text)
gorqIfNoneMatch = lens _gorqIfNoneMatch (\ s a -> s{_gorqIfNoneMatch = a});

-- | FIXME: Undocumented member.
gorqBucket :: Lens' GetObject BucketName
gorqBucket = lens _gorqBucket (\ s a -> s{_gorqBucket = a});

-- | FIXME: Undocumented member.
gorqKey :: Lens' GetObject ObjectKey
gorqKey = lens _gorqKey (\ s a -> s{_gorqKey = a});

instance AWSRequest GetObject where
        type Sv GetObject = S3
        type Rs GetObject = GetObjectResponse
        request = get
        response
          = receiveBody
              (\ s h x ->
                 GetObjectResponse' <$>
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
                     <*> (pure (fromEnum s))
                     <*> (pure x))

instance ToHeaders GetObject where
        toHeaders GetObject'{..}
          = mconcat
              ["If-Match" =# _gorqIfMatch,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _gorqSSECustomerAlgorithm,
               "x-amz-server-side-encryption-customer-key" =#
                 _gorqSSECustomerKey,
               "x-amz-request-payer" =# _gorqRequestPayer,
               "If-Modified-Since" =# _gorqIfModifiedSince,
               "Range" =# _gorqRange,
               "If-Unmodified-Since" =# _gorqIfUnmodifiedSince,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _gorqSSECustomerKeyMD5,
               "If-None-Match" =# _gorqIfNoneMatch]

instance ToPath GetObject where
        toPath GetObject'{..}
          = mconcat
              ["/", toText _gorqBucket, "/", toText _gorqKey]

instance ToQuery GetObject where
        toQuery GetObject'{..}
          = mconcat
              ["versionId" =: _gorqVersionId,
               "response-content-type" =: _gorqResponseContentType,
               "response-content-disposition" =:
                 _gorqResponseContentDisposition,
               "response-content-language" =:
                 _gorqResponseContentLanguage,
               "response-content-encoding" =:
                 _gorqResponseContentEncoding,
               "response-cache-control" =:
                 _gorqResponseCacheControl,
               "response-expires" =: _gorqResponseExpires]

-- | /See:/ 'getObjectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gorsVersionId'
--
-- * 'gorsETag'
--
-- * 'gorsRequestCharged'
--
-- * 'gorsContentLength'
--
-- * 'gorsRestore'
--
-- * 'gorsExpires'
--
-- * 'gorsDeleteMarker'
--
-- * 'gorsExpiration'
--
-- * 'gorsSSECustomerAlgorithm'
--
-- * 'gorsMissingMeta'
--
-- * 'gorsWebsiteRedirectLocation'
--
-- * 'gorsAcceptRanges'
--
-- * 'gorsContentEncoding'
--
-- * 'gorsSSEKMSKeyId'
--
-- * 'gorsSSECustomerKeyMD5'
--
-- * 'gorsMetadata'
--
-- * 'gorsReplicationStatus'
--
-- * 'gorsCacheControl'
--
-- * 'gorsContentLanguage'
--
-- * 'gorsLastModified'
--
-- * 'gorsContentDisposition'
--
-- * 'gorsServerSideEncryption'
--
-- * 'gorsContentType'
--
-- * 'gorsStatus'
--
-- * 'gorsBody'
data GetObjectResponse = GetObjectResponse'
    { _gorsVersionId               :: !(Maybe ObjectVersionId)
    , _gorsETag                    :: !(Maybe ETag)
    , _gorsRequestCharged          :: !(Maybe RequestCharged)
    , _gorsContentLength           :: !(Maybe Int)
    , _gorsRestore                 :: !(Maybe Text)
    , _gorsExpires                 :: !(Maybe RFC822)
    , _gorsDeleteMarker            :: !(Maybe Bool)
    , _gorsExpiration              :: !(Maybe Text)
    , _gorsSSECustomerAlgorithm    :: !(Maybe Text)
    , _gorsMissingMeta             :: !(Maybe Int)
    , _gorsWebsiteRedirectLocation :: !(Maybe Text)
    , _gorsAcceptRanges            :: !(Maybe Text)
    , _gorsContentEncoding         :: !(Maybe Text)
    , _gorsSSEKMSKeyId             :: !(Maybe (Sensitive Text))
    , _gorsSSECustomerKeyMD5       :: !(Maybe Text)
    , _gorsMetadata                :: !(Map Text Text)
    , _gorsReplicationStatus       :: !(Maybe ReplicationStatus)
    , _gorsCacheControl            :: !(Maybe Text)
    , _gorsContentLanguage         :: !(Maybe Text)
    , _gorsLastModified            :: !(Maybe RFC822)
    , _gorsContentDisposition      :: !(Maybe Text)
    , _gorsServerSideEncryption    :: !(Maybe ServerSideEncryption)
    , _gorsContentType             :: !(Maybe Text)
    , _gorsStatus                  :: !Int
    , _gorsBody                    :: !RsBody
    } deriving (Show,Generic)

-- | 'GetObjectResponse' smart constructor.
getObjectResponse :: Int -> RsBody -> GetObjectResponse
getObjectResponse pStatus pBody =
    GetObjectResponse'
    { _gorsVersionId = Nothing
    , _gorsETag = Nothing
    , _gorsRequestCharged = Nothing
    , _gorsContentLength = Nothing
    , _gorsRestore = Nothing
    , _gorsExpires = Nothing
    , _gorsDeleteMarker = Nothing
    , _gorsExpiration = Nothing
    , _gorsSSECustomerAlgorithm = Nothing
    , _gorsMissingMeta = Nothing
    , _gorsWebsiteRedirectLocation = Nothing
    , _gorsAcceptRanges = Nothing
    , _gorsContentEncoding = Nothing
    , _gorsSSEKMSKeyId = Nothing
    , _gorsSSECustomerKeyMD5 = Nothing
    , _gorsMetadata = mempty
    , _gorsReplicationStatus = Nothing
    , _gorsCacheControl = Nothing
    , _gorsContentLanguage = Nothing
    , _gorsLastModified = Nothing
    , _gorsContentDisposition = Nothing
    , _gorsServerSideEncryption = Nothing
    , _gorsContentType = Nothing
    , _gorsStatus = pStatus
    , _gorsBody = pBody
    }

-- | Version of the object.
gorsVersionId :: Lens' GetObjectResponse (Maybe ObjectVersionId)
gorsVersionId = lens _gorsVersionId (\ s a -> s{_gorsVersionId = a});

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL
gorsETag :: Lens' GetObjectResponse (Maybe ETag)
gorsETag = lens _gorsETag (\ s a -> s{_gorsETag = a});

-- | FIXME: Undocumented member.
gorsRequestCharged :: Lens' GetObjectResponse (Maybe RequestCharged)
gorsRequestCharged = lens _gorsRequestCharged (\ s a -> s{_gorsRequestCharged = a});

-- | Size of the body in bytes.
gorsContentLength :: Lens' GetObjectResponse (Maybe Int)
gorsContentLength = lens _gorsContentLength (\ s a -> s{_gorsContentLength = a});

-- | Provides information about object restoration operation and expiration
-- time of the restored object copy.
gorsRestore :: Lens' GetObjectResponse (Maybe Text)
gorsRestore = lens _gorsRestore (\ s a -> s{_gorsRestore = a});

-- | The date and time at which the object is no longer cacheable.
gorsExpires :: Lens' GetObjectResponse (Maybe UTCTime)
gorsExpires = lens _gorsExpires (\ s a -> s{_gorsExpires = a}) . mapping _Time;

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
gorsDeleteMarker :: Lens' GetObjectResponse (Maybe Bool)
gorsDeleteMarker = lens _gorsDeleteMarker (\ s a -> s{_gorsDeleteMarker = a});

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id
-- key value pairs providing object expiration information. The value of
-- the rule-id is URL encoded.
gorsExpiration :: Lens' GetObjectResponse (Maybe Text)
gorsExpiration = lens _gorsExpiration (\ s a -> s{_gorsExpiration = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
gorsSSECustomerAlgorithm :: Lens' GetObjectResponse (Maybe Text)
gorsSSECustomerAlgorithm = lens _gorsSSECustomerAlgorithm (\ s a -> s{_gorsSSECustomerAlgorithm = a});

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP
-- that supports more flexible metadata than the REST API. For example,
-- using SOAP, you can create metadata whose values are not legal HTTP
-- headers.
gorsMissingMeta :: Lens' GetObjectResponse (Maybe Int)
gorsMissingMeta = lens _gorsMissingMeta (\ s a -> s{_gorsMissingMeta = a});

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
gorsWebsiteRedirectLocation :: Lens' GetObjectResponse (Maybe Text)
gorsWebsiteRedirectLocation = lens _gorsWebsiteRedirectLocation (\ s a -> s{_gorsWebsiteRedirectLocation = a});

-- | FIXME: Undocumented member.
gorsAcceptRanges :: Lens' GetObjectResponse (Maybe Text)
gorsAcceptRanges = lens _gorsAcceptRanges (\ s a -> s{_gorsAcceptRanges = a});

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
gorsContentEncoding :: Lens' GetObjectResponse (Maybe Text)
gorsContentEncoding = lens _gorsContentEncoding (\ s a -> s{_gorsContentEncoding = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
gorsSSEKMSKeyId :: Lens' GetObjectResponse (Maybe Text)
gorsSSEKMSKeyId = lens _gorsSSEKMSKeyId (\ s a -> s{_gorsSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
gorsSSECustomerKeyMD5 :: Lens' GetObjectResponse (Maybe Text)
gorsSSECustomerKeyMD5 = lens _gorsSSECustomerKeyMD5 (\ s a -> s{_gorsSSECustomerKeyMD5 = a});

-- | A map of metadata to store with the object in S3.
gorsMetadata :: Lens' GetObjectResponse (HashMap Text Text)
gorsMetadata = lens _gorsMetadata (\ s a -> s{_gorsMetadata = a}) . _Map;

-- | FIXME: Undocumented member.
gorsReplicationStatus :: Lens' GetObjectResponse (Maybe ReplicationStatus)
gorsReplicationStatus = lens _gorsReplicationStatus (\ s a -> s{_gorsReplicationStatus = a});

-- | Specifies caching behavior along the request\/reply chain.
gorsCacheControl :: Lens' GetObjectResponse (Maybe Text)
gorsCacheControl = lens _gorsCacheControl (\ s a -> s{_gorsCacheControl = a});

-- | The language the content is in.
gorsContentLanguage :: Lens' GetObjectResponse (Maybe Text)
gorsContentLanguage = lens _gorsContentLanguage (\ s a -> s{_gorsContentLanguage = a});

-- | Last modified date of the object
gorsLastModified :: Lens' GetObjectResponse (Maybe UTCTime)
gorsLastModified = lens _gorsLastModified (\ s a -> s{_gorsLastModified = a}) . mapping _Time;

-- | Specifies presentational information for the object.
gorsContentDisposition :: Lens' GetObjectResponse (Maybe Text)
gorsContentDisposition = lens _gorsContentDisposition (\ s a -> s{_gorsContentDisposition = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
gorsServerSideEncryption :: Lens' GetObjectResponse (Maybe ServerSideEncryption)
gorsServerSideEncryption = lens _gorsServerSideEncryption (\ s a -> s{_gorsServerSideEncryption = a});

-- | A standard MIME type describing the format of the object data.
gorsContentType :: Lens' GetObjectResponse (Maybe Text)
gorsContentType = lens _gorsContentType (\ s a -> s{_gorsContentType = a});

-- | FIXME: Undocumented member.
gorsStatus :: Lens' GetObjectResponse Int
gorsStatus = lens _gorsStatus (\ s a -> s{_gorsStatus = a});

-- | Object data.
gorsBody :: Lens' GetObjectResponse RsBody
gorsBody = lens _gorsBody (\ s a -> s{_gorsBody = a});
