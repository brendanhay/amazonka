{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.S3.HeadObject
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

-- | The HEAD operation retrieves metadata from an object without returning
-- the object itself. This operation is useful if you\'re only interested
-- in an object\'s metadata. To use HEAD, you must have READ access to the
-- object.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/HeadObject.html>
module Network.AWS.S3.HeadObject
    (
    -- * Request
      HeadObject
    -- ** Request constructor
    , headObject
    -- ** Request lenses
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

    -- * Response
    , HeadObjectResponse
    -- ** Response constructor
    , headObjectResponse
    -- ** Response lenses
    , horVersionId
    , horETag
    , horRequestCharged
    , horContentLength
    , horRestore
    , horExpires
    , horDeleteMarker
    , horExpiration
    , horSSECustomerAlgorithm
    , horMissingMeta
    , horWebsiteRedirectLocation
    , horAcceptRanges
    , horContentEncoding
    , horSSEKMSKeyId
    , horSSECustomerKeyMD5
    , horMetadata
    , horReplicationStatus
    , horCacheControl
    , horContentLanguage
    , horLastModified
    , horContentDisposition
    , horServerSideEncryption
    , horContentType
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.S3.Types

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
data HeadObject = HeadObject'{_hoIfMatch :: Maybe Text, _hoVersionId :: Maybe ObjectVersionId, _hoSSECustomerAlgorithm :: Maybe Text, _hoSSECustomerKey :: Maybe (Sensitive Text), _hoRequestPayer :: Maybe RequestPayer, _hoIfModifiedSince :: Maybe RFC822, _hoRange :: Maybe Text, _hoIfUnmodifiedSince :: Maybe RFC822, _hoSSECustomerKeyMD5 :: Maybe Text, _hoIfNoneMatch :: Maybe Text, _hoBucket :: BucketName, _hoKey :: ObjectKey} deriving (Eq, Read, Show)

-- | 'HeadObject' smart constructor.
headObject :: BucketName -> ObjectKey -> HeadObject
headObject pBucket pKey = HeadObject'{_hoIfMatch = Nothing, _hoVersionId = Nothing, _hoSSECustomerAlgorithm = Nothing, _hoSSECustomerKey = Nothing, _hoRequestPayer = Nothing, _hoIfModifiedSince = Nothing, _hoRange = Nothing, _hoIfUnmodifiedSince = Nothing, _hoSSECustomerKeyMD5 = Nothing, _hoIfNoneMatch = Nothing, _hoBucket = pBucket, _hoKey = pKey};

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
hoIfMatch :: Lens' HeadObject (Maybe Text)
hoIfMatch = lens _hoIfMatch (\ s a -> s{_hoIfMatch = a});

-- | VersionId used to reference a specific version of the object.
hoVersionId :: Lens' HeadObject (Maybe ObjectVersionId)
hoVersionId = lens _hoVersionId (\ s a -> s{_hoVersionId = a});

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
hoSSECustomerAlgorithm :: Lens' HeadObject (Maybe Text)
hoSSECustomerAlgorithm = lens _hoSSECustomerAlgorithm (\ s a -> s{_hoSSECustomerAlgorithm = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header.
hoSSECustomerKey :: Lens' HeadObject (Maybe Text)
hoSSECustomerKey = lens _hoSSECustomerKey (\ s a -> s{_hoSSECustomerKey = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
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

-- | FIXME: Undocumented member.
hoBucket :: Lens' HeadObject BucketName
hoBucket = lens _hoBucket (\ s a -> s{_hoBucket = a});

-- | FIXME: Undocumented member.
hoKey :: Lens' HeadObject ObjectKey
hoKey = lens _hoKey (\ s a -> s{_hoKey = a});

instance AWSRequest HeadObject where
        type Sv HeadObject = S3
        type Rs HeadObject = HeadObjectResponse
        request = head'
        response
          = receiveXML
              (\ s h x ->
                 HeadObjectResponse' <$>
                   h .#? "x-amz-version-id" <*> h .#? "ETag" <*>
                     h .#? "x-amz-request-charged"
                     <*> h .#? "Content-Length"
                     <*> h .#? "x-amz-restore"
                     <*> h .#? "Expires"
                     <*> h .#? "x-amz-delete-marker"
                     <*> h .#? "x-amz-expiration"
                     <*>
                     h .#?
                       "x-amz-server-side-encryption-customer-algorithm"
                     <*> h .#? "x-amz-missing-meta"
                     <*> h .#? "x-amz-website-redirect-location"
                     <*> h .#? "accept-ranges"
                     <*> h .#? "Content-Encoding"
                     <*>
                     h .#? "x-amz-server-side-encryption-aws-kms-key-id"
                     <*>
                     h .#? "x-amz-server-side-encryption-customer-key-MD5"
                     <*> parseHeadersMap "x-amz-meta-" h
                     <*> h .#? "x-amz-replication-status"
                     <*> h .#? "Cache-Control"
                     <*> h .#? "Content-Language"
                     <*> h .#? "Last-Modified"
                     <*> h .#? "Content-Disposition"
                     <*> h .#? "x-amz-server-side-encryption"
                     <*> h .#? "Content-Type")

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
          = mconcat ["/", toText _hoBucket, "/", toText _hoKey]

instance ToQuery HeadObject where
        toQuery HeadObject'{..}
          = mconcat ["versionId" =: _hoVersionId]

-- | /See:/ 'headObjectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'horVersionId'
--
-- * 'horETag'
--
-- * 'horRequestCharged'
--
-- * 'horContentLength'
--
-- * 'horRestore'
--
-- * 'horExpires'
--
-- * 'horDeleteMarker'
--
-- * 'horExpiration'
--
-- * 'horSSECustomerAlgorithm'
--
-- * 'horMissingMeta'
--
-- * 'horWebsiteRedirectLocation'
--
-- * 'horAcceptRanges'
--
-- * 'horContentEncoding'
--
-- * 'horSSEKMSKeyId'
--
-- * 'horSSECustomerKeyMD5'
--
-- * 'horMetadata'
--
-- * 'horReplicationStatus'
--
-- * 'horCacheControl'
--
-- * 'horContentLanguage'
--
-- * 'horLastModified'
--
-- * 'horContentDisposition'
--
-- * 'horServerSideEncryption'
--
-- * 'horContentType'
data HeadObjectResponse = HeadObjectResponse'{_horVersionId :: Maybe ObjectVersionId, _horETag :: Maybe ETag, _horRequestCharged :: Maybe RequestCharged, _horContentLength :: Maybe Int, _horRestore :: Maybe Text, _horExpires :: Maybe RFC822, _horDeleteMarker :: Maybe Bool, _horExpiration :: Maybe Text, _horSSECustomerAlgorithm :: Maybe Text, _horMissingMeta :: Maybe Int, _horWebsiteRedirectLocation :: Maybe Text, _horAcceptRanges :: Maybe Text, _horContentEncoding :: Maybe Text, _horSSEKMSKeyId :: Maybe (Sensitive Text), _horSSECustomerKeyMD5 :: Maybe Text, _horMetadata :: HashMap Text Text, _horReplicationStatus :: Maybe ReplicationStatus, _horCacheControl :: Maybe Text, _horContentLanguage :: Maybe Text, _horLastModified :: Maybe RFC822, _horContentDisposition :: Maybe Text, _horServerSideEncryption :: Maybe ServerSideEncryption, _horContentType :: Maybe Text} deriving (Eq, Read, Show)

-- | 'HeadObjectResponse' smart constructor.
headObjectResponse :: HeadObjectResponse
headObjectResponse = HeadObjectResponse'{_horVersionId = Nothing, _horETag = Nothing, _horRequestCharged = Nothing, _horContentLength = Nothing, _horRestore = Nothing, _horExpires = Nothing, _horDeleteMarker = Nothing, _horExpiration = Nothing, _horSSECustomerAlgorithm = Nothing, _horMissingMeta = Nothing, _horWebsiteRedirectLocation = Nothing, _horAcceptRanges = Nothing, _horContentEncoding = Nothing, _horSSEKMSKeyId = Nothing, _horSSECustomerKeyMD5 = Nothing, _horMetadata = mempty, _horReplicationStatus = Nothing, _horCacheControl = Nothing, _horContentLanguage = Nothing, _horLastModified = Nothing, _horContentDisposition = Nothing, _horServerSideEncryption = Nothing, _horContentType = Nothing};

-- | Version of the object.
horVersionId :: Lens' HeadObjectResponse (Maybe ObjectVersionId)
horVersionId = lens _horVersionId (\ s a -> s{_horVersionId = a});

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL
horETag :: Lens' HeadObjectResponse (Maybe ETag)
horETag = lens _horETag (\ s a -> s{_horETag = a});

-- | FIXME: Undocumented member.
horRequestCharged :: Lens' HeadObjectResponse (Maybe RequestCharged)
horRequestCharged = lens _horRequestCharged (\ s a -> s{_horRequestCharged = a});

-- | Size of the body in bytes.
horContentLength :: Lens' HeadObjectResponse (Maybe Int)
horContentLength = lens _horContentLength (\ s a -> s{_horContentLength = a});

-- | Provides information about object restoration operation and expiration
-- time of the restored object copy.
horRestore :: Lens' HeadObjectResponse (Maybe Text)
horRestore = lens _horRestore (\ s a -> s{_horRestore = a});

-- | The date and time at which the object is no longer cacheable.
horExpires :: Lens' HeadObjectResponse (Maybe UTCTime)
horExpires = lens _horExpires (\ s a -> s{_horExpires = a}) . mapping _Time;

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
horDeleteMarker :: Lens' HeadObjectResponse (Maybe Bool)
horDeleteMarker = lens _horDeleteMarker (\ s a -> s{_horDeleteMarker = a});

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id
-- key value pairs providing object expiration information. The value of
-- the rule-id is URL encoded.
horExpiration :: Lens' HeadObjectResponse (Maybe Text)
horExpiration = lens _horExpiration (\ s a -> s{_horExpiration = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
horSSECustomerAlgorithm :: Lens' HeadObjectResponse (Maybe Text)
horSSECustomerAlgorithm = lens _horSSECustomerAlgorithm (\ s a -> s{_horSSECustomerAlgorithm = a});

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP
-- that supports more flexible metadata than the REST API. For example,
-- using SOAP, you can create metadata whose values are not legal HTTP
-- headers.
horMissingMeta :: Lens' HeadObjectResponse (Maybe Int)
horMissingMeta = lens _horMissingMeta (\ s a -> s{_horMissingMeta = a});

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
horWebsiteRedirectLocation :: Lens' HeadObjectResponse (Maybe Text)
horWebsiteRedirectLocation = lens _horWebsiteRedirectLocation (\ s a -> s{_horWebsiteRedirectLocation = a});

-- | FIXME: Undocumented member.
horAcceptRanges :: Lens' HeadObjectResponse (Maybe Text)
horAcceptRanges = lens _horAcceptRanges (\ s a -> s{_horAcceptRanges = a});

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
horContentEncoding :: Lens' HeadObjectResponse (Maybe Text)
horContentEncoding = lens _horContentEncoding (\ s a -> s{_horContentEncoding = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
horSSEKMSKeyId :: Lens' HeadObjectResponse (Maybe Text)
horSSEKMSKeyId = lens _horSSEKMSKeyId (\ s a -> s{_horSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
horSSECustomerKeyMD5 :: Lens' HeadObjectResponse (Maybe Text)
horSSECustomerKeyMD5 = lens _horSSECustomerKeyMD5 (\ s a -> s{_horSSECustomerKeyMD5 = a});

-- | A map of metadata to store with the object in S3.
horMetadata :: Lens' HeadObjectResponse (HashMap Text Text)
horMetadata = lens _horMetadata (\ s a -> s{_horMetadata = a}) . _Coerce;

-- | FIXME: Undocumented member.
horReplicationStatus :: Lens' HeadObjectResponse (Maybe ReplicationStatus)
horReplicationStatus = lens _horReplicationStatus (\ s a -> s{_horReplicationStatus = a});

-- | Specifies caching behavior along the request\/reply chain.
horCacheControl :: Lens' HeadObjectResponse (Maybe Text)
horCacheControl = lens _horCacheControl (\ s a -> s{_horCacheControl = a});

-- | The language the content is in.
horContentLanguage :: Lens' HeadObjectResponse (Maybe Text)
horContentLanguage = lens _horContentLanguage (\ s a -> s{_horContentLanguage = a});

-- | Last modified date of the object
horLastModified :: Lens' HeadObjectResponse (Maybe UTCTime)
horLastModified = lens _horLastModified (\ s a -> s{_horLastModified = a}) . mapping _Time;

-- | Specifies presentational information for the object.
horContentDisposition :: Lens' HeadObjectResponse (Maybe Text)
horContentDisposition = lens _horContentDisposition (\ s a -> s{_horContentDisposition = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
horServerSideEncryption :: Lens' HeadObjectResponse (Maybe ServerSideEncryption)
horServerSideEncryption = lens _horServerSideEncryption (\ s a -> s{_horServerSideEncryption = a});

-- | A standard MIME type describing the format of the object data.
horContentType :: Lens' HeadObjectResponse (Maybe Text)
horContentType = lens _horContentType (\ s a -> s{_horContentType = a});
