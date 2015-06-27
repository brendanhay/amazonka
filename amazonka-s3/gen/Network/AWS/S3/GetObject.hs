{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.GetObject
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

-- | Retrieves objects from Amazon S3.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetObject.html>
module Network.AWS.S3.GetObject
    (
    -- * Request
      GetObject
    -- ** Request constructor
    , getObject
    -- ** Request lenses
    , goIfMatch
    , goVersionId
    , goResponseContentType
    , goResponseContentDisposition
    , goResponseContentLanguage
    , goSSECustomerAlgorithm
    , goSSECustomerKey
    , goRequestPayer
    , goResponseContentEncoding
    , goIfModifiedSince
    , goRange
    , goIfUnmodifiedSince
    , goSSECustomerKeyMD5
    , goResponseCacheControl
    , goResponseExpires
    , goIfNoneMatch
    , goBucket
    , goKey

    -- * Response
    , GetObjectResponse
    -- ** Response constructor
    , getObjectResponse
    -- ** Response lenses
    , gorVersionId
    , gorETag
    , gorRequestCharged
    , gorContentLength
    , gorRestore
    , gorExpires
    , gorDeleteMarker
    , gorExpiration
    , gorSSECustomerAlgorithm
    , gorMissingMeta
    , gorWebsiteRedirectLocation
    , gorAcceptRanges
    , gorContentEncoding
    , gorSSEKMSKeyId
    , gorSSECustomerKeyMD5
    , gorMetadata
    , gorReplicationStatus
    , gorCacheControl
    , gorContentLanguage
    , gorLastModified
    , gorContentDisposition
    , gorServerSideEncryption
    , gorContentType
    , gorStatus
    , gorBody
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getObject' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goIfMatch'
--
-- * 'goVersionId'
--
-- * 'goResponseContentType'
--
-- * 'goResponseContentDisposition'
--
-- * 'goResponseContentLanguage'
--
-- * 'goSSECustomerAlgorithm'
--
-- * 'goSSECustomerKey'
--
-- * 'goRequestPayer'
--
-- * 'goResponseContentEncoding'
--
-- * 'goIfModifiedSince'
--
-- * 'goRange'
--
-- * 'goIfUnmodifiedSince'
--
-- * 'goSSECustomerKeyMD5'
--
-- * 'goResponseCacheControl'
--
-- * 'goResponseExpires'
--
-- * 'goIfNoneMatch'
--
-- * 'goBucket'
--
-- * 'goKey'
data GetObject = GetObject'
    { _goIfMatch                    :: Maybe Text
    , _goVersionId                  :: Maybe ObjectVersionId
    , _goResponseContentType        :: Maybe Text
    , _goResponseContentDisposition :: Maybe Text
    , _goResponseContentLanguage    :: Maybe Text
    , _goSSECustomerAlgorithm       :: Maybe Text
    , _goSSECustomerKey             :: Maybe (Sensitive Text)
    , _goRequestPayer               :: Maybe RequestPayer
    , _goResponseContentEncoding    :: Maybe Text
    , _goIfModifiedSince            :: Maybe RFC822
    , _goRange                      :: Maybe Text
    , _goIfUnmodifiedSince          :: Maybe RFC822
    , _goSSECustomerKeyMD5          :: Maybe Text
    , _goResponseCacheControl       :: Maybe Text
    , _goResponseExpires            :: Maybe RFC822
    , _goIfNoneMatch                :: Maybe Text
    , _goBucket                     :: BucketName
    , _goKey                        :: ObjectKey
    } deriving (Eq,Read,Show)

-- | 'GetObject' smart constructor.
getObject :: BucketName -> ObjectKey -> GetObject
getObject pBucket pKey =
    GetObject'
    { _goIfMatch = Nothing
    , _goVersionId = Nothing
    , _goResponseContentType = Nothing
    , _goResponseContentDisposition = Nothing
    , _goResponseContentLanguage = Nothing
    , _goSSECustomerAlgorithm = Nothing
    , _goSSECustomerKey = Nothing
    , _goRequestPayer = Nothing
    , _goResponseContentEncoding = Nothing
    , _goIfModifiedSince = Nothing
    , _goRange = Nothing
    , _goIfUnmodifiedSince = Nothing
    , _goSSECustomerKeyMD5 = Nothing
    , _goResponseCacheControl = Nothing
    , _goResponseExpires = Nothing
    , _goIfNoneMatch = Nothing
    , _goBucket = pBucket
    , _goKey = pKey
    }

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
goIfMatch :: Lens' GetObject (Maybe Text)
goIfMatch = lens _goIfMatch (\ s a -> s{_goIfMatch = a});

-- | VersionId used to reference a specific version of the object.
goVersionId :: Lens' GetObject (Maybe ObjectVersionId)
goVersionId = lens _goVersionId (\ s a -> s{_goVersionId = a});

-- | Sets the Content-Type header of the response.
goResponseContentType :: Lens' GetObject (Maybe Text)
goResponseContentType = lens _goResponseContentType (\ s a -> s{_goResponseContentType = a});

-- | Sets the Content-Disposition header of the response
goResponseContentDisposition :: Lens' GetObject (Maybe Text)
goResponseContentDisposition = lens _goResponseContentDisposition (\ s a -> s{_goResponseContentDisposition = a});

-- | Sets the Content-Language header of the response.
goResponseContentLanguage :: Lens' GetObject (Maybe Text)
goResponseContentLanguage = lens _goResponseContentLanguage (\ s a -> s{_goResponseContentLanguage = a});

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
goSSECustomerAlgorithm :: Lens' GetObject (Maybe Text)
goSSECustomerAlgorithm = lens _goSSECustomerAlgorithm (\ s a -> s{_goSSECustomerAlgorithm = a});

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header.
goSSECustomerKey :: Lens' GetObject (Maybe Text)
goSSECustomerKey = lens _goSSECustomerKey (\ s a -> s{_goSSECustomerKey = a}) . mapping _Sensitive;

-- | FIXME: Undocumented member.
goRequestPayer :: Lens' GetObject (Maybe RequestPayer)
goRequestPayer = lens _goRequestPayer (\ s a -> s{_goRequestPayer = a});

-- | Sets the Content-Encoding header of the response.
goResponseContentEncoding :: Lens' GetObject (Maybe Text)
goResponseContentEncoding = lens _goResponseContentEncoding (\ s a -> s{_goResponseContentEncoding = a});

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
goIfModifiedSince :: Lens' GetObject (Maybe UTCTime)
goIfModifiedSince = lens _goIfModifiedSince (\ s a -> s{_goIfModifiedSince = a}) . mapping _Time;

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http:\/\/www.w3.org\/Protocols\/rfc2616\/rfc2616-sec14.html#sec14.35.
goRange :: Lens' GetObject (Maybe Text)
goRange = lens _goRange (\ s a -> s{_goRange = a});

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
goIfUnmodifiedSince :: Lens' GetObject (Maybe UTCTime)
goIfUnmodifiedSince = lens _goIfUnmodifiedSince (\ s a -> s{_goIfUnmodifiedSince = a}) . mapping _Time;

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
goSSECustomerKeyMD5 :: Lens' GetObject (Maybe Text)
goSSECustomerKeyMD5 = lens _goSSECustomerKeyMD5 (\ s a -> s{_goSSECustomerKeyMD5 = a});

-- | Sets the Cache-Control header of the response.
goResponseCacheControl :: Lens' GetObject (Maybe Text)
goResponseCacheControl = lens _goResponseCacheControl (\ s a -> s{_goResponseCacheControl = a});

-- | Sets the Expires header of the response.
goResponseExpires :: Lens' GetObject (Maybe UTCTime)
goResponseExpires = lens _goResponseExpires (\ s a -> s{_goResponseExpires = a}) . mapping _Time;

-- | Return the object only if its entity tag (ETag) is different from the
-- one specified, otherwise return a 304 (not modified).
goIfNoneMatch :: Lens' GetObject (Maybe Text)
goIfNoneMatch = lens _goIfNoneMatch (\ s a -> s{_goIfNoneMatch = a});

-- | FIXME: Undocumented member.
goBucket :: Lens' GetObject BucketName
goBucket = lens _goBucket (\ s a -> s{_goBucket = a});

-- | FIXME: Undocumented member.
goKey :: Lens' GetObject ObjectKey
goKey = lens _goKey (\ s a -> s{_goKey = a});

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
              ["If-Match" =# _goIfMatch,
               "x-amz-server-side-encryption-customer-algorithm" =#
                 _goSSECustomerAlgorithm,
               "x-amz-server-side-encryption-customer-key" =#
                 _goSSECustomerKey,
               "x-amz-request-payer" =# _goRequestPayer,
               "If-Modified-Since" =# _goIfModifiedSince,
               "Range" =# _goRange,
               "If-Unmodified-Since" =# _goIfUnmodifiedSince,
               "x-amz-server-side-encryption-customer-key-MD5" =#
                 _goSSECustomerKeyMD5,
               "If-None-Match" =# _goIfNoneMatch]

instance ToPath GetObject where
        toPath GetObject'{..}
          = mconcat ["/", toText _goBucket, "/", toText _goKey]

instance ToQuery GetObject where
        toQuery GetObject'{..}
          = mconcat
              ["versionId" =: _goVersionId,
               "response-content-type" =: _goResponseContentType,
               "response-content-disposition" =:
                 _goResponseContentDisposition,
               "response-content-language" =:
                 _goResponseContentLanguage,
               "response-content-encoding" =:
                 _goResponseContentEncoding,
               "response-cache-control" =: _goResponseCacheControl,
               "response-expires" =: _goResponseExpires]

-- | /See:/ 'getObjectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gorVersionId'
--
-- * 'gorETag'
--
-- * 'gorRequestCharged'
--
-- * 'gorContentLength'
--
-- * 'gorRestore'
--
-- * 'gorExpires'
--
-- * 'gorDeleteMarker'
--
-- * 'gorExpiration'
--
-- * 'gorSSECustomerAlgorithm'
--
-- * 'gorMissingMeta'
--
-- * 'gorWebsiteRedirectLocation'
--
-- * 'gorAcceptRanges'
--
-- * 'gorContentEncoding'
--
-- * 'gorSSEKMSKeyId'
--
-- * 'gorSSECustomerKeyMD5'
--
-- * 'gorMetadata'
--
-- * 'gorReplicationStatus'
--
-- * 'gorCacheControl'
--
-- * 'gorContentLanguage'
--
-- * 'gorLastModified'
--
-- * 'gorContentDisposition'
--
-- * 'gorServerSideEncryption'
--
-- * 'gorContentType'
--
-- * 'gorStatus'
--
-- * 'gorBody'
data GetObjectResponse = GetObjectResponse'
    { _gorVersionId               :: Maybe ObjectVersionId
    , _gorETag                    :: Maybe ETag
    , _gorRequestCharged          :: Maybe RequestCharged
    , _gorContentLength           :: Maybe Int
    , _gorRestore                 :: Maybe Text
    , _gorExpires                 :: Maybe RFC822
    , _gorDeleteMarker            :: Maybe Bool
    , _gorExpiration              :: Maybe Text
    , _gorSSECustomerAlgorithm    :: Maybe Text
    , _gorMissingMeta             :: Maybe Int
    , _gorWebsiteRedirectLocation :: Maybe Text
    , _gorAcceptRanges            :: Maybe Text
    , _gorContentEncoding         :: Maybe Text
    , _gorSSEKMSKeyId             :: Maybe (Sensitive Text)
    , _gorSSECustomerKeyMD5       :: Maybe Text
    , _gorMetadata                :: Map Text Text
    , _gorReplicationStatus       :: Maybe ReplicationStatus
    , _gorCacheControl            :: Maybe Text
    , _gorContentLanguage         :: Maybe Text
    , _gorLastModified            :: Maybe RFC822
    , _gorContentDisposition      :: Maybe Text
    , _gorServerSideEncryption    :: Maybe ServerSideEncryption
    , _gorContentType             :: Maybe Text
    , _gorStatus                  :: !Int
    , _gorBody                    :: RsBody
    } deriving (Show)

-- | 'GetObjectResponse' smart constructor.
getObjectResponse :: Int -> RsBody -> GetObjectResponse
getObjectResponse pStatus pBody =
    GetObjectResponse'
    { _gorVersionId = Nothing
    , _gorETag = Nothing
    , _gorRequestCharged = Nothing
    , _gorContentLength = Nothing
    , _gorRestore = Nothing
    , _gorExpires = Nothing
    , _gorDeleteMarker = Nothing
    , _gorExpiration = Nothing
    , _gorSSECustomerAlgorithm = Nothing
    , _gorMissingMeta = Nothing
    , _gorWebsiteRedirectLocation = Nothing
    , _gorAcceptRanges = Nothing
    , _gorContentEncoding = Nothing
    , _gorSSEKMSKeyId = Nothing
    , _gorSSECustomerKeyMD5 = Nothing
    , _gorMetadata = mempty
    , _gorReplicationStatus = Nothing
    , _gorCacheControl = Nothing
    , _gorContentLanguage = Nothing
    , _gorLastModified = Nothing
    , _gorContentDisposition = Nothing
    , _gorServerSideEncryption = Nothing
    , _gorContentType = Nothing
    , _gorStatus = pStatus
    , _gorBody = pBody
    }

-- | Version of the object.
gorVersionId :: Lens' GetObjectResponse (Maybe ObjectVersionId)
gorVersionId = lens _gorVersionId (\ s a -> s{_gorVersionId = a});

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL
gorETag :: Lens' GetObjectResponse (Maybe ETag)
gorETag = lens _gorETag (\ s a -> s{_gorETag = a});

-- | FIXME: Undocumented member.
gorRequestCharged :: Lens' GetObjectResponse (Maybe RequestCharged)
gorRequestCharged = lens _gorRequestCharged (\ s a -> s{_gorRequestCharged = a});

-- | Size of the body in bytes.
gorContentLength :: Lens' GetObjectResponse (Maybe Int)
gorContentLength = lens _gorContentLength (\ s a -> s{_gorContentLength = a});

-- | Provides information about object restoration operation and expiration
-- time of the restored object copy.
gorRestore :: Lens' GetObjectResponse (Maybe Text)
gorRestore = lens _gorRestore (\ s a -> s{_gorRestore = a});

-- | The date and time at which the object is no longer cacheable.
gorExpires :: Lens' GetObjectResponse (Maybe UTCTime)
gorExpires = lens _gorExpires (\ s a -> s{_gorExpires = a}) . mapping _Time;

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
gorDeleteMarker :: Lens' GetObjectResponse (Maybe Bool)
gorDeleteMarker = lens _gorDeleteMarker (\ s a -> s{_gorDeleteMarker = a});

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id
-- key value pairs providing object expiration information. The value of
-- the rule-id is URL encoded.
gorExpiration :: Lens' GetObjectResponse (Maybe Text)
gorExpiration = lens _gorExpiration (\ s a -> s{_gorExpiration = a});

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
gorSSECustomerAlgorithm :: Lens' GetObjectResponse (Maybe Text)
gorSSECustomerAlgorithm = lens _gorSSECustomerAlgorithm (\ s a -> s{_gorSSECustomerAlgorithm = a});

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP
-- that supports more flexible metadata than the REST API. For example,
-- using SOAP, you can create metadata whose values are not legal HTTP
-- headers.
gorMissingMeta :: Lens' GetObjectResponse (Maybe Int)
gorMissingMeta = lens _gorMissingMeta (\ s a -> s{_gorMissingMeta = a});

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL.
-- Amazon S3 stores the value of this header in the object metadata.
gorWebsiteRedirectLocation :: Lens' GetObjectResponse (Maybe Text)
gorWebsiteRedirectLocation = lens _gorWebsiteRedirectLocation (\ s a -> s{_gorWebsiteRedirectLocation = a});

-- | FIXME: Undocumented member.
gorAcceptRanges :: Lens' GetObjectResponse (Maybe Text)
gorAcceptRanges = lens _gorAcceptRanges (\ s a -> s{_gorAcceptRanges = a});

-- | Specifies what content encodings have been applied to the object and
-- thus what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
gorContentEncoding :: Lens' GetObjectResponse (Maybe Text)
gorContentEncoding = lens _gorContentEncoding (\ s a -> s{_gorContentEncoding = a});

-- | If present, specifies the ID of the AWS Key Management Service (KMS)
-- master encryption key that was used for the object.
gorSSEKMSKeyId :: Lens' GetObjectResponse (Maybe Text)
gorSSEKMSKeyId = lens _gorSSEKMSKeyId (\ s a -> s{_gorSSEKMSKeyId = a}) . mapping _Sensitive;

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
gorSSECustomerKeyMD5 :: Lens' GetObjectResponse (Maybe Text)
gorSSECustomerKeyMD5 = lens _gorSSECustomerKeyMD5 (\ s a -> s{_gorSSECustomerKeyMD5 = a});

-- | A map of metadata to store with the object in S3.
gorMetadata :: Lens' GetObjectResponse (HashMap Text Text)
gorMetadata = lens _gorMetadata (\ s a -> s{_gorMetadata = a}) . _Map;

-- | FIXME: Undocumented member.
gorReplicationStatus :: Lens' GetObjectResponse (Maybe ReplicationStatus)
gorReplicationStatus = lens _gorReplicationStatus (\ s a -> s{_gorReplicationStatus = a});

-- | Specifies caching behavior along the request\/reply chain.
gorCacheControl :: Lens' GetObjectResponse (Maybe Text)
gorCacheControl = lens _gorCacheControl (\ s a -> s{_gorCacheControl = a});

-- | The language the content is in.
gorContentLanguage :: Lens' GetObjectResponse (Maybe Text)
gorContentLanguage = lens _gorContentLanguage (\ s a -> s{_gorContentLanguage = a});

-- | Last modified date of the object
gorLastModified :: Lens' GetObjectResponse (Maybe UTCTime)
gorLastModified = lens _gorLastModified (\ s a -> s{_gorLastModified = a}) . mapping _Time;

-- | Specifies presentational information for the object.
gorContentDisposition :: Lens' GetObjectResponse (Maybe Text)
gorContentDisposition = lens _gorContentDisposition (\ s a -> s{_gorContentDisposition = a});

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
gorServerSideEncryption :: Lens' GetObjectResponse (Maybe ServerSideEncryption)
gorServerSideEncryption = lens _gorServerSideEncryption (\ s a -> s{_gorServerSideEncryption = a});

-- | A standard MIME type describing the format of the object data.
gorContentType :: Lens' GetObjectResponse (Maybe Text)
gorContentType = lens _gorContentType (\ s a -> s{_gorContentType = a});

-- | FIXME: Undocumented member.
gorStatus :: Lens' GetObjectResponse Int
gorStatus = lens _gorStatus (\ s a -> s{_gorStatus = a});

-- | Object data.
gorBody :: Lens' GetObjectResponse RsBody
gorBody = lens _gorBody (\ s a -> s{_gorBody = a});
