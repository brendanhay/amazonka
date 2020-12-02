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
-- Module      : Network.AWS.S3.GetObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves objects from Amazon S3.
module Network.AWS.S3.GetObject
    (
    -- * Creating a Request
      getObject
    , GetObject
    -- * Request Lenses
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
    , goPartNumber
    , goRange
    , goIfUnmodifiedSince
    , goSSECustomerKeyMD5
    , goResponseCacheControl
    , goResponseExpires
    , goIfNoneMatch
    , goBucket
    , goKey

    -- * Destructuring the Response
    , getObjectResponse
    , GetObjectResponse
    -- * Response Lenses
    , gorsRequestCharged
    , gorsPartsCount
    , gorsETag
    , gorsVersionId
    , gorsContentLength
    , gorsExpires
    , gorsRestore
    , gorsExpiration
    , gorsDeleteMarker
    , gorsSSECustomerAlgorithm
    , gorsTagCount
    , gorsMissingMeta
    , gorsWebsiteRedirectLocation
    , gorsAcceptRanges
    , gorsStorageClass
    , gorsSSECustomerKeyMD5
    , gorsSSEKMSKeyId
    , gorsContentEncoding
    , gorsMetadata
    , gorsReplicationStatus
    , gorsCacheControl
    , gorsContentLanguage
    , gorsLastModified
    , gorsContentDisposition
    , gorsContentRange
    , gorsServerSideEncryption
    , gorsContentType
    , gorsResponseStatus
    , gorsBody
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getObject' smart constructor.
data GetObject = GetObject'
  { _goIfMatch                    :: !(Maybe Text)
  , _goVersionId                  :: !(Maybe ObjectVersionId)
  , _goResponseContentType        :: !(Maybe Text)
  , _goResponseContentDisposition :: !(Maybe Text)
  , _goResponseContentLanguage    :: !(Maybe Text)
  , _goSSECustomerAlgorithm       :: !(Maybe Text)
  , _goSSECustomerKey             :: !(Maybe (Sensitive Text))
  , _goRequestPayer               :: !(Maybe RequestPayer)
  , _goResponseContentEncoding    :: !(Maybe Text)
  , _goIfModifiedSince            :: !(Maybe RFC822)
  , _goPartNumber                 :: !(Maybe Int)
  , _goRange                      :: !(Maybe Text)
  , _goIfUnmodifiedSince          :: !(Maybe RFC822)
  , _goSSECustomerKeyMD5          :: !(Maybe Text)
  , _goResponseCacheControl       :: !(Maybe Text)
  , _goResponseExpires            :: !(Maybe RFC822)
  , _goIfNoneMatch                :: !(Maybe Text)
  , _goBucket                     :: !BucketName
  , _goKey                        :: !ObjectKey
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goIfMatch' - Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
--
-- * 'goVersionId' - VersionId used to reference a specific version of the object.
--
-- * 'goResponseContentType' - Sets the Content-Type header of the response.
--
-- * 'goResponseContentDisposition' - Sets the Content-Disposition header of the response
--
-- * 'goResponseContentLanguage' - Sets the Content-Language header of the response.
--
-- * 'goSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (e.g., AES256).
--
-- * 'goSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side​-encryption​-customer-algorithm header.
--
-- * 'goRequestPayer' - Undocumented member.
--
-- * 'goResponseContentEncoding' - Sets the Content-Encoding header of the response.
--
-- * 'goIfModifiedSince' - Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
--
-- * 'goPartNumber' - Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' GET request for the part specified. Useful for downloading just a part of an object.
--
-- * 'goRange' - Downloads the specified range bytes of an object. For more information about the HTTP Range header, go to http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
--
-- * 'goIfUnmodifiedSince' - Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
--
-- * 'goSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
--
-- * 'goResponseCacheControl' - Sets the Cache-Control header of the response.
--
-- * 'goResponseExpires' - Sets the Expires header of the response.
--
-- * 'goIfNoneMatch' - Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
--
-- * 'goBucket' - Undocumented member.
--
-- * 'goKey' - Undocumented member.
getObject
    :: BucketName -- ^ 'goBucket'
    -> ObjectKey -- ^ 'goKey'
    -> GetObject
getObject pBucket_ pKey_ =
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
    , _goPartNumber = Nothing
    , _goRange = Nothing
    , _goIfUnmodifiedSince = Nothing
    , _goSSECustomerKeyMD5 = Nothing
    , _goResponseCacheControl = Nothing
    , _goResponseExpires = Nothing
    , _goIfNoneMatch = Nothing
    , _goBucket = pBucket_
    , _goKey = pKey_
    }


-- | Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
goIfMatch :: Lens' GetObject (Maybe Text)
goIfMatch = lens _goIfMatch (\ s a -> s{_goIfMatch = a})

-- | VersionId used to reference a specific version of the object.
goVersionId :: Lens' GetObject (Maybe ObjectVersionId)
goVersionId = lens _goVersionId (\ s a -> s{_goVersionId = a})

-- | Sets the Content-Type header of the response.
goResponseContentType :: Lens' GetObject (Maybe Text)
goResponseContentType = lens _goResponseContentType (\ s a -> s{_goResponseContentType = a})

-- | Sets the Content-Disposition header of the response
goResponseContentDisposition :: Lens' GetObject (Maybe Text)
goResponseContentDisposition = lens _goResponseContentDisposition (\ s a -> s{_goResponseContentDisposition = a})

-- | Sets the Content-Language header of the response.
goResponseContentLanguage :: Lens' GetObject (Maybe Text)
goResponseContentLanguage = lens _goResponseContentLanguage (\ s a -> s{_goResponseContentLanguage = a})

-- | Specifies the algorithm to use to when encrypting the object (e.g., AES256).
goSSECustomerAlgorithm :: Lens' GetObject (Maybe Text)
goSSECustomerAlgorithm = lens _goSSECustomerAlgorithm (\ s a -> s{_goSSECustomerAlgorithm = a})

-- | Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side​-encryption​-customer-algorithm header.
goSSECustomerKey :: Lens' GetObject (Maybe Text)
goSSECustomerKey = lens _goSSECustomerKey (\ s a -> s{_goSSECustomerKey = a}) . mapping _Sensitive

-- | Undocumented member.
goRequestPayer :: Lens' GetObject (Maybe RequestPayer)
goRequestPayer = lens _goRequestPayer (\ s a -> s{_goRequestPayer = a})

-- | Sets the Content-Encoding header of the response.
goResponseContentEncoding :: Lens' GetObject (Maybe Text)
goResponseContentEncoding = lens _goResponseContentEncoding (\ s a -> s{_goResponseContentEncoding = a})

-- | Return the object only if it has been modified since the specified time, otherwise return a 304 (not modified).
goIfModifiedSince :: Lens' GetObject (Maybe UTCTime)
goIfModifiedSince = lens _goIfModifiedSince (\ s a -> s{_goIfModifiedSince = a}) . mapping _Time

-- | Part number of the object being read. This is a positive integer between 1 and 10,000. Effectively performs a 'ranged' GET request for the part specified. Useful for downloading just a part of an object.
goPartNumber :: Lens' GetObject (Maybe Int)
goPartNumber = lens _goPartNumber (\ s a -> s{_goPartNumber = a})

-- | Downloads the specified range bytes of an object. For more information about the HTTP Range header, go to http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
goRange :: Lens' GetObject (Maybe Text)
goRange = lens _goRange (\ s a -> s{_goRange = a})

-- | Return the object only if it has not been modified since the specified time, otherwise return a 412 (precondition failed).
goIfUnmodifiedSince :: Lens' GetObject (Maybe UTCTime)
goIfUnmodifiedSince = lens _goIfUnmodifiedSince (\ s a -> s{_goIfUnmodifiedSince = a}) . mapping _Time

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
goSSECustomerKeyMD5 :: Lens' GetObject (Maybe Text)
goSSECustomerKeyMD5 = lens _goSSECustomerKeyMD5 (\ s a -> s{_goSSECustomerKeyMD5 = a})

-- | Sets the Cache-Control header of the response.
goResponseCacheControl :: Lens' GetObject (Maybe Text)
goResponseCacheControl = lens _goResponseCacheControl (\ s a -> s{_goResponseCacheControl = a})

-- | Sets the Expires header of the response.
goResponseExpires :: Lens' GetObject (Maybe UTCTime)
goResponseExpires = lens _goResponseExpires (\ s a -> s{_goResponseExpires = a}) . mapping _Time

-- | Return the object only if its entity tag (ETag) is different from the one specified, otherwise return a 304 (not modified).
goIfNoneMatch :: Lens' GetObject (Maybe Text)
goIfNoneMatch = lens _goIfNoneMatch (\ s a -> s{_goIfNoneMatch = a})

-- | Undocumented member.
goBucket :: Lens' GetObject BucketName
goBucket = lens _goBucket (\ s a -> s{_goBucket = a})

-- | Undocumented member.
goKey :: Lens' GetObject ObjectKey
goKey = lens _goKey (\ s a -> s{_goKey = a})

instance AWSRequest GetObject where
        type Rs GetObject = GetObjectResponse
        request = get s3
        response
          = receiveBody
              (\ s h x ->
                 GetObjectResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (h .#? "x-amz-mp-parts-count")
                     <*> (h .#? "ETag")
                     <*> (h .#? "x-amz-version-id")
                     <*> (h .#? "Content-Length")
                     <*> (h .#? "Expires")
                     <*> (h .#? "x-amz-restore")
                     <*> (h .#? "x-amz-expiration")
                     <*> (h .#? "x-amz-delete-marker")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-algorithm")
                     <*> (h .#? "x-amz-tagging-count")
                     <*> (h .#? "x-amz-missing-meta")
                     <*> (h .#? "x-amz-website-redirect-location")
                     <*> (h .#? "accept-ranges")
                     <*> (h .#? "x-amz-storage-class")
                     <*>
                     (h .#?
                        "x-amz-server-side-encryption-customer-key-MD5")
                     <*>
                     (h .#? "x-amz-server-side-encryption-aws-kms-key-id")
                     <*> (h .#? "Content-Encoding")
                     <*> (parseHeadersMap "x-amz-meta-" h)
                     <*> (h .#? "x-amz-replication-status")
                     <*> (h .#? "Cache-Control")
                     <*> (h .#? "Content-Language")
                     <*> (h .#? "Last-Modified")
                     <*> (h .#? "Content-Disposition")
                     <*> (h .#? "Content-Range")
                     <*> (h .#? "x-amz-server-side-encryption")
                     <*> (h .#? "Content-Type")
                     <*> (pure (fromEnum s))
                     <*> (pure x))

instance Hashable GetObject where

instance NFData GetObject where

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
          = mconcat ["/", toBS _goBucket, "/", toBS _goKey]

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
               "partNumber" =: _goPartNumber,
               "response-cache-control" =: _goResponseCacheControl,
               "response-expires" =: _goResponseExpires]

-- | /See:/ 'getObjectResponse' smart constructor.
data GetObjectResponse = GetObjectResponse'
  { _gorsRequestCharged          :: !(Maybe RequestCharged)
  , _gorsPartsCount              :: !(Maybe Int)
  , _gorsETag                    :: !(Maybe ETag)
  , _gorsVersionId               :: !(Maybe ObjectVersionId)
  , _gorsContentLength           :: !(Maybe Integer)
  , _gorsExpires                 :: !(Maybe RFC822)
  , _gorsRestore                 :: !(Maybe Text)
  , _gorsExpiration              :: !(Maybe Text)
  , _gorsDeleteMarker            :: !(Maybe Bool)
  , _gorsSSECustomerAlgorithm    :: !(Maybe Text)
  , _gorsTagCount                :: !(Maybe Int)
  , _gorsMissingMeta             :: !(Maybe Int)
  , _gorsWebsiteRedirectLocation :: !(Maybe Text)
  , _gorsAcceptRanges            :: !(Maybe Text)
  , _gorsStorageClass            :: !(Maybe StorageClass)
  , _gorsSSECustomerKeyMD5       :: !(Maybe Text)
  , _gorsSSEKMSKeyId             :: !(Maybe (Sensitive Text))
  , _gorsContentEncoding         :: !(Maybe Text)
  , _gorsMetadata                :: !(Map Text Text)
  , _gorsReplicationStatus       :: !(Maybe ReplicationStatus)
  , _gorsCacheControl            :: !(Maybe Text)
  , _gorsContentLanguage         :: !(Maybe Text)
  , _gorsLastModified            :: !(Maybe RFC822)
  , _gorsContentDisposition      :: !(Maybe Text)
  , _gorsContentRange            :: !(Maybe Text)
  , _gorsServerSideEncryption    :: !(Maybe ServerSideEncryption)
  , _gorsContentType             :: !(Maybe Text)
  , _gorsResponseStatus          :: !Int
  , _gorsBody                    :: !RsBody
  } deriving (Show, Generic)


-- | Creates a value of 'GetObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gorsRequestCharged' - Undocumented member.
--
-- * 'gorsPartsCount' - The count of parts this object has.
--
-- * 'gorsETag' - An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL
--
-- * 'gorsVersionId' - Version of the object.
--
-- * 'gorsContentLength' - Size of the body in bytes.
--
-- * 'gorsExpires' - The date and time at which the object is no longer cacheable.
--
-- * 'gorsRestore' - Provides information about object restoration operation and expiration time of the restored object copy.
--
-- * 'gorsExpiration' - If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key value pairs providing object expiration information. The value of the rule-id is URL encoded.
--
-- * 'gorsDeleteMarker' - Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
--
-- * 'gorsSSECustomerAlgorithm' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
--
-- * 'gorsTagCount' - The number of tags, if any, on the object.
--
-- * 'gorsMissingMeta' - This is set to the number of metadata entries not returned in x-amz-meta headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
--
-- * 'gorsWebsiteRedirectLocation' - If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
--
-- * 'gorsAcceptRanges' - Undocumented member.
--
-- * 'gorsStorageClass' - Undocumented member.
--
-- * 'gorsSSECustomerKeyMD5' - If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key.
--
-- * 'gorsSSEKMSKeyId' - If present, specifies the ID of the AWS Key Management Service (KMS) master encryption key that was used for the object.
--
-- * 'gorsContentEncoding' - Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
--
-- * 'gorsMetadata' - A map of metadata to store with the object in S3.
--
-- * 'gorsReplicationStatus' - Undocumented member.
--
-- * 'gorsCacheControl' - Specifies caching behavior along the request/reply chain.
--
-- * 'gorsContentLanguage' - The language the content is in.
--
-- * 'gorsLastModified' - Last modified date of the object
--
-- * 'gorsContentDisposition' - Specifies presentational information for the object.
--
-- * 'gorsContentRange' - The portion of the object returned in the response.
--
-- * 'gorsServerSideEncryption' - The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
--
-- * 'gorsContentType' - A standard MIME type describing the format of the object data.
--
-- * 'gorsResponseStatus' - -- | The response status code.
--
-- * 'gorsBody' - Object data.
getObjectResponse
    :: Int -- ^ 'gorsResponseStatus'
    -> RsBody -- ^ 'gorsBody'
    -> GetObjectResponse
getObjectResponse pResponseStatus_ pBody_ =
  GetObjectResponse'
    { _gorsRequestCharged = Nothing
    , _gorsPartsCount = Nothing
    , _gorsETag = Nothing
    , _gorsVersionId = Nothing
    , _gorsContentLength = Nothing
    , _gorsExpires = Nothing
    , _gorsRestore = Nothing
    , _gorsExpiration = Nothing
    , _gorsDeleteMarker = Nothing
    , _gorsSSECustomerAlgorithm = Nothing
    , _gorsTagCount = Nothing
    , _gorsMissingMeta = Nothing
    , _gorsWebsiteRedirectLocation = Nothing
    , _gorsAcceptRanges = Nothing
    , _gorsStorageClass = Nothing
    , _gorsSSECustomerKeyMD5 = Nothing
    , _gorsSSEKMSKeyId = Nothing
    , _gorsContentEncoding = Nothing
    , _gorsMetadata = mempty
    , _gorsReplicationStatus = Nothing
    , _gorsCacheControl = Nothing
    , _gorsContentLanguage = Nothing
    , _gorsLastModified = Nothing
    , _gorsContentDisposition = Nothing
    , _gorsContentRange = Nothing
    , _gorsServerSideEncryption = Nothing
    , _gorsContentType = Nothing
    , _gorsResponseStatus = pResponseStatus_
    , _gorsBody = pBody_
    }


-- | Undocumented member.
gorsRequestCharged :: Lens' GetObjectResponse (Maybe RequestCharged)
gorsRequestCharged = lens _gorsRequestCharged (\ s a -> s{_gorsRequestCharged = a})

-- | The count of parts this object has.
gorsPartsCount :: Lens' GetObjectResponse (Maybe Int)
gorsPartsCount = lens _gorsPartsCount (\ s a -> s{_gorsPartsCount = a})

-- | An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL
gorsETag :: Lens' GetObjectResponse (Maybe ETag)
gorsETag = lens _gorsETag (\ s a -> s{_gorsETag = a})

-- | Version of the object.
gorsVersionId :: Lens' GetObjectResponse (Maybe ObjectVersionId)
gorsVersionId = lens _gorsVersionId (\ s a -> s{_gorsVersionId = a})

-- | Size of the body in bytes.
gorsContentLength :: Lens' GetObjectResponse (Maybe Integer)
gorsContentLength = lens _gorsContentLength (\ s a -> s{_gorsContentLength = a})

-- | The date and time at which the object is no longer cacheable.
gorsExpires :: Lens' GetObjectResponse (Maybe UTCTime)
gorsExpires = lens _gorsExpires (\ s a -> s{_gorsExpires = a}) . mapping _Time

-- | Provides information about object restoration operation and expiration time of the restored object copy.
gorsRestore :: Lens' GetObjectResponse (Maybe Text)
gorsRestore = lens _gorsRestore (\ s a -> s{_gorsRestore = a})

-- | If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key value pairs providing object expiration information. The value of the rule-id is URL encoded.
gorsExpiration :: Lens' GetObjectResponse (Maybe Text)
gorsExpiration = lens _gorsExpiration (\ s a -> s{_gorsExpiration = a})

-- | Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response.
gorsDeleteMarker :: Lens' GetObjectResponse (Maybe Bool)
gorsDeleteMarker = lens _gorsDeleteMarker (\ s a -> s{_gorsDeleteMarker = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used.
gorsSSECustomerAlgorithm :: Lens' GetObjectResponse (Maybe Text)
gorsSSECustomerAlgorithm = lens _gorsSSECustomerAlgorithm (\ s a -> s{_gorsSSECustomerAlgorithm = a})

-- | The number of tags, if any, on the object.
gorsTagCount :: Lens' GetObjectResponse (Maybe Int)
gorsTagCount = lens _gorsTagCount (\ s a -> s{_gorsTagCount = a})

-- | This is set to the number of metadata entries not returned in x-amz-meta headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers.
gorsMissingMeta :: Lens' GetObjectResponse (Maybe Int)
gorsMissingMeta = lens _gorsMissingMeta (\ s a -> s{_gorsMissingMeta = a})

-- | If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata.
gorsWebsiteRedirectLocation :: Lens' GetObjectResponse (Maybe Text)
gorsWebsiteRedirectLocation = lens _gorsWebsiteRedirectLocation (\ s a -> s{_gorsWebsiteRedirectLocation = a})

-- | Undocumented member.
gorsAcceptRanges :: Lens' GetObjectResponse (Maybe Text)
gorsAcceptRanges = lens _gorsAcceptRanges (\ s a -> s{_gorsAcceptRanges = a})

-- | Undocumented member.
gorsStorageClass :: Lens' GetObjectResponse (Maybe StorageClass)
gorsStorageClass = lens _gorsStorageClass (\ s a -> s{_gorsStorageClass = a})

-- | If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key.
gorsSSECustomerKeyMD5 :: Lens' GetObjectResponse (Maybe Text)
gorsSSECustomerKeyMD5 = lens _gorsSSECustomerKeyMD5 (\ s a -> s{_gorsSSECustomerKeyMD5 = a})

-- | If present, specifies the ID of the AWS Key Management Service (KMS) master encryption key that was used for the object.
gorsSSEKMSKeyId :: Lens' GetObjectResponse (Maybe Text)
gorsSSEKMSKeyId = lens _gorsSSEKMSKeyId (\ s a -> s{_gorsSSEKMSKeyId = a}) . mapping _Sensitive

-- | Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field.
gorsContentEncoding :: Lens' GetObjectResponse (Maybe Text)
gorsContentEncoding = lens _gorsContentEncoding (\ s a -> s{_gorsContentEncoding = a})

-- | A map of metadata to store with the object in S3.
gorsMetadata :: Lens' GetObjectResponse (HashMap Text Text)
gorsMetadata = lens _gorsMetadata (\ s a -> s{_gorsMetadata = a}) . _Map

-- | Undocumented member.
gorsReplicationStatus :: Lens' GetObjectResponse (Maybe ReplicationStatus)
gorsReplicationStatus = lens _gorsReplicationStatus (\ s a -> s{_gorsReplicationStatus = a})

-- | Specifies caching behavior along the request/reply chain.
gorsCacheControl :: Lens' GetObjectResponse (Maybe Text)
gorsCacheControl = lens _gorsCacheControl (\ s a -> s{_gorsCacheControl = a})

-- | The language the content is in.
gorsContentLanguage :: Lens' GetObjectResponse (Maybe Text)
gorsContentLanguage = lens _gorsContentLanguage (\ s a -> s{_gorsContentLanguage = a})

-- | Last modified date of the object
gorsLastModified :: Lens' GetObjectResponse (Maybe UTCTime)
gorsLastModified = lens _gorsLastModified (\ s a -> s{_gorsLastModified = a}) . mapping _Time

-- | Specifies presentational information for the object.
gorsContentDisposition :: Lens' GetObjectResponse (Maybe Text)
gorsContentDisposition = lens _gorsContentDisposition (\ s a -> s{_gorsContentDisposition = a})

-- | The portion of the object returned in the response.
gorsContentRange :: Lens' GetObjectResponse (Maybe Text)
gorsContentRange = lens _gorsContentRange (\ s a -> s{_gorsContentRange = a})

-- | The Server-side encryption algorithm used when storing this object in S3 (e.g., AES256, aws:kms).
gorsServerSideEncryption :: Lens' GetObjectResponse (Maybe ServerSideEncryption)
gorsServerSideEncryption = lens _gorsServerSideEncryption (\ s a -> s{_gorsServerSideEncryption = a})

-- | A standard MIME type describing the format of the object data.
gorsContentType :: Lens' GetObjectResponse (Maybe Text)
gorsContentType = lens _gorsContentType (\ s a -> s{_gorsContentType = a})

-- | -- | The response status code.
gorsResponseStatus :: Lens' GetObjectResponse Int
gorsResponseStatus = lens _gorsResponseStatus (\ s a -> s{_gorsResponseStatus = a})

-- | Object data.
gorsBody :: Lens' GetObjectResponse RsBody
gorsBody = lens _gorsBody (\ s a -> s{_gorsBody = a})
