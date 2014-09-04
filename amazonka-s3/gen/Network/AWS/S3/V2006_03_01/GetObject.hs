{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves objects from Amazon S3.
module Network.AWS.S3.V2006_03_01.GetObject
    (
    -- * Request
      GetObject
    -- ** Request constructor
    , mkGetObjectRequest
    -- ** Request lenses
    , gorBucket
    , gorIfMatch
    , gorIfModifiedSince
    , gorIfNoneMatch
    , gorIfUnmodifiedSince
    , gorKey
    , gorRange
    , gorResponseCacheControl
    , gorResponseContentDisposition
    , gorResponseContentEncoding
    , gorResponseContentLanguage
    , gorResponseContentType
    , gorResponseExpires
    , gorVersionId
    , gorSSECustomerAlgorithm
    , gorSSECustomerKey
    , gorSSECustomerKeyMD5

    -- * Response
    , GetObjectResponse
    -- ** Response lenses
    , gooBody
    , gooDeleteMarker
    , gooAcceptRanges
    , gooExpiration
    , gooRestore
    , gooLastModified
    , gooContentLength
    , gooETag
    , gooMissingMeta
    , gooVersionId
    , gooCacheControl
    , gooContentDisposition
    , gooContentEncoding
    , gooContentLanguage
    , gooContentType
    , gooExpires
    , gooWebsiteRedirectLocation
    , gooServerSideEncryption
    , gooMetadata
    , gooSSECustomerAlgorithm
    , gooSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetObject' request.
mkGetObjectRequest :: BucketName -- ^ 'gorBucket'
                   -> ObjectKey -- ^ 'gorKey'
                   -> GetObject
mkGetObjectRequest p1 p2 = GetObject
    { _gorBucket = p1
    , _gorIfMatch = Nothing
    , _gorIfModifiedSince = Nothing
    , _gorIfNoneMatch = Nothing
    , _gorIfUnmodifiedSince = Nothing
    , _gorKey = p6
    , _gorRange = Nothing
    , _gorResponseCacheControl = Nothing
    , _gorResponseContentDisposition = Nothing
    , _gorResponseContentEncoding = Nothing
    , _gorResponseContentLanguage = Nothing
    , _gorResponseContentType = Nothing
    , _gorResponseExpires = Nothing
    , _gorVersionId = Nothing
    , _gorSSECustomerAlgorithm = Nothing
    , _gorSSECustomerKey = Nothing
    , _gorSSECustomerKeyMD5 = Nothing
    }
{-# INLINE mkGetObjectRequest #-}

data GetObject = GetObject
    { _gorBucket :: BucketName
    , _gorIfMatch :: Maybe Text
      -- ^ Return the object only if its entity tag (ETag) is the same as
      -- the one specified, otherwise return a 412 (precondition failed).
    , _gorIfModifiedSince :: Maybe RFC822
      -- ^ Return the object only if it has been modified since the
      -- specified time, otherwise return a 304 (not modified).
    , _gorIfNoneMatch :: Maybe Text
      -- ^ Return the object only if its entity tag (ETag) is different from
      -- the one specified, otherwise return a 304 (not modified).
    , _gorIfUnmodifiedSince :: Maybe RFC822
      -- ^ Return the object only if it has not been modified since the
      -- specified time, otherwise return a 412 (precondition failed).
    , _gorKey :: ObjectKey
    , _gorRange :: Maybe Text
      -- ^ Downloads the specified range bytes of an object. For more
      -- information about the HTTP Range header, go to
      -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
    , _gorResponseCacheControl :: Maybe Text
      -- ^ Sets the Cache-Control header of the response.
    , _gorResponseContentDisposition :: Maybe Text
      -- ^ Sets the Content-Disposition header of the response.
    , _gorResponseContentEncoding :: Maybe Text
      -- ^ Sets the Content-Encoding header of the response.
    , _gorResponseContentLanguage :: Maybe Text
      -- ^ Sets the Content-Language header of the response.
    , _gorResponseContentType :: Maybe Text
      -- ^ Sets the Content-Type header of the response.
    , _gorResponseExpires :: Maybe RFC822
      -- ^ Sets the Expires header of the response.
    , _gorVersionId :: Maybe ObjectVersionId
      -- ^ VersionId used to reference a specific version of the object.
    , _gorSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , _gorSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header.
    , _gorSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    } deriving (Show, Generic)

gorBucket :: Lens' GetObject (BucketName)
gorBucket = lens _gorBucket (\s a -> s { _gorBucket = a })
{-# INLINE gorBucket #-}

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
gorIfMatch :: Lens' GetObject (Maybe Text)
gorIfMatch = lens _gorIfMatch (\s a -> s { _gorIfMatch = a })
{-# INLINE gorIfMatch #-}

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
gorIfModifiedSince :: Lens' GetObject (Maybe RFC822)
gorIfModifiedSince = lens _gorIfModifiedSince (\s a -> s { _gorIfModifiedSince = a })
{-# INLINE gorIfModifiedSince #-}

-- | Return the object only if its entity tag (ETag) is different from the one
-- specified, otherwise return a 304 (not modified).
gorIfNoneMatch :: Lens' GetObject (Maybe Text)
gorIfNoneMatch = lens _gorIfNoneMatch (\s a -> s { _gorIfNoneMatch = a })
{-# INLINE gorIfNoneMatch #-}

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
gorIfUnmodifiedSince :: Lens' GetObject (Maybe RFC822)
gorIfUnmodifiedSince = lens _gorIfUnmodifiedSince (\s a -> s { _gorIfUnmodifiedSince = a })
{-# INLINE gorIfUnmodifiedSince #-}

gorKey :: Lens' GetObject (ObjectKey)
gorKey = lens _gorKey (\s a -> s { _gorKey = a })
{-# INLINE gorKey #-}

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
gorRange :: Lens' GetObject (Maybe Text)
gorRange = lens _gorRange (\s a -> s { _gorRange = a })
{-# INLINE gorRange #-}

-- | Sets the Cache-Control header of the response.
gorResponseCacheControl :: Lens' GetObject (Maybe Text)
gorResponseCacheControl = lens _gorResponseCacheControl (\s a -> s { _gorResponseCacheControl = a })
{-# INLINE gorResponseCacheControl #-}

-- | Sets the Content-Disposition header of the response.
gorResponseContentDisposition :: Lens' GetObject (Maybe Text)
gorResponseContentDisposition = lens _gorResponseContentDisposition (\s a -> s { _gorResponseContentDisposition = a })
{-# INLINE gorResponseContentDisposition #-}

-- | Sets the Content-Encoding header of the response.
gorResponseContentEncoding :: Lens' GetObject (Maybe Text)
gorResponseContentEncoding = lens _gorResponseContentEncoding (\s a -> s { _gorResponseContentEncoding = a })
{-# INLINE gorResponseContentEncoding #-}

-- | Sets the Content-Language header of the response.
gorResponseContentLanguage :: Lens' GetObject (Maybe Text)
gorResponseContentLanguage = lens _gorResponseContentLanguage (\s a -> s { _gorResponseContentLanguage = a })
{-# INLINE gorResponseContentLanguage #-}

-- | Sets the Content-Type header of the response.
gorResponseContentType :: Lens' GetObject (Maybe Text)
gorResponseContentType = lens _gorResponseContentType (\s a -> s { _gorResponseContentType = a })
{-# INLINE gorResponseContentType #-}

-- | Sets the Expires header of the response.
gorResponseExpires :: Lens' GetObject (Maybe RFC822)
gorResponseExpires = lens _gorResponseExpires (\s a -> s { _gorResponseExpires = a })
{-# INLINE gorResponseExpires #-}

-- | VersionId used to reference a specific version of the object.
gorVersionId :: Lens' GetObject (Maybe ObjectVersionId)
gorVersionId = lens _gorVersionId (\s a -> s { _gorVersionId = a })
{-# INLINE gorVersionId #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
gorSSECustomerAlgorithm :: Lens' GetObject (Maybe Text)
gorSSECustomerAlgorithm = lens _gorSSECustomerAlgorithm (\s a -> s { _gorSSECustomerAlgorithm = a })
{-# INLINE gorSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
gorSSECustomerKey :: Lens' GetObject (Maybe Text)
gorSSECustomerKey = lens _gorSSECustomerKey (\s a -> s { _gorSSECustomerKey = a })
{-# INLINE gorSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
gorSSECustomerKeyMD5 :: Lens' GetObject (Maybe Text)
gorSSECustomerKeyMD5 = lens _gorSSECustomerKeyMD5 (\s a -> s { _gorSSECustomerKeyMD5 = a })
{-# INLINE gorSSECustomerKeyMD5 #-}

instance ToPath GetObject where
    toPath GetObject{..} = mconcat
        [ "/"
        , toBS _gorBucket
        , "/"
        , toBS _gorKey
        ]

instance ToQuery GetObject where
    toQuery GetObject{..} = mconcat
        [ "response-cache-control" =? _gorResponseCacheControl
        , "response-content-disposition" =? _gorResponseContentDisposition
        , "response-content-encoding" =? _gorResponseContentEncoding
        , "response-content-language" =? _gorResponseContentLanguage
        , "response-content-type" =? _gorResponseContentType
        , "response-expires" =? _gorResponseExpires
        , "versionId" =? _gorVersionId
        ]

instance ToHeaders GetObject

instance ToBody GetObject

data GetObjectResponse = GetObjectResponse
    { _gooBody :: RsBody
      -- ^ Object data.
    , _gooDeleteMarker :: Maybe Bool
      -- ^ Specifies whether the object retrieved was (true) or was not
      -- (false) a Delete Marker. If false, this response header does not
      -- appear in the response.
    , _gooAcceptRanges :: Maybe Text
    , _gooExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured (see PUT Bucket
      -- lifecycle), the response includes this header. It includes the
      -- expiry-date and rule-id key value pairs providing object
      -- expiration information. The value of the rule-id is URL encoded.
    , _gooRestore :: Maybe Text
      -- ^ Provides information about object restoration operation and
      -- expiration time of the restored object copy.
    , _gooLastModified :: Maybe RFC822
      -- ^ Last modified date of the object.
    , _gooContentLength :: Maybe Integer
      -- ^ Size of the body in bytes.
    , _gooETag :: Maybe ETag
      -- ^ An ETag is an opaque identifier assigned by a web server to a
      -- specific version of a resource found at a URL.
    , _gooMissingMeta :: Maybe Integer
      -- ^ This is set to the number of metadata entries not returned in
      -- x-amz-meta headers. This can happen if you create metadata using
      -- an API like SOAP that supports more flexible metadata than the
      -- REST API. For example, using SOAP, you can create metadata whose
      -- values are not legal HTTP headers.
    , _gooVersionId :: Maybe ObjectVersionId
      -- ^ Version of the object.
    , _gooCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , _gooContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , _gooContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object
      -- and thus what decoding mechanisms must be applied to obtain the
      -- media-type referenced by the Content-Type header field.
    , _gooContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , _gooContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , _gooExpires :: Maybe RFC822
      -- ^ The date and time at which the object is no longer cacheable.
    , _gooWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for
      -- this object to another object in the same bucket or to an
      -- external URL. Amazon S3 stores the value of this header in the
      -- object metadata.
    , _gooServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _gooMetadata :: Map Text Text
      -- ^ A map of metadata to store with the object in S3.
    , _gooSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _gooSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    } deriving (Show, Generic)

-- | Object data.
gooBody :: Lens' GetObjectResponse (RsBody)
gooBody = lens _gooBody (\s a -> s { _gooBody = a })
{-# INLINE gooBody #-}

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
gooDeleteMarker :: Lens' GetObjectResponse (Maybe Bool)
gooDeleteMarker = lens _gooDeleteMarker (\s a -> s { _gooDeleteMarker = a })
{-# INLINE gooDeleteMarker #-}

gooAcceptRanges :: Lens' GetObjectResponse (Maybe Text)
gooAcceptRanges = lens _gooAcceptRanges (\s a -> s { _gooAcceptRanges = a })
{-# INLINE gooAcceptRanges #-}

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id key
-- value pairs providing object expiration information. The value of the
-- rule-id is URL encoded.
gooExpiration :: Lens' GetObjectResponse (Maybe RFC822)
gooExpiration = lens _gooExpiration (\s a -> s { _gooExpiration = a })
{-# INLINE gooExpiration #-}

-- | Provides information about object restoration operation and expiration time
-- of the restored object copy.
gooRestore :: Lens' GetObjectResponse (Maybe Text)
gooRestore = lens _gooRestore (\s a -> s { _gooRestore = a })
{-# INLINE gooRestore #-}

-- | Last modified date of the object.
gooLastModified :: Lens' GetObjectResponse (Maybe RFC822)
gooLastModified = lens _gooLastModified (\s a -> s { _gooLastModified = a })
{-# INLINE gooLastModified #-}

-- | Size of the body in bytes.
gooContentLength :: Lens' GetObjectResponse (Maybe Integer)
gooContentLength = lens _gooContentLength (\s a -> s { _gooContentLength = a })
{-# INLINE gooContentLength #-}

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
gooETag :: Lens' GetObjectResponse (Maybe ETag)
gooETag = lens _gooETag (\s a -> s { _gooETag = a })
{-# INLINE gooETag #-}

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP that
-- supports more flexible metadata than the REST API. For example, using SOAP,
-- you can create metadata whose values are not legal HTTP headers.
gooMissingMeta :: Lens' GetObjectResponse (Maybe Integer)
gooMissingMeta = lens _gooMissingMeta (\s a -> s { _gooMissingMeta = a })
{-# INLINE gooMissingMeta #-}

-- | Version of the object.
gooVersionId :: Lens' GetObjectResponse (Maybe ObjectVersionId)
gooVersionId = lens _gooVersionId (\s a -> s { _gooVersionId = a })
{-# INLINE gooVersionId #-}

-- | Specifies caching behavior along the request/reply chain.
gooCacheControl :: Lens' GetObjectResponse (Maybe Text)
gooCacheControl = lens _gooCacheControl (\s a -> s { _gooCacheControl = a })
{-# INLINE gooCacheControl #-}

-- | Specifies presentational information for the object.
gooContentDisposition :: Lens' GetObjectResponse (Maybe Text)
gooContentDisposition = lens _gooContentDisposition (\s a -> s { _gooContentDisposition = a })
{-# INLINE gooContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
gooContentEncoding :: Lens' GetObjectResponse (Maybe Text)
gooContentEncoding = lens _gooContentEncoding (\s a -> s { _gooContentEncoding = a })
{-# INLINE gooContentEncoding #-}

-- | The language the content is in.
gooContentLanguage :: Lens' GetObjectResponse (Maybe Text)
gooContentLanguage = lens _gooContentLanguage (\s a -> s { _gooContentLanguage = a })
{-# INLINE gooContentLanguage #-}

-- | A standard MIME type describing the format of the object data.
gooContentType :: Lens' GetObjectResponse (Maybe Text)
gooContentType = lens _gooContentType (\s a -> s { _gooContentType = a })
{-# INLINE gooContentType #-}

-- | The date and time at which the object is no longer cacheable.
gooExpires :: Lens' GetObjectResponse (Maybe RFC822)
gooExpires = lens _gooExpires (\s a -> s { _gooExpires = a })
{-# INLINE gooExpires #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
gooWebsiteRedirectLocation :: Lens' GetObjectResponse (Maybe Text)
gooWebsiteRedirectLocation = lens _gooWebsiteRedirectLocation (\s a -> s { _gooWebsiteRedirectLocation = a })
{-# INLINE gooWebsiteRedirectLocation #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
gooServerSideEncryption :: Lens' GetObjectResponse (Maybe ServerSideEncryption)
gooServerSideEncryption = lens _gooServerSideEncryption (\s a -> s { _gooServerSideEncryption = a })
{-# INLINE gooServerSideEncryption #-}

-- | A map of metadata to store with the object in S3.
gooMetadata :: Lens' GetObjectResponse (Map Text Text)
gooMetadata = lens _gooMetadata (\s a -> s { _gooMetadata = a })
{-# INLINE gooMetadata #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
gooSSECustomerAlgorithm :: Lens' GetObjectResponse (Maybe Text)
gooSSECustomerAlgorithm = lens _gooSSECustomerAlgorithm (\s a -> s { _gooSSECustomerAlgorithm = a })
{-# INLINE gooSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
gooSSECustomerKeyMD5 :: Lens' GetObjectResponse (Maybe Text)
gooSSECustomerKeyMD5 = lens _gooSSECustomerKeyMD5 (\s a -> s { _gooSSECustomerKeyMD5 = a })
{-# INLINE gooSSECustomerKeyMD5 #-}

instance AWSRequest GetObject where
    type Sv GetObject = S3
    type Rs GetObject = GetObjectResponse

    request = get
    response _ = bodyResponse $ \hs bdy ->
        return $! pure GetObjectResponse
            <*> pure (RsBody bdy)
            <*> hs ~:? "x-amz-delete-marker"
            <*> hs ~:? "accept-ranges"
            <*> hs ~:? "x-amz-expiration"
            <*> hs ~:? "x-amz-restore"
            <*> hs ~:? "Last-Modified"
            <*> hs ~:? "Content-Length"
            <*> hs ~:? "ETag"
            <*> hs ~:? "x-amz-missing-meta"
            <*> hs ~:? "x-amz-version-id"
            <*> hs ~:? "Cache-Control"
            <*> hs ~:? "Content-Disposition"
            <*> hs ~:? "Content-Encoding"
            <*> hs ~:? "Content-Language"
            <*> hs ~:? "Content-Type"
            <*> hs ~:? "Expires"
            <*> hs ~:? "x-amz-website-redirect-location"
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> (Map <$> hs ~:: "x-amz-meta-")
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
