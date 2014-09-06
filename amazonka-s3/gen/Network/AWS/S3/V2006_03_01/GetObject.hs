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
    , mkGetObject
    -- ** Request lenses
    , goBucket
    , goIfMatch
    , goIfModifiedSince
    , goIfNoneMatch
    , goIfUnmodifiedSince
    , goKey
    , goRange
    , goResponseCacheControl
    , goResponseContentDisposition
    , goResponseContentEncoding
    , goResponseContentLanguage
    , goResponseContentType
    , goResponseExpires
    , goVersionId
    , goSSECustomerAlgorithm
    , goSSECustomerKey
    , goSSECustomerKeyMD5

    -- * Response
    , GetObjectResponse
    -- ** Response lenses
    , gorsBody
    , gorsDeleteMarker
    , gorsAcceptRanges
    , gorsExpiration
    , gorsRestore
    , gorsLastModified
    , gorsContentLength
    , gorsETag
    , gorsMissingMeta
    , gorsVersionId
    , gorsCacheControl
    , gorsContentDisposition
    , gorsContentEncoding
    , gorsContentLanguage
    , gorsContentType
    , gorsExpires
    , gorsWebsiteRedirectLocation
    , gorsServerSideEncryption
    , gorsMetadata
    , gorsSSECustomerAlgorithm
    , gorsSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data GetObject = GetObject
    { _goBucket :: BucketName
    , _goIfMatch :: Maybe Text
    , _goIfModifiedSince :: Maybe RFC822
    , _goIfNoneMatch :: Maybe Text
    , _goIfUnmodifiedSince :: Maybe RFC822
    , _goKey :: ObjectKey
    , _goRange :: Maybe Text
    , _goResponseCacheControl :: Maybe Text
    , _goResponseContentDisposition :: Maybe Text
    , _goResponseContentEncoding :: Maybe Text
    , _goResponseContentLanguage :: Maybe Text
    , _goResponseContentType :: Maybe Text
    , _goResponseExpires :: Maybe RFC822
    , _goVersionId :: Maybe ObjectVersionId
    , _goSSECustomerAlgorithm :: Maybe Text
    , _goSSECustomerKey :: Maybe Text
    , _goSSECustomerKeyMD5 :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetObject' request.
mkGetObject :: BucketName -- ^ 'goBucket'
            -> ObjectKey -- ^ 'goKey'
            -> GetObject
mkGetObject p1 p6 = GetObject
    { _goBucket = p1
    , _goIfMatch = Nothing
    , _goIfModifiedSince = Nothing
    , _goIfNoneMatch = Nothing
    , _goIfUnmodifiedSince = Nothing
    , _goKey = p6
    , _goRange = Nothing
    , _goResponseCacheControl = Nothing
    , _goResponseContentDisposition = Nothing
    , _goResponseContentEncoding = Nothing
    , _goResponseContentLanguage = Nothing
    , _goResponseContentType = Nothing
    , _goResponseExpires = Nothing
    , _goVersionId = Nothing
    , _goSSECustomerAlgorithm = Nothing
    , _goSSECustomerKey = Nothing
    , _goSSECustomerKeyMD5 = Nothing
    }
{-# INLINE mkGetObject #-}

goBucket :: Lens' GetObject BucketName
goBucket = lens _goBucket (\s a -> s { _goBucket = a })
{-# INLINE goBucket #-}

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
goIfMatch :: Lens' GetObject (Maybe Text)
goIfMatch = lens _goIfMatch (\s a -> s { _goIfMatch = a })
{-# INLINE goIfMatch #-}

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
goIfModifiedSince :: Lens' GetObject (Maybe RFC822)
goIfModifiedSince =
    lens _goIfModifiedSince (\s a -> s { _goIfModifiedSince = a })
{-# INLINE goIfModifiedSince #-}

-- | Return the object only if its entity tag (ETag) is different from the one
-- specified, otherwise return a 304 (not modified).
goIfNoneMatch :: Lens' GetObject (Maybe Text)
goIfNoneMatch = lens _goIfNoneMatch (\s a -> s { _goIfNoneMatch = a })
{-# INLINE goIfNoneMatch #-}

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
goIfUnmodifiedSince :: Lens' GetObject (Maybe RFC822)
goIfUnmodifiedSince =
    lens _goIfUnmodifiedSince (\s a -> s { _goIfUnmodifiedSince = a })
{-# INLINE goIfUnmodifiedSince #-}

goKey :: Lens' GetObject ObjectKey
goKey = lens _goKey (\s a -> s { _goKey = a })
{-# INLINE goKey #-}

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
goRange :: Lens' GetObject (Maybe Text)
goRange = lens _goRange (\s a -> s { _goRange = a })
{-# INLINE goRange #-}

-- | Sets the Cache-Control header of the response.
goResponseCacheControl :: Lens' GetObject (Maybe Text)
goResponseCacheControl =
    lens _goResponseCacheControl (\s a -> s { _goResponseCacheControl = a })
{-# INLINE goResponseCacheControl #-}

-- | Sets the Content-Disposition header of the response.
goResponseContentDisposition :: Lens' GetObject (Maybe Text)
goResponseContentDisposition =
    lens _goResponseContentDisposition
         (\s a -> s { _goResponseContentDisposition = a })
{-# INLINE goResponseContentDisposition #-}

-- | Sets the Content-Encoding header of the response.
goResponseContentEncoding :: Lens' GetObject (Maybe Text)
goResponseContentEncoding =
    lens _goResponseContentEncoding
         (\s a -> s { _goResponseContentEncoding = a })
{-# INLINE goResponseContentEncoding #-}

-- | Sets the Content-Language header of the response.
goResponseContentLanguage :: Lens' GetObject (Maybe Text)
goResponseContentLanguage =
    lens _goResponseContentLanguage
         (\s a -> s { _goResponseContentLanguage = a })
{-# INLINE goResponseContentLanguage #-}

-- | Sets the Content-Type header of the response.
goResponseContentType :: Lens' GetObject (Maybe Text)
goResponseContentType =
    lens _goResponseContentType (\s a -> s { _goResponseContentType = a })
{-# INLINE goResponseContentType #-}

-- | Sets the Expires header of the response.
goResponseExpires :: Lens' GetObject (Maybe RFC822)
goResponseExpires =
    lens _goResponseExpires (\s a -> s { _goResponseExpires = a })
{-# INLINE goResponseExpires #-}

-- | VersionId used to reference a specific version of the object.
goVersionId :: Lens' GetObject (Maybe ObjectVersionId)
goVersionId = lens _goVersionId (\s a -> s { _goVersionId = a })
{-# INLINE goVersionId #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
goSSECustomerAlgorithm :: Lens' GetObject (Maybe Text)
goSSECustomerAlgorithm =
    lens _goSSECustomerAlgorithm (\s a -> s { _goSSECustomerAlgorithm = a })
{-# INLINE goSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
goSSECustomerKey :: Lens' GetObject (Maybe Text)
goSSECustomerKey =
    lens _goSSECustomerKey (\s a -> s { _goSSECustomerKey = a })
{-# INLINE goSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
goSSECustomerKeyMD5 :: Lens' GetObject (Maybe Text)
goSSECustomerKeyMD5 =
    lens _goSSECustomerKeyMD5 (\s a -> s { _goSSECustomerKeyMD5 = a })
{-# INLINE goSSECustomerKeyMD5 #-}

instance ToPath GetObject where
    toPath GetObject{..} = mconcat
        [ "/"
        , toBS _goBucket
        , "/"
        , toBS _goKey
        ]

instance ToQuery GetObject where
    toQuery GetObject{..} = mconcat
        [ "response-cache-control" =? _goResponseCacheControl
        , "response-content-disposition" =? _goResponseContentDisposition
        , "response-content-encoding" =? _goResponseContentEncoding
        , "response-content-language" =? _goResponseContentLanguage
        , "response-content-type" =? _goResponseContentType
        , "response-expires" =? _goResponseExpires
        , "versionId" =? _goVersionId
        ]

instance ToHeaders GetObject where
    toHeaders GetObject{..} = concat
        [ "If-Match" =: _goIfMatch
        , "If-Modified-Since" =: _goIfModifiedSince
        , "If-None-Match" =: _goIfNoneMatch
        , "If-Unmodified-Since" =: _goIfUnmodifiedSince
        , "Range" =: _goRange
        , "x-amz-server-side-encryption-customer-algorithm" =: _goSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _goSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _goSSECustomerKeyMD5
        ]

instance ToBody GetObject

data GetObjectResponse = GetObjectResponse
    { _gorsBody :: RsBody
    , _gorsDeleteMarker :: Maybe Bool
    , _gorsAcceptRanges :: Maybe Text
    , _gorsExpiration :: Maybe RFC822
    , _gorsRestore :: Maybe Text
    , _gorsLastModified :: Maybe RFC822
    , _gorsContentLength :: Maybe Integer
    , _gorsETag :: Maybe ETag
    , _gorsMissingMeta :: Maybe Integer
    , _gorsVersionId :: Maybe ObjectVersionId
    , _gorsCacheControl :: Maybe Text
    , _gorsContentDisposition :: Maybe Text
    , _gorsContentEncoding :: Maybe Text
    , _gorsContentLanguage :: Maybe Text
    , _gorsContentType :: Maybe Text
    , _gorsExpires :: Maybe RFC822
    , _gorsWebsiteRedirectLocation :: Maybe Text
    , _gorsServerSideEncryption :: Maybe ServerSideEncryption
    , _gorsMetadata :: Map Text Text
    , _gorsSSECustomerAlgorithm :: Maybe Text
    , _gorsSSECustomerKeyMD5 :: Maybe Text
    } deriving (Show, Generic)

-- | Object data.
gorsBody :: Lens' GetObjectResponse RsBody
gorsBody = lens _gorsBody (\s a -> s { _gorsBody = a })
{-# INLINE gorsBody #-}

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
gorsDeleteMarker :: Lens' GetObjectResponse (Maybe Bool)
gorsDeleteMarker =
    lens _gorsDeleteMarker (\s a -> s { _gorsDeleteMarker = a })
{-# INLINE gorsDeleteMarker #-}

gorsAcceptRanges :: Lens' GetObjectResponse (Maybe Text)
gorsAcceptRanges =
    lens _gorsAcceptRanges (\s a -> s { _gorsAcceptRanges = a })
{-# INLINE gorsAcceptRanges #-}

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id key
-- value pairs providing object expiration information. The value of the
-- rule-id is URL encoded.
gorsExpiration :: Lens' GetObjectResponse (Maybe RFC822)
gorsExpiration = lens _gorsExpiration (\s a -> s { _gorsExpiration = a })
{-# INLINE gorsExpiration #-}

-- | Provides information about object restoration operation and expiration time
-- of the restored object copy.
gorsRestore :: Lens' GetObjectResponse (Maybe Text)
gorsRestore = lens _gorsRestore (\s a -> s { _gorsRestore = a })
{-# INLINE gorsRestore #-}

-- | Last modified date of the object.
gorsLastModified :: Lens' GetObjectResponse (Maybe RFC822)
gorsLastModified =
    lens _gorsLastModified (\s a -> s { _gorsLastModified = a })
{-# INLINE gorsLastModified #-}

-- | Size of the body in bytes.
gorsContentLength :: Lens' GetObjectResponse (Maybe Integer)
gorsContentLength =
    lens _gorsContentLength (\s a -> s { _gorsContentLength = a })
{-# INLINE gorsContentLength #-}

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
gorsETag :: Lens' GetObjectResponse (Maybe ETag)
gorsETag = lens _gorsETag (\s a -> s { _gorsETag = a })
{-# INLINE gorsETag #-}

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP that
-- supports more flexible metadata than the REST API. For example, using SOAP,
-- you can create metadata whose values are not legal HTTP headers.
gorsMissingMeta :: Lens' GetObjectResponse (Maybe Integer)
gorsMissingMeta = lens _gorsMissingMeta (\s a -> s { _gorsMissingMeta = a })
{-# INLINE gorsMissingMeta #-}

-- | Version of the object.
gorsVersionId :: Lens' GetObjectResponse (Maybe ObjectVersionId)
gorsVersionId = lens _gorsVersionId (\s a -> s { _gorsVersionId = a })
{-# INLINE gorsVersionId #-}

-- | Specifies caching behavior along the request/reply chain.
gorsCacheControl :: Lens' GetObjectResponse (Maybe Text)
gorsCacheControl =
    lens _gorsCacheControl (\s a -> s { _gorsCacheControl = a })
{-# INLINE gorsCacheControl #-}

-- | Specifies presentational information for the object.
gorsContentDisposition :: Lens' GetObjectResponse (Maybe Text)
gorsContentDisposition =
    lens _gorsContentDisposition (\s a -> s { _gorsContentDisposition = a })
{-# INLINE gorsContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
gorsContentEncoding :: Lens' GetObjectResponse (Maybe Text)
gorsContentEncoding =
    lens _gorsContentEncoding (\s a -> s { _gorsContentEncoding = a })
{-# INLINE gorsContentEncoding #-}

-- | The language the content is in.
gorsContentLanguage :: Lens' GetObjectResponse (Maybe Text)
gorsContentLanguage =
    lens _gorsContentLanguage (\s a -> s { _gorsContentLanguage = a })
{-# INLINE gorsContentLanguage #-}

-- | A standard MIME type describing the format of the object data.
gorsContentType :: Lens' GetObjectResponse (Maybe Text)
gorsContentType = lens _gorsContentType (\s a -> s { _gorsContentType = a })
{-# INLINE gorsContentType #-}

-- | The date and time at which the object is no longer cacheable.
gorsExpires :: Lens' GetObjectResponse (Maybe RFC822)
gorsExpires = lens _gorsExpires (\s a -> s { _gorsExpires = a })
{-# INLINE gorsExpires #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
gorsWebsiteRedirectLocation :: Lens' GetObjectResponse (Maybe Text)
gorsWebsiteRedirectLocation =
    lens _gorsWebsiteRedirectLocation
         (\s a -> s { _gorsWebsiteRedirectLocation = a })
{-# INLINE gorsWebsiteRedirectLocation #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
gorsServerSideEncryption :: Lens' GetObjectResponse (Maybe ServerSideEncryption)
gorsServerSideEncryption =
    lens _gorsServerSideEncryption
         (\s a -> s { _gorsServerSideEncryption = a })
{-# INLINE gorsServerSideEncryption #-}

-- | A map of metadata to store with the object in S3.
gorsMetadata :: Lens' GetObjectResponse (Map Text Text)
gorsMetadata = lens _gorsMetadata (\s a -> s { _gorsMetadata = a })
{-# INLINE gorsMetadata #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
gorsSSECustomerAlgorithm :: Lens' GetObjectResponse (Maybe Text)
gorsSSECustomerAlgorithm =
    lens _gorsSSECustomerAlgorithm
         (\s a -> s { _gorsSSECustomerAlgorithm = a })
{-# INLINE gorsSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
gorsSSECustomerKeyMD5 :: Lens' GetObjectResponse (Maybe Text)
gorsSSECustomerKeyMD5 =
    lens _gorsSSECustomerKeyMD5 (\s a -> s { _gorsSSECustomerKeyMD5 = a })
{-# INLINE gorsSSECustomerKeyMD5 #-}

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
