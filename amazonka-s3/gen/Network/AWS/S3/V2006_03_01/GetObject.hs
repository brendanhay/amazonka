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
    , getObject
    -- ** Request lenses
    , gorBucket
    , gorKey
    , gorIfMatch
    , gorIfModifiedSince
    , gorIfNoneMatch
    , gorIfUnmodifiedSince
    , gorRange
    , gorSSECustomerAlgorithm
    , gorSSECustomerKey
    , gorSSECustomerKeyMD5
    , gorVersionId
    , gorResponseCacheControl
    , gorResponseContentDisposition
    , gorResponseContentEncoding
    , gorResponseContentLanguage
    , gorResponseContentType
    , gorResponseExpires

    -- * Response
    , GetObjectResponse
    -- ** Response lenses
    , gooBody
    , gooAcceptRanges
    , gooCacheControl
    , gooContentDisposition
    , gooContentEncoding
    , gooContentLanguage
    , gooContentLength
    , gooContentType
    , gooDeleteMarker
    , gooETag
    , gooExpiration
    , gooExpires
    , gooLastModified
    , gooMetadata
    , gooMissingMeta
    , gooVersionId
    , gooRestore
    , gooSSECustomerAlgorithm
    , gooSSECustomerKeyMD5
    , gooServerSideEncryption
    , gooWebsiteRedirectLocation
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetObject' request.
getObject :: BucketName -- ^ 'gorBucket'
          -> ObjectKey -- ^ 'gorKey'
          -> GetObject
getObject p1 p2 = GetObject
    { _gorBucket = p1
    , _gorKey = p2
    , _gorIfMatch = Nothing
    , _gorIfModifiedSince = Nothing
    , _gorIfNoneMatch = Nothing
    , _gorIfUnmodifiedSince = Nothing
    , _gorRange = Nothing
    , _gorSSECustomerAlgorithm = Nothing
    , _gorSSECustomerKey = Nothing
    , _gorSSECustomerKeyMD5 = Nothing
    , _gorVersionId = Nothing
    , _gorResponseCacheControl = Nothing
    , _gorResponseContentDisposition = Nothing
    , _gorResponseContentEncoding = Nothing
    , _gorResponseContentLanguage = Nothing
    , _gorResponseContentType = Nothing
    , _gorResponseExpires = Nothing
    }
{-# INLINE getObject #-}

data GetObject = GetObject
    { _gorBucket :: BucketName
    , _gorKey :: ObjectKey
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
    , _gorRange :: Maybe Text
      -- ^ Downloads the specified range bytes of an object. For more
      -- information about the HTTP Range header, go to
      -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
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
    , _gorVersionId :: Maybe ObjectVersionId
      -- ^ VersionId used to reference a specific version of the object.
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
    } deriving (Show, Generic)

gorBucket :: Lens' GetObject (BucketName)
gorBucket f x =
    f (_gorBucket x)
        <&> \y -> x { _gorBucket = y }
{-# INLINE gorBucket #-}

gorKey :: Lens' GetObject (ObjectKey)
gorKey f x =
    f (_gorKey x)
        <&> \y -> x { _gorKey = y }
{-# INLINE gorKey #-}

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
gorIfMatch :: Lens' GetObject (Maybe Text)
gorIfMatch f x =
    f (_gorIfMatch x)
        <&> \y -> x { _gorIfMatch = y }
{-# INLINE gorIfMatch #-}

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
gorIfModifiedSince :: Lens' GetObject (Maybe RFC822)
gorIfModifiedSince f x =
    f (_gorIfModifiedSince x)
        <&> \y -> x { _gorIfModifiedSince = y }
{-# INLINE gorIfModifiedSince #-}

-- | Return the object only if its entity tag (ETag) is different from the one
-- specified, otherwise return a 304 (not modified).
gorIfNoneMatch :: Lens' GetObject (Maybe Text)
gorIfNoneMatch f x =
    f (_gorIfNoneMatch x)
        <&> \y -> x { _gorIfNoneMatch = y }
{-# INLINE gorIfNoneMatch #-}

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
gorIfUnmodifiedSince :: Lens' GetObject (Maybe RFC822)
gorIfUnmodifiedSince f x =
    f (_gorIfUnmodifiedSince x)
        <&> \y -> x { _gorIfUnmodifiedSince = y }
{-# INLINE gorIfUnmodifiedSince #-}

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
gorRange :: Lens' GetObject (Maybe Text)
gorRange f x =
    f (_gorRange x)
        <&> \y -> x { _gorRange = y }
{-# INLINE gorRange #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
gorSSECustomerAlgorithm :: Lens' GetObject (Maybe Text)
gorSSECustomerAlgorithm f x =
    f (_gorSSECustomerAlgorithm x)
        <&> \y -> x { _gorSSECustomerAlgorithm = y }
{-# INLINE gorSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
gorSSECustomerKey :: Lens' GetObject (Maybe Text)
gorSSECustomerKey f x =
    f (_gorSSECustomerKey x)
        <&> \y -> x { _gorSSECustomerKey = y }
{-# INLINE gorSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
gorSSECustomerKeyMD5 :: Lens' GetObject (Maybe Text)
gorSSECustomerKeyMD5 f x =
    f (_gorSSECustomerKeyMD5 x)
        <&> \y -> x { _gorSSECustomerKeyMD5 = y }
{-# INLINE gorSSECustomerKeyMD5 #-}

-- | VersionId used to reference a specific version of the object.
gorVersionId :: Lens' GetObject (Maybe ObjectVersionId)
gorVersionId f x =
    f (_gorVersionId x)
        <&> \y -> x { _gorVersionId = y }
{-# INLINE gorVersionId #-}

-- | Sets the Cache-Control header of the response.
gorResponseCacheControl :: Lens' GetObject (Maybe Text)
gorResponseCacheControl f x =
    f (_gorResponseCacheControl x)
        <&> \y -> x { _gorResponseCacheControl = y }
{-# INLINE gorResponseCacheControl #-}

-- | Sets the Content-Disposition header of the response.
gorResponseContentDisposition :: Lens' GetObject (Maybe Text)
gorResponseContentDisposition f x =
    f (_gorResponseContentDisposition x)
        <&> \y -> x { _gorResponseContentDisposition = y }
{-# INLINE gorResponseContentDisposition #-}

-- | Sets the Content-Encoding header of the response.
gorResponseContentEncoding :: Lens' GetObject (Maybe Text)
gorResponseContentEncoding f x =
    f (_gorResponseContentEncoding x)
        <&> \y -> x { _gorResponseContentEncoding = y }
{-# INLINE gorResponseContentEncoding #-}

-- | Sets the Content-Language header of the response.
gorResponseContentLanguage :: Lens' GetObject (Maybe Text)
gorResponseContentLanguage f x =
    f (_gorResponseContentLanguage x)
        <&> \y -> x { _gorResponseContentLanguage = y }
{-# INLINE gorResponseContentLanguage #-}

-- | Sets the Content-Type header of the response.
gorResponseContentType :: Lens' GetObject (Maybe Text)
gorResponseContentType f x =
    f (_gorResponseContentType x)
        <&> \y -> x { _gorResponseContentType = y }
{-# INLINE gorResponseContentType #-}

-- | Sets the Expires header of the response.
gorResponseExpires :: Lens' GetObject (Maybe RFC822)
gorResponseExpires f x =
    f (_gorResponseExpires x)
        <&> \y -> x { _gorResponseExpires = y }
{-# INLINE gorResponseExpires #-}

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

instance ToHeaders GetObject where
    toHeaders GetObject{..} = concat
        [ "If-Match" =: _gorIfMatch
        , "If-Modified-Since" =: _gorIfModifiedSince
        , "If-None-Match" =: _gorIfNoneMatch
        , "If-Unmodified-Since" =: _gorIfUnmodifiedSince
        , "Range" =: _gorRange
        , "x-amz-server-side-encryption-customer-algorithm" =: _gorSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _gorSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _gorSSECustomerKeyMD5
        ]

instance ToBody GetObject

data GetObjectResponse = GetObjectResponse
    { _gooBody :: RsBody
      -- ^ Object data.
    , _gooAcceptRanges :: Maybe Text
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
    , _gooContentLength :: Maybe Integer
      -- ^ Size of the body in bytes.
    , _gooContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , _gooDeleteMarker :: Maybe Bool
      -- ^ Specifies whether the object retrieved was (true) or was not
      -- (false) a Delete Marker. If false, this response header does not
      -- appear in the response.
    , _gooETag :: Maybe ETag
      -- ^ An ETag is an opaque identifier assigned by a web server to a
      -- specific version of a resource found at a URL.
    , _gooExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured (see PUT Bucket
      -- lifecycle), the response includes this header. It includes the
      -- expiry-date and rule-id key value pairs providing object
      -- expiration information. The value of the rule-id is URL encoded.
    , _gooExpires :: Maybe RFC822
      -- ^ The date and time at which the object is no longer cacheable.
    , _gooLastModified :: Maybe RFC822
      -- ^ Last modified date of the object.
    , _gooMetadata :: Map Text Text
      -- ^ A map of metadata to store with the object in S3.
    , _gooMissingMeta :: Maybe Integer
      -- ^ This is set to the number of metadata entries not returned in
      -- x-amz-meta headers. This can happen if you create metadata using
      -- an API like SOAP that supports more flexible metadata than the
      -- REST API. For example, using SOAP, you can create metadata whose
      -- values are not legal HTTP headers.
    , _gooVersionId :: Maybe ObjectVersionId
      -- ^ Version of the object.
    , _gooRestore :: Maybe Text
      -- ^ Provides information about object restoration operation and
      -- expiration time of the restored object copy.
    , _gooSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _gooSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , _gooServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _gooWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for
      -- this object to another object in the same bucket or to an
      -- external URL. Amazon S3 stores the value of this header in the
      -- object metadata.
    } deriving (Show, Generic)

-- | Object data.
gooBody :: Lens' GetObjectResponse (RsBody)
gooBody f x =
    f (_gooBody x)
        <&> \y -> x { _gooBody = y }
{-# INLINE gooBody #-}

gooAcceptRanges :: Lens' GetObjectResponse (Maybe Text)
gooAcceptRanges f x =
    f (_gooAcceptRanges x)
        <&> \y -> x { _gooAcceptRanges = y }
{-# INLINE gooAcceptRanges #-}

-- | Specifies caching behavior along the request/reply chain.
gooCacheControl :: Lens' GetObjectResponse (Maybe Text)
gooCacheControl f x =
    f (_gooCacheControl x)
        <&> \y -> x { _gooCacheControl = y }
{-# INLINE gooCacheControl #-}

-- | Specifies presentational information for the object.
gooContentDisposition :: Lens' GetObjectResponse (Maybe Text)
gooContentDisposition f x =
    f (_gooContentDisposition x)
        <&> \y -> x { _gooContentDisposition = y }
{-# INLINE gooContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
gooContentEncoding :: Lens' GetObjectResponse (Maybe Text)
gooContentEncoding f x =
    f (_gooContentEncoding x)
        <&> \y -> x { _gooContentEncoding = y }
{-# INLINE gooContentEncoding #-}

-- | The language the content is in.
gooContentLanguage :: Lens' GetObjectResponse (Maybe Text)
gooContentLanguage f x =
    f (_gooContentLanguage x)
        <&> \y -> x { _gooContentLanguage = y }
{-# INLINE gooContentLanguage #-}

-- | Size of the body in bytes.
gooContentLength :: Lens' GetObjectResponse (Maybe Integer)
gooContentLength f x =
    f (_gooContentLength x)
        <&> \y -> x { _gooContentLength = y }
{-# INLINE gooContentLength #-}

-- | A standard MIME type describing the format of the object data.
gooContentType :: Lens' GetObjectResponse (Maybe Text)
gooContentType f x =
    f (_gooContentType x)
        <&> \y -> x { _gooContentType = y }
{-# INLINE gooContentType #-}

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
gooDeleteMarker :: Lens' GetObjectResponse (Maybe Bool)
gooDeleteMarker f x =
    f (_gooDeleteMarker x)
        <&> \y -> x { _gooDeleteMarker = y }
{-# INLINE gooDeleteMarker #-}

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
gooETag :: Lens' GetObjectResponse (Maybe ETag)
gooETag f x =
    f (_gooETag x)
        <&> \y -> x { _gooETag = y }
{-# INLINE gooETag #-}

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id key
-- value pairs providing object expiration information. The value of the
-- rule-id is URL encoded.
gooExpiration :: Lens' GetObjectResponse (Maybe RFC822)
gooExpiration f x =
    f (_gooExpiration x)
        <&> \y -> x { _gooExpiration = y }
{-# INLINE gooExpiration #-}

-- | The date and time at which the object is no longer cacheable.
gooExpires :: Lens' GetObjectResponse (Maybe RFC822)
gooExpires f x =
    f (_gooExpires x)
        <&> \y -> x { _gooExpires = y }
{-# INLINE gooExpires #-}

-- | Last modified date of the object.
gooLastModified :: Lens' GetObjectResponse (Maybe RFC822)
gooLastModified f x =
    f (_gooLastModified x)
        <&> \y -> x { _gooLastModified = y }
{-# INLINE gooLastModified #-}

-- | A map of metadata to store with the object in S3.
gooMetadata :: Lens' GetObjectResponse (Map Text Text)
gooMetadata f x =
    f (_gooMetadata x)
        <&> \y -> x { _gooMetadata = y }
{-# INLINE gooMetadata #-}

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP that
-- supports more flexible metadata than the REST API. For example, using SOAP,
-- you can create metadata whose values are not legal HTTP headers.
gooMissingMeta :: Lens' GetObjectResponse (Maybe Integer)
gooMissingMeta f x =
    f (_gooMissingMeta x)
        <&> \y -> x { _gooMissingMeta = y }
{-# INLINE gooMissingMeta #-}

-- | Version of the object.
gooVersionId :: Lens' GetObjectResponse (Maybe ObjectVersionId)
gooVersionId f x =
    f (_gooVersionId x)
        <&> \y -> x { _gooVersionId = y }
{-# INLINE gooVersionId #-}

-- | Provides information about object restoration operation and expiration time
-- of the restored object copy.
gooRestore :: Lens' GetObjectResponse (Maybe Text)
gooRestore f x =
    f (_gooRestore x)
        <&> \y -> x { _gooRestore = y }
{-# INLINE gooRestore #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
gooSSECustomerAlgorithm :: Lens' GetObjectResponse (Maybe Text)
gooSSECustomerAlgorithm f x =
    f (_gooSSECustomerAlgorithm x)
        <&> \y -> x { _gooSSECustomerAlgorithm = y }
{-# INLINE gooSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
gooSSECustomerKeyMD5 :: Lens' GetObjectResponse (Maybe Text)
gooSSECustomerKeyMD5 f x =
    f (_gooSSECustomerKeyMD5 x)
        <&> \y -> x { _gooSSECustomerKeyMD5 = y }
{-# INLINE gooSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
gooServerSideEncryption :: Lens' GetObjectResponse (Maybe ServerSideEncryption)
gooServerSideEncryption f x =
    f (_gooServerSideEncryption x)
        <&> \y -> x { _gooServerSideEncryption = y }
{-# INLINE gooServerSideEncryption #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
gooWebsiteRedirectLocation :: Lens' GetObjectResponse (Maybe Text)
gooWebsiteRedirectLocation f x =
    f (_gooWebsiteRedirectLocation x)
        <&> \y -> x { _gooWebsiteRedirectLocation = y }
{-# INLINE gooWebsiteRedirectLocation #-}

instance AWSRequest GetObject where
    type Sv GetObject = S3
    type Rs GetObject = GetObjectResponse

    request = get
    response _ = bodyResponse $ \hs bdy ->
        return $! pure GetObjectResponse
            <*> pure (RsBody bdy)
            <*> hs ~:? "accept-ranges"
            <*> hs ~:? "Cache-Control"
            <*> hs ~:? "Content-Disposition"
            <*> hs ~:? "Content-Encoding"
            <*> hs ~:? "Content-Language"
            <*> hs ~:? "Content-Length"
            <*> hs ~:? "Content-Type"
            <*> hs ~:? "x-amz-delete-marker"
            <*> hs ~:? "ETag"
            <*> hs ~:? "x-amz-expiration"
            <*> hs ~:? "Expires"
            <*> hs ~:? "Last-Modified"
            <*> (Map <$> hs ~:: "x-amz-meta-")
            <*> hs ~:? "x-amz-missing-meta"
            <*> hs ~:? "x-amz-version-id"
            <*> hs ~:? "x-amz-restore"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> hs ~:? "x-amz-website-redirect-location"
