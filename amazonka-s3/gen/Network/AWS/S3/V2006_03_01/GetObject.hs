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

gorBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> GetObject
    -> f GetObject
gorBucket f x =
    (\y -> x { _gorBucket = y })
       <$> f (_gorBucket x)
{-# INLINE gorBucket #-}

gorKey
    :: Functor f
    => (ObjectKey
    -> f (ObjectKey))
    -> GetObject
    -> f GetObject
gorKey f x =
    (\y -> x { _gorKey = y })
       <$> f (_gorKey x)
{-# INLINE gorKey #-}

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
gorIfMatch
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObject
    -> f GetObject
gorIfMatch f x =
    (\y -> x { _gorIfMatch = y })
       <$> f (_gorIfMatch x)
{-# INLINE gorIfMatch #-}

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
gorIfModifiedSince
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> GetObject
    -> f GetObject
gorIfModifiedSince f x =
    (\y -> x { _gorIfModifiedSince = y })
       <$> f (_gorIfModifiedSince x)
{-# INLINE gorIfModifiedSince #-}

-- | Return the object only if its entity tag (ETag) is different from the one
-- specified, otherwise return a 304 (not modified).
gorIfNoneMatch
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObject
    -> f GetObject
gorIfNoneMatch f x =
    (\y -> x { _gorIfNoneMatch = y })
       <$> f (_gorIfNoneMatch x)
{-# INLINE gorIfNoneMatch #-}

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
gorIfUnmodifiedSince
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> GetObject
    -> f GetObject
gorIfUnmodifiedSince f x =
    (\y -> x { _gorIfUnmodifiedSince = y })
       <$> f (_gorIfUnmodifiedSince x)
{-# INLINE gorIfUnmodifiedSince #-}

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
gorRange
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObject
    -> f GetObject
gorRange f x =
    (\y -> x { _gorRange = y })
       <$> f (_gorRange x)
{-# INLINE gorRange #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
gorSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObject
    -> f GetObject
gorSSECustomerAlgorithm f x =
    (\y -> x { _gorSSECustomerAlgorithm = y })
       <$> f (_gorSSECustomerAlgorithm x)
{-# INLINE gorSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
gorSSECustomerKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObject
    -> f GetObject
gorSSECustomerKey f x =
    (\y -> x { _gorSSECustomerKey = y })
       <$> f (_gorSSECustomerKey x)
{-# INLINE gorSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
gorSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObject
    -> f GetObject
gorSSECustomerKeyMD5 f x =
    (\y -> x { _gorSSECustomerKeyMD5 = y })
       <$> f (_gorSSECustomerKeyMD5 x)
{-# INLINE gorSSECustomerKeyMD5 #-}

-- | VersionId used to reference a specific version of the object.
gorVersionId
    :: Functor f
    => (Maybe ObjectVersionId
    -> f (Maybe ObjectVersionId))
    -> GetObject
    -> f GetObject
gorVersionId f x =
    (\y -> x { _gorVersionId = y })
       <$> f (_gorVersionId x)
{-# INLINE gorVersionId #-}

-- | Sets the Cache-Control header of the response.
gorResponseCacheControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObject
    -> f GetObject
gorResponseCacheControl f x =
    (\y -> x { _gorResponseCacheControl = y })
       <$> f (_gorResponseCacheControl x)
{-# INLINE gorResponseCacheControl #-}

-- | Sets the Content-Disposition header of the response.
gorResponseContentDisposition
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObject
    -> f GetObject
gorResponseContentDisposition f x =
    (\y -> x { _gorResponseContentDisposition = y })
       <$> f (_gorResponseContentDisposition x)
{-# INLINE gorResponseContentDisposition #-}

-- | Sets the Content-Encoding header of the response.
gorResponseContentEncoding
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObject
    -> f GetObject
gorResponseContentEncoding f x =
    (\y -> x { _gorResponseContentEncoding = y })
       <$> f (_gorResponseContentEncoding x)
{-# INLINE gorResponseContentEncoding #-}

-- | Sets the Content-Language header of the response.
gorResponseContentLanguage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObject
    -> f GetObject
gorResponseContentLanguage f x =
    (\y -> x { _gorResponseContentLanguage = y })
       <$> f (_gorResponseContentLanguage x)
{-# INLINE gorResponseContentLanguage #-}

-- | Sets the Content-Type header of the response.
gorResponseContentType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObject
    -> f GetObject
gorResponseContentType f x =
    (\y -> x { _gorResponseContentType = y })
       <$> f (_gorResponseContentType x)
{-# INLINE gorResponseContentType #-}

-- | Sets the Expires header of the response.
gorResponseExpires
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> GetObject
    -> f GetObject
gorResponseExpires f x =
    (\y -> x { _gorResponseExpires = y })
       <$> f (_gorResponseExpires x)
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
gooBody
    :: Functor f
    => (RsBody
    -> f (RsBody))
    -> GetObjectResponse
    -> f GetObjectResponse
gooBody f x =
    (\y -> x { _gooBody = y })
       <$> f (_gooBody x)
{-# INLINE gooBody #-}

gooAcceptRanges
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObjectResponse
    -> f GetObjectResponse
gooAcceptRanges f x =
    (\y -> x { _gooAcceptRanges = y })
       <$> f (_gooAcceptRanges x)
{-# INLINE gooAcceptRanges #-}

-- | Specifies caching behavior along the request/reply chain.
gooCacheControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObjectResponse
    -> f GetObjectResponse
gooCacheControl f x =
    (\y -> x { _gooCacheControl = y })
       <$> f (_gooCacheControl x)
{-# INLINE gooCacheControl #-}

-- | Specifies presentational information for the object.
gooContentDisposition
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObjectResponse
    -> f GetObjectResponse
gooContentDisposition f x =
    (\y -> x { _gooContentDisposition = y })
       <$> f (_gooContentDisposition x)
{-# INLINE gooContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
gooContentEncoding
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObjectResponse
    -> f GetObjectResponse
gooContentEncoding f x =
    (\y -> x { _gooContentEncoding = y })
       <$> f (_gooContentEncoding x)
{-# INLINE gooContentEncoding #-}

-- | The language the content is in.
gooContentLanguage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObjectResponse
    -> f GetObjectResponse
gooContentLanguage f x =
    (\y -> x { _gooContentLanguage = y })
       <$> f (_gooContentLanguage x)
{-# INLINE gooContentLanguage #-}

-- | Size of the body in bytes.
gooContentLength
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> GetObjectResponse
    -> f GetObjectResponse
gooContentLength f x =
    (\y -> x { _gooContentLength = y })
       <$> f (_gooContentLength x)
{-# INLINE gooContentLength #-}

-- | A standard MIME type describing the format of the object data.
gooContentType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObjectResponse
    -> f GetObjectResponse
gooContentType f x =
    (\y -> x { _gooContentType = y })
       <$> f (_gooContentType x)
{-# INLINE gooContentType #-}

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
gooDeleteMarker
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> GetObjectResponse
    -> f GetObjectResponse
gooDeleteMarker f x =
    (\y -> x { _gooDeleteMarker = y })
       <$> f (_gooDeleteMarker x)
{-# INLINE gooDeleteMarker #-}

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
gooETag
    :: Functor f
    => (Maybe ETag
    -> f (Maybe ETag))
    -> GetObjectResponse
    -> f GetObjectResponse
gooETag f x =
    (\y -> x { _gooETag = y })
       <$> f (_gooETag x)
{-# INLINE gooETag #-}

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id key
-- value pairs providing object expiration information. The value of the
-- rule-id is URL encoded.
gooExpiration
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> GetObjectResponse
    -> f GetObjectResponse
gooExpiration f x =
    (\y -> x { _gooExpiration = y })
       <$> f (_gooExpiration x)
{-# INLINE gooExpiration #-}

-- | The date and time at which the object is no longer cacheable.
gooExpires
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> GetObjectResponse
    -> f GetObjectResponse
gooExpires f x =
    (\y -> x { _gooExpires = y })
       <$> f (_gooExpires x)
{-# INLINE gooExpires #-}

-- | Last modified date of the object.
gooLastModified
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> GetObjectResponse
    -> f GetObjectResponse
gooLastModified f x =
    (\y -> x { _gooLastModified = y })
       <$> f (_gooLastModified x)
{-# INLINE gooLastModified #-}

-- | A map of metadata to store with the object in S3.
gooMetadata
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> GetObjectResponse
    -> f GetObjectResponse
gooMetadata f x =
    (\y -> x { _gooMetadata = y })
       <$> f (_gooMetadata x)
{-# INLINE gooMetadata #-}

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP that
-- supports more flexible metadata than the REST API. For example, using SOAP,
-- you can create metadata whose values are not legal HTTP headers.
gooMissingMeta
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> GetObjectResponse
    -> f GetObjectResponse
gooMissingMeta f x =
    (\y -> x { _gooMissingMeta = y })
       <$> f (_gooMissingMeta x)
{-# INLINE gooMissingMeta #-}

-- | Version of the object.
gooVersionId
    :: Functor f
    => (Maybe ObjectVersionId
    -> f (Maybe ObjectVersionId))
    -> GetObjectResponse
    -> f GetObjectResponse
gooVersionId f x =
    (\y -> x { _gooVersionId = y })
       <$> f (_gooVersionId x)
{-# INLINE gooVersionId #-}

-- | Provides information about object restoration operation and expiration time
-- of the restored object copy.
gooRestore
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObjectResponse
    -> f GetObjectResponse
gooRestore f x =
    (\y -> x { _gooRestore = y })
       <$> f (_gooRestore x)
{-# INLINE gooRestore #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
gooSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObjectResponse
    -> f GetObjectResponse
gooSSECustomerAlgorithm f x =
    (\y -> x { _gooSSECustomerAlgorithm = y })
       <$> f (_gooSSECustomerAlgorithm x)
{-# INLINE gooSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
gooSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObjectResponse
    -> f GetObjectResponse
gooSSECustomerKeyMD5 f x =
    (\y -> x { _gooSSECustomerKeyMD5 = y })
       <$> f (_gooSSECustomerKeyMD5 x)
{-# INLINE gooSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
gooServerSideEncryption
    :: Functor f
    => (Maybe ServerSideEncryption
    -> f (Maybe ServerSideEncryption))
    -> GetObjectResponse
    -> f GetObjectResponse
gooServerSideEncryption f x =
    (\y -> x { _gooServerSideEncryption = y })
       <$> f (_gooServerSideEncryption x)
{-# INLINE gooServerSideEncryption #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
gooWebsiteRedirectLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetObjectResponse
    -> f GetObjectResponse
gooWebsiteRedirectLocation f x =
    (\y -> x { _gooWebsiteRedirectLocation = y })
       <$> f (_gooWebsiteRedirectLocation x)
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
