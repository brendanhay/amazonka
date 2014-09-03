{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.HeadObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The HEAD operation retrieves metadata from an object without returning the
-- object itself. This operation is useful if you're only interested in an
-- object's metadata. To use HEAD, you must have READ access to the object.
module Network.AWS.S3.V2006_03_01.HeadObject
    (
    -- * Request
      HeadObject
    -- ** Request constructor
    , headObject
    -- ** Request lenses
    , horBucket
    , horKey
    , horIfMatch
    , horIfModifiedSince
    , horIfNoneMatch
    , horIfUnmodifiedSince
    , horRange
    , horSSECustomerAlgorithm
    , horSSECustomerKey
    , horSSECustomerKeyMD5
    , horVersionId

    -- * Response
    , HeadObjectResponse
    -- ** Response lenses
    , hooAcceptRanges
    , hooCacheControl
    , hooContentDisposition
    , hooContentEncoding
    , hooContentLanguage
    , hooContentLength
    , hooContentType
    , hooDeleteMarker
    , hooETag
    , hooExpiration
    , hooExpires
    , hooLastModified
    , hooMetadata
    , hooMissingMeta
    , hooVersionId
    , hooRestore
    , hooSSECustomerAlgorithm
    , hooSSECustomerKeyMD5
    , hooServerSideEncryption
    , hooWebsiteRedirectLocation
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'HeadObject' request.
headObject :: BucketName -- ^ 'horBucket'
           -> ObjectKey -- ^ 'horKey'
           -> HeadObject
headObject p1 p2 = HeadObject
    { _horBucket = p1
    , _horKey = p2
    , _horIfMatch = Nothing
    , _horIfModifiedSince = Nothing
    , _horIfNoneMatch = Nothing
    , _horIfUnmodifiedSince = Nothing
    , _horRange = Nothing
    , _horSSECustomerAlgorithm = Nothing
    , _horSSECustomerKey = Nothing
    , _horSSECustomerKeyMD5 = Nothing
    , _horVersionId = Nothing
    }

data HeadObject = HeadObject
    { _horBucket :: BucketName
    , _horKey :: ObjectKey
    , _horIfMatch :: Maybe Text
      -- ^ Return the object only if its entity tag (ETag) is the same as
      -- the one specified, otherwise return a 412 (precondition failed).
    , _horIfModifiedSince :: Maybe RFC822
      -- ^ Return the object only if it has been modified since the
      -- specified time, otherwise return a 304 (not modified).
    , _horIfNoneMatch :: Maybe Text
      -- ^ Return the object only if its entity tag (ETag) is different from
      -- the one specified, otherwise return a 304 (not modified).
    , _horIfUnmodifiedSince :: Maybe RFC822
      -- ^ Return the object only if it has not been modified since the
      -- specified time, otherwise return a 412 (precondition failed).
    , _horRange :: Maybe Text
      -- ^ Downloads the specified range bytes of an object. For more
      -- information about the HTTP Range header, go to
      -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
    , _horSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , _horSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header.
    , _horSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    , _horVersionId :: Maybe ObjectVersionId
      -- ^ VersionId used to reference a specific version of the object.
    } deriving (Show, Generic)

horBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> HeadObject
    -> f HeadObject
horBucket f x =
    (\y -> x { _horBucket = y })
       <$> f (_horBucket x)
{-# INLINE horBucket #-}

horKey
    :: Functor f
    => (ObjectKey
    -> f (ObjectKey))
    -> HeadObject
    -> f HeadObject
horKey f x =
    (\y -> x { _horKey = y })
       <$> f (_horKey x)
{-# INLINE horKey #-}

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
horIfMatch
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObject
    -> f HeadObject
horIfMatch f x =
    (\y -> x { _horIfMatch = y })
       <$> f (_horIfMatch x)
{-# INLINE horIfMatch #-}

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
horIfModifiedSince
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> HeadObject
    -> f HeadObject
horIfModifiedSince f x =
    (\y -> x { _horIfModifiedSince = y })
       <$> f (_horIfModifiedSince x)
{-# INLINE horIfModifiedSince #-}

-- | Return the object only if its entity tag (ETag) is different from the one
-- specified, otherwise return a 304 (not modified).
horIfNoneMatch
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObject
    -> f HeadObject
horIfNoneMatch f x =
    (\y -> x { _horIfNoneMatch = y })
       <$> f (_horIfNoneMatch x)
{-# INLINE horIfNoneMatch #-}

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
horIfUnmodifiedSince
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> HeadObject
    -> f HeadObject
horIfUnmodifiedSince f x =
    (\y -> x { _horIfUnmodifiedSince = y })
       <$> f (_horIfUnmodifiedSince x)
{-# INLINE horIfUnmodifiedSince #-}

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
horRange
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObject
    -> f HeadObject
horRange f x =
    (\y -> x { _horRange = y })
       <$> f (_horRange x)
{-# INLINE horRange #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
horSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObject
    -> f HeadObject
horSSECustomerAlgorithm f x =
    (\y -> x { _horSSECustomerAlgorithm = y })
       <$> f (_horSSECustomerAlgorithm x)
{-# INLINE horSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
horSSECustomerKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObject
    -> f HeadObject
horSSECustomerKey f x =
    (\y -> x { _horSSECustomerKey = y })
       <$> f (_horSSECustomerKey x)
{-# INLINE horSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
horSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObject
    -> f HeadObject
horSSECustomerKeyMD5 f x =
    (\y -> x { _horSSECustomerKeyMD5 = y })
       <$> f (_horSSECustomerKeyMD5 x)
{-# INLINE horSSECustomerKeyMD5 #-}

-- | VersionId used to reference a specific version of the object.
horVersionId
    :: Functor f
    => (Maybe ObjectVersionId
    -> f (Maybe ObjectVersionId))
    -> HeadObject
    -> f HeadObject
horVersionId f x =
    (\y -> x { _horVersionId = y })
       <$> f (_horVersionId x)
{-# INLINE horVersionId #-}

instance ToPath HeadObject where
    toPath HeadObject{..} = mconcat
        [ "/"
        , toBS _horBucket
        , "/"
        , toBS _horKey
        ]

instance ToQuery HeadObject where
    toQuery HeadObject{..} = mconcat
        [ "versionId" =? _horVersionId
        ]

instance ToHeaders HeadObject where
    toHeaders HeadObject{..} = concat
        [ "If-Match" =: _horIfMatch
        , "If-Modified-Since" =: _horIfModifiedSince
        , "If-None-Match" =: _horIfNoneMatch
        , "If-Unmodified-Since" =: _horIfUnmodifiedSince
        , "Range" =: _horRange
        , "x-amz-server-side-encryption-customer-algorithm" =: _horSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _horSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _horSSECustomerKeyMD5
        ]

instance ToBody HeadObject

data HeadObjectResponse = HeadObjectResponse
    { _hooAcceptRanges :: Maybe Text
    , _hooCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , _hooContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , _hooContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object
      -- and thus what decoding mechanisms must be applied to obtain the
      -- media-type referenced by the Content-Type header field.
    , _hooContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , _hooContentLength :: Maybe Integer
      -- ^ Size of the body in bytes.
    , _hooContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , _hooDeleteMarker :: Maybe Bool
      -- ^ Specifies whether the object retrieved was (true) or was not
      -- (false) a Delete Marker. If false, this response header does not
      -- appear in the response.
    , _hooETag :: Maybe ETag
      -- ^ An ETag is an opaque identifier assigned by a web server to a
      -- specific version of a resource found at a URL.
    , _hooExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured (see PUT Bucket
      -- lifecycle), the response includes this header. It includes the
      -- expiry-date and rule-id key value pairs providing object
      -- expiration information. The value of the rule-id is URL encoded.
    , _hooExpires :: Maybe RFC822
      -- ^ The date and time at which the object is no longer cacheable.
    , _hooLastModified :: Maybe RFC822
      -- ^ Last modified date of the object.
    , _hooMetadata :: Map Text Text
      -- ^ A map of metadata to store with the object in S3.
    , _hooMissingMeta :: Maybe Integer
      -- ^ This is set to the number of metadata entries not returned in
      -- x-amz-meta headers. This can happen if you create metadata using
      -- an API like SOAP that supports more flexible metadata than the
      -- REST API. For example, using SOAP, you can create metadata whose
      -- values are not legal HTTP headers.
    , _hooVersionId :: Maybe ObjectVersionId
      -- ^ Version of the object.
    , _hooRestore :: Maybe Text
      -- ^ Provides information about object restoration operation and
      -- expiration time of the restored object copy.
    , _hooSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _hooSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , _hooServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _hooWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for
      -- this object to another object in the same bucket or to an
      -- external URL. Amazon S3 stores the value of this header in the
      -- object metadata.
    } deriving (Show, Generic)

hooAcceptRanges
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooAcceptRanges f x =
    (\y -> x { _hooAcceptRanges = y })
       <$> f (_hooAcceptRanges x)
{-# INLINE hooAcceptRanges #-}

-- | Specifies caching behavior along the request/reply chain.
hooCacheControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooCacheControl f x =
    (\y -> x { _hooCacheControl = y })
       <$> f (_hooCacheControl x)
{-# INLINE hooCacheControl #-}

-- | Specifies presentational information for the object.
hooContentDisposition
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooContentDisposition f x =
    (\y -> x { _hooContentDisposition = y })
       <$> f (_hooContentDisposition x)
{-# INLINE hooContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
hooContentEncoding
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooContentEncoding f x =
    (\y -> x { _hooContentEncoding = y })
       <$> f (_hooContentEncoding x)
{-# INLINE hooContentEncoding #-}

-- | The language the content is in.
hooContentLanguage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooContentLanguage f x =
    (\y -> x { _hooContentLanguage = y })
       <$> f (_hooContentLanguage x)
{-# INLINE hooContentLanguage #-}

-- | Size of the body in bytes.
hooContentLength
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooContentLength f x =
    (\y -> x { _hooContentLength = y })
       <$> f (_hooContentLength x)
{-# INLINE hooContentLength #-}

-- | A standard MIME type describing the format of the object data.
hooContentType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooContentType f x =
    (\y -> x { _hooContentType = y })
       <$> f (_hooContentType x)
{-# INLINE hooContentType #-}

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
hooDeleteMarker
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooDeleteMarker f x =
    (\y -> x { _hooDeleteMarker = y })
       <$> f (_hooDeleteMarker x)
{-# INLINE hooDeleteMarker #-}

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
hooETag
    :: Functor f
    => (Maybe ETag
    -> f (Maybe ETag))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooETag f x =
    (\y -> x { _hooETag = y })
       <$> f (_hooETag x)
{-# INLINE hooETag #-}

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id key
-- value pairs providing object expiration information. The value of the
-- rule-id is URL encoded.
hooExpiration
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooExpiration f x =
    (\y -> x { _hooExpiration = y })
       <$> f (_hooExpiration x)
{-# INLINE hooExpiration #-}

-- | The date and time at which the object is no longer cacheable.
hooExpires
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooExpires f x =
    (\y -> x { _hooExpires = y })
       <$> f (_hooExpires x)
{-# INLINE hooExpires #-}

-- | Last modified date of the object.
hooLastModified
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooLastModified f x =
    (\y -> x { _hooLastModified = y })
       <$> f (_hooLastModified x)
{-# INLINE hooLastModified #-}

-- | A map of metadata to store with the object in S3.
hooMetadata
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooMetadata f x =
    (\y -> x { _hooMetadata = y })
       <$> f (_hooMetadata x)
{-# INLINE hooMetadata #-}

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP that
-- supports more flexible metadata than the REST API. For example, using SOAP,
-- you can create metadata whose values are not legal HTTP headers.
hooMissingMeta
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooMissingMeta f x =
    (\y -> x { _hooMissingMeta = y })
       <$> f (_hooMissingMeta x)
{-# INLINE hooMissingMeta #-}

-- | Version of the object.
hooVersionId
    :: Functor f
    => (Maybe ObjectVersionId
    -> f (Maybe ObjectVersionId))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooVersionId f x =
    (\y -> x { _hooVersionId = y })
       <$> f (_hooVersionId x)
{-# INLINE hooVersionId #-}

-- | Provides information about object restoration operation and expiration time
-- of the restored object copy.
hooRestore
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooRestore f x =
    (\y -> x { _hooRestore = y })
       <$> f (_hooRestore x)
{-# INLINE hooRestore #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
hooSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooSSECustomerAlgorithm f x =
    (\y -> x { _hooSSECustomerAlgorithm = y })
       <$> f (_hooSSECustomerAlgorithm x)
{-# INLINE hooSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
hooSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooSSECustomerKeyMD5 f x =
    (\y -> x { _hooSSECustomerKeyMD5 = y })
       <$> f (_hooSSECustomerKeyMD5 x)
{-# INLINE hooSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
hooServerSideEncryption
    :: Functor f
    => (Maybe ServerSideEncryption
    -> f (Maybe ServerSideEncryption))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooServerSideEncryption f x =
    (\y -> x { _hooServerSideEncryption = y })
       <$> f (_hooServerSideEncryption x)
{-# INLINE hooServerSideEncryption #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
hooWebsiteRedirectLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> HeadObjectResponse
    -> f HeadObjectResponse
hooWebsiteRedirectLocation f x =
    (\y -> x { _hooWebsiteRedirectLocation = y })
       <$> f (_hooWebsiteRedirectLocation x)
{-# INLINE hooWebsiteRedirectLocation #-}

instance AWSRequest HeadObject where
    type Sv HeadObject = S3
    type Rs HeadObject = HeadObjectResponse

    request = head
    response _ = headerResponse $ \hs ->
        pure HeadObjectResponse
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
