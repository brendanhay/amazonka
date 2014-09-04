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
    , mkHeadObjectRequest
    -- ** Request lenses
    , horBucket
    , horIfMatch
    , horIfModifiedSince
    , horIfNoneMatch
    , horIfUnmodifiedSince
    , horKey
    , horRange
    , horVersionId
    , horSSECustomerAlgorithm
    , horSSECustomerKey
    , horSSECustomerKeyMD5

    -- * Response
    , HeadObjectResponse
    -- ** Response lenses
    , hooDeleteMarker
    , hooAcceptRanges
    , hooExpiration
    , hooRestore
    , hooLastModified
    , hooContentLength
    , hooETag
    , hooMissingMeta
    , hooVersionId
    , hooCacheControl
    , hooContentDisposition
    , hooContentEncoding
    , hooContentLanguage
    , hooContentType
    , hooExpires
    , hooWebsiteRedirectLocation
    , hooServerSideEncryption
    , hooMetadata
    , hooSSECustomerAlgorithm
    , hooSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'HeadObject' request.
mkHeadObjectRequest :: BucketName -- ^ 'horBucket'
                    -> ObjectKey -- ^ 'horKey'
                    -> HeadObject
mkHeadObjectRequest p1 p2 = HeadObject
    { _horBucket = p1
    , _horIfMatch = Nothing
    , _horIfModifiedSince = Nothing
    , _horIfNoneMatch = Nothing
    , _horIfUnmodifiedSince = Nothing
    , _horKey = p6
    , _horRange = Nothing
    , _horVersionId = Nothing
    , _horSSECustomerAlgorithm = Nothing
    , _horSSECustomerKey = Nothing
    , _horSSECustomerKeyMD5 = Nothing
    }
{-# INLINE mkHeadObjectRequest #-}

data HeadObject = HeadObject
    { _horBucket :: BucketName
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
    , _horKey :: ObjectKey
    , _horRange :: Maybe Text
      -- ^ Downloads the specified range bytes of an object. For more
      -- information about the HTTP Range header, go to
      -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
    , _horVersionId :: Maybe ObjectVersionId
      -- ^ VersionId used to reference a specific version of the object.
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
    } deriving (Show, Generic)

horBucket :: Lens' HeadObject (BucketName)
horBucket = lens _horBucket (\s a -> s { _horBucket = a })
{-# INLINE horBucket #-}

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
horIfMatch :: Lens' HeadObject (Maybe Text)
horIfMatch = lens _horIfMatch (\s a -> s { _horIfMatch = a })
{-# INLINE horIfMatch #-}

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
horIfModifiedSince :: Lens' HeadObject (Maybe RFC822)
horIfModifiedSince = lens _horIfModifiedSince (\s a -> s { _horIfModifiedSince = a })
{-# INLINE horIfModifiedSince #-}

-- | Return the object only if its entity tag (ETag) is different from the one
-- specified, otherwise return a 304 (not modified).
horIfNoneMatch :: Lens' HeadObject (Maybe Text)
horIfNoneMatch = lens _horIfNoneMatch (\s a -> s { _horIfNoneMatch = a })
{-# INLINE horIfNoneMatch #-}

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
horIfUnmodifiedSince :: Lens' HeadObject (Maybe RFC822)
horIfUnmodifiedSince = lens _horIfUnmodifiedSince (\s a -> s { _horIfUnmodifiedSince = a })
{-# INLINE horIfUnmodifiedSince #-}

horKey :: Lens' HeadObject (ObjectKey)
horKey = lens _horKey (\s a -> s { _horKey = a })
{-# INLINE horKey #-}

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
horRange :: Lens' HeadObject (Maybe Text)
horRange = lens _horRange (\s a -> s { _horRange = a })
{-# INLINE horRange #-}

-- | VersionId used to reference a specific version of the object.
horVersionId :: Lens' HeadObject (Maybe ObjectVersionId)
horVersionId = lens _horVersionId (\s a -> s { _horVersionId = a })
{-# INLINE horVersionId #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
horSSECustomerAlgorithm :: Lens' HeadObject (Maybe Text)
horSSECustomerAlgorithm = lens _horSSECustomerAlgorithm (\s a -> s { _horSSECustomerAlgorithm = a })
{-# INLINE horSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
horSSECustomerKey :: Lens' HeadObject (Maybe Text)
horSSECustomerKey = lens _horSSECustomerKey (\s a -> s { _horSSECustomerKey = a })
{-# INLINE horSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
horSSECustomerKeyMD5 :: Lens' HeadObject (Maybe Text)
horSSECustomerKeyMD5 = lens _horSSECustomerKeyMD5 (\s a -> s { _horSSECustomerKeyMD5 = a })
{-# INLINE horSSECustomerKeyMD5 #-}

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

instance ToHeaders HeadObject

instance ToBody HeadObject

data HeadObjectResponse = HeadObjectResponse
    { _hooDeleteMarker :: Maybe Bool
      -- ^ Specifies whether the object retrieved was (true) or was not
      -- (false) a Delete Marker. If false, this response header does not
      -- appear in the response.
    , _hooAcceptRanges :: Maybe Text
    , _hooExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured (see PUT Bucket
      -- lifecycle), the response includes this header. It includes the
      -- expiry-date and rule-id key value pairs providing object
      -- expiration information. The value of the rule-id is URL encoded.
    , _hooRestore :: Maybe Text
      -- ^ Provides information about object restoration operation and
      -- expiration time of the restored object copy.
    , _hooLastModified :: Maybe RFC822
      -- ^ Last modified date of the object.
    , _hooContentLength :: Maybe Integer
      -- ^ Size of the body in bytes.
    , _hooETag :: Maybe ETag
      -- ^ An ETag is an opaque identifier assigned by a web server to a
      -- specific version of a resource found at a URL.
    , _hooMissingMeta :: Maybe Integer
      -- ^ This is set to the number of metadata entries not returned in
      -- x-amz-meta headers. This can happen if you create metadata using
      -- an API like SOAP that supports more flexible metadata than the
      -- REST API. For example, using SOAP, you can create metadata whose
      -- values are not legal HTTP headers.
    , _hooVersionId :: Maybe ObjectVersionId
      -- ^ Version of the object.
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
    , _hooContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , _hooExpires :: Maybe RFC822
      -- ^ The date and time at which the object is no longer cacheable.
    , _hooWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for
      -- this object to another object in the same bucket or to an
      -- external URL. Amazon S3 stores the value of this header in the
      -- object metadata.
    , _hooServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _hooMetadata :: Map Text Text
      -- ^ A map of metadata to store with the object in S3.
    , _hooSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _hooSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    } deriving (Show, Generic)

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
hooDeleteMarker :: Lens' HeadObjectResponse (Maybe Bool)
hooDeleteMarker = lens _hooDeleteMarker (\s a -> s { _hooDeleteMarker = a })
{-# INLINE hooDeleteMarker #-}

hooAcceptRanges :: Lens' HeadObjectResponse (Maybe Text)
hooAcceptRanges = lens _hooAcceptRanges (\s a -> s { _hooAcceptRanges = a })
{-# INLINE hooAcceptRanges #-}

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id key
-- value pairs providing object expiration information. The value of the
-- rule-id is URL encoded.
hooExpiration :: Lens' HeadObjectResponse (Maybe RFC822)
hooExpiration = lens _hooExpiration (\s a -> s { _hooExpiration = a })
{-# INLINE hooExpiration #-}

-- | Provides information about object restoration operation and expiration time
-- of the restored object copy.
hooRestore :: Lens' HeadObjectResponse (Maybe Text)
hooRestore = lens _hooRestore (\s a -> s { _hooRestore = a })
{-# INLINE hooRestore #-}

-- | Last modified date of the object.
hooLastModified :: Lens' HeadObjectResponse (Maybe RFC822)
hooLastModified = lens _hooLastModified (\s a -> s { _hooLastModified = a })
{-# INLINE hooLastModified #-}

-- | Size of the body in bytes.
hooContentLength :: Lens' HeadObjectResponse (Maybe Integer)
hooContentLength = lens _hooContentLength (\s a -> s { _hooContentLength = a })
{-# INLINE hooContentLength #-}

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
hooETag :: Lens' HeadObjectResponse (Maybe ETag)
hooETag = lens _hooETag (\s a -> s { _hooETag = a })
{-# INLINE hooETag #-}

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP that
-- supports more flexible metadata than the REST API. For example, using SOAP,
-- you can create metadata whose values are not legal HTTP headers.
hooMissingMeta :: Lens' HeadObjectResponse (Maybe Integer)
hooMissingMeta = lens _hooMissingMeta (\s a -> s { _hooMissingMeta = a })
{-# INLINE hooMissingMeta #-}

-- | Version of the object.
hooVersionId :: Lens' HeadObjectResponse (Maybe ObjectVersionId)
hooVersionId = lens _hooVersionId (\s a -> s { _hooVersionId = a })
{-# INLINE hooVersionId #-}

-- | Specifies caching behavior along the request/reply chain.
hooCacheControl :: Lens' HeadObjectResponse (Maybe Text)
hooCacheControl = lens _hooCacheControl (\s a -> s { _hooCacheControl = a })
{-# INLINE hooCacheControl #-}

-- | Specifies presentational information for the object.
hooContentDisposition :: Lens' HeadObjectResponse (Maybe Text)
hooContentDisposition = lens _hooContentDisposition (\s a -> s { _hooContentDisposition = a })
{-# INLINE hooContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
hooContentEncoding :: Lens' HeadObjectResponse (Maybe Text)
hooContentEncoding = lens _hooContentEncoding (\s a -> s { _hooContentEncoding = a })
{-# INLINE hooContentEncoding #-}

-- | The language the content is in.
hooContentLanguage :: Lens' HeadObjectResponse (Maybe Text)
hooContentLanguage = lens _hooContentLanguage (\s a -> s { _hooContentLanguage = a })
{-# INLINE hooContentLanguage #-}

-- | A standard MIME type describing the format of the object data.
hooContentType :: Lens' HeadObjectResponse (Maybe Text)
hooContentType = lens _hooContentType (\s a -> s { _hooContentType = a })
{-# INLINE hooContentType #-}

-- | The date and time at which the object is no longer cacheable.
hooExpires :: Lens' HeadObjectResponse (Maybe RFC822)
hooExpires = lens _hooExpires (\s a -> s { _hooExpires = a })
{-# INLINE hooExpires #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
hooWebsiteRedirectLocation :: Lens' HeadObjectResponse (Maybe Text)
hooWebsiteRedirectLocation = lens _hooWebsiteRedirectLocation (\s a -> s { _hooWebsiteRedirectLocation = a })
{-# INLINE hooWebsiteRedirectLocation #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
hooServerSideEncryption :: Lens' HeadObjectResponse (Maybe ServerSideEncryption)
hooServerSideEncryption = lens _hooServerSideEncryption (\s a -> s { _hooServerSideEncryption = a })
{-# INLINE hooServerSideEncryption #-}

-- | A map of metadata to store with the object in S3.
hooMetadata :: Lens' HeadObjectResponse (Map Text Text)
hooMetadata = lens _hooMetadata (\s a -> s { _hooMetadata = a })
{-# INLINE hooMetadata #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
hooSSECustomerAlgorithm :: Lens' HeadObjectResponse (Maybe Text)
hooSSECustomerAlgorithm = lens _hooSSECustomerAlgorithm (\s a -> s { _hooSSECustomerAlgorithm = a })
{-# INLINE hooSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
hooSSECustomerKeyMD5 :: Lens' HeadObjectResponse (Maybe Text)
hooSSECustomerKeyMD5 = lens _hooSSECustomerKeyMD5 (\s a -> s { _hooSSECustomerKeyMD5 = a })
{-# INLINE hooSSECustomerKeyMD5 #-}

instance AWSRequest HeadObject where
    type Sv HeadObject = S3
    type Rs HeadObject = HeadObjectResponse

    request = head
    response _ = headerResponse $ \hs ->
        pure HeadObjectResponse
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
