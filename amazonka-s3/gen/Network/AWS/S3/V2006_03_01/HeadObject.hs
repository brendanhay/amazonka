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
    , mkHeadObject
    -- ** Request lenses
    , hoBucket
    , hoIfMatch
    , hoIfModifiedSince
    , hoIfNoneMatch
    , hoIfUnmodifiedSince
    , hoKey
    , hoRange
    , hoVersionId
    , hoSSECustomerAlgorithm
    , hoSSECustomerKey
    , hoSSECustomerKeyMD5

    -- * Response
    , HeadObjectResponse
    -- ** Response lenses
    , horsDeleteMarker
    , horsAcceptRanges
    , horsExpiration
    , horsRestore
    , horsLastModified
    , horsContentLength
    , horsETag
    , horsMissingMeta
    , horsVersionId
    , horsCacheControl
    , horsContentDisposition
    , horsContentEncoding
    , horsContentLanguage
    , horsContentType
    , horsExpires
    , horsWebsiteRedirectLocation
    , horsServerSideEncryption
    , horsMetadata
    , horsSSECustomerAlgorithm
    , horsSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data HeadObject = HeadObject
    { _hoBucket :: BucketName
    , _hoIfMatch :: Maybe Text
    , _hoIfModifiedSince :: Maybe RFC822
    , _hoIfNoneMatch :: Maybe Text
    , _hoIfUnmodifiedSince :: Maybe RFC822
    , _hoKey :: ObjectKey
    , _hoRange :: Maybe Text
    , _hoVersionId :: Maybe ObjectVersionId
    , _hoSSECustomerAlgorithm :: Maybe Text
    , _hoSSECustomerKey :: Maybe Text
    , _hoSSECustomerKeyMD5 :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'HeadObject' request.
mkHeadObject :: BucketName -- ^ 'hoBucket'
             -> ObjectKey -- ^ 'hoKey'
             -> HeadObject
mkHeadObject p1 p6 = HeadObject
    { _hoBucket = p1
    , _hoIfMatch = Nothing
    , _hoIfModifiedSince = Nothing
    , _hoIfNoneMatch = Nothing
    , _hoIfUnmodifiedSince = Nothing
    , _hoKey = p6
    , _hoRange = Nothing
    , _hoVersionId = Nothing
    , _hoSSECustomerAlgorithm = Nothing
    , _hoSSECustomerKey = Nothing
    , _hoSSECustomerKeyMD5 = Nothing
    }
{-# INLINE mkHeadObject #-}

hoBucket :: Lens' HeadObject BucketName
hoBucket = lens _hoBucket (\s a -> s { _hoBucket = a })
{-# INLINE hoBucket #-}

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
hoIfMatch :: Lens' HeadObject (Maybe Text)
hoIfMatch = lens _hoIfMatch (\s a -> s { _hoIfMatch = a })
{-# INLINE hoIfMatch #-}

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
hoIfModifiedSince :: Lens' HeadObject (Maybe RFC822)
hoIfModifiedSince =
    lens _hoIfModifiedSince (\s a -> s { _hoIfModifiedSince = a })
{-# INLINE hoIfModifiedSince #-}

-- | Return the object only if its entity tag (ETag) is different from the one
-- specified, otherwise return a 304 (not modified).
hoIfNoneMatch :: Lens' HeadObject (Maybe Text)
hoIfNoneMatch = lens _hoIfNoneMatch (\s a -> s { _hoIfNoneMatch = a })
{-# INLINE hoIfNoneMatch #-}

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
hoIfUnmodifiedSince :: Lens' HeadObject (Maybe RFC822)
hoIfUnmodifiedSince =
    lens _hoIfUnmodifiedSince (\s a -> s { _hoIfUnmodifiedSince = a })
{-# INLINE hoIfUnmodifiedSince #-}

hoKey :: Lens' HeadObject ObjectKey
hoKey = lens _hoKey (\s a -> s { _hoKey = a })
{-# INLINE hoKey #-}

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
hoRange :: Lens' HeadObject (Maybe Text)
hoRange = lens _hoRange (\s a -> s { _hoRange = a })
{-# INLINE hoRange #-}

-- | VersionId used to reference a specific version of the object.
hoVersionId :: Lens' HeadObject (Maybe ObjectVersionId)
hoVersionId = lens _hoVersionId (\s a -> s { _hoVersionId = a })
{-# INLINE hoVersionId #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
hoSSECustomerAlgorithm :: Lens' HeadObject (Maybe Text)
hoSSECustomerAlgorithm =
    lens _hoSSECustomerAlgorithm (\s a -> s { _hoSSECustomerAlgorithm = a })
{-# INLINE hoSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
hoSSECustomerKey :: Lens' HeadObject (Maybe Text)
hoSSECustomerKey =
    lens _hoSSECustomerKey (\s a -> s { _hoSSECustomerKey = a })
{-# INLINE hoSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
hoSSECustomerKeyMD5 :: Lens' HeadObject (Maybe Text)
hoSSECustomerKeyMD5 =
    lens _hoSSECustomerKeyMD5 (\s a -> s { _hoSSECustomerKeyMD5 = a })
{-# INLINE hoSSECustomerKeyMD5 #-}

instance ToPath HeadObject where
    toPath HeadObject{..} = mconcat
        [ "/"
        , toBS _hoBucket
        , "/"
        , toBS _hoKey
        ]

instance ToQuery HeadObject where
    toQuery HeadObject{..} = mconcat
        [ "versionId" =? _hoVersionId
        ]

instance ToHeaders HeadObject where
    toHeaders HeadObject{..} = concat
        [ "If-Match" =: _hoIfMatch
        , "If-Modified-Since" =: _hoIfModifiedSince
        , "If-None-Match" =: _hoIfNoneMatch
        , "If-Unmodified-Since" =: _hoIfUnmodifiedSince
        , "Range" =: _hoRange
        , "x-amz-server-side-encryption-customer-algorithm" =: _hoSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _hoSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _hoSSECustomerKeyMD5
        ]

instance ToBody HeadObject

data HeadObjectResponse = HeadObjectResponse
    { _horsDeleteMarker :: Maybe Bool
    , _horsAcceptRanges :: Maybe Text
    , _horsExpiration :: Maybe RFC822
    , _horsRestore :: Maybe Text
    , _horsLastModified :: Maybe RFC822
    , _horsContentLength :: Maybe Integer
    , _horsETag :: Maybe ETag
    , _horsMissingMeta :: Maybe Integer
    , _horsVersionId :: Maybe ObjectVersionId
    , _horsCacheControl :: Maybe Text
    , _horsContentDisposition :: Maybe Text
    , _horsContentEncoding :: Maybe Text
    , _horsContentLanguage :: Maybe Text
    , _horsContentType :: Maybe Text
    , _horsExpires :: Maybe RFC822
    , _horsWebsiteRedirectLocation :: Maybe Text
    , _horsServerSideEncryption :: Maybe ServerSideEncryption
    , _horsMetadata :: Map Text Text
    , _horsSSECustomerAlgorithm :: Maybe Text
    , _horsSSECustomerKeyMD5 :: Maybe Text
    } deriving (Show, Generic)

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
horsDeleteMarker :: Lens' HeadObjectResponse (Maybe Bool)
horsDeleteMarker =
    lens _horsDeleteMarker (\s a -> s { _horsDeleteMarker = a })
{-# INLINE horsDeleteMarker #-}

horsAcceptRanges :: Lens' HeadObjectResponse (Maybe Text)
horsAcceptRanges =
    lens _horsAcceptRanges (\s a -> s { _horsAcceptRanges = a })
{-# INLINE horsAcceptRanges #-}

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id key
-- value pairs providing object expiration information. The value of the
-- rule-id is URL encoded.
horsExpiration :: Lens' HeadObjectResponse (Maybe RFC822)
horsExpiration = lens _horsExpiration (\s a -> s { _horsExpiration = a })
{-# INLINE horsExpiration #-}

-- | Provides information about object restoration operation and expiration time
-- of the restored object copy.
horsRestore :: Lens' HeadObjectResponse (Maybe Text)
horsRestore = lens _horsRestore (\s a -> s { _horsRestore = a })
{-# INLINE horsRestore #-}

-- | Last modified date of the object.
horsLastModified :: Lens' HeadObjectResponse (Maybe RFC822)
horsLastModified =
    lens _horsLastModified (\s a -> s { _horsLastModified = a })
{-# INLINE horsLastModified #-}

-- | Size of the body in bytes.
horsContentLength :: Lens' HeadObjectResponse (Maybe Integer)
horsContentLength =
    lens _horsContentLength (\s a -> s { _horsContentLength = a })
{-# INLINE horsContentLength #-}

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
horsETag :: Lens' HeadObjectResponse (Maybe ETag)
horsETag = lens _horsETag (\s a -> s { _horsETag = a })
{-# INLINE horsETag #-}

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP that
-- supports more flexible metadata than the REST API. For example, using SOAP,
-- you can create metadata whose values are not legal HTTP headers.
horsMissingMeta :: Lens' HeadObjectResponse (Maybe Integer)
horsMissingMeta = lens _horsMissingMeta (\s a -> s { _horsMissingMeta = a })
{-# INLINE horsMissingMeta #-}

-- | Version of the object.
horsVersionId :: Lens' HeadObjectResponse (Maybe ObjectVersionId)
horsVersionId = lens _horsVersionId (\s a -> s { _horsVersionId = a })
{-# INLINE horsVersionId #-}

-- | Specifies caching behavior along the request/reply chain.
horsCacheControl :: Lens' HeadObjectResponse (Maybe Text)
horsCacheControl =
    lens _horsCacheControl (\s a -> s { _horsCacheControl = a })
{-# INLINE horsCacheControl #-}

-- | Specifies presentational information for the object.
horsContentDisposition :: Lens' HeadObjectResponse (Maybe Text)
horsContentDisposition =
    lens _horsContentDisposition (\s a -> s { _horsContentDisposition = a })
{-# INLINE horsContentDisposition #-}

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
horsContentEncoding :: Lens' HeadObjectResponse (Maybe Text)
horsContentEncoding =
    lens _horsContentEncoding (\s a -> s { _horsContentEncoding = a })
{-# INLINE horsContentEncoding #-}

-- | The language the content is in.
horsContentLanguage :: Lens' HeadObjectResponse (Maybe Text)
horsContentLanguage =
    lens _horsContentLanguage (\s a -> s { _horsContentLanguage = a })
{-# INLINE horsContentLanguage #-}

-- | A standard MIME type describing the format of the object data.
horsContentType :: Lens' HeadObjectResponse (Maybe Text)
horsContentType = lens _horsContentType (\s a -> s { _horsContentType = a })
{-# INLINE horsContentType #-}

-- | The date and time at which the object is no longer cacheable.
horsExpires :: Lens' HeadObjectResponse (Maybe RFC822)
horsExpires = lens _horsExpires (\s a -> s { _horsExpires = a })
{-# INLINE horsExpires #-}

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
horsWebsiteRedirectLocation :: Lens' HeadObjectResponse (Maybe Text)
horsWebsiteRedirectLocation =
    lens _horsWebsiteRedirectLocation
         (\s a -> s { _horsWebsiteRedirectLocation = a })
{-# INLINE horsWebsiteRedirectLocation #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
horsServerSideEncryption :: Lens' HeadObjectResponse (Maybe ServerSideEncryption)
horsServerSideEncryption =
    lens _horsServerSideEncryption
         (\s a -> s { _horsServerSideEncryption = a })
{-# INLINE horsServerSideEncryption #-}

-- | A map of metadata to store with the object in S3.
horsMetadata :: Lens' HeadObjectResponse (Map Text Text)
horsMetadata = lens _horsMetadata (\s a -> s { _horsMetadata = a })
{-# INLINE horsMetadata #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
horsSSECustomerAlgorithm :: Lens' HeadObjectResponse (Maybe Text)
horsSSECustomerAlgorithm =
    lens _horsSSECustomerAlgorithm
         (\s a -> s { _horsSSECustomerAlgorithm = a })
{-# INLINE horsSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
horsSSECustomerKeyMD5 :: Lens' HeadObjectResponse (Maybe Text)
horsSSECustomerKeyMD5 =
    lens _horsSSECustomerKeyMD5 (\s a -> s { _horsSSECustomerKeyMD5 = a })
{-# INLINE horsSSECustomerKeyMD5 #-}

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
