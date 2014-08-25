{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.S3.V2006_03_01.HeadObject where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'HeadObject' request.
headObject :: BucketName -- ^ '_horBucket'
           -> ObjectKey -- ^ '_horKey'
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

makeLenses ''HeadObject

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

makeLenses ''HeadObjectResponse

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
