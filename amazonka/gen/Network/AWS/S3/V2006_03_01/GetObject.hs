{-# LANGUAGE DeriveGeneric               #-}
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
module Network.AWS.S3.V2006_03_01.GetObject where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.RestS3
import           Network.AWS.S3.V2006_03_01.Types
import           Network.HTTP.Client  (Response)
import           Prelude              hiding (head)

-- | Default GetObject request.
getObject :: BucketName -- ^ '_gorBucket'
          -> ObjectKey -- ^ '_gorKey'
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

instance ToPath GetObject where
    toPath GetObject{..} = mconcat
        [ "/"
        , toBS _gorBucket
        , "/"
        , toBS _gorKey
        ]

instance ToQuery GetObject

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

instance AWSRequest GetObject where
    type Sv GetObject = S3

    request  = get
    response = bodyResponse $ \hs bdy ->
        return $! pure GetObjectResponse
            <*> pure (Body bdy)
            <*> (hs `filterHeaders` "x-amz-meta-")
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
            <*> hs ~:? "x-amz-missing-meta"
            <*> hs ~:? "x-amz-version-id"
            <*> hs ~:? "x-amz-restore"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> hs ~:? "x-amz-website-redirect-location"

data instance Rs GetObject = GetObjectResponse
    { _gooBody :: BodySource
      -- ^ Object data.
    , _gooMetadata :: HashMap Text Text
      -- ^ A map of metadata to store with the object in S3.
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
