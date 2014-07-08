{-# LANGUAGE DeriveGeneric               #-}
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
module Network.AWS.S3.V2006_03_01.HeadObject where

import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)

-- | Smart constructor utilising default fields to
-- specify the minimum viable HeadObject request.
headObject :: BucketName -- ^ 'horBucket'
           -> ObjectKey -- ^ 'horKey'
           -> HeadObject
headObject p1 p2 = HeadObject
    { horBucket = p1
    , horKey = p2
    , horIfMatch = Nothing
    , horIfModifiedSince = Nothing
    , horIfNoneMatch = Nothing
    , horIfUnmodifiedSince = Nothing
    , horRange = Nothing
    , horSSECustomerAlgorithm = Nothing
    , horSSECustomerKey = Nothing
    , horSSECustomerKeyMD5 = Nothing
    , horVersionId = Nothing
    }

data HeadObject = HeadObject
    { horBucket :: BucketName
    , horKey :: ObjectKey
    , horIfMatch :: Maybe Text
      -- ^ Return the object only if its entity tag (ETag) is the same as
      -- the one specified, otherwise return a 412 (precondition failed).
    , horIfModifiedSince :: Maybe RFC822
      -- ^ Return the object only if it has been modified since the
      -- specified time, otherwise return a 304 (not modified).
    , horIfNoneMatch :: Maybe Text
      -- ^ Return the object only if its entity tag (ETag) is different from
      -- the one specified, otherwise return a 304 (not modified).
    , horIfUnmodifiedSince :: Maybe RFC822
      -- ^ Return the object only if it has not been modified since the
      -- specified time, otherwise return a 412 (precondition failed).
    , horRange :: Maybe Text
      -- ^ Downloads the specified range bytes of an object. For more
      -- information about the HTTP Range header, go to
      -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
    , horSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , horSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header.
    , horSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    , horVersionId :: Maybe ObjectVersionId
      -- ^ VersionId used to reference a specific version of the object.
    } deriving (Eq, Show, Generic)

instance ToPath HeadObject where
    toPath HeadObject{..} = mconcat
        [ "/"
        , toBS horBucket
        , "/"
        , toBS horKey
        ]

instance ToQuery HeadObject

instance ToHeaders HeadObject where
    toHeaders HeadObject{..} = concat
        [ "If-Match" =: horIfMatch
        , "If-Modified-Since" =: horIfModifiedSince
        , "If-None-Match" =: horIfNoneMatch
        , "If-Unmodified-Since" =: horIfUnmodifiedSince
        , "Range" =: horRange
        , "x-amz-server-side-encryption-customer-algorithm" =: horSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: horSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: horSSECustomerKeyMD5
        ]

instance ToBody HeadObject

instance AWSRequest HeadObject where
    type Sv HeadObject = S3

    request  = head
fromList [("payload",Null),("name",String "HeadObjectResponse"),("shape",Object fromList [("streaming",Bool False),("location",String "body"),("required",Bool False),("name",String "HeadObjectOutput"),("documentation",Null),("common",Object fromList [("streaming",Bool False),("location",String "body"),("required",Bool False),("name",String "HeadObjectOutput"),("documentation",Null),("location_name",String "HeadObjectOutput"),("xml_name",String "HeadObjectOutput")]),("location_name",String "HeadObjectOutput"),("xml_name",String "HeadObjectOutput"),("fields",Object fromList [("ETag",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "ETag"),("documentation",String "An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL"),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "ETag"),("documentation",String "An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL"),("location_name",String "ETag"),("xml_name",String "ETag")]),("location_name",String "ETag"),("type",String "Text"),("xml_name",String "ETag")]),("VersionId",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "ObjectVersionId"),("documentation",String "Version of the object."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "ObjectVersionId"),("documentation",String "Version of the object."),("location_name",String "x-amz-version-id"),("xml_name",String "ObjectVersionId")]),("location_name",String "x-amz-version-id"),("type",String "Text"),("xml_name",String "ObjectVersionId")]),("ContentLength",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "ContentLength"),("documentation",String "Size of the body in bytes."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "ContentLength"),("documentation",String "Size of the body in bytes."),("location_name",String "Content-Length"),("xml_name",String "ContentLength")]),("location_name",String "Content-Length"),("type",String "Integer"),("xml_name",String "ContentLength")]),("Expires",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "Expires"),("documentation",String "The date and time at which the object is no longer cacheable."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "Expires"),("documentation",String "The date and time at which the object is no longer cacheable."),("location_name",String "Expires"),("xml_name",String "Expires")]),("location_name",String "Expires"),("type",String "UTCTime"),("xml_name",String "Expires")]),("Restore",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "Restore"),("documentation",String "Provides information about object restoration operation and expiration time of the restored object copy."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "Restore"),("documentation",String "Provides information about object restoration operation and expiration time of the restored object copy."),("location_name",String "x-amz-restore"),("xml_name",String "Restore")]),("location_name",String "x-amz-restore"),("type",String "Text"),("xml_name",String "Restore")]),("Expiration",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "Expiration"),("documentation",String "If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key value pairs providing object expiration information. The value of the rule-id is URL encoded."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "Expiration"),("documentation",String "If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key value pairs providing object expiration information. The value of the rule-id is URL encoded."),("location_name",String "x-amz-expiration"),("xml_name",String "Expiration")]),("location_name",String "x-amz-expiration"),("type",String "UTCTime"),("xml_name",String "Expiration")]),("DeleteMarker",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "DeleteMarker"),("documentation",String "Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "DeleteMarker"),("documentation",String "Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response."),("location_name",String "x-amz-delete-marker"),("xml_name",String "DeleteMarker")]),("location_name",String "x-amz-delete-marker"),("type",String "Bool"),("xml_name",String "DeleteMarker")]),("SSECustomerAlgorithm",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "SSECustomerAlgorithm"),("documentation",String "If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "SSECustomerAlgorithm"),("documentation",String "If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used."),("location_name",String "x-amz-server-side-encryption-customer-algorithm"),("xml_name",String "SSECustomerAlgorithm")]),("location_name",String "x-amz-server-side-encryption-customer-algorithm"),("type",String "Text"),("xml_name",String "SSECustomerAlgorithm")]),("MissingMeta",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "MissingMeta"),("documentation",String "This is set to the number of metadata entries not returned in x-amz-meta headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "MissingMeta"),("documentation",String "This is set to the number of metadata entries not returned in x-amz-meta headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers."),("location_name",String "x-amz-missing-meta"),("xml_name",String "MissingMeta")]),("location_name",String "x-amz-missing-meta"),("type",String "Integer"),("xml_name",String "MissingMeta")]),("WebsiteRedirectLocation",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "WebsiteRedirectLocation"),("documentation",String "If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "WebsiteRedirectLocation"),("documentation",String "If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata."),("location_name",String "x-amz-website-redirect-location"),("xml_name",String "WebsiteRedirectLocation")]),("location_name",String "x-amz-website-redirect-location"),("type",String "Text"),("xml_name",String "WebsiteRedirectLocation")]),("AcceptRanges",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "AcceptRanges"),("documentation",Null),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "AcceptRanges"),("documentation",Null),("location_name",String "accept-ranges"),("xml_name",String "AcceptRanges")]),("location_name",String "accept-ranges"),("type",String "Text"),("xml_name",String "AcceptRanges")]),("SSECustomerKeyMD5",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "SSECustomerKeyMD5"),("documentation",String "If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "SSECustomerKeyMD5"),("documentation",String "If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key."),("location_name",String "x-amz-server-side-encryption-customer-key-MD5"),("xml_name",String "SSECustomerKeyMD5")]),("location_name",String "x-amz-server-side-encryption-customer-key-MD5"),("type",String "Text"),("xml_name",String "SSECustomerKeyMD5")]),("ContentEncoding",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "ContentEncoding"),("documentation",String "Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "ContentEncoding"),("documentation",String "Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field."),("location_name",String "Content-Encoding"),("xml_name",String "ContentEncoding")]),("location_name",String "Content-Encoding"),("type",String "Text"),("xml_name",String "ContentEncoding")]),("Metadata",Object fromList [("streaming",Bool False),("location",String "header"),("value",Object fromList [("streaming",Bool False),("location",String "body"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",Null),("documentation",String "The metadata value."),("common",Object fromList [("streaming",Bool False),("location",String "body"),("required",Bool False),("name",Null),("documentation",String "The metadata value."),("location_name",Null),("xml_name",Null)]),("location_name",Null),("type",String "Text"),("xml_name",Null)]),("required",Bool False),("key",Object fromList [("streaming",Bool False),("location",String "body"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",Null),("documentation",String "The metadata key. This will be prefixed with x-amz-meta- before sending to S3 as a header. The x-amz-meta- header will be stripped from the key when retrieving headers."),("common",Object fromList [("streaming",Bool False),("location",String "body"),("required",Bool False),("name",Null),("documentation",String "The metadata key. This will be prefixed with x-amz-meta- before sending to S3 as a header. The x-amz-meta- header will be stripped from the key when retrieving headers."),("location_name",Null),("xml_name",Null)]),("location_name",Null),("type",String "Text"),("xml_name",Null)]),("name",Null),("documentation",String "A map of metadata to store with the object in S3."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",Null),("documentation",String "A map of metadata to store with the object in S3."),("location_name",String "x-amz-meta-"),("xml_name",Null)]),("location_name",String "x-amz-meta-"),("xml_name",Null)]),("CacheControl",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "CacheControl"),("documentation",String "Specifies caching behavior along the request/reply chain."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "CacheControl"),("documentation",String "Specifies caching behavior along the request/reply chain."),("location_name",String "Cache-Control"),("xml_name",String "CacheControl")]),("location_name",String "Cache-Control"),("type",String "Text"),("xml_name",String "CacheControl")]),("ContentLanguage",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "ContentLanguage"),("documentation",String "The language the content is in."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "ContentLanguage"),("documentation",String "The language the content is in."),("location_name",String "Content-Language"),("xml_name",String "ContentLanguage")]),("location_name",String "Content-Language"),("type",String "Text"),("xml_name",String "ContentLanguage")]),("LastModified",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "LastModified"),("documentation",String "Last modified date of the object"),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "LastModified"),("documentation",String "Last modified date of the object"),("location_name",String "Last-Modified"),("xml_name",String "LastModified")]),("location_name",String "Last-Modified"),("type",String "UTCTime"),("xml_name",String "LastModified")]),("ContentDisposition",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "ContentDisposition"),("documentation",String "Specifies presentational information for the object."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "ContentDisposition"),("documentation",String "Specifies presentational information for the object."),("location_name",String "Content-Disposition"),("xml_name",String "ContentDisposition")]),("location_name",String "Content-Disposition"),("type",String "Text"),("xml_name",String "ContentDisposition")]),("ServerSideEncryption",Object fromList [("streaming",Bool False),("location",String "header"),("values",Object fromList [("ServerSideEncryptionAES256",String "AES256")]),("required",Bool False),("name",String "ServerSideEncryption"),("documentation",String "The Server-side encryption algorithm used when storing this object in S3."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "ServerSideEncryption"),("documentation",String "The Server-side encryption algorithm used when storing this object in S3."),("location_name",String "x-amz-server-side-encryption"),("xml_name",String "ServerSideEncryption")]),("location_name",String "x-amz-server-side-encryption"),("xml_name",String "ServerSideEncryption")]),("ContentType",Object fromList [("streaming",Bool False),("location",String "header"),("pattern",Null),("required",Bool False),("min_length",Number 0.0),("max_length",Number 0.0),("name",String "ContentType"),("documentation",String "A standard MIME type describing the format of the object data."),("common",Object fromList [("streaming",Bool False),("location",String "header"),("required",Bool False),("name",String "ContentType"),("documentation",String "A standard MIME type describing the format of the object data."),("location_name",String "Content-Type"),("xml_name",String "ContentType")]),("location_name",String "Content-Type"),("type",String "Text"),("xml_name",String "ContentType")])])]),("fields",Array (fromList [Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool True),("required",Bool False),("name",Null),("documentation",String "A map of metadata to store with the object in S3."),("location_name",String "x-amz-meta-"),("type",String "HashMap Text Text"),("xml_name",Null),("prefixed",String "hooMetadata")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "AcceptRanges"),("documentation",Null),("location_name",String "accept-ranges"),("type",String "Maybe Text"),("xml_name",String "AcceptRanges"),("prefixed",String "hooAcceptRanges")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "CacheControl"),("documentation",String "Specifies caching behavior along the request/reply chain."),("location_name",String "Cache-Control"),("type",String "Maybe Text"),("xml_name",String "CacheControl"),("prefixed",String "hooCacheControl")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "ContentDisposition"),("documentation",String "Specifies presentational information for the object."),("location_name",String "Content-Disposition"),("type",String "Maybe Text"),("xml_name",String "ContentDisposition"),("prefixed",String "hooContentDisposition")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "ContentEncoding"),("documentation",String "Specifies what content encodings have been applied to the object and thus what decoding mechanisms must be applied to obtain the media-type referenced by the Content-Type header field."),("location_name",String "Content-Encoding"),("type",String "Maybe Text"),("xml_name",String "ContentEncoding"),("prefixed",String "hooContentEncoding")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "ContentLanguage"),("documentation",String "The language the content is in."),("location_name",String "Content-Language"),("type",String "Maybe Text"),("xml_name",String "ContentLanguage"),("prefixed",String "hooContentLanguage")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "ContentLength"),("documentation",String "Size of the body in bytes."),("location_name",String "Content-Length"),("type",String "Maybe Integer"),("xml_name",String "ContentLength"),("prefixed",String "hooContentLength")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "ContentType"),("documentation",String "A standard MIME type describing the format of the object data."),("location_name",String "Content-Type"),("type",String "Maybe Text"),("xml_name",String "ContentType"),("prefixed",String "hooContentType")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "DeleteMarker"),("documentation",String "Specifies whether the object retrieved was (true) or was not (false) a Delete Marker. If false, this response header does not appear in the response."),("location_name",String "x-amz-delete-marker"),("type",String "Maybe Bool"),("xml_name",String "DeleteMarker"),("prefixed",String "hooDeleteMarker")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "ETag"),("documentation",String "An ETag is an opaque identifier assigned by a web server to a specific version of a resource found at a URL"),("location_name",String "ETag"),("type",String "Maybe ETag"),("xml_name",String "ETag"),("prefixed",String "hooETag")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "Expiration"),("documentation",String "If the object expiration is configured (see PUT Bucket lifecycle), the response includes this header. It includes the expiry-date and rule-id key value pairs providing object expiration information. The value of the rule-id is URL encoded."),("location_name",String "x-amz-expiration"),("type",String "Maybe RFC822"),("xml_name",String "Expiration"),("prefixed",String "hooExpiration")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "Expires"),("documentation",String "The date and time at which the object is no longer cacheable."),("location_name",String "Expires"),("type",String "Maybe RFC822"),("xml_name",String "Expires"),("prefixed",String "hooExpires")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "LastModified"),("documentation",String "Last modified date of the object"),("location_name",String "Last-Modified"),("type",String "Maybe RFC822"),("xml_name",String "LastModified"),("prefixed",String "hooLastModified")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "MissingMeta"),("documentation",String "This is set to the number of metadata entries not returned in x-amz-meta headers. This can happen if you create metadata using an API like SOAP that supports more flexible metadata than the REST API. For example, using SOAP, you can create metadata whose values are not legal HTTP headers."),("location_name",String "x-amz-missing-meta"),("type",String "Maybe Integer"),("xml_name",String "MissingMeta"),("prefixed",String "hooMissingMeta")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "ObjectVersionId"),("documentation",String "Version of the object."),("location_name",String "x-amz-version-id"),("type",String "Maybe ObjectVersionId"),("xml_name",String "ObjectVersionId"),("prefixed",String "hooVersionId")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "Restore"),("documentation",String "Provides information about object restoration operation and expiration time of the restored object copy."),("location_name",String "x-amz-restore"),("type",String "Maybe Text"),("xml_name",String "Restore"),("prefixed",String "hooRestore")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "SSECustomerAlgorithm"),("documentation",String "If server-side encryption with a customer-provided encryption key was requested, the response will include this header confirming the encryption algorithm used."),("location_name",String "x-amz-server-side-encryption-customer-algorithm"),("type",String "Maybe Text"),("xml_name",String "SSECustomerAlgorithm"),("prefixed",String "hooSSECustomerAlgorithm")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "SSECustomerKeyMD5"),("documentation",String "If server-side encryption with a customer-provided encryption key was requested, the response will include this header to provide round trip message integrity verification of the customer-provided encryption key."),("location_name",String "x-amz-server-side-encryption-customer-key-MD5"),("type",String "Maybe Text"),("xml_name",String "SSECustomerKeyMD5"),("prefixed",String "hooSSECustomerKeyMD5")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "ServerSideEncryption"),("documentation",String "The Server-side encryption algorithm used when storing this object in S3."),("location_name",String "x-amz-server-side-encryption"),("type",String "Maybe ServerSideEncryption"),("xml_name",String "ServerSideEncryption"),("prefixed",String "hooServerSideEncryption")],Object fromList [("streaming",Bool False),("location",String "header"),("default",Bool False),("monoid",Bool False),("required",Bool False),("name",String "WebsiteRedirectLocation"),("documentation",String "If the bucket is configured as a website, redirects requests for this object to another object in the same bucket or to an external URL. Amazon S3 stores the value of this header in the object metadata."),("location_name",String "x-amz-website-redirect-location"),("type",String "Maybe Text"),("xml_name",String "WebsiteRedirectLocation"),("prefixed",String "hooWebsiteRedirectLocation")]]))]

data instance Rs HeadObject = HeadObjectResponse
    { hooMetadata :: HashMap Text Text
      -- ^ A map of metadata to store with the object in S3.
    , hooAcceptRanges :: Maybe Text
    , hooCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , hooContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , hooContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object
      -- and thus what decoding mechanisms must be applied to obtain the
      -- media-type referenced by the Content-Type header field.
    , hooContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , hooContentLength :: Maybe Integer
      -- ^ Size of the body in bytes.
    , hooContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , hooDeleteMarker :: Maybe Bool
      -- ^ Specifies whether the object retrieved was (true) or was not
      -- (false) a Delete Marker. If false, this response header does not
      -- appear in the response.
    , hooETag :: Maybe ETag
      -- ^ An ETag is an opaque identifier assigned by a web server to a
      -- specific version of a resource found at a URL.
    , hooExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured (see PUT Bucket
      -- lifecycle), the response includes this header. It includes the
      -- expiry-date and rule-id key value pairs providing object
      -- expiration information. The value of the rule-id is URL encoded.
    , hooExpires :: Maybe RFC822
      -- ^ The date and time at which the object is no longer cacheable.
    , hooLastModified :: Maybe RFC822
      -- ^ Last modified date of the object.
    , hooMissingMeta :: Maybe Integer
      -- ^ This is set to the number of metadata entries not returned in
      -- x-amz-meta headers. This can happen if you create metadata using
      -- an API like SOAP that supports more flexible metadata than the
      -- REST API. For example, using SOAP, you can create metadata whose
      -- values are not legal HTTP headers.
    , hooVersionId :: Maybe ObjectVersionId
      -- ^ Version of the object.
    , hooRestore :: Maybe Text
      -- ^ Provides information about object restoration operation and
      -- expiration time of the restored object copy.
    , hooSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , hooSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , hooServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , hooWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for
      -- this object to another object in the same bucket or to an
      -- external URL. Amazon S3 stores the value of this header in the
      -- object metadata.
    } deriving (Eq, Show, Generic)
