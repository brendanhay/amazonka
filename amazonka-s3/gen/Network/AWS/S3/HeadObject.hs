{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.HeadObject
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The HEAD operation retrieves metadata from an object without returning the
-- object itself. This operation is useful if you're only interested in an
-- object's metadata. To use HEAD, you must have READ access to the object.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/HeadObject.html>
module Network.AWS.S3.HeadObject
    (
    -- * Request
      HeadObject
    -- ** Request constructor
    , headObject
    -- ** Request lenses
    , hoBucket
    , hoIfMatch
    , hoIfModifiedSince
    , hoIfNoneMatch
    , hoIfUnmodifiedSince
    , hoKey
    , hoRange
    , hoSSECustomerAlgorithm
    , hoSSECustomerKey
    , hoSSECustomerKeyMD5
    , hoVersionId

    -- * Response
    , HeadObjectResponse
    -- ** Response constructor
    , headObjectResponse
    -- ** Response lenses
    , horAcceptRanges
    , horCacheControl
    , horContentDisposition
    , horContentEncoding
    , horContentLanguage
    , horContentLength
    , horContentType
    , horDeleteMarker
    , horETag
    , horExpiration
    , horExpires
    , horLastModified
    , horMetadata
    , horMissingMeta
    , horRestore
    , horSSECustomerAlgorithm
    , horSSECustomerKeyMD5
    , horSSEKMSKeyId
    , horServerSideEncryption
    , horVersionId
    , horWebsiteRedirectLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

data HeadObject = HeadObject
    { _hoBucket               :: Text
    , _hoIfMatch              :: Maybe Text
    , _hoIfModifiedSince      :: Maybe RFC822
    , _hoIfNoneMatch          :: Maybe Text
    , _hoIfUnmodifiedSince    :: Maybe RFC822
    , _hoKey                  :: Text
    , _hoRange                :: Maybe Text
    , _hoSSECustomerAlgorithm :: Maybe Text
    , _hoSSECustomerKey       :: Maybe (Sensitive Text)
    , _hoSSECustomerKeyMD5    :: Maybe Text
    , _hoVersionId            :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'HeadObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hoBucket' @::@ 'Text'
--
-- * 'hoIfMatch' @::@ 'Maybe' 'Text'
--
-- * 'hoIfModifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'hoIfNoneMatch' @::@ 'Maybe' 'Text'
--
-- * 'hoIfUnmodifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'hoKey' @::@ 'Text'
--
-- * 'hoRange' @::@ 'Maybe' 'Text'
--
-- * 'hoSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'hoSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'hoSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'hoVersionId' @::@ 'Maybe' 'Text'
--
headObject :: Text -- ^ 'hoBucket'
           -> Text -- ^ 'hoKey'
           -> HeadObject
headObject p1 p2 = HeadObject
    { _hoBucket               = p1
    , _hoKey                  = p2
    , _hoIfMatch              = Nothing
    , _hoIfModifiedSince      = Nothing
    , _hoIfNoneMatch          = Nothing
    , _hoIfUnmodifiedSince    = Nothing
    , _hoRange                = Nothing
    , _hoVersionId            = Nothing
    , _hoSSECustomerAlgorithm = Nothing
    , _hoSSECustomerKey       = Nothing
    , _hoSSECustomerKeyMD5    = Nothing
    }

hoBucket :: Lens' HeadObject Text
hoBucket = lens _hoBucket (\s a -> s { _hoBucket = a })

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
hoIfMatch :: Lens' HeadObject (Maybe Text)
hoIfMatch = lens _hoIfMatch (\s a -> s { _hoIfMatch = a })

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
hoIfModifiedSince :: Lens' HeadObject (Maybe UTCTime)
hoIfModifiedSince =
    lens _hoIfModifiedSince (\s a -> s { _hoIfModifiedSince = a })
        . mapping _Time

-- | Return the object only if its entity tag (ETag) is different from the one
-- specified, otherwise return a 304 (not modified).
hoIfNoneMatch :: Lens' HeadObject (Maybe Text)
hoIfNoneMatch = lens _hoIfNoneMatch (\s a -> s { _hoIfNoneMatch = a })

-- | Return the object only if it has not been modified since the specified time,
-- otherwise return a 412 (precondition failed).
hoIfUnmodifiedSince :: Lens' HeadObject (Maybe UTCTime)
hoIfUnmodifiedSince =
    lens _hoIfUnmodifiedSince (\s a -> s { _hoIfUnmodifiedSince = a })
        . mapping _Time

hoKey :: Lens' HeadObject Text
hoKey = lens _hoKey (\s a -> s { _hoKey = a })

-- | Downloads the specified range bytes of an object. For more information about
-- the HTTP Range header, go to
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
hoRange :: Lens' HeadObject (Maybe Text)
hoRange = lens _hoRange (\s a -> s { _hoRange = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g., AES256,
-- aws:kms).
hoSSECustomerAlgorithm :: Lens' HeadObject (Maybe Text)
hoSSECustomerAlgorithm =
    lens _hoSSECustomerAlgorithm (\s a -> s { _hoSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side​-encryption​-customer-algorithm header.
hoSSECustomerKey :: Lens' HeadObject (Maybe Text)
hoSSECustomerKey = lens _hoSSECustomerKey (\s a -> s { _hoSSECustomerKey = a }) . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321.
-- Amazon S3 uses this header for a message integrity check to ensure the
-- encryption key was transmitted without error.
hoSSECustomerKeyMD5 :: Lens' HeadObject (Maybe Text)
hoSSECustomerKeyMD5 =
    lens _hoSSECustomerKeyMD5 (\s a -> s { _hoSSECustomerKeyMD5 = a })

-- | VersionId used to reference a specific version of the object.
hoVersionId :: Lens' HeadObject (Maybe Text)
hoVersionId = lens _hoVersionId (\s a -> s { _hoVersionId = a })

data HeadObjectResponse = HeadObjectResponse
    { _horAcceptRanges            :: Maybe Text
    , _horCacheControl            :: Maybe Text
    , _horContentDisposition      :: Maybe Text
    , _horContentEncoding         :: Maybe Text
    , _horContentLanguage         :: Maybe Text
    , _horContentLength           :: Maybe Int
    , _horContentType             :: Maybe Text
    , _horDeleteMarker            :: Maybe Bool
    , _horETag                    :: Maybe Text
    , _horExpiration              :: Maybe Text
    , _horExpires                 :: Maybe RFC822
    , _horLastModified            :: Maybe RFC822
    , _horMetadata                :: Map (CI Text) Text
    , _horMissingMeta             :: Maybe Int
    , _horRestore                 :: Maybe Text
    , _horSSECustomerAlgorithm    :: Maybe Text
    , _horSSECustomerKeyMD5       :: Maybe Text
    , _horSSEKMSKeyId             :: Maybe (Sensitive Text)
    , _horServerSideEncryption    :: Maybe ServerSideEncryption
    , _horVersionId               :: Maybe Text
    , _horWebsiteRedirectLocation :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'HeadObjectResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'horAcceptRanges' @::@ 'Maybe' 'Text'
--
-- * 'horCacheControl' @::@ 'Maybe' 'Text'
--
-- * 'horContentDisposition' @::@ 'Maybe' 'Text'
--
-- * 'horContentEncoding' @::@ 'Maybe' 'Text'
--
-- * 'horContentLanguage' @::@ 'Maybe' 'Text'
--
-- * 'horContentLength' @::@ 'Maybe' 'Int'
--
-- * 'horContentType' @::@ 'Maybe' 'Text'
--
-- * 'horDeleteMarker' @::@ 'Maybe' 'Bool'
--
-- * 'horETag' @::@ 'Maybe' 'Text'
--
-- * 'horExpiration' @::@ 'Maybe' 'Text'
--
-- * 'horExpires' @::@ 'Maybe' 'UTCTime'
--
-- * 'horLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'horMetadata' @::@ 'HashMap' ('CI' 'Text') 'Text'
--
-- * 'horMissingMeta' @::@ 'Maybe' 'Int'
--
-- * 'horRestore' @::@ 'Maybe' 'Text'
--
-- * 'horSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'horSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'horSSEKMSKeyId' @::@ 'Maybe' 'Text'
--
-- * 'horServerSideEncryption' @::@ 'Maybe' 'ServerSideEncryption'
--
-- * 'horVersionId' @::@ 'Maybe' 'Text'
--
-- * 'horWebsiteRedirectLocation' @::@ 'Maybe' 'Text'
--
headObjectResponse :: HeadObjectResponse
headObjectResponse = HeadObjectResponse
    { _horDeleteMarker            = Nothing
    , _horAcceptRanges            = Nothing
    , _horExpiration              = Nothing
    , _horRestore                 = Nothing
    , _horLastModified            = Nothing
    , _horContentLength           = Nothing
    , _horETag                    = Nothing
    , _horMissingMeta             = Nothing
    , _horVersionId               = Nothing
    , _horCacheControl            = Nothing
    , _horContentDisposition      = Nothing
    , _horContentEncoding         = Nothing
    , _horContentLanguage         = Nothing
    , _horContentType             = Nothing
    , _horExpires                 = Nothing
    , _horWebsiteRedirectLocation = Nothing
    , _horServerSideEncryption    = Nothing
    , _horMetadata                = mempty
    , _horSSECustomerAlgorithm    = Nothing
    , _horSSECustomerKeyMD5       = Nothing
    , _horSSEKMSKeyId             = Nothing
    }

horAcceptRanges :: Lens' HeadObjectResponse (Maybe Text)
horAcceptRanges = lens _horAcceptRanges (\s a -> s { _horAcceptRanges = a })

-- | Specifies caching behavior along the request/reply chain.
horCacheControl :: Lens' HeadObjectResponse (Maybe Text)
horCacheControl = lens _horCacheControl (\s a -> s { _horCacheControl = a })

-- | Specifies presentational information for the object.
horContentDisposition :: Lens' HeadObjectResponse (Maybe Text)
horContentDisposition =
    lens _horContentDisposition (\s a -> s { _horContentDisposition = a })

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type referenced
-- by the Content-Type header field.
horContentEncoding :: Lens' HeadObjectResponse (Maybe Text)
horContentEncoding =
    lens _horContentEncoding (\s a -> s { _horContentEncoding = a })

-- | The language the content is in.
horContentLanguage :: Lens' HeadObjectResponse (Maybe Text)
horContentLanguage =
    lens _horContentLanguage (\s a -> s { _horContentLanguage = a })

-- | Size of the body in bytes.
horContentLength :: Lens' HeadObjectResponse (Maybe Int)
horContentLength = lens _horContentLength (\s a -> s { _horContentLength = a })

-- | A standard MIME type describing the format of the object data.
horContentType :: Lens' HeadObjectResponse (Maybe Text)
horContentType = lens _horContentType (\s a -> s { _horContentType = a })

-- | Specifies whether the object retrieved was (true) or was not (false) a Delete
-- Marker. If false, this response header does not appear in the response.
horDeleteMarker :: Lens' HeadObjectResponse (Maybe Bool)
horDeleteMarker = lens _horDeleteMarker (\s a -> s { _horDeleteMarker = a })

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL
horETag :: Lens' HeadObjectResponse (Maybe Text)
horETag = lens _horETag (\s a -> s { _horETag = a })

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id key
-- value pairs providing object expiration information. The value of the rule-id
-- is URL encoded.
horExpiration :: Lens' HeadObjectResponse (Maybe Text)
horExpiration = lens _horExpiration (\s a -> s { _horExpiration = a })

-- | The date and time at which the object is no longer cacheable.
horExpires :: Lens' HeadObjectResponse (Maybe UTCTime)
horExpires = lens _horExpires (\s a -> s { _horExpires = a }) . mapping _Time

-- | Last modified date of the object
horLastModified :: Lens' HeadObjectResponse (Maybe UTCTime)
horLastModified = lens _horLastModified (\s a -> s { _horLastModified = a }) . mapping _Time

-- | A map of metadata to store with the object in S3.
horMetadata :: Lens' HeadObjectResponse (HashMap (CI Text) Text)
horMetadata = lens _horMetadata (\s a -> s { _horMetadata = a }) . _Map

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP that
-- supports more flexible metadata than the REST API. For example, using SOAP,
-- you can create metadata whose values are not legal HTTP headers.
horMissingMeta :: Lens' HeadObjectResponse (Maybe Int)
horMissingMeta = lens _horMissingMeta (\s a -> s { _horMissingMeta = a })

-- | Provides information about object restoration operation and expiration time
-- of the restored object copy.
horRestore :: Lens' HeadObjectResponse (Maybe Text)
horRestore = lens _horRestore (\s a -> s { _horRestore = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
horSSECustomerAlgorithm :: Lens' HeadObjectResponse (Maybe Text)
horSSECustomerAlgorithm =
    lens _horSSECustomerAlgorithm (\s a -> s { _horSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
horSSECustomerKeyMD5 :: Lens' HeadObjectResponse (Maybe Text)
horSSECustomerKeyMD5 =
    lens _horSSECustomerKeyMD5 (\s a -> s { _horSSECustomerKeyMD5 = a })

-- | If present, specifies the ID of the AWS Key Management Service (KMS) master
-- encryption key that was used for the object.
horSSEKMSKeyId :: Lens' HeadObjectResponse (Maybe Text)
horSSEKMSKeyId = lens _horSSEKMSKeyId (\s a -> s { _horSSEKMSKeyId = a }) . mapping _Sensitive

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
horServerSideEncryption :: Lens' HeadObjectResponse (Maybe ServerSideEncryption)
horServerSideEncryption =
    lens _horServerSideEncryption (\s a -> s { _horServerSideEncryption = a })

-- | Version of the object.
horVersionId :: Lens' HeadObjectResponse (Maybe Text)
horVersionId = lens _horVersionId (\s a -> s { _horVersionId = a })

-- | If the bucket is configured as a website, redirects requests for this object
-- to another object in the same bucket or to an external URL. Amazon S3 stores
-- the value of this header in the object metadata.
horWebsiteRedirectLocation :: Lens' HeadObjectResponse (Maybe Text)
horWebsiteRedirectLocation =
    lens _horWebsiteRedirectLocation
        (\s a -> s { _horWebsiteRedirectLocation = a })

instance ToPath HeadObject where
    toPath HeadObject{..} = mconcat
        [ "/"
        , toText _hoBucket
        , "/"
        , toText _hoKey
        ]

instance ToQuery HeadObject where
    toQuery rq = "versionId" =? _hoVersionId rq

instance ToHeaders HeadObject where
    toHeaders HeadObject{..} = mconcat
        [ "If-Match"                                        =: _hoIfMatch
        , "If-Modified-Since"                               =: _hoIfModifiedSince
        , "If-None-Match"                                   =: _hoIfNoneMatch
        , "If-Unmodified-Since"                             =: _hoIfUnmodifiedSince
        , "Range"                                           =: _hoRange
        , "x-amz-server-side-encryption-customer-algorithm" =: _hoSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"       =: _hoSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"   =: _hoSSECustomerKeyMD5
        ]

instance ToXMLRoot HeadObject where
    toXMLRoot = const (namespaced ns "HeadObject" [])

instance ToXML HeadObject

instance AWSRequest HeadObject where
    type Sv HeadObject = S3
    type Rs HeadObject = HeadObjectResponse

    request  = head
    response = headerResponse $ \h -> HeadObjectResponse
        <$> h ~:? "accept-ranges"
        <*> h ~:? "Cache-Control"
        <*> h ~:? "Content-Disposition"
        <*> h ~:? "Content-Encoding"
        <*> h ~:? "Content-Language"
        <*> h ~:? "Content-Length"
        <*> h ~:? "Content-Type"
        <*> h ~:? "x-amz-delete-marker"
        <*> h ~:? "ETag"
        <*> h ~:? "x-amz-expiration"
        <*> h ~:? "Expires"
        <*> h ~:? "Last-Modified"
        <*> h ~:: "x-amz-meta-"
        <*> h ~:? "x-amz-missing-meta"
        <*> h ~:? "x-amz-restore"
        <*> h ~:? "x-amz-server-side-encryption-customer-algorithm"
        <*> h ~:? "x-amz-server-side-encryption-customer-key-MD5"
        <*> h ~:? "x-amz-server-side-encryption-aws-kms-key-id"
        <*> h ~:? "x-amz-server-side-encryption"
        <*> h ~:? "x-amz-version-id"
        <*> h ~:? "x-amz-website-redirect-location"
