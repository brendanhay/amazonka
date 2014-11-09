{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.S3.HeadObject
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
    , HeadObjectOutput
    -- ** Response constructor
    , headObjectOutput
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
    , hooRestore
    , hooSSECustomerAlgorithm
    , hooSSECustomerKeyMD5
    , hooServerSideEncryption
    , hooVersionId
    , hooWebsiteRedirectLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

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
    } deriving (Eq, Ord, Show, Generic)

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

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
hoIfUnmodifiedSince :: Lens' HeadObject (Maybe UTCTime)
hoIfUnmodifiedSince =
    lens _hoIfUnmodifiedSince (\s a -> s { _hoIfUnmodifiedSince = a })
        . mapping _Time

hoKey :: Lens' HeadObject Text
hoKey = lens _hoKey (\s a -> s { _hoKey = a })

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
hoRange :: Lens' HeadObject (Maybe Text)
hoRange = lens _hoRange (\s a -> s { _hoRange = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
hoSSECustomerAlgorithm :: Lens' HeadObject (Maybe Text)
hoSSECustomerAlgorithm =
    lens _hoSSECustomerAlgorithm (\s a -> s { _hoSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
hoSSECustomerKey :: Lens' HeadObject (Maybe Text)
hoSSECustomerKey = lens _hoSSECustomerKey (\s a -> s { _hoSSECustomerKey = a })
    . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
hoSSECustomerKeyMD5 :: Lens' HeadObject (Maybe Text)
hoSSECustomerKeyMD5 =
    lens _hoSSECustomerKeyMD5 (\s a -> s { _hoSSECustomerKeyMD5 = a })

-- | VersionId used to reference a specific version of the object.
hoVersionId :: Lens' HeadObject (Maybe Text)
hoVersionId = lens _hoVersionId (\s a -> s { _hoVersionId = a })

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

data HeadObjectOutput = HeadObjectOutput
    { _hooAcceptRanges            :: Maybe Text
    , _hooCacheControl            :: Maybe Text
    , _hooContentDisposition      :: Maybe Text
    , _hooContentEncoding         :: Maybe Text
    , _hooContentLanguage         :: Maybe Text
    , _hooContentLength           :: Maybe Int
    , _hooContentType             :: Maybe Text
    , _hooDeleteMarker            :: Maybe Bool
    , _hooETag                    :: Maybe Text
    , _hooExpiration              :: Maybe RFC822
    , _hooExpires                 :: Maybe RFC822
    , _hooLastModified            :: Maybe RFC822
    , _hooMetadata                :: Map Text Text
    , _hooMissingMeta             :: Maybe Int
    , _hooRestore                 :: Maybe Text
    , _hooSSECustomerAlgorithm    :: Maybe Text
    , _hooSSECustomerKeyMD5       :: Maybe Text
    , _hooServerSideEncryption    :: Maybe Text
    , _hooVersionId               :: Maybe Text
    , _hooWebsiteRedirectLocation :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'HeadObjectOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hooAcceptRanges' @::@ 'Maybe' 'Text'
--
-- * 'hooCacheControl' @::@ 'Maybe' 'Text'
--
-- * 'hooContentDisposition' @::@ 'Maybe' 'Text'
--
-- * 'hooContentEncoding' @::@ 'Maybe' 'Text'
--
-- * 'hooContentLanguage' @::@ 'Maybe' 'Text'
--
-- * 'hooContentLength' @::@ 'Maybe' 'Int'
--
-- * 'hooContentType' @::@ 'Maybe' 'Text'
--
-- * 'hooDeleteMarker' @::@ 'Maybe' 'Bool'
--
-- * 'hooETag' @::@ 'Maybe' 'Text'
--
-- * 'hooExpiration' @::@ 'Maybe' 'UTCTime'
--
-- * 'hooExpires' @::@ 'Maybe' 'UTCTime'
--
-- * 'hooLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'hooMetadata' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'hooMissingMeta' @::@ 'Maybe' 'Int'
--
-- * 'hooRestore' @::@ 'Maybe' 'Text'
--
-- * 'hooSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'hooSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'hooServerSideEncryption' @::@ 'Maybe' 'Text'
--
-- * 'hooVersionId' @::@ 'Maybe' 'Text'
--
-- * 'hooWebsiteRedirectLocation' @::@ 'Maybe' 'Text'
--
headObjectOutput :: HeadObjectOutput
headObjectOutput = HeadObjectOutput
    { _hooDeleteMarker            = Nothing
    , _hooAcceptRanges            = Nothing
    , _hooExpiration              = Nothing
    , _hooRestore                 = Nothing
    , _hooLastModified            = Nothing
    , _hooContentLength           = Nothing
    , _hooETag                    = Nothing
    , _hooMissingMeta             = Nothing
    , _hooVersionId               = Nothing
    , _hooCacheControl            = Nothing
    , _hooContentDisposition      = Nothing
    , _hooContentEncoding         = Nothing
    , _hooContentLanguage         = Nothing
    , _hooContentType             = Nothing
    , _hooExpires                 = Nothing
    , _hooWebsiteRedirectLocation = Nothing
    , _hooServerSideEncryption    = Nothing
    , _hooMetadata                = mempty
    , _hooSSECustomerAlgorithm    = Nothing
    , _hooSSECustomerKeyMD5       = Nothing
    }

hooAcceptRanges :: Lens' HeadObjectOutput (Maybe Text)
hooAcceptRanges = lens _hooAcceptRanges (\s a -> s { _hooAcceptRanges = a })

-- | Specifies caching behavior along the request/reply chain.
hooCacheControl :: Lens' HeadObjectOutput (Maybe Text)
hooCacheControl = lens _hooCacheControl (\s a -> s { _hooCacheControl = a })

-- | Specifies presentational information for the object.
hooContentDisposition :: Lens' HeadObjectOutput (Maybe Text)
hooContentDisposition =
    lens _hooContentDisposition (\s a -> s { _hooContentDisposition = a })

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
hooContentEncoding :: Lens' HeadObjectOutput (Maybe Text)
hooContentEncoding =
    lens _hooContentEncoding (\s a -> s { _hooContentEncoding = a })

-- | The language the content is in.
hooContentLanguage :: Lens' HeadObjectOutput (Maybe Text)
hooContentLanguage =
    lens _hooContentLanguage (\s a -> s { _hooContentLanguage = a })

-- | Size of the body in bytes.
hooContentLength :: Lens' HeadObjectOutput (Maybe Int)
hooContentLength = lens _hooContentLength (\s a -> s { _hooContentLength = a })

-- | A standard MIME type describing the format of the object data.
hooContentType :: Lens' HeadObjectOutput (Maybe Text)
hooContentType = lens _hooContentType (\s a -> s { _hooContentType = a })

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
hooDeleteMarker :: Lens' HeadObjectOutput (Maybe Bool)
hooDeleteMarker = lens _hooDeleteMarker (\s a -> s { _hooDeleteMarker = a })

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
hooETag :: Lens' HeadObjectOutput (Maybe Text)
hooETag = lens _hooETag (\s a -> s { _hooETag = a })

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id
-- key value pairs providing object expiration information. The value of the
-- rule-id is URL encoded.
hooExpiration :: Lens' HeadObjectOutput (Maybe UTCTime)
hooExpiration = lens _hooExpiration (\s a -> s { _hooExpiration = a })
    . mapping _Time

-- | The date and time at which the object is no longer cacheable.
hooExpires :: Lens' HeadObjectOutput (Maybe UTCTime)
hooExpires = lens _hooExpires (\s a -> s { _hooExpires = a })
    . mapping _Time

-- | Last modified date of the object.
hooLastModified :: Lens' HeadObjectOutput (Maybe UTCTime)
hooLastModified = lens _hooLastModified (\s a -> s { _hooLastModified = a })
    . mapping _Time

-- | A map of metadata to store with the object in S3.
hooMetadata :: Lens' HeadObjectOutput (HashMap Text Text)
hooMetadata = lens _hooMetadata (\s a -> s { _hooMetadata = a })
    . _Map

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP
-- that supports more flexible metadata than the REST API. For example,
-- using SOAP, you can create metadata whose values are not legal HTTP
-- headers.
hooMissingMeta :: Lens' HeadObjectOutput (Maybe Int)
hooMissingMeta = lens _hooMissingMeta (\s a -> s { _hooMissingMeta = a })

-- | Provides information about object restoration operation and expiration
-- time of the restored object copy.
hooRestore :: Lens' HeadObjectOutput (Maybe Text)
hooRestore = lens _hooRestore (\s a -> s { _hooRestore = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
hooSSECustomerAlgorithm :: Lens' HeadObjectOutput (Maybe Text)
hooSSECustomerAlgorithm =
    lens _hooSSECustomerAlgorithm (\s a -> s { _hooSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
hooSSECustomerKeyMD5 :: Lens' HeadObjectOutput (Maybe Text)
hooSSECustomerKeyMD5 =
    lens _hooSSECustomerKeyMD5 (\s a -> s { _hooSSECustomerKeyMD5 = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
hooServerSideEncryption :: Lens' HeadObjectOutput (Maybe Text)
hooServerSideEncryption =
    lens _hooServerSideEncryption (\s a -> s { _hooServerSideEncryption = a })

-- | Version of the object.
hooVersionId :: Lens' HeadObjectOutput (Maybe Text)
hooVersionId = lens _hooVersionId (\s a -> s { _hooVersionId = a })

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
hooWebsiteRedirectLocation :: Lens' HeadObjectOutput (Maybe Text)
hooWebsiteRedirectLocation =
    lens _hooWebsiteRedirectLocation
        (\s a -> s { _hooWebsiteRedirectLocation = a })

instance AWSRequest HeadObject where
    type Sv HeadObject = S3
    type Rs HeadObject = HeadObjectOutput

    request  = head
    response = const . xmlResponse $ \h x -> HeadObjectOutput
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
        <*> h ~:? "x-amz-server-side-encryption"
        <*> h ~:? "x-amz-version-id"
        <*> h ~:? "x-amz-website-redirect-location"
