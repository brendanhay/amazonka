{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves objects from Amazon S3.
module Network.AWS.S3.GetObject
    (
    -- * Request
      GetObject
    -- ** Request constructor
    , getObject
    -- ** Request lenses
    , gorBucket
    , gorIfMatch
    , gorIfModifiedSince
    , gorIfNoneMatch
    , gorIfUnmodifiedSince
    , gorKey
    , gorRange
    , gorResponseCacheControl
    , gorResponseContentDisposition
    , gorResponseContentEncoding
    , gorResponseContentLanguage
    , gorResponseContentType
    , gorResponseExpires
    , gorSSECustomerAlgorithm
    , gorSSECustomerKey
    , gorSSECustomerKeyMD5
    , gorVersionId

    -- * Response
    , GetObjectOutput
    -- ** Response constructor
    , getObjectOutput
    -- ** Response lenses
    , gooAcceptRanges
    , gooBody
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
    , gooRestore
    , gooSSECustomerAlgorithm
    , gooSSECustomerKeyMD5
    , gooServerSideEncryption
    , gooVersionId
    , gooWebsiteRedirectLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data GetObject = GetObject
    { _gorBucket                     :: BucketName
    , _gorIfMatch                    :: Maybe Text
    , _gorIfModifiedSince            :: Maybe RFC822
    , _gorIfNoneMatch                :: Maybe Text
    , _gorIfUnmodifiedSince          :: Maybe RFC822
    , _gorKey                        :: ObjectKey
    , _gorRange                      :: Maybe Text
    , _gorResponseCacheControl       :: Maybe Text
    , _gorResponseContentDisposition :: Maybe Text
    , _gorResponseContentEncoding    :: Maybe Text
    , _gorResponseContentLanguage    :: Maybe Text
    , _gorResponseContentType        :: Maybe Text
    , _gorResponseExpires            :: Maybe RFC822
    , _gorSSECustomerAlgorithm       :: Maybe Text
    , _gorSSECustomerKey             :: Maybe (Sensitive Text)
    , _gorSSECustomerKeyMD5          :: Maybe Text
    , _gorVersionId                  :: Maybe ObjectVersionId
    } deriving (Eq, Show, Generic)

-- | 'GetObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gorBucket' @::@ 'BucketName'
--
-- * 'gorIfMatch' @::@ 'Maybe' 'Text'
--
-- * 'gorIfModifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'gorIfNoneMatch' @::@ 'Maybe' 'Text'
--
-- * 'gorIfUnmodifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'gorKey' @::@ 'ObjectKey'
--
-- * 'gorRange' @::@ 'Maybe' 'Text'
--
-- * 'gorResponseCacheControl' @::@ 'Maybe' 'Text'
--
-- * 'gorResponseContentDisposition' @::@ 'Maybe' 'Text'
--
-- * 'gorResponseContentEncoding' @::@ 'Maybe' 'Text'
--
-- * 'gorResponseContentLanguage' @::@ 'Maybe' 'Text'
--
-- * 'gorResponseContentType' @::@ 'Maybe' 'Text'
--
-- * 'gorResponseExpires' @::@ 'Maybe' 'UTCTime'
--
-- * 'gorSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'gorSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'gorSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'gorVersionId' @::@ 'Maybe' 'ObjectVersionId'
--
getObject :: BucketName -- ^ 'gorBucket'
          -> ObjectKey -- ^ 'gorKey'
          -> GetObject
getObject p1 p2 = GetObject
    { _gorBucket                     = p1
    , _gorKey                        = p2
    , _gorIfMatch                    = Nothing
    , _gorIfModifiedSince            = Nothing
    , _gorIfNoneMatch                = Nothing
    , _gorIfUnmodifiedSince          = Nothing
    , _gorRange                      = Nothing
    , _gorResponseCacheControl       = Nothing
    , _gorResponseContentDisposition = Nothing
    , _gorResponseContentEncoding    = Nothing
    , _gorResponseContentLanguage    = Nothing
    , _gorResponseContentType        = Nothing
    , _gorResponseExpires            = Nothing
    , _gorVersionId                  = Nothing
    , _gorSSECustomerAlgorithm       = Nothing
    , _gorSSECustomerKey             = Nothing
    , _gorSSECustomerKeyMD5          = Nothing
    }

gorBucket :: Lens' GetObject BucketName
gorBucket = lens _gorBucket (\s a -> s { _gorBucket = a })

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
gorIfMatch :: Lens' GetObject (Maybe Text)
gorIfMatch = lens _gorIfMatch (\s a -> s { _gorIfMatch = a })

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
gorIfModifiedSince :: Lens' GetObject (Maybe UTCTime)
gorIfModifiedSince =
    lens _gorIfModifiedSince (\s a -> s { _gorIfModifiedSince = a })
        . mapping _Time

-- | Return the object only if its entity tag (ETag) is different from the one
-- specified, otherwise return a 304 (not modified).
gorIfNoneMatch :: Lens' GetObject (Maybe Text)
gorIfNoneMatch = lens _gorIfNoneMatch (\s a -> s { _gorIfNoneMatch = a })

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
gorIfUnmodifiedSince :: Lens' GetObject (Maybe UTCTime)
gorIfUnmodifiedSince =
    lens _gorIfUnmodifiedSince (\s a -> s { _gorIfUnmodifiedSince = a })
        . mapping _Time

gorKey :: Lens' GetObject ObjectKey
gorKey = lens _gorKey (\s a -> s { _gorKey = a })

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
gorRange :: Lens' GetObject (Maybe Text)
gorRange = lens _gorRange (\s a -> s { _gorRange = a })

-- | Sets the Cache-Control header of the response.
gorResponseCacheControl :: Lens' GetObject (Maybe Text)
gorResponseCacheControl =
    lens _gorResponseCacheControl (\s a -> s { _gorResponseCacheControl = a })

-- | Sets the Content-Disposition header of the response.
gorResponseContentDisposition :: Lens' GetObject (Maybe Text)
gorResponseContentDisposition =
    lens _gorResponseContentDisposition
        (\s a -> s { _gorResponseContentDisposition = a })

-- | Sets the Content-Encoding header of the response.
gorResponseContentEncoding :: Lens' GetObject (Maybe Text)
gorResponseContentEncoding =
    lens _gorResponseContentEncoding
        (\s a -> s { _gorResponseContentEncoding = a })

-- | Sets the Content-Language header of the response.
gorResponseContentLanguage :: Lens' GetObject (Maybe Text)
gorResponseContentLanguage =
    lens _gorResponseContentLanguage
        (\s a -> s { _gorResponseContentLanguage = a })

-- | Sets the Content-Type header of the response.
gorResponseContentType :: Lens' GetObject (Maybe Text)
gorResponseContentType =
    lens _gorResponseContentType (\s a -> s { _gorResponseContentType = a })

-- | Sets the Expires header of the response.
gorResponseExpires :: Lens' GetObject (Maybe UTCTime)
gorResponseExpires =
    lens _gorResponseExpires (\s a -> s { _gorResponseExpires = a })
        . mapping _Time

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
gorSSECustomerAlgorithm :: Lens' GetObject (Maybe Text)
gorSSECustomerAlgorithm =
    lens _gorSSECustomerAlgorithm (\s a -> s { _gorSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
gorSSECustomerKey :: Lens' GetObject (Maybe Text)
gorSSECustomerKey =
    lens _gorSSECustomerKey (\s a -> s { _gorSSECustomerKey = a })
        . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
gorSSECustomerKeyMD5 :: Lens' GetObject (Maybe Text)
gorSSECustomerKeyMD5 =
    lens _gorSSECustomerKeyMD5 (\s a -> s { _gorSSECustomerKeyMD5 = a })

-- | VersionId used to reference a specific version of the object.
gorVersionId :: Lens' GetObject (Maybe ObjectVersionId)
gorVersionId = lens _gorVersionId (\s a -> s { _gorVersionId = a })

instance ToPath GetObject where
    toPath GetObject{..} = mconcat
        [ "/"
        , toText _gorBucket
        , "/"
        , toText _gorKey
        ]

instance ToQuery GetObject where
    toQuery GetObject{..} = mconcat
        [ "response-cache-control"       =? _gorResponseCacheControl
        , "response-content-disposition" =? _gorResponseContentDisposition
        , "response-content-encoding"    =? _gorResponseContentEncoding
        , "response-content-language"    =? _gorResponseContentLanguage
        , "response-content-type"        =? _gorResponseContentType
        , "response-expires"             =? _gorResponseExpires
        , "versionId"                    =? _gorVersionId
        ]

instance ToHeaders GetObject where
    toHeaders GetObject{..} = mconcat
        [ "If-Match"                                        =: _gorIfMatch
        , "If-Modified-Since"                               =: _gorIfModifiedSince
        , "If-None-Match"                                   =: _gorIfNoneMatch
        , "If-Unmodified-Since"                             =: _gorIfUnmodifiedSince
        , "Range"                                           =: _gorRange
        , "x-amz-server-side-encryption-customer-algorithm" =: _gorSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"       =: _gorSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"   =: _gorSSECustomerKeyMD5
        ]

data GetObjectOutput = GetObjectOutput
    { _gooAcceptRanges            :: Maybe Text
    , _gooBody                    :: RsBody
    , _gooCacheControl            :: Maybe Text
    , _gooContentDisposition      :: Maybe Text
    , _gooContentEncoding         :: Maybe Text
    , _gooContentLanguage         :: Maybe Text
    , _gooContentLength           :: Maybe Int
    , _gooContentType             :: Maybe Text
    , _gooDeleteMarker            :: Maybe Bool
    , _gooETag                    :: Maybe ETag
    , _gooExpiration              :: Maybe RFC822
    , _gooExpires                 :: Maybe RFC822
    , _gooLastModified            :: Maybe RFC822
    , _gooMetadata                :: Map Text Text
    , _gooMissingMeta             :: Maybe Int
    , _gooRestore                 :: Maybe Text
    , _gooSSECustomerAlgorithm    :: Maybe Text
    , _gooSSECustomerKeyMD5       :: Maybe Text
    , _gooServerSideEncryption    :: Maybe Text
    , _gooVersionId               :: Maybe ObjectVersionId
    , _gooWebsiteRedirectLocation :: Maybe Text
    } deriving (Show, Generic)

-- | 'GetObjectOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gooAcceptRanges' @::@ 'Maybe' 'Text'
--
-- * 'gooBody' @::@ 'RsBody'
--
-- * 'gooCacheControl' @::@ 'Maybe' 'Text'
--
-- * 'gooContentDisposition' @::@ 'Maybe' 'Text'
--
-- * 'gooContentEncoding' @::@ 'Maybe' 'Text'
--
-- * 'gooContentLanguage' @::@ 'Maybe' 'Text'
--
-- * 'gooContentLength' @::@ 'Maybe' 'Int'
--
-- * 'gooContentType' @::@ 'Maybe' 'Text'
--
-- * 'gooDeleteMarker' @::@ 'Maybe' 'Bool'
--
-- * 'gooETag' @::@ 'Maybe' 'ETag'
--
-- * 'gooExpiration' @::@ 'Maybe' 'UTCTime'
--
-- * 'gooExpires' @::@ 'Maybe' 'UTCTime'
--
-- * 'gooLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'gooMetadata' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'gooMissingMeta' @::@ 'Maybe' 'Int'
--
-- * 'gooRestore' @::@ 'Maybe' 'Text'
--
-- * 'gooSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'gooSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'gooServerSideEncryption' @::@ 'Maybe' 'Text'
--
-- * 'gooVersionId' @::@ 'Maybe' 'ObjectVersionId'
--
-- * 'gooWebsiteRedirectLocation' @::@ 'Maybe' 'Text'
--
getObjectOutput :: RsBody -- ^ 'gooBody'
                -> GetObjectOutput
getObjectOutput p1 = GetObjectOutput
    { _gooBody                    = p1
    , _gooDeleteMarker            = Nothing
    , _gooAcceptRanges            = Nothing
    , _gooExpiration              = Nothing
    , _gooRestore                 = Nothing
    , _gooLastModified            = Nothing
    , _gooContentLength           = Nothing
    , _gooETag                    = Nothing
    , _gooMissingMeta             = Nothing
    , _gooVersionId               = Nothing
    , _gooCacheControl            = Nothing
    , _gooContentDisposition      = Nothing
    , _gooContentEncoding         = Nothing
    , _gooContentLanguage         = Nothing
    , _gooContentType             = Nothing
    , _gooExpires                 = Nothing
    , _gooWebsiteRedirectLocation = Nothing
    , _gooServerSideEncryption    = Nothing
    , _gooMetadata                = mempty
    , _gooSSECustomerAlgorithm    = Nothing
    , _gooSSECustomerKeyMD5       = Nothing
    }

gooAcceptRanges :: Lens' GetObjectOutput (Maybe Text)
gooAcceptRanges = lens _gooAcceptRanges (\s a -> s { _gooAcceptRanges = a })

-- | Object data.
gooBody :: Lens' GetObjectOutput RsBody
gooBody = lens _gooBody (\s a -> s { _gooBody = a })

-- | Specifies caching behavior along the request/reply chain.
gooCacheControl :: Lens' GetObjectOutput (Maybe Text)
gooCacheControl = lens _gooCacheControl (\s a -> s { _gooCacheControl = a })

-- | Specifies presentational information for the object.
gooContentDisposition :: Lens' GetObjectOutput (Maybe Text)
gooContentDisposition =
    lens _gooContentDisposition (\s a -> s { _gooContentDisposition = a })

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
gooContentEncoding :: Lens' GetObjectOutput (Maybe Text)
gooContentEncoding =
    lens _gooContentEncoding (\s a -> s { _gooContentEncoding = a })

-- | The language the content is in.
gooContentLanguage :: Lens' GetObjectOutput (Maybe Text)
gooContentLanguage =
    lens _gooContentLanguage (\s a -> s { _gooContentLanguage = a })

-- | Size of the body in bytes.
gooContentLength :: Lens' GetObjectOutput (Maybe Int)
gooContentLength = lens _gooContentLength (\s a -> s { _gooContentLength = a })

-- | A standard MIME type describing the format of the object data.
gooContentType :: Lens' GetObjectOutput (Maybe Text)
gooContentType = lens _gooContentType (\s a -> s { _gooContentType = a })

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
gooDeleteMarker :: Lens' GetObjectOutput (Maybe Bool)
gooDeleteMarker = lens _gooDeleteMarker (\s a -> s { _gooDeleteMarker = a })

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
gooETag :: Lens' GetObjectOutput (Maybe ETag)
gooETag = lens _gooETag (\s a -> s { _gooETag = a })

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id
-- key value pairs providing object expiration information. The value of the
-- rule-id is URL encoded.
gooExpiration :: Lens' GetObjectOutput (Maybe UTCTime)
gooExpiration = lens _gooExpiration (\s a -> s { _gooExpiration = a })
    . mapping _Time

-- | The date and time at which the object is no longer cacheable.
gooExpires :: Lens' GetObjectOutput (Maybe UTCTime)
gooExpires = lens _gooExpires (\s a -> s { _gooExpires = a })
    . mapping _Time

-- | Last modified date of the object.
gooLastModified :: Lens' GetObjectOutput (Maybe UTCTime)
gooLastModified = lens _gooLastModified (\s a -> s { _gooLastModified = a })
    . mapping _Time

-- | A map of metadata to store with the object in S3.
gooMetadata :: Lens' GetObjectOutput (HashMap Text Text)
gooMetadata = lens _gooMetadata (\s a -> s { _gooMetadata = a })
    . _Map

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP
-- that supports more flexible metadata than the REST API. For example,
-- using SOAP, you can create metadata whose values are not legal HTTP
-- headers.
gooMissingMeta :: Lens' GetObjectOutput (Maybe Int)
gooMissingMeta = lens _gooMissingMeta (\s a -> s { _gooMissingMeta = a })

-- | Provides information about object restoration operation and expiration
-- time of the restored object copy.
gooRestore :: Lens' GetObjectOutput (Maybe Text)
gooRestore = lens _gooRestore (\s a -> s { _gooRestore = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
gooSSECustomerAlgorithm :: Lens' GetObjectOutput (Maybe Text)
gooSSECustomerAlgorithm =
    lens _gooSSECustomerAlgorithm (\s a -> s { _gooSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
gooSSECustomerKeyMD5 :: Lens' GetObjectOutput (Maybe Text)
gooSSECustomerKeyMD5 =
    lens _gooSSECustomerKeyMD5 (\s a -> s { _gooSSECustomerKeyMD5 = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
gooServerSideEncryption :: Lens' GetObjectOutput (Maybe Text)
gooServerSideEncryption =
    lens _gooServerSideEncryption (\s a -> s { _gooServerSideEncryption = a })

-- | Version of the object.
gooVersionId :: Lens' GetObjectOutput (Maybe ObjectVersionId)
gooVersionId = lens _gooVersionId (\s a -> s { _gooVersionId = a })

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
gooWebsiteRedirectLocation :: Lens' GetObjectOutput (Maybe Text)
gooWebsiteRedirectLocation =
    lens _gooWebsiteRedirectLocation
        (\s a -> s { _gooWebsiteRedirectLocation = a })

instance AWSRequest GetObject where
    type Sv GetObject = S3
    type Rs GetObject = GetObjectOutput

    request  = get'
    response = const . bodyResponse $ \h b -> GetObjectOutput
        <$> h ~:? "accept-ranges"
        <*> pure (RsBody b)
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
