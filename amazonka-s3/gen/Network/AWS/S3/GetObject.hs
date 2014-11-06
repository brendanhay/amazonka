{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

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
import Network.AWS.Request.Xml
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
    } deriving (Eq, Ord, Show, Generic)

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

instance ToBody GetObject

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
    } deriving (Eq, Show, Generic)

instance AWSRequest GetObject where
    type Sv GetObject = S3
    type Rs GetObject = GetObjectOutput

    request  = get
    response = const . bodyResponse $ \h b ->
        <$> h ~: "accept-ranges"
        <*> pure (RsBody b)
        <*> h ~: "Cache-Control"
        <*> h ~: "Content-Disposition"
        <*> h ~: "Content-Encoding"
        <*> h ~: "Content-Language"
        <*> h ~: "Content-Length"
        <*> h ~: "Content-Type"
        <*> h ~: "x-amz-delete-marker"
        <*> h ~: "ETag"
        <*> h ~: "x-amz-expiration"
        <*> h ~: "Expires"
        <*> h ~: "Last-Modified"
        <*> (Map <$> h ~:: "x-amz-meta-")
        <*> h ~: "x-amz-missing-meta"
        <*> h ~: "x-amz-restore"
        <*> h ~: "x-amz-server-side-encryption-customer-algorithm"
        <*> h ~: "x-amz-server-side-encryption-customer-key-MD5"
        <*> h ~: "x-amz-server-side-encryption"
        <*> h ~: "x-amz-version-id"
        <*> h ~: "x-amz-website-redirect-location"
