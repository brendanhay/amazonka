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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetObject.html>
module Network.AWS.S3.GetObject
    (
    -- * Request
      GetObject
    -- ** Request constructor
    , getObject
    -- ** Request lenses
    , goBucket
    , goIfMatch
    , goIfModifiedSince
    , goIfNoneMatch
    , goIfUnmodifiedSince
    , goKey
    , goRange
    , goResponseCacheControl
    , goResponseContentDisposition
    , goResponseContentEncoding
    , goResponseContentLanguage
    , goResponseContentType
    , goResponseExpires
    , goSSECustomerAlgorithm
    , goSSECustomerKey
    , goSSECustomerKeyMD5
    , goSSEKMSKeyId
    , goVersionId

    -- * Response
    , GetObjectResponse
    -- ** Response constructor
    , getObjectResponse
    -- ** Response lenses
    , gorAcceptRanges
    , gorBody
    , gorCacheControl
    , gorContentDisposition
    , gorContentEncoding
    , gorContentLanguage
    , gorContentLength
    , gorContentType
    , gorDeleteMarker
    , gorETag
    , gorExpiration
    , gorExpires
    , gorLastModified
    , gorMetadata
    , gorMissingMeta
    , gorRestore
    , gorSSECustomerAlgorithm
    , gorSSECustomerKeyMD5
    , gorSSEKMSKeyId
    , gorServerSideEncryption
    , gorVersionId
    , gorWebsiteRedirectLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

data GetObject = GetObject
    { _goBucket                     :: Text
    , _goIfMatch                    :: Maybe Text
    , _goIfModifiedSince            :: Maybe RFC822
    , _goIfNoneMatch                :: Maybe Text
    , _goIfUnmodifiedSince          :: Maybe RFC822
    , _goKey                        :: Text
    , _goRange                      :: Maybe Text
    , _goResponseCacheControl       :: Maybe Text
    , _goResponseContentDisposition :: Maybe Text
    , _goResponseContentEncoding    :: Maybe Text
    , _goResponseContentLanguage    :: Maybe Text
    , _goResponseContentType        :: Maybe Text
    , _goResponseExpires            :: Maybe RFC822
    , _goSSECustomerAlgorithm       :: Maybe Text
    , _goSSECustomerKey             :: Maybe (Sensitive Text)
    , _goSSECustomerKeyMD5          :: Maybe Text
    , _goSSEKMSKeyId                :: Maybe (Sensitive Text)
    , _goVersionId                  :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'GetObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goBucket' @::@ 'Text'
--
-- * 'goIfMatch' @::@ 'Maybe' 'Text'
--
-- * 'goIfModifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'goIfNoneMatch' @::@ 'Maybe' 'Text'
--
-- * 'goIfUnmodifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'goKey' @::@ 'Text'
--
-- * 'goRange' @::@ 'Maybe' 'Text'
--
-- * 'goResponseCacheControl' @::@ 'Maybe' 'Text'
--
-- * 'goResponseContentDisposition' @::@ 'Maybe' 'Text'
--
-- * 'goResponseContentEncoding' @::@ 'Maybe' 'Text'
--
-- * 'goResponseContentLanguage' @::@ 'Maybe' 'Text'
--
-- * 'goResponseContentType' @::@ 'Maybe' 'Text'
--
-- * 'goResponseExpires' @::@ 'Maybe' 'UTCTime'
--
-- * 'goSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'goSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'goSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'goSSEKMSKeyId' @::@ 'Maybe' 'Text'
--
-- * 'goVersionId' @::@ 'Maybe' 'Text'
--
getObject :: Text -- ^ 'goBucket'
          -> Text -- ^ 'goKey'
          -> GetObject
getObject p1 p2 = GetObject
    { _goBucket                     = p1
    , _goKey                        = p2
    , _goIfMatch                    = Nothing
    , _goIfModifiedSince            = Nothing
    , _goIfNoneMatch                = Nothing
    , _goIfUnmodifiedSince          = Nothing
    , _goRange                      = Nothing
    , _goResponseCacheControl       = Nothing
    , _goResponseContentDisposition = Nothing
    , _goResponseContentEncoding    = Nothing
    , _goResponseContentLanguage    = Nothing
    , _goResponseContentType        = Nothing
    , _goResponseExpires            = Nothing
    , _goVersionId                  = Nothing
    , _goSSECustomerAlgorithm       = Nothing
    , _goSSECustomerKey             = Nothing
    , _goSSECustomerKeyMD5          = Nothing
    , _goSSEKMSKeyId                = Nothing
    }

goBucket :: Lens' GetObject Text
goBucket = lens _goBucket (\s a -> s { _goBucket = a })

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
goIfMatch :: Lens' GetObject (Maybe Text)
goIfMatch = lens _goIfMatch (\s a -> s { _goIfMatch = a })

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
goIfModifiedSince :: Lens' GetObject (Maybe UTCTime)
goIfModifiedSince =
    lens _goIfModifiedSince (\s a -> s { _goIfModifiedSince = a })
        . mapping _Time

-- | Return the object only if its entity tag (ETag) is different from the one
-- specified, otherwise return a 304 (not modified).
goIfNoneMatch :: Lens' GetObject (Maybe Text)
goIfNoneMatch = lens _goIfNoneMatch (\s a -> s { _goIfNoneMatch = a })

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
goIfUnmodifiedSince :: Lens' GetObject (Maybe UTCTime)
goIfUnmodifiedSince =
    lens _goIfUnmodifiedSince (\s a -> s { _goIfUnmodifiedSince = a })
        . mapping _Time

goKey :: Lens' GetObject Text
goKey = lens _goKey (\s a -> s { _goKey = a })

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
goRange :: Lens' GetObject (Maybe Text)
goRange = lens _goRange (\s a -> s { _goRange = a })

-- | Sets the Cache-Control header of the response.
goResponseCacheControl :: Lens' GetObject (Maybe Text)
goResponseCacheControl =
    lens _goResponseCacheControl (\s a -> s { _goResponseCacheControl = a })

-- | Sets the Content-Disposition header of the response.
goResponseContentDisposition :: Lens' GetObject (Maybe Text)
goResponseContentDisposition =
    lens _goResponseContentDisposition
        (\s a -> s { _goResponseContentDisposition = a })

-- | Sets the Content-Encoding header of the response.
goResponseContentEncoding :: Lens' GetObject (Maybe Text)
goResponseContentEncoding =
    lens _goResponseContentEncoding
        (\s a -> s { _goResponseContentEncoding = a })

-- | Sets the Content-Language header of the response.
goResponseContentLanguage :: Lens' GetObject (Maybe Text)
goResponseContentLanguage =
    lens _goResponseContentLanguage
        (\s a -> s { _goResponseContentLanguage = a })

-- | Sets the Content-Type header of the response.
goResponseContentType :: Lens' GetObject (Maybe Text)
goResponseContentType =
    lens _goResponseContentType (\s a -> s { _goResponseContentType = a })

-- | Sets the Expires header of the response.
goResponseExpires :: Lens' GetObject (Maybe UTCTime)
goResponseExpires =
    lens _goResponseExpires (\s a -> s { _goResponseExpires = a })
        . mapping _Time

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
goSSECustomerAlgorithm :: Lens' GetObject (Maybe Text)
goSSECustomerAlgorithm =
    lens _goSSECustomerAlgorithm (\s a -> s { _goSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
goSSECustomerKey :: Lens' GetObject (Maybe Text)
goSSECustomerKey = lens _goSSECustomerKey (\s a -> s { _goSSECustomerKey = a }) . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
goSSECustomerKeyMD5 :: Lens' GetObject (Maybe Text)
goSSECustomerKeyMD5 =
    lens _goSSECustomerKeyMD5 (\s a -> s { _goSSECustomerKeyMD5 = a })

-- | Specifies the AWS KMS key ID to use for object encryption.
goSSEKMSKeyId :: Lens' GetObject (Maybe Text)
goSSEKMSKeyId = lens _goSSEKMSKeyId (\s a -> s { _goSSEKMSKeyId = a }) . mapping _Sensitive

-- | VersionId used to reference a specific version of the object.
goVersionId :: Lens' GetObject (Maybe Text)
goVersionId = lens _goVersionId (\s a -> s { _goVersionId = a })

data GetObjectResponse = GetObjectResponse
    { _gorAcceptRanges            :: Maybe Text
    , _gorBody                    :: RsBody
    , _gorCacheControl            :: Maybe Text
    , _gorContentDisposition      :: Maybe Text
    , _gorContentEncoding         :: Maybe Text
    , _gorContentLanguage         :: Maybe Text
    , _gorContentLength           :: Maybe Int
    , _gorContentType             :: Maybe Text
    , _gorDeleteMarker            :: Maybe Bool
    , _gorETag                    :: Maybe Text
    , _gorExpiration              :: Maybe RFC822
    , _gorExpires                 :: Maybe RFC822
    , _gorLastModified            :: Maybe RFC822
    , _gorMetadata                :: Map (CI Text) Text
    , _gorMissingMeta             :: Maybe Int
    , _gorRestore                 :: Maybe Text
    , _gorSSECustomerAlgorithm    :: Maybe Text
    , _gorSSECustomerKeyMD5       :: Maybe Text
    , _gorSSEKMSKeyId             :: Maybe (Sensitive Text)
    , _gorServerSideEncryption    :: Maybe Text
    , _gorVersionId               :: Maybe Text
    , _gorWebsiteRedirectLocation :: Maybe Text
    } deriving (Show)

-- | 'GetObjectResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gorAcceptRanges' @::@ 'Maybe' 'Text'
--
-- * 'gorBody' @::@ 'RsBody'
--
-- * 'gorCacheControl' @::@ 'Maybe' 'Text'
--
-- * 'gorContentDisposition' @::@ 'Maybe' 'Text'
--
-- * 'gorContentEncoding' @::@ 'Maybe' 'Text'
--
-- * 'gorContentLanguage' @::@ 'Maybe' 'Text'
--
-- * 'gorContentLength' @::@ 'Maybe' 'Int'
--
-- * 'gorContentType' @::@ 'Maybe' 'Text'
--
-- * 'gorDeleteMarker' @::@ 'Maybe' 'Bool'
--
-- * 'gorETag' @::@ 'Maybe' 'Text'
--
-- * 'gorExpiration' @::@ 'Maybe' 'UTCTime'
--
-- * 'gorExpires' @::@ 'Maybe' 'UTCTime'
--
-- * 'gorLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'gorMetadata' @::@ 'HashMap' ('CI' 'Text') 'Text'
--
-- * 'gorMissingMeta' @::@ 'Maybe' 'Int'
--
-- * 'gorRestore' @::@ 'Maybe' 'Text'
--
-- * 'gorSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'gorSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'gorSSEKMSKeyId' @::@ 'Maybe' 'Text'
--
-- * 'gorServerSideEncryption' @::@ 'Maybe' 'Text'
--
-- * 'gorVersionId' @::@ 'Maybe' 'Text'
--
-- * 'gorWebsiteRedirectLocation' @::@ 'Maybe' 'Text'
--
getObjectResponse :: RsBody -- ^ 'gorBody'
                  -> GetObjectResponse
getObjectResponse p1 = GetObjectResponse
    { _gorBody                    = p1
    , _gorDeleteMarker            = Nothing
    , _gorAcceptRanges            = Nothing
    , _gorExpiration              = Nothing
    , _gorRestore                 = Nothing
    , _gorLastModified            = Nothing
    , _gorContentLength           = Nothing
    , _gorETag                    = Nothing
    , _gorMissingMeta             = Nothing
    , _gorVersionId               = Nothing
    , _gorCacheControl            = Nothing
    , _gorContentDisposition      = Nothing
    , _gorContentEncoding         = Nothing
    , _gorContentLanguage         = Nothing
    , _gorContentType             = Nothing
    , _gorExpires                 = Nothing
    , _gorWebsiteRedirectLocation = Nothing
    , _gorServerSideEncryption    = Nothing
    , _gorMetadata                = mempty
    , _gorSSECustomerAlgorithm    = Nothing
    , _gorSSECustomerKeyMD5       = Nothing
    , _gorSSEKMSKeyId             = Nothing
    }

gorAcceptRanges :: Lens' GetObjectResponse (Maybe Text)
gorAcceptRanges = lens _gorAcceptRanges (\s a -> s { _gorAcceptRanges = a })

-- | Object data.
gorBody :: Lens' GetObjectResponse RsBody
gorBody = lens _gorBody (\s a -> s { _gorBody = a })

-- | Specifies caching behavior along the request/reply chain.
gorCacheControl :: Lens' GetObjectResponse (Maybe Text)
gorCacheControl = lens _gorCacheControl (\s a -> s { _gorCacheControl = a })

-- | Specifies presentational information for the object.
gorContentDisposition :: Lens' GetObjectResponse (Maybe Text)
gorContentDisposition =
    lens _gorContentDisposition (\s a -> s { _gorContentDisposition = a })

-- | Specifies what content encodings have been applied to the object and thus
-- what decoding mechanisms must be applied to obtain the media-type
-- referenced by the Content-Type header field.
gorContentEncoding :: Lens' GetObjectResponse (Maybe Text)
gorContentEncoding =
    lens _gorContentEncoding (\s a -> s { _gorContentEncoding = a })

-- | The language the content is in.
gorContentLanguage :: Lens' GetObjectResponse (Maybe Text)
gorContentLanguage =
    lens _gorContentLanguage (\s a -> s { _gorContentLanguage = a })

-- | Size of the body in bytes.
gorContentLength :: Lens' GetObjectResponse (Maybe Int)
gorContentLength = lens _gorContentLength (\s a -> s { _gorContentLength = a })

-- | A standard MIME type describing the format of the object data.
gorContentType :: Lens' GetObjectResponse (Maybe Text)
gorContentType = lens _gorContentType (\s a -> s { _gorContentType = a })

-- | Specifies whether the object retrieved was (true) or was not (false) a
-- Delete Marker. If false, this response header does not appear in the
-- response.
gorDeleteMarker :: Lens' GetObjectResponse (Maybe Bool)
gorDeleteMarker = lens _gorDeleteMarker (\s a -> s { _gorDeleteMarker = a })

-- | An ETag is an opaque identifier assigned by a web server to a specific
-- version of a resource found at a URL.
gorETag :: Lens' GetObjectResponse (Maybe Text)
gorETag = lens _gorETag (\s a -> s { _gorETag = a })

-- | If the object expiration is configured (see PUT Bucket lifecycle), the
-- response includes this header. It includes the expiry-date and rule-id
-- key value pairs providing object expiration information. The value of the
-- rule-id is URL encoded.
gorExpiration :: Lens' GetObjectResponse (Maybe UTCTime)
gorExpiration = lens _gorExpiration (\s a -> s { _gorExpiration = a }) . mapping _Time

-- | The date and time at which the object is no longer cacheable.
gorExpires :: Lens' GetObjectResponse (Maybe UTCTime)
gorExpires = lens _gorExpires (\s a -> s { _gorExpires = a }) . mapping _Time

-- | Last modified date of the object.
gorLastModified :: Lens' GetObjectResponse (Maybe UTCTime)
gorLastModified = lens _gorLastModified (\s a -> s { _gorLastModified = a }) . mapping _Time

-- | A map of metadata to store with the object in S3.
gorMetadata :: Lens' GetObjectResponse (HashMap (CI Text) Text)
gorMetadata = lens _gorMetadata (\s a -> s { _gorMetadata = a }) . _Map

-- | This is set to the number of metadata entries not returned in x-amz-meta
-- headers. This can happen if you create metadata using an API like SOAP
-- that supports more flexible metadata than the REST API. For example,
-- using SOAP, you can create metadata whose values are not legal HTTP
-- headers.
gorMissingMeta :: Lens' GetObjectResponse (Maybe Int)
gorMissingMeta = lens _gorMissingMeta (\s a -> s { _gorMissingMeta = a })

-- | Provides information about object restoration operation and expiration
-- time of the restored object copy.
gorRestore :: Lens' GetObjectResponse (Maybe Text)
gorRestore = lens _gorRestore (\s a -> s { _gorRestore = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
gorSSECustomerAlgorithm :: Lens' GetObjectResponse (Maybe Text)
gorSSECustomerAlgorithm =
    lens _gorSSECustomerAlgorithm (\s a -> s { _gorSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
gorSSECustomerKeyMD5 :: Lens' GetObjectResponse (Maybe Text)
gorSSECustomerKeyMD5 =
    lens _gorSSECustomerKeyMD5 (\s a -> s { _gorSSECustomerKeyMD5 = a })

-- | If present, specifies the AWS KMS key used to encrypt the object.
gorSSEKMSKeyId :: Lens' GetObjectResponse (Maybe Text)
gorSSEKMSKeyId = lens _gorSSEKMSKeyId (\s a -> s { _gorSSEKMSKeyId = a }) . mapping _Sensitive

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
gorServerSideEncryption :: Lens' GetObjectResponse (Maybe Text)
gorServerSideEncryption =
    lens _gorServerSideEncryption (\s a -> s { _gorServerSideEncryption = a })

-- | Version of the object.
gorVersionId :: Lens' GetObjectResponse (Maybe Text)
gorVersionId = lens _gorVersionId (\s a -> s { _gorVersionId = a })

-- | If the bucket is configured as a website, redirects requests for this
-- object to another object in the same bucket or to an external URL. Amazon
-- S3 stores the value of this header in the object metadata.
gorWebsiteRedirectLocation :: Lens' GetObjectResponse (Maybe Text)
gorWebsiteRedirectLocation =
    lens _gorWebsiteRedirectLocation
        (\s a -> s { _gorWebsiteRedirectLocation = a })

instance ToPath GetObject where
    toPath GetObject{..} = mconcat
        [ "/"
        , toText _goBucket
        , "/"
        , toText _goKey
        ]

instance ToQuery GetObject where
    toQuery GetObject{..} = mconcat
        [ "response-cache-control"       =? _goResponseCacheControl
        , "response-content-disposition" =? _goResponseContentDisposition
        , "response-content-encoding"    =? _goResponseContentEncoding
        , "response-content-language"    =? _goResponseContentLanguage
        , "response-content-type"        =? _goResponseContentType
        , "response-expires"             =? _goResponseExpires
        , "versionId"                    =? _goVersionId
        ]

instance ToHeaders GetObject where
    toHeaders GetObject{..} = mconcat
        [ "If-Match"                                        =: _goIfMatch
        , "If-Modified-Since"                               =: _goIfModifiedSince
        , "If-None-Match"                                   =: _goIfNoneMatch
        , "If-Unmodified-Since"                             =: _goIfUnmodifiedSince
        , "Range"                                           =: _goRange
        , "x-amz-server-side-encryption-customer-algorithm" =: _goSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"       =: _goSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"   =: _goSSECustomerKeyMD5
        , "x-amz-server-side-encryption-aws-kms-key-id"     =: _goSSEKMSKeyId
        ]

instance ToXMLRoot GetObject where
    toXMLRoot = const (namespaced ns "GetObject" [])

instance ToXML GetObject

instance AWSRequest GetObject where
    type Sv GetObject = S3
    type Rs GetObject = GetObjectResponse

    request  = get
    response = bodyResponse $ \h b -> GetObjectResponse
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
        <*> h ~:? "x-amz-server-side-encryption-aws-kms-key-id"
        <*> h ~:? "x-amz-server-side-encryption"
        <*> h ~:? "x-amz-version-id"
        <*> h ~:? "x-amz-website-redirect-location"
