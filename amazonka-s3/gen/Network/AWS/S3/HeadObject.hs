{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , horBucket
    , horIfMatch
    , horIfModifiedSince
    , horIfNoneMatch
    , horIfUnmodifiedSince
    , horKey
    , horRange
    , horSSECustomerAlgorithm
    , horSSECustomerKey
    , horSSECustomerKeyMD5
    , horVersionId

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
    { _horBucket               :: BucketName
    , _horIfMatch              :: Maybe Text
    , _horIfModifiedSince      :: Maybe RFC822
    , _horIfNoneMatch          :: Maybe Text
    , _horIfUnmodifiedSince    :: Maybe RFC822
    , _horKey                  :: ObjectKey
    , _horRange                :: Maybe Text
    , _horSSECustomerAlgorithm :: Maybe Text
    , _horSSECustomerKey       :: Maybe (Sensitive Text)
    , _horSSECustomerKeyMD5    :: Maybe Text
    , _horVersionId            :: Maybe ObjectVersionId
    } deriving (Eq, Ord, Show, Generic)

-- | 'HeadObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'horBucket' @::@ 'BucketName'
--
-- * 'horIfMatch' @::@ 'Maybe' 'Text'
--
-- * 'horIfModifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'horIfNoneMatch' @::@ 'Maybe' 'Text'
--
-- * 'horIfUnmodifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'horKey' @::@ 'ObjectKey'
--
-- * 'horRange' @::@ 'Maybe' 'Text'
--
-- * 'horSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'horSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'horSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'horVersionId' @::@ 'Maybe' 'ObjectVersionId'
--
headObject :: BucketName -- ^ 'horBucket'
           -> ObjectKey -- ^ 'horKey'
           -> HeadObject
headObject p1 p2 = HeadObject
    { _horBucket               = p1
    , _horKey                  = p2
    , _horIfMatch              = Nothing
    , _horIfModifiedSince      = Nothing
    , _horIfNoneMatch          = Nothing
    , _horIfUnmodifiedSince    = Nothing
    , _horRange                = Nothing
    , _horVersionId            = Nothing
    , _horSSECustomerAlgorithm = Nothing
    , _horSSECustomerKey       = Nothing
    , _horSSECustomerKeyMD5    = Nothing
    }

horBucket :: Lens' HeadObject BucketName
horBucket = lens _horBucket (\s a -> s { _horBucket = a })

-- | Return the object only if its entity tag (ETag) is the same as the one
-- specified, otherwise return a 412 (precondition failed).
horIfMatch :: Lens' HeadObject (Maybe Text)
horIfMatch = lens _horIfMatch (\s a -> s { _horIfMatch = a })

-- | Return the object only if it has been modified since the specified time,
-- otherwise return a 304 (not modified).
horIfModifiedSince :: Lens' HeadObject (Maybe UTCTime)
horIfModifiedSince =
    lens _horIfModifiedSince (\s a -> s { _horIfModifiedSince = a })
        . mapping _Time

-- | Return the object only if its entity tag (ETag) is different from the one
-- specified, otherwise return a 304 (not modified).
horIfNoneMatch :: Lens' HeadObject (Maybe Text)
horIfNoneMatch = lens _horIfNoneMatch (\s a -> s { _horIfNoneMatch = a })

-- | Return the object only if it has not been modified since the specified
-- time, otherwise return a 412 (precondition failed).
horIfUnmodifiedSince :: Lens' HeadObject (Maybe UTCTime)
horIfUnmodifiedSince =
    lens _horIfUnmodifiedSince (\s a -> s { _horIfUnmodifiedSince = a })
        . mapping _Time

horKey :: Lens' HeadObject ObjectKey
horKey = lens _horKey (\s a -> s { _horKey = a })

-- | Downloads the specified range bytes of an object. For more information
-- about the HTTP Range header, go to
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
horRange :: Lens' HeadObject (Maybe Text)
horRange = lens _horRange (\s a -> s { _horRange = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
horSSECustomerAlgorithm :: Lens' HeadObject (Maybe Text)
horSSECustomerAlgorithm =
    lens _horSSECustomerAlgorithm (\s a -> s { _horSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
horSSECustomerKey :: Lens' HeadObject (Maybe Text)
horSSECustomerKey =
    lens _horSSECustomerKey (\s a -> s { _horSSECustomerKey = a })
        . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
horSSECustomerKeyMD5 :: Lens' HeadObject (Maybe Text)
horSSECustomerKeyMD5 =
    lens _horSSECustomerKeyMD5 (\s a -> s { _horSSECustomerKeyMD5 = a })

-- | VersionId used to reference a specific version of the object.
horVersionId :: Lens' HeadObject (Maybe ObjectVersionId)
horVersionId = lens _horVersionId (\s a -> s { _horVersionId = a })

instance ToPath HeadObject where
    toPath HeadObject{..} = mconcat
        [ "/"
        , toText _horBucket
        , "/"
        , toText _horKey
        ]

instance ToQuery HeadObject where

instance ToHeaders HeadObject where
    toHeaders HeadObject{..} = mconcat
        [ "If-Match"                                        =: _horIfMatch
        , "If-Modified-Since"                               =: _horIfModifiedSince
        , "If-None-Match"                                   =: _horIfNoneMatch
        , "If-Unmodified-Since"                             =: _horIfUnmodifiedSince
        , "Range"                                           =: _horRange
        , "x-amz-server-side-encryption-customer-algorithm" =: _horSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"       =: _horSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"   =: _horSSECustomerKeyMD5
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
    , _hooETag                    :: Maybe ETag
    , _hooExpiration              :: Maybe RFC822
    , _hooExpires                 :: Maybe RFC822
    , _hooLastModified            :: Maybe RFC822
    , _hooMetadata                :: Map Text Text
    , _hooMissingMeta             :: Maybe Int
    , _hooRestore                 :: Maybe Text
    , _hooSSECustomerAlgorithm    :: Maybe Text
    , _hooSSECustomerKeyMD5       :: Maybe Text
    , _hooServerSideEncryption    :: Maybe Text
    , _hooVersionId               :: Maybe ObjectVersionId
    , _hooWebsiteRedirectLocation :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

instance AWSRequest HeadObject where
    type Sv HeadObject = S3
    type Rs HeadObject = HeadObjectOutput

    request  = head
    response = const . xmlResponse $ \h x ->
        <$> h ~: "accept-ranges"
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
