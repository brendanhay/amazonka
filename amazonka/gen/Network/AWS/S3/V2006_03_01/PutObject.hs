{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds an object to a bucket.
module Network.AWS.S3.V2006_03_01.PutObject where

import           Control.Applicative
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

-- | Default PutObject request.
putObject :: BucketName -- ^ 'porBucket'
          -> ObjectKey -- ^ 'porKey'
          -> ByteString -- ^ 'porBody'
          -> PutObject
putObject p1 p2 p3 = PutObject
    { porBucket = p1
    , porKey = p2
    , porBody = p3
    , porMetadata = mempty
    , porCacheControl = Nothing
    , porContentDisposition = Nothing
    , porContentEncoding = Nothing
    , porContentLanguage = Nothing
    , porContentLength = Nothing
    , porContentMD5 = Nothing
    , porContentType = Nothing
    , porExpires = Nothing
    , porGrantFullControl = Nothing
    , porGrantRead = Nothing
    , porGrantReadACP = Nothing
    , porGrantWriteACP = Nothing
    , porACL = Nothing
    , porSSECustomerAlgorithm = Nothing
    , porSSECustomerKey = Nothing
    , porSSECustomerKeyMD5 = Nothing
    , porServerSideEncryption = Nothing
    , porStorageClass = Nothing
    , porWebsiteRedirectLocation = Nothing
    }

data PutObject = PutObject
    { porBucket :: BucketName
    , porKey :: ObjectKey
    , porBody :: ByteString
    , porMetadata :: HashMap Text Text
      -- ^ A map of metadata to store with the object in S3.
    , porCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , porContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , porContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object
      -- and thus what decoding mechanisms must be applied to obtain the
      -- media-type referenced by the Content-Type header field.
    , porContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , porContentLength :: Maybe Integer
      -- ^ Size of the body in bytes. This parameter is useful when the size
      -- of the body cannot be determined automatically.
    , porContentMD5 :: Maybe Text
    , porContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , porExpires :: Maybe RFC822
      -- ^ The date and time at which the object is no longer cacheable.
    , porGrantFullControl :: Maybe Text
      -- ^ Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on
      -- the object.
    , porGrantRead :: Maybe Text
      -- ^ Allows grantee to read the object data and its metadata.
    , porGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the object ACL.
    , porGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable object.
    , porACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
    , porSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , porSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header.
    , porSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    , porServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , porStorageClass :: Maybe StorageClass
      -- ^ The type of storage to use for the object. Defaults to
      -- 'STANDARD'.
    , porWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for
      -- this object to another object in the same bucket or to an
      -- external URL. Amazon S3 stores the value of this header in the
      -- object metadata.
    } deriving (Eq, Show, Generic)

instance ToPath PutObject where
    toPath PutObject{..} = mconcat
        [ "/"
        , toBS porBucket
        , "/"
        , toBS porKey
        ]

instance ToQuery PutObject

instance ToHeaders PutObject where
    toHeaders PutObject{..} = concat
        [ "x-amz-meta-" =: porMetadata
        , "Cache-Control" =: porCacheControl
        , "Content-Disposition" =: porContentDisposition
        , "Content-Encoding" =: porContentEncoding
        , "Content-Language" =: porContentLanguage
        , "Content-Length" =: porContentLength
        , "Content-MD5" =: porContentMD5
        , "Content-Type" =: porContentType
        , "Expires" =: porExpires
        , "x-amz-grant-full-control" =: porGrantFullControl
        , "x-amz-grant-read" =: porGrantRead
        , "x-amz-grant-read-acp" =: porGrantReadACP
        , "x-amz-grant-write-acp" =: porGrantWriteACP
        , "x-amz-acl" =: porACL
        , "x-amz-server-side-encryption-customer-algorithm" =: porSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: porSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: porSSECustomerKeyMD5
        , "x-amz-server-side-encryption" =: porServerSideEncryption
        , "x-amz-storage-class" =: porStorageClass
        , "x-amz-website-redirect-location" =: porWebsiteRedirectLocation
        ]

instance ToBody PutObject where
    toBody = undefined -- toBody . porBody

instance AWSRequest PutObject where
    type Sv PutObject = S3

    request  = put
    response = headerResponse $ \hs ->
        pure PutObjectResponse
            <*> hs ~:? "ETag" hs
            <*> hs ~:? "x-amz-expiration" hs
            <*> hs ~:? "x-amz-version-id" hs
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm" hs
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5" hs
            <*> hs ~:? "x-amz-server-side-encryption" hs

data instance Rs PutObject = PutObjectResponse
    { pooETag :: Maybe ETag
      -- ^ Entity tag for the uploaded object.
    , pooExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured, this will contain the
      -- expiration date (expiry-date) and rule ID (rule-id). The value of
      -- rule-id is URL encoded.
    , pooVersionId :: Maybe ObjectVersionId
      -- ^ Version of the object.
    , pooSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , pooSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , pooServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Eq, Show, Generic)
