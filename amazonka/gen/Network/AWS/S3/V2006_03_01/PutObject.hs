{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)
True

-- | Default PutObject request.
putObject :: BucketName -- ^ '_porBucket'
          -> ObjectKey -- ^ '_porKey'
          -> RqBody -- ^ '_porBody'
          -> PutObject
putObject p1 p2 p3 = PutObject
    { _porBucket = p1
    , _porKey = p2
    , _porBody = p3
    , _porMetadata = mempty
    , _porCacheControl = Nothing
    , _porContentDisposition = Nothing
    , _porContentEncoding = Nothing
    , _porContentLanguage = Nothing
    , _porContentLength = Nothing
    , _porContentMD5 = Nothing
    , _porContentType = Nothing
    , _porExpires = Nothing
    , _porGrantFullControl = Nothing
    , _porGrantRead = Nothing
    , _porGrantReadACP = Nothing
    , _porGrantWriteACP = Nothing
    , _porACL = Nothing
    , _porSSECustomerAlgorithm = Nothing
    , _porSSECustomerKey = Nothing
    , _porSSECustomerKeyMD5 = Nothing
    , _porServerSideEncryption = Nothing
    , _porStorageClass = Nothing
    , _porWebsiteRedirectLocation = Nothing
    }

data PutObject = PutObject
    { _porBucket :: BucketName
    , _porKey :: ObjectKey
    , _porBody :: RqBody
    , _porMetadata :: HashMap Text Text
      -- ^ A map of metadata to store with the object in S3.
    , _porCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , _porContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , _porContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object
      -- and thus what decoding mechanisms must be applied to obtain the
      -- media-type referenced by the Content-Type header field.
    , _porContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , _porContentLength :: Maybe Integer
      -- ^ Size of the body in bytes. This parameter is useful when the size
      -- of the body cannot be determined automatically.
    , _porContentMD5 :: Maybe Text
    , _porContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , _porExpires :: Maybe RFC822
      -- ^ The date and time at which the object is no longer cacheable.
    , _porGrantFullControl :: Maybe Text
      -- ^ Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on
      -- the object.
    , _porGrantRead :: Maybe Text
      -- ^ Allows grantee to read the object data and its metadata.
    , _porGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the object ACL.
    , _porGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable object.
    , _porACL :: Maybe ObjectCannedACL
      -- ^ The canned ACL to apply to the object.
    , _porSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , _porSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header.
    , _porSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    , _porServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _porStorageClass :: Maybe StorageClass
      -- ^ The type of storage to use for the object. Defaults to
      -- 'STANDARD'.
    , _porWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for
      -- this object to another object in the same bucket or to an
      -- external URL. Amazon S3 stores the value of this header in the
      -- object metadata.
    } deriving (Generic)

instance ToPath PutObject where
    toPath PutObject{..} = mconcat
        [ "/"
        , toBS _porBucket
        , "/"
        , toBS _porKey
        ]

instance ToQuery PutObject

instance ToHeaders PutObject where
    toHeaders PutObject{..} = concat
        [ "x-amz-meta-" =: _porMetadata
        , "Cache-Control" =: _porCacheControl
        , "Content-Disposition" =: _porContentDisposition
        , "Content-Encoding" =: _porContentEncoding
        , "Content-Language" =: _porContentLanguage
        , "Content-Length" =: _porContentLength
        , "Content-MD5" =: _porContentMD5
        , "Content-Type" =: _porContentType
        , "Expires" =: _porExpires
        , "x-amz-grant-full-control" =: _porGrantFullControl
        , "x-amz-grant-read" =: _porGrantRead
        , "x-amz-grant-read-acp" =: _porGrantReadACP
        , "x-amz-grant-write-acp" =: _porGrantWriteACP
        , "x-amz-acl" =: _porACL
        , "x-amz-server-side-encryption-customer-algorithm" =: _porSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _porSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _porSSECustomerKeyMD5
        , "x-amz-server-side-encryption" =: _porServerSideEncryption
        , "x-amz-storage-class" =: _porStorageClass
        , "x-amz-website-redirect-location" =: _porWebsiteRedirectLocation
        ]

instance ToBody PutObject where
    toBody = toBody . _porBody

instance AWSRequest PutObject where
    type Sv PutObject = S3
    type Rs PutObject = PutObjectResponse

    request = put

    response _ = headerResponse $ \hs ->
        pure PutObjectResponse
            <*> hs ~:? "ETag"
            <*> hs ~:? "x-amz-expiration"
            <*> hs ~:? "x-amz-version-id"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
            <*> hs ~:? "x-amz-server-side-encryption"

data PutObjectResponse = PutObjectResponse
    { _pooETag :: Maybe ETag
      -- ^ Entity tag for the uploaded object.
    , _pooExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured, this will contain the
      -- expiration date (expiry-date) and rule ID (rule-id). The value of
      -- rule-id is URL encoded.
    , _pooVersionId :: Maybe ObjectVersionId
      -- ^ Version of the object.
    , _pooSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _pooSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , _pooServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Generic)
