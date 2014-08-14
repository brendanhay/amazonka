{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.S3.V2006_03_01.UploadPart
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Uploads a part in a multipart upload. Note: After you initiate multipart
-- upload and upload one or more parts, you must either complete or abort
-- multipart upload in order to stop getting charged for storage of the
-- uploaded parts. Only after you either complete or abort multipart upload,
-- Amazon S3 frees up the parts storage and stops charging you for the parts
-- storage.
module Network.AWS.S3.V2006_03_01.UploadPart where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UploadPart' request.
uploadPart :: BucketName -- ^ '_uprBucket'
           -> Text -- ^ '_uprUploadId'
           -> ObjectKey -- ^ '_uprKey'
           -> Integer -- ^ '_uprPartNumber'
           -> RqBody -- ^ '_uprBody'
           -> UploadPart
uploadPart p1 p2 p3 p4 p5 = UploadPart
    { _uprBucket = p1
    , _uprUploadId = p2
    , _uprKey = p3
    , _uprPartNumber = p4
    , _uprBody = p5
    , _uprContentLength = Nothing
    , _uprContentMD5 = Nothing
    , _uprSSECustomerAlgorithm = Nothing
    , _uprSSECustomerKey = Nothing
    , _uprSSECustomerKeyMD5 = Nothing
    }

data UploadPart = UploadPart
    { _uprBucket :: BucketName
    , _uprUploadId :: Text
      -- ^ Upload ID identifying the multipart upload whose part is being
      -- uploaded.
    , _uprKey :: ObjectKey
    , _uprPartNumber :: Integer
      -- ^ Part number of part being uploaded.
    , _uprBody :: RqBody
    , _uprContentLength :: Maybe Integer
      -- ^ Size of the body in bytes. This parameter is useful when the size
      -- of the body cannot be determined automatically.
    , _uprContentMD5 :: Maybe Text
    , _uprSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , _uprSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header. This must be the same encryption key specified in the
      -- initiate multipart upload request.
    , _uprSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    } deriving (Show, Generic)

makeLenses ''UploadPart

instance ToPath UploadPart where
    toPath UploadPart{..} = mconcat
        [ "/"
        , toBS _uprBucket
        , "/"
        , toBS _uprKey
        ]

instance ToQuery UploadPart where
    toQuery UploadPart{..} = mconcat
        [ "partNumber" =? _uprPartNumber
        , "uploadId" =? _uprUploadId
        ]

instance ToHeaders UploadPart where
    toHeaders UploadPart{..} = concat
        [ "Content-Length" =: _uprContentLength
        , "Content-MD5" =: _uprContentMD5
        , "x-amz-server-side-encryption-customer-algorithm" =: _uprSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _uprSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _uprSSECustomerKeyMD5
        ]

instance ToBody UploadPart where
    toBody = toBody . _uprBody

data UploadPartResponse = UploadPartResponse
    { _upoETag :: Maybe ETag
      -- ^ Entity tag for the uploaded object.
    , _upoSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _upoSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , _upoServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Show, Generic)

makeLenses ''UploadPartResponse

instance AWSRequest UploadPart where
    type Sv UploadPart = S3
    type Rs UploadPart = UploadPartResponse

    request = put
    response _ = headerResponse $ \hs ->
        pure UploadPartResponse
            <*> hs ~:? "ETag"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
            <*> hs ~:? "x-amz-server-side-encryption"
