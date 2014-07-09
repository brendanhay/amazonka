{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

-- | Default UploadPart request.
uploadPart :: BucketName -- ^ 'uprBucket'
           -> Text -- ^ 'uprUploadId'
           -> ObjectKey -- ^ 'uprKey'
           -> Integer -- ^ 'uprPartNumber'
           -> ByteString -- ^ 'uprBody'
           -> UploadPart
uploadPart p1 p2 p3 p4 p5 = UploadPart
    { uprBucket = p1
    , uprUploadId = p2
    , uprKey = p3
    , uprPartNumber = p4
    , uprBody = p5
    , uprContentLength = Nothing
    , uprContentMD5 = Nothing
    , uprSSECustomerAlgorithm = Nothing
    , uprSSECustomerKey = Nothing
    , uprSSECustomerKeyMD5 = Nothing
    }

data UploadPart = UploadPart
    { uprBucket :: BucketName
    , uprUploadId :: Text
      -- ^ Upload ID identifying the multipart upload whose part is being
      -- uploaded.
    , uprKey :: ObjectKey
    , uprPartNumber :: Integer
      -- ^ Part number of part being uploaded.
    , uprBody :: ByteString
    , uprContentLength :: Maybe Integer
      -- ^ Size of the body in bytes. This parameter is useful when the size
      -- of the body cannot be determined automatically.
    , uprContentMD5 :: Maybe Text
    , uprSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , uprSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header. This must be the same encryption key specified in the
      -- initiate multipart upload request.
    , uprSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    } deriving (Eq, Show, Generic)

instance ToPath UploadPart where
    toPath UploadPart{..} = mconcat
        [ "/"
        , toBS uprBucket
        , "/"
        , toBS uprKey
        ]

instance ToQuery UploadPart

instance ToHeaders UploadPart where
    toHeaders UploadPart{..} = concat
        [ "Content-Length" =: uprContentLength
        , "Content-MD5" =: uprContentMD5
        , "x-amz-server-side-encryption-customer-algorithm" =: uprSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: uprSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: uprSSECustomerKeyMD5
        ]

instance ToBody UploadPart where
    toBody = undefined -- toBody . uprBody

instance AWSRequest UploadPart where
    type Sv UploadPart = S3

    request  = put
    response = headerResponse $ \hs ->
        pure UploadPartResponse
            <*> hs ~:? "ETag" hs
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm" hs
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5" hs
            <*> hs ~:? "x-amz-server-side-encryption" hs

data instance Rs UploadPart = UploadPartResponse
    { upoETag :: Maybe ETag
      -- ^ Entity tag for the uploaded object.
    , upoSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , upoSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , upoServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Eq, Show, Generic)
