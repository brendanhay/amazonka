{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.UploadPartCopy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Uploads a part by copying data from an existing object as data source.
module Network.AWS.S3.V2006_03_01.UploadPartCopy where

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
import           Network.HTTP.Client  (Response)
import           Prelude              hiding (head)

-- | Default UploadPartCopy request.
uploadPartCopy :: Text -- ^ 'upcrCopySource'
               -> BucketName -- ^ 'upcrBucket'
               -> Text -- ^ 'upcrUploadId'
               -> ObjectKey -- ^ 'upcrKey'
               -> Integer -- ^ 'upcrPartNumber'
               -> UploadPartCopy
uploadPartCopy p1 p2 p3 p4 p5 = UploadPartCopy
    { upcrCopySource = p1
    , upcrBucket = p2
    , upcrUploadId = p3
    , upcrKey = p4
    , upcrPartNumber = p5
    , upcrCopySourceIfMatch = Nothing
    , upcrCopySourceIfModifiedSince = Nothing
    , upcrCopySourceIfNoneMatch = Nothing
    , upcrCopySourceIfUnmodifiedSince = Nothing
    , upcrCopySourceRange = Nothing
    , upcrCopySourceSSECustomerAlgorithm = Nothing
    , upcrCopySourceSSECustomerKey = Nothing
    , upcrCopySourceSSECustomerKeyMD5 = Nothing
    , upcrSSECustomerAlgorithm = Nothing
    , upcrSSECustomerKey = Nothing
    , upcrSSECustomerKeyMD5 = Nothing
    }

data UploadPartCopy = UploadPartCopy
    { upcrCopySource :: Text
      -- ^ The name of the source bucket and key name of the source object,
      -- separated by a slash (/). Must be URL-encoded.
    , upcrBucket :: BucketName
    , upcrUploadId :: Text
      -- ^ Upload ID identifying the multipart upload whose part is being
      -- copied.
    , upcrKey :: ObjectKey
    , upcrPartNumber :: Integer
      -- ^ Part number of part being copied.
    , upcrCopySourceIfMatch :: Maybe Text
      -- ^ Copies the object if its entity tag (ETag) matches the specified
      -- tag.
    , upcrCopySourceIfModifiedSince :: Maybe RFC822
      -- ^ Copies the object if it has been modified since the specified
      -- time.
    , upcrCopySourceIfNoneMatch :: Maybe Text
      -- ^ Copies the object if its entity tag (ETag) is different than the
      -- specified ETag.
    , upcrCopySourceIfUnmodifiedSince :: Maybe RFC822
      -- ^ Copies the object if it hasn't been modified since the specified
      -- time.
    , upcrCopySourceRange :: Maybe Text
      -- ^ The range of bytes to copy from the source object. The range
      -- value must use the form bytes=first-last, where the first and
      -- last are the zero-based byte offsets to copy. For example,
      -- bytes=0-9 indicates that you want to copy the first ten bytes of
      -- the source. You can copy a range only if the source object is
      -- greater than 5 GB.
    , upcrCopySourceSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use when decrypting the source object
      -- (e.g., AES256).
    , upcrCopySourceSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use to decrypt the source object. The encryption key provided in
      -- this header must be one that was used when the source object was
      -- created.
    , upcrCopySourceSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    , upcrSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , upcrSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header. This must be the same encryption key specified in the
      -- initiate multipart upload request.
    , upcrSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    } deriving (Eq, Show, Generic)

instance ToPath UploadPartCopy where
    toPath UploadPartCopy{..} = mconcat
        [ "/"
        , toBS upcrBucket
        , "/"
        , toBS upcrKey
        ]

instance ToQuery UploadPartCopy

instance ToHeaders UploadPartCopy where
    toHeaders UploadPartCopy{..} = concat
        [ "x-amz-copy-source" =: upcrCopySource
        , "x-amz-copy-source-if-match" =: upcrCopySourceIfMatch
        , "x-amz-copy-source-if-modified-since" =: upcrCopySourceIfModifiedSince
        , "x-amz-copy-source-if-none-match" =: upcrCopySourceIfNoneMatch
        , "x-amz-copy-source-if-unmodified-since" =: upcrCopySourceIfUnmodifiedSince
        , "x-amz-copy-source-range" =: upcrCopySourceRange
        , "x-amz-copy-source-server-side-encryption-customer-algorithm" =: upcrCopySourceSSECustomerAlgorithm
        , "x-amz-copy-source-server-side-encryption-customer-key" =: upcrCopySourceSSECustomerKey
        , "x-amz-copy-source-server-side-encryption-customer-key-MD5" =: upcrCopySourceSSECustomerKeyMD5
        , "x-amz-server-side-encryption-customer-algorithm" =: upcrSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: upcrSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: upcrSSECustomerKeyMD5
        ]

instance ToBody UploadPartCopy

instance AWSRequest UploadPartCopy where
    type Sv UploadPartCopy = S3

    request  = put
    response = xmlResponse $ \hs xml ->
        pure UploadPartCopyResponse
            <*> xml %|? "CopyPartResult"
            <*> hs ~:? "x-amz-copy-source-version-id"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
            <*> hs ~:? "x-amz-server-side-encryption"

data instance Rs UploadPartCopy = UploadPartCopyResponse
    { upcoCopyPartResult :: Maybe CopyPartResult
    , upcoCopySourceVersionId :: Maybe Text
      -- ^ The version of the source object that was copied, if you have
      -- enabled versioning on the source bucket.
    , upcoSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , upcoSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , upcoServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Eq, Show, Generic)
