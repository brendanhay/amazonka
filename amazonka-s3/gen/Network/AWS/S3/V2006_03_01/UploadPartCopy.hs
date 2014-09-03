{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.S3.V2006_03_01.UploadPartCopy
    (
    -- * Request
      UploadPartCopy
    -- ** Request constructor
    , uploadPartCopy
    -- ** Request lenses
    , upcrCopySource
    , upcrBucket
    , upcrUploadId
    , upcrKey
    , upcrPartNumber
    , upcrCopySourceIfMatch
    , upcrCopySourceIfModifiedSince
    , upcrCopySourceIfNoneMatch
    , upcrCopySourceIfUnmodifiedSince
    , upcrCopySourceRange
    , upcrCopySourceSSECustomerAlgorithm
    , upcrCopySourceSSECustomerKey
    , upcrCopySourceSSECustomerKeyMD5
    , upcrSSECustomerAlgorithm
    , upcrSSECustomerKey
    , upcrSSECustomerKeyMD5

    -- * Response
    , UploadPartCopyResponse
    -- ** Response lenses
    , upcoCopyPartResult
    , upcoCopySourceVersionId
    , upcoSSECustomerAlgorithm
    , upcoSSECustomerKeyMD5
    , upcoServerSideEncryption
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UploadPartCopy' request.
uploadPartCopy :: Text -- ^ 'upcrCopySource'
               -> BucketName -- ^ 'upcrBucket'
               -> Text -- ^ 'upcrUploadId'
               -> ObjectKey -- ^ 'upcrKey'
               -> Integer -- ^ 'upcrPartNumber'
               -> UploadPartCopy
uploadPartCopy p1 p2 p3 p4 p5 = UploadPartCopy
    { _upcrCopySource = p1
    , _upcrBucket = p2
    , _upcrUploadId = p3
    , _upcrKey = p4
    , _upcrPartNumber = p5
    , _upcrCopySourceIfMatch = Nothing
    , _upcrCopySourceIfModifiedSince = Nothing
    , _upcrCopySourceIfNoneMatch = Nothing
    , _upcrCopySourceIfUnmodifiedSince = Nothing
    , _upcrCopySourceRange = Nothing
    , _upcrCopySourceSSECustomerAlgorithm = Nothing
    , _upcrCopySourceSSECustomerKey = Nothing
    , _upcrCopySourceSSECustomerKeyMD5 = Nothing
    , _upcrSSECustomerAlgorithm = Nothing
    , _upcrSSECustomerKey = Nothing
    , _upcrSSECustomerKeyMD5 = Nothing
    }

data UploadPartCopy = UploadPartCopy
    { _upcrCopySource :: Text
      -- ^ The name of the source bucket and key name of the source object,
      -- separated by a slash (/). Must be URL-encoded.
    , _upcrBucket :: BucketName
    , _upcrUploadId :: Text
      -- ^ Upload ID identifying the multipart upload whose part is being
      -- copied.
    , _upcrKey :: ObjectKey
    , _upcrPartNumber :: Integer
      -- ^ Part number of part being copied.
    , _upcrCopySourceIfMatch :: Maybe Text
      -- ^ Copies the object if its entity tag (ETag) matches the specified
      -- tag.
    , _upcrCopySourceIfModifiedSince :: Maybe RFC822
      -- ^ Copies the object if it has been modified since the specified
      -- time.
    , _upcrCopySourceIfNoneMatch :: Maybe Text
      -- ^ Copies the object if its entity tag (ETag) is different than the
      -- specified ETag.
    , _upcrCopySourceIfUnmodifiedSince :: Maybe RFC822
      -- ^ Copies the object if it hasn't been modified since the specified
      -- time.
    , _upcrCopySourceRange :: Maybe Text
      -- ^ The range of bytes to copy from the source object. The range
      -- value must use the form bytes=first-last, where the first and
      -- last are the zero-based byte offsets to copy. For example,
      -- bytes=0-9 indicates that you want to copy the first ten bytes of
      -- the source. You can copy a range only if the source object is
      -- greater than 5 GB.
    , _upcrCopySourceSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use when decrypting the source object
      -- (e.g., AES256).
    , _upcrCopySourceSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use to decrypt the source object. The encryption key provided in
      -- this header must be one that was used when the source object was
      -- created.
    , _upcrCopySourceSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    , _upcrSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , _upcrSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header. This must be the same encryption key specified in the
      -- initiate multipart upload request.
    , _upcrSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    } deriving (Show, Generic)

-- | The name of the source bucket and key name of the source object, separated
-- by a slash (/). Must be URL-encoded.
upcrCopySource
    :: Functor f
    => (Text
    -> f (Text))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrCopySource f x =
    (\y -> x { _upcrCopySource = y })
       <$> f (_upcrCopySource x)
{-# INLINE upcrCopySource #-}

upcrBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrBucket f x =
    (\y -> x { _upcrBucket = y })
       <$> f (_upcrBucket x)
{-# INLINE upcrBucket #-}

-- | Upload ID identifying the multipart upload whose part is being copied.
upcrUploadId
    :: Functor f
    => (Text
    -> f (Text))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrUploadId f x =
    (\y -> x { _upcrUploadId = y })
       <$> f (_upcrUploadId x)
{-# INLINE upcrUploadId #-}

upcrKey
    :: Functor f
    => (ObjectKey
    -> f (ObjectKey))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrKey f x =
    (\y -> x { _upcrKey = y })
       <$> f (_upcrKey x)
{-# INLINE upcrKey #-}

-- | Part number of part being copied.
upcrPartNumber
    :: Functor f
    => (Integer
    -> f (Integer))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrPartNumber f x =
    (\y -> x { _upcrPartNumber = y })
       <$> f (_upcrPartNumber x)
{-# INLINE upcrPartNumber #-}

-- | Copies the object if its entity tag (ETag) matches the specified tag.
upcrCopySourceIfMatch
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrCopySourceIfMatch f x =
    (\y -> x { _upcrCopySourceIfMatch = y })
       <$> f (_upcrCopySourceIfMatch x)
{-# INLINE upcrCopySourceIfMatch #-}

-- | Copies the object if it has been modified since the specified time.
upcrCopySourceIfModifiedSince
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrCopySourceIfModifiedSince f x =
    (\y -> x { _upcrCopySourceIfModifiedSince = y })
       <$> f (_upcrCopySourceIfModifiedSince x)
{-# INLINE upcrCopySourceIfModifiedSince #-}

-- | Copies the object if its entity tag (ETag) is different than the specified
-- ETag.
upcrCopySourceIfNoneMatch
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrCopySourceIfNoneMatch f x =
    (\y -> x { _upcrCopySourceIfNoneMatch = y })
       <$> f (_upcrCopySourceIfNoneMatch x)
{-# INLINE upcrCopySourceIfNoneMatch #-}

-- | Copies the object if it hasn't been modified since the specified time.
upcrCopySourceIfUnmodifiedSince
    :: Functor f
    => (Maybe RFC822
    -> f (Maybe RFC822))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrCopySourceIfUnmodifiedSince f x =
    (\y -> x { _upcrCopySourceIfUnmodifiedSince = y })
       <$> f (_upcrCopySourceIfUnmodifiedSince x)
{-# INLINE upcrCopySourceIfUnmodifiedSince #-}

-- | The range of bytes to copy from the source object. The range value must use
-- the form bytes=first-last, where the first and last are the zero-based byte
-- offsets to copy. For example, bytes=0-9 indicates that you want to copy the
-- first ten bytes of the source. You can copy a range only if the source
-- object is greater than 5 GB.
upcrCopySourceRange
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrCopySourceRange f x =
    (\y -> x { _upcrCopySourceRange = y })
       <$> f (_upcrCopySourceRange x)
{-# INLINE upcrCopySourceRange #-}

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
upcrCopySourceSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrCopySourceSSECustomerAlgorithm f x =
    (\y -> x { _upcrCopySourceSSECustomerAlgorithm = y })
       <$> f (_upcrCopySourceSSECustomerAlgorithm x)
{-# INLINE upcrCopySourceSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header must
-- be one that was used when the source object was created.
upcrCopySourceSSECustomerKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrCopySourceSSECustomerKey f x =
    (\y -> x { _upcrCopySourceSSECustomerKey = y })
       <$> f (_upcrCopySourceSSECustomerKey x)
{-# INLINE upcrCopySourceSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcrCopySourceSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrCopySourceSSECustomerKeyMD5 f x =
    (\y -> x { _upcrCopySourceSSECustomerKeyMD5 = y })
       <$> f (_upcrCopySourceSSECustomerKeyMD5 x)
{-# INLINE upcrCopySourceSSECustomerKeyMD5 #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
upcrSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrSSECustomerAlgorithm f x =
    (\y -> x { _upcrSSECustomerAlgorithm = y })
       <$> f (_upcrSSECustomerAlgorithm x)
{-# INLINE upcrSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
-- This must be the same encryption key specified in the initiate multipart
-- upload request.
upcrSSECustomerKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrSSECustomerKey f x =
    (\y -> x { _upcrSSECustomerKey = y })
       <$> f (_upcrSSECustomerKey x)
{-# INLINE upcrSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcrSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartCopy
    -> f UploadPartCopy
upcrSSECustomerKeyMD5 f x =
    (\y -> x { _upcrSSECustomerKeyMD5 = y })
       <$> f (_upcrSSECustomerKeyMD5 x)
{-# INLINE upcrSSECustomerKeyMD5 #-}

instance ToPath UploadPartCopy where
    toPath UploadPartCopy{..} = mconcat
        [ "/"
        , toBS _upcrBucket
        , "/"
        , toBS _upcrKey
        ]

instance ToQuery UploadPartCopy where
    toQuery UploadPartCopy{..} = mconcat
        [ "partNumber" =? _upcrPartNumber
        , "uploadId" =? _upcrUploadId
        ]

instance ToHeaders UploadPartCopy where
    toHeaders UploadPartCopy{..} = concat
        [ "x-amz-copy-source" =: _upcrCopySource
        , "x-amz-copy-source-if-match" =: _upcrCopySourceIfMatch
        , "x-amz-copy-source-if-modified-since" =: _upcrCopySourceIfModifiedSince
        , "x-amz-copy-source-if-none-match" =: _upcrCopySourceIfNoneMatch
        , "x-amz-copy-source-if-unmodified-since" =: _upcrCopySourceIfUnmodifiedSince
        , "x-amz-copy-source-range" =: _upcrCopySourceRange
        , "x-amz-copy-source-server-side-encryption-customer-algorithm" =: _upcrCopySourceSSECustomerAlgorithm
        , "x-amz-copy-source-server-side-encryption-customer-key" =: _upcrCopySourceSSECustomerKey
        , "x-amz-copy-source-server-side-encryption-customer-key-MD5" =: _upcrCopySourceSSECustomerKeyMD5
        , "x-amz-server-side-encryption-customer-algorithm" =: _upcrSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _upcrSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _upcrSSECustomerKeyMD5
        ]

instance ToBody UploadPartCopy

data UploadPartCopyResponse = UploadPartCopyResponse
    { _upcoCopyPartResult :: Maybe CopyPartResult
    , _upcoCopySourceVersionId :: Maybe Text
      -- ^ The version of the source object that was copied, if you have
      -- enabled versioning on the source bucket.
    , _upcoSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _upcoSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , _upcoServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Show, Generic)

upcoCopyPartResult
    :: Functor f
    => (Maybe CopyPartResult
    -> f (Maybe CopyPartResult))
    -> UploadPartCopyResponse
    -> f UploadPartCopyResponse
upcoCopyPartResult f x =
    (\y -> x { _upcoCopyPartResult = y })
       <$> f (_upcoCopyPartResult x)
{-# INLINE upcoCopyPartResult #-}

-- | The version of the source object that was copied, if you have enabled
-- versioning on the source bucket.
upcoCopySourceVersionId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartCopyResponse
    -> f UploadPartCopyResponse
upcoCopySourceVersionId f x =
    (\y -> x { _upcoCopySourceVersionId = y })
       <$> f (_upcoCopySourceVersionId x)
{-# INLINE upcoCopySourceVersionId #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
upcoSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartCopyResponse
    -> f UploadPartCopyResponse
upcoSSECustomerAlgorithm f x =
    (\y -> x { _upcoSSECustomerAlgorithm = y })
       <$> f (_upcoSSECustomerAlgorithm x)
{-# INLINE upcoSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
upcoSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartCopyResponse
    -> f UploadPartCopyResponse
upcoSSECustomerKeyMD5 f x =
    (\y -> x { _upcoSSECustomerKeyMD5 = y })
       <$> f (_upcoSSECustomerKeyMD5 x)
{-# INLINE upcoSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
upcoServerSideEncryption
    :: Functor f
    => (Maybe ServerSideEncryption
    -> f (Maybe ServerSideEncryption))
    -> UploadPartCopyResponse
    -> f UploadPartCopyResponse
upcoServerSideEncryption f x =
    (\y -> x { _upcoServerSideEncryption = y })
       <$> f (_upcoServerSideEncryption x)
{-# INLINE upcoServerSideEncryption #-}

instance AWSRequest UploadPartCopy where
    type Sv UploadPartCopy = S3
    type Rs UploadPartCopy = UploadPartCopyResponse

    request = put
    response _ = cursorResponse $ \hs xml ->
        pure UploadPartCopyResponse
            <*> xml %|? "CopyPartResult"
            <*> hs ~:? "x-amz-copy-source-version-id"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
            <*> hs ~:? "x-amz-server-side-encryption"
