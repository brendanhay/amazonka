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
    , mkUploadPartCopyRequest
    -- ** Request lenses
    , upcrBucket
    , upcrCopySource
    , upcrCopySourceIfMatch
    , upcrCopySourceIfModifiedSince
    , upcrCopySourceIfNoneMatch
    , upcrCopySourceIfUnmodifiedSince
    , upcrCopySourceRange
    , upcrKey
    , upcrPartNumber
    , upcrUploadId
    , upcrSSECustomerAlgorithm
    , upcrSSECustomerKey
    , upcrSSECustomerKeyMD5
    , upcrCopySourceSSECustomerAlgorithm
    , upcrCopySourceSSECustomerKey
    , upcrCopySourceSSECustomerKeyMD5

    -- * Response
    , UploadPartCopyResponse
    -- ** Response lenses
    , upcoCopySourceVersionId
    , upcoCopyPartResult
    , upcoServerSideEncryption
    , upcoSSECustomerAlgorithm
    , upcoSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UploadPartCopy' request.
mkUploadPartCopyRequest :: BucketName -- ^ 'upcrBucket'
                        -> Text -- ^ 'upcrCopySource'
                        -> ObjectKey -- ^ 'upcrKey'
                        -> Integer -- ^ 'upcrPartNumber'
                        -> Text -- ^ 'upcrUploadId'
                        -> UploadPartCopy
mkUploadPartCopyRequest p1 p2 p3 p4 p5 = UploadPartCopy
    { _upcrBucket = p1
    , _upcrCopySource = p2
    , _upcrCopySourceIfMatch = Nothing
    , _upcrCopySourceIfModifiedSince = Nothing
    , _upcrCopySourceIfNoneMatch = Nothing
    , _upcrCopySourceIfUnmodifiedSince = Nothing
    , _upcrCopySourceRange = Nothing
    , _upcrKey = p8
    , _upcrPartNumber = p9
    , _upcrUploadId = p10
    , _upcrSSECustomerAlgorithm = Nothing
    , _upcrSSECustomerKey = Nothing
    , _upcrSSECustomerKeyMD5 = Nothing
    , _upcrCopySourceSSECustomerAlgorithm = Nothing
    , _upcrCopySourceSSECustomerKey = Nothing
    , _upcrCopySourceSSECustomerKeyMD5 = Nothing
    }
{-# INLINE mkUploadPartCopyRequest #-}

data UploadPartCopy = UploadPartCopy
    { _upcrBucket :: BucketName
    , _upcrCopySource :: Text
      -- ^ The name of the source bucket and key name of the source object,
      -- separated by a slash (/). Must be URL-encoded.
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
    , _upcrKey :: ObjectKey
    , _upcrPartNumber :: Integer
      -- ^ Part number of part being copied.
    , _upcrUploadId :: Text
      -- ^ Upload ID identifying the multipart upload whose part is being
      -- copied.
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
    } deriving (Show, Generic)

upcrBucket :: Lens' UploadPartCopy (BucketName)
upcrBucket = lens _upcrBucket (\s a -> s { _upcrBucket = a })
{-# INLINE upcrBucket #-}

-- | The name of the source bucket and key name of the source object, separated
-- by a slash (/). Must be URL-encoded.
upcrCopySource :: Lens' UploadPartCopy (Text)
upcrCopySource = lens _upcrCopySource (\s a -> s { _upcrCopySource = a })
{-# INLINE upcrCopySource #-}

-- | Copies the object if its entity tag (ETag) matches the specified tag.
upcrCopySourceIfMatch :: Lens' UploadPartCopy (Maybe Text)
upcrCopySourceIfMatch = lens _upcrCopySourceIfMatch (\s a -> s { _upcrCopySourceIfMatch = a })
{-# INLINE upcrCopySourceIfMatch #-}

-- | Copies the object if it has been modified since the specified time.
upcrCopySourceIfModifiedSince :: Lens' UploadPartCopy (Maybe RFC822)
upcrCopySourceIfModifiedSince = lens _upcrCopySourceIfModifiedSince (\s a -> s { _upcrCopySourceIfModifiedSince = a })
{-# INLINE upcrCopySourceIfModifiedSince #-}

-- | Copies the object if its entity tag (ETag) is different than the specified
-- ETag.
upcrCopySourceIfNoneMatch :: Lens' UploadPartCopy (Maybe Text)
upcrCopySourceIfNoneMatch = lens _upcrCopySourceIfNoneMatch (\s a -> s { _upcrCopySourceIfNoneMatch = a })
{-# INLINE upcrCopySourceIfNoneMatch #-}

-- | Copies the object if it hasn't been modified since the specified time.
upcrCopySourceIfUnmodifiedSince :: Lens' UploadPartCopy (Maybe RFC822)
upcrCopySourceIfUnmodifiedSince = lens _upcrCopySourceIfUnmodifiedSince (\s a -> s { _upcrCopySourceIfUnmodifiedSince = a })
{-# INLINE upcrCopySourceIfUnmodifiedSince #-}

-- | The range of bytes to copy from the source object. The range value must use
-- the form bytes=first-last, where the first and last are the zero-based byte
-- offsets to copy. For example, bytes=0-9 indicates that you want to copy the
-- first ten bytes of the source. You can copy a range only if the source
-- object is greater than 5 GB.
upcrCopySourceRange :: Lens' UploadPartCopy (Maybe Text)
upcrCopySourceRange = lens _upcrCopySourceRange (\s a -> s { _upcrCopySourceRange = a })
{-# INLINE upcrCopySourceRange #-}

upcrKey :: Lens' UploadPartCopy (ObjectKey)
upcrKey = lens _upcrKey (\s a -> s { _upcrKey = a })
{-# INLINE upcrKey #-}

-- | Part number of part being copied.
upcrPartNumber :: Lens' UploadPartCopy (Integer)
upcrPartNumber = lens _upcrPartNumber (\s a -> s { _upcrPartNumber = a })
{-# INLINE upcrPartNumber #-}

-- | Upload ID identifying the multipart upload whose part is being copied.
upcrUploadId :: Lens' UploadPartCopy (Text)
upcrUploadId = lens _upcrUploadId (\s a -> s { _upcrUploadId = a })
{-# INLINE upcrUploadId #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
upcrSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcrSSECustomerAlgorithm = lens _upcrSSECustomerAlgorithm (\s a -> s { _upcrSSECustomerAlgorithm = a })
{-# INLINE upcrSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
-- This must be the same encryption key specified in the initiate multipart
-- upload request.
upcrSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcrSSECustomerKey = lens _upcrSSECustomerKey (\s a -> s { _upcrSSECustomerKey = a })
{-# INLINE upcrSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcrSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcrSSECustomerKeyMD5 = lens _upcrSSECustomerKeyMD5 (\s a -> s { _upcrSSECustomerKeyMD5 = a })
{-# INLINE upcrSSECustomerKeyMD5 #-}

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
upcrCopySourceSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcrCopySourceSSECustomerAlgorithm = lens _upcrCopySourceSSECustomerAlgorithm (\s a -> s { _upcrCopySourceSSECustomerAlgorithm = a })
{-# INLINE upcrCopySourceSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header must
-- be one that was used when the source object was created.
upcrCopySourceSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcrCopySourceSSECustomerKey = lens _upcrCopySourceSSECustomerKey (\s a -> s { _upcrCopySourceSSECustomerKey = a })
{-# INLINE upcrCopySourceSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcrCopySourceSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcrCopySourceSSECustomerKeyMD5 = lens _upcrCopySourceSSECustomerKeyMD5 (\s a -> s { _upcrCopySourceSSECustomerKeyMD5 = a })
{-# INLINE upcrCopySourceSSECustomerKeyMD5 #-}

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

instance ToHeaders UploadPartCopy

instance ToBody UploadPartCopy

data UploadPartCopyResponse = UploadPartCopyResponse
    { _upcoCopySourceVersionId :: Maybe Text
      -- ^ The version of the source object that was copied, if you have
      -- enabled versioning on the source bucket.
    , _upcoCopyPartResult :: Maybe CopyPartResult
    , _upcoServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _upcoSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _upcoSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    } deriving (Show, Generic)

-- | The version of the source object that was copied, if you have enabled
-- versioning on the source bucket.
upcoCopySourceVersionId :: Lens' UploadPartCopyResponse (Maybe Text)
upcoCopySourceVersionId = lens _upcoCopySourceVersionId (\s a -> s { _upcoCopySourceVersionId = a })
{-# INLINE upcoCopySourceVersionId #-}

upcoCopyPartResult :: Lens' UploadPartCopyResponse (Maybe CopyPartResult)
upcoCopyPartResult = lens _upcoCopyPartResult (\s a -> s { _upcoCopyPartResult = a })
{-# INLINE upcoCopyPartResult #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
upcoServerSideEncryption :: Lens' UploadPartCopyResponse (Maybe ServerSideEncryption)
upcoServerSideEncryption = lens _upcoServerSideEncryption (\s a -> s { _upcoServerSideEncryption = a })
{-# INLINE upcoServerSideEncryption #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
upcoSSECustomerAlgorithm :: Lens' UploadPartCopyResponse (Maybe Text)
upcoSSECustomerAlgorithm = lens _upcoSSECustomerAlgorithm (\s a -> s { _upcoSSECustomerAlgorithm = a })
{-# INLINE upcoSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
upcoSSECustomerKeyMD5 :: Lens' UploadPartCopyResponse (Maybe Text)
upcoSSECustomerKeyMD5 = lens _upcoSSECustomerKeyMD5 (\s a -> s { _upcoSSECustomerKeyMD5 = a })
{-# INLINE upcoSSECustomerKeyMD5 #-}

instance AWSRequest UploadPartCopy where
    type Sv UploadPartCopy = S3
    type Rs UploadPartCopy = UploadPartCopyResponse

    request = put
    response _ = cursorResponse $ \hs xml ->
        pure UploadPartCopyResponse
            <*> hs ~:? "x-amz-copy-source-version-id"
            <*> xml %|? "CopyPartResult"
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
