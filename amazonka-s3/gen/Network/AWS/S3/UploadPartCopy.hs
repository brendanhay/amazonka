{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.UploadPartCopy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Uploads a part by copying data from an existing object as data source.
module Network.AWS.S3.UploadPartCopy
    (
    -- * Request
      UploadPartCopy
    -- ** Request constructor
    , uploadPartCopy
    -- ** Request lenses
    , upcBucket
    , upcCopySource
    , upcCopySourceIfMatch
    , upcCopySourceIfModifiedSince
    , upcCopySourceIfNoneMatch
    , upcCopySourceIfUnmodifiedSince
    , upcCopySourceRange
    , upcKey
    , upcPartNumber
    , upcUploadId
    , upcSSECustomerAlgorithm
    , upcSSECustomerKey
    , upcSSECustomerKeyMD5
    , upcCopySourceSSECustomerAlgorithm
    , upcCopySourceSSECustomerKey
    , upcCopySourceSSECustomerKeyMD5

    -- * Response
    , UploadPartCopyResponse
    -- ** Response constructor
    , uploadPartCopyResponse
    -- ** Response lenses
    , upcrCopySourceVersionId
    , upcrCopyPartResult
    , upcrServerSideEncryption
    , upcrSSECustomerAlgorithm
    , upcrSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data UploadPartCopy = UploadPartCopy
    { _upcBucket :: BucketName
    , _upcCopySource :: Text
    , _upcCopySourceIfMatch :: Maybe Text
    , _upcCopySourceIfModifiedSince :: Maybe RFC822
    , _upcCopySourceIfNoneMatch :: Maybe Text
    , _upcCopySourceIfUnmodifiedSince :: Maybe RFC822
    , _upcCopySourceRange :: Maybe Text
    , _upcKey :: ObjectKey
    , _upcPartNumber :: !Integer
    , _upcUploadId :: Text
    , _upcSSECustomerAlgorithm :: Maybe Text
    , _upcSSECustomerKey :: Maybe Text
    , _upcSSECustomerKeyMD5 :: Maybe Text
    , _upcCopySourceSSECustomerAlgorithm :: Maybe Text
    , _upcCopySourceSSECustomerKey :: Maybe Text
    , _upcCopySourceSSECustomerKeyMD5 :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UploadPartCopy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @CopySource ::@ @Text@
--
-- * @CopySourceIfMatch ::@ @Maybe Text@
--
-- * @CopySourceIfModifiedSince ::@ @Maybe RFC822@
--
-- * @CopySourceIfNoneMatch ::@ @Maybe Text@
--
-- * @CopySourceIfUnmodifiedSince ::@ @Maybe RFC822@
--
-- * @CopySourceRange ::@ @Maybe Text@
--
-- * @Key ::@ @ObjectKey@
--
-- * @PartNumber ::@ @Integer@
--
-- * @UploadId ::@ @Text@
--
-- * @SSECustomerAlgorithm ::@ @Maybe Text@
--
-- * @SSECustomerKey ::@ @Maybe Text@
--
-- * @SSECustomerKeyMD5 ::@ @Maybe Text@
--
-- * @CopySourceSSECustomerAlgorithm ::@ @Maybe Text@
--
-- * @CopySourceSSECustomerKey ::@ @Maybe Text@
--
-- * @CopySourceSSECustomerKeyMD5 ::@ @Maybe Text@
--
uploadPartCopy :: BucketName -- ^ 'upcBucket'
                 -> Text -- ^ 'upcUploadId'
                 -> Text -- ^ 'upcCopySource'
                 -> ObjectKey -- ^ 'upcKey'
                 -> Integer -- ^ 'upcPartNumber'
                 -> UploadPartCopy
uploadPartCopy p1 p10 p2 p8 p9 = UploadPartCopy
    { _upcBucket = p1
    , _upcCopySource = p2
    , _upcCopySourceIfMatch = Nothing
    , _upcCopySourceIfModifiedSince = Nothing
    , _upcCopySourceIfNoneMatch = Nothing
    , _upcCopySourceIfUnmodifiedSince = Nothing
    , _upcCopySourceRange = Nothing
    , _upcKey = p8
    , _upcPartNumber = p9
    , _upcUploadId = p10
    , _upcSSECustomerAlgorithm = Nothing
    , _upcSSECustomerKey = Nothing
    , _upcSSECustomerKeyMD5 = Nothing
    , _upcCopySourceSSECustomerAlgorithm = Nothing
    , _upcCopySourceSSECustomerKey = Nothing
    , _upcCopySourceSSECustomerKeyMD5 = Nothing
    }

upcBucket :: Lens' UploadPartCopy BucketName
upcBucket = lens _upcBucket (\s a -> s { _upcBucket = a })

-- | The name of the source bucket and key name of the source object, separated
-- by a slash (/). Must be URL-encoded.
upcCopySource :: Lens' UploadPartCopy Text
upcCopySource = lens _upcCopySource (\s a -> s { _upcCopySource = a })

-- | Copies the object if its entity tag (ETag) matches the specified tag.
upcCopySourceIfMatch :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceIfMatch =
    lens _upcCopySourceIfMatch (\s a -> s { _upcCopySourceIfMatch = a })

-- | Copies the object if it has been modified since the specified time.
upcCopySourceIfModifiedSince :: Lens' UploadPartCopy (Maybe RFC822)
upcCopySourceIfModifiedSince =
    lens _upcCopySourceIfModifiedSince
         (\s a -> s { _upcCopySourceIfModifiedSince = a })

-- | Copies the object if its entity tag (ETag) is different than the specified
-- ETag.
upcCopySourceIfNoneMatch :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceIfNoneMatch =
    lens _upcCopySourceIfNoneMatch
         (\s a -> s { _upcCopySourceIfNoneMatch = a })

-- | Copies the object if it hasn't been modified since the specified time.
upcCopySourceIfUnmodifiedSince :: Lens' UploadPartCopy (Maybe RFC822)
upcCopySourceIfUnmodifiedSince =
    lens _upcCopySourceIfUnmodifiedSince
         (\s a -> s { _upcCopySourceIfUnmodifiedSince = a })

-- | The range of bytes to copy from the source object. The range value must use
-- the form bytes=first-last, where the first and last are the zero-based byte
-- offsets to copy. For example, bytes=0-9 indicates that you want to copy the
-- first ten bytes of the source. You can copy a range only if the source
-- object is greater than 5 GB.
upcCopySourceRange :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceRange =
    lens _upcCopySourceRange (\s a -> s { _upcCopySourceRange = a })

upcKey :: Lens' UploadPartCopy ObjectKey
upcKey = lens _upcKey (\s a -> s { _upcKey = a })

-- | Part number of part being copied.
upcPartNumber :: Lens' UploadPartCopy Integer
upcPartNumber = lens _upcPartNumber (\s a -> s { _upcPartNumber = a })

-- | Upload ID identifying the multipart upload whose part is being copied.
upcUploadId :: Lens' UploadPartCopy Text
upcUploadId = lens _upcUploadId (\s a -> s { _upcUploadId = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
upcSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerAlgorithm =
    lens _upcSSECustomerAlgorithm
         (\s a -> s { _upcSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
-- This must be the same encryption key specified in the initiate multipart
-- upload request.
upcSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerKey =
    lens _upcSSECustomerKey (\s a -> s { _upcSSECustomerKey = a })

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerKeyMD5 =
    lens _upcSSECustomerKeyMD5 (\s a -> s { _upcSSECustomerKeyMD5 = a })

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
upcCopySourceSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerAlgorithm =
    lens _upcCopySourceSSECustomerAlgorithm
         (\s a -> s { _upcCopySourceSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header must
-- be one that was used when the source object was created.
upcCopySourceSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerKey =
    lens _upcCopySourceSSECustomerKey
         (\s a -> s { _upcCopySourceSSECustomerKey = a })

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcCopySourceSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerKeyMD5 =
    lens _upcCopySourceSSECustomerKeyMD5
         (\s a -> s { _upcCopySourceSSECustomerKeyMD5 = a })

instance ToPath UploadPartCopy

instance ToQuery UploadPartCopy

instance ToHeaders UploadPartCopy where
    toHeaders UploadPartCopy{..} = concat
        [ "x-amz-copy-source" =: _upcCopySource
        , "x-amz-copy-source-if-match" =: _upcCopySourceIfMatch
        , "x-amz-copy-source-if-modified-since" =: _upcCopySourceIfModifiedSince
        , "x-amz-copy-source-if-none-match" =: _upcCopySourceIfNoneMatch
        , "x-amz-copy-source-if-unmodified-since" =: _upcCopySourceIfUnmodifiedSince
        , "x-amz-copy-source-range" =: _upcCopySourceRange
        , "x-amz-server-side-encryption-customer-algorithm" =: _upcSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _upcSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _upcSSECustomerKeyMD5
        , "x-amz-copy-source-server-side-encryption-customer-algorithm" =: _upcCopySourceSSECustomerAlgorithm
        , "x-amz-copy-source-server-side-encryption-customer-key" =: _upcCopySourceSSECustomerKey
        , "x-amz-copy-source-server-side-encryption-customer-key-MD5" =: _upcCopySourceSSECustomerKeyMD5
        ]

instance ToBody UploadPartCopy

data UploadPartCopyResponse = UploadPartCopyResponse
    { _upcrCopySourceVersionId :: Maybe Text
    , _upcrCopyPartResult :: Maybe CopyPartResult
    , _upcrServerSideEncryption :: Maybe ServerSideEncryption
    , _upcrSSECustomerAlgorithm :: Maybe Text
    , _upcrSSECustomerKeyMD5 :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UploadPartCopyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CopySourceVersionId ::@ @Maybe Text@
--
-- * @CopyPartResult ::@ @Maybe CopyPartResult@
--
-- * @ServerSideEncryption ::@ @Maybe ServerSideEncryption@
--
-- * @SSECustomerAlgorithm ::@ @Maybe Text@
--
-- * @SSECustomerKeyMD5 ::@ @Maybe Text@
--
uploadPartCopyResponse :: UploadPartCopyResponse
uploadPartCopyResponse = UploadPartCopyResponse
    { _upcrCopySourceVersionId = Nothing
    , _upcrCopyPartResult = Nothing
    , _upcrServerSideEncryption = Nothing
    , _upcrSSECustomerAlgorithm = Nothing
    , _upcrSSECustomerKeyMD5 = Nothing
    }

-- | The version of the source object that was copied, if you have enabled
-- versioning on the source bucket.
upcrCopySourceVersionId :: Lens' UploadPartCopyResponse (Maybe Text)
upcrCopySourceVersionId =
    lens _upcrCopySourceVersionId
         (\s a -> s { _upcrCopySourceVersionId = a })

upcrCopyPartResult :: Lens' UploadPartCopyResponse (Maybe CopyPartResult)
upcrCopyPartResult =
    lens _upcrCopyPartResult (\s a -> s { _upcrCopyPartResult = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
upcrServerSideEncryption :: Lens' UploadPartCopyResponse (Maybe ServerSideEncryption)
upcrServerSideEncryption =
    lens _upcrServerSideEncryption
         (\s a -> s { _upcrServerSideEncryption = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
upcrSSECustomerAlgorithm :: Lens' UploadPartCopyResponse (Maybe Text)
upcrSSECustomerAlgorithm =
    lens _upcrSSECustomerAlgorithm
         (\s a -> s { _upcrSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
upcrSSECustomerKeyMD5 :: Lens' UploadPartCopyResponse (Maybe Text)
upcrSSECustomerKeyMD5 =
    lens _upcrSSECustomerKeyMD5 (\s a -> s { _upcrSSECustomerKeyMD5 = a })

instance AWSRequest UploadPartCopy where
    type Sv UploadPartCopy = S3
    type Rs UploadPartCopy = UploadPartCopyResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure UploadPartCopyResponse
            <*> hs ~:? "x-amz-copy-source-version-id"
            <*> xml %|? "CopyPartResult"
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
