{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , upcrBucket
    , upcrCopySource
    , upcrCopySourceIfMatch
    , upcrCopySourceIfModifiedSince
    , upcrCopySourceIfNoneMatch
    , upcrCopySourceIfUnmodifiedSince
    , upcrCopySourceRange
    , upcrCopySourceSSECustomerAlgorithm
    , upcrCopySourceSSECustomerKey
    , upcrCopySourceSSECustomerKeyMD5
    , upcrKey
    , upcrPartNumber
    , upcrSSECustomerAlgorithm
    , upcrSSECustomerKey
    , upcrSSECustomerKeyMD5
    , upcrUploadId

    -- * Response
    , UploadPartCopyOutput
    -- ** Response constructor
    , uploadPartCopyOutput
    -- ** Response lenses
    , upcoCopyPartResult
    , upcoCopySourceVersionId
    , upcoSSECustomerAlgorithm
    , upcoSSECustomerKeyMD5
    , upcoServerSideEncryption
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types

data UploadPartCopy = UploadPartCopy
    { _upcrBucket                         :: Text
    , _upcrCopySource                     :: Text
    , _upcrCopySourceIfMatch              :: Maybe Text
    , _upcrCopySourceIfModifiedSince      :: Maybe RFC822
    , _upcrCopySourceIfNoneMatch          :: Maybe Text
    , _upcrCopySourceIfUnmodifiedSince    :: Maybe RFC822
    , _upcrCopySourceRange                :: Maybe Text
    , _upcrCopySourceSSECustomerAlgorithm :: Maybe Text
    , _upcrCopySourceSSECustomerKey       :: Maybe (Sensitive Text)
    , _upcrCopySourceSSECustomerKeyMD5    :: Maybe Text
    , _upcrKey                            :: Text
    , _upcrPartNumber                     :: Int
    , _upcrSSECustomerAlgorithm           :: Maybe Text
    , _upcrSSECustomerKey                 :: Maybe (Sensitive Text)
    , _upcrSSECustomerKeyMD5              :: Maybe Text
    , _upcrUploadId                       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UploadPartCopy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upcrBucket' @::@ 'Text'
--
-- * 'upcrCopySource' @::@ 'Text'
--
-- * 'upcrCopySourceIfMatch' @::@ 'Maybe' 'Text'
--
-- * 'upcrCopySourceIfModifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'upcrCopySourceIfNoneMatch' @::@ 'Maybe' 'Text'
--
-- * 'upcrCopySourceIfUnmodifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'upcrCopySourceRange' @::@ 'Maybe' 'Text'
--
-- * 'upcrCopySourceSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'upcrCopySourceSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'upcrCopySourceSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'upcrKey' @::@ 'Text'
--
-- * 'upcrPartNumber' @::@ 'Int'
--
-- * 'upcrSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'upcrSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'upcrSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'upcrUploadId' @::@ 'Text'
--
uploadPartCopy :: Text -- ^ 'upcrBucket'
               -> Text -- ^ 'upcrCopySource'
               -> Text -- ^ 'upcrKey'
               -> Int -- ^ 'upcrPartNumber'
               -> Text -- ^ 'upcrUploadId'
               -> UploadPartCopy
uploadPartCopy p1 p2 p3 p4 p5 = UploadPartCopy
    { _upcrBucket                         = p1
    , _upcrCopySource                     = p2
    , _upcrKey                            = p3
    , _upcrPartNumber                     = p4
    , _upcrUploadId                       = p5
    , _upcrCopySourceIfMatch              = Nothing
    , _upcrCopySourceIfModifiedSince      = Nothing
    , _upcrCopySourceIfNoneMatch          = Nothing
    , _upcrCopySourceIfUnmodifiedSince    = Nothing
    , _upcrCopySourceRange                = Nothing
    , _upcrSSECustomerAlgorithm           = Nothing
    , _upcrSSECustomerKey                 = Nothing
    , _upcrSSECustomerKeyMD5              = Nothing
    , _upcrCopySourceSSECustomerAlgorithm = Nothing
    , _upcrCopySourceSSECustomerKey       = Nothing
    , _upcrCopySourceSSECustomerKeyMD5    = Nothing
    }

upcrBucket :: Lens' UploadPartCopy Text
upcrBucket = lens _upcrBucket (\s a -> s { _upcrBucket = a })

-- | The name of the source bucket and key name of the source object,
-- separated by a slash (/). Must be URL-encoded.
upcrCopySource :: Lens' UploadPartCopy Text
upcrCopySource = lens _upcrCopySource (\s a -> s { _upcrCopySource = a })

-- | Copies the object if its entity tag (ETag) matches the specified tag.
upcrCopySourceIfMatch :: Lens' UploadPartCopy (Maybe Text)
upcrCopySourceIfMatch =
    lens _upcrCopySourceIfMatch (\s a -> s { _upcrCopySourceIfMatch = a })

-- | Copies the object if it has been modified since the specified time.
upcrCopySourceIfModifiedSince :: Lens' UploadPartCopy (Maybe UTCTime)
upcrCopySourceIfModifiedSince =
    lens _upcrCopySourceIfModifiedSince
        (\s a -> s { _upcrCopySourceIfModifiedSince = a })
            . mapping _Time

-- | Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
upcrCopySourceIfNoneMatch :: Lens' UploadPartCopy (Maybe Text)
upcrCopySourceIfNoneMatch =
    lens _upcrCopySourceIfNoneMatch
        (\s a -> s { _upcrCopySourceIfNoneMatch = a })

-- | Copies the object if it hasn't been modified since the specified time.
upcrCopySourceIfUnmodifiedSince :: Lens' UploadPartCopy (Maybe UTCTime)
upcrCopySourceIfUnmodifiedSince =
    lens _upcrCopySourceIfUnmodifiedSince
        (\s a -> s { _upcrCopySourceIfUnmodifiedSince = a })
            . mapping _Time

-- | The range of bytes to copy from the source object. The range value must
-- use the form bytes=first-last, where the first and last are the
-- zero-based byte offsets to copy. For example, bytes=0-9 indicates that
-- you want to copy the first ten bytes of the source. You can copy a range
-- only if the source object is greater than 5 GB.
upcrCopySourceRange :: Lens' UploadPartCopy (Maybe Text)
upcrCopySourceRange =
    lens _upcrCopySourceRange (\s a -> s { _upcrCopySourceRange = a })

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
upcrCopySourceSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcrCopySourceSSECustomerAlgorithm =
    lens _upcrCopySourceSSECustomerAlgorithm
        (\s a -> s { _upcrCopySourceSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
upcrCopySourceSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcrCopySourceSSECustomerKey =
    lens _upcrCopySourceSSECustomerKey
        (\s a -> s { _upcrCopySourceSSECustomerKey = a })
            . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcrCopySourceSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcrCopySourceSSECustomerKeyMD5 =
    lens _upcrCopySourceSSECustomerKeyMD5
        (\s a -> s { _upcrCopySourceSSECustomerKeyMD5 = a })

upcrKey :: Lens' UploadPartCopy Text
upcrKey = lens _upcrKey (\s a -> s { _upcrKey = a })

-- | Part number of part being copied.
upcrPartNumber :: Lens' UploadPartCopy Int
upcrPartNumber = lens _upcrPartNumber (\s a -> s { _upcrPartNumber = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
upcrSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcrSSECustomerAlgorithm =
    lens _upcrSSECustomerAlgorithm
        (\s a -> s { _upcrSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
-- This must be the same encryption key specified in the initiate multipart
-- upload request.
upcrSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcrSSECustomerKey =
    lens _upcrSSECustomerKey (\s a -> s { _upcrSSECustomerKey = a })
        . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcrSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcrSSECustomerKeyMD5 =
    lens _upcrSSECustomerKeyMD5 (\s a -> s { _upcrSSECustomerKeyMD5 = a })

-- | Upload ID identifying the multipart upload whose part is being copied.
upcrUploadId :: Lens' UploadPartCopy Text
upcrUploadId = lens _upcrUploadId (\s a -> s { _upcrUploadId = a })

instance ToPath UploadPartCopy where
    toPath UploadPartCopy{..} = mconcat
        [ "/"
        , toText _upcrBucket
        , "/"
        , toText _upcrKey
        ]

instance ToQuery UploadPartCopy where
    toQuery UploadPartCopy{..} = mconcat
        [ "partNumber" =? _upcrPartNumber
        , "uploadId"   =? _upcrUploadId
        ]

instance ToHeaders UploadPartCopy where
    toHeaders UploadPartCopy{..} = mconcat
        [ "x-amz-copy-source"                                           =: _upcrCopySource
        , "x-amz-copy-source-if-match"                                  =: _upcrCopySourceIfMatch
        , "x-amz-copy-source-if-modified-since"                         =: _upcrCopySourceIfModifiedSince
        , "x-amz-copy-source-if-none-match"                             =: _upcrCopySourceIfNoneMatch
        , "x-amz-copy-source-if-unmodified-since"                       =: _upcrCopySourceIfUnmodifiedSince
        , "x-amz-copy-source-range"                                     =: _upcrCopySourceRange
        , "x-amz-server-side-encryption-customer-algorithm"             =: _upcrSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"                   =: _upcrSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"               =: _upcrSSECustomerKeyMD5
        , "x-amz-copy-source-server-side-encryption-customer-algorithm" =: _upcrCopySourceSSECustomerAlgorithm
        , "x-amz-copy-source-server-side-encryption-customer-key"       =: _upcrCopySourceSSECustomerKey
        , "x-amz-copy-source-server-side-encryption-customer-key-MD5"   =: _upcrCopySourceSSECustomerKeyMD5
        ]

instance ToBody UploadPartCopy

data UploadPartCopyOutput = UploadPartCopyOutput
    { _upcoCopyPartResult       :: Maybe CopyPartResult
    , _upcoCopySourceVersionId  :: Maybe Text
    , _upcoSSECustomerAlgorithm :: Maybe Text
    , _upcoSSECustomerKeyMD5    :: Maybe Text
    , _upcoServerSideEncryption :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'UploadPartCopyOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upcoCopyPartResult' @::@ 'Maybe' 'CopyPartResult'
--
-- * 'upcoCopySourceVersionId' @::@ 'Maybe' 'Text'
--
-- * 'upcoSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'upcoSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'upcoServerSideEncryption' @::@ 'Maybe' 'Text'
--
uploadPartCopyOutput :: UploadPartCopyOutput
uploadPartCopyOutput = UploadPartCopyOutput
    { _upcoCopySourceVersionId  = Nothing
    , _upcoCopyPartResult       = Nothing
    , _upcoServerSideEncryption = Nothing
    , _upcoSSECustomerAlgorithm = Nothing
    , _upcoSSECustomerKeyMD5    = Nothing
    }

upcoCopyPartResult :: Lens' UploadPartCopyOutput (Maybe CopyPartResult)
upcoCopyPartResult =
    lens _upcoCopyPartResult (\s a -> s { _upcoCopyPartResult = a })

-- | The version of the source object that was copied, if you have enabled
-- versioning on the source bucket.
upcoCopySourceVersionId :: Lens' UploadPartCopyOutput (Maybe Text)
upcoCopySourceVersionId =
    lens _upcoCopySourceVersionId (\s a -> s { _upcoCopySourceVersionId = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
upcoSSECustomerAlgorithm :: Lens' UploadPartCopyOutput (Maybe Text)
upcoSSECustomerAlgorithm =
    lens _upcoSSECustomerAlgorithm
        (\s a -> s { _upcoSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
upcoSSECustomerKeyMD5 :: Lens' UploadPartCopyOutput (Maybe Text)
upcoSSECustomerKeyMD5 =
    lens _upcoSSECustomerKeyMD5 (\s a -> s { _upcoSSECustomerKeyMD5 = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
upcoServerSideEncryption :: Lens' UploadPartCopyOutput (Maybe Text)
upcoServerSideEncryption =
    lens _upcoServerSideEncryption
        (\s a -> s { _upcoServerSideEncryption = a })

instance AWSRequest UploadPartCopy where
    type Sv UploadPartCopy = S3
    type Rs UploadPartCopy = UploadPartCopyOutput

    request  = put'
    response = const . xmlResponse $ \h x -> UploadPartCopyOutput
        <$> x %| "CopyPartResult"
        <*> h ~:? "x-amz-copy-source-version-id"
        <*> h ~:? "x-amz-server-side-encryption-customer-algorithm"
        <*> h ~:? "x-amz-server-side-encryption-customer-key-MD5"
        <*> h ~:? "x-amz-server-side-encryption"
