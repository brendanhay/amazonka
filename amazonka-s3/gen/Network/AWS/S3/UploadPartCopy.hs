{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
    , upcCopySourceSSECustomerAlgorithm
    , upcCopySourceSSECustomerKey
    , upcCopySourceSSECustomerKeyMD5
    , upcKey
    , upcPartNumber
    , upcSSECustomerAlgorithm
    , upcSSECustomerKey
    , upcSSECustomerKeyMD5
    , upcUploadId

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
import Network.AWS.Request
import Network.AWS.S3.Types

data UploadPartCopy = UploadPartCopy
    { _upcBucket                         :: Text
    , _upcCopySource                     :: Text
    , _upcCopySourceIfMatch              :: Maybe Text
    , _upcCopySourceIfModifiedSince      :: Maybe RFC822
    , _upcCopySourceIfNoneMatch          :: Maybe Text
    , _upcCopySourceIfUnmodifiedSince    :: Maybe RFC822
    , _upcCopySourceRange                :: Maybe Text
    , _upcCopySourceSSECustomerAlgorithm :: Maybe Text
    , _upcCopySourceSSECustomerKey       :: Maybe (Sensitive Text)
    , _upcCopySourceSSECustomerKeyMD5    :: Maybe Text
    , _upcKey                            :: Text
    , _upcPartNumber                     :: Int
    , _upcSSECustomerAlgorithm           :: Maybe Text
    , _upcSSECustomerKey                 :: Maybe (Sensitive Text)
    , _upcSSECustomerKeyMD5              :: Maybe Text
    , _upcUploadId                       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UploadPartCopy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upcBucket' @::@ 'Text'
--
-- * 'upcCopySource' @::@ 'Text'
--
-- * 'upcCopySourceIfMatch' @::@ 'Maybe' 'Text'
--
-- * 'upcCopySourceIfModifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'upcCopySourceIfNoneMatch' @::@ 'Maybe' 'Text'
--
-- * 'upcCopySourceIfUnmodifiedSince' @::@ 'Maybe' 'UTCTime'
--
-- * 'upcCopySourceRange' @::@ 'Maybe' 'Text'
--
-- * 'upcCopySourceSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'upcCopySourceSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'upcCopySourceSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'upcKey' @::@ 'Text'
--
-- * 'upcPartNumber' @::@ 'Int'
--
-- * 'upcSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'upcSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'upcSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'upcUploadId' @::@ 'Text'
--
uploadPartCopy :: Text -- ^ 'upcBucket'
               -> Text -- ^ 'upcCopySource'
               -> Text -- ^ 'upcKey'
               -> Int -- ^ 'upcPartNumber'
               -> Text -- ^ 'upcUploadId'
               -> UploadPartCopy
uploadPartCopy p1 p2 p3 p4 p5 = UploadPartCopy
    { _upcBucket                         = p1
    , _upcCopySource                     = p2
    , _upcKey                            = p3
    , _upcPartNumber                     = p4
    , _upcUploadId                       = p5
    , _upcCopySourceIfMatch              = Nothing
    , _upcCopySourceIfModifiedSince      = Nothing
    , _upcCopySourceIfNoneMatch          = Nothing
    , _upcCopySourceIfUnmodifiedSince    = Nothing
    , _upcCopySourceRange                = Nothing
    , _upcSSECustomerAlgorithm           = Nothing
    , _upcSSECustomerKey                 = Nothing
    , _upcSSECustomerKeyMD5              = Nothing
    , _upcCopySourceSSECustomerAlgorithm = Nothing
    , _upcCopySourceSSECustomerKey       = Nothing
    , _upcCopySourceSSECustomerKeyMD5    = Nothing
    }

upcBucket :: Lens' UploadPartCopy Text
upcBucket = lens _upcBucket (\s a -> s { _upcBucket = a })

-- | The name of the source bucket and key name of the source object,
-- separated by a slash (/). Must be URL-encoded.
upcCopySource :: Lens' UploadPartCopy Text
upcCopySource = lens _upcCopySource (\s a -> s { _upcCopySource = a })

-- | Copies the object if its entity tag (ETag) matches the specified tag.
upcCopySourceIfMatch :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceIfMatch =
    lens _upcCopySourceIfMatch (\s a -> s { _upcCopySourceIfMatch = a })

-- | Copies the object if it has been modified since the specified time.
upcCopySourceIfModifiedSince :: Lens' UploadPartCopy (Maybe UTCTime)
upcCopySourceIfModifiedSince =
    lens _upcCopySourceIfModifiedSince
        (\s a -> s { _upcCopySourceIfModifiedSince = a })
            . mapping _Time

-- | Copies the object if its entity tag (ETag) is different than the
-- specified ETag.
upcCopySourceIfNoneMatch :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceIfNoneMatch =
    lens _upcCopySourceIfNoneMatch
        (\s a -> s { _upcCopySourceIfNoneMatch = a })

-- | Copies the object if it hasn't been modified since the specified time.
upcCopySourceIfUnmodifiedSince :: Lens' UploadPartCopy (Maybe UTCTime)
upcCopySourceIfUnmodifiedSince =
    lens _upcCopySourceIfUnmodifiedSince
        (\s a -> s { _upcCopySourceIfUnmodifiedSince = a })
            . mapping _Time

-- | The range of bytes to copy from the source object. The range value must
-- use the form bytes=first-last, where the first and last are the
-- zero-based byte offsets to copy. For example, bytes=0-9 indicates that
-- you want to copy the first ten bytes of the source. You can copy a range
-- only if the source object is greater than 5 GB.
upcCopySourceRange :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceRange =
    lens _upcCopySourceRange (\s a -> s { _upcCopySourceRange = a })

-- | Specifies the algorithm to use when decrypting the source object (e.g.,
-- AES256).
upcCopySourceSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerAlgorithm =
    lens _upcCopySourceSSECustomerAlgorithm
        (\s a -> s { _upcCopySourceSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use to
-- decrypt the source object. The encryption key provided in this header
-- must be one that was used when the source object was created.
upcCopySourceSSECustomerKey :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerKey =
    lens _upcCopySourceSSECustomerKey
        (\s a -> s { _upcCopySourceSSECustomerKey = a })
            . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcCopySourceSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSECustomerKeyMD5 =
    lens _upcCopySourceSSECustomerKeyMD5
        (\s a -> s { _upcCopySourceSSECustomerKeyMD5 = a })

upcKey :: Lens' UploadPartCopy Text
upcKey = lens _upcKey (\s a -> s { _upcKey = a })

-- | Part number of part being copied.
upcPartNumber :: Lens' UploadPartCopy Int
upcPartNumber = lens _upcPartNumber (\s a -> s { _upcPartNumber = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
upcSSECustomerAlgorithm :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerAlgorithm =
    lens _upcSSECustomerAlgorithm (\s a -> s { _upcSSECustomerAlgorithm = a })

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
        . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upcSSECustomerKeyMD5 :: Lens' UploadPartCopy (Maybe Text)
upcSSECustomerKeyMD5 =
    lens _upcSSECustomerKeyMD5 (\s a -> s { _upcSSECustomerKeyMD5 = a })

-- | Upload ID identifying the multipart upload whose part is being copied.
upcUploadId :: Lens' UploadPartCopy Text
upcUploadId = lens _upcUploadId (\s a -> s { _upcUploadId = a })

instance ToPath UploadPartCopy where
    toPath UploadPartCopy{..} = mconcat
        [ "/"
        , toText _upcBucket
        , "/"
        , toText _upcKey
        ]

instance ToQuery UploadPartCopy where
    toQuery UploadPartCopy{..} = mconcat
        [ "partNumber" =? _upcPartNumber
        , "uploadId"   =? _upcUploadId
        ]

instance ToHeaders UploadPartCopy where
    toHeaders UploadPartCopy{..} = mconcat
        [ "x-amz-copy-source"                                           =: _upcCopySource
        , "x-amz-copy-source-if-match"                                  =: _upcCopySourceIfMatch
        , "x-amz-copy-source-if-modified-since"                         =: _upcCopySourceIfModifiedSince
        , "x-amz-copy-source-if-none-match"                             =: _upcCopySourceIfNoneMatch
        , "x-amz-copy-source-if-unmodified-since"                       =: _upcCopySourceIfUnmodifiedSince
        , "x-amz-copy-source-range"                                     =: _upcCopySourceRange
        , "x-amz-server-side-encryption-customer-algorithm"             =: _upcSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"                   =: _upcSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"               =: _upcSSECustomerKeyMD5
        , "x-amz-copy-source-server-side-encryption-customer-algorithm" =: _upcCopySourceSSECustomerAlgorithm
        , "x-amz-copy-source-server-side-encryption-customer-key"       =: _upcCopySourceSSECustomerKey
        , "x-amz-copy-source-server-side-encryption-customer-key-MD5"   =: _upcCopySourceSSECustomerKeyMD5
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

    request  = put
    response = const . xmlResponse $ \h x -> UploadPartCopyOutput
        <$> x %| "CopyPartResult"
        <*> h ~:? "x-amz-copy-source-version-id"
        <*> h ~:? "x-amz-server-side-encryption-customer-algorithm"
        <*> h ~:? "x-amz-server-side-encryption-customer-key-MD5"
        <*> h ~:? "x-amz-server-side-encryption"
