{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/UploadPartCopy.html>
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
    , upcCopySourceSSEKMSKeyId
    , upcKey
    , upcPartNumber
    , upcSSECustomerAlgorithm
    , upcSSECustomerKey
    , upcSSECustomerKeyMD5
    , upcUploadId

    -- * Response
    , UploadPartCopyResponse
    -- ** Response constructor
    , uploadPartCopyResponse
    -- ** Response lenses
    , upcrCopyPartResult
    , upcrCopySourceVersionId
    , upcrSSECustomerAlgorithm
    , upcrSSECustomerKeyMD5
    , upcrSSEKMSKeyId
    , upcrServerSideEncryption
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

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
    , _upcCopySourceSSEKMSKeyId          :: Maybe (Sensitive Text)
    , _upcKey                            :: Text
    , _upcPartNumber                     :: Int
    , _upcSSECustomerAlgorithm           :: Maybe Text
    , _upcSSECustomerKey                 :: Maybe (Sensitive Text)
    , _upcSSECustomerKeyMD5              :: Maybe Text
    , _upcUploadId                       :: Text
    } deriving (Eq, Ord, Show)

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
-- * 'upcCopySourceSSEKMSKeyId' @::@ 'Maybe' 'Text'
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
    , _upcCopySourceSSEKMSKeyId          = Nothing
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

-- | Specifies the AWS KMS key ID to use for object encryption.
upcCopySourceSSEKMSKeyId :: Lens' UploadPartCopy (Maybe Text)
upcCopySourceSSEKMSKeyId =
    lens _upcCopySourceSSEKMSKeyId
        (\s a -> s { _upcCopySourceSSEKMSKeyId = a })
            . mapping _Sensitive

upcKey :: Lens' UploadPartCopy Text
upcKey = lens _upcKey (\s a -> s { _upcKey = a })

-- | Part number of part being copied.
upcPartNumber :: Lens' UploadPartCopy Int
upcPartNumber = lens _upcPartNumber (\s a -> s { _upcPartNumber = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
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

data UploadPartCopyResponse = UploadPartCopyResponse
    { _upcrCopyPartResult       :: Maybe CopyPartResult
    , _upcrCopySourceVersionId  :: Maybe Text
    , _upcrSSECustomerAlgorithm :: Maybe Text
    , _upcrSSECustomerKeyMD5    :: Maybe Text
    , _upcrSSEKMSKeyId          :: Maybe (Sensitive Text)
    , _upcrServerSideEncryption :: Maybe Text
    } deriving (Eq, Show)

-- | 'UploadPartCopyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upcrCopyPartResult' @::@ 'Maybe' 'CopyPartResult'
--
-- * 'upcrCopySourceVersionId' @::@ 'Maybe' 'Text'
--
-- * 'upcrSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'upcrSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'upcrSSEKMSKeyId' @::@ 'Maybe' 'Text'
--
-- * 'upcrServerSideEncryption' @::@ 'Maybe' 'Text'
--
uploadPartCopyResponse :: UploadPartCopyResponse
uploadPartCopyResponse = UploadPartCopyResponse
    { _upcrCopySourceVersionId  = Nothing
    , _upcrCopyPartResult       = Nothing
    , _upcrServerSideEncryption = Nothing
    , _upcrSSECustomerAlgorithm = Nothing
    , _upcrSSECustomerKeyMD5    = Nothing
    , _upcrSSEKMSKeyId          = Nothing
    }

upcrCopyPartResult :: Lens' UploadPartCopyResponse (Maybe CopyPartResult)
upcrCopyPartResult =
    lens _upcrCopyPartResult (\s a -> s { _upcrCopyPartResult = a })

-- | The version of the source object that was copied, if you have enabled
-- versioning on the source bucket.
upcrCopySourceVersionId :: Lens' UploadPartCopyResponse (Maybe Text)
upcrCopySourceVersionId =
    lens _upcrCopySourceVersionId (\s a -> s { _upcrCopySourceVersionId = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
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

-- | If present, specifies the AWS KMS key used to encrypt the object.
upcrSSEKMSKeyId :: Lens' UploadPartCopyResponse (Maybe Text)
upcrSSEKMSKeyId = lens _upcrSSEKMSKeyId (\s a -> s { _upcrSSEKMSKeyId = a }) . mapping _Sensitive

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
upcrServerSideEncryption :: Lens' UploadPartCopyResponse (Maybe Text)
upcrServerSideEncryption =
    lens _upcrServerSideEncryption
        (\s a -> s { _upcrServerSideEncryption = a })

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
        , "x-amz-server-side-encryption-aws-kms-key-id"                 =: _upcCopySourceSSEKMSKeyId
        ]

instance ToXMLRoot UploadPartCopy where
    toXMLRoot = const (namespaced ns "UploadPartCopy" [])

instance ToXML UploadPartCopy

instance AWSRequest UploadPartCopy where
    type Sv UploadPartCopy = S3
    type Rs UploadPartCopy = UploadPartCopyResponse

    request  = put
    response = xmlHeaderResponse $ \h x -> UploadPartCopyResponse
        <$> x .@? "CopyPartResult"
        <*> h ~:? "x-amz-copy-source-version-id"
        <*> h ~:? "x-amz-server-side-encryption-customer-algorithm"
        <*> h ~:? "x-amz-server-side-encryption-customer-key-MD5"
        <*> h ~:? "x-amz-server-side-encryption-aws-kms-key-id"
        <*> h ~:? "x-amz-server-side-encryption"
