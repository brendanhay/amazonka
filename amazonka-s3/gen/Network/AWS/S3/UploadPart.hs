{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.UploadPart
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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/UploadPart.html>
module Network.AWS.S3.UploadPart
    (
    -- * Request
      UploadPart
    -- ** Request constructor
    , uploadPart
    -- ** Request lenses
    , upBody
    , upBucket
    , upContentLength
    , upContentMD5
    , upKey
    , upPartNumber
    , upSSECustomerAlgorithm
    , upSSECustomerKey
    , upSSECustomerKeyMD5
    , upSSEKMSKeyId
    , upUploadId

    -- * Response
    , UploadPartResponse
    -- ** Response constructor
    , uploadPartResponse
    -- ** Response lenses
    , uprETag
    , uprSSECustomerAlgorithm
    , uprSSECustomerKeyMD5
    , uprSSEKMSKeyId
    , uprServerSideEncryption
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types
import qualified GHC.Exts

data UploadPart = UploadPart
    { _upBody                 :: Maybe Base64
    , _upBucket               :: Text
    , _upContentLength        :: Maybe Int
    , _upContentMD5           :: Maybe Text
    , _upKey                  :: Text
    , _upPartNumber           :: Int
    , _upSSECustomerAlgorithm :: Maybe Text
    , _upSSECustomerKey       :: Maybe (Sensitive Text)
    , _upSSECustomerKeyMD5    :: Maybe Text
    , _upSSEKMSKeyId          :: Maybe (Sensitive Text)
    , _upUploadId             :: Text
    } deriving (Eq, Show, Generic)

-- | 'UploadPart' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upBody' @::@ 'Maybe' 'Base64'
--
-- * 'upBucket' @::@ 'Text'
--
-- * 'upContentLength' @::@ 'Maybe' 'Int'
--
-- * 'upContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'upKey' @::@ 'Text'
--
-- * 'upPartNumber' @::@ 'Int'
--
-- * 'upSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'upSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'upSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'upSSEKMSKeyId' @::@ 'Maybe' 'Text'
--
-- * 'upUploadId' @::@ 'Text'
--
uploadPart :: Text -- ^ 'upBucket'
           -> Text -- ^ 'upKey'
           -> Int -- ^ 'upPartNumber'
           -> Text -- ^ 'upUploadId'
           -> UploadPart
uploadPart p1 p2 p3 p4 = UploadPart
    { _upBucket               = p1
    , _upKey                  = p2
    , _upPartNumber           = p3
    , _upUploadId             = p4
    , _upBody                 = Nothing
    , _upContentLength        = Nothing
    , _upContentMD5           = Nothing
    , _upSSECustomerAlgorithm = Nothing
    , _upSSECustomerKey       = Nothing
    , _upSSECustomerKeyMD5    = Nothing
    , _upSSEKMSKeyId          = Nothing
    }

upBody :: Lens' UploadPart (Maybe Base64)
upBody = lens _upBody (\s a -> s { _upBody = a })

upBucket :: Lens' UploadPart Text
upBucket = lens _upBucket (\s a -> s { _upBucket = a })

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
upContentLength :: Lens' UploadPart (Maybe Int)
upContentLength = lens _upContentLength (\s a -> s { _upContentLength = a })

upContentMD5 :: Lens' UploadPart (Maybe Text)
upContentMD5 = lens _upContentMD5 (\s a -> s { _upContentMD5 = a })

upKey :: Lens' UploadPart Text
upKey = lens _upKey (\s a -> s { _upKey = a })

-- | Part number of part being uploaded.
upPartNumber :: Lens' UploadPart Int
upPartNumber = lens _upPartNumber (\s a -> s { _upPartNumber = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256, aws:kms).
upSSECustomerAlgorithm :: Lens' UploadPart (Maybe Text)
upSSECustomerAlgorithm =
    lens _upSSECustomerAlgorithm (\s a -> s { _upSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
-- This must be the same encryption key specified in the initiate multipart
-- upload request.
upSSECustomerKey :: Lens' UploadPart (Maybe Text)
upSSECustomerKey = lens _upSSECustomerKey (\s a -> s { _upSSECustomerKey = a })
    . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upSSECustomerKeyMD5 :: Lens' UploadPart (Maybe Text)
upSSECustomerKeyMD5 =
    lens _upSSECustomerKeyMD5 (\s a -> s { _upSSECustomerKeyMD5 = a })

-- | Specifies the AWS KMS key ID to use for object encryption.
upSSEKMSKeyId :: Lens' UploadPart (Maybe Text)
upSSEKMSKeyId = lens _upSSEKMSKeyId (\s a -> s { _upSSEKMSKeyId = a })
    . mapping _Sensitive

-- | Upload ID identifying the multipart upload whose part is being uploaded.
upUploadId :: Lens' UploadPart Text
upUploadId = lens _upUploadId (\s a -> s { _upUploadId = a })

data UploadPartResponse = UploadPartResponse
    { _uprETag                 :: Maybe Text
    , _uprSSECustomerAlgorithm :: Maybe Text
    , _uprSSECustomerKeyMD5    :: Maybe Text
    , _uprSSEKMSKeyId          :: Maybe (Sensitive Text)
    , _uprServerSideEncryption :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UploadPartResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uprETag' @::@ 'Maybe' 'Text'
--
-- * 'uprSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'uprSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'uprSSEKMSKeyId' @::@ 'Maybe' 'Text'
--
-- * 'uprServerSideEncryption' @::@ 'Maybe' 'Text'
--
uploadPartResponse :: UploadPartResponse
uploadPartResponse = UploadPartResponse
    { _uprServerSideEncryption = Nothing
    , _uprETag                 = Nothing
    , _uprSSECustomerAlgorithm = Nothing
    , _uprSSECustomerKeyMD5    = Nothing
    , _uprSSEKMSKeyId          = Nothing
    }

-- | Entity tag for the uploaded object.
uprETag :: Lens' UploadPartResponse (Maybe Text)
uprETag = lens _uprETag (\s a -> s { _uprETag = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
uprSSECustomerAlgorithm :: Lens' UploadPartResponse (Maybe Text)
uprSSECustomerAlgorithm =
    lens _uprSSECustomerAlgorithm (\s a -> s { _uprSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
uprSSECustomerKeyMD5 :: Lens' UploadPartResponse (Maybe Text)
uprSSECustomerKeyMD5 =
    lens _uprSSECustomerKeyMD5 (\s a -> s { _uprSSECustomerKeyMD5 = a })

-- | If present, specifies the AWS KMS key used to encrypt the object.
uprSSEKMSKeyId :: Lens' UploadPartResponse (Maybe Text)
uprSSEKMSKeyId = lens _uprSSEKMSKeyId (\s a -> s { _uprSSEKMSKeyId = a })
    . mapping _Sensitive

-- | The Server-side encryption algorithm used when storing this object in S3
-- (e.g., AES256, aws:kms).
uprServerSideEncryption :: Lens' UploadPartResponse (Maybe Text)
uprServerSideEncryption =
    lens _uprServerSideEncryption (\s a -> s { _uprServerSideEncryption = a })

instance ToPath UploadPart where
    toPath UploadPart{..} = mconcat
        [ "/"
        , toText _upBucket
        , "/"
        , toText _upKey
        ]

instance ToQuery UploadPart where
    toQuery UploadPart{..} = mconcat
          [   "partNumber" =? _upPartNumber
          ,   "uploadId"   =? _upUploadId
        ]

instance ToHeaders UploadPart where
    toHeaders UploadPart{..} = mconcat
        [ "Content-Length"                                  =: _upContentLength
        , "Content-MD5"                                     =: _upContentMD5
        , "x-amz-server-side-encryption-customer-algorithm" =: _upSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"       =: _upSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"   =: _upSSECustomerKeyMD5
        , "x-amz-server-side-encryption-aws-kms-key-id"     =: _upSSEKMSKeyId
        ]

instance ToBody UploadPart where

instance AWSRequest UploadPart where
    type Sv UploadPart = S3
    type Rs UploadPart = UploadPartResponse

    request  = put
    response = xmlHeaderResponse $ \h x -> UploadPartResponse
        <$> h ~:? "ETag"
        <*> h ~:? "x-amz-server-side-encryption-customer-algorithm"
        <*> h ~:? "x-amz-server-side-encryption-customer-key-MD5"
        <*> h ~:? "x-amz-server-side-encryption-aws-kms-key-id"
        <*> h ~:? "x-amz-server-side-encryption"
