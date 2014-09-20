{-# LANGUAGE DeriveGeneric               #-}
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
    , upUploadId
    , upSSECustomerAlgorithm
    , upSSECustomerKey
    , upSSECustomerKeyMD5

    -- * Response
    , UploadPartResponse
    -- ** Response constructor
    , uploadPartResponse
    -- ** Response lenses
    , uprServerSideEncryption
    , uprETag
    , uprSSECustomerAlgorithm
    , uprSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data UploadPart = UploadPart
    { _upBody :: RqBody
    , _upBucket :: BucketName
    , _upContentLength :: Maybe Integer
    , _upContentMD5 :: Maybe Text
    , _upKey :: ObjectKey
    , _upPartNumber :: !Integer
    , _upUploadId :: Text
    , _upSSECustomerAlgorithm :: Maybe Text
    , _upSSECustomerKey :: Maybe Text
    , _upSSECustomerKeyMD5 :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UploadPart' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Body ::@ @RqBody@
--
-- * @Bucket ::@ @BucketName@
--
-- * @ContentLength ::@ @Maybe Integer@
--
-- * @ContentMD5 ::@ @Maybe Text@
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
uploadPart :: RqBody -- ^ 'upBody'
           -> BucketName -- ^ 'upBucket'
           -> ObjectKey -- ^ 'upKey'
           -> Integer -- ^ 'upPartNumber'
           -> Text -- ^ 'upUploadId'
           -> UploadPart
uploadPart p1 p2 p5 p6 p7 = UploadPart
    { _upBody = p1
    , _upBucket = p2
    , _upContentLength = Nothing
    , _upContentMD5 = Nothing
    , _upKey = p5
    , _upPartNumber = p6
    , _upUploadId = p7
    , _upSSECustomerAlgorithm = Nothing
    , _upSSECustomerKey = Nothing
    , _upSSECustomerKeyMD5 = Nothing
    }

upBody :: Lens' UploadPart RqBody
upBody = lens _upBody (\s a -> s { _upBody = a })

upBucket :: Lens' UploadPart BucketName
upBucket = lens _upBucket (\s a -> s { _upBucket = a })

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
upContentLength :: Lens' UploadPart (Maybe Integer)
upContentLength = lens _upContentLength (\s a -> s { _upContentLength = a })

upContentMD5 :: Lens' UploadPart (Maybe Text)
upContentMD5 = lens _upContentMD5 (\s a -> s { _upContentMD5 = a })

upKey :: Lens' UploadPart ObjectKey
upKey = lens _upKey (\s a -> s { _upKey = a })

-- | Part number of part being uploaded.
upPartNumber :: Lens' UploadPart Integer
upPartNumber = lens _upPartNumber (\s a -> s { _upPartNumber = a })

-- | Upload ID identifying the multipart upload whose part is being uploaded.
upUploadId :: Lens' UploadPart Text
upUploadId = lens _upUploadId (\s a -> s { _upUploadId = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
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
upSSECustomerKey =
    lens _upSSECustomerKey (\s a -> s { _upSSECustomerKey = a })

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upSSECustomerKeyMD5 :: Lens' UploadPart (Maybe Text)
upSSECustomerKeyMD5 =
    lens _upSSECustomerKeyMD5 (\s a -> s { _upSSECustomerKeyMD5 = a })

instance ToPath UploadPart

instance ToQuery UploadPart

instance ToHeaders UploadPart where
    toHeaders UploadPart{..} = concat
        [ "Content-Length" =: _upContentLength
        , "Content-MD5" =: _upContentMD5
        , "x-amz-server-side-encryption-customer-algorithm" =: _upSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _upSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _upSSECustomerKeyMD5
        ]

instance ToBody UploadPart where
    toBody = toBody . _upBody

data UploadPartResponse = UploadPartResponse
    { _uprServerSideEncryption :: Maybe ServerSideEncryption
    , _uprETag :: Maybe ETag
    , _uprSSECustomerAlgorithm :: Maybe Text
    , _uprSSECustomerKeyMD5 :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UploadPartResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ServerSideEncryption ::@ @Maybe ServerSideEncryption@
--
-- * @ETag ::@ @Maybe ETag@
--
-- * @SSECustomerAlgorithm ::@ @Maybe Text@
--
-- * @SSECustomerKeyMD5 ::@ @Maybe Text@
--
uploadPartResponse :: UploadPartResponse
uploadPartResponse = UploadPartResponse
    { _uprServerSideEncryption = Nothing
    , _uprETag = Nothing
    , _uprSSECustomerAlgorithm = Nothing
    , _uprSSECustomerKeyMD5 = Nothing
    }

-- | The Server-side encryption algorithm used when storing this object in S3.
uprServerSideEncryption :: Lens' UploadPartResponse (Maybe ServerSideEncryption)
uprServerSideEncryption =
    lens _uprServerSideEncryption
         (\s a -> s { _uprServerSideEncryption = a })

-- | Entity tag for the uploaded object.
uprETag :: Lens' UploadPartResponse (Maybe ETag)
uprETag = lens _uprETag (\s a -> s { _uprETag = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
uprSSECustomerAlgorithm :: Lens' UploadPartResponse (Maybe Text)
uprSSECustomerAlgorithm =
    lens _uprSSECustomerAlgorithm
         (\s a -> s { _uprSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
uprSSECustomerKeyMD5 :: Lens' UploadPartResponse (Maybe Text)
uprSSECustomerKeyMD5 =
    lens _uprSSECustomerKeyMD5 (\s a -> s { _uprSSECustomerKeyMD5 = a })

instance AWSRequest UploadPart where
    type Sv UploadPart = S3
    type Rs UploadPart = UploadPartResponse

    request = get
    response _ = headerResponse $ \hs ->
        pure UploadPartResponse
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> hs ~:? "ETag"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
