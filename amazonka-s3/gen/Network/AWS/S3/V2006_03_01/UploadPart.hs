{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.S3.V2006_03_01.UploadPart
    (
    -- * Request
      UploadPart
    -- ** Request constructor
    , uploadPart
    -- ** Request lenses
    , uprBucket
    , uprUploadId
    , uprKey
    , uprPartNumber
    , uprBody
    , uprContentLength
    , uprContentMD5
    , uprSSECustomerAlgorithm
    , uprSSECustomerKey
    , uprSSECustomerKeyMD5

    -- * Response
    , UploadPartResponse
    -- ** Response lenses
    , upoETag
    , upoSSECustomerAlgorithm
    , upoSSECustomerKeyMD5
    , upoServerSideEncryption
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UploadPart' request.
uploadPart :: BucketName -- ^ 'uprBucket'
           -> Text -- ^ 'uprUploadId'
           -> ObjectKey -- ^ 'uprKey'
           -> Integer -- ^ 'uprPartNumber'
           -> RqBody -- ^ 'uprBody'
           -> UploadPart
uploadPart p1 p2 p3 p4 p5 = UploadPart
    { _uprBucket = p1
    , _uprUploadId = p2
    , _uprKey = p3
    , _uprPartNumber = p4
    , _uprBody = p5
    , _uprContentLength = Nothing
    , _uprContentMD5 = Nothing
    , _uprSSECustomerAlgorithm = Nothing
    , _uprSSECustomerKey = Nothing
    , _uprSSECustomerKeyMD5 = Nothing
    }
{-# INLINE uploadPart #-}

data UploadPart = UploadPart
    { _uprBucket :: BucketName
    , _uprUploadId :: Text
      -- ^ Upload ID identifying the multipart upload whose part is being
      -- uploaded.
    , _uprKey :: ObjectKey
    , _uprPartNumber :: Integer
      -- ^ Part number of part being uploaded.
    , _uprBody :: RqBody
    , _uprContentLength :: Maybe Integer
      -- ^ Size of the body in bytes. This parameter is useful when the size
      -- of the body cannot be determined automatically.
    , _uprContentMD5 :: Maybe Text
    , _uprSSECustomerAlgorithm :: Maybe Text
      -- ^ Specifies the algorithm to use to when encrypting the object
      -- (e.g., AES256).
    , _uprSSECustomerKey :: Maybe Text
      -- ^ Specifies the customer-provided encryption key for Amazon S3 to
      -- use in encrypting data. This value is used to store the object
      -- and then it is discarded; Amazon does not store the encryption
      -- key. The key must be appropriate for use with the algorithm
      -- specified in the
      -- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm
      -- header. This must be the same encryption key specified in the
      -- initiate multipart upload request.
    , _uprSSECustomerKeyMD5 :: Maybe Text
      -- ^ Specifies the 128-bit MD5 digest of the encryption key according
      -- to RFC 1321. Amazon S3 uses this header for a message integrity
      -- check to ensure the encryption key was transmitted without error.
    } deriving (Show, Generic)

uprBucket :: Lens' UploadPart (BucketName)
uprBucket f x =
    f (_uprBucket x)
        <&> \y -> x { _uprBucket = y }
{-# INLINE uprBucket #-}

-- | Upload ID identifying the multipart upload whose part is being uploaded.
uprUploadId :: Lens' UploadPart (Text)
uprUploadId f x =
    f (_uprUploadId x)
        <&> \y -> x { _uprUploadId = y }
{-# INLINE uprUploadId #-}

uprKey :: Lens' UploadPart (ObjectKey)
uprKey f x =
    f (_uprKey x)
        <&> \y -> x { _uprKey = y }
{-# INLINE uprKey #-}

-- | Part number of part being uploaded.
uprPartNumber :: Lens' UploadPart (Integer)
uprPartNumber f x =
    f (_uprPartNumber x)
        <&> \y -> x { _uprPartNumber = y }
{-# INLINE uprPartNumber #-}

uprBody :: Lens' UploadPart (RqBody)
uprBody f x =
    f (_uprBody x)
        <&> \y -> x { _uprBody = y }
{-# INLINE uprBody #-}

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
uprContentLength :: Lens' UploadPart (Maybe Integer)
uprContentLength f x =
    f (_uprContentLength x)
        <&> \y -> x { _uprContentLength = y }
{-# INLINE uprContentLength #-}

uprContentMD5 :: Lens' UploadPart (Maybe Text)
uprContentMD5 f x =
    f (_uprContentMD5 x)
        <&> \y -> x { _uprContentMD5 = y }
{-# INLINE uprContentMD5 #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
uprSSECustomerAlgorithm :: Lens' UploadPart (Maybe Text)
uprSSECustomerAlgorithm f x =
    f (_uprSSECustomerAlgorithm x)
        <&> \y -> x { _uprSSECustomerAlgorithm = y }
{-# INLINE uprSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
-- This must be the same encryption key specified in the initiate multipart
-- upload request.
uprSSECustomerKey :: Lens' UploadPart (Maybe Text)
uprSSECustomerKey f x =
    f (_uprSSECustomerKey x)
        <&> \y -> x { _uprSSECustomerKey = y }
{-# INLINE uprSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
uprSSECustomerKeyMD5 :: Lens' UploadPart (Maybe Text)
uprSSECustomerKeyMD5 f x =
    f (_uprSSECustomerKeyMD5 x)
        <&> \y -> x { _uprSSECustomerKeyMD5 = y }
{-# INLINE uprSSECustomerKeyMD5 #-}

instance ToPath UploadPart where
    toPath UploadPart{..} = mconcat
        [ "/"
        , toBS _uprBucket
        , "/"
        , toBS _uprKey
        ]

instance ToQuery UploadPart where
    toQuery UploadPart{..} = mconcat
        [ "partNumber" =? _uprPartNumber
        , "uploadId" =? _uprUploadId
        ]

instance ToHeaders UploadPart where
    toHeaders UploadPart{..} = concat
        [ "Content-Length" =: _uprContentLength
        , "Content-MD5" =: _uprContentMD5
        , "x-amz-server-side-encryption-customer-algorithm" =: _uprSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key" =: _uprSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5" =: _uprSSECustomerKeyMD5
        ]

instance ToBody UploadPart where
    toBody = toBody . _uprBody

data UploadPartResponse = UploadPartResponse
    { _upoETag :: Maybe ETag
      -- ^ Entity tag for the uploaded object.
    , _upoSSECustomerAlgorithm :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header confirming
      -- the encryption algorithm used.
    , _upoSSECustomerKeyMD5 :: Maybe Text
      -- ^ If server-side encryption with a customer-provided encryption key
      -- was requested, the response will include this header to provide
      -- round trip message integrity verification of the
      -- customer-provided encryption key.
    , _upoServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Show, Generic)

-- | Entity tag for the uploaded object.
upoETag :: Lens' UploadPartResponse (Maybe ETag)
upoETag f x =
    f (_upoETag x)
        <&> \y -> x { _upoETag = y }
{-# INLINE upoETag #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
upoSSECustomerAlgorithm :: Lens' UploadPartResponse (Maybe Text)
upoSSECustomerAlgorithm f x =
    f (_upoSSECustomerAlgorithm x)
        <&> \y -> x { _upoSSECustomerAlgorithm = y }
{-# INLINE upoSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
upoSSECustomerKeyMD5 :: Lens' UploadPartResponse (Maybe Text)
upoSSECustomerKeyMD5 f x =
    f (_upoSSECustomerKeyMD5 x)
        <&> \y -> x { _upoSSECustomerKeyMD5 = y }
{-# INLINE upoSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
upoServerSideEncryption :: Lens' UploadPartResponse (Maybe ServerSideEncryption)
upoServerSideEncryption f x =
    f (_upoServerSideEncryption x)
        <&> \y -> x { _upoServerSideEncryption = y }
{-# INLINE upoServerSideEncryption #-}

instance AWSRequest UploadPart where
    type Sv UploadPart = S3
    type Rs UploadPart = UploadPartResponse

    request = put
    response _ = headerResponse $ \hs ->
        pure UploadPartResponse
            <*> hs ~:? "ETag"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
            <*> hs ~:? "x-amz-server-side-encryption"
