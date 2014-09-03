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

uprBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> UploadPart
    -> f UploadPart
uprBucket f x =
    (\y -> x { _uprBucket = y })
       <$> f (_uprBucket x)
{-# INLINE uprBucket #-}

-- | Upload ID identifying the multipart upload whose part is being uploaded.
uprUploadId
    :: Functor f
    => (Text
    -> f (Text))
    -> UploadPart
    -> f UploadPart
uprUploadId f x =
    (\y -> x { _uprUploadId = y })
       <$> f (_uprUploadId x)
{-# INLINE uprUploadId #-}

uprKey
    :: Functor f
    => (ObjectKey
    -> f (ObjectKey))
    -> UploadPart
    -> f UploadPart
uprKey f x =
    (\y -> x { _uprKey = y })
       <$> f (_uprKey x)
{-# INLINE uprKey #-}

-- | Part number of part being uploaded.
uprPartNumber
    :: Functor f
    => (Integer
    -> f (Integer))
    -> UploadPart
    -> f UploadPart
uprPartNumber f x =
    (\y -> x { _uprPartNumber = y })
       <$> f (_uprPartNumber x)
{-# INLINE uprPartNumber #-}

uprBody
    :: Functor f
    => (RqBody
    -> f (RqBody))
    -> UploadPart
    -> f UploadPart
uprBody f x =
    (\y -> x { _uprBody = y })
       <$> f (_uprBody x)
{-# INLINE uprBody #-}

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
uprContentLength
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> UploadPart
    -> f UploadPart
uprContentLength f x =
    (\y -> x { _uprContentLength = y })
       <$> f (_uprContentLength x)
{-# INLINE uprContentLength #-}

uprContentMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPart
    -> f UploadPart
uprContentMD5 f x =
    (\y -> x { _uprContentMD5 = y })
       <$> f (_uprContentMD5 x)
{-# INLINE uprContentMD5 #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
uprSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPart
    -> f UploadPart
uprSSECustomerAlgorithm f x =
    (\y -> x { _uprSSECustomerAlgorithm = y })
       <$> f (_uprSSECustomerAlgorithm x)
{-# INLINE uprSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
-- This must be the same encryption key specified in the initiate multipart
-- upload request.
uprSSECustomerKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPart
    -> f UploadPart
uprSSECustomerKey f x =
    (\y -> x { _uprSSECustomerKey = y })
       <$> f (_uprSSECustomerKey x)
{-# INLINE uprSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
uprSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPart
    -> f UploadPart
uprSSECustomerKeyMD5 f x =
    (\y -> x { _uprSSECustomerKeyMD5 = y })
       <$> f (_uprSSECustomerKeyMD5 x)
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
upoETag
    :: Functor f
    => (Maybe ETag
    -> f (Maybe ETag))
    -> UploadPartResponse
    -> f UploadPartResponse
upoETag f x =
    (\y -> x { _upoETag = y })
       <$> f (_upoETag x)
{-# INLINE upoETag #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
upoSSECustomerAlgorithm
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartResponse
    -> f UploadPartResponse
upoSSECustomerAlgorithm f x =
    (\y -> x { _upoSSECustomerAlgorithm = y })
       <$> f (_upoSSECustomerAlgorithm x)
{-# INLINE upoSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
upoSSECustomerKeyMD5
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UploadPartResponse
    -> f UploadPartResponse
upoSSECustomerKeyMD5 f x =
    (\y -> x { _upoSSECustomerKeyMD5 = y })
       <$> f (_upoSSECustomerKeyMD5 x)
{-# INLINE upoSSECustomerKeyMD5 #-}

-- | The Server-side encryption algorithm used when storing this object in S3.
upoServerSideEncryption
    :: Functor f
    => (Maybe ServerSideEncryption
    -> f (Maybe ServerSideEncryption))
    -> UploadPartResponse
    -> f UploadPartResponse
upoServerSideEncryption f x =
    (\y -> x { _upoServerSideEncryption = y })
       <$> f (_upoServerSideEncryption x)
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
