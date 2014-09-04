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
    , mkUploadPartRequest
    -- ** Request lenses
    , uprBody
    , uprBucket
    , uprContentLength
    , uprContentMD5
    , uprKey
    , uprPartNumber
    , uprUploadId
    , uprSSECustomerAlgorithm
    , uprSSECustomerKey
    , uprSSECustomerKeyMD5

    -- * Response
    , UploadPartResponse
    -- ** Response lenses
    , upoServerSideEncryption
    , upoETag
    , upoSSECustomerAlgorithm
    , upoSSECustomerKeyMD5
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UploadPart' request.
mkUploadPartRequest :: RqBody -- ^ 'uprBody'
                    -> BucketName -- ^ 'uprBucket'
                    -> ObjectKey -- ^ 'uprKey'
                    -> Integer -- ^ 'uprPartNumber'
                    -> Text -- ^ 'uprUploadId'
                    -> UploadPart
mkUploadPartRequest p1 p2 p3 p4 p5 = UploadPart
    { _uprBody = p1
    , _uprBucket = p2
    , _uprContentLength = Nothing
    , _uprContentMD5 = Nothing
    , _uprKey = p5
    , _uprPartNumber = p6
    , _uprUploadId = p7
    , _uprSSECustomerAlgorithm = Nothing
    , _uprSSECustomerKey = Nothing
    , _uprSSECustomerKeyMD5 = Nothing
    }
{-# INLINE mkUploadPartRequest #-}

data UploadPart = UploadPart
    { _uprBody :: RqBody
    , _uprBucket :: BucketName
    , _uprContentLength :: Maybe Integer
      -- ^ Size of the body in bytes. This parameter is useful when the size
      -- of the body cannot be determined automatically.
    , _uprContentMD5 :: Maybe Text
    , _uprKey :: ObjectKey
    , _uprPartNumber :: Integer
      -- ^ Part number of part being uploaded.
    , _uprUploadId :: Text
      -- ^ Upload ID identifying the multipart upload whose part is being
      -- uploaded.
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

uprBody :: Lens' UploadPart (RqBody)
uprBody = lens _uprBody (\s a -> s { _uprBody = a })
{-# INLINE uprBody #-}

uprBucket :: Lens' UploadPart (BucketName)
uprBucket = lens _uprBucket (\s a -> s { _uprBucket = a })
{-# INLINE uprBucket #-}

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
uprContentLength :: Lens' UploadPart (Maybe Integer)
uprContentLength = lens _uprContentLength (\s a -> s { _uprContentLength = a })
{-# INLINE uprContentLength #-}

uprContentMD5 :: Lens' UploadPart (Maybe Text)
uprContentMD5 = lens _uprContentMD5 (\s a -> s { _uprContentMD5 = a })
{-# INLINE uprContentMD5 #-}

uprKey :: Lens' UploadPart (ObjectKey)
uprKey = lens _uprKey (\s a -> s { _uprKey = a })
{-# INLINE uprKey #-}

-- | Part number of part being uploaded.
uprPartNumber :: Lens' UploadPart (Integer)
uprPartNumber = lens _uprPartNumber (\s a -> s { _uprPartNumber = a })
{-# INLINE uprPartNumber #-}

-- | Upload ID identifying the multipart upload whose part is being uploaded.
uprUploadId :: Lens' UploadPart (Text)
uprUploadId = lens _uprUploadId (\s a -> s { _uprUploadId = a })
{-# INLINE uprUploadId #-}

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
uprSSECustomerAlgorithm :: Lens' UploadPart (Maybe Text)
uprSSECustomerAlgorithm = lens _uprSSECustomerAlgorithm (\s a -> s { _uprSSECustomerAlgorithm = a })
{-# INLINE uprSSECustomerAlgorithm #-}

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
-- This must be the same encryption key specified in the initiate multipart
-- upload request.
uprSSECustomerKey :: Lens' UploadPart (Maybe Text)
uprSSECustomerKey = lens _uprSSECustomerKey (\s a -> s { _uprSSECustomerKey = a })
{-# INLINE uprSSECustomerKey #-}

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
uprSSECustomerKeyMD5 :: Lens' UploadPart (Maybe Text)
uprSSECustomerKeyMD5 = lens _uprSSECustomerKeyMD5 (\s a -> s { _uprSSECustomerKeyMD5 = a })
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

instance ToHeaders UploadPart

instance ToBody UploadPart

data UploadPartResponse = UploadPartResponse
    { _upoServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    , _upoETag :: Maybe ETag
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
    } deriving (Show, Generic)

-- | The Server-side encryption algorithm used when storing this object in S3.
upoServerSideEncryption :: Lens' UploadPartResponse (Maybe ServerSideEncryption)
upoServerSideEncryption = lens _upoServerSideEncryption (\s a -> s { _upoServerSideEncryption = a })
{-# INLINE upoServerSideEncryption #-}

-- | Entity tag for the uploaded object.
upoETag :: Lens' UploadPartResponse (Maybe ETag)
upoETag = lens _upoETag (\s a -> s { _upoETag = a })
{-# INLINE upoETag #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the encryption
-- algorithm used.
upoSSECustomerAlgorithm :: Lens' UploadPartResponse (Maybe Text)
upoSSECustomerAlgorithm = lens _upoSSECustomerAlgorithm (\s a -> s { _upoSSECustomerAlgorithm = a })
{-# INLINE upoSSECustomerAlgorithm #-}

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
upoSSECustomerKeyMD5 :: Lens' UploadPartResponse (Maybe Text)
upoSSECustomerKeyMD5 = lens _upoSSECustomerKeyMD5 (\s a -> s { _upoSSECustomerKeyMD5 = a })
{-# INLINE upoSSECustomerKeyMD5 #-}

instance AWSRequest UploadPart where
    type Sv UploadPart = S3
    type Rs UploadPart = UploadPartResponse

    request = put
    response _ = headerResponse $ \hs ->
        pure UploadPartResponse
            <*> hs ~:? "x-amz-server-side-encryption"
            <*> hs ~:? "ETag"
            <*> hs ~:? "x-amz-server-side-encryption-customer-algorithm"
            <*> hs ~:? "x-amz-server-side-encryption-customer-key-MD5"
