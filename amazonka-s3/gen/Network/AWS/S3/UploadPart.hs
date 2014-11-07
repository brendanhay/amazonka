{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , uprBody
    , uprBucket
    , uprContentLength
    , uprContentMD5
    , uprKey
    , uprPartNumber
    , uprSSECustomerAlgorithm
    , uprSSECustomerKey
    , uprSSECustomerKeyMD5
    , uprUploadId

    -- * Response
    , UploadPartOutput
    -- ** Response constructor
    , uploadPartOutput
    -- ** Response lenses
    , upoETag
    , upoSSECustomerAlgorithm
    , upoSSECustomerKeyMD5
    , upoServerSideEncryption
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data UploadPart = UploadPart
    { _uprBody                 :: RqBody
    , _uprBucket               :: Text
    , _uprContentLength        :: Maybe Int
    , _uprContentMD5           :: Maybe Text
    , _uprKey                  :: Text
    , _uprPartNumber           :: Int
    , _uprSSECustomerAlgorithm :: Maybe Text
    , _uprSSECustomerKey       :: Maybe (Sensitive Text)
    , _uprSSECustomerKeyMD5    :: Maybe Text
    , _uprUploadId             :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UploadPart' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uprBody' @::@ 'RqBody'
--
-- * 'uprBucket' @::@ 'Text'
--
-- * 'uprContentLength' @::@ 'Maybe' 'Int'
--
-- * 'uprContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'uprKey' @::@ 'Text'
--
-- * 'uprPartNumber' @::@ 'Int'
--
-- * 'uprSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'uprSSECustomerKey' @::@ 'Maybe' 'Text'
--
-- * 'uprSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'uprUploadId' @::@ 'Text'
--
uploadPart :: RqBody -- ^ 'uprBody'
           -> Text -- ^ 'uprBucket'
           -> Text -- ^ 'uprKey'
           -> Int -- ^ 'uprPartNumber'
           -> Text -- ^ 'uprUploadId'
           -> UploadPart
uploadPart p1 p2 p3 p4 p5 = UploadPart
    { _uprBody                 = p1
    , _uprBucket               = p2
    , _uprKey                  = p3
    , _uprPartNumber           = p4
    , _uprUploadId             = p5
    , _uprContentLength        = Nothing
    , _uprContentMD5           = Nothing
    , _uprSSECustomerAlgorithm = Nothing
    , _uprSSECustomerKey       = Nothing
    , _uprSSECustomerKeyMD5    = Nothing
    }

uprBody :: Lens' UploadPart RqBody
uprBody = lens _uprBody (\s a -> s { _uprBody = a })

uprBucket :: Lens' UploadPart Text
uprBucket = lens _uprBucket (\s a -> s { _uprBucket = a })

-- | Size of the body in bytes. This parameter is useful when the size of the
-- body cannot be determined automatically.
uprContentLength :: Lens' UploadPart (Maybe Int)
uprContentLength = lens _uprContentLength (\s a -> s { _uprContentLength = a })

uprContentMD5 :: Lens' UploadPart (Maybe Text)
uprContentMD5 = lens _uprContentMD5 (\s a -> s { _uprContentMD5 = a })

uprKey :: Lens' UploadPart Text
uprKey = lens _uprKey (\s a -> s { _uprKey = a })

-- | Part number of part being uploaded.
uprPartNumber :: Lens' UploadPart Int
uprPartNumber = lens _uprPartNumber (\s a -> s { _uprPartNumber = a })

-- | Specifies the algorithm to use to when encrypting the object (e.g.,
-- AES256).
uprSSECustomerAlgorithm :: Lens' UploadPart (Maybe Text)
uprSSECustomerAlgorithm =
    lens _uprSSECustomerAlgorithm (\s a -> s { _uprSSECustomerAlgorithm = a })

-- | Specifies the customer-provided encryption key for Amazon S3 to use in
-- encrypting data. This value is used to store the object and then it is
-- discarded; Amazon does not store the encryption key. The key must be
-- appropriate for use with the algorithm specified in the
-- x-amz-server-side&#x200B;-encryption&#x200B;-customer-algorithm header.
-- This must be the same encryption key specified in the initiate multipart
-- upload request.
uprSSECustomerKey :: Lens' UploadPart (Maybe Text)
uprSSECustomerKey =
    lens _uprSSECustomerKey (\s a -> s { _uprSSECustomerKey = a })
        . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
uprSSECustomerKeyMD5 :: Lens' UploadPart (Maybe Text)
uprSSECustomerKeyMD5 =
    lens _uprSSECustomerKeyMD5 (\s a -> s { _uprSSECustomerKeyMD5 = a })

-- | Upload ID identifying the multipart upload whose part is being uploaded.
uprUploadId :: Lens' UploadPart Text
uprUploadId = lens _uprUploadId (\s a -> s { _uprUploadId = a })

instance ToPath UploadPart where
    toPath UploadPart{..} = mconcat
        [ "/"
        , toText _uprBucket
        , "/"
        , toText _uprKey
        ]

instance ToQuery UploadPart where
    toQuery UploadPart{..} = mconcat
        [ "partNumber" =? _uprPartNumber
        , "uploadId"   =? _uprUploadId
        ]

instance ToHeaders UploadPart where
    toHeaders UploadPart{..} = mconcat
        [ "Content-Length"                                  =: _uprContentLength
        , "Content-MD5"                                     =: _uprContentMD5
        , "x-amz-server-side-encryption-customer-algorithm" =: _uprSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"       =: _uprSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"   =: _uprSSECustomerKeyMD5
        ]

instance ToBody UploadPart where
    toBody = toBody . _uprBody

data UploadPartOutput = UploadPartOutput
    { _upoETag                 :: Maybe Text
    , _upoSSECustomerAlgorithm :: Maybe Text
    , _upoSSECustomerKeyMD5    :: Maybe Text
    , _upoServerSideEncryption :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UploadPartOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upoETag' @::@ 'Maybe' 'Text'
--
-- * 'upoSSECustomerAlgorithm' @::@ 'Maybe' 'Text'
--
-- * 'upoSSECustomerKeyMD5' @::@ 'Maybe' 'Text'
--
-- * 'upoServerSideEncryption' @::@ 'Maybe' 'Text'
--
uploadPartOutput :: UploadPartOutput
uploadPartOutput = UploadPartOutput
    { _upoServerSideEncryption = Nothing
    , _upoETag                 = Nothing
    , _upoSSECustomerAlgorithm = Nothing
    , _upoSSECustomerKeyMD5    = Nothing
    }

-- | Entity tag for the uploaded object.
upoETag :: Lens' UploadPartOutput (Maybe Text)
upoETag = lens _upoETag (\s a -> s { _upoETag = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header confirming the
-- encryption algorithm used.
upoSSECustomerAlgorithm :: Lens' UploadPartOutput (Maybe Text)
upoSSECustomerAlgorithm =
    lens _upoSSECustomerAlgorithm (\s a -> s { _upoSSECustomerAlgorithm = a })

-- | If server-side encryption with a customer-provided encryption key was
-- requested, the response will include this header to provide round trip
-- message integrity verification of the customer-provided encryption key.
upoSSECustomerKeyMD5 :: Lens' UploadPartOutput (Maybe Text)
upoSSECustomerKeyMD5 =
    lens _upoSSECustomerKeyMD5 (\s a -> s { _upoSSECustomerKeyMD5 = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
upoServerSideEncryption :: Lens' UploadPartOutput (Maybe Text)
upoServerSideEncryption =
    lens _upoServerSideEncryption (\s a -> s { _upoServerSideEncryption = a })

instance AWSRequest UploadPart where
    type Sv UploadPart = S3
    type Rs UploadPart = UploadPartOutput

    request  = put'
    response = const . xmlResponse $ \h x -> UploadPartOutput
        <$> h ~:? "ETag"
        <*> h ~:? "x-amz-server-side-encryption-customer-algorithm"
        <*> h ~:? "x-amz-server-side-encryption-customer-key-MD5"
        <*> h ~:? "x-amz-server-side-encryption"
