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
    , upSSECustomerAlgorithm
    , upSSECustomerKey
    , upSSECustomerKeyMD5
    , upUploadId

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
    { _upBody                 :: RqBody
    , _upBucket               :: Text
    , _upContentLength        :: Maybe Int
    , _upContentMD5           :: Maybe Text
    , _upKey                  :: Text
    , _upPartNumber           :: Int
    , _upSSECustomerAlgorithm :: Maybe Text
    , _upSSECustomerKey       :: Maybe (Sensitive Text)
    , _upSSECustomerKeyMD5    :: Maybe Text
    , _upUploadId             :: Text
    } deriving (Show, Generic)

-- | 'UploadPart' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upBody' @::@ 'RqBody'
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
-- * 'upUploadId' @::@ 'Text'
--
uploadPart :: RqBody -- ^ 'upBody'
           -> Text -- ^ 'upBucket'
           -> Text -- ^ 'upKey'
           -> Int -- ^ 'upPartNumber'
           -> Text -- ^ 'upUploadId'
           -> UploadPart
uploadPart p1 p2 p3 p4 p5 = UploadPart
    { _upBody                 = p1
    , _upBucket               = p2
    , _upKey                  = p3
    , _upPartNumber           = p4
    , _upUploadId             = p5
    , _upContentLength        = Nothing
    , _upContentMD5           = Nothing
    , _upSSECustomerAlgorithm = Nothing
    , _upSSECustomerKey       = Nothing
    , _upSSECustomerKeyMD5    = Nothing
    }

upBody :: Lens' UploadPart RqBody
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
upSSECustomerKey = lens _upSSECustomerKey (\s a -> s { _upSSECustomerKey = a })
    . mapping _Sensitive

-- | Specifies the 128-bit MD5 digest of the encryption key according to RFC
-- 1321. Amazon S3 uses this header for a message integrity check to ensure
-- the encryption key was transmitted without error.
upSSECustomerKeyMD5 :: Lens' UploadPart (Maybe Text)
upSSECustomerKeyMD5 =
    lens _upSSECustomerKeyMD5 (\s a -> s { _upSSECustomerKeyMD5 = a })

-- | Upload ID identifying the multipart upload whose part is being uploaded.
upUploadId :: Lens' UploadPart Text
upUploadId = lens _upUploadId (\s a -> s { _upUploadId = a })

instance ToPath UploadPart where
    toPath UploadPart{..} = mconcat
        [ "/"
        , toText _upBucket
        , "/"
        , toText _upKey
        ]

instance ToQuery UploadPart where
    toQuery UploadPart{..} = mconcat
        [ "partNumber" =? _upPartNumber
        , "uploadId"   =? _upUploadId
        ]

instance ToHeaders UploadPart where
    toHeaders UploadPart{..} = mconcat
        [ "Content-Length"                                  =: _upContentLength
        , "Content-MD5"                                     =: _upContentMD5
        , "x-amz-server-side-encryption-customer-algorithm" =: _upSSECustomerAlgorithm
        , "x-amz-server-side-encryption-customer-key"       =: _upSSECustomerKey
        , "x-amz-server-side-encryption-customer-key-MD5"   =: _upSSECustomerKeyMD5
        ]

instance ToBody UploadPart where
    toBody = toBody . _upBody

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

instance FromXML UploadPartOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "UploadPartOutput"
instance AWSRequest UploadPart where
    type Sv UploadPart = S3
    type Rs UploadPart = UploadPartOutput

    request  = put
    response = xmlResponse $ \h x -> UploadPartOutput
        <$> h ~:? "ETag"
        <*> h ~:? "x-amz-server-side-encryption-customer-algorithm"
        <*> h ~:? "x-amz-server-side-encryption-customer-key-MD5"
        <*> h ~:? "x-amz-server-side-encryption"
