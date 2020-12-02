{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketRequestPayment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the request payment configuration for a bucket. By default, the bucket owner pays for downloads from the bucket. This configuration parameter enables the bucket owner (only) to specify that the person requesting the download will be charged for the download. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> .
--
--
-- The following operations are related to @PutBucketRequestPayment@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketRequestPayment.html GetBucketRequestPayment>
module Network.AWS.S3.PutBucketRequestPayment
  ( -- * Creating a Request
    putBucketRequestPayment,
    PutBucketRequestPayment,

    -- * Request Lenses
    pbrpContentMD5,
    pbrpExpectedBucketOwner,
    pbrpBucket,
    pbrpRequestPaymentConfiguration,

    -- * Destructuring the Response
    putBucketRequestPaymentResponse,
    PutBucketRequestPaymentResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketRequestPayment' smart constructor.
data PutBucketRequestPayment = PutBucketRequestPayment'
  { _pbrpContentMD5 ::
      !(Maybe Text),
    _pbrpExpectedBucketOwner :: !(Maybe Text),
    _pbrpBucket :: !BucketName,
    _pbrpRequestPaymentConfiguration ::
      !RequestPaymentConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketRequestPayment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbrpContentMD5' - >The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> . For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- * 'pbrpExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pbrpBucket' - The bucket name.
--
-- * 'pbrpRequestPaymentConfiguration' - Container for Payer.
putBucketRequestPayment ::
  -- | 'pbrpBucket'
  BucketName ->
  -- | 'pbrpRequestPaymentConfiguration'
  RequestPaymentConfiguration ->
  PutBucketRequestPayment
putBucketRequestPayment pBucket_ pRequestPaymentConfiguration_ =
  PutBucketRequestPayment'
    { _pbrpContentMD5 = Nothing,
      _pbrpExpectedBucketOwner = Nothing,
      _pbrpBucket = pBucket_,
      _pbrpRequestPaymentConfiguration = pRequestPaymentConfiguration_
    }

-- | >The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> . For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
pbrpContentMD5 :: Lens' PutBucketRequestPayment (Maybe Text)
pbrpContentMD5 = lens _pbrpContentMD5 (\s a -> s {_pbrpContentMD5 = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pbrpExpectedBucketOwner :: Lens' PutBucketRequestPayment (Maybe Text)
pbrpExpectedBucketOwner = lens _pbrpExpectedBucketOwner (\s a -> s {_pbrpExpectedBucketOwner = a})

-- | The bucket name.
pbrpBucket :: Lens' PutBucketRequestPayment BucketName
pbrpBucket = lens _pbrpBucket (\s a -> s {_pbrpBucket = a})

-- | Container for Payer.
pbrpRequestPaymentConfiguration :: Lens' PutBucketRequestPayment RequestPaymentConfiguration
pbrpRequestPaymentConfiguration = lens _pbrpRequestPaymentConfiguration (\s a -> s {_pbrpRequestPaymentConfiguration = a})

instance AWSRequest PutBucketRequestPayment where
  type Rs PutBucketRequestPayment = PutBucketRequestPaymentResponse
  request = putXML s3
  response = receiveNull PutBucketRequestPaymentResponse'

instance Hashable PutBucketRequestPayment

instance NFData PutBucketRequestPayment

instance ToElement PutBucketRequestPayment where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}RequestPaymentConfiguration"
      . _pbrpRequestPaymentConfiguration

instance ToHeaders PutBucketRequestPayment where
  toHeaders PutBucketRequestPayment' {..} =
    mconcat
      [ "Content-MD5" =# _pbrpContentMD5,
        "x-amz-expected-bucket-owner" =# _pbrpExpectedBucketOwner
      ]

instance ToPath PutBucketRequestPayment where
  toPath PutBucketRequestPayment' {..} =
    mconcat ["/", toBS _pbrpBucket]

instance ToQuery PutBucketRequestPayment where
  toQuery = const (mconcat ["requestPayment"])

-- | /See:/ 'putBucketRequestPaymentResponse' smart constructor.
data PutBucketRequestPaymentResponse = PutBucketRequestPaymentResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketRequestPaymentResponse' with the minimum fields required to make a request.
putBucketRequestPaymentResponse ::
  PutBucketRequestPaymentResponse
putBucketRequestPaymentResponse = PutBucketRequestPaymentResponse'

instance NFData PutBucketRequestPaymentResponse
