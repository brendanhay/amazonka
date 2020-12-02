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
-- Module      : Network.AWS.S3.GetBucketRequestPayment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the request payment configuration of a bucket. To use this version of the operation, you must be the bucket owner. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets> .
--
--
-- The following operations are related to @GetBucketRequestPayment@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjects.html ListObjects>
module Network.AWS.S3.GetBucketRequestPayment
  ( -- * Creating a Request
    getBucketRequestPayment,
    GetBucketRequestPayment,

    -- * Request Lenses
    gbrpExpectedBucketOwner,
    gbrpBucket,

    -- * Destructuring the Response
    getBucketRequestPaymentResponse,
    GetBucketRequestPaymentResponse,

    -- * Response Lenses
    gbrprsPayer,
    gbrprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketRequestPayment' smart constructor.
data GetBucketRequestPayment = GetBucketRequestPayment'
  { _gbrpExpectedBucketOwner ::
      !(Maybe Text),
    _gbrpBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketRequestPayment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrpExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gbrpBucket' - The name of the bucket for which to get the payment request configuration
getBucketRequestPayment ::
  -- | 'gbrpBucket'
  BucketName ->
  GetBucketRequestPayment
getBucketRequestPayment pBucket_ =
  GetBucketRequestPayment'
    { _gbrpExpectedBucketOwner = Nothing,
      _gbrpBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gbrpExpectedBucketOwner :: Lens' GetBucketRequestPayment (Maybe Text)
gbrpExpectedBucketOwner = lens _gbrpExpectedBucketOwner (\s a -> s {_gbrpExpectedBucketOwner = a})

-- | The name of the bucket for which to get the payment request configuration
gbrpBucket :: Lens' GetBucketRequestPayment BucketName
gbrpBucket = lens _gbrpBucket (\s a -> s {_gbrpBucket = a})

instance AWSRequest GetBucketRequestPayment where
  type Rs GetBucketRequestPayment = GetBucketRequestPaymentResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketRequestPaymentResponse'
            <$> (x .@? "Payer") <*> (pure (fromEnum s))
      )

instance Hashable GetBucketRequestPayment

instance NFData GetBucketRequestPayment

instance ToHeaders GetBucketRequestPayment where
  toHeaders GetBucketRequestPayment' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gbrpExpectedBucketOwner]

instance ToPath GetBucketRequestPayment where
  toPath GetBucketRequestPayment' {..} =
    mconcat ["/", toBS _gbrpBucket]

instance ToQuery GetBucketRequestPayment where
  toQuery = const (mconcat ["requestPayment"])

-- | /See:/ 'getBucketRequestPaymentResponse' smart constructor.
data GetBucketRequestPaymentResponse = GetBucketRequestPaymentResponse'
  { _gbrprsPayer ::
      !(Maybe Payer),
    _gbrprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketRequestPaymentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrprsPayer' - Specifies who pays for the download and request fees.
--
-- * 'gbrprsResponseStatus' - -- | The response status code.
getBucketRequestPaymentResponse ::
  -- | 'gbrprsResponseStatus'
  Int ->
  GetBucketRequestPaymentResponse
getBucketRequestPaymentResponse pResponseStatus_ =
  GetBucketRequestPaymentResponse'
    { _gbrprsPayer = Nothing,
      _gbrprsResponseStatus = pResponseStatus_
    }

-- | Specifies who pays for the download and request fees.
gbrprsPayer :: Lens' GetBucketRequestPaymentResponse (Maybe Payer)
gbrprsPayer = lens _gbrprsPayer (\s a -> s {_gbrprsPayer = a})

-- | -- | The response status code.
gbrprsResponseStatus :: Lens' GetBucketRequestPaymentResponse Int
gbrprsResponseStatus = lens _gbrprsResponseStatus (\s a -> s {_gbrprsResponseStatus = a})

instance NFData GetBucketRequestPaymentResponse
