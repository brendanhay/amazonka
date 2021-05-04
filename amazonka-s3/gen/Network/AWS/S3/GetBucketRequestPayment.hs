{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketRequestPayment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the request payment configuration of a bucket. To use this
-- version of the operation, you must be the bucket owner. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets>.
--
-- The following operations are related to @GetBucketRequestPayment@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjects.html ListObjects>
module Network.AWS.S3.GetBucketRequestPayment
  ( -- * Creating a Request
    GetBucketRequestPayment (..),
    newGetBucketRequestPayment,

    -- * Request Lenses
    getBucketRequestPayment_expectedBucketOwner,
    getBucketRequestPayment_bucket,

    -- * Destructuring the Response
    GetBucketRequestPaymentResponse (..),
    newGetBucketRequestPaymentResponse,

    -- * Response Lenses
    getBucketRequestPaymentResponse_payer,
    getBucketRequestPaymentResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketRequestPayment' smart constructor.
data GetBucketRequestPayment = GetBucketRequestPayment'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket for which to get the payment request
    -- configuration
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBucketRequestPayment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketRequestPayment_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getBucketRequestPayment_bucket' - The name of the bucket for which to get the payment request
-- configuration
newGetBucketRequestPayment ::
  -- | 'bucket'
  BucketName ->
  GetBucketRequestPayment
newGetBucketRequestPayment pBucket_ =
  GetBucketRequestPayment'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketRequestPayment_expectedBucketOwner :: Lens.Lens' GetBucketRequestPayment (Prelude.Maybe Prelude.Text)
getBucketRequestPayment_expectedBucketOwner = Lens.lens (\GetBucketRequestPayment' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketRequestPayment' {} a -> s {expectedBucketOwner = a} :: GetBucketRequestPayment)

-- | The name of the bucket for which to get the payment request
-- configuration
getBucketRequestPayment_bucket :: Lens.Lens' GetBucketRequestPayment BucketName
getBucketRequestPayment_bucket = Lens.lens (\GetBucketRequestPayment' {bucket} -> bucket) (\s@GetBucketRequestPayment' {} a -> s {bucket = a} :: GetBucketRequestPayment)

instance Prelude.AWSRequest GetBucketRequestPayment where
  type
    Rs GetBucketRequestPayment =
      GetBucketRequestPaymentResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketRequestPaymentResponse'
            Prelude.<$> (x Prelude..@? "Payer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketRequestPayment

instance Prelude.NFData GetBucketRequestPayment

instance Prelude.ToHeaders GetBucketRequestPayment where
  toHeaders GetBucketRequestPayment' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance Prelude.ToPath GetBucketRequestPayment where
  toPath GetBucketRequestPayment' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery GetBucketRequestPayment where
  toQuery =
    Prelude.const (Prelude.mconcat ["requestPayment"])

-- | /See:/ 'newGetBucketRequestPaymentResponse' smart constructor.
data GetBucketRequestPaymentResponse = GetBucketRequestPaymentResponse'
  { -- | Specifies who pays for the download and request fees.
    payer :: Prelude.Maybe Payer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBucketRequestPaymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payer', 'getBucketRequestPaymentResponse_payer' - Specifies who pays for the download and request fees.
--
-- 'httpStatus', 'getBucketRequestPaymentResponse_httpStatus' - The response's http status code.
newGetBucketRequestPaymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketRequestPaymentResponse
newGetBucketRequestPaymentResponse pHttpStatus_ =
  GetBucketRequestPaymentResponse'
    { payer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies who pays for the download and request fees.
getBucketRequestPaymentResponse_payer :: Lens.Lens' GetBucketRequestPaymentResponse (Prelude.Maybe Payer)
getBucketRequestPaymentResponse_payer = Lens.lens (\GetBucketRequestPaymentResponse' {payer} -> payer) (\s@GetBucketRequestPaymentResponse' {} a -> s {payer = a} :: GetBucketRequestPaymentResponse)

-- | The response's http status code.
getBucketRequestPaymentResponse_httpStatus :: Lens.Lens' GetBucketRequestPaymentResponse Prelude.Int
getBucketRequestPaymentResponse_httpStatus = Lens.lens (\GetBucketRequestPaymentResponse' {httpStatus} -> httpStatus) (\s@GetBucketRequestPaymentResponse' {} a -> s {httpStatus = a} :: GetBucketRequestPaymentResponse)

instance
  Prelude.NFData
    GetBucketRequestPaymentResponse
