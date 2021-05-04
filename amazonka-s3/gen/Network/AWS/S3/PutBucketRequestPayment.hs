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
-- Module      : Network.AWS.S3.PutBucketRequestPayment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the request payment configuration for a bucket. By default, the
-- bucket owner pays for downloads from the bucket. This configuration
-- parameter enables the bucket owner (only) to specify that the person
-- requesting the download will be charged for the download. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/RequesterPaysBuckets.html Requester Pays Buckets>.
--
-- The following operations are related to @PutBucketRequestPayment@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketRequestPayment.html GetBucketRequestPayment>
module Network.AWS.S3.PutBucketRequestPayment
  ( -- * Creating a Request
    PutBucketRequestPayment (..),
    newPutBucketRequestPayment,

    -- * Request Lenses
    putBucketRequestPayment_expectedBucketOwner,
    putBucketRequestPayment_contentMD5,
    putBucketRequestPayment_bucket,
    putBucketRequestPayment_requestPaymentConfiguration,

    -- * Destructuring the Response
    PutBucketRequestPaymentResponse (..),
    newPutBucketRequestPaymentResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutBucketRequestPayment' smart constructor.
data PutBucketRequestPayment = PutBucketRequestPayment'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded 128-bit MD5 digest of the data. You must use this
    -- header as a message integrity check to verify that the request body was
    -- not corrupted in transit. For more information, see
    -- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS
    -- SDKs, this field is calculated automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The bucket name.
    bucket :: BucketName,
    -- | Container for Payer.
    requestPaymentConfiguration :: RequestPaymentConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBucketRequestPayment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putBucketRequestPayment_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'contentMD5', 'putBucketRequestPayment_contentMD5' - The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
--
-- 'bucket', 'putBucketRequestPayment_bucket' - The bucket name.
--
-- 'requestPaymentConfiguration', 'putBucketRequestPayment_requestPaymentConfiguration' - Container for Payer.
newPutBucketRequestPayment ::
  -- | 'bucket'
  BucketName ->
  -- | 'requestPaymentConfiguration'
  RequestPaymentConfiguration ->
  PutBucketRequestPayment
newPutBucketRequestPayment
  pBucket_
  pRequestPaymentConfiguration_ =
    PutBucketRequestPayment'
      { expectedBucketOwner =
          Prelude.Nothing,
        contentMD5 = Prelude.Nothing,
        bucket = pBucket_,
        requestPaymentConfiguration =
          pRequestPaymentConfiguration_
      }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putBucketRequestPayment_expectedBucketOwner :: Lens.Lens' PutBucketRequestPayment (Prelude.Maybe Prelude.Text)
putBucketRequestPayment_expectedBucketOwner = Lens.lens (\PutBucketRequestPayment' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketRequestPayment' {} a -> s {expectedBucketOwner = a} :: PutBucketRequestPayment)

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
putBucketRequestPayment_contentMD5 :: Lens.Lens' PutBucketRequestPayment (Prelude.Maybe Prelude.Text)
putBucketRequestPayment_contentMD5 = Lens.lens (\PutBucketRequestPayment' {contentMD5} -> contentMD5) (\s@PutBucketRequestPayment' {} a -> s {contentMD5 = a} :: PutBucketRequestPayment)

-- | The bucket name.
putBucketRequestPayment_bucket :: Lens.Lens' PutBucketRequestPayment BucketName
putBucketRequestPayment_bucket = Lens.lens (\PutBucketRequestPayment' {bucket} -> bucket) (\s@PutBucketRequestPayment' {} a -> s {bucket = a} :: PutBucketRequestPayment)

-- | Container for Payer.
putBucketRequestPayment_requestPaymentConfiguration :: Lens.Lens' PutBucketRequestPayment RequestPaymentConfiguration
putBucketRequestPayment_requestPaymentConfiguration = Lens.lens (\PutBucketRequestPayment' {requestPaymentConfiguration} -> requestPaymentConfiguration) (\s@PutBucketRequestPayment' {} a -> s {requestPaymentConfiguration = a} :: PutBucketRequestPayment)

instance Prelude.AWSRequest PutBucketRequestPayment where
  type
    Rs PutBucketRequestPayment =
      PutBucketRequestPaymentResponse
  request = Request.putXML defaultService
  response =
    Response.receiveNull
      PutBucketRequestPaymentResponse'

instance Prelude.Hashable PutBucketRequestPayment

instance Prelude.NFData PutBucketRequestPayment

instance Prelude.ToElement PutBucketRequestPayment where
  toElement PutBucketRequestPayment' {..} =
    Prelude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}RequestPaymentConfiguration"
      requestPaymentConfiguration

instance Prelude.ToHeaders PutBucketRequestPayment where
  toHeaders PutBucketRequestPayment' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner,
        "Content-MD5" Prelude.=# contentMD5
      ]

instance Prelude.ToPath PutBucketRequestPayment where
  toPath PutBucketRequestPayment' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery PutBucketRequestPayment where
  toQuery =
    Prelude.const (Prelude.mconcat ["requestPayment"])

-- | /See:/ 'newPutBucketRequestPaymentResponse' smart constructor.
data PutBucketRequestPaymentResponse = PutBucketRequestPaymentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBucketRequestPaymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketRequestPaymentResponse ::
  PutBucketRequestPaymentResponse
newPutBucketRequestPaymentResponse =
  PutBucketRequestPaymentResponse'

instance
  Prelude.NFData
    PutBucketRequestPaymentResponse
