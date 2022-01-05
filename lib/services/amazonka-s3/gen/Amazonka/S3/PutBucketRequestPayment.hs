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
-- Module      : Amazonka.S3.PutBucketRequestPayment
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
module Amazonka.S3.PutBucketRequestPayment
  ( -- * Creating a Request
    PutBucketRequestPayment (..),
    newPutBucketRequestPayment,

    -- * Request Lenses
    putBucketRequestPayment_contentMD5,
    putBucketRequestPayment_expectedBucketOwner,
    putBucketRequestPayment_bucket,
    putBucketRequestPayment_requestPaymentConfiguration,

    -- * Destructuring the Response
    PutBucketRequestPaymentResponse (..),
    newPutBucketRequestPaymentResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketRequestPayment' smart constructor.
data PutBucketRequestPayment = PutBucketRequestPayment'
  { -- | The base64-encoded 128-bit MD5 digest of the data. You must use this
    -- header as a message integrity check to verify that the request body was
    -- not corrupted in transit. For more information, see
    -- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket name.
    bucket :: BucketName,
    -- | Container for Payer.
    requestPaymentConfiguration :: RequestPaymentConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketRequestPayment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentMD5', 'putBucketRequestPayment_contentMD5' - The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putBucketRequestPayment_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
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
      { contentMD5 =
          Prelude.Nothing,
        expectedBucketOwner = Prelude.Nothing,
        bucket = pBucket_,
        requestPaymentConfiguration =
          pRequestPaymentConfiguration_
      }

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putBucketRequestPayment_contentMD5 :: Lens.Lens' PutBucketRequestPayment (Prelude.Maybe Prelude.Text)
putBucketRequestPayment_contentMD5 = Lens.lens (\PutBucketRequestPayment' {contentMD5} -> contentMD5) (\s@PutBucketRequestPayment' {} a -> s {contentMD5 = a} :: PutBucketRequestPayment)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putBucketRequestPayment_expectedBucketOwner :: Lens.Lens' PutBucketRequestPayment (Prelude.Maybe Prelude.Text)
putBucketRequestPayment_expectedBucketOwner = Lens.lens (\PutBucketRequestPayment' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketRequestPayment' {} a -> s {expectedBucketOwner = a} :: PutBucketRequestPayment)

-- | The bucket name.
putBucketRequestPayment_bucket :: Lens.Lens' PutBucketRequestPayment BucketName
putBucketRequestPayment_bucket = Lens.lens (\PutBucketRequestPayment' {bucket} -> bucket) (\s@PutBucketRequestPayment' {} a -> s {bucket = a} :: PutBucketRequestPayment)

-- | Container for Payer.
putBucketRequestPayment_requestPaymentConfiguration :: Lens.Lens' PutBucketRequestPayment RequestPaymentConfiguration
putBucketRequestPayment_requestPaymentConfiguration = Lens.lens (\PutBucketRequestPayment' {requestPaymentConfiguration} -> requestPaymentConfiguration) (\s@PutBucketRequestPayment' {} a -> s {requestPaymentConfiguration = a} :: PutBucketRequestPayment)

instance Core.AWSRequest PutBucketRequestPayment where
  type
    AWSResponse PutBucketRequestPayment =
      PutBucketRequestPaymentResponse
  request =
    Request.s3vhost
      Prelude.. Request.putXML defaultService
  response =
    Response.receiveNull
      PutBucketRequestPaymentResponse'

instance Prelude.Hashable PutBucketRequestPayment where
  hashWithSalt _salt PutBucketRequestPayment' {..} =
    _salt `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` requestPaymentConfiguration

instance Prelude.NFData PutBucketRequestPayment where
  rnf PutBucketRequestPayment' {..} =
    Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf requestPaymentConfiguration

instance Core.ToElement PutBucketRequestPayment where
  toElement PutBucketRequestPayment' {..} =
    Core.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}RequestPaymentConfiguration"
      requestPaymentConfiguration

instance Core.ToHeaders PutBucketRequestPayment where
  toHeaders PutBucketRequestPayment' {..} =
    Prelude.mconcat
      [ "Content-MD5" Core.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath PutBucketRequestPayment where
  toPath PutBucketRequestPayment' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery PutBucketRequestPayment where
  toQuery =
    Prelude.const (Prelude.mconcat ["requestPayment"])

-- | /See:/ 'newPutBucketRequestPaymentResponse' smart constructor.
data PutBucketRequestPaymentResponse = PutBucketRequestPaymentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
