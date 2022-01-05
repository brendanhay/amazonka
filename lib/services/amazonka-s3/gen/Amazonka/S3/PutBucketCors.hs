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
-- Module      : Amazonka.S3.PutBucketCors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the @cors@ configuration for your bucket. If the configuration
-- exists, Amazon S3 replaces it.
--
-- To use this operation, you must be allowed to perform the
-- @s3:PutBucketCORS@ action. By default, the bucket owner has this
-- permission and can grant it to others.
--
-- You set this configuration on a bucket so that the bucket can service
-- cross-origin requests. For example, you might want to enable a request
-- whose origin is @http:\/\/www.example.com@ to access your Amazon S3
-- bucket at @my.example.bucket.com@ by using the browser\'s
-- @XMLHttpRequest@ capability.
--
-- To enable cross-origin resource sharing (CORS) on a bucket, you add the
-- @cors@ subresource to the bucket. The @cors@ subresource is an XML
-- document in which you configure rules that identify origins and the HTTP
-- methods that can be executed on your bucket. The document is limited to
-- 64 KB in size.
--
-- When Amazon S3 receives a cross-origin request (or a pre-flight OPTIONS
-- request) against a bucket, it evaluates the @cors@ configuration on the
-- bucket and uses the first @CORSRule@ rule that matches the incoming
-- browser request to enable a cross-origin request. For a rule to match,
-- the following conditions must be met:
--
-- -   The request\'s @Origin@ header must match @AllowedOrigin@ elements.
--
-- -   The request method (for example, GET, PUT, HEAD, and so on) or the
--     @Access-Control-Request-Method@ header in case of a pre-flight
--     @OPTIONS@ request must be one of the @AllowedMethod@ elements.
--
-- -   Every header specified in the @Access-Control-Request-Headers@
--     request header of a pre-flight request must match an @AllowedHeader@
--     element.
--
-- For more information about CORS, go to
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing>
-- in the /Amazon S3 User Guide/.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketCors.html GetBucketCors>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketCors.html DeleteBucketCors>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTOPTIONSobject.html RESTOPTIONSobject>
module Amazonka.S3.PutBucketCors
  ( -- * Creating a Request
    PutBucketCors (..),
    newPutBucketCors,

    -- * Request Lenses
    putBucketCors_contentMD5,
    putBucketCors_expectedBucketOwner,
    putBucketCors_bucket,
    putBucketCors_cORSConfiguration,

    -- * Destructuring the Response
    PutBucketCorsResponse (..),
    newPutBucketCorsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketCors' smart constructor.
data PutBucketCors = PutBucketCors'
  { -- | The base64-encoded 128-bit MD5 digest of the data. This header must be
    -- used as a message integrity check to verify that the request body was
    -- not corrupted in transit. For more information, go to
    -- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Specifies the bucket impacted by the @cors@configuration.
    bucket :: BucketName,
    -- | Describes the cross-origin access configuration for objects in an Amazon
    -- S3 bucket. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing>
    -- in the /Amazon S3 User Guide/.
    cORSConfiguration :: CORSConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketCors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentMD5', 'putBucketCors_contentMD5' - The base64-encoded 128-bit MD5 digest of the data. This header must be
-- used as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, go to
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putBucketCors_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'putBucketCors_bucket' - Specifies the bucket impacted by the @cors@configuration.
--
-- 'cORSConfiguration', 'putBucketCors_cORSConfiguration' - Describes the cross-origin access configuration for objects in an Amazon
-- S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing>
-- in the /Amazon S3 User Guide/.
newPutBucketCors ::
  -- | 'bucket'
  BucketName ->
  -- | 'cORSConfiguration'
  CORSConfiguration ->
  PutBucketCors
newPutBucketCors pBucket_ pCORSConfiguration_ =
  PutBucketCors'
    { contentMD5 = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      bucket = pBucket_,
      cORSConfiguration = pCORSConfiguration_
    }

-- | The base64-encoded 128-bit MD5 digest of the data. This header must be
-- used as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, go to
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putBucketCors_contentMD5 :: Lens.Lens' PutBucketCors (Prelude.Maybe Prelude.Text)
putBucketCors_contentMD5 = Lens.lens (\PutBucketCors' {contentMD5} -> contentMD5) (\s@PutBucketCors' {} a -> s {contentMD5 = a} :: PutBucketCors)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putBucketCors_expectedBucketOwner :: Lens.Lens' PutBucketCors (Prelude.Maybe Prelude.Text)
putBucketCors_expectedBucketOwner = Lens.lens (\PutBucketCors' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketCors' {} a -> s {expectedBucketOwner = a} :: PutBucketCors)

-- | Specifies the bucket impacted by the @cors@configuration.
putBucketCors_bucket :: Lens.Lens' PutBucketCors BucketName
putBucketCors_bucket = Lens.lens (\PutBucketCors' {bucket} -> bucket) (\s@PutBucketCors' {} a -> s {bucket = a} :: PutBucketCors)

-- | Describes the cross-origin access configuration for objects in an Amazon
-- S3 bucket. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing>
-- in the /Amazon S3 User Guide/.
putBucketCors_cORSConfiguration :: Lens.Lens' PutBucketCors CORSConfiguration
putBucketCors_cORSConfiguration = Lens.lens (\PutBucketCors' {cORSConfiguration} -> cORSConfiguration) (\s@PutBucketCors' {} a -> s {cORSConfiguration = a} :: PutBucketCors)

instance Core.AWSRequest PutBucketCors where
  type
    AWSResponse PutBucketCors =
      PutBucketCorsResponse
  request =
    Request.contentMD5Header
      Prelude.. Request.s3vhost
      Prelude.. Request.putXML defaultService
  response =
    Response.receiveNull PutBucketCorsResponse'

instance Prelude.Hashable PutBucketCors where
  hashWithSalt _salt PutBucketCors' {..} =
    _salt `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` cORSConfiguration

instance Prelude.NFData PutBucketCors where
  rnf PutBucketCors' {..} =
    Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf cORSConfiguration

instance Core.ToElement PutBucketCors where
  toElement PutBucketCors' {..} =
    Core.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}CORSConfiguration"
      cORSConfiguration

instance Core.ToHeaders PutBucketCors where
  toHeaders PutBucketCors' {..} =
    Prelude.mconcat
      [ "Content-MD5" Core.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath PutBucketCors where
  toPath PutBucketCors' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery PutBucketCors where
  toQuery = Prelude.const (Prelude.mconcat ["cors"])

-- | /See:/ 'newPutBucketCorsResponse' smart constructor.
data PutBucketCorsResponse = PutBucketCorsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketCorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketCorsResponse ::
  PutBucketCorsResponse
newPutBucketCorsResponse = PutBucketCorsResponse'

instance Prelude.NFData PutBucketCorsResponse where
  rnf _ = ()
