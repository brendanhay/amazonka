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
-- Module      : Amazonka.S3.PutBucketWebsite
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the configuration of the website that is specified in the @website@
-- subresource. To configure a bucket as a website, you can add this
-- subresource on the bucket with website configuration information such as
-- the file name of the index document and any redirect rules. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3>.
--
-- This PUT action requires the @S3:PutBucketWebsite@ permission. By
-- default, only the bucket owner can configure the website attached to a
-- bucket; however, bucket owners can allow other users to set the website
-- configuration by writing a bucket policy that grants them the
-- @S3:PutBucketWebsite@ permission.
--
-- To redirect all website requests sent to the bucket\'s website endpoint,
-- you add a website configuration with the following elements. Because all
-- requests are sent to another website, you don\'t need to provide index
-- document name for the bucket.
--
-- -   @WebsiteConfiguration@
--
-- -   @RedirectAllRequestsTo@
--
-- -   @HostName@
--
-- -   @Protocol@
--
-- If you want granular control over redirects, you can use the following
-- elements to add routing rules that describe conditions for redirecting
-- requests and information about the redirect destination. In this case,
-- the website configuration must provide an index document for the bucket,
-- because some requests might not be redirected.
--
-- -   @WebsiteConfiguration@
--
-- -   @IndexDocument@
--
-- -   @Suffix@
--
-- -   @ErrorDocument@
--
-- -   @Key@
--
-- -   @RoutingRules@
--
-- -   @RoutingRule@
--
-- -   @Condition@
--
-- -   @HttpErrorCodeReturnedEquals@
--
-- -   @KeyPrefixEquals@
--
-- -   @Redirect@
--
-- -   @Protocol@
--
-- -   @HostName@
--
-- -   @ReplaceKeyPrefixWith@
--
-- -   @ReplaceKeyWith@
--
-- -   @HttpRedirectCode@
--
-- Amazon S3 has a limitation of 50 routing rules per website
-- configuration. If you require more than 50 routing rules, you can use
-- object redirect. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/how-to-page-redirect.html Configuring an Object Redirect>
-- in the /Amazon S3 User Guide/.
module Amazonka.S3.PutBucketWebsite
  ( -- * Creating a Request
    PutBucketWebsite (..),
    newPutBucketWebsite,

    -- * Request Lenses
    putBucketWebsite_contentMD5,
    putBucketWebsite_expectedBucketOwner,
    putBucketWebsite_bucket,
    putBucketWebsite_websiteConfiguration,

    -- * Destructuring the Response
    PutBucketWebsiteResponse (..),
    newPutBucketWebsiteResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketWebsite' smart constructor.
data PutBucketWebsite = PutBucketWebsite'
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
    -- | Container for the request.
    websiteConfiguration :: WebsiteConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketWebsite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentMD5', 'putBucketWebsite_contentMD5' - The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putBucketWebsite_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'putBucketWebsite_bucket' - The bucket name.
--
-- 'websiteConfiguration', 'putBucketWebsite_websiteConfiguration' - Container for the request.
newPutBucketWebsite ::
  -- | 'bucket'
  BucketName ->
  -- | 'websiteConfiguration'
  WebsiteConfiguration ->
  PutBucketWebsite
newPutBucketWebsite pBucket_ pWebsiteConfiguration_ =
  PutBucketWebsite'
    { contentMD5 = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      bucket = pBucket_,
      websiteConfiguration = pWebsiteConfiguration_
    }

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putBucketWebsite_contentMD5 :: Lens.Lens' PutBucketWebsite (Prelude.Maybe Prelude.Text)
putBucketWebsite_contentMD5 = Lens.lens (\PutBucketWebsite' {contentMD5} -> contentMD5) (\s@PutBucketWebsite' {} a -> s {contentMD5 = a} :: PutBucketWebsite)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putBucketWebsite_expectedBucketOwner :: Lens.Lens' PutBucketWebsite (Prelude.Maybe Prelude.Text)
putBucketWebsite_expectedBucketOwner = Lens.lens (\PutBucketWebsite' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketWebsite' {} a -> s {expectedBucketOwner = a} :: PutBucketWebsite)

-- | The bucket name.
putBucketWebsite_bucket :: Lens.Lens' PutBucketWebsite BucketName
putBucketWebsite_bucket = Lens.lens (\PutBucketWebsite' {bucket} -> bucket) (\s@PutBucketWebsite' {} a -> s {bucket = a} :: PutBucketWebsite)

-- | Container for the request.
putBucketWebsite_websiteConfiguration :: Lens.Lens' PutBucketWebsite WebsiteConfiguration
putBucketWebsite_websiteConfiguration = Lens.lens (\PutBucketWebsite' {websiteConfiguration} -> websiteConfiguration) (\s@PutBucketWebsite' {} a -> s {websiteConfiguration = a} :: PutBucketWebsite)

instance Core.AWSRequest PutBucketWebsite where
  type
    AWSResponse PutBucketWebsite =
      PutBucketWebsiteResponse
  request =
    Request.s3vhost
      Prelude.. Request.putXML defaultService
  response =
    Response.receiveNull PutBucketWebsiteResponse'

instance Prelude.Hashable PutBucketWebsite where
  hashWithSalt _salt PutBucketWebsite' {..} =
    _salt `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` websiteConfiguration

instance Prelude.NFData PutBucketWebsite where
  rnf PutBucketWebsite' {..} =
    Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf websiteConfiguration

instance Core.ToElement PutBucketWebsite where
  toElement PutBucketWebsite' {..} =
    Core.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}WebsiteConfiguration"
      websiteConfiguration

instance Core.ToHeaders PutBucketWebsite where
  toHeaders PutBucketWebsite' {..} =
    Prelude.mconcat
      [ "Content-MD5" Core.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath PutBucketWebsite where
  toPath PutBucketWebsite' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery PutBucketWebsite where
  toQuery = Prelude.const (Prelude.mconcat ["website"])

-- | /See:/ 'newPutBucketWebsiteResponse' smart constructor.
data PutBucketWebsiteResponse = PutBucketWebsiteResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketWebsiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketWebsiteResponse ::
  PutBucketWebsiteResponse
newPutBucketWebsiteResponse =
  PutBucketWebsiteResponse'

instance Prelude.NFData PutBucketWebsiteResponse where
  rnf _ = ()
