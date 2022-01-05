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
-- Module      : Amazonka.S3.PutBucketOwnershipControls
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or modifies @OwnershipControls@ for an Amazon S3 bucket. To use
-- this operation, you must have the @s3:PutBucketOwnershipControls@
-- permission. For more information about Amazon S3 permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy>.
--
-- For information about Amazon S3 Object Ownership, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/about-object-ownership.html Using Object Ownership>.
--
-- The following operations are related to @PutBucketOwnershipControls@:
--
-- -   GetBucketOwnershipControls
--
-- -   DeleteBucketOwnershipControls
module Amazonka.S3.PutBucketOwnershipControls
  ( -- * Creating a Request
    PutBucketOwnershipControls (..),
    newPutBucketOwnershipControls,

    -- * Request Lenses
    putBucketOwnershipControls_contentMD5,
    putBucketOwnershipControls_expectedBucketOwner,
    putBucketOwnershipControls_bucket,
    putBucketOwnershipControls_ownershipControls,

    -- * Destructuring the Response
    PutBucketOwnershipControlsResponse (..),
    newPutBucketOwnershipControlsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketOwnershipControls' smart constructor.
data PutBucketOwnershipControls = PutBucketOwnershipControls'
  { -- | The MD5 hash of the @OwnershipControls@ request body.
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to
    -- set.
    bucket :: BucketName,
    -- | The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) that you
    -- want to apply to this Amazon S3 bucket.
    ownershipControls :: OwnershipControls
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketOwnershipControls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentMD5', 'putBucketOwnershipControls_contentMD5' - The MD5 hash of the @OwnershipControls@ request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putBucketOwnershipControls_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'putBucketOwnershipControls_bucket' - The name of the Amazon S3 bucket whose @OwnershipControls@ you want to
-- set.
--
-- 'ownershipControls', 'putBucketOwnershipControls_ownershipControls' - The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) that you
-- want to apply to this Amazon S3 bucket.
newPutBucketOwnershipControls ::
  -- | 'bucket'
  BucketName ->
  -- | 'ownershipControls'
  OwnershipControls ->
  PutBucketOwnershipControls
newPutBucketOwnershipControls
  pBucket_
  pOwnershipControls_ =
    PutBucketOwnershipControls'
      { contentMD5 =
          Prelude.Nothing,
        expectedBucketOwner = Prelude.Nothing,
        bucket = pBucket_,
        ownershipControls = pOwnershipControls_
      }

-- | The MD5 hash of the @OwnershipControls@ request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putBucketOwnershipControls_contentMD5 :: Lens.Lens' PutBucketOwnershipControls (Prelude.Maybe Prelude.Text)
putBucketOwnershipControls_contentMD5 = Lens.lens (\PutBucketOwnershipControls' {contentMD5} -> contentMD5) (\s@PutBucketOwnershipControls' {} a -> s {contentMD5 = a} :: PutBucketOwnershipControls)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putBucketOwnershipControls_expectedBucketOwner :: Lens.Lens' PutBucketOwnershipControls (Prelude.Maybe Prelude.Text)
putBucketOwnershipControls_expectedBucketOwner = Lens.lens (\PutBucketOwnershipControls' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketOwnershipControls' {} a -> s {expectedBucketOwner = a} :: PutBucketOwnershipControls)

-- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to
-- set.
putBucketOwnershipControls_bucket :: Lens.Lens' PutBucketOwnershipControls BucketName
putBucketOwnershipControls_bucket = Lens.lens (\PutBucketOwnershipControls' {bucket} -> bucket) (\s@PutBucketOwnershipControls' {} a -> s {bucket = a} :: PutBucketOwnershipControls)

-- | The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) that you
-- want to apply to this Amazon S3 bucket.
putBucketOwnershipControls_ownershipControls :: Lens.Lens' PutBucketOwnershipControls OwnershipControls
putBucketOwnershipControls_ownershipControls = Lens.lens (\PutBucketOwnershipControls' {ownershipControls} -> ownershipControls) (\s@PutBucketOwnershipControls' {} a -> s {ownershipControls = a} :: PutBucketOwnershipControls)

instance Core.AWSRequest PutBucketOwnershipControls where
  type
    AWSResponse PutBucketOwnershipControls =
      PutBucketOwnershipControlsResponse
  request =
    Request.s3vhost
      Prelude.. Request.putXML defaultService
  response =
    Response.receiveNull
      PutBucketOwnershipControlsResponse'

instance Prelude.Hashable PutBucketOwnershipControls where
  hashWithSalt _salt PutBucketOwnershipControls' {..} =
    _salt `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` ownershipControls

instance Prelude.NFData PutBucketOwnershipControls where
  rnf PutBucketOwnershipControls' {..} =
    Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf ownershipControls

instance Core.ToElement PutBucketOwnershipControls where
  toElement PutBucketOwnershipControls' {..} =
    Core.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}OwnershipControls"
      ownershipControls

instance Core.ToHeaders PutBucketOwnershipControls where
  toHeaders PutBucketOwnershipControls' {..} =
    Prelude.mconcat
      [ "Content-MD5" Core.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath PutBucketOwnershipControls where
  toPath PutBucketOwnershipControls' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery PutBucketOwnershipControls where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["ownershipControls"])

-- | /See:/ 'newPutBucketOwnershipControlsResponse' smart constructor.
data PutBucketOwnershipControlsResponse = PutBucketOwnershipControlsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketOwnershipControlsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketOwnershipControlsResponse ::
  PutBucketOwnershipControlsResponse
newPutBucketOwnershipControlsResponse =
  PutBucketOwnershipControlsResponse'

instance
  Prelude.NFData
    PutBucketOwnershipControlsResponse
  where
  rnf _ = ()
