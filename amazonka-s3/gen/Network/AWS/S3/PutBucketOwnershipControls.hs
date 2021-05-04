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
-- Module      : Network.AWS.S3.PutBucketOwnershipControls
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
module Network.AWS.S3.PutBucketOwnershipControls
  ( -- * Creating a Request
    PutBucketOwnershipControls (..),
    newPutBucketOwnershipControls,

    -- * Request Lenses
    putBucketOwnershipControls_expectedBucketOwner,
    putBucketOwnershipControls_contentMD5,
    putBucketOwnershipControls_bucket,
    putBucketOwnershipControls_ownershipControls,

    -- * Destructuring the Response
    PutBucketOwnershipControlsResponse (..),
    newPutBucketOwnershipControlsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutBucketOwnershipControls' smart constructor.
data PutBucketOwnershipControls = PutBucketOwnershipControls'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The MD5 hash of the @OwnershipControls@ request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS
    -- SDKs, this field is calculated automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to
    -- set.
    bucket :: BucketName,
    -- | The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) that you
    -- want to apply to this Amazon S3 bucket.
    ownershipControls :: OwnershipControls
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBucketOwnershipControls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putBucketOwnershipControls_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'contentMD5', 'putBucketOwnershipControls_contentMD5' - The MD5 hash of the @OwnershipControls@ request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
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
      { expectedBucketOwner =
          Prelude.Nothing,
        contentMD5 = Prelude.Nothing,
        bucket = pBucket_,
        ownershipControls = pOwnershipControls_
      }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putBucketOwnershipControls_expectedBucketOwner :: Lens.Lens' PutBucketOwnershipControls (Prelude.Maybe Prelude.Text)
putBucketOwnershipControls_expectedBucketOwner = Lens.lens (\PutBucketOwnershipControls' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketOwnershipControls' {} a -> s {expectedBucketOwner = a} :: PutBucketOwnershipControls)

-- | The MD5 hash of the @OwnershipControls@ request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
putBucketOwnershipControls_contentMD5 :: Lens.Lens' PutBucketOwnershipControls (Prelude.Maybe Prelude.Text)
putBucketOwnershipControls_contentMD5 = Lens.lens (\PutBucketOwnershipControls' {contentMD5} -> contentMD5) (\s@PutBucketOwnershipControls' {} a -> s {contentMD5 = a} :: PutBucketOwnershipControls)

-- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to
-- set.
putBucketOwnershipControls_bucket :: Lens.Lens' PutBucketOwnershipControls BucketName
putBucketOwnershipControls_bucket = Lens.lens (\PutBucketOwnershipControls' {bucket} -> bucket) (\s@PutBucketOwnershipControls' {} a -> s {bucket = a} :: PutBucketOwnershipControls)

-- | The @OwnershipControls@ (BucketOwnerPreferred or ObjectWriter) that you
-- want to apply to this Amazon S3 bucket.
putBucketOwnershipControls_ownershipControls :: Lens.Lens' PutBucketOwnershipControls OwnershipControls
putBucketOwnershipControls_ownershipControls = Lens.lens (\PutBucketOwnershipControls' {ownershipControls} -> ownershipControls) (\s@PutBucketOwnershipControls' {} a -> s {ownershipControls = a} :: PutBucketOwnershipControls)

instance
  Prelude.AWSRequest
    PutBucketOwnershipControls
  where
  type
    Rs PutBucketOwnershipControls =
      PutBucketOwnershipControlsResponse
  request = Request.putXML defaultService
  response =
    Response.receiveNull
      PutBucketOwnershipControlsResponse'

instance Prelude.Hashable PutBucketOwnershipControls

instance Prelude.NFData PutBucketOwnershipControls

instance Prelude.ToElement PutBucketOwnershipControls where
  toElement PutBucketOwnershipControls' {..} =
    Prelude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}OwnershipControls"
      ownershipControls

instance Prelude.ToHeaders PutBucketOwnershipControls where
  toHeaders PutBucketOwnershipControls' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner,
        "Content-MD5" Prelude.=# contentMD5
      ]

instance Prelude.ToPath PutBucketOwnershipControls where
  toPath PutBucketOwnershipControls' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery PutBucketOwnershipControls where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["ownershipControls"])

-- | /See:/ 'newPutBucketOwnershipControlsResponse' smart constructor.
data PutBucketOwnershipControlsResponse = PutBucketOwnershipControlsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
