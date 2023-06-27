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
-- Module      : Amazonka.S3.PutBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets an analytics configuration for the bucket (specified by the
-- analytics configuration ID). You can have up to 1,000 analytics
-- configurations per bucket.
--
-- You can choose to have storage class analysis export analysis reports
-- sent to a comma-separated values (CSV) flat file. See the @DataExport@
-- request element. Reports are updated daily and are based on the object
-- filters that you configure. When selecting data export, you specify a
-- destination bucket and an optional destination prefix where the file is
-- written. You can export the data to a destination bucket in a different
-- account. However, the destination bucket must be in the same Region as
-- the bucket that you are making the PUT analytics configuration to. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis>.
--
-- You must create a bucket policy on the destination bucket where the
-- exported file is written to grant permissions to Amazon S3 to write
-- objects to the bucket. For an example policy, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/example-bucket-policies.html#example-bucket-policies-use-case-9 Granting Permissions for Amazon S3 Inventory and Storage Class Analysis>.
--
-- To use this operation, you must have permissions to perform the
-- @s3:PutAnalyticsConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- @PutBucketAnalyticsConfiguration@ has the following special errors:
--
-- -   -   /HTTP Error: HTTP 400 Bad Request/
--
--     -   /Code: InvalidArgument/
--
--     -   /Cause: Invalid argument./
--
-- -   -   /HTTP Error: HTTP 400 Bad Request/
--
--     -   /Code: TooManyConfigurations/
--
--     -   /Cause: You are attempting to create a new configuration but
--         have already reached the 1,000-configuration limit./
--
-- -   -   /HTTP Error: HTTP 403 Forbidden/
--
--     -   /Code: AccessDenied/
--
--     -   /Cause: You are not the owner of the specified bucket, or you do
--         not have the s3:PutAnalyticsConfiguration bucket permission to
--         set the configuration on the bucket./
--
-- The following operations are related to
-- @PutBucketAnalyticsConfiguration@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAnalyticsConfiguration.html GetBucketAnalyticsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketAnalyticsConfiguration.html DeleteBucketAnalyticsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketAnalyticsConfigurations.html ListBucketAnalyticsConfigurations>
module Amazonka.S3.PutBucketAnalyticsConfiguration
  ( -- * Creating a Request
    PutBucketAnalyticsConfiguration (..),
    newPutBucketAnalyticsConfiguration,

    -- * Request Lenses
    putBucketAnalyticsConfiguration_expectedBucketOwner,
    putBucketAnalyticsConfiguration_bucket,
    putBucketAnalyticsConfiguration_id,
    putBucketAnalyticsConfiguration_analyticsConfiguration,

    -- * Destructuring the Response
    PutBucketAnalyticsConfigurationResponse (..),
    newPutBucketAnalyticsConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketAnalyticsConfiguration' smart constructor.
data PutBucketAnalyticsConfiguration = PutBucketAnalyticsConfiguration'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket to which an analytics configuration is stored.
    bucket :: BucketName,
    -- | The ID that identifies the analytics configuration.
    id :: Prelude.Text,
    -- | The configuration and any analyses for the analytics filter.
    analyticsConfiguration :: AnalyticsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketAnalyticsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putBucketAnalyticsConfiguration_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'putBucketAnalyticsConfiguration_bucket' - The name of the bucket to which an analytics configuration is stored.
--
-- 'id', 'putBucketAnalyticsConfiguration_id' - The ID that identifies the analytics configuration.
--
-- 'analyticsConfiguration', 'putBucketAnalyticsConfiguration_analyticsConfiguration' - The configuration and any analyses for the analytics filter.
newPutBucketAnalyticsConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Prelude.Text ->
  -- | 'analyticsConfiguration'
  AnalyticsConfiguration ->
  PutBucketAnalyticsConfiguration
newPutBucketAnalyticsConfiguration
  pBucket_
  pId_
  pAnalyticsConfiguration_ =
    PutBucketAnalyticsConfiguration'
      { expectedBucketOwner =
          Prelude.Nothing,
        bucket = pBucket_,
        id = pId_,
        analyticsConfiguration =
          pAnalyticsConfiguration_
      }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putBucketAnalyticsConfiguration_expectedBucketOwner :: Lens.Lens' PutBucketAnalyticsConfiguration (Prelude.Maybe Prelude.Text)
putBucketAnalyticsConfiguration_expectedBucketOwner = Lens.lens (\PutBucketAnalyticsConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketAnalyticsConfiguration' {} a -> s {expectedBucketOwner = a} :: PutBucketAnalyticsConfiguration)

-- | The name of the bucket to which an analytics configuration is stored.
putBucketAnalyticsConfiguration_bucket :: Lens.Lens' PutBucketAnalyticsConfiguration BucketName
putBucketAnalyticsConfiguration_bucket = Lens.lens (\PutBucketAnalyticsConfiguration' {bucket} -> bucket) (\s@PutBucketAnalyticsConfiguration' {} a -> s {bucket = a} :: PutBucketAnalyticsConfiguration)

-- | The ID that identifies the analytics configuration.
putBucketAnalyticsConfiguration_id :: Lens.Lens' PutBucketAnalyticsConfiguration Prelude.Text
putBucketAnalyticsConfiguration_id = Lens.lens (\PutBucketAnalyticsConfiguration' {id} -> id) (\s@PutBucketAnalyticsConfiguration' {} a -> s {id = a} :: PutBucketAnalyticsConfiguration)

-- | The configuration and any analyses for the analytics filter.
putBucketAnalyticsConfiguration_analyticsConfiguration :: Lens.Lens' PutBucketAnalyticsConfiguration AnalyticsConfiguration
putBucketAnalyticsConfiguration_analyticsConfiguration = Lens.lens (\PutBucketAnalyticsConfiguration' {analyticsConfiguration} -> analyticsConfiguration) (\s@PutBucketAnalyticsConfiguration' {} a -> s {analyticsConfiguration = a} :: PutBucketAnalyticsConfiguration)

instance
  Core.AWSRequest
    PutBucketAnalyticsConfiguration
  where
  type
    AWSResponse PutBucketAnalyticsConfiguration =
      PutBucketAnalyticsConfigurationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveNull
      PutBucketAnalyticsConfigurationResponse'

instance
  Prelude.Hashable
    PutBucketAnalyticsConfiguration
  where
  hashWithSalt
    _salt
    PutBucketAnalyticsConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` expectedBucketOwner
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` analyticsConfiguration

instance
  Prelude.NFData
    PutBucketAnalyticsConfiguration
  where
  rnf PutBucketAnalyticsConfiguration' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf analyticsConfiguration

instance
  Data.ToElement
    PutBucketAnalyticsConfiguration
  where
  toElement PutBucketAnalyticsConfiguration' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}AnalyticsConfiguration"
      analyticsConfiguration

instance
  Data.ToHeaders
    PutBucketAnalyticsConfiguration
  where
  toHeaders PutBucketAnalyticsConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath PutBucketAnalyticsConfiguration where
  toPath PutBucketAnalyticsConfiguration' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery PutBucketAnalyticsConfiguration where
  toQuery PutBucketAnalyticsConfiguration' {..} =
    Prelude.mconcat ["id" Data.=: id, "analytics"]

-- | /See:/ 'newPutBucketAnalyticsConfigurationResponse' smart constructor.
data PutBucketAnalyticsConfigurationResponse = PutBucketAnalyticsConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketAnalyticsConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketAnalyticsConfigurationResponse ::
  PutBucketAnalyticsConfigurationResponse
newPutBucketAnalyticsConfigurationResponse =
  PutBucketAnalyticsConfigurationResponse'

instance
  Prelude.NFData
    PutBucketAnalyticsConfigurationResponse
  where
  rnf _ = ()
