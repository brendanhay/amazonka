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
-- Module      : Amazonka.S3.PutBucketMetricsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets a metrics configuration (specified by the metrics configuration ID)
-- for the bucket. You can have up to 1,000 metrics configurations per
-- bucket. If you\'re updating an existing metrics configuration, note that
-- this is a full replacement of the existing metrics configuration. If you
-- don\'t include the elements you want to keep, they are erased.
--
-- To use this operation, you must have permissions to perform the
-- @s3:PutMetricsConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- For information about CloudWatch request metrics for Amazon S3, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch>.
--
-- The following operations are related to @PutBucketMetricsConfiguration@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketMetricsConfiguration.html DeleteBucketMetricsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketMetricsConfiguration.html GetBucketMetricsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketMetricsConfigurations.html ListBucketMetricsConfigurations>
--
-- @GetBucketLifecycle@ has the following special error:
--
-- -   Error code: @TooManyConfigurations@
--
--     -   Description: You are attempting to create a new configuration
--         but have already reached the 1,000-configuration limit.
--
--     -   HTTP Status Code: HTTP 400 Bad Request
module Amazonka.S3.PutBucketMetricsConfiguration
  ( -- * Creating a Request
    PutBucketMetricsConfiguration (..),
    newPutBucketMetricsConfiguration,

    -- * Request Lenses
    putBucketMetricsConfiguration_expectedBucketOwner,
    putBucketMetricsConfiguration_bucket,
    putBucketMetricsConfiguration_id,
    putBucketMetricsConfiguration_metricsConfiguration,

    -- * Destructuring the Response
    PutBucketMetricsConfigurationResponse (..),
    newPutBucketMetricsConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketMetricsConfiguration' smart constructor.
data PutBucketMetricsConfiguration = PutBucketMetricsConfiguration'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket for which the metrics configuration is set.
    bucket :: BucketName,
    -- | The ID used to identify the metrics configuration.
    id :: Prelude.Text,
    -- | Specifies the metrics configuration.
    metricsConfiguration :: MetricsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketMetricsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putBucketMetricsConfiguration_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'putBucketMetricsConfiguration_bucket' - The name of the bucket for which the metrics configuration is set.
--
-- 'id', 'putBucketMetricsConfiguration_id' - The ID used to identify the metrics configuration.
--
-- 'metricsConfiguration', 'putBucketMetricsConfiguration_metricsConfiguration' - Specifies the metrics configuration.
newPutBucketMetricsConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Prelude.Text ->
  -- | 'metricsConfiguration'
  MetricsConfiguration ->
  PutBucketMetricsConfiguration
newPutBucketMetricsConfiguration
  pBucket_
  pId_
  pMetricsConfiguration_ =
    PutBucketMetricsConfiguration'
      { expectedBucketOwner =
          Prelude.Nothing,
        bucket = pBucket_,
        id = pId_,
        metricsConfiguration =
          pMetricsConfiguration_
      }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putBucketMetricsConfiguration_expectedBucketOwner :: Lens.Lens' PutBucketMetricsConfiguration (Prelude.Maybe Prelude.Text)
putBucketMetricsConfiguration_expectedBucketOwner = Lens.lens (\PutBucketMetricsConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketMetricsConfiguration' {} a -> s {expectedBucketOwner = a} :: PutBucketMetricsConfiguration)

-- | The name of the bucket for which the metrics configuration is set.
putBucketMetricsConfiguration_bucket :: Lens.Lens' PutBucketMetricsConfiguration BucketName
putBucketMetricsConfiguration_bucket = Lens.lens (\PutBucketMetricsConfiguration' {bucket} -> bucket) (\s@PutBucketMetricsConfiguration' {} a -> s {bucket = a} :: PutBucketMetricsConfiguration)

-- | The ID used to identify the metrics configuration.
putBucketMetricsConfiguration_id :: Lens.Lens' PutBucketMetricsConfiguration Prelude.Text
putBucketMetricsConfiguration_id = Lens.lens (\PutBucketMetricsConfiguration' {id} -> id) (\s@PutBucketMetricsConfiguration' {} a -> s {id = a} :: PutBucketMetricsConfiguration)

-- | Specifies the metrics configuration.
putBucketMetricsConfiguration_metricsConfiguration :: Lens.Lens' PutBucketMetricsConfiguration MetricsConfiguration
putBucketMetricsConfiguration_metricsConfiguration = Lens.lens (\PutBucketMetricsConfiguration' {metricsConfiguration} -> metricsConfiguration) (\s@PutBucketMetricsConfiguration' {} a -> s {metricsConfiguration = a} :: PutBucketMetricsConfiguration)

instance
  Core.AWSRequest
    PutBucketMetricsConfiguration
  where
  type
    AWSResponse PutBucketMetricsConfiguration =
      PutBucketMetricsConfigurationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveNull
      PutBucketMetricsConfigurationResponse'

instance
  Prelude.Hashable
    PutBucketMetricsConfiguration
  where
  hashWithSalt _salt PutBucketMetricsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` metricsConfiguration

instance Prelude.NFData PutBucketMetricsConfiguration where
  rnf PutBucketMetricsConfiguration' {..} =
    Prelude.rnf expectedBucketOwner `Prelude.seq`
      Prelude.rnf bucket `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf metricsConfiguration

instance Data.ToElement PutBucketMetricsConfiguration where
  toElement PutBucketMetricsConfiguration' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}MetricsConfiguration"
      metricsConfiguration

instance Data.ToHeaders PutBucketMetricsConfiguration where
  toHeaders PutBucketMetricsConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath PutBucketMetricsConfiguration where
  toPath PutBucketMetricsConfiguration' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery PutBucketMetricsConfiguration where
  toQuery PutBucketMetricsConfiguration' {..} =
    Prelude.mconcat ["id" Data.=: id, "metrics"]

-- | /See:/ 'newPutBucketMetricsConfigurationResponse' smart constructor.
data PutBucketMetricsConfigurationResponse = PutBucketMetricsConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketMetricsConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketMetricsConfigurationResponse ::
  PutBucketMetricsConfigurationResponse
newPutBucketMetricsConfigurationResponse =
  PutBucketMetricsConfigurationResponse'

instance
  Prelude.NFData
    PutBucketMetricsConfigurationResponse
  where
  rnf _ = ()
