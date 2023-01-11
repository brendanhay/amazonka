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
-- Module      : Amazonka.S3.GetBucketIntelligentTieringConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the S3 Intelligent-Tiering configuration from the specified bucket.
--
-- The S3 Intelligent-Tiering storage class is designed to optimize storage
-- costs by automatically moving data to the most cost-effective storage
-- access tier, without performance impact or operational overhead. S3
-- Intelligent-Tiering delivers automatic cost savings in three low latency
-- and high throughput access tiers. To get the lowest storage cost on data
-- that can be accessed in minutes to hours, you can choose to activate
-- additional archiving capabilities.
--
-- The S3 Intelligent-Tiering storage class is the ideal storage class for
-- data with unknown, changing, or unpredictable access patterns,
-- independent of object size or retention period. If the size of an object
-- is less than 128 KB, it is not monitored and not eligible for
-- auto-tiering. Smaller objects can be stored, but they are always charged
-- at the Frequent Access tier rates in the S3 Intelligent-Tiering storage
-- class.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects>.
--
-- Operations related to @GetBucketIntelligentTieringConfiguration@
-- include:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketIntelligentTieringConfiguration.html DeleteBucketIntelligentTieringConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketIntelligentTieringConfiguration.html PutBucketIntelligentTieringConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketIntelligentTieringConfigurations.html ListBucketIntelligentTieringConfigurations>
module Amazonka.S3.GetBucketIntelligentTieringConfiguration
  ( -- * Creating a Request
    GetBucketIntelligentTieringConfiguration (..),
    newGetBucketIntelligentTieringConfiguration,

    -- * Request Lenses
    getBucketIntelligentTieringConfiguration_bucket,
    getBucketIntelligentTieringConfiguration_id,

    -- * Destructuring the Response
    GetBucketIntelligentTieringConfigurationResponse (..),
    newGetBucketIntelligentTieringConfigurationResponse,

    -- * Response Lenses
    getBucketIntelligentTieringConfigurationResponse_intelligentTieringConfiguration,
    getBucketIntelligentTieringConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketIntelligentTieringConfiguration' smart constructor.
data GetBucketIntelligentTieringConfiguration = GetBucketIntelligentTieringConfiguration'
  { -- | The name of the Amazon S3 bucket whose configuration you want to modify
    -- or retrieve.
    bucket :: BucketName,
    -- | The ID used to identify the S3 Intelligent-Tiering configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketIntelligentTieringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'getBucketIntelligentTieringConfiguration_bucket' - The name of the Amazon S3 bucket whose configuration you want to modify
-- or retrieve.
--
-- 'id', 'getBucketIntelligentTieringConfiguration_id' - The ID used to identify the S3 Intelligent-Tiering configuration.
newGetBucketIntelligentTieringConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Prelude.Text ->
  GetBucketIntelligentTieringConfiguration
newGetBucketIntelligentTieringConfiguration
  pBucket_
  pId_ =
    GetBucketIntelligentTieringConfiguration'
      { bucket =
          pBucket_,
        id = pId_
      }

-- | The name of the Amazon S3 bucket whose configuration you want to modify
-- or retrieve.
getBucketIntelligentTieringConfiguration_bucket :: Lens.Lens' GetBucketIntelligentTieringConfiguration BucketName
getBucketIntelligentTieringConfiguration_bucket = Lens.lens (\GetBucketIntelligentTieringConfiguration' {bucket} -> bucket) (\s@GetBucketIntelligentTieringConfiguration' {} a -> s {bucket = a} :: GetBucketIntelligentTieringConfiguration)

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
getBucketIntelligentTieringConfiguration_id :: Lens.Lens' GetBucketIntelligentTieringConfiguration Prelude.Text
getBucketIntelligentTieringConfiguration_id = Lens.lens (\GetBucketIntelligentTieringConfiguration' {id} -> id) (\s@GetBucketIntelligentTieringConfiguration' {} a -> s {id = a} :: GetBucketIntelligentTieringConfiguration)

instance
  Core.AWSRequest
    GetBucketIntelligentTieringConfiguration
  where
  type
    AWSResponse
      GetBucketIntelligentTieringConfiguration =
      GetBucketIntelligentTieringConfigurationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketIntelligentTieringConfigurationResponse'
            Prelude.<$> (Data.parseXML x)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetBucketIntelligentTieringConfiguration
  where
  hashWithSalt
    _salt
    GetBucketIntelligentTieringConfiguration' {..} =
      _salt `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    GetBucketIntelligentTieringConfiguration
  where
  rnf GetBucketIntelligentTieringConfiguration' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    GetBucketIntelligentTieringConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetBucketIntelligentTieringConfiguration
  where
  toPath GetBucketIntelligentTieringConfiguration' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance
  Data.ToQuery
    GetBucketIntelligentTieringConfiguration
  where
  toQuery GetBucketIntelligentTieringConfiguration' {..} =
    Prelude.mconcat
      ["id" Data.=: id, "intelligent-tiering"]

-- | /See:/ 'newGetBucketIntelligentTieringConfigurationResponse' smart constructor.
data GetBucketIntelligentTieringConfigurationResponse = GetBucketIntelligentTieringConfigurationResponse'
  { -- | Container for S3 Intelligent-Tiering configuration.
    intelligentTieringConfiguration :: Prelude.Maybe IntelligentTieringConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketIntelligentTieringConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intelligentTieringConfiguration', 'getBucketIntelligentTieringConfigurationResponse_intelligentTieringConfiguration' - Container for S3 Intelligent-Tiering configuration.
--
-- 'httpStatus', 'getBucketIntelligentTieringConfigurationResponse_httpStatus' - The response's http status code.
newGetBucketIntelligentTieringConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketIntelligentTieringConfigurationResponse
newGetBucketIntelligentTieringConfigurationResponse
  pHttpStatus_ =
    GetBucketIntelligentTieringConfigurationResponse'
      { intelligentTieringConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Container for S3 Intelligent-Tiering configuration.
getBucketIntelligentTieringConfigurationResponse_intelligentTieringConfiguration :: Lens.Lens' GetBucketIntelligentTieringConfigurationResponse (Prelude.Maybe IntelligentTieringConfiguration)
getBucketIntelligentTieringConfigurationResponse_intelligentTieringConfiguration = Lens.lens (\GetBucketIntelligentTieringConfigurationResponse' {intelligentTieringConfiguration} -> intelligentTieringConfiguration) (\s@GetBucketIntelligentTieringConfigurationResponse' {} a -> s {intelligentTieringConfiguration = a} :: GetBucketIntelligentTieringConfigurationResponse)

-- | The response's http status code.
getBucketIntelligentTieringConfigurationResponse_httpStatus :: Lens.Lens' GetBucketIntelligentTieringConfigurationResponse Prelude.Int
getBucketIntelligentTieringConfigurationResponse_httpStatus = Lens.lens (\GetBucketIntelligentTieringConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetBucketIntelligentTieringConfigurationResponse' {} a -> s {httpStatus = a} :: GetBucketIntelligentTieringConfigurationResponse)

instance
  Prelude.NFData
    GetBucketIntelligentTieringConfigurationResponse
  where
  rnf
    GetBucketIntelligentTieringConfigurationResponse' {..} =
      Prelude.rnf intelligentTieringConfiguration
        `Prelude.seq` Prelude.rnf httpStatus
