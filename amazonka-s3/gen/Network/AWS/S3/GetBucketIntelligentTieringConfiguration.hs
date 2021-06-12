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
-- Module      : Network.AWS.S3.GetBucketIntelligentTieringConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the S3 Intelligent-Tiering configuration from the specified bucket.
--
-- The S3 Intelligent-Tiering storage class is designed to optimize storage
-- costs by automatically moving data to the most cost-effective storage
-- access tier, without additional operational overhead. S3
-- Intelligent-Tiering delivers automatic cost savings by moving data
-- between access tiers, when access patterns change.
--
-- The S3 Intelligent-Tiering storage class is suitable for objects larger
-- than 128 KB that you plan to store for at least 30 days. If the size of
-- an object is less than 128 KB, it is not eligible for auto-tiering.
-- Smaller objects can be stored, but they are always charged at the
-- frequent access tier rates in the S3 Intelligent-Tiering storage class.
--
-- If you delete an object before the end of the 30-day minimum storage
-- duration period, you are charged for 30 days. For more information, see
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
module Network.AWS.S3.GetBucketIntelligentTieringConfiguration
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketIntelligentTieringConfiguration' smart constructor.
data GetBucketIntelligentTieringConfiguration = GetBucketIntelligentTieringConfiguration'
  { -- | The name of the Amazon S3 bucket whose configuration you want to modify
    -- or retrieve.
    bucket :: BucketName,
    -- | The ID used to identify the S3 Intelligent-Tiering configuration.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
getBucketIntelligentTieringConfiguration_id :: Lens.Lens' GetBucketIntelligentTieringConfiguration Core.Text
getBucketIntelligentTieringConfiguration_id = Lens.lens (\GetBucketIntelligentTieringConfiguration' {id} -> id) (\s@GetBucketIntelligentTieringConfiguration' {} a -> s {id = a} :: GetBucketIntelligentTieringConfiguration)

instance
  Core.AWSRequest
    GetBucketIntelligentTieringConfiguration
  where
  type
    AWSResponse
      GetBucketIntelligentTieringConfiguration =
      GetBucketIntelligentTieringConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketIntelligentTieringConfigurationResponse'
            Core.<$> (Core.parseXML x)
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetBucketIntelligentTieringConfiguration

instance
  Core.NFData
    GetBucketIntelligentTieringConfiguration

instance
  Core.ToHeaders
    GetBucketIntelligentTieringConfiguration
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetBucketIntelligentTieringConfiguration
  where
  toPath GetBucketIntelligentTieringConfiguration' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance
  Core.ToQuery
    GetBucketIntelligentTieringConfiguration
  where
  toQuery GetBucketIntelligentTieringConfiguration' {..} =
    Core.mconcat
      ["id" Core.=: id, "intelligent-tiering"]

-- | /See:/ 'newGetBucketIntelligentTieringConfigurationResponse' smart constructor.
data GetBucketIntelligentTieringConfigurationResponse = GetBucketIntelligentTieringConfigurationResponse'
  { -- | Container for S3 Intelligent-Tiering configuration.
    intelligentTieringConfiguration :: Core.Maybe IntelligentTieringConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetBucketIntelligentTieringConfigurationResponse
newGetBucketIntelligentTieringConfigurationResponse
  pHttpStatus_ =
    GetBucketIntelligentTieringConfigurationResponse'
      { intelligentTieringConfiguration =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Container for S3 Intelligent-Tiering configuration.
getBucketIntelligentTieringConfigurationResponse_intelligentTieringConfiguration :: Lens.Lens' GetBucketIntelligentTieringConfigurationResponse (Core.Maybe IntelligentTieringConfiguration)
getBucketIntelligentTieringConfigurationResponse_intelligentTieringConfiguration = Lens.lens (\GetBucketIntelligentTieringConfigurationResponse' {intelligentTieringConfiguration} -> intelligentTieringConfiguration) (\s@GetBucketIntelligentTieringConfigurationResponse' {} a -> s {intelligentTieringConfiguration = a} :: GetBucketIntelligentTieringConfigurationResponse)

-- | The response's http status code.
getBucketIntelligentTieringConfigurationResponse_httpStatus :: Lens.Lens' GetBucketIntelligentTieringConfigurationResponse Core.Int
getBucketIntelligentTieringConfigurationResponse_httpStatus = Lens.lens (\GetBucketIntelligentTieringConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetBucketIntelligentTieringConfigurationResponse' {} a -> s {httpStatus = a} :: GetBucketIntelligentTieringConfigurationResponse)

instance
  Core.NFData
    GetBucketIntelligentTieringConfigurationResponse
