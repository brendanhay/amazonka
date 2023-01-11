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
-- Module      : Amazonka.S3.DeleteBucketIntelligentTieringConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the S3 Intelligent-Tiering configuration from the specified
-- bucket.
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
-- Operations related to @DeleteBucketIntelligentTieringConfiguration@
-- include:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketIntelligentTieringConfiguration.html GetBucketIntelligentTieringConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketIntelligentTieringConfiguration.html PutBucketIntelligentTieringConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketIntelligentTieringConfigurations.html ListBucketIntelligentTieringConfigurations>
module Amazonka.S3.DeleteBucketIntelligentTieringConfiguration
  ( -- * Creating a Request
    DeleteBucketIntelligentTieringConfiguration (..),
    newDeleteBucketIntelligentTieringConfiguration,

    -- * Request Lenses
    deleteBucketIntelligentTieringConfiguration_bucket,
    deleteBucketIntelligentTieringConfiguration_id,

    -- * Destructuring the Response
    DeleteBucketIntelligentTieringConfigurationResponse (..),
    newDeleteBucketIntelligentTieringConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newDeleteBucketIntelligentTieringConfiguration' smart constructor.
data DeleteBucketIntelligentTieringConfiguration = DeleteBucketIntelligentTieringConfiguration'
  { -- | The name of the Amazon S3 bucket whose configuration you want to modify
    -- or retrieve.
    bucket :: BucketName,
    -- | The ID used to identify the S3 Intelligent-Tiering configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketIntelligentTieringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'deleteBucketIntelligentTieringConfiguration_bucket' - The name of the Amazon S3 bucket whose configuration you want to modify
-- or retrieve.
--
-- 'id', 'deleteBucketIntelligentTieringConfiguration_id' - The ID used to identify the S3 Intelligent-Tiering configuration.
newDeleteBucketIntelligentTieringConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Prelude.Text ->
  DeleteBucketIntelligentTieringConfiguration
newDeleteBucketIntelligentTieringConfiguration
  pBucket_
  pId_ =
    DeleteBucketIntelligentTieringConfiguration'
      { bucket =
          pBucket_,
        id = pId_
      }

-- | The name of the Amazon S3 bucket whose configuration you want to modify
-- or retrieve.
deleteBucketIntelligentTieringConfiguration_bucket :: Lens.Lens' DeleteBucketIntelligentTieringConfiguration BucketName
deleteBucketIntelligentTieringConfiguration_bucket = Lens.lens (\DeleteBucketIntelligentTieringConfiguration' {bucket} -> bucket) (\s@DeleteBucketIntelligentTieringConfiguration' {} a -> s {bucket = a} :: DeleteBucketIntelligentTieringConfiguration)

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
deleteBucketIntelligentTieringConfiguration_id :: Lens.Lens' DeleteBucketIntelligentTieringConfiguration Prelude.Text
deleteBucketIntelligentTieringConfiguration_id = Lens.lens (\DeleteBucketIntelligentTieringConfiguration' {id} -> id) (\s@DeleteBucketIntelligentTieringConfiguration' {} a -> s {id = a} :: DeleteBucketIntelligentTieringConfiguration)

instance
  Core.AWSRequest
    DeleteBucketIntelligentTieringConfiguration
  where
  type
    AWSResponse
      DeleteBucketIntelligentTieringConfiguration =
      DeleteBucketIntelligentTieringConfigurationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteBucketIntelligentTieringConfigurationResponse'

instance
  Prelude.Hashable
    DeleteBucketIntelligentTieringConfiguration
  where
  hashWithSalt
    _salt
    DeleteBucketIntelligentTieringConfiguration' {..} =
      _salt `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DeleteBucketIntelligentTieringConfiguration
  where
  rnf DeleteBucketIntelligentTieringConfiguration' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    DeleteBucketIntelligentTieringConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteBucketIntelligentTieringConfiguration
  where
  toPath
    DeleteBucketIntelligentTieringConfiguration' {..} =
      Prelude.mconcat ["/", Data.toBS bucket]

instance
  Data.ToQuery
    DeleteBucketIntelligentTieringConfiguration
  where
  toQuery
    DeleteBucketIntelligentTieringConfiguration' {..} =
      Prelude.mconcat
        ["id" Data.=: id, "intelligent-tiering"]

-- | /See:/ 'newDeleteBucketIntelligentTieringConfigurationResponse' smart constructor.
data DeleteBucketIntelligentTieringConfigurationResponse = DeleteBucketIntelligentTieringConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketIntelligentTieringConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketIntelligentTieringConfigurationResponse ::
  DeleteBucketIntelligentTieringConfigurationResponse
newDeleteBucketIntelligentTieringConfigurationResponse =
  DeleteBucketIntelligentTieringConfigurationResponse'

instance
  Prelude.NFData
    DeleteBucketIntelligentTieringConfigurationResponse
  where
  rnf _ = ()
