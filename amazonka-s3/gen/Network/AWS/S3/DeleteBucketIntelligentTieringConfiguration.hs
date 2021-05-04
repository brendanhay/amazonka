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
-- Module      : Network.AWS.S3.DeleteBucketIntelligentTieringConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- Operations related to @DeleteBucketIntelligentTieringConfiguration@
-- include:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketIntelligentTieringConfiguration.html GetBucketIntelligentTieringConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketIntelligentTieringConfiguration.html PutBucketIntelligentTieringConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketIntelligentTieringConfigurations.html ListBucketIntelligentTieringConfigurations>
module Network.AWS.S3.DeleteBucketIntelligentTieringConfiguration
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newDeleteBucketIntelligentTieringConfiguration' smart constructor.
data DeleteBucketIntelligentTieringConfiguration = DeleteBucketIntelligentTieringConfiguration'
  { -- | The name of the Amazon S3 bucket whose configuration you want to modify
    -- or retrieve.
    bucket :: BucketName,
    -- | The ID used to identify the S3 Intelligent-Tiering configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DeleteBucketIntelligentTieringConfiguration
  where
  type
    Rs DeleteBucketIntelligentTieringConfiguration =
      DeleteBucketIntelligentTieringConfigurationResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteBucketIntelligentTieringConfigurationResponse'

instance
  Prelude.Hashable
    DeleteBucketIntelligentTieringConfiguration

instance
  Prelude.NFData
    DeleteBucketIntelligentTieringConfiguration

instance
  Prelude.ToHeaders
    DeleteBucketIntelligentTieringConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DeleteBucketIntelligentTieringConfiguration
  where
  toPath
    DeleteBucketIntelligentTieringConfiguration' {..} =
      Prelude.mconcat ["/", Prelude.toBS bucket]

instance
  Prelude.ToQuery
    DeleteBucketIntelligentTieringConfiguration
  where
  toQuery
    DeleteBucketIntelligentTieringConfiguration' {..} =
      Prelude.mconcat
        ["id" Prelude.=: id, "intelligent-tiering"]

-- | /See:/ 'newDeleteBucketIntelligentTieringConfigurationResponse' smart constructor.
data DeleteBucketIntelligentTieringConfigurationResponse = DeleteBucketIntelligentTieringConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
