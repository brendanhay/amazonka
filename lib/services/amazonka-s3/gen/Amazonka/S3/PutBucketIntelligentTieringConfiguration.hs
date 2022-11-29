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
-- Module      : Amazonka.S3.PutBucketIntelligentTieringConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts a S3 Intelligent-Tiering configuration to the specified bucket. You
-- can have up to 1,000 S3 Intelligent-Tiering configurations per bucket.
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
-- Operations related to @PutBucketIntelligentTieringConfiguration@
-- include:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketIntelligentTieringConfiguration.html DeleteBucketIntelligentTieringConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketIntelligentTieringConfiguration.html GetBucketIntelligentTieringConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketIntelligentTieringConfigurations.html ListBucketIntelligentTieringConfigurations>
--
-- You only need S3 Intelligent-Tiering enabled on a bucket if you want to
-- automatically move objects stored in the S3 Intelligent-Tiering storage
-- class to the Archive Access or Deep Archive Access tier.
--
-- __Special Errors__
--
-- -   __HTTP 400 Bad Request Error__
--
--     -   /Code:/ InvalidArgument
--
--     -   /Cause:/ Invalid Argument
--
-- -   __HTTP 400 Bad Request Error__
--
--     -   /Code:/ TooManyConfigurations
--
--     -   /Cause:/ You are attempting to create a new configuration but
--         have already reached the 1,000-configuration limit.
--
-- -   __HTTP 403 Forbidden Error__
--
--     -   /Code:/ AccessDenied
--
--     -   /Cause:/ You are not the owner of the specified bucket, or you
--         do not have the @s3:PutIntelligentTieringConfiguration@ bucket
--         permission to set the configuration on the bucket.
module Amazonka.S3.PutBucketIntelligentTieringConfiguration
  ( -- * Creating a Request
    PutBucketIntelligentTieringConfiguration (..),
    newPutBucketIntelligentTieringConfiguration,

    -- * Request Lenses
    putBucketIntelligentTieringConfiguration_bucket,
    putBucketIntelligentTieringConfiguration_id,
    putBucketIntelligentTieringConfiguration_intelligentTieringConfiguration,

    -- * Destructuring the Response
    PutBucketIntelligentTieringConfigurationResponse (..),
    newPutBucketIntelligentTieringConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketIntelligentTieringConfiguration' smart constructor.
data PutBucketIntelligentTieringConfiguration = PutBucketIntelligentTieringConfiguration'
  { -- | The name of the Amazon S3 bucket whose configuration you want to modify
    -- or retrieve.
    bucket :: BucketName,
    -- | The ID used to identify the S3 Intelligent-Tiering configuration.
    id :: Prelude.Text,
    -- | Container for S3 Intelligent-Tiering configuration.
    intelligentTieringConfiguration :: IntelligentTieringConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketIntelligentTieringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'putBucketIntelligentTieringConfiguration_bucket' - The name of the Amazon S3 bucket whose configuration you want to modify
-- or retrieve.
--
-- 'id', 'putBucketIntelligentTieringConfiguration_id' - The ID used to identify the S3 Intelligent-Tiering configuration.
--
-- 'intelligentTieringConfiguration', 'putBucketIntelligentTieringConfiguration_intelligentTieringConfiguration' - Container for S3 Intelligent-Tiering configuration.
newPutBucketIntelligentTieringConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Prelude.Text ->
  -- | 'intelligentTieringConfiguration'
  IntelligentTieringConfiguration ->
  PutBucketIntelligentTieringConfiguration
newPutBucketIntelligentTieringConfiguration
  pBucket_
  pId_
  pIntelligentTieringConfiguration_ =
    PutBucketIntelligentTieringConfiguration'
      { bucket =
          pBucket_,
        id = pId_,
        intelligentTieringConfiguration =
          pIntelligentTieringConfiguration_
      }

-- | The name of the Amazon S3 bucket whose configuration you want to modify
-- or retrieve.
putBucketIntelligentTieringConfiguration_bucket :: Lens.Lens' PutBucketIntelligentTieringConfiguration BucketName
putBucketIntelligentTieringConfiguration_bucket = Lens.lens (\PutBucketIntelligentTieringConfiguration' {bucket} -> bucket) (\s@PutBucketIntelligentTieringConfiguration' {} a -> s {bucket = a} :: PutBucketIntelligentTieringConfiguration)

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
putBucketIntelligentTieringConfiguration_id :: Lens.Lens' PutBucketIntelligentTieringConfiguration Prelude.Text
putBucketIntelligentTieringConfiguration_id = Lens.lens (\PutBucketIntelligentTieringConfiguration' {id} -> id) (\s@PutBucketIntelligentTieringConfiguration' {} a -> s {id = a} :: PutBucketIntelligentTieringConfiguration)

-- | Container for S3 Intelligent-Tiering configuration.
putBucketIntelligentTieringConfiguration_intelligentTieringConfiguration :: Lens.Lens' PutBucketIntelligentTieringConfiguration IntelligentTieringConfiguration
putBucketIntelligentTieringConfiguration_intelligentTieringConfiguration = Lens.lens (\PutBucketIntelligentTieringConfiguration' {intelligentTieringConfiguration} -> intelligentTieringConfiguration) (\s@PutBucketIntelligentTieringConfiguration' {} a -> s {intelligentTieringConfiguration = a} :: PutBucketIntelligentTieringConfiguration)

instance
  Core.AWSRequest
    PutBucketIntelligentTieringConfiguration
  where
  type
    AWSResponse
      PutBucketIntelligentTieringConfiguration =
      PutBucketIntelligentTieringConfigurationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveNull
      PutBucketIntelligentTieringConfigurationResponse'

instance
  Prelude.Hashable
    PutBucketIntelligentTieringConfiguration
  where
  hashWithSalt
    _salt
    PutBucketIntelligentTieringConfiguration' {..} =
      _salt `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` intelligentTieringConfiguration

instance
  Prelude.NFData
    PutBucketIntelligentTieringConfiguration
  where
  rnf PutBucketIntelligentTieringConfiguration' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf intelligentTieringConfiguration

instance
  Core.ToElement
    PutBucketIntelligentTieringConfiguration
  where
  toElement
    PutBucketIntelligentTieringConfiguration' {..} =
      Core.mkElement
        "{http://s3.amazonaws.com/doc/2006-03-01/}IntelligentTieringConfiguration"
        intelligentTieringConfiguration

instance
  Core.ToHeaders
    PutBucketIntelligentTieringConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    PutBucketIntelligentTieringConfiguration
  where
  toPath PutBucketIntelligentTieringConfiguration' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance
  Core.ToQuery
    PutBucketIntelligentTieringConfiguration
  where
  toQuery PutBucketIntelligentTieringConfiguration' {..} =
    Prelude.mconcat
      ["id" Core.=: id, "intelligent-tiering"]

-- | /See:/ 'newPutBucketIntelligentTieringConfigurationResponse' smart constructor.
data PutBucketIntelligentTieringConfigurationResponse = PutBucketIntelligentTieringConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketIntelligentTieringConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketIntelligentTieringConfigurationResponse ::
  PutBucketIntelligentTieringConfigurationResponse
newPutBucketIntelligentTieringConfigurationResponse =
  PutBucketIntelligentTieringConfigurationResponse'

instance
  Prelude.NFData
    PutBucketIntelligentTieringConfigurationResponse
  where
  rnf _ = ()
