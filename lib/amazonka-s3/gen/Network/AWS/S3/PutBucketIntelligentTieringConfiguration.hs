{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketIntelligentTieringConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts a S3 Intelligent-Tiering configuration to the specified bucket.
--
-- The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead. S3 Intelligent-Tiering delivers automatic cost savings by moving data between access tiers, when access patterns change.
-- The S3 Intelligent-Tiering storage class is suitable for objects larger than 128 KB that you plan to store for at least 30 days. If the size of an object is less than 128 KB, it is not eligible for auto-tiering. Smaller objects can be stored, but they are always charged at the frequent access tier rates in the S3 Intelligent-Tiering storage class.
-- If you delete an object before the end of the 30-day minimum storage duration period, you are charged for 30 days. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
-- Operations related to @PutBucketIntelligentTieringConfiguration@ include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketIntelligentTieringConfiguration.html DeleteBucketIntelligentTieringConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketIntelligentTieringConfiguration.html GetBucketIntelligentTieringConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketIntelligentTieringConfigurations.html ListBucketIntelligentTieringConfigurations>
module Network.AWS.S3.PutBucketIntelligentTieringConfiguration
  ( -- * Creating a request
    PutBucketIntelligentTieringConfiguration (..),
    mkPutBucketIntelligentTieringConfiguration,

    -- ** Request lenses
    pbitcBucket,
    pbitcId,
    pbitcIntelligentTieringConfiguration,

    -- * Destructuring the response
    PutBucketIntelligentTieringConfigurationResponse (..),
    mkPutBucketIntelligentTieringConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketIntelligentTieringConfiguration' smart constructor.
data PutBucketIntelligentTieringConfiguration = PutBucketIntelligentTieringConfiguration'
  { -- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
    bucket :: Types.BucketName,
    -- | The ID used to identify the S3 Intelligent-Tiering configuration.
    id :: Types.Id,
    -- | Container for S3 Intelligent-Tiering configuration.
    intelligentTieringConfiguration :: Types.IntelligentTieringConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketIntelligentTieringConfiguration' value with any optional fields omitted.
mkPutBucketIntelligentTieringConfiguration ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'id'
  Types.Id ->
  -- | 'intelligentTieringConfiguration'
  Types.IntelligentTieringConfiguration ->
  PutBucketIntelligentTieringConfiguration
mkPutBucketIntelligentTieringConfiguration
  bucket
  id
  intelligentTieringConfiguration =
    PutBucketIntelligentTieringConfiguration'
      { bucket,
        id,
        intelligentTieringConfiguration
      }

-- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbitcBucket :: Lens.Lens' PutBucketIntelligentTieringConfiguration Types.BucketName
pbitcBucket = Lens.field @"bucket"
{-# DEPRECATED pbitcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbitcId :: Lens.Lens' PutBucketIntelligentTieringConfiguration Types.Id
pbitcId = Lens.field @"id"
{-# DEPRECATED pbitcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Container for S3 Intelligent-Tiering configuration.
--
-- /Note:/ Consider using 'intelligentTieringConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbitcIntelligentTieringConfiguration :: Lens.Lens' PutBucketIntelligentTieringConfiguration Types.IntelligentTieringConfiguration
pbitcIntelligentTieringConfiguration = Lens.field @"intelligentTieringConfiguration"
{-# DEPRECATED pbitcIntelligentTieringConfiguration "Use generic-lens or generic-optics with 'intelligentTieringConfiguration' instead." #-}

instance Core.AWSRequest PutBucketIntelligentTieringConfiguration where
  type
    Rs PutBucketIntelligentTieringConfiguration =
      PutBucketIntelligentTieringConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery =
          Core.toQueryValue "id" id
            Core.<> (Core.pure ("intelligent-tiering", "")),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveNull
      PutBucketIntelligentTieringConfigurationResponse'

-- | /See:/ 'mkPutBucketIntelligentTieringConfigurationResponse' smart constructor.
data PutBucketIntelligentTieringConfigurationResponse = PutBucketIntelligentTieringConfigurationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketIntelligentTieringConfigurationResponse' value with any optional fields omitted.
mkPutBucketIntelligentTieringConfigurationResponse ::
  PutBucketIntelligentTieringConfigurationResponse
mkPutBucketIntelligentTieringConfigurationResponse =
  PutBucketIntelligentTieringConfigurationResponse'
