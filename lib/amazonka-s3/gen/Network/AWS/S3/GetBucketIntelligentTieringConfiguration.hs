{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketIntelligentTieringConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the S3 Intelligent-Tiering configuration from the specified bucket.
--
-- The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead. S3 Intelligent-Tiering delivers automatic cost savings by moving data between access tiers, when access patterns change.
-- The S3 Intelligent-Tiering storage class is suitable for objects larger than 128 KB that you plan to store for at least 30 days. If the size of an object is less than 128 KB, it is not eligible for auto-tiering. Smaller objects can be stored, but they are always charged at the frequent access tier rates in the S3 Intelligent-Tiering storage class. 
-- If you delete an object before the end of the 30-day minimum storage duration period, you are charged for 30 days. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
-- Operations related to @GetBucketIntelligentTieringConfiguration@ include: 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketIntelligentTieringConfiguration.html DeleteBucketIntelligentTieringConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketIntelligentTieringConfiguration.html PutBucketIntelligentTieringConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketIntelligentTieringConfigurations.html ListBucketIntelligentTieringConfigurations> 
--
--
module Network.AWS.S3.GetBucketIntelligentTieringConfiguration
    (
    -- * Creating a request
      GetBucketIntelligentTieringConfiguration (..)
    , mkGetBucketIntelligentTieringConfiguration
    -- ** Request lenses
    , gbitcBucket
    , gbitcId

    -- * Destructuring the response
    , GetBucketIntelligentTieringConfigurationResponse (..)
    , mkGetBucketIntelligentTieringConfigurationResponse
    -- ** Response lenses
    , gbitcrrsIntelligentTieringConfiguration
    , gbitcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketIntelligentTieringConfiguration' smart constructor.
data GetBucketIntelligentTieringConfiguration = GetBucketIntelligentTieringConfiguration'
  { bucket :: Types.BucketName
    -- ^ The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
  , id :: Types.Id
    -- ^ The ID used to identify the S3 Intelligent-Tiering configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketIntelligentTieringConfiguration' value with any optional fields omitted.
mkGetBucketIntelligentTieringConfiguration
    :: Types.BucketName -- ^ 'bucket'
    -> Types.Id -- ^ 'id'
    -> GetBucketIntelligentTieringConfiguration
mkGetBucketIntelligentTieringConfiguration bucket id
  = GetBucketIntelligentTieringConfiguration'{bucket, id}

-- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbitcBucket :: Lens.Lens' GetBucketIntelligentTieringConfiguration Types.BucketName
gbitcBucket = Lens.field @"bucket"
{-# INLINEABLE gbitcBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbitcId :: Lens.Lens' GetBucketIntelligentTieringConfiguration Types.Id
gbitcId = Lens.field @"id"
{-# INLINEABLE gbitcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetBucketIntelligentTieringConfiguration
         where
        toQuery GetBucketIntelligentTieringConfiguration{..}
          = Core.toQueryPair "id" id Core.<>
              Core.toQueryPair "intelligent-tiering" ("" :: Core.Text)

instance Core.ToHeaders GetBucketIntelligentTieringConfiguration
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetBucketIntelligentTieringConfiguration
         where
        type Rs GetBucketIntelligentTieringConfiguration =
             GetBucketIntelligentTieringConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetBucketIntelligentTieringConfigurationResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetBucketIntelligentTieringConfigurationResponse' smart constructor.
data GetBucketIntelligentTieringConfigurationResponse = GetBucketIntelligentTieringConfigurationResponse'
  { intelligentTieringConfiguration :: Core.Maybe Types.IntelligentTieringConfiguration
    -- ^ Container for S3 Intelligent-Tiering configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketIntelligentTieringConfigurationResponse' value with any optional fields omitted.
mkGetBucketIntelligentTieringConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBucketIntelligentTieringConfigurationResponse
mkGetBucketIntelligentTieringConfigurationResponse responseStatus
  = GetBucketIntelligentTieringConfigurationResponse'{intelligentTieringConfiguration
                                                        = Core.Nothing,
                                                      responseStatus}

-- | Container for S3 Intelligent-Tiering configuration.
--
-- /Note:/ Consider using 'intelligentTieringConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbitcrrsIntelligentTieringConfiguration :: Lens.Lens' GetBucketIntelligentTieringConfigurationResponse (Core.Maybe Types.IntelligentTieringConfiguration)
gbitcrrsIntelligentTieringConfiguration = Lens.field @"intelligentTieringConfiguration"
{-# INLINEABLE gbitcrrsIntelligentTieringConfiguration #-}
{-# DEPRECATED intelligentTieringConfiguration "Use generic-lens or generic-optics with 'intelligentTieringConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbitcrrsResponseStatus :: Lens.Lens' GetBucketIntelligentTieringConfigurationResponse Core.Int
gbitcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbitcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
