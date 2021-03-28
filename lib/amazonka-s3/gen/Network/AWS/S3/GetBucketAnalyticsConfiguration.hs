{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the GET operation returns an analytics configuration (identified by the analytics configuration ID) from the bucket.
--
-- To use this operation, you must have permissions to perform the @s3:GetAnalyticsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> in the /Amazon Simple Storage Service Developer Guide/ . 
-- For information about Amazon S3 analytics feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis> in the /Amazon Simple Storage Service Developer Guide/ .
-- __Related Resources__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketAnalyticsConfiguration.html DeleteBucketAnalyticsConfiguration> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketAnalyticsConfigurations.html ListBucketAnalyticsConfigurations> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAnalyticsConfiguration.html PutBucketAnalyticsConfiguration> 
--
--
module Network.AWS.S3.GetBucketAnalyticsConfiguration
    (
    -- * Creating a request
      GetBucketAnalyticsConfiguration (..)
    , mkGetBucketAnalyticsConfiguration
    -- ** Request lenses
    , gbacfBucket
    , gbacfId
    , gbacfExpectedBucketOwner

    -- * Destructuring the response
    , GetBucketAnalyticsConfigurationResponse (..)
    , mkGetBucketAnalyticsConfigurationResponse
    -- ** Response lenses
    , gbacrfrsAnalyticsConfiguration
    , gbacrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketAnalyticsConfiguration' smart constructor.
data GetBucketAnalyticsConfiguration = GetBucketAnalyticsConfiguration'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket from which an analytics configuration is retrieved.
  , id :: Types.AnalyticsId
    -- ^ The ID that identifies the analytics configuration.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketAnalyticsConfiguration' value with any optional fields omitted.
mkGetBucketAnalyticsConfiguration
    :: Types.BucketName -- ^ 'bucket'
    -> Types.AnalyticsId -- ^ 'id'
    -> GetBucketAnalyticsConfiguration
mkGetBucketAnalyticsConfiguration bucket id
  = GetBucketAnalyticsConfiguration'{bucket, id,
                                     expectedBucketOwner = Core.Nothing}

-- | The name of the bucket from which an analytics configuration is retrieved.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacfBucket :: Lens.Lens' GetBucketAnalyticsConfiguration Types.BucketName
gbacfBucket = Lens.field @"bucket"
{-# INLINEABLE gbacfBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The ID that identifies the analytics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacfId :: Lens.Lens' GetBucketAnalyticsConfiguration Types.AnalyticsId
gbacfId = Lens.field @"id"
{-# INLINEABLE gbacfId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacfExpectedBucketOwner :: Lens.Lens' GetBucketAnalyticsConfiguration (Core.Maybe Types.AccountId)
gbacfExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE gbacfExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery GetBucketAnalyticsConfiguration where
        toQuery GetBucketAnalyticsConfiguration{..}
          = Core.toQueryPair "id" id Core.<>
              Core.toQueryPair "analytics" ("" :: Core.Text)

instance Core.ToHeaders GetBucketAnalyticsConfiguration where
        toHeaders GetBucketAnalyticsConfiguration{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest GetBucketAnalyticsConfiguration where
        type Rs GetBucketAnalyticsConfiguration =
             GetBucketAnalyticsConfigurationResponse
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
                 GetBucketAnalyticsConfigurationResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetBucketAnalyticsConfigurationResponse' smart constructor.
data GetBucketAnalyticsConfigurationResponse = GetBucketAnalyticsConfigurationResponse'
  { analyticsConfiguration :: Core.Maybe Types.AnalyticsConfiguration
    -- ^ The configuration and any analyses for the analytics filter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketAnalyticsConfigurationResponse' value with any optional fields omitted.
mkGetBucketAnalyticsConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBucketAnalyticsConfigurationResponse
mkGetBucketAnalyticsConfigurationResponse responseStatus
  = GetBucketAnalyticsConfigurationResponse'{analyticsConfiguration =
                                               Core.Nothing,
                                             responseStatus}

-- | The configuration and any analyses for the analytics filter.
--
-- /Note:/ Consider using 'analyticsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacrfrsAnalyticsConfiguration :: Lens.Lens' GetBucketAnalyticsConfigurationResponse (Core.Maybe Types.AnalyticsConfiguration)
gbacrfrsAnalyticsConfiguration = Lens.field @"analyticsConfiguration"
{-# INLINEABLE gbacrfrsAnalyticsConfiguration #-}
{-# DEPRECATED analyticsConfiguration "Use generic-lens or generic-optics with 'analyticsConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbacrfrsResponseStatus :: Lens.Lens' GetBucketAnalyticsConfigurationResponse Core.Int
gbacrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbacrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
