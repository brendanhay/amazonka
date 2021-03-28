{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Lightsail content delivery network (CDN) distribution.
--
-- A distribution is a globally distributed network of caching servers that improve the performance of your website or web application hosted on a Lightsail instance. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-content-delivery-network-distributions Content delivery networks in Amazon Lightsail> .
module Network.AWS.Lightsail.CreateDistribution
    (
    -- * Creating a request
      CreateDistribution (..)
    , mkCreateDistribution
    -- ** Request lenses
    , cdDistributionName
    , cdOrigin
    , cdDefaultCacheBehavior
    , cdBundleId
    , cdCacheBehaviorSettings
    , cdCacheBehaviors
    , cdTags

    -- * Destructuring the response
    , CreateDistributionResponse (..)
    , mkCreateDistributionResponse
    -- ** Response lenses
    , cdrrsDistribution
    , cdrrsOperation
    , cdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDistribution' smart constructor.
data CreateDistribution = CreateDistribution'
  { distributionName :: Types.DistributionName
    -- ^ The name for the distribution.
  , origin :: Types.InputOrigin
    -- ^ An object that describes the origin resource for the distribution, such as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
  , defaultCacheBehavior :: Types.CacheBehavior
    -- ^ An object that describes the default cache behavior for the distribution.
  , bundleId :: Core.Text
    -- ^ The bundle ID to use for the distribution.
--
-- A distribution bundle describes the specifications of your distribution, such as the monthly cost and monthly network transfer quota.
-- Use the @GetDistributionBundles@ action to get a list of distribution bundle IDs that you can specify.
  , cacheBehaviorSettings :: Core.Maybe Types.CacheSettings
    -- ^ An object that describes the cache behavior settings for the distribution.
  , cacheBehaviors :: Core.Maybe [Types.CacheBehaviorPerPath]
    -- ^ An array of objects that describe the per-path cache behavior for the distribution.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values to add to the distribution during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDistribution' value with any optional fields omitted.
mkCreateDistribution
    :: Types.DistributionName -- ^ 'distributionName'
    -> Types.InputOrigin -- ^ 'origin'
    -> Types.CacheBehavior -- ^ 'defaultCacheBehavior'
    -> Core.Text -- ^ 'bundleId'
    -> CreateDistribution
mkCreateDistribution distributionName origin defaultCacheBehavior
  bundleId
  = CreateDistribution'{distributionName, origin,
                        defaultCacheBehavior, bundleId,
                        cacheBehaviorSettings = Core.Nothing,
                        cacheBehaviors = Core.Nothing, tags = Core.Nothing}

-- | The name for the distribution.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDistributionName :: Lens.Lens' CreateDistribution Types.DistributionName
cdDistributionName = Lens.field @"distributionName"
{-# INLINEABLE cdDistributionName #-}
{-# DEPRECATED distributionName "Use generic-lens or generic-optics with 'distributionName' instead"  #-}

-- | An object that describes the origin resource for the distribution, such as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdOrigin :: Lens.Lens' CreateDistribution Types.InputOrigin
cdOrigin = Lens.field @"origin"
{-# INLINEABLE cdOrigin #-}
{-# DEPRECATED origin "Use generic-lens or generic-optics with 'origin' instead"  #-}

-- | An object that describes the default cache behavior for the distribution.
--
-- /Note:/ Consider using 'defaultCacheBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDefaultCacheBehavior :: Lens.Lens' CreateDistribution Types.CacheBehavior
cdDefaultCacheBehavior = Lens.field @"defaultCacheBehavior"
{-# INLINEABLE cdDefaultCacheBehavior #-}
{-# DEPRECATED defaultCacheBehavior "Use generic-lens or generic-optics with 'defaultCacheBehavior' instead"  #-}

-- | The bundle ID to use for the distribution.
--
-- A distribution bundle describes the specifications of your distribution, such as the monthly cost and monthly network transfer quota.
-- Use the @GetDistributionBundles@ action to get a list of distribution bundle IDs that you can specify.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdBundleId :: Lens.Lens' CreateDistribution Core.Text
cdBundleId = Lens.field @"bundleId"
{-# INLINEABLE cdBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | An object that describes the cache behavior settings for the distribution.
--
-- /Note:/ Consider using 'cacheBehaviorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCacheBehaviorSettings :: Lens.Lens' CreateDistribution (Core.Maybe Types.CacheSettings)
cdCacheBehaviorSettings = Lens.field @"cacheBehaviorSettings"
{-# INLINEABLE cdCacheBehaviorSettings #-}
{-# DEPRECATED cacheBehaviorSettings "Use generic-lens or generic-optics with 'cacheBehaviorSettings' instead"  #-}

-- | An array of objects that describe the per-path cache behavior for the distribution.
--
-- /Note:/ Consider using 'cacheBehaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCacheBehaviors :: Lens.Lens' CreateDistribution (Core.Maybe [Types.CacheBehaviorPerPath])
cdCacheBehaviors = Lens.field @"cacheBehaviors"
{-# INLINEABLE cdCacheBehaviors #-}
{-# DEPRECATED cacheBehaviors "Use generic-lens or generic-optics with 'cacheBehaviors' instead"  #-}

-- | The tag keys and optional values to add to the distribution during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTags :: Lens.Lens' CreateDistribution (Core.Maybe [Types.Tag])
cdTags = Lens.field @"tags"
{-# INLINEABLE cdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDistribution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDistribution where
        toHeaders CreateDistribution{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.CreateDistribution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDistribution where
        toJSON CreateDistribution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("distributionName" Core..= distributionName),
                  Core.Just ("origin" Core..= origin),
                  Core.Just ("defaultCacheBehavior" Core..= defaultCacheBehavior),
                  Core.Just ("bundleId" Core..= bundleId),
                  ("cacheBehaviorSettings" Core..=) Core.<$> cacheBehaviorSettings,
                  ("cacheBehaviors" Core..=) Core.<$> cacheBehaviors,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateDistribution where
        type Rs CreateDistribution = CreateDistributionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDistributionResponse' Core.<$>
                   (x Core..:? "distribution") Core.<*> x Core..:? "operation"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDistributionResponse' smart constructor.
data CreateDistributionResponse = CreateDistributionResponse'
  { distribution :: Core.Maybe Types.LightsailDistribution
    -- ^ An object that describes the distribution created.
  , operation :: Core.Maybe Types.Operation
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateDistributionResponse' value with any optional fields omitted.
mkCreateDistributionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDistributionResponse
mkCreateDistributionResponse responseStatus
  = CreateDistributionResponse'{distribution = Core.Nothing,
                                operation = Core.Nothing, responseStatus}

-- | An object that describes the distribution created.
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDistribution :: Lens.Lens' CreateDistributionResponse (Core.Maybe Types.LightsailDistribution)
cdrrsDistribution = Lens.field @"distribution"
{-# INLINEABLE cdrrsDistribution #-}
{-# DEPRECATED distribution "Use generic-lens or generic-optics with 'distribution' instead"  #-}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsOperation :: Lens.Lens' CreateDistributionResponse (Core.Maybe Types.Operation)
cdrrsOperation = Lens.field @"operation"
{-# INLINEABLE cdrrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' CreateDistributionResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
