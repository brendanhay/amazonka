{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetOriginRequestPolicyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an origin request policy configuration.
--
-- To get an origin request policy configuration, you must provide the policy’s identifier. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
module Network.AWS.CloudFront.GetOriginRequestPolicyConfig
    (
    -- * Creating a request
      GetOriginRequestPolicyConfig (..)
    , mkGetOriginRequestPolicyConfig
    -- ** Request lenses
    , gorpcId

    -- * Destructuring the response
    , GetOriginRequestPolicyConfigResponse (..)
    , mkGetOriginRequestPolicyConfigResponse
    -- ** Response lenses
    , gorpcrrsETag
    , gorpcrrsOriginRequestPolicyConfig
    , gorpcrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetOriginRequestPolicyConfig' smart constructor.
newtype GetOriginRequestPolicyConfig = GetOriginRequestPolicyConfig'
  { id :: Core.Text
    -- ^ The unique identifier for the origin request policy. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetOriginRequestPolicyConfig' value with any optional fields omitted.
mkGetOriginRequestPolicyConfig
    :: Core.Text -- ^ 'id'
    -> GetOriginRequestPolicyConfig
mkGetOriginRequestPolicyConfig id
  = GetOriginRequestPolicyConfig'{id}

-- | The unique identifier for the origin request policy. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorpcId :: Lens.Lens' GetOriginRequestPolicyConfig Core.Text
gorpcId = Lens.field @"id"
{-# INLINEABLE gorpcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetOriginRequestPolicyConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetOriginRequestPolicyConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetOriginRequestPolicyConfig where
        type Rs GetOriginRequestPolicyConfig =
             GetOriginRequestPolicyConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/origin-request-policy/" Core.<> Core.toText id Core.<>
                             "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetOriginRequestPolicyConfigResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetOriginRequestPolicyConfigResponse' smart constructor.
data GetOriginRequestPolicyConfigResponse = GetOriginRequestPolicyConfigResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The current version of the origin request policy.
  , originRequestPolicyConfig :: Core.Maybe Types.OriginRequestPolicyConfig
    -- ^ The origin request policy configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetOriginRequestPolicyConfigResponse' value with any optional fields omitted.
mkGetOriginRequestPolicyConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetOriginRequestPolicyConfigResponse
mkGetOriginRequestPolicyConfigResponse responseStatus
  = GetOriginRequestPolicyConfigResponse'{eTag = Core.Nothing,
                                          originRequestPolicyConfig = Core.Nothing, responseStatus}

-- | The current version of the origin request policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorpcrrsETag :: Lens.Lens' GetOriginRequestPolicyConfigResponse (Core.Maybe Core.Text)
gorpcrrsETag = Lens.field @"eTag"
{-# INLINEABLE gorpcrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The origin request policy configuration.
--
-- /Note:/ Consider using 'originRequestPolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorpcrrsOriginRequestPolicyConfig :: Lens.Lens' GetOriginRequestPolicyConfigResponse (Core.Maybe Types.OriginRequestPolicyConfig)
gorpcrrsOriginRequestPolicyConfig = Lens.field @"originRequestPolicyConfig"
{-# INLINEABLE gorpcrrsOriginRequestPolicyConfig #-}
{-# DEPRECATED originRequestPolicyConfig "Use generic-lens or generic-optics with 'originRequestPolicyConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorpcrrsResponseStatus :: Lens.Lens' GetOriginRequestPolicyConfigResponse Core.Int
gorpcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gorpcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
