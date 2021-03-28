{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateOriginRequestPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an origin request policy configuration.
--
-- When you update an origin request policy configuration, all the fields are updated with the values provided in the request. You cannot update some fields independent of others. To update an origin request policy configuration:
--
--     * Use @GetOriginRequestPolicyConfig@ to get the current configuration.
--
--
--     * Locally modify the fields in the origin request policy configuration that you want to update.
--
--
--     * Call @UpdateOriginRequestPolicy@ by providing the entire origin request policy configuration, including the fields that you modified and those that you didn’t.
--
--
module Network.AWS.CloudFront.UpdateOriginRequestPolicy
    (
    -- * Creating a request
      UpdateOriginRequestPolicy (..)
    , mkUpdateOriginRequestPolicy
    -- ** Request lenses
    , uorpOriginRequestPolicyConfig
    , uorpId
    , uorpIfMatch

    -- * Destructuring the response
    , UpdateOriginRequestPolicyResponse (..)
    , mkUpdateOriginRequestPolicyResponse
    -- ** Response lenses
    , uorprrsETag
    , uorprrsOriginRequestPolicy
    , uorprrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateOriginRequestPolicy' smart constructor.
data UpdateOriginRequestPolicy = UpdateOriginRequestPolicy'
  { originRequestPolicyConfig :: Types.OriginRequestPolicyConfig
    -- ^ An origin request policy configuration.
  , id :: Core.Text
    -- ^ The unique identifier for the origin request policy that you are updating. The identifier is returned in a cache behavior’s @OriginRequestPolicyId@ field in the response to @GetDistributionConfig@ .
  , ifMatch :: Core.Maybe Core.Text
    -- ^ The version of the origin request policy that you are updating. The version is returned in the origin request policy’s @ETag@ field in the response to @GetOriginRequestPolicyConfig@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateOriginRequestPolicy' value with any optional fields omitted.
mkUpdateOriginRequestPolicy
    :: Types.OriginRequestPolicyConfig -- ^ 'originRequestPolicyConfig'
    -> Core.Text -- ^ 'id'
    -> UpdateOriginRequestPolicy
mkUpdateOriginRequestPolicy originRequestPolicyConfig id
  = UpdateOriginRequestPolicy'{originRequestPolicyConfig, id,
                               ifMatch = Core.Nothing}

-- | An origin request policy configuration.
--
-- /Note:/ Consider using 'originRequestPolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uorpOriginRequestPolicyConfig :: Lens.Lens' UpdateOriginRequestPolicy Types.OriginRequestPolicyConfig
uorpOriginRequestPolicyConfig = Lens.field @"originRequestPolicyConfig"
{-# INLINEABLE uorpOriginRequestPolicyConfig #-}
{-# DEPRECATED originRequestPolicyConfig "Use generic-lens or generic-optics with 'originRequestPolicyConfig' instead"  #-}

-- | The unique identifier for the origin request policy that you are updating. The identifier is returned in a cache behavior’s @OriginRequestPolicyId@ field in the response to @GetDistributionConfig@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uorpId :: Lens.Lens' UpdateOriginRequestPolicy Core.Text
uorpId = Lens.field @"id"
{-# INLINEABLE uorpId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The version of the origin request policy that you are updating. The version is returned in the origin request policy’s @ETag@ field in the response to @GetOriginRequestPolicyConfig@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uorpIfMatch :: Lens.Lens' UpdateOriginRequestPolicy (Core.Maybe Core.Text)
uorpIfMatch = Lens.field @"ifMatch"
{-# INLINEABLE uorpIfMatch #-}
{-# DEPRECATED ifMatch "Use generic-lens or generic-optics with 'ifMatch' instead"  #-}

instance Core.ToQuery UpdateOriginRequestPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateOriginRequestPolicy where
        toHeaders UpdateOriginRequestPolicy{..}
          = Core.toHeaders "If-Match" ifMatch

instance Core.AWSRequest UpdateOriginRequestPolicy where
        type Rs UpdateOriginRequestPolicy =
             UpdateOriginRequestPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/2020-05-31/origin-request-policy/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 UpdateOriginRequestPolicyResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*> Core.parseXML x Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateOriginRequestPolicyResponse' smart constructor.
data UpdateOriginRequestPolicyResponse = UpdateOriginRequestPolicyResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The current version of the origin request policy.
  , originRequestPolicy :: Core.Maybe Types.OriginRequestPolicy
    -- ^ An origin request policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateOriginRequestPolicyResponse' value with any optional fields omitted.
mkUpdateOriginRequestPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateOriginRequestPolicyResponse
mkUpdateOriginRequestPolicyResponse responseStatus
  = UpdateOriginRequestPolicyResponse'{eTag = Core.Nothing,
                                       originRequestPolicy = Core.Nothing, responseStatus}

-- | The current version of the origin request policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uorprrsETag :: Lens.Lens' UpdateOriginRequestPolicyResponse (Core.Maybe Core.Text)
uorprrsETag = Lens.field @"eTag"
{-# INLINEABLE uorprrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | An origin request policy.
--
-- /Note:/ Consider using 'originRequestPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uorprrsOriginRequestPolicy :: Lens.Lens' UpdateOriginRequestPolicyResponse (Core.Maybe Types.OriginRequestPolicy)
uorprrsOriginRequestPolicy = Lens.field @"originRequestPolicy"
{-# INLINEABLE uorprrsOriginRequestPolicy #-}
{-# DEPRECATED originRequestPolicy "Use generic-lens or generic-optics with 'originRequestPolicy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uorprrsResponseStatus :: Lens.Lens' UpdateOriginRequestPolicyResponse Core.Int
uorprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uorprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
