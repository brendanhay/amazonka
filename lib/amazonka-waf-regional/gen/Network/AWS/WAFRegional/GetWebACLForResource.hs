{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetWebACLForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the web ACL for the specified resource, either an application load balancer or Amazon API Gateway stage.
module Network.AWS.WAFRegional.GetWebACLForResource
    (
    -- * Creating a request
      GetWebACLForResource (..)
    , mkGetWebACLForResource
    -- ** Request lenses
    , gwaclfrResourceArn

    -- * Destructuring the response
    , GetWebACLForResourceResponse (..)
    , mkGetWebACLForResourceResponse
    -- ** Response lenses
    , gwaclfrrrsWebACLSummary
    , gwaclfrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkGetWebACLForResource' smart constructor.
newtype GetWebACLForResource = GetWebACLForResource'
  { resourceArn :: Types.ResourceArn
    -- ^ The ARN (Amazon Resource Name) of the resource for which to get the web ACL, either an application load balancer or Amazon API Gateway stage.
--
-- The ARN should be in one of the following formats:
--
--     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @ 
--
--
--     * For an Amazon API Gateway stage: @arn:aws:apigateway:/region/ ::/restapis//api-id/ /stages//stage-name/ @ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetWebACLForResource' value with any optional fields omitted.
mkGetWebACLForResource
    :: Types.ResourceArn -- ^ 'resourceArn'
    -> GetWebACLForResource
mkGetWebACLForResource resourceArn
  = GetWebACLForResource'{resourceArn}

-- | The ARN (Amazon Resource Name) of the resource for which to get the web ACL, either an application load balancer or Amazon API Gateway stage.
--
-- The ARN should be in one of the following formats:
--
--     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @ 
--
--
--     * For an Amazon API Gateway stage: @arn:aws:apigateway:/region/ ::/restapis//api-id/ /stages//stage-name/ @ 
--
--
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwaclfrResourceArn :: Lens.Lens' GetWebACLForResource Types.ResourceArn
gwaclfrResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE gwaclfrResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery GetWebACLForResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetWebACLForResource where
        toHeaders GetWebACLForResource{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.GetWebACLForResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetWebACLForResource where
        toJSON GetWebACLForResource{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ResourceArn" Core..= resourceArn)])

instance Core.AWSRequest GetWebACLForResource where
        type Rs GetWebACLForResource = GetWebACLForResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetWebACLForResourceResponse' Core.<$>
                   (x Core..:? "WebACLSummary") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetWebACLForResourceResponse' smart constructor.
data GetWebACLForResourceResponse = GetWebACLForResourceResponse'
  { webACLSummary :: Core.Maybe Types.WebACLSummary
    -- ^ Information about the web ACL that you specified in the @GetWebACLForResource@ request. If there is no associated resource, a null WebACLSummary is returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetWebACLForResourceResponse' value with any optional fields omitted.
mkGetWebACLForResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetWebACLForResourceResponse
mkGetWebACLForResourceResponse responseStatus
  = GetWebACLForResourceResponse'{webACLSummary = Core.Nothing,
                                  responseStatus}

-- | Information about the web ACL that you specified in the @GetWebACLForResource@ request. If there is no associated resource, a null WebACLSummary is returned.
--
-- /Note:/ Consider using 'webACLSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwaclfrrrsWebACLSummary :: Lens.Lens' GetWebACLForResourceResponse (Core.Maybe Types.WebACLSummary)
gwaclfrrrsWebACLSummary = Lens.field @"webACLSummary"
{-# INLINEABLE gwaclfrrrsWebACLSummary #-}
{-# DEPRECATED webACLSummary "Use generic-lens or generic-optics with 'webACLSummary' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwaclfrrrsResponseStatus :: Lens.Lens' GetWebACLForResourceResponse Core.Int
gwaclfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gwaclfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
