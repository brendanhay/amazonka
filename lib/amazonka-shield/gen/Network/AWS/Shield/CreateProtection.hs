{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.CreateProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables AWS Shield Advanced for a specific AWS resource. The resource can be an Amazon CloudFront distribution, Elastic Load Balancing load balancer, AWS Global Accelerator accelerator, Elastic IP Address, or an Amazon Route 53 hosted zone.
--
-- You can add protection to only a single resource with each CreateProtection request. If you want to add protection to multiple resources at once, use the <https://console.aws.amazon.com/waf/ AWS WAF console> . For more information see <https://docs.aws.amazon.com/waf/latest/developerguide/getting-started-ddos.html Getting Started with AWS Shield Advanced> and <https://docs.aws.amazon.com/waf/latest/developerguide/configure-new-protection.html Add AWS Shield Advanced Protection to more AWS Resources> .
module Network.AWS.Shield.CreateProtection
    (
    -- * Creating a request
      CreateProtection (..)
    , mkCreateProtection
    -- ** Request lenses
    , cpName
    , cpResourceArn

    -- * Destructuring the response
    , CreateProtectionResponse (..)
    , mkCreateProtectionResponse
    -- ** Response lenses
    , cprrsProtectionId
    , cprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkCreateProtection' smart constructor.
data CreateProtection = CreateProtection'
  { name :: Types.Name
    -- ^ Friendly name for the @Protection@ you are creating.
  , resourceArn :: Types.ResourceArn
    -- ^ The ARN (Amazon Resource Name) of the resource to be protected.
--
-- The ARN should be in one of the following formats:
--
--     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @ 
--
--
--     * For an Elastic Load Balancer (Classic Load Balancer): @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer//load-balancer-name/ @ 
--
--
--     * For an AWS CloudFront distribution: @arn:aws:cloudfront::/account-id/ :distribution//distribution-id/ @ 
--
--
--     * For an AWS Global Accelerator accelerator: @arn:aws:globalaccelerator::/account-id/ :accelerator//accelerator-id/ @ 
--
--
--     * For Amazon Route 53: @arn:aws:route53:::hostedzone//hosted-zone-id/ @ 
--
--
--     * For an Elastic IP address: @arn:aws:ec2:/region/ :/account-id/ :eip-allocation//allocation-id/ @ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProtection' value with any optional fields omitted.
mkCreateProtection
    :: Types.Name -- ^ 'name'
    -> Types.ResourceArn -- ^ 'resourceArn'
    -> CreateProtection
mkCreateProtection name resourceArn
  = CreateProtection'{name, resourceArn}

-- | Friendly name for the @Protection@ you are creating.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreateProtection Types.Name
cpName = Lens.field @"name"
{-# INLINEABLE cpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ARN (Amazon Resource Name) of the resource to be protected.
--
-- The ARN should be in one of the following formats:
--
--     * For an Application Load Balancer: @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer/app//load-balancer-name/ //load-balancer-id/ @ 
--
--
--     * For an Elastic Load Balancer (Classic Load Balancer): @arn:aws:elasticloadbalancing:/region/ :/account-id/ :loadbalancer//load-balancer-name/ @ 
--
--
--     * For an AWS CloudFront distribution: @arn:aws:cloudfront::/account-id/ :distribution//distribution-id/ @ 
--
--
--     * For an AWS Global Accelerator accelerator: @arn:aws:globalaccelerator::/account-id/ :accelerator//accelerator-id/ @ 
--
--
--     * For Amazon Route 53: @arn:aws:route53:::hostedzone//hosted-zone-id/ @ 
--
--
--     * For an Elastic IP address: @arn:aws:ec2:/region/ :/account-id/ :eip-allocation//allocation-id/ @ 
--
--
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpResourceArn :: Lens.Lens' CreateProtection Types.ResourceArn
cpResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE cpResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery CreateProtection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateProtection where
        toHeaders CreateProtection{..}
          = Core.pure ("X-Amz-Target", "AWSShield_20160616.CreateProtection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateProtection where
        toJSON CreateProtection{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("ResourceArn" Core..= resourceArn)])

instance Core.AWSRequest CreateProtection where
        type Rs CreateProtection = CreateProtectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateProtectionResponse' Core.<$>
                   (x Core..:? "ProtectionId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateProtectionResponse' smart constructor.
data CreateProtectionResponse = CreateProtectionResponse'
  { protectionId :: Core.Maybe Types.ProtectionId
    -- ^ The unique identifier (ID) for the 'Protection' object that is created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProtectionResponse' value with any optional fields omitted.
mkCreateProtectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateProtectionResponse
mkCreateProtectionResponse responseStatus
  = CreateProtectionResponse'{protectionId = Core.Nothing,
                              responseStatus}

-- | The unique identifier (ID) for the 'Protection' object that is created.
--
-- /Note:/ Consider using 'protectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsProtectionId :: Lens.Lens' CreateProtectionResponse (Core.Maybe Types.ProtectionId)
cprrsProtectionId = Lens.field @"protectionId"
{-# INLINEABLE cprrsProtectionId #-}
{-# DEPRECATED protectionId "Use generic-lens or generic-optics with 'protectionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreateProtectionResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
