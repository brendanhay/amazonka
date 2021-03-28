{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeTargetGroupAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the attributes for the specified target group.
--
-- For more information, see the following:
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-target-groups.html#target-group-attributes Target group attributes> in the /Application Load Balancers Guide/ 
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-target-groups.html#target-group-attributes Target group attributes> in the /Network Load Balancers Guide/ 
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/target-groups.html#target-group-attributes Target group attributes> in the /Gateway Load Balancers Guide/ 
--
--
module Network.AWS.ELBv2.DescribeTargetGroupAttributes
    (
    -- * Creating a request
      DescribeTargetGroupAttributes (..)
    , mkDescribeTargetGroupAttributes
    -- ** Request lenses
    , dtgaTargetGroupArn

    -- * Destructuring the response
    , DescribeTargetGroupAttributesResponse (..)
    , mkDescribeTargetGroupAttributesResponse
    -- ** Response lenses
    , dtgarrsAttributes
    , dtgarrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTargetGroupAttributes' smart constructor.
newtype DescribeTargetGroupAttributes = DescribeTargetGroupAttributes'
  { targetGroupArn :: Types.TargetGroupArn
    -- ^ The Amazon Resource Name (ARN) of the target group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTargetGroupAttributes' value with any optional fields omitted.
mkDescribeTargetGroupAttributes
    :: Types.TargetGroupArn -- ^ 'targetGroupArn'
    -> DescribeTargetGroupAttributes
mkDescribeTargetGroupAttributes targetGroupArn
  = DescribeTargetGroupAttributes'{targetGroupArn}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaTargetGroupArn :: Lens.Lens' DescribeTargetGroupAttributes Types.TargetGroupArn
dtgaTargetGroupArn = Lens.field @"targetGroupArn"
{-# INLINEABLE dtgaTargetGroupArn #-}
{-# DEPRECATED targetGroupArn "Use generic-lens or generic-optics with 'targetGroupArn' instead"  #-}

instance Core.ToQuery DescribeTargetGroupAttributes where
        toQuery DescribeTargetGroupAttributes{..}
          = Core.toQueryPair "Action"
              ("DescribeTargetGroupAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "TargetGroupArn" targetGroupArn

instance Core.ToHeaders DescribeTargetGroupAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeTargetGroupAttributes where
        type Rs DescribeTargetGroupAttributes =
             DescribeTargetGroupAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeTargetGroupAttributesResult"
              (\ s h x ->
                 DescribeTargetGroupAttributesResponse' Core.<$>
                   (x Core..@? "Attributes" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeTargetGroupAttributesResponse' smart constructor.
data DescribeTargetGroupAttributesResponse = DescribeTargetGroupAttributesResponse'
  { attributes :: Core.Maybe [Types.TargetGroupAttribute]
    -- ^ Information about the target group attributes
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTargetGroupAttributesResponse' value with any optional fields omitted.
mkDescribeTargetGroupAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeTargetGroupAttributesResponse
mkDescribeTargetGroupAttributesResponse responseStatus
  = DescribeTargetGroupAttributesResponse'{attributes = Core.Nothing,
                                           responseStatus}

-- | Information about the target group attributes
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgarrsAttributes :: Lens.Lens' DescribeTargetGroupAttributesResponse (Core.Maybe [Types.TargetGroupAttribute])
dtgarrsAttributes = Lens.field @"attributes"
{-# INLINEABLE dtgarrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgarrsResponseStatus :: Lens.Lens' DescribeTargetGroupAttributesResponse Core.Int
dtgarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtgarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
