{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVpcAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified VPC. You can specify only one attribute at a time.
module Network.AWS.EC2.DescribeVpcAttribute
    (
    -- * Creating a request
      DescribeVpcAttribute (..)
    , mkDescribeVpcAttribute
    -- ** Request lenses
    , dvafAttribute
    , dvafVpcId
    , dvafDryRun

    -- * Destructuring the response
    , DescribeVpcAttributeResponse (..)
    , mkDescribeVpcAttributeResponse
    -- ** Response lenses
    , dvarfrsEnableDnsHostnames
    , dvarfrsEnableDnsSupport
    , dvarfrsVpcId
    , dvarfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVpcAttribute' smart constructor.
data DescribeVpcAttribute = DescribeVpcAttribute'
  { attribute :: Types.VpcAttributeName
    -- ^ The VPC attribute.
  , vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcAttribute' value with any optional fields omitted.
mkDescribeVpcAttribute
    :: Types.VpcAttributeName -- ^ 'attribute'
    -> Types.VpcId -- ^ 'vpcId'
    -> DescribeVpcAttribute
mkDescribeVpcAttribute attribute vpcId
  = DescribeVpcAttribute'{attribute, vpcId, dryRun = Core.Nothing}

-- | The VPC attribute.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvafAttribute :: Lens.Lens' DescribeVpcAttribute Types.VpcAttributeName
dvafAttribute = Lens.field @"attribute"
{-# INLINEABLE dvafAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvafVpcId :: Lens.Lens' DescribeVpcAttribute Types.VpcId
dvafVpcId = Lens.field @"vpcId"
{-# INLINEABLE dvafVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvafDryRun :: Lens.Lens' DescribeVpcAttribute (Core.Maybe Core.Bool)
dvafDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvafDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DescribeVpcAttribute where
        toQuery DescribeVpcAttribute{..}
          = Core.toQueryPair "Action" ("DescribeVpcAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Attribute" attribute
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DescribeVpcAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVpcAttribute where
        type Rs DescribeVpcAttribute = DescribeVpcAttributeResponse
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
          = Response.receiveXML
              (\ s h x ->
                 DescribeVpcAttributeResponse' Core.<$>
                   (x Core..@? "enableDnsHostnames") Core.<*>
                     x Core..@? "enableDnsSupport"
                     Core.<*> x Core..@? "vpcId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeVpcAttributeResponse' smart constructor.
data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse'
  { enableDnsHostnames :: Core.Maybe Types.AttributeBooleanValue
    -- ^ Indicates whether the instances launched in the VPC get DNS hostnames. If this attribute is @true@ , instances in the VPC get DNS hostnames; otherwise, they do not.
  , enableDnsSupport :: Core.Maybe Types.AttributeBooleanValue
    -- ^ Indicates whether DNS resolution is enabled for the VPC. If this attribute is @true@ , the Amazon DNS server resolves DNS hostnames for your instances to their corresponding IP addresses; otherwise, it does not.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcAttributeResponse' value with any optional fields omitted.
mkDescribeVpcAttributeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpcAttributeResponse
mkDescribeVpcAttributeResponse responseStatus
  = DescribeVpcAttributeResponse'{enableDnsHostnames = Core.Nothing,
                                  enableDnsSupport = Core.Nothing, vpcId = Core.Nothing,
                                  responseStatus}

-- | Indicates whether the instances launched in the VPC get DNS hostnames. If this attribute is @true@ , instances in the VPC get DNS hostnames; otherwise, they do not.
--
-- /Note:/ Consider using 'enableDnsHostnames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarfrsEnableDnsHostnames :: Lens.Lens' DescribeVpcAttributeResponse (Core.Maybe Types.AttributeBooleanValue)
dvarfrsEnableDnsHostnames = Lens.field @"enableDnsHostnames"
{-# INLINEABLE dvarfrsEnableDnsHostnames #-}
{-# DEPRECATED enableDnsHostnames "Use generic-lens or generic-optics with 'enableDnsHostnames' instead"  #-}

-- | Indicates whether DNS resolution is enabled for the VPC. If this attribute is @true@ , the Amazon DNS server resolves DNS hostnames for your instances to their corresponding IP addresses; otherwise, it does not.
--
-- /Note:/ Consider using 'enableDnsSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarfrsEnableDnsSupport :: Lens.Lens' DescribeVpcAttributeResponse (Core.Maybe Types.AttributeBooleanValue)
dvarfrsEnableDnsSupport = Lens.field @"enableDnsSupport"
{-# INLINEABLE dvarfrsEnableDnsSupport #-}
{-# DEPRECATED enableDnsSupport "Use generic-lens or generic-optics with 'enableDnsSupport' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarfrsVpcId :: Lens.Lens' DescribeVpcAttributeResponse (Core.Maybe Core.Text)
dvarfrsVpcId = Lens.field @"vpcId"
{-# INLINEABLE dvarfrsVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarfrsResponseStatus :: Lens.Lens' DescribeVpcAttributeResponse Core.Int
dvarfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvarfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
