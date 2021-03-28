{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeAccountAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes attributes of your AWS account. The following are the supported account attributes:
--
--
--     * @supported-platforms@ : Indicates whether your account can launch instances into EC2-Classic and EC2-VPC, or only into EC2-VPC.
--
--
--     * @default-vpc@ : The ID of the default VPC for your account, or @none@ .
--
--
--     * @max-instances@ : This attribute is no longer supported. The returned value does not reflect your actual vCPU limit for running On-Demand Instances. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-on-demand-instances.html#ec2-on-demand-instances-limits On-Demand Instance Limits> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
--     * @vpc-max-security-groups-per-interface@ : The maximum number of security groups that you can assign to a network interface.
--
--
--     * @max-elastic-ips@ : The maximum number of Elastic IP addresses that you can allocate for use with EC2-Classic. 
--
--
--     * @vpc-max-elastic-ips@ : The maximum number of Elastic IP addresses that you can allocate for use with EC2-VPC.
--
--
module Network.AWS.EC2.DescribeAccountAttributes
    (
    -- * Creating a request
      DescribeAccountAttributes (..)
    , mkDescribeAccountAttributes
    -- ** Request lenses
    , daaAttributeNames
    , daaDryRun

    -- * Destructuring the response
    , DescribeAccountAttributesResponse (..)
    , mkDescribeAccountAttributesResponse
    -- ** Response lenses
    , daarrsAccountAttributes
    , daarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  { attributeNames :: Core.Maybe [Types.AccountAttributeName]
    -- ^ The account attribute names.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountAttributes' value with any optional fields omitted.
mkDescribeAccountAttributes
    :: DescribeAccountAttributes
mkDescribeAccountAttributes
  = DescribeAccountAttributes'{attributeNames = Core.Nothing,
                               dryRun = Core.Nothing}

-- | The account attribute names.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaAttributeNames :: Lens.Lens' DescribeAccountAttributes (Core.Maybe [Types.AccountAttributeName])
daaAttributeNames = Lens.field @"attributeNames"
{-# INLINEABLE daaAttributeNames #-}
{-# DEPRECATED attributeNames "Use generic-lens or generic-optics with 'attributeNames' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaDryRun :: Lens.Lens' DescribeAccountAttributes (Core.Maybe Core.Bool)
daaDryRun = Lens.field @"dryRun"
{-# INLINEABLE daaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DescribeAccountAttributes where
        toQuery DescribeAccountAttributes{..}
          = Core.toQueryPair "Action"
              ("DescribeAccountAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "AttributeName")
                attributeNames
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DescribeAccountAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAccountAttributes where
        type Rs DescribeAccountAttributes =
             DescribeAccountAttributesResponse
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
                 DescribeAccountAttributesResponse' Core.<$>
                   (x Core..@? "accountAttributeSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { accountAttributes :: Core.Maybe [Types.AccountAttribute]
    -- ^ Information about the account attributes.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountAttributesResponse' value with any optional fields omitted.
mkDescribeAccountAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAccountAttributesResponse
mkDescribeAccountAttributesResponse responseStatus
  = DescribeAccountAttributesResponse'{accountAttributes =
                                         Core.Nothing,
                                       responseStatus}

-- | Information about the account attributes.
--
-- /Note:/ Consider using 'accountAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsAccountAttributes :: Lens.Lens' DescribeAccountAttributesResponse (Core.Maybe [Types.AccountAttribute])
daarrsAccountAttributes = Lens.field @"accountAttributes"
{-# INLINEABLE daarrsAccountAttributes #-}
{-# DEPRECATED accountAttributes "Use generic-lens or generic-optics with 'accountAttributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsResponseStatus :: Lens.Lens' DescribeAccountAttributesResponse Core.Int
daarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
