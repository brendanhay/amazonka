{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateDefaultVpc
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a default VPC with a size @/16@ IPv4 CIDR block and a default subnet in each Availability Zone. For more information about the components of a default VPC, see <https://docs.aws.amazon.com/vpc/latest/userguide/default-vpc.html Default VPC and Default Subnets> in the /Amazon Virtual Private Cloud User Guide/ . You cannot specify the components of the default VPC yourself.
--
-- If you deleted your previous default VPC, you can create a default VPC. You cannot have more than one default VPC per Region.
-- If your account supports EC2-Classic, you cannot use this action to create a default VPC in a Region that supports EC2-Classic. If you want a default VPC in a Region that supports EC2-Classic, see "I really want a default VPC for my existing EC2 account. Is that possible?" in the <http://aws.amazon.com/vpc/faqs/#Default_VPCs Default VPCs FAQ> .
module Network.AWS.EC2.CreateDefaultVpc
    (
    -- * Creating a request
      CreateDefaultVpc (..)
    , mkCreateDefaultVpc
    -- ** Request lenses
    , cdvDryRun

    -- * Destructuring the response
    , CreateDefaultVpcResponse (..)
    , mkCreateDefaultVpcResponse
    -- ** Response lenses
    , cdvrrsVpc
    , cdvrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDefaultVpc' smart constructor.
newtype CreateDefaultVpc = CreateDefaultVpc'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDefaultVpc' value with any optional fields omitted.
mkCreateDefaultVpc
    :: CreateDefaultVpc
mkCreateDefaultVpc = CreateDefaultVpc'{dryRun = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvDryRun :: Lens.Lens' CreateDefaultVpc (Core.Maybe Core.Bool)
cdvDryRun = Lens.field @"dryRun"
{-# INLINEABLE cdvDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery CreateDefaultVpc where
        toQuery CreateDefaultVpc{..}
          = Core.toQueryPair "Action" ("CreateDefaultVpc" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders CreateDefaultVpc where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateDefaultVpc where
        type Rs CreateDefaultVpc = CreateDefaultVpcResponse
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
                 CreateDefaultVpcResponse' Core.<$>
                   (x Core..@? "vpc") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDefaultVpcResponse' smart constructor.
data CreateDefaultVpcResponse = CreateDefaultVpcResponse'
  { vpc :: Core.Maybe Types.Vpc
    -- ^ Information about the VPC.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDefaultVpcResponse' value with any optional fields omitted.
mkCreateDefaultVpcResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDefaultVpcResponse
mkCreateDefaultVpcResponse responseStatus
  = CreateDefaultVpcResponse'{vpc = Core.Nothing, responseStatus}

-- | Information about the VPC.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvrrsVpc :: Lens.Lens' CreateDefaultVpcResponse (Core.Maybe Types.Vpc)
cdvrrsVpc = Lens.field @"vpc"
{-# INLINEABLE cdvrrsVpc #-}
{-# DEPRECATED vpc "Use generic-lens or generic-optics with 'vpc' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvrrsResponseStatus :: Lens.Lens' CreateDefaultVpcResponse Core.Int
cdvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
